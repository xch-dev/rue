use std::{fs, path::Path};

use clvmr::{Allocator, serde::node_from_bytes};
use indexmap::IndexMap;
use log::debug;
use rue_ast::{AstFunctionItem, AstNode};
use rue_diagnostic::{DiagnosticKind, SourceKind};
use rue_hir::{
    Declaration, FunctionKind, FunctionSymbol, HirId, ParameterSymbol, Symbol, SymbolId, Test,
};
use rue_parser::SyntaxToken;
use rue_types::{FunctionType, Type};

use crate::{
    BlockKind, Compiler, CompletionContext, ReturnTarget, SyntaxItemKind, compile_block,
    compile_generic_parameters, compile_type, const_eval::decode_node, create_binding,
};

pub fn declare_function(ctx: &mut Compiler, function: &AstFunctionItem) -> SymbolId {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        function.syntax().text_range(),
    );

    let symbol = ctx.alloc_symbol(Symbol::Unresolved);

    if function.test().is_some() {
        let path = ctx.source().kind.clone();

        ctx.add_test(Test {
            name: function.name().map(|name| name.text().to_string()),
            path,
            symbol,
        });
    }

    ctx.push_declaration(Declaration::Symbol(symbol));

    let scope = ctx.alloc_child_scope();

    let vars = if let Some(generic_parameters) = function.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let range = function.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let return_type = if let Some(return_type) = function.return_type() {
        compile_type(ctx, &return_type)
    } else {
        ctx.builtins().nil.ty
    };

    let mut parameters = IndexMap::new();
    let mut param_types = IndexMap::new();
    let mut nil_terminated = true;

    let len = function.parameters().count();

    for (i, parameter) in function.parameters().enumerate() {
        let is_spread = if let Some(spread) = parameter.spread() {
            if i == len - 1 {
                true
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
                false
            }
        } else {
            false
        };

        if is_spread {
            nil_terminated = false;
        }

        let symbol = ctx.alloc_symbol(Symbol::Unresolved);

        ctx.push_declaration(Declaration::Symbol(symbol));

        let ty = if let Some(ty) = parameter.ty() {
            compile_type(ctx, &ty)
        } else {
            debug!("Unresolved function parameter type");
            ctx.builtins().unresolved.ty
        };

        *ctx.symbol_mut(symbol) = Symbol::Parameter(ParameterSymbol { name: None, ty });

        let name = parameter
            .binding()
            .map_or(String::new(), |binding| binding.syntax().text().to_string());

        param_types.insert(name.clone(), ty);
        parameters.insert(name, symbol);

        ctx.pop_declaration();
    }

    ctx.pop_scope(range.end());

    let body = ctx.builtins().unresolved.hir;

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        params: param_types,
        nil_terminated,
        ret: return_type,
    }));

    if function.source_path().is_some() {
        for &parameter in parameters.values() {
            ctx.reference(Declaration::Symbol(parameter), None);
        }
    }

    let name = function.name().map(|name| ctx.local_name(&name));

    *ctx.symbol_mut(symbol) = Symbol::Function(FunctionSymbol {
        name,
        ty,
        scope,
        vars,
        parameters,
        nil_terminated,
        return_type,
        body,
        kind: if function.inline().is_some() {
            FunctionKind::Inline
        } else if function.source_path().is_some() {
            FunctionKind::External
        } else if function.extern_kw().is_some() {
            FunctionKind::Sequential
        } else {
            FunctionKind::BinaryTree
        },
    });

    if let Some(name) = function.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        } else {
            ctx.last_scope_mut().insert_symbol(
                name.text().to_string(),
                symbol,
                function.export().is_some(),
            );
        }

        ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());
    }

    ctx.pop_declaration();

    symbol
}

pub fn compile_function(ctx: &mut Compiler, function: &AstFunctionItem, symbol: SymbolId) {
    ctx.push_declaration(Declaration::Symbol(symbol));

    let Symbol::Function(FunctionSymbol {
        scope,
        parameters,
        return_type,
        ..
    }) = ctx.symbol(symbol).clone()
    else {
        unreachable!();
    };

    let range = function.syntax().text_range();
    ctx.push_scope(scope, range.start());

    for (i, parameter) in function.parameters().enumerate() {
        let symbol = parameters[i];

        ctx.push_declaration(Declaration::Symbol(symbol));

        if let Some(binding) = parameter.binding() {
            create_binding(ctx, symbol, &binding);
        }

        ctx.pop_declaration();
    }

    let resolved_body = if let Some(source_path) = function.source_path() {
        compile_external_function(ctx, &source_path)
    } else if let Some(body) = function.body() {
        ctx.push_return_target(Some(ReturnTarget {
            expected_type: Some(return_type),
            types: Vec::new(),
        }));
        let value = compile_block(
            ctx,
            &body,
            BlockKind::Body,
            Some(return_type),
            function.return_type().is_some(),
        );
        ctx.pop_return_target();
        ctx.assign_type(body.syntax(), value.ty, return_type);
        value.hir
    } else {
        debug!("Unresolved function body");
        ctx.builtins().unresolved.hir
    };

    ctx.pop_scope(range.end());

    let Symbol::Function(FunctionSymbol { body, .. }) = ctx.symbol_mut(symbol) else {
        unreachable!();
    };

    *body = resolved_body;

    ctx.pop_declaration();
}

fn compile_external_function(ctx: &mut Compiler, source_path: &SyntaxToken) -> HirId {
    let unresolved = ctx.builtins().unresolved.hir;
    let path_text = source_path
        .text()
        .strip_prefix('"')
        .and_then(|path| path.strip_suffix('"'))
        .unwrap_or(source_path.text());
    let relative_path = Path::new(path_text);

    if relative_path.is_absolute() {
        ctx.diagnostic(source_path, DiagnosticKind::AbsoluteExternalPath);
        return unresolved;
    }

    if relative_path
        .extension()
        .is_none_or(|extension| extension != "hex")
    {
        ctx.diagnostic(source_path, DiagnosticKind::InvalidExternalExtension);
        return unresolved;
    }

    let SourceKind::File(source_file) = &ctx.source().kind else {
        ctx.diagnostic(source_path, DiagnosticKind::ExternalFromNonFileSource);
        return unresolved;
    };
    let Some(parent) = Path::new(source_file).parent() else {
        ctx.diagnostic(source_path, DiagnosticKind::ExternalFromNonFileSource);
        return unresolved;
    };
    let unresolved_path = parent.join(relative_path);
    let resolved_path = match unresolved_path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            ctx.diagnostic(
                source_path,
                DiagnosticKind::ExternalFileRead(format!("{path_text}: {error}")),
            );
            return unresolved;
        }
    };

    let (bytes, should_cache) = if let Some(bytes) = ctx.external_program(&resolved_path) {
        (bytes.to_vec(), false)
    } else {
        let contents = match fs::read_to_string(&resolved_path) {
            Ok(contents) => contents,
            Err(error) => {
                ctx.diagnostic(
                    source_path,
                    DiagnosticKind::ExternalFileRead(format!("{path_text}: {error}")),
                );
                return unresolved;
            }
        };
        let hex = contents
            .chars()
            .filter(|character| !character.is_ascii_whitespace())
            .collect::<String>();
        let bytes = match hex::decode(hex) {
            Ok(bytes) => bytes,
            Err(error) => {
                ctx.diagnostic(
                    source_path,
                    DiagnosticKind::InvalidExternalHex(error.to_string()),
                );
                return unresolved;
            }
        };
        (bytes, true)
    };

    let mut allocator = Allocator::new();
    let program = match node_from_bytes(&mut allocator, &bytes) {
        Ok(program) => program,
        Err(error) => {
            ctx.diagnostic(
                source_path,
                DiagnosticKind::InvalidExternalClvm(error.to_string()),
            );
            return unresolved;
        }
    };
    if should_cache {
        ctx.cache_external_program(resolved_path, bytes);
    }
    let hir = decode_node(ctx, &allocator, program);
    ctx.alloc_hir(hir)
}
