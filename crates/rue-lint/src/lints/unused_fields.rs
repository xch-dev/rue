use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use rue_ast::{AstNode, AstStructItem};
use rue_diagnostic::{Lint, Name, Note, SourceKind, SrcLoc};
use rue_hir::{Database, Declaration, FunctionKind, Hir, HirId, Statement, Symbol};
use rue_types::{Type, TypeId};

use crate::{HirLintPass, LintContext, LintInput, srcloc};

/// Identifies a struct by the location of its name, which every copy of it (such as for generic
/// arguments or narrowing) shares.
type StructKey = (SourceKind, usize);

fn struct_key(name: &Name) -> Option<StructKey> {
    let srcloc = name.srcloc()?;
    Some((srcloc.source.kind.clone(), srcloc.span.start))
}

/// Reports the fields of structs that are never read.
///
/// A struct's value is also its layout, so a field that's never read by name can still be used
/// by something that reads the whole value, such as a hash, another puzzle, or the output of a
/// program. A struct's fields are only reported if every value of it is only ever read by field
/// (or passed to a function that takes the struct), rather than escaping somewhere else.
#[derive(Debug)]
pub(crate) struct UnusedStructField<'a> {
    entrypoints: &'a HashSet<Declaration>,
    /// The fields that are read in each struct.
    reads: HashMap<StructKey, HashSet<String>>,
    /// The location of each field's name in each struct, in the order they're declared.
    fields: HashMap<StructKey, Vec<(String, SrcLoc)>>,
    /// The structs declared in the documents being linted, once they've been found, along with
    /// the index that identifies each of them in the rest of the pass.
    candidates: Option<HashMap<StructKey, usize>>,
    /// The structs that a value of each type can contain, for the types seen so far.
    mentions: HashMap<TypeId, Rc<[usize]>>,
    /// The structs whose values escape, so their fields can be read in other ways.
    escaped: HashSet<usize>,
    /// The structs that have a value somewhere, rather than only being used as a type (such as in
    /// the signature of an external function).
    seen: HashSet<usize>,
}

impl<'a> UnusedStructField<'a> {
    pub(crate) fn new(input: &'a LintInput<'_>) -> Self {
        let mut reads: HashMap<StructKey, HashSet<String>> = HashMap::new();

        for read in &input.sites.field_reads {
            if let Some(key) = struct_key(&read.struct_name) {
                reads.entry(key).or_default().insert(read.field.clone());
            }
        }

        let mut fields: HashMap<StructKey, Vec<(String, SrcLoc)>> = HashMap::new();

        for &(source, document) in &input.documents {
            for item in document
                .syntax()
                .descendants()
                .filter_map(AstStructItem::cast)
            {
                let Some(name) = item.name() else {
                    continue;
                };

                let key = (source.kind.clone(), usize::from(name.text_range().start()));

                fields.insert(
                    key,
                    item.fields()
                        .filter_map(|field| field.name())
                        .map(|name| (name.text().to_string(), srcloc(source, name.text_range())))
                        .collect(),
                );
            }
        }

        Self {
            entrypoints: input.entrypoints,
            reads,
            fields,
            candidates: None,
            mentions: HashMap::new(),
            escaped: HashSet::new(),
            seen: HashSet::new(),
        }
    }

    fn candidates(&mut self, db: &Database) -> &HashMap<StructKey, usize> {
        self.candidates.get_or_insert_with(|| {
            db.relevant_declarations()
                .filter_map(|declaration| match declaration {
                    Declaration::Type(ty) => match db.ty(ty) {
                        Type::Struct(ty) => ty.name.as_ref().and_then(struct_key),
                        _ => None,
                    },
                    Declaration::Symbol(_) => None,
                })
                .enumerate()
                .map(|(index, key)| (key, index))
                .collect()
        })
    }

    /// The structs declared in the documents being linted that a value of the type can contain.
    fn mentions(&mut self, db: &Database, ty: TypeId) -> Rc<[usize]> {
        if let Some(mentions) = self.mentions.get(&ty) {
            return mentions.clone();
        }

        let mut keys = HashSet::new();
        collect_structs(db, ty, &mut keys, &mut HashSet::new());

        let candidates = self.candidates(db);
        let mentions = keys
            .iter()
            .filter_map(|key| candidates.get(key).copied())
            .collect::<Rc<[usize]>>();

        self.mentions.insert(ty, mentions.clone());
        mentions
    }

    fn type_mentions(&mut self, db: &Database, ty: TypeId, key: usize) -> bool {
        self.mentions(db, ty).contains(&key)
    }

    /// Whether the value of the current node can be read other than by field, by following it
    /// through the nodes that only pass it along.
    fn escapes(&mut self, cx: &LintContext<'_>, hir: HirId, key: usize) -> bool {
        let db = cx.db();
        let mut child = hir;

        for &parent in cx.parents().iter().rev() {
            match db.hir(parent) {
                Hir::FieldAccess(access) if access.value == child => return false,
                Hir::If(condition, ..) if *condition != child => child = parent,
                Hir::Block(block) => {
                    if block.body == Some(child) {
                        child = parent;
                        continue;
                    }

                    return self.escapes_from_statement(db, &block.statements, child, key);
                }
                Hir::FunctionCall(call) if call.function != child => {
                    return !self.is_struct_argument(db, call, child, key);
                }
                _ => return true,
            }
        }

        // A function's body is returned, but a constant's value is read where it's referenced
        match cx.enclosing_declaration() {
            Some(Declaration::Symbol(symbol)) => {
                cx.enclosing_function().is_some()
                    || !matches!(db.symbol(symbol), Symbol::Constant(_))
            }
            _ => true,
        }
    }

    fn escapes_from_statement(
        &mut self,
        db: &Database,
        statements: &[Statement],
        child: HirId,
        key: usize,
    ) -> bool {
        for statement in statements {
            match statement {
                // A binding is read where it's referenced, as long as its type still has the
                // struct in it (rather than being annotated with a wider type)
                Statement::Let(symbol, _) => {
                    if let Symbol::Binding(binding) = db.symbol(*symbol)
                        && binding.value.hir == child
                    {
                        return !self.type_mentions(db, binding.value.ty, key);
                    }
                }
                // The value of an expression statement is discarded, and the `then` block of an
                // `if` statement exits
                Statement::Expr(statement) if statement.hir == child => return false,
                Statement::If(statement) if statement.then == child => return false,
                _ => {}
            }
        }

        true
    }

    /// Whether the node is an argument of a function that takes the struct for that parameter, so
    /// that it's read where the parameter is referenced.
    fn is_struct_argument(
        &mut self,
        db: &Database,
        call: &rue_hir::FunctionCall,
        child: HirId,
        key: usize,
    ) -> bool {
        let Hir::Reference(function) = db.hir(db.without_casts(call.function)) else {
            return false;
        };

        let Symbol::Function(function) = db.symbol(*function) else {
            return false;
        };

        // The last argument is spread into the parameters if the call isn't nil-terminated
        if function.kind == FunctionKind::External || !call.nil_terminated {
            return false;
        }

        let Some(index) = call.args.iter().position(|&arg| arg == child) else {
            return false;
        };

        let Some(&parameter) = function
            .parameters
            .get_index(index)
            .map(|(_, symbol)| symbol)
        else {
            return false;
        };

        let Symbol::Parameter(parameter) = db.symbol(parameter) else {
            return false;
        };

        self.type_mentions(db, parameter.ty, key)
    }
}

impl HirLintPass for UnusedStructField<'_> {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnusedStructField]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        // A type guard checks every field of the struct
        if let Hir::Guard(guard) = node {
            let mentions = self.mentions(cx.db(), guard.ty);
            self.escaped.extend(mentions.iter().copied());
        }

        let Some(ty) = cx.db().hir_type(hir) else {
            return;
        };

        for &key in self.mentions(cx.db(), ty).iter() {
            if !self.escaped.contains(&key) && self.escapes(cx, hir, key) {
                self.escaped.insert(key);
            }

            self.seen.insert(key);
        }
    }

    fn finish(&mut self, cx: &mut LintContext<'_>) {
        let mut reports = Vec::new();
        let db = cx.db();
        self.candidates(db);
        let candidates = self
            .candidates
            .as_ref()
            .expect("candidates were just found");

        for declaration in db.relevant_declarations() {
            let Declaration::Type(ty) = declaration else {
                continue;
            };

            let Type::Struct(ty) = db.ty(ty) else {
                continue;
            };

            let Some(name) = ty.name.clone() else {
                continue;
            };

            let Some(key) = struct_key(&name) else {
                continue;
            };

            let Some(&index) = candidates.get(&key) else {
                continue;
            };

            // A struct that's never used is reported by `unused_struct` instead, and the fields
            // of exported structs can be read by anything that imports them
            if name.text().starts_with('_')
                || self.entrypoints.contains(&declaration)
                || db.reference_parents(declaration).is_empty()
                || self.escaped.contains(&index)
                || !self.seen.contains(&index)
            {
                continue;
            }

            let reads = self.reads.get(&key);

            for (field, srcloc) in self.fields.get(&key).into_iter().flatten() {
                if field.starts_with('_') || reads.is_some_and(|reads| reads.contains(field)) {
                    continue;
                }

                reports.push((
                    srcloc.clone(),
                    format!("Field `{field}` of struct `{}` is never read", name.text()),
                ));
            }
        }

        for (srcloc, message) in reports {
            if let Some(diagnostic) = cx.report(Lint::UnusedStructField, srcloc, message) {
                diagnostic.notes.push(Note::new(
                    "If it's only needed for the layout of the struct, prefix its name with `_`",
                ));
            }
        }
    }
}

/// Adds the structs that a value of the type can contain.
fn collect_structs(
    db: &Database,
    ty: TypeId,
    keys: &mut HashSet<StructKey>,
    visited: &mut HashSet<TypeId>,
) {
    if !visited.insert(ty) {
        return;
    }

    match db.ty(ty) {
        Type::Struct(ty) => {
            if let Some(key) = ty.name.as_ref().and_then(struct_key) {
                keys.insert(key);
            }

            collect_structs(db, ty.inner, keys, visited);

            for &generic in &ty.generics {
                collect_structs(db, generic, keys, visited);
            }
        }
        Type::Alias(alias) => {
            collect_structs(db, alias.inner, keys, visited);

            for &generic in &alias.generics {
                collect_structs(db, generic, keys, visited);
            }
        }
        Type::Ref(inner) => collect_structs(db, *inner, keys, visited),
        Type::Pair(pair) => {
            collect_structs(db, pair.first, keys, visited);
            collect_structs(db, pair.rest, keys, visited);
        }
        Type::Union(union) => {
            for &ty in &union.types {
                collect_structs(db, ty, keys, visited);
            }
        }
        Type::Apply(apply) => {
            collect_structs(db, apply.inner, keys, visited);

            for &generic in apply.generics.values() {
                collect_structs(db, generic, keys, visited);
            }
        }
        Type::Unresolved
        | Type::Generic(_)
        | Type::Any
        | Type::Never
        | Type::Atom(_)
        | Type::Function(_) => {}
    }
}
