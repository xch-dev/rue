use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_hir::{Import, ImportId, Items};
use rue_parser::SyntaxToken;

use crate::Compiler;

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let imports = construct_imports(
        ctx,
        Vec::new(),
        &path.segments().collect::<Vec<_>>(),
        import.export().is_some(),
    );

    for import in imports {
        ctx.last_scope_mut().add_import(import);
    }
}

fn construct_imports(
    ctx: &mut Compiler,
    mut path: Vec<SyntaxToken>,
    segments: &[AstImportPathSegment],
    exported: bool,
) -> Vec<ImportId> {
    for segment in segments.iter().take(segments.len() - 1) {
        if let Some(name) = segment.name() {
            path.push(name);
        }
    }

    let Some(last) = segments.last() else {
        return vec![];
    };

    if let Some(name) = last.name() {
        vec![ctx.alloc_import(Import {
            path,
            items: Items::Named(vec![name]),
            exported,
        })]
    } else if let Some(star) = last.star() {
        vec![ctx.alloc_import(Import {
            path,
            items: Items::All(star),
            exported,
        })]
    } else if let items = last.items().collect::<Vec<_>>()
        && !items.is_empty()
    {
        let mut imports = Vec::new();

        for item in items {
            imports.extend(construct_imports(
                ctx,
                path.clone(),
                &item.segments().collect::<Vec<_>>(),
                exported,
            ));
        }

        imports
    } else {
        vec![]
    }
}
