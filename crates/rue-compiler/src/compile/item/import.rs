use std::iter::Peekable;

use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_hir::{Import, ImportId};

use crate::Compiler;

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let segments = path.segments().peekable();

    let imports = construct_import(ctx, segments, import.export().is_some());

    for import in imports {
        ctx.last_scope_mut().import(import);
    }
}

pub fn construct_import(
    ctx: &mut Compiler,
    mut segments: Peekable<impl Iterator<Item = AstImportPathSegment>>,
    exported: bool,
) -> Vec<ImportId> {
    let mut imports = Vec::new();

    let Some(first) = segments.next() else {
        return imports;
    };

    let Some(name) = first.name() else {
        return imports;
    };

    let next = segments.peek();

    let import = Import {
        name: name.clone(),
        exported,
        include_self: next.is_none(),
        include_all: next.is_some_and(|segment| segment.star().is_some()),
        children: if next.is_some_and(|segment| segment.name().is_some()) {
            construct_import(ctx, segments, exported)
        } else {
            Vec::new()
        },
    };

    imports.push(ctx.alloc_import(import));

    imports
}
