use std::iter::Peekable;

use indexmap::IndexMap;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_hir::Import;

use crate::Compiler;

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let segments = path.segments().peekable();

    let imports = construct_import(segments, import.export().is_some());

    for (name, import) in imports {
        ctx.last_scope_mut().import(name, import);
    }
}

pub fn construct_import(
    mut segments: Peekable<impl Iterator<Item = AstImportPathSegment>>,
    exported: bool,
) -> IndexMap<String, Import> {
    let mut imports = IndexMap::new();

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
            construct_import(segments, exported)
        } else {
            IndexMap::new()
        },
    };

    imports.insert(name.text().to_string(), import);

    imports
}
