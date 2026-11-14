use rue_ast::AstDocument;

use crate::{
    FormatError, FormatOptions, analysis::Layout, document::Doc, emit::Formatter,
    token_stream::TokenStream,
};

pub fn format_document(
    document: &AstDocument,
    stream: &TokenStream,
    options: &FormatOptions,
) -> Result<Doc, FormatError> {
    let layout = Layout::new(document, stream)?;
    Formatter::new(stream, layout, options).format()
}
