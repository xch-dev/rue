use rue_diagnostic::Source;
use rue_parser::SyntaxToken;

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: Option<SyntaxToken>,
    pub source: Source,
}
