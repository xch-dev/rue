use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;

use crate::SyntaxKind;

pub type SyntaxNode = rowan::SyntaxNode<RueLang>;
pub type SyntaxToken = rowan::SyntaxToken<RueLang>;
pub type SyntaxElement = rowan::SyntaxElement<RueLang>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<RueLang>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<RueLang>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RueLang;

impl Language for RueLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}
