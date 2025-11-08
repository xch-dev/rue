use std::fmt;

use id_arena::Id;
use indexmap::IndexMap;
use rue_parser::SyntaxToken;
use rue_types::TypeId;

use crate::{HirId, ScopeId, Value};

pub type SymbolId = Id<Symbol>;

#[derive(Debug, Clone)]
pub enum Symbol {
    Unresolved,
    Module(ModuleSymbol),
    Function(FunctionSymbol),
    Builtin(Builtin),
    Parameter(ParameterSymbol),
    Constant(ConstantSymbol),
    Binding(BindingSymbol),
}

impl Symbol {
    pub fn name(&self) -> Option<&SyntaxToken> {
        match self {
            Self::Unresolved => None,
            Self::Module(module) => module.name.as_ref(),
            Self::Function(function) => function.name.as_ref(),
            Self::Builtin(_) => None,
            Self::Parameter(parameter) => parameter.name.as_ref(),
            Self::Constant(constant) => constant.name.as_ref(),
            Self::Binding(binding) => binding.name.as_ref(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleSymbol {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub declarations: ModuleDeclarations,
}

#[derive(Debug, Default, Clone)]
pub struct ModuleDeclarations {
    pub types: Vec<(TypeId, ScopeId)>,
    pub symbols: Vec<SymbolId>,
    pub modules: Vec<SymbolId>,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub parameters: IndexMap<String, SymbolId>,
    pub nil_terminated: bool,
    pub return_type: TypeId,
    pub body: HirId,
    pub kind: FunctionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionKind {
    BinaryTree,
    Sequential,
    Inline,
}

#[derive(Debug, Clone)]
pub struct ParameterSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct ConstantSymbol {
    pub name: Option<SyntaxToken>,
    pub value: Value,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<SyntaxToken>,
    pub value: Value,
    pub inline: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Sha256 { inline: bool },
    Keccak256 { inline: bool },
    Concat,
    CoinId,
    Substr,
    Sum,
    Difference,
    Product,
    Divmod,
    Modpow,
    Any,
    All,
    PubkeyForExp,
    G1Sum,
    G1Difference,
    G2Sum,
    G2Difference,
    G1Map,
    G2Map,
    BlsPairingIdentity,
    BlsVerify,
    Secp256K1Verify,
    Secp256R1Verify,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Sha256 { inline } =>
                    if *inline {
                        "sha256_inline"
                    } else {
                        "sha256"
                    },
                Self::Keccak256 { inline } =>
                    if *inline {
                        "keccak256_inline"
                    } else {
                        "keccak256"
                    },
                Self::Concat => "concat",
                Self::CoinId => "coinid",
                Self::Substr => "substr",
                Self::Sum => "sum",
                Self::Difference => "difference",
                Self::Product => "product",
                Self::Divmod => "divmod",
                Self::Modpow => "modpow",
                Self::Any => "any",
                Self::All => "all",
                Self::PubkeyForExp => "pubkey_for_exp",
                Self::G1Sum => "g1_sum",
                Self::G1Difference => "g1_difference",
                Self::G2Sum => "g2_sum",
                Self::G2Difference => "g2_difference",
                Self::G1Map => "g1_map",
                Self::G2Map => "g2_map",
                Self::BlsPairingIdentity => "bls_pairing_identity",
                Self::BlsVerify => "bls_verify",
                Self::Secp256K1Verify => "secp256k1_verify",
                Self::Secp256R1Verify => "secp256r1_verify",
            }
        )
    }
}
