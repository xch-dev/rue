use std::fmt;

use id_arena::Id;
use indexmap::IndexMap;
use rue_diagnostic::Name;
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
    pub fn name(&self) -> Option<Name> {
        match self {
            Self::Unresolved => None,
            Self::Module(module) => module.name.clone(),
            Self::Function(function) => function.name.clone(),
            Self::Builtin(builtin) => Some(Name::new(builtin.to_string(), None)),
            Self::Parameter(parameter) => parameter.name.clone(),
            Self::Constant(constant) => constant.name.clone(),
            Self::Binding(binding) => binding.name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleSymbol {
    pub name: Option<Name>,
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
    pub name: Option<Name>,
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
    External,
    Inline,
}

#[derive(Debug, Clone)]
pub struct ParameterSymbol {
    pub name: Option<Name>,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct ConstantSymbol {
    pub name: Option<Name>,
    pub value: Value,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<Name>,
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

impl Builtin {
    /// Human readable signatures for each of the ways this builtin can be called, used for
    /// documentation. Builtins are type checked specially, so they don't have a function type.
    pub fn signatures(&self) -> &'static [&'static str] {
        match self {
            Self::Sha256 { inline: false } | Self::Keccak256 { inline: false } => &[
                "(value: Bytes) -> Bytes32",
                "(...values: List<Bytes>) -> Bytes32",
            ],
            Self::Sha256 { inline: true } | Self::Keccak256 { inline: true } => {
                &["(value: Bytes) -> Bytes32"]
            }
            Self::Concat => &["(values: List<Bytes>) -> Bytes"],
            Self::CoinId => {
                &["(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32"]
            }
            Self::Substr => &[
                "(value: Bytes, start: Int) -> Bytes",
                "(value: Bytes, start: Int, end: Int) -> Bytes",
            ],
            Self::Sum | Self::Difference | Self::Product => &["(values: List<Int>) -> Int"],
            Self::Divmod => &["(numerator: Int, denominator: Int) -> (Int, Int)"],
            Self::Modpow => &["(base: Int, exponent: Int, modulus: Int) -> Int"],
            Self::Any | Self::All => &["(values: List<Bool>) -> Bool"],
            Self::PubkeyForExp => &["(exponent: Bytes32) -> PublicKey"],
            Self::G1Sum | Self::G1Difference => &["(values: List<PublicKey>) -> PublicKey"],
            Self::G2Sum | Self::G2Difference => &["(values: List<Signature>) -> Signature"],
            Self::G1Map => &[
                "(data: Bytes) -> PublicKey",
                "(data: Bytes, dst: Bytes) -> PublicKey",
            ],
            Self::G2Map => &[
                "(data: Bytes) -> Signature",
                "(data: Bytes, dst: Bytes) -> Signature",
            ],
            Self::BlsPairingIdentity => &[
                "(g1: PublicKey, g2: Signature, ...) -> nil",
                "(...pairs: AlternatingList<PublicKey, Signature>) -> nil",
            ],
            Self::BlsVerify => &[
                "(signature: Signature, public_key: PublicKey, message: Bytes, ...) -> nil",
                "(signature: Signature, ...pairs: AlternatingList<PublicKey, Bytes>) -> nil",
            ],
            Self::Secp256K1Verify => {
                &["(public_key: K1PublicKey, message_hash: Bytes32, signature: K1Signature) -> nil"]
            }
            Self::Secp256R1Verify => {
                &["(public_key: R1PublicKey, message_hash: Bytes32, signature: R1Signature) -> nil"]
            }
        }
    }
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
