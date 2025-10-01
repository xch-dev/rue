use id_arena::Id;
use rue_parser::SyntaxToken;
use rue_types::TypeId;

use crate::{HirId, ScopeId};

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
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub parameters: Vec<SymbolId>,
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
    pub ty: TypeId,
    pub value: HirId,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub value: HirId,
    pub inline: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Sha256 { inline: bool },
    Keccak256 { inline: bool },
    CoinId,
    Substr,
    BlsPairingIdentity,
}
