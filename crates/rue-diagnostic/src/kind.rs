use thiserror::Error;

use crate::DiagnosticSeverity;

#[derive(Debug, Clone, Error)]
pub enum DiagnosticKind {
    #[error("Unknown `{0}`")]
    UnknownToken(String),

    #[error("Expected {}, found {}", list_of(.1), .0)]
    UnexpectedToken(String, Vec<String>),

    #[error("Unterminated block comment")]
    UnterminatedBlockComment,

    #[error("Unterminated string literal")]
    UnterminatedString,

    #[error("Unterminated hex literal")]
    UnterminatedHex,

    #[error("Duplicate symbol `{0}` found in scope")]
    DuplicateSymbol(String),

    #[error("Duplicate type `{0}` found in scope")]
    DuplicateType(String),

    #[error("Undeclared symbol `{0}`")]
    UndeclaredSymbol(String),

    #[error("Undeclared type `{0}`")]
    UndeclaredType(String),

    #[error("Unnecessary empty generic parameter list specified")]
    EmptyGenericParameters,

    #[error("Unnecessary empty generic argument list specified")]
    EmptyGenericArguments,

    #[error("Unnecessary empty subtype fields specified")]
    EmptySubtypeFields,

    #[error("Duplicate field `{0}` specified")]
    DuplicateField(String),

    #[error("Undeclared field `{0}` used in generic parameter")]
    UndeclaredSubtypeField(String),

    #[error("Expected {0} generic arguments, but found {1}")]
    ExpectedGenericArguments(usize, usize),

    #[error("Cannot assign `{0}` to `{1}`, since they are incompatible")]
    IncompatibleType(String, String),

    #[error("Cannot assign `{0}` to `{1}` without a cast, since they are semantically different")]
    UnassignableType(String, String),

    #[error("Cannot cast `{0}` to `{1}`, since they have different runtime representations")]
    IncompatibleCast(String, String),

    #[error("Unnecessary cast from `{0}` to `{1}`, since they are compatible with each other")]
    UnnecessaryCast(String, String),

    #[error("Cannot type guard from `{0}` to `{1}`, since they do not overlap")]
    IncompatibleGuard(String, String),

    #[error("Unnecessary type guard from `{0}` to `{1}`, since they are already compatible types")]
    UnnecessaryGuard(String, String),

    #[error(
        "Cannot compare `{0}` to `{1}` without a type guard, since the runtime value must be constrained"
    )]
    UnconstrainableComparison(String, String),

    #[error("Recursion depth exceeded while checking type")]
    TypeCheckDepthExceeded,

    #[error("Cannot check if value is of function type at runtime")]
    FunctionTypeCheck,

    #[error("Cannot use `{0}` operator with `{1}` and `{2}`")]
    IncompatibleBinaryOp(String, String, String),

    #[error("Cannot use `{0}` operator with `{1}`")]
    IncompatibleUnaryOp(String, String),

    #[error("Value returned by statement is unused")]
    UnusedStatementValue,

    #[error("Let bindings must have a value, since they cannot be reassigned")]
    MissingLetValue,

    #[error("Expected statement block to end in `return` or `raise`, not an expression")]
    UnexpectedImplicitReturn,

    #[error("Unnecessary `return` statement, end the function with an expression instead")]
    UnnecessaryExplicitReturn,

    #[error("Block does not return a value")]
    MissingReturn,

    #[error("Unknown field `{0}` on type `{1}`")]
    UnknownField(String, String),

    #[error("Unnecessary `+` operator, since it has no effect")]
    UnnecessaryPlus,

    #[error("Path separator `::` is not allowed in the first segment")]
    PathSeparatorInFirstSegment,

    #[error("Cannot subpath non-module symbol `{0}`")]
    SubpathNotSupported(String),

    #[error("Cannot reference private symbol `{0}` in module `{1}`")]
    PrivateSymbol(String, String),

    #[error("Cannot reference private type `{0}` in module `{1}`")]
    PrivateType(String, String),

    #[error("Expected symbol, but found type `{0}`")]
    ExpectedSymbol(String),

    #[error("Expected type, but found symbol `{0}`")]
    ExpectedType(String),

    #[error("Generic arguments are not permitted on symbol references")]
    GenericArgumentsOnSymbolReference,

    #[error("Cannot initialize non-struct type `{0}`")]
    NonStructInitializer(String),

    #[error("Struct `{0}` is missing required fields: {1}")]
    MissingRequiredFields(String, String),

    #[error("Type of parameter cannot be inferred, it must be specified explicitly")]
    CannotInferParameterType,

    #[error("Cannot disambiguate between multiple functions in type `{0}`")]
    CannotDisambiguateFunctionTypes(String),

    #[error("Cannot call non-function value with type `{0}`")]
    InvalidFunctionCall(String),

    #[error("Expected {0} arguments, but found {1}")]
    ExpectedArguments(usize, usize),

    #[error("Expected between {0} and {1} arguments, but found {1}")]
    ExpectedArgumentsBetween(usize, usize, usize),

    #[error("Expected an even number of arguments")]
    ExpectedEvenArguments,

    #[error("Expected one argument followed by an even number of additional arguments")]
    ExpectedOneArgumentEvenAdditional,

    #[error("Cannot destructure non-pair parameter with type `{0}`")]
    CannotDestructureParameter(String),

    #[error("Can only spread the last element in a list")]
    NonFinalSpread,

    #[error("Spread operator must be used on functions with spread parameters")]
    InvalidSpread,

    #[error("Spread operator cannot be used on this builtin")]
    InvalidSpreadBuiltin,

    #[error("Field `{0}` exists on struct `{1}`, but it's not present in the underlying type")]
    MissingField(String, String),

    #[error("Unused function `{0}`")]
    UnusedFunction(String),

    #[error("Unused constant `{0}`")]
    UnusedConstant(String),

    #[error("Unused binding `{0}`")]
    UnusedBinding(String),

    #[error("Unused parameter `{0}`")]
    UnusedParameter(String),

    #[error("Unused struct `{0}`")]
    UnusedStruct(String),

    #[error("Unused type alias `{0}`")]
    UnusedTypeAlias(String),

    #[error("Unused generic type `{0}`")]
    UnusedGenericType(String),

    #[error("Condition always evaluates to `false`")]
    AlwaysFalseCondition,

    #[error("Condition always evaluates to `true`")]
    AlwaysTrueCondition,

    #[error("Cannot destructure type `{0}` into a pair")]
    CannotDestructurePair(String),

    #[error("Entrypoint `{0}` references itself")]
    RecursiveEntrypoint(String),
}

impl DiagnosticKind {
    pub fn severity(&self) -> DiagnosticSeverity {
        match self {
            Self::UnknownToken(..)
            | Self::UnexpectedToken(..)
            | Self::UnterminatedBlockComment
            | Self::UnterminatedString
            | Self::UnterminatedHex
            | Self::DuplicateSymbol(..)
            | Self::DuplicateType(..)
            | Self::UndeclaredSymbol(..)
            | Self::UndeclaredType(..)
            | Self::ExpectedGenericArguments(..)
            | Self::ExpectedArgumentsBetween(..)
            | Self::ExpectedEvenArguments
            | Self::ExpectedOneArgumentEvenAdditional
            | Self::DuplicateField(..)
            | Self::UndeclaredSubtypeField(..)
            | Self::IncompatibleType(..)
            | Self::UnassignableType(..)
            | Self::IncompatibleCast(..)
            | Self::IncompatibleGuard(..)
            | Self::UnconstrainableComparison(..)
            | Self::TypeCheckDepthExceeded
            | Self::FunctionTypeCheck
            | Self::IncompatibleBinaryOp(..)
            | Self::IncompatibleUnaryOp(..)
            | Self::MissingLetValue
            | Self::UnexpectedImplicitReturn
            | Self::MissingReturn
            | Self::UnknownField(..)
            | Self::PathSeparatorInFirstSegment
            | Self::SubpathNotSupported(..)
            | Self::PrivateSymbol(..)
            | Self::PrivateType(..)
            | Self::ExpectedSymbol(..)
            | Self::ExpectedType(..)
            | Self::GenericArgumentsOnSymbolReference
            | Self::NonStructInitializer(..)
            | Self::MissingRequiredFields(..)
            | Self::CannotInferParameterType
            | Self::CannotDisambiguateFunctionTypes(..)
            | Self::InvalidFunctionCall(..)
            | Self::ExpectedArguments(..)
            | Self::CannotDestructureParameter(..)
            | Self::NonFinalSpread
            | Self::InvalidSpread
            | Self::InvalidSpreadBuiltin
            | Self::MissingField(..)
            | Self::CannotDestructurePair(..)
            | Self::RecursiveEntrypoint(..) => DiagnosticSeverity::Error,
            Self::EmptyGenericParameters
            | Self::EmptyGenericArguments
            | Self::EmptySubtypeFields
            | Self::UnnecessaryCast(..)
            | Self::UnnecessaryGuard(..)
            | Self::UnnecessaryExplicitReturn
            | Self::UnnecessaryPlus
            | Self::UnusedFunction(..)
            | Self::UnusedConstant(..)
            | Self::UnusedBinding(..)
            | Self::UnusedParameter(..)
            | Self::UnusedStruct(..)
            | Self::UnusedTypeAlias(..)
            | Self::UnusedGenericType(..)
            | Self::AlwaysFalseCondition
            | Self::AlwaysTrueCondition
            | Self::UnusedStatementValue => DiagnosticSeverity::Warning,
        }
    }
}

fn list_of(kinds: &[String]) -> String {
    match kinds.len() {
        0 => "nothing".to_string(),
        1 => kinds[0].clone(),
        _ => format!(
            "one of {}",
            kinds
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}
