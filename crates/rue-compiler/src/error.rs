use std::{fmt, ops::Range};

use indoc::formatdoc;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    span: Range<usize>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DiagnosticKind {
        &self.kind
    }

    pub fn span(&self) -> &Range<usize> {
        &self.span
    }

    pub fn is_error(&self) -> bool {
        matches!(self.kind, DiagnosticKind::Error(_))
    }

    pub fn is_warning(&self) -> bool {
        matches!(self.kind, DiagnosticKind::Warning(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DiagnosticKind {
    Warning(WarningKind),
    Error(ErrorKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WarningKind {
    UnusedFunction(String),
    UnusedInlineFunction(String),
    UnusedParameter(String),
    UnusedConst(String),
    UnusedInlineConst(String),
    UnusedLet(String),
    UnusedGenericType(String),
    UnusedEnum(String),
    UnusedEnumVariant(String),
    UnusedStruct(String),
    UnusedTypeAlias(String),
    RedundantNullableType(String),
    RedundantTypeCheck(String),
}

impl fmt::Display for WarningKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            Self::UnusedFunction(name) => format!("Unused function `{name}`."),
            Self::UnusedInlineFunction(name) => format!("Unused inline function `{name}`."),
            Self::UnusedParameter(name) => format!("Unused parameter `{name}`."),
            Self::UnusedConst(name) => format!("Unused constant `{name}`."),
            Self::UnusedInlineConst(name) => format!("Unused inline constant `{name}`."),
            Self::UnusedLet(name) => format!("Unused let binding `{name}`."),
            Self::UnusedGenericType(name) => format!("Unused generic type `{name}`."),
            Self::UnusedEnum(name) => format!("Unused enum `{name}`."),
            Self::UnusedEnumVariant(name) => format!("Unused enum variant `{name}`."),
            Self::UnusedStruct(name) => format!("Unused struct `{name}`."),
            Self::UnusedTypeAlias(name) => format!("Unused type alias `{name}`."),
            Self::RedundantNullableType(ty) => {
                format!("This has no effect, since `{ty}` is already a nullable type.")
            }
            Self::RedundantTypeCheck(ty) => format!(
                "It's redundant to guard against `{ty}`, since the value already has that type."
            ),
        };
        write!(f, "{}", message.trim())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    // Duplicate definitions.
    DuplicateType(String),
    DuplicateSymbol(String),
    ModuleNameTakenByEnum(String),
    EnumNameTakenByModule(String),

    // Invalid symbol references.
    UnknownSymbol(String),
    UnknownType(String),
    InlineFunctionReference(String),
    ModuleReference(String),

    // Types.
    RecursiveTypeAlias(String),
    TypeMismatch(String, String),
    CastMismatch(String, String),
    CannotInferType,

    // Function calls.
    UncallableType(String),
    ArgumentMismatch(usize, usize),
    ArgumentMismatchSpread(usize, usize),
    ArgumentMismatchOptional(usize, usize),

    // Field initialization.
    UninitializableType(String),
    InvalidEnumVariantInitializer(String),
    InvalidEnumVariantReference(String),
    DuplicateInitializerField(String),
    UnknownInitializerField(String),
    MissingInitializerFields(Vec<String>),

    // Field access.
    UnknownField(String),
    InvalidFieldAccess(String, String),
    InvalidIndexAccess(String),

    // Spread and optional.
    InvalidSpreadItem,
    InvalidSpreadArgument,
    InvalidSpreadParameter,
    InvalidSpreadField,
    InvalidOptionalParameter,
    InvalidOptionalField,
    OptionalParameterSpread,
    OptionalFieldSpread,
    UnsupportedFunctionSpread,
    RequiredFunctionSpread,

    // Enum variant definitions.
    DuplicateEnumVariant(String),
    DuplicateEnumDiscriminant(String),
    EnumDiscriminantTooLarge,

    // Paths.
    UnknownEnumVariantPath(String),
    UnknownModulePath(String),
    PrivateSymbol(String),
    PrivateType(String),
    InvalidTypePath(String),
    InvalidSymbolPath(Option<String>),
    ExpectedTypePath(String),
    ExpectedSymbolPath(String),

    // Type guards.
    ImpossibleTypeCheck(String, String),
    RecursiveTypeCheck(String, String),

    // Blocks.
    ImplicitReturnInIf,
    ExplicitReturnInExpr,
    EmptyBlock,

    // Atoms.
    NonAtomEquality(String),
    IntegerTooLarge,

    // Recursive constants.
    RecursiveConstantReference,
    RecursiveInlineConstantReference,
    RecursiveInlineFunctionCall,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            // Duplicate definitions.
            Self::DuplicateType(name) => format!("There is already a type named `{name}` in this scope."),
            Self::DuplicateSymbol(name) => format!("There is already a symbol named `{name}` in this scope."),
            Self::ModuleNameTakenByEnum(name) => formatdoc!("
                There is already an enum type named `{name}` in this scope. \
                This isn't allowed to prevent ambiguity when referencing items.
            "),
            Self::EnumNameTakenByModule(name) => formatdoc!("
                There is already a module named `{name}` in this scope. \
                This isn't allowed to prevent ambiguity when referencing items.
            "),

            // Invalid symbol references.
            Self::UnknownSymbol(name) => format!("Reference to unknown symbol `{name}`."),
            Self::UnknownType(name) => format!("Reference to unknown type `{name}`."),
            Self::InlineFunctionReference(name) => formatdoc!("
                Cannot reference inline function `{name}`, since it is not a value. \
                Inline functions must be resolved at compile time. \
                Try calling the function instead.
            "),
            Self::ModuleReference(name) => formatdoc!("
                Cannot reference module `{name}`, since it is not a value. \
                Perhaps you meant to use the `::` operator to access a symbol in the module?
            "),

            // Types.
            Self::RecursiveTypeAlias(name) => formatdoc!("
                Cycle detected when resolving type alias `{name}`. \
                Type aliases cannot reference themselves.
            "),
            Self::TypeMismatch(found, expected) => format!("Expected type `{expected}`, but found `{found}`."),
            Self::CastMismatch(found, expected) => format!("Cannot cast type `{found}` to `{expected}`."),
            Self::CannotInferType => "Lambda parameter type could not be inferred.".to_string(),

            // Function calls.
            Self::UncallableType(ty) => format!("Expression with type `{ty}` cannot be called, since it is not a function."),
            Self::ArgumentMismatch(found, expected) => {
                format!(
                    "Expected {expected} argument{}, but found {found}.",
                    if *expected == 1 { "" } else { "s" }
                )
            }
            Self::ArgumentMismatchSpread (found, expected) => {
                format!(
                    "Expected at least {expected} argument{}, but found {found}.",
                    if *expected == 1 { "" } else { "s" }
                )
            }
            Self::ArgumentMismatchOptional (found, expected)=> {
                format!("Expected either {} or {expected} arguments, but found {found}.", expected - 1)
            }

            // Field initialization.
            Self::UninitializableType(ty) => formatdoc!("
                Cannot initializable type `{ty}`. \
                Only structs and enum variants with fields can be initialized.
            "),
            Self::InvalidEnumVariantReference(name) => formatdoc!("
                Cannot reference enum variant `{name}`. \
                Enum variants with fields cannot be referenced directly. \
                Consider initializing the enum variant instead.
            "),
            Self::InvalidEnumVariantInitializer(name) => formatdoc!("
                Cannot initialize enum variant `{name}`. \
                Enum variants without fields cannot be initialized. \
                Consider referencing the enum variant directly.
            "),
            Self::DuplicateInitializerField(name) => format!("Duplicate field `{name}` specified in initializer."),
            Self::UnknownInitializerField(name) => format!("Unknown field `{name}` specified in initializer."),
            Self::MissingInitializerFields(fields) => format!("Missing fields in initializer: {}.", join_names(fields)),

            // Field access.
            Self::UnknownField(name) => format!("Cannot reference unknown field `{name}`."),
            Self::InvalidFieldAccess(field, ty) => format!("Cannot reference field `{field}` of type `{ty}`."),
            Self::InvalidIndexAccess(ty) => format!("Cannot index into type `{ty}`."),

            // Spread and optional.
            Self::InvalidSpreadItem => formatdoc!("
                The spread operator can only be used on the last item in a list. \
                This is because it requires recursion at runtime to concatenate lists together. \
                By only allowing it on the last item by default, this additional complexity and runtime cost is avoided.
            "),
            Self::InvalidSpreadArgument => formatdoc!("
                The spread operator can only be used on the last argument in a function call. \
                This is because it requires recursion at runtime to concatenate lists together. \
                By only allowing it on the last item by default, this additional complexity and runtime cost is avoided.
            "),
            Self::InvalidSpreadParameter => formatdoc!("
                The spread operator can only be used on the last parameter in a function. \
                Otherwise, it would be ambiguous where the parameter should start and end.
            "),
            Self::InvalidSpreadField => formatdoc!("
                The spread operator can only be used on the last field. \
                Otherwise, it would be ambiguous where the field should start and end.
            "),
            Self::InvalidOptionalParameter => formatdoc!("
                Only the last parameter in a function can be optional. \
                Otherwise, it would be ambiguous which optional parameter was specified.
            "),
            Self::InvalidOptionalField => formatdoc!("
                Only the last field can be optional. \
                Otherwise, it would be ambiguous which optional field was specified.
            "),
            Self::OptionalParameterSpread => "The spread operator cannot be used on optional parameters.".to_string(),
            Self::OptionalFieldSpread => "The spread operator cannot be used on optional fields.".to_string(),
            Self::UnsupportedFunctionSpread => "This function does not support the spread operator on its last argument.".to_string(),
            Self::RequiredFunctionSpread => "This function requires the spread operator on its last argument.".to_string(),

            // Enum variant definitions.
            Self::DuplicateEnumVariant(name) => format!("Duplicate enum variant `{name}` specified."),
            Self::DuplicateEnumDiscriminant(discriminant) => format!("Duplicate enum discriminant `{discriminant}` specified."),
            Self::EnumDiscriminantTooLarge => "Enum discriminant is too large to allocate in CLVM.".to_string(),

            // Paths.
            Self::UnknownEnumVariantPath(name) => format!("Unknown enum variant `{name}`."),
            Self::UnknownModulePath(name) => format!("Could not resolve `{name}` in module."),
            Self::PrivateSymbol(name) => format!("Cannot access private symbol `{name}` in module."),
            Self::PrivateType(name) => format!("Cannot access private type `{name}` in module."),
            Self::InvalidTypePath(ty) => format!("Cannot path into type `{ty}`."),
            Self::InvalidSymbolPath(name) => if let Some(name) = name {
                format!("Cannot path into symbol `{name}`.")
            } else {
                "Cannot path into symbol.".to_string()
            },
            Self::ExpectedTypePath(name) => format!("Expected type, but found symbol `{name}` instead."),
            Self::ExpectedSymbolPath(name) => format!("Expected symbol, but found type `{name}` instead."),

            // Type guards.
            Self::ImpossibleTypeCheck(from, to) => format!("Cannot check type `{from}` against `{to}`."),
            Self::RecursiveTypeCheck(from, to) => format!("Checking type `{from}` against `{to}` would result in infinite recursion at runtime."),

            // Blocks.
            Self::ImplicitReturnInIf => formatdoc!("
                Implicit returns are not allowed in if statements. \
                Either use an explicit return statement at the end of the block, \
                or raise an error.
            "),
            Self::ExplicitReturnInExpr => "Explicit return is not allowed within expressions.".to_string(),
            Self::EmptyBlock => "Blocks must either return an expression or raise an error.".to_string(),

            // Atoms.
            Self::NonAtomEquality(ty) => format!("Cannot check equality on non-atom type `{ty}`."),
            Self::IntegerTooLarge => "Integer literal is too large to allocate in CLVM.".to_string(),

            // Recursive constants.
            Self::RecursiveConstantReference => "Cannot recursively reference constant.".to_string(),
            Self::RecursiveInlineConstantReference => "Cannot recursively reference inline constant.".to_string(),
            Self::RecursiveInlineFunctionCall => "Cannot recursively call inline function.".to_string(),
        };
        write!(f, "{}", message.trim())
    }
}

/// Join a list of names into a string, wrapped in backticks.
fn join_names(kinds: &[String]) -> String {
    let names: Vec<String> = kinds.iter().map(|kind| format!("`{kind}`")).collect();
    names.join(", ")
}
