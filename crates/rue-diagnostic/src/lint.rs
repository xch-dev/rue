use std::{fmt, str::FromStr};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::DiagnosticSeverity;

/// How a lint is reported when it's triggered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LintLevel {
    /// The lint is not reported.
    Allow,
    /// The lint is reported as a warning.
    Warn,
    /// The lint is reported as an error, which causes compilation to fail.
    Deny,
}

impl LintLevel {
    /// The severity of diagnostics reported at this level, or `None` if they aren't reported.
    pub fn severity(self) -> Option<DiagnosticSeverity> {
        match self {
            Self::Allow => None,
            Self::Warn => Some(DiagnosticSeverity::Warning),
            Self::Deny => Some(DiagnosticSeverity::Error),
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Deny => "deny",
        }
    }
}

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("Unknown lint level `{0}`, expected `allow`, `warn` or `deny`")]
pub struct UnknownLintLevel(pub String);

impl FromStr for LintLevel {
    type Err = UnknownLintLevel;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "allow" => Ok(Self::Allow),
            "warn" => Ok(Self::Warn),
            "deny" => Ok(Self::Deny),
            _ => Err(UnknownLintLevel(s.to_string())),
        }
    }
}

macro_rules! lints {
    ($( $variant:ident($name:literal, $level:ident, $description:literal), )*) => {
        /// The registry of lints. Each lint has a stable name, a default level and a description.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Lint {
            $( $variant, )*
        }

        impl Lint {
            /// Every lint in the registry.
            pub const ALL: &[Self] = &[$( Self::$variant, )*];

            /// The stable name used to configure the lint.
            pub fn name(self) -> &'static str {
                match self {
                    $( Self::$variant => $name, )*
                }
            }

            /// The level the lint is reported at if it isn't configured.
            pub fn default_level(self) -> LintLevel {
                match self {
                    $( Self::$variant => LintLevel::$level, )*
                }
            }

            /// A short description of what the lint reports.
            pub fn description(self) -> &'static str {
                match self {
                    $( Self::$variant => $description, )*
                }
            }
        }
    };
}

lints! {
    UnusedFunction("unused_function", Warn, "Functions that are never used"),
    UnusedConstant("unused_constant", Warn, "Constants that are never used"),
    UnusedBinding("unused_binding", Warn, "Let bindings that are never used"),
    UnusedParameter("unused_parameter", Warn, "Function parameters that are never used"),
    UnusedStruct("unused_struct", Warn, "Structs that are never used"),
    UnusedTypeAlias("unused_type_alias", Warn, "Type aliases that are never used"),
    UnusedGenericType("unused_generic_type", Warn, "Generic type parameters that are never used"),
    UnusedImport("unused_import", Warn, "Imports that are never used"),
    UnusedValue("unused_value", Warn, "Values returned by expression statements that are discarded"),
    EmptyGenerics("empty_generics", Warn, "Empty generic parameter or argument lists"),
    UnnecessaryCast("unnecessary_cast", Warn, "Casts between types that are already compatible"),
    UnnecessaryGuard("unnecessary_guard", Warn, "Type guards between types that are already compatible"),
    IndistinguishableGuard("indistinguishable_guard", Warn, "Type guards that are always true at runtime"),
    UnnecessaryPlus("unnecessary_plus", Warn, "Unary `+` operators, which have no effect"),
    ConstantCondition("constant_condition", Warn, "Conditions that always evaluate to the same value"),
    DoubleNegation("double_negation", Warn, "Use of `--`, which could be confused with a decrement operator"),
    InlineIfMayRaise("inline_if_may_raise", Warn, "`inline if` branches that can raise, even though both are always evaluated"),
}

impl Lint {
    /// The number of lints in the registry.
    pub const COUNT: usize = Self::ALL.len();

    /// Looks up a lint by its stable name.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::ALL.iter().copied().find(|lint| lint.name() == name)
    }

    fn index(self) -> usize {
        self as usize
    }
}

impl fmt::Display for Lint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("Unknown lint `{0}`")]
pub struct UnknownLint(pub String);

impl FromStr for Lint {
    type Err = UnknownLint;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_name(s).ok_or_else(|| UnknownLint(s.to_string()))
    }
}

/// The configured level of every lint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintLevels([LintLevel; Lint::COUNT]);

impl Default for LintLevels {
    fn default() -> Self {
        let mut levels = [LintLevel::Allow; Lint::COUNT];

        for &lint in Lint::ALL {
            levels[lint.index()] = lint.default_level();
        }

        Self(levels)
    }
}

impl LintLevels {
    pub fn get(&self, lint: Lint) -> LintLevel {
        self.0[lint.index()]
    }

    pub fn set(&mut self, lint: Lint, level: LintLevel) {
        self.0[lint.index()] = level;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::DiagnosticKind;

    #[test]
    fn names_are_unique_and_round_trip() {
        let mut names = HashSet::new();

        for &lint in Lint::ALL {
            assert!(names.insert(lint.name()), "duplicate lint `{lint}`");
            assert_eq!(Lint::from_name(lint.name()), Some(lint));
            assert!(
                lint.name()
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || c == '_'),
                "lint `{lint}` must be snake_case"
            );
            assert!(!lint.description().is_empty());
        }

        assert_eq!(Lint::from_name("not_a_lint"), None);
        assert!("not_a_lint".parse::<Lint>().is_err());
    }

    #[test]
    fn registry_is_in_declaration_order() {
        for (index, &lint) in Lint::ALL.iter().enumerate() {
            assert_eq!(lint.index(), index);
        }
    }

    #[test]
    fn levels_default_and_override() {
        let mut levels = LintLevels::default();

        for &lint in Lint::ALL {
            assert_eq!(levels.get(lint), lint.default_level());
        }

        levels.set(Lint::UnusedBinding, LintLevel::Deny);
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Deny);
        assert_eq!(
            levels.get(Lint::UnusedFunction),
            Lint::UnusedFunction.default_level()
        );
    }

    #[test]
    fn level_severity() {
        assert_eq!(LintLevel::Allow.severity(), None);
        assert_eq!(
            LintLevel::Warn.severity(),
            Some(DiagnosticSeverity::Warning)
        );
        assert_eq!(LintLevel::Deny.severity(), Some(DiagnosticSeverity::Error));
    }

    #[test]
    fn diagnostic_severity_follows_level() {
        let kind = DiagnosticKind::UnusedBinding("x".to_string());
        assert_eq!(kind.lint(), Some(Lint::UnusedBinding));

        let mut levels = LintLevels::default();

        for (level, severity) in [
            (LintLevel::Allow, None),
            (LintLevel::Warn, Some(DiagnosticSeverity::Warning)),
            (LintLevel::Deny, Some(DiagnosticSeverity::Error)),
        ] {
            levels.set(Lint::UnusedBinding, level);
            assert_eq!(kind.severity(&levels), severity);
        }

        // Diagnostics that aren't lints are errors regardless of the configured levels
        assert_eq!(DiagnosticKind::MissingReturn.lint(), None);
        assert_eq!(
            DiagnosticKind::MissingReturn.severity(&levels),
            Some(DiagnosticSeverity::Error)
        );
    }

    #[test]
    fn level_parsing() {
        for level in [LintLevel::Allow, LintLevel::Warn, LintLevel::Deny] {
            assert_eq!(level.name().parse::<LintLevel>(), Ok(level));
        }

        assert!("forbid".parse::<LintLevel>().is_err());
    }
}
