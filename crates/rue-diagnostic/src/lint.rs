use std::{fmt, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize, de};
use thiserror::Error;

use crate::{DiagnosticKind, DiagnosticSeverity};

/// How a lint is reported when it's triggered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum LintLevel {
    /// The lint is not reported.
    Allow,
    /// The lint is reported as a warning.
    Warn,
    /// The lint is reported as an error, which causes compilation to fail.
    Deny,
    /// The lint is reported as an error like [`Deny`](Self::Deny), but it can't be set to any
    /// other level afterwards, such as by an attribute.
    Forbid,
}

impl LintLevel {
    /// The severity of diagnostics reported at this level, or `None` if they aren't reported.
    pub fn severity(self) -> Option<DiagnosticSeverity> {
        match self {
            Self::Allow => None,
            Self::Warn => Some(DiagnosticSeverity::Warning),
            Self::Deny | Self::Forbid => Some(DiagnosticSeverity::Error),
        }
    }

    /// Whether a lint at this level can be set to the given level. A forbidden lint can't be set
    /// to anything other than `forbid`.
    pub fn can_change_to(self, level: LintLevel) -> bool {
        self != Self::Forbid || level == Self::Forbid
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Deny => "deny",
            Self::Forbid => "forbid",
        }
    }
}

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.name())
    }
}

/// The level set by a lint attribute, which is either a [`LintLevel`] or `expect`. An `expect`
/// attribute allows the lint, but expects it to be reported inside of what it's attached to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttributeLevel {
    Level(LintLevel),
    Expect,
}

impl AttributeLevel {
    /// The level that lints are reported at inside of the attribute.
    pub fn lint_level(self) -> LintLevel {
        match self {
            Self::Level(level) => level,
            Self::Expect => LintLevel::Allow,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Level(level) => level.name(),
            Self::Expect => "expect",
        }
    }
}

impl From<LintLevel> for AttributeLevel {
    fn from(level: LintLevel) -> Self {
        Self::Level(level)
    }
}

impl fmt::Display for AttributeLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.name())
    }
}

impl FromStr for AttributeLevel {
    type Err = UnknownLintLevel;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "expect" => Ok(Self::Expect),
            _ => s.parse().map(Self::Level),
        }
    }
}

/// Where a lint was forbidden, so that attempts to change its level can explain why they failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ForbiddenBy {
    /// The project's lint levels, which come from `Rue.toml` and the command line.
    Project,
    /// The `[lints]` section of `Rue.toml`.
    Manifest,
    /// An earlier `-F`/`--forbid` flag on the command line.
    Flag,
    /// A `#[forbid(...)]` attribute on the given line, numbered from 1.
    Attribute { line: usize },
}

impl fmt::Display for ForbiddenBy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Project => f.write_str("the project"),
            Self::Manifest => f.write_str("`Rue.toml`"),
            Self::Flag => f.write_str("an earlier `--forbid` flag"),
            Self::Attribute { line } => write!(f, "the attribute on line {line}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("Unknown lint level `{0}`, expected `allow`, `warn`, `deny` or `forbid`")]
pub struct UnknownLintLevel(pub String);

impl FromStr for LintLevel {
    type Err = UnknownLintLevel;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "allow" => Ok(Self::Allow),
            "warn" => Ok(Self::Warn),
            "deny" => Ok(Self::Deny),
            "forbid" => Ok(Self::Forbid),
            _ => Err(UnknownLintLevel(s.to_string())),
        }
    }
}

// Levels are parsed the same way everywhere, so that invalid ones have the same message
impl<'de> Deserialize<'de> for LintLevel {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct LevelVisitor;

        impl de::Visitor<'_> for LevelVisitor {
            type Value = LintLevel;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a lint level (`allow`, `warn`, `deny` or `forbid`)")
            }

            fn visit_str<E: de::Error>(self, value: &str) -> Result<LintLevel, E> {
                value.parse().map_err(E::custom)
            }
        }

        deserializer.deserialize_str(LevelVisitor)
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
    UnusedStructField("unused_struct_field", Warn, "Struct fields that are never read, in structs whose values are only ever read by field"),
    UnusedExport("unused_export", Warn, "Exported items of a module that are only used inside of it"),
    EmptyGenerics("empty_generics", Warn, "Empty generic parameter or argument lists"),
    UnnecessaryCast("unnecessary_cast", Warn, "Casts between types that are already compatible"),
    UnnecessaryGuard("unnecessary_guard", Warn, "Type guards between types that are already compatible"),
    IndistinguishableGuard("indistinguishable_guard", Warn, "Type guards that are always true at runtime"),
    UnnecessaryPlus("unnecessary_plus", Warn, "Unary `+` operators, which have no effect"),
    UnnecessaryReturn("unnecessary_return", Warn, "`return` statements directly in a function body, which can end in an expression instead"),
    ConstantCondition("constant_condition", Warn, "Conditions that always evaluate to the same value"),
    DoubleNegation("double_negation", Warn, "Use of `--`, which could be confused with a decrement operator"),
    UnreachableCode("unreachable_code", Warn, "Code after a `return`, `raise` or other expression that never finishes, which is never evaluated"),
    InlineIfMayRaise("inline_if_may_raise", Warn, "`inline if` branches that can raise, even though both are always evaluated"),
    SelfComparison("self_comparison", Warn, "Comparisons and operations between an expression and itself, which always have the same result"),
    IdenticalBranches("identical_branches", Warn, "`if` expressions whose branches are the same, so the condition has no effect"),
    DuplicateCondition("duplicate_condition", Warn, "Conditions in an `else if` chain, `&&` or `||` that repeat an earlier one"),
    BoolComparison("bool_comparison", Warn, "Comparisons with `true` or `false`, which can use the value or its negation instead"),
    NeedlessBool("needless_bool", Warn, "`if` expressions that return `true` and `false`, which can use the condition instead"),
    NegatedCondition("negated_condition", Allow, "`if` expressions with a negated condition and an `else` branch, which can swap the branches instead"),
    CollapsibleIf("collapsible_if", Warn, "`else` blocks that only contain an `if` expression, which can be written as `else if`"),
    ManualAssert("manual_assert", Warn, "`if` statements that only `raise`, which can be written as an `assert`"),
    NeedlessLetReturn("needless_let_return", Warn, "Blocks that end by returning a binding declared directly before, which can end in its value instead"),
    RedundantFieldNames("redundant_field_names", Warn, "Struct fields written as `name: name`, which can use the shorthand `name` instead"),
    UnnecessaryParens("unnecessary_parens", Warn, "Parentheses that don't change how an expression is evaluated"),
    NamingConvention("naming_convention", Warn, "Names that don't follow the naming convention for what they name"),
    ShadowedBinding("shadowed_binding", Allow, "Bindings and parameters that shadow another binding or parameter with the same name"),
    DebugStatement("debug_statement", Allow, "`debug` statements, which shouldn't be left in a release build"),
    UnconditionalRecursion("unconditional_recursion", Warn, "Functions that call themselves on every path, so they never return"),
    RedundantInline("redundant_inline", Warn, "`inline` constants and bindings that are only used once, which are already inlined"),
    LargeInline("large_inline", Warn, "`inline` functions whose body is duplicated in enough places that calling them instead would make the program cheaper"),
    RepeatedExpensiveCall("repeated_expensive_call", Warn, "Calls that are evaluated more than once with the same arguments, where reusing the result would make the program cheaper"),
    NonExhaustiveUnionGuard("non_exhaustive_union_guard", Warn, "Chains of type guards on the same value that end in a `raise`, when the value can still be another type of its union there"),
    BoolBitwiseOperator("bool_bitwise_operator", Warn, "`&` and `|` on `Bool` values whose right side can raise or is expensive, which `&&` and `||` would only evaluate when needed"),
    BytesIntConfusion("bytes_int_confusion", Warn, "Casts of byte literals to `Int` whose value is negative or not in canonical form"),
    MissingEntrypointReturnType("missing_entrypoint_return_type", Warn, "Entrypoints (`main` and exported functions) whose return type accepts any value, so what they output isn't checked"),
    UnknownLint("unknown_lint", Warn, "Lint names in attributes or `Rue.toml` that don't refer to a known lint or group"),
    UnfulfilledLintExpectation("unfulfilled_lint_expectation", Warn, "`expect` attributes whose lint is never reported inside of them"),
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
        f.pad(self.name())
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

macro_rules! lint_groups {
    ($( $variant:ident($name:literal, $description:literal, [$( $lint:ident ),* $(,)?]), )*) => {
        /// Named groups of lints, which can be configured anywhere a lint can.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum LintGroup {
            $( $variant, )*
        }

        impl LintGroup {
            /// Every lint group in the registry.
            pub const ALL: &[Self] = &[$( Self::$variant, )*];

            /// The stable name used to configure the group, which can't be the name of a lint.
            pub fn name(self) -> &'static str {
                match self {
                    $( Self::$variant => $name, )*
                }
            }

            /// A short description of the lints in the group.
            pub fn description(self) -> &'static str {
                match self {
                    $( Self::$variant => $description, )*
                }
            }

            /// The lints in the group.
            pub fn lints(self) -> &'static [Lint] {
                match self {
                    $( Self::$variant => &[$( Lint::$lint, )*], )*
                }
            }
        }
    };
}

lint_groups! {
    Unused("unused", "Code that is never used", [
        UnusedFunction,
        UnusedConstant,
        UnusedBinding,
        UnusedParameter,
        UnusedStruct,
        UnusedTypeAlias,
        UnusedGenericType,
        UnusedImport,
        UnusedValue,
        UnusedStructField,
        UnusedExport,
    ]),
    Style("style", "Code that works, but could be written more simply or idiomatically", [
        EmptyGenerics,
        UnnecessaryCast,
        UnnecessaryGuard,
        UnnecessaryPlus,
        UnnecessaryReturn,
        BoolComparison,
        NeedlessBool,
        NegatedCondition,
        CollapsibleIf,
        ManualAssert,
        NeedlessLetReturn,
        RedundantFieldNames,
        UnnecessaryParens,
        NamingConvention,
        ShadowedBinding,
        MissingEntrypointReturnType,
    ]),
    Suspicious("suspicious", "Code that's likely to be a mistake", [
        IndistinguishableGuard,
        ConstantCondition,
        DoubleNegation,
        UnreachableCode,
        InlineIfMayRaise,
        SelfComparison,
        IdenticalBranches,
        DuplicateCondition,
        UnconditionalRecursion,
        BytesIntConfusion,
        BoolBitwiseOperator,
        NonExhaustiveUnionGuard,
    ]),
    Cost("cost", "Code that costs more to run or is larger than it needs to be", [
        RedundantInline,
        LargeInline,
        RepeatedExpensiveCall,
    ]),
}

impl LintGroup {
    /// Looks up a lint group by its stable name.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::ALL.iter().copied().find(|group| group.name() == name)
    }
}

impl fmt::Display for LintGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.name())
    }
}

/// A name that can be used to configure lints, which is either a single lint or a lint group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LintName {
    Lint(Lint),
    Group(LintGroup),
}

impl LintName {
    /// Looks up a lint or lint group by its stable name.
    pub fn from_name(name: &str) -> Option<Self> {
        Lint::from_name(name)
            .map(Self::Lint)
            .or_else(|| LintGroup::from_name(name).map(Self::Group))
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Lint(lint) => lint.name(),
            Self::Group(group) => group.name(),
        }
    }

    /// The lints that this name refers to.
    pub fn lints(self) -> &'static [Lint] {
        match self {
            Self::Lint(lint) => std::slice::from_ref(&Lint::ALL[lint.index()]),
            Self::Group(group) => group.lints(),
        }
    }
}

impl fmt::Display for LintName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.name())
    }
}

impl FromStr for LintName {
    type Err = UnknownLint;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_name(s).ok_or_else(|| UnknownLint(s.to_string()))
    }
}

impl LintName {
    /// The diagnostic for trying to set this lint or group to a level, when the given lint (which
    /// it refers to) is forbidden. A group is reported once, rather than once for each lint in it.
    pub fn forbidden_override(
        self,
        lint: Lint,
        level: impl Into<AttributeLevel>,
        forbidden_by: ForbiddenBy,
    ) -> DiagnosticKind {
        match self {
            Self::Lint(_) => DiagnosticKind::ForbiddenLintOverride {
                lint,
                level: level.into(),
                forbidden_by,
            },
            Self::Group(group) => DiagnosticKind::ForbiddenLintGroupOverride {
                group,
                lint,
                level: level.into(),
                forbidden_by,
            },
        }
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

    /// Sets the level of a lint, even if it's forbidden. Use [`try_set`](Self::try_set) for
    /// overrides that must respect `forbid`.
    pub fn set(&mut self, lint: Lint, level: LintLevel) {
        self.0[lint.index()] = level;
    }

    /// Sets the level of a lint, unless it's forbidden and the new level isn't `forbid`. Returns
    /// whether the level was set.
    #[must_use]
    pub fn try_set(&mut self, lint: Lint, level: LintLevel) -> bool {
        let allowed = self.get(lint).can_change_to(level);

        if allowed {
            self.set(lint, level);
        }

        allowed
    }

    /// Sets the level of each lint or group, such as from the `[lints]` section of `Rue.toml`. A
    /// lint's own level takes precedence over the level of any group it's in, regardless of the
    /// order they're given in, unless the group forbids it. Otherwise, the last level wins.
    ///
    /// Overrides that try to change the level of a forbidden lint are returned, along with the
    /// first lint they couldn't change. The other lints in a group are still set.
    pub fn apply(
        &mut self,
        overrides: &[(LintName, LintLevel)],
    ) -> Vec<(LintName, Lint, LintLevel)> {
        let groups = overrides
            .iter()
            .filter(|(name, _)| matches!(name, LintName::Group(_)));
        let lints = overrides
            .iter()
            .filter(|(name, _)| matches!(name, LintName::Lint(_)));

        let mut rejected = Vec::new();

        for &(name, level) in groups.chain(lints) {
            let mut forbidden = None;

            for &lint in name.lints() {
                if !self.try_set(lint, level) {
                    forbidden.get_or_insert((name, lint, level));
                }
            }

            rejected.extend(forbidden);
        }

        rejected
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

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
    fn group_names_are_unique_and_round_trip() {
        let mut names = Lint::ALL
            .iter()
            .map(|lint| lint.name())
            .collect::<HashSet<_>>();

        for &group in LintGroup::ALL {
            assert!(
                names.insert(group.name()),
                "group `{group}` clashes with another lint or group"
            );
            assert_eq!(LintGroup::from_name(group.name()), Some(group));
            assert_eq!(group.name().parse::<LintName>(), Ok(LintName::Group(group)));
            assert!(!group.description().is_empty());
            assert!(!group.lints().is_empty());
        }

        for &lint in Lint::ALL {
            assert_eq!(lint.name().parse::<LintName>(), Ok(LintName::Lint(lint)));
            assert_eq!(LintName::Lint(lint).lints(), &[lint]);
        }

        assert_eq!(
            "not_a_lint".parse::<LintName>(),
            Err(UnknownLint("not_a_lint".to_string()))
        );
    }

    #[test]
    fn unused_group_has_every_unused_lint() {
        let expected = Lint::ALL
            .iter()
            .copied()
            .filter(|lint| lint.name().starts_with("unused_"))
            .collect::<Vec<_>>();

        assert_eq!(LintGroup::Unused.lints(), expected);
    }

    #[test]
    fn groups_have_the_lints_they_describe() {
        assert_eq!(
            LintGroup::Style.lints(),
            [
                Lint::EmptyGenerics,
                Lint::UnnecessaryCast,
                Lint::UnnecessaryGuard,
                Lint::UnnecessaryPlus,
                Lint::UnnecessaryReturn,
                Lint::BoolComparison,
                Lint::NeedlessBool,
                Lint::NegatedCondition,
                Lint::CollapsibleIf,
                Lint::ManualAssert,
                Lint::NeedlessLetReturn,
                Lint::RedundantFieldNames,
                Lint::UnnecessaryParens,
                Lint::NamingConvention,
                Lint::ShadowedBinding,
                Lint::MissingEntrypointReturnType,
            ]
        );
        assert_eq!(
            LintGroup::Suspicious.lints(),
            [
                Lint::IndistinguishableGuard,
                Lint::ConstantCondition,
                Lint::DoubleNegation,
                Lint::UnreachableCode,
                Lint::InlineIfMayRaise,
                Lint::SelfComparison,
                Lint::IdenticalBranches,
                Lint::DuplicateCondition,
                Lint::UnconditionalRecursion,
                Lint::BytesIntConfusion,
                Lint::BoolBitwiseOperator,
                Lint::NonExhaustiveUnionGuard,
            ]
        );
        assert_eq!(
            LintGroup::Cost.lints(),
            [
                Lint::RedundantInline,
                Lint::LargeInline,
                Lint::RepeatedExpensiveCall,
            ]
        );
    }

    #[test]
    fn lints_are_in_at_most_one_group() {
        for &lint in Lint::ALL {
            let groups = LintGroup::ALL
                .iter()
                .filter(|group| group.lints().contains(&lint))
                .collect::<Vec<_>>();

            assert!(
                groups.len() <= 1,
                "lint `{lint}` is in more than one group: {groups:?}"
            );
        }
    }

    #[test]
    fn lints_override_their_groups_in_any_order() {
        for overrides in [
            [
                (LintName::Group(LintGroup::Unused), LintLevel::Deny),
                (LintName::Lint(Lint::UnusedBinding), LintLevel::Allow),
            ],
            [
                (LintName::Lint(Lint::UnusedBinding), LintLevel::Allow),
                (LintName::Group(LintGroup::Unused), LintLevel::Deny),
            ],
        ] {
            let mut levels = LintLevels::default();
            levels.apply(&overrides);

            assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
            assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Deny);
            assert_eq!(
                levels.get(Lint::DoubleNegation),
                Lint::DoubleNegation.default_level()
            );
        }

        // Unless the group forbids the lint
        let mut levels = LintLevels::default();
        assert_eq!(
            levels.apply(&[
                (LintName::Lint(Lint::UnusedBinding), LintLevel::Allow),
                (LintName::Group(LintGroup::Unused), LintLevel::Forbid),
                (LintName::Lint(Lint::UnusedImport), LintLevel::Forbid),
            ]),
            vec![(
                LintName::Lint(Lint::UnusedBinding),
                Lint::UnusedBinding,
                LintLevel::Allow
            )]
        );
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);

        // Otherwise, the last level wins
        let mut levels = LintLevels::default();
        levels.apply(&[
            (LintName::Lint(Lint::UnusedBinding), LintLevel::Deny),
            (LintName::Group(LintGroup::Unused), LintLevel::Deny),
            (LintName::Group(LintGroup::Unused), LintLevel::Allow),
            (LintName::Lint(Lint::UnusedBinding), LintLevel::Warn),
        ]);

        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Warn);
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Allow);
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
    fn forbidden_levels_can_only_be_forbidden_again() {
        let mut levels = LintLevels::default();

        assert!(levels.try_set(Lint::UnusedBinding, LintLevel::Forbid));
        assert!(!levels.try_set(Lint::UnusedBinding, LintLevel::Allow));
        assert!(!levels.try_set(Lint::UnusedBinding, LintLevel::Deny));
        assert!(levels.try_set(Lint::UnusedBinding, LintLevel::Forbid));
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);

        assert!(levels.try_set(Lint::UnusedFunction, LintLevel::Allow));
        assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Allow);
    }

    #[test]
    fn level_severity() {
        assert_eq!(LintLevel::Allow.severity(), None);
        assert_eq!(
            LintLevel::Warn.severity(),
            Some(DiagnosticSeverity::Warning)
        );
        assert_eq!(LintLevel::Deny.severity(), Some(DiagnosticSeverity::Error));
        assert_eq!(
            LintLevel::Forbid.severity(),
            Some(DiagnosticSeverity::Error)
        );
    }

    #[test]
    fn lint_diagnostics_carry_their_lint() {
        let kind = DiagnosticKind::Lint {
            lint: Lint::UnusedBinding,
            message: "Unused binding `x`".to_string(),
        };

        assert_eq!(kind.lint(), Some(Lint::UnusedBinding));
        assert_eq!(kind.to_string(), "Unused binding `x`");
        assert_eq!(DiagnosticKind::MissingReturn.lint(), None);
    }

    #[test]
    fn level_parsing() {
        for level in [
            LintLevel::Allow,
            LintLevel::Warn,
            LintLevel::Deny,
            LintLevel::Forbid,
        ] {
            assert_eq!(level.name().parse::<LintLevel>(), Ok(level));
        }

        assert!("expect".parse::<LintLevel>().is_err());
    }

    #[test]
    fn attribute_level_parsing() {
        for level in [
            LintLevel::Allow,
            LintLevel::Warn,
            LintLevel::Deny,
            LintLevel::Forbid,
        ] {
            let attribute_level = AttributeLevel::from(level);
            assert_eq!(attribute_level.name().parse(), Ok(attribute_level));
            assert_eq!(attribute_level.lint_level(), level);
        }

        assert_eq!("expect".parse(), Ok(AttributeLevel::Expect));
        assert_eq!(AttributeLevel::Expect.lint_level(), LintLevel::Allow);
        assert_eq!(AttributeLevel::Expect.to_string(), "expect");
        assert!("forbidden".parse::<AttributeLevel>().is_err());
    }

    #[test]
    fn display_respects_width() {
        assert_eq!(format!("{:5}|", LintLevel::Warn), "warn |");
        assert_eq!(format!("{:16}|", Lint::UnusedValue), "unused_value    |");
    }
}
