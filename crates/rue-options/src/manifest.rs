use std::{collections::BTreeMap, fmt, ops::Range};

use rue_diagnostic::{
    DiagnosticKind, ForbiddenBy, Lint, LintLevel, LintLevels, LintName, LintOptions,
};
use serde::{Deserialize, Deserializer, Serialize, de};
use toml::Spanned;

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub compiler: CompilerSection,
    #[serde(default)]
    pub formatter: FormatOptions,
    /// Overrides for the level and options of each lint or lint group, keyed by name. The span of
    /// each name is kept so that problems with it can be reported where it's written.
    #[serde(
        default,
        skip_serializing_if = "BTreeMap::is_empty",
        deserialize_with = "deserialize_lints"
    )]
    pub lints: BTreeMap<Spanned<String>, LintConfig>,
}

/// How a lint or group is configured in the `[lints]` section of a manifest, which is either its
/// level (such as `unused = "deny"`), or a table with its level and options (such as
/// `large_inline = { level = "warn", min_savings = 50000 }`), like in Cargo.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum LintConfig {
    Level(LintLevel),
    Table {
        #[serde(skip_serializing_if = "Option::is_none")]
        level: Option<LintLevel>,
        #[serde(flatten)]
        options: BTreeMap<String, u64>,
    },
}

impl LintConfig {
    pub fn level(&self) -> Option<LintLevel> {
        match self {
            Self::Level(level) => Some(*level),
            Self::Table { level, .. } => *level,
        }
    }

    pub fn options(&self) -> impl Iterator<Item = (&str, u64)> {
        let options = match self {
            Self::Level(_) => None,
            Self::Table { options, .. } => Some(options),
        };

        options
            .into_iter()
            .flatten()
            .map(|(option, &value)| (option.as_str(), value))
    }
}

impl<'de> Deserialize<'de> for LintConfig {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ConfigVisitor;

        impl<'de> de::Visitor<'de> for ConfigVisitor {
            type Value = LintConfig;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(
                    "a lint level (`allow`, `warn`, `deny` or `forbid`), or a table with a `level` and options",
                )
            }

            fn visit_str<E: de::Error>(self, value: &str) -> Result<LintConfig, E> {
                value.parse().map(LintConfig::Level).map_err(E::custom)
            }

            fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<LintConfig, A::Error> {
                let mut level = None;
                let mut options = BTreeMap::new();

                while let Some(key) = map.next_key::<String>()? {
                    if key == "level" {
                        level = Some(map.next_value::<LintLevel>()?);
                        continue;
                    }

                    let value = map.next_value::<toml::Value>()?;

                    let Some(value) = value
                        .as_integer()
                        .and_then(|value| u64::try_from(value).ok())
                    else {
                        return Err(de::Error::custom(format!(
                            "Expected lint option `{key}` to be a non-negative integer"
                        )));
                    };

                    options.insert(key, value);
                }

                Ok(LintConfig::Table { level, options })
            }
        }

        deserializer.deserialize_any(ConfigVisitor)
    }
}

/// Parses the `[lints]` section, and checks that each lint's options are ones it has. Unknown
/// names aren't errors, since they're reported as lints instead.
fn deserialize_lints<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<BTreeMap<Spanned<String>, LintConfig>, D::Error> {
    let lints = BTreeMap::<Spanned<String>, LintConfig>::deserialize(deserializer)?;

    for (name, config) in &lints {
        for (option, value) in config.options() {
            match LintName::from_name(name.get_ref()) {
                Some(LintName::Lint(lint)) => {
                    LintOptions::default()
                        .set(lint, option, value)
                        .map_err(de::Error::custom)?;
                }
                Some(LintName::Group(group)) => {
                    return Err(de::Error::custom(format!(
                        "Lint group `{group}` can't have options, but the lints in it can"
                    )));
                }
                None => {}
            }
        }
    }

    Ok(lints)
}

/// A name in the `[lints]` section of a manifest that doesn't refer to a known lint or group.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownLint {
    pub name: String,
    /// The span of the name in the manifest, including any quotes.
    pub span: Range<usize>,
}

/// An entry in the `[lints]` section of a manifest that tries to change the level of a lint that
/// a group forbids, such as `unused_binding = "allow"` alongside `unused = "forbid"`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForbiddenLint {
    pub name: LintName,
    /// The first lint that the entry couldn't change.
    pub lint: Lint,
    pub level: LintLevel,
    /// The span of the name in the manifest, including any quotes.
    pub span: Range<usize>,
}

impl ForbiddenLint {
    pub fn kind(&self) -> DiagnosticKind {
        self.name
            .forbidden_override(self.lint, self.level, ForbiddenBy::Manifest)
    }
}

/// The problems found while applying the `[lints]` section of a manifest.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct LintProblems {
    /// Names that don't refer to a known lint or group, which are ignored.
    pub unknown: Vec<UnknownLint>,
    /// Entries that try to change the level of a forbidden lint, which are errors.
    pub forbidden: Vec<ForbiddenLint>,
}

impl Manifest {
    /// Applies the `[lints]` section to the given lint levels, and returns any problems with it. A
    /// lint's own level takes precedence over the level of any group it's in, unless the group
    /// forbids it.
    pub fn apply_lints(&self, levels: &mut LintLevels) -> LintProblems {
        let mut overrides = Vec::new();
        let mut spans = Vec::new();
        let mut problems = LintProblems::default();

        for (name, config) in &self.lints {
            if let Some(lint) = LintName::from_name(name.get_ref()) {
                if let Some(level) = config.level() {
                    overrides.push((lint, level));
                    spans.push((lint, name.span()));
                }
            } else {
                problems.unknown.push(UnknownLint {
                    name: name.get_ref().clone(),
                    span: name.span(),
                });
            }
        }

        for (name, lint, level) in levels.apply(&overrides) {
            // Each name appears once, since they're the keys of a table
            let span = spans
                .iter()
                .find(|(other, _)| *other == name)
                .map(|(_, span)| span.clone())
                .unwrap_or_default();

            problems.forbidden.push(ForbiddenLint {
                name,
                lint,
                level,
                span,
            });
        }

        problems
    }

    /// Applies the options set in the `[lints]` section. Options of unknown lints are ignored,
    /// and the rest were checked when the manifest was parsed.
    pub fn apply_lint_options(&self, options: &mut LintOptions) {
        for (name, config) in &self.lints {
            if let Some(LintName::Lint(lint)) = LintName::from_name(name.get_ref()) {
                for (option, value) in config.options() {
                    let _ = options.set(lint, option, value);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct CompilerSection {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    pub entrypoint: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub std: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dist_dir: Option<String>,
}

impl Default for CompilerSection {
    fn default() -> Self {
        Self {
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
            entrypoint: "puzzles".to_string(),
            std: None,
            dist_dir: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
pub struct FormatOptions {
    pub max_width: usize,
    pub indent_width: usize,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            max_width: 100,
            indent_width: 4,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_lints_section() {
        let manifest: Manifest = toml::from_str(
            r#"
            [compiler]
            entrypoint = "puzzles"

            [lints]
            unused_binding = "allow"
            unused_function = "warn"
            unnecessary_cast = "deny"
            not_a_lint = "deny"
            "#,
        )
        .unwrap();

        let mut levels = LintLevels::default();
        let unknown = manifest
            .apply_lints(&mut levels)
            .unknown
            .into_iter()
            .map(|lint| lint.name)
            .collect::<Vec<_>>();

        assert_eq!(unknown, vec!["not_a_lint".to_string()]);
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
        assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Warn);
        assert_eq!(levels.get(Lint::UnnecessaryCast), LintLevel::Deny);
        assert_eq!(
            levels.get(Lint::UnusedParameter),
            Lint::UnusedParameter.default_level()
        );
    }

    #[test]
    fn lints_override_their_groups() {
        // Keys are sorted, so `unused` comes after the lints in it regardless of where it's written
        let manifest: Manifest = toml::from_str(
            r#"
            [compiler]
            entrypoint = "puzzles"

            [lints]
            unused_binding = "allow"
            unused = "deny"
            unused_import = "warn"
            "#,
        )
        .unwrap();

        let mut levels = LintLevels::default();
        assert_eq!(manifest.apply_lints(&mut levels), LintProblems::default());
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Warn);
        assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Deny);
        assert_eq!(levels.get(Lint::UnusedValue), LintLevel::Deny);
        assert_eq!(
            levels.get(Lint::UnnecessaryCast),
            Lint::UnnecessaryCast.default_level()
        );
    }

    #[test]
    fn lints_cannot_override_forbidden_groups() {
        let text = "[compiler]\n\n[lints]\nunused = \"forbid\"\nunused_binding = \"allow\"\nunused_import = \"forbid\"\n";
        let manifest: Manifest = toml::from_str(text).unwrap();

        let mut levels = LintLevels::default();
        let problems = manifest.apply_lints(&mut levels);

        assert_eq!(
            problems.forbidden,
            vec![ForbiddenLint {
                name: LintName::Lint(Lint::UnusedBinding),
                lint: Lint::UnusedBinding,
                level: LintLevel::Allow,
                span: text.find("unused_binding").unwrap()
                    ..text.find("unused_binding").unwrap() + "unused_binding".len(),
            }]
        );
        assert_eq!(
            problems.forbidden[0].kind().to_string(),
            "Cannot set lint `unused_binding` to `allow`, since it's forbidden by `Rue.toml`"
        );
        assert!(problems.unknown.is_empty());
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);

        // A lint can still forbid itself when its group sets a different level
        let manifest: Manifest = toml::from_str(
            "[compiler]\n\n[lints]\nunused = \"allow\"\nunused_binding = \"forbid\"\n",
        )
        .unwrap();

        let mut levels = LintLevels::default();
        assert_eq!(manifest.apply_lints(&mut levels), LintProblems::default());
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Allow);
    }

    #[test]
    fn rejects_unknown_lint_level() {
        let result = toml::from_str::<Manifest>(
            r#"
            [compiler]
            entrypoint = "puzzles"

            [lints]
            unused_binding = "expect"
            "#,
        );

        assert!(result.is_err());
    }

    #[test]
    fn invalid_lint_levels_list_the_valid_ones() {
        for (value, message) in [
            (
                "\"expect\"",
                "Unknown lint level `expect`, expected `allow`, `warn`, `deny` or `forbid`",
            ),
            (
                "1",
                "expected a lint level (`allow`, `warn`, `deny` or `forbid`)",
            ),
        ] {
            let error = toml::from_str::<Manifest>(&format!(
                "[compiler]\n\n[lints]\nunused_binding = {value}\n"
            ))
            .unwrap_err();

            assert!(error.message().contains(message), "{}", error.message());
        }
    }

    #[test]
    fn lints_can_set_options_in_a_table() {
        let manifest: Manifest = toml::from_str(
            r#"
            [compiler]
            entrypoint = "puzzles"

            [lints]
            large_inline = { level = "deny", min_savings = 50000 }
            repeated_expensive_call.min_savings = 0
            "#,
        )
        .unwrap();

        let mut levels = LintLevels::default();
        assert_eq!(manifest.apply_lints(&mut levels), LintProblems::default());
        assert_eq!(levels.get(Lint::LargeInline), LintLevel::Deny);

        // A table without a level leaves the level as it is
        assert_eq!(
            levels.get(Lint::RepeatedExpensiveCall),
            Lint::RepeatedExpensiveCall.default_level()
        );

        let mut options = LintOptions::default();
        manifest.apply_lint_options(&mut options);
        assert_eq!(options.large_inline_min_savings, 50000);
        assert_eq!(options.repeated_expensive_call_min_savings, 0);
    }

    #[test]
    fn invalid_lint_options_are_errors() {
        for (entry, message) in [
            (
                "large_inline = { min_saving = 1 }",
                "Unknown option `min_saving` for lint `large_inline`, expected `min_savings`",
            ),
            (
                "unused_binding = { level = \"deny\", min_savings = 1 }",
                "Unknown option `min_savings` for lint `unused_binding`, which doesn't have any options",
            ),
            (
                "cost = { min_savings = 1 }",
                "Lint group `cost` can't have options, but the lints in it can",
            ),
            (
                "large_inline = { min_savings = -1 }",
                "Expected lint option `min_savings` to be a non-negative integer",
            ),
            (
                "large_inline = { min_savings = \"many\" }",
                "Expected lint option `min_savings` to be a non-negative integer",
            ),
            (
                "large_inline = { level = \"often\" }",
                "Unknown lint level `often`",
            ),
        ] {
            let error = toml::from_str::<Manifest>(&format!("[compiler]\n\n[lints]\n{entry}\n"))
                .unwrap_err();

            assert!(error.message().contains(message), "{}", error.message());
        }

        // Options of unknown lints are ignored, since the name is reported instead
        let manifest: Manifest =
            toml::from_str("[compiler]\n\n[lints]\ntypo = { min_savings = 1 }\n").unwrap();
        let mut options = LintOptions::default();
        manifest.apply_lint_options(&mut options);
        assert_eq!(options, LintOptions::default());
    }

    #[test]
    fn lint_tables_round_trip() {
        let text = "[compiler]\nentrypoint = \"puzzles\"\n\n[lints]\nlarge_inline = { level = \"deny\", min_savings = 50000 }\nunused = \"allow\"\n";
        let manifest: Manifest = toml::from_str(text).unwrap();
        let reparsed: Manifest = toml::from_str(&toml::to_string(&manifest).unwrap()).unwrap();

        assert_eq!(
            reparsed.lints.values().collect::<Vec<_>>(),
            manifest.lints.values().collect::<Vec<_>>()
        );
    }

    #[test]
    fn omits_empty_lints_section() {
        let manifest = Manifest::default();
        let text = toml::to_string(&manifest).unwrap();
        assert!(!text.contains("[lints]"));
    }

    #[test]
    fn unknown_group_names_are_reported_like_lints() {
        let text = "[compiler]\n\n[lints]\nunuse = \"deny\"\nunused = \"allow\"\n";
        let manifest: Manifest = toml::from_str(text).unwrap();

        let mut levels = LintLevels::default();
        let unknown = manifest.apply_lints(&mut levels).unknown;

        assert_eq!(
            unknown,
            vec![UnknownLint {
                name: "unuse".to_string(),
                span: text.find("unuse ").unwrap()..text.find("unuse ").unwrap() + 5,
            }]
        );
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
    }

    #[test]
    fn unknown_lints_have_spans_in_any_table_layout() {
        for text in [
            "[lints] # project lints\ntypo = \"allow\"\n",
            "lints = { typo = \"allow\" }\n",
            "lints.typo = \"allow\"\n",
            "[lints]\n\"typo\" = \"allow\"\n",
        ] {
            let manifest: Manifest = toml::from_str(&format!("{text}[compiler]\n")).unwrap();

            let unknown = manifest.apply_lints(&mut LintLevels::default()).unknown;

            assert_eq!(unknown.len(), 1, "{text}");
            assert_eq!(unknown[0].name, "typo");
        }

        let text = "[compiler]\n\n[lints]\nunused_binding = \"allow\"\n\"typo\" = \"deny\"\n";
        let manifest: Manifest = toml::from_str(text).unwrap();
        let unknown = manifest.apply_lints(&mut LintLevels::default()).unknown;

        assert_eq!(&text[unknown[0].span.clone()], "\"typo\"");
    }
}
