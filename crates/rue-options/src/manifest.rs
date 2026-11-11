use std::{collections::BTreeMap, ops::Range};

use rue_diagnostic::{DiagnosticKind, ForbiddenBy, Lint, LintLevel, LintLevels, LintName};
use serde::{Deserialize, Serialize};
use toml::Spanned;

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub compiler: CompilerSection,
    #[serde(default)]
    pub formatter: FormatOptions,
    /// Overrides for the level of each lint or lint group, keyed by name. The span of each name is
    /// kept so that problems with it can be reported where it's written.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub lints: BTreeMap<Spanned<String>, LintLevel>,
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

        for (name, &level) in &self.lints {
            if let Some(lint) = LintName::from_name(name.get_ref()) {
                overrides.push((lint, level));
                spans.push((lint, name.span()));
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
