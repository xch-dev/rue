use std::{collections::BTreeMap, ops::Range};

use rue_diagnostic::{Lint, LintLevel, LintLevels};
use serde::{Deserialize, Serialize};
use toml::Spanned;

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub compiler: CompilerSection,
    #[serde(default)]
    pub formatter: FormatOptions,
    /// Overrides for the level of each lint, keyed by lint name. The span of each name is kept so
    /// that unknown lints can be reported where they're written.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub lints: BTreeMap<Spanned<String>, LintLevel>,
}

/// A name in the `[lints]` section of a manifest that doesn't refer to a known lint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownLint {
    pub name: String,
    /// The span of the name in the manifest, including any quotes.
    pub span: Range<usize>,
}

impl Manifest {
    /// Applies the `[lints]` section to the given lint levels, and returns any lints that don't
    /// exist.
    pub fn apply_lints(&self, levels: &mut LintLevels) -> Vec<UnknownLint> {
        let mut unknown = Vec::new();

        for (name, &level) in &self.lints {
            if let Some(lint) = Lint::from_name(name.get_ref()) {
                levels.set(lint, level);
            } else {
                unknown.push(UnknownLint {
                    name: name.get_ref().clone(),
                    span: name.span(),
                });
            }
        }

        unknown
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
    fn unknown_lints_have_spans_in_any_table_layout() {
        for text in [
            "[lints] # project lints\ntypo = \"allow\"\n",
            "lints = { typo = \"allow\" }\n",
            "lints.typo = \"allow\"\n",
            "[lints]\n\"typo\" = \"allow\"\n",
        ] {
            let manifest: Manifest = toml::from_str(&format!("{text}[compiler]\n")).unwrap();

            let unknown = manifest.apply_lints(&mut LintLevels::default());

            assert_eq!(unknown.len(), 1, "{text}");
            assert_eq!(unknown[0].name, "typo");
        }

        let text = "[compiler]\n\n[lints]\nunused_binding = \"allow\"\n\"typo\" = \"deny\"\n";
        let manifest: Manifest = toml::from_str(text).unwrap();
        let unknown = manifest.apply_lints(&mut LintLevels::default());

        assert_eq!(&text[unknown[0].span.clone()], "\"typo\"");
    }
}
