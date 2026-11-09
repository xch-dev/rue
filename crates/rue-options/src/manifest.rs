use std::collections::BTreeMap;

use rue_diagnostic::{Lint, LintLevel, LintLevels};
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub compiler: CompilerSection,
    #[serde(default)]
    pub formatter: FormatOptions,
    /// Overrides for the level of each lint, keyed by lint name.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub lints: BTreeMap<String, LintLevel>,
}

impl Manifest {
    /// Applies the `[lints]` section to the given lint levels, and returns the names of any
    /// lints that don't exist.
    pub fn apply_lints(&self, levels: &mut LintLevels) -> Vec<String> {
        let mut unknown = Vec::new();

        for (name, &level) in &self.lints {
            if let Some(lint) = Lint::from_name(name) {
                levels.set(lint, level);
            } else {
                unknown.push(name.clone());
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
        let unknown = manifest.apply_lints(&mut levels);

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
            unused_binding = "forbid"
            "#,
        );

        assert!(result.is_err());
    }

    #[test]
    fn omits_empty_lints_section() {
        let manifest = Manifest::default();
        let text = toml::to_string(&manifest).unwrap();
        assert!(!text.contains("[lints]"));
    }
}
