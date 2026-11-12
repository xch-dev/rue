use thiserror::Error;

use crate::Lint;

/// Options that change when some lints are reported, which can be set for each lint in `Rue.toml`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintOptions {
    /// The smallest estimated cost that removing `inline` from a function must save for
    /// `large_inline` to report it.
    pub large_inline_min_savings: u64,
    /// The smallest estimated cost that storing a repeated call in a binding must save for
    /// `repeated_expensive_call` to report it.
    pub repeated_expensive_call_min_savings: u64,
}

impl Default for LintOptions {
    fn default() -> Self {
        Self {
            // Removing `inline` changes how the code is written, so it's only worth it if it
            // saves more than about 10 bytes
            large_inline_min_savings: 120_000,
            repeated_expensive_call_min_savings: 1,
        }
    }
}

/// A problem with an option set for a lint.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LintOptionError {
    #[error("Unknown option `{option}` for lint `{lint}`{}", expected_options(*lint))]
    UnknownOption { lint: Lint, option: String },
}

fn expected_options(lint: Lint) -> String {
    match lint.options() {
        [] => ", which doesn't have any options".to_string(),
        options => format!(
            ", expected {}",
            options
                .iter()
                .map(|option| format!("`{option}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

impl Lint {
    /// The names of the options that can be set for the lint.
    pub fn options(self) -> &'static [&'static str] {
        match self {
            Self::LargeInline | Self::RepeatedExpensiveCall => &["min_savings"],
            _ => &[],
        }
    }
}

impl LintOptions {
    /// Sets an option of a lint, from its name and value.
    pub fn set(&mut self, lint: Lint, option: &str, value: u64) -> Result<(), LintOptionError> {
        let field = match (lint, option) {
            (Lint::LargeInline, "min_savings") => &mut self.large_inline_min_savings,
            (Lint::RepeatedExpensiveCall, "min_savings") => {
                &mut self.repeated_expensive_call_min_savings
            }
            _ => {
                return Err(LintOptionError::UnknownOption {
                    lint,
                    option: option.to_string(),
                });
            }
        };

        *field = value;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_listed_option_can_be_set() {
        for &lint in Lint::ALL {
            for option in lint.options() {
                let mut options = LintOptions::default();
                assert_eq!(options.set(lint, option, 42), Ok(()), "{lint}.{option}");
                assert_ne!(options, LintOptions::default(), "{lint}.{option}");
            }

            assert!(
                LintOptions::default()
                    .set(lint, "not_an_option", 1)
                    .is_err()
            );
        }
    }

    #[test]
    fn unknown_options_list_the_valid_ones() {
        let mut options = LintOptions::default();

        assert_eq!(
            options
                .set(Lint::LargeInline, "min_saving", 1)
                .unwrap_err()
                .to_string(),
            "Unknown option `min_saving` for lint `large_inline`, expected `min_savings`"
        );
        assert_eq!(
            options
                .set(Lint::UnusedBinding, "min_savings", 1)
                .unwrap_err()
                .to_string(),
            "Unknown option `min_savings` for lint `unused_binding`, which doesn't have any options"
        );
    }
}
