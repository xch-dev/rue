use rue_ast::{
    AstBinding, AstConstantItem, AstFunctionItem, AstFunctionParameter, AstGenericParameters,
    AstLetStmt, AstModuleItem, AstNode, AstStructField, AstStructItem, AstTypeAliasItem,
};
use rue_diagnostic::{Lint, Note, Source};
use rue_parser::SyntaxToken;

use crate::{LintContext, LintInput, srcloc};

pub(crate) const LINTS: &[Lint] = &[Lint::NamingConvention];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Case {
    /// `snake_case`, for functions, modules, fields, parameters and bindings.
    Snake,
    /// `UpperCamelCase`, for structs, type aliases and generic types.
    UpperCamel,
    /// `SCREAMING_SNAKE_CASE`, for constants.
    ScreamingSnake,
    /// Either `snake_case` or `SCREAMING_SNAKE_CASE`, for parameters and fields. Curried parameters
    /// (and the fields of structs that hold them) are conventionally in `SCREAMING_SNAKE_CASE`.
    SnakeOrScreamingSnake,
}

impl Case {
    fn name(self) -> &'static str {
        match self {
            Self::Snake => "snake_case",
            Self::UpperCamel => "UpperCamelCase",
            Self::ScreamingSnake => "SCREAMING_SNAKE_CASE",
            Self::SnakeOrScreamingSnake => "snake_case or SCREAMING_SNAKE_CASE",
        }
    }

    fn matches(self, name: &str) -> bool {
        match self {
            Self::Snake => !name.chars().any(char::is_uppercase),
            Self::ScreamingSnake => !name.chars().any(char::is_lowercase),
            Self::SnakeOrScreamingSnake => {
                Self::Snake.matches(name) || Self::ScreamingSnake.matches(name)
            }
            Self::UpperCamel => {
                name.chars().next().is_none_or(char::is_uppercase) && !name.contains('_')
            }
        }
    }

    fn convert(self, name: &str) -> String {
        let words = words(name);

        match self {
            Self::Snake | Self::SnakeOrScreamingSnake => words.join("_").to_lowercase(),
            Self::ScreamingSnake => words.join("_").to_uppercase(),
            Self::UpperCamel => words
                .iter()
                .map(|word| {
                    let mut chars = word.chars();
                    chars.next().map_or_else(String::new, |first| {
                        first
                            .to_uppercase()
                            .chain(chars.flat_map(char::to_lowercase))
                            .collect()
                    })
                })
                .collect(),
        }
    }
}

/// Splits a name into words, at underscores and where it changes from lowercase to uppercase
/// (or from an acronym to a word, such as the `Hash` in `XMLHash`).
fn words(name: &str) -> Vec<String> {
    let mut words = Vec::new();

    for part in name.split('_').filter(|part| !part.is_empty()) {
        let chars = part.chars().collect::<Vec<_>>();
        let mut word = String::new();

        for (index, &char) in chars.iter().enumerate() {
            let previous = index.checked_sub(1).map(|index| chars[index]);
            let next = chars.get(index + 1).copied();

            let boundary = char.is_uppercase()
                && previous.is_some_and(|previous| {
                    previous.is_lowercase()
                        || previous.is_ascii_digit()
                        || (previous.is_uppercase() && next.is_some_and(char::is_lowercase))
                });

            if boundary && !word.is_empty() {
                words.push(std::mem::take(&mut word));
            }

            word.push(char);
        }

        if !word.is_empty() {
            words.push(word);
        }
    }

    words
}

/// Reports names that don't follow the naming convention for what they name.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for &(source, document) in &input.documents {
        for node in document.syntax().descendants() {
            if let Some(item) = AstFunctionItem::cast(node.clone()) {
                check_name(cx, source, item.name(), Case::Snake, "Function");
            } else if let Some(item) = AstModuleItem::cast(node.clone()) {
                check_name(cx, source, item.name(), Case::Snake, "Module");
            } else if let Some(item) = AstConstantItem::cast(node.clone()) {
                check_name(cx, source, item.name(), Case::ScreamingSnake, "Constant");
            } else if let Some(item) = AstStructItem::cast(node.clone()) {
                check_name(cx, source, item.name(), Case::UpperCamel, "Struct");
            } else if let Some(item) = AstTypeAliasItem::cast(node.clone()) {
                check_name(cx, source, item.name(), Case::UpperCamel, "Type alias");
            } else if let Some(field) = AstStructField::cast(node.clone()) {
                check_name(
                    cx,
                    source,
                    field.name(),
                    Case::SnakeOrScreamingSnake,
                    "Field",
                );
            } else if let Some(parameters) = AstGenericParameters::cast(node.clone()) {
                for name in parameters.names() {
                    check_name(cx, source, Some(name), Case::UpperCamel, "Generic type");
                }
            } else if let Some(parameter) = AstFunctionParameter::cast(node.clone()) {
                for name in parameter
                    .binding()
                    .iter()
                    .flat_map(|binding| binding_names(binding, false))
                {
                    check_name(
                        cx,
                        source,
                        Some(name),
                        Case::SnakeOrScreamingSnake,
                        "Parameter",
                    );
                }
            } else if let Some(stmt) = AstLetStmt::cast(node.clone()) {
                for name in stmt
                    .binding()
                    .iter()
                    .flat_map(|binding| binding_names(binding, false))
                {
                    check_name(cx, source, Some(name), Case::Snake, "Binding");
                }
            }
        }
    }
}

fn check_name(
    cx: &mut LintContext<'_>,
    source: &Source,
    name: Option<SyntaxToken>,
    case: Case,
    kind: &str,
) {
    let Some(name) = name else {
        return;
    };

    // Leading underscores (such as for unused names) aren't part of the name's case
    let text = name.text();
    let trimmed = text.trim_start_matches('_');

    if case.matches(trimmed) {
        return;
    }

    let prefix = &text[..text.len() - trimmed.len()];
    let suggestion = format!("{prefix}{}", case.convert(trimmed));

    if let Some(diagnostic) = cx.report(
        Lint::NamingConvention,
        srcloc(source, name.text_range()),
        format!("{kind} `{text}` should be named in {}", case.name()),
    ) {
        diagnostic
            .notes
            .push(Note::new(format!("Consider renaming it to `{suggestion}`")));
    }
}

/// The names that a binding (such as a destructuring pattern) declares, in order. A struct field
/// without a binding declares a binding with the field's name, which is only included if
/// `shorthand` is set, since it's named by the struct rather than the binding.
pub(crate) fn binding_names(binding: &AstBinding, shorthand: bool) -> Vec<SyntaxToken> {
    let mut names = Vec::new();
    collect_binding_names(binding, shorthand, &mut names);
    names
}

fn collect_binding_names(binding: &AstBinding, shorthand: bool, names: &mut Vec<SyntaxToken>) {
    match binding {
        AstBinding::NamedBinding(binding) => names.extend(binding.name()),
        AstBinding::PairBinding(binding) => {
            for binding in [binding.first(), binding.rest()].into_iter().flatten() {
                collect_binding_names(&binding, shorthand, names);
            }
        }
        AstBinding::ListBinding(binding) => {
            for item in binding.items() {
                if let Some(binding) = item.binding() {
                    collect_binding_names(&binding, shorthand, names);
                }
            }
        }
        AstBinding::StructBinding(binding) => {
            for field in binding.fields() {
                match field.binding() {
                    Some(binding) => collect_binding_names(&binding, shorthand, names),
                    None if shorthand => names.extend(field.name()),
                    None => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert() {
        assert_eq!(Case::Snake.convert("fooBar"), "foo_bar");
        assert_eq!(Case::Snake.convert("FooBar"), "foo_bar");
        assert_eq!(Case::Snake.convert("XMLHash"), "xml_hash");
        assert_eq!(Case::Snake.convert("hash256Value"), "hash256_value");
        assert_eq!(Case::ScreamingSnake.convert("maxAmount"), "MAX_AMOUNT");
        assert_eq!(Case::ScreamingSnake.convert("max_amount"), "MAX_AMOUNT");
        assert_eq!(Case::UpperCamel.convert("coin_info"), "CoinInfo");
        assert_eq!(Case::UpperCamel.convert("coinInfo"), "CoinInfo");
        assert_eq!(Case::UpperCamel.convert("COIN_INFO"), "CoinInfo");
    }

    #[test]
    fn test_matches() {
        assert!(Case::Snake.matches("foo_bar2"));
        assert!(!Case::Snake.matches("fooBar"));
        assert!(Case::ScreamingSnake.matches("MAX_2"));
        assert!(!Case::ScreamingSnake.matches("Max"));
        assert!(Case::UpperCamel.matches("Bytes32"));
        assert!(Case::UpperCamel.matches("T"));
        assert!(!Case::UpperCamel.matches("coin"));
        assert!(!Case::UpperCamel.matches("Coin_Info"));
        assert!(Case::SnakeOrScreamingSnake.matches("MOD_HASH"));
        assert!(Case::SnakeOrScreamingSnake.matches("mod_hash"));
        assert!(!Case::SnakeOrScreamingSnake.matches("Mod_Hash"));
    }
}
