use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Example {
    pub name: String,
    /// The group it's listed under, such as `Basics` or `Puzzles`.
    pub group: String,
    pub description: String,
    pub content: String,
    /// The solution its snapshot runs it with, written as CLVM.
    pub solution: String,
    /// The snapshot itself, which the tests compare results with.
    #[cfg(test)]
    #[wasm_bindgen(skip)]
    pub snapshot: &'static str,
}

macro_rules! example {
    ($name:literal, $group:literal, $description:literal, $path:literal) => {
        Example {
            name: $name.to_string(),
            group: $group.to_string(),
            description: $description.to_string(),
            content: include_str!(concat!("../../examples/", $path, ".rue")).to_string(),
            solution: snapshot_solution(include_str!(concat!("../../examples/", $path, ".yaml"))),
            #[cfg(test)]
            snapshot: include_str!(concat!("../../examples/", $path, ".yaml")),
        }
    };
}

#[wasm_bindgen(js_name = getExamples)]
pub fn get_examples() -> Vec<Example> {
    vec![
        example!(
            "Hello World",
            "Basics",
            "The smallest program, which returns a string",
            "hello_world"
        ),
        example!("Factorial", "Basics", "Recursion, with tests", "factorial"),
        example!(
            "Fizz Buzz",
            "Basics",
            "Lists, strings and conditions",
            "fizz_buzz"
        ),
        example!(
            "JSON",
            "Basics",
            "A JSON parser and serializer, using structs and type guards",
            "json"
        ),
        example!(
            "Brainf*ck",
            "Basics",
            "An interpreter for a tiny language",
            "brainfck"
        ),
        example!(
            "P2 Delegated Conditions",
            "Puzzles",
            "Spends that are signed by a public key",
            "puzzles/p2_delegated_conditions"
        ),
        example!(
            "Royalty Split",
            "Puzzles",
            "Pays out a coin's value to several addresses by share",
            "puzzles/royalty_split"
        ),
        example!(
            "CAT",
            "Puzzles",
            "The Chia Asset Token standard",
            "puzzles/cat"
        ),
        example!(
            "Singleton",
            "Puzzles",
            "The singleton standard, which keeps a coin unique",
            "puzzles/singleton"
        ),
        example!(
            "Partial Offer",
            "Puzzles",
            "An offer that can be taken in part",
            "puzzles/partial_offer"
        ),
    ]
}

/// The `solution` of a snapshot. Snapshots without one are run with nil.
fn snapshot_solution(snapshot: &str) -> String {
    snapshot_value(snapshot, "solution").unwrap_or_else(|| "()".to_string())
}

/// A top-level field of a snapshot, which is a plain or single-quoted YAML scalar on one line.
pub(crate) fn snapshot_value(snapshot: &str, key: &str) -> Option<String> {
    let value = snapshot
        .lines()
        .find_map(|line| line.strip_prefix(key)?.strip_prefix(": "))?;

    Some(
        match value
            .strip_prefix('\'')
            .and_then(|value| value.strip_suffix('\''))
        {
            Some(quoted) => quoted.replace("''", "'"),
            None => value.to_string(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_solutions() {
        assert_eq!(snapshot_solution("program: ()\nsolution: (5)\n"), "(5)");
        assert_eq!(
            snapshot_solution("solution: '(''{ \"a\": 1 }'')'\n"),
            "('{ \"a\": 1 }')"
        );
        assert_eq!(snapshot_solution("program: ()\n"), "()");
        // Only top-level fields count, not those of tests
        assert_eq!(
            snapshot_solution("tests:\n- name: a\n  solution: (1)\n"),
            "()"
        );
        assert_eq!(
            snapshot_value("runtime_cost: 20\n", "runtime_cost").as_deref(),
            Some("20")
        );
    }
}
