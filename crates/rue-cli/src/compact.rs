//! A more concise form of the stack traces that Chialisp's renderer writes in its Python style.
//!
//! The renderer writes each CLVM frame that it can symbolize, which includes several for each
//! call: the setup that binds a function's captures, the function itself (located at the start of
//! its body), and each expression that was being evaluated in it (such as the call to the next
//! function, or the assertion that failed). Here, each function is shown once, at the innermost
//! place it was at, which is where a debugger would say it was.

/// What the renderer writes for a frame.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Item {
    /// A frame that's named: a function, the body of an inlined function, or a frame that's
    /// unknown or ambiguous. The renderer only writes parameter values under these.
    Function {
        /// The file, line and column it's at, if it's located.
        location: Option<String>,
        name: String,
        /// The source line at the location, without its indentation.
        excerpt: Option<String>,
        /// The values of the parameters, and notes such as repeated frames.
        details: Vec<String>,
    },
    /// An expression that was being evaluated in the frame before it, which is always located.
    Expression { location: String, excerpt: String },
    /// A note about frames that aren't shown, such as `<... 2 older frames omitted ...>`.
    Note(String),
}

/// A function in the compact trace.
#[derive(Debug)]
struct Entry {
    location: Option<String>,
    name: Option<String>,
    excerpt: Option<String>,
    details: Vec<String>,
}

/// Shows each function in the trace once, at the innermost location it was at, with the values of
/// its parameters, indented to go under the failure it's for. Expressions in a function move it to
/// where it was. Expressions that can't be told to be in a function (such as the setup of a
/// function's captures, before it) are left out when a function follows them. If the trace isn't
/// in the shape the renderer writes, it's shown as it is, with the same indentation.
pub fn compact_trace(trace: &str) -> String {
    let Some(items) = parse(trace) else {
        return indent(trace.lines().map(str::to_string));
    };

    let mut lines = Vec::new();
    let mut entries: Vec<Entry> = Vec::new();
    // An expression that isn't in a function that's known, which is only kept if no function
    // follows it, in which case it's the innermost place the program was at
    let mut pending: Option<Entry> = None;

    for item in items {
        match item {
            Item::Function {
                location,
                name,
                excerpt,
                details,
            } => {
                pending = None;
                entries.push(Entry {
                    location,
                    name: Some(name),
                    excerpt,
                    details,
                });
            }
            Item::Expression { location, excerpt } => match entries.last_mut() {
                // The function was at this expression when it called the next frame
                Some(entry) if entry.name.as_deref().is_some_and(is_known) => {
                    entry.location = Some(location);
                    entry.excerpt = Some(excerpt);
                }
                _ => {
                    pending = Some(Entry {
                        location: Some(location),
                        name: None,
                        excerpt: Some(excerpt),
                        details: Vec::new(),
                    });
                }
            },
            Item::Note(note) => match entries.last_mut() {
                Some(entry) => entry.details.push(note),
                None => lines.push(note),
            },
        }
    }

    entries.extend(pending);

    for entry in entries {
        lines.push(match (entry.location, entry.name) {
            (Some(location), Some(name)) => format!("{location}, in {name}"),
            (Some(location), None) => location,
            (None, Some(name)) => format!("in {name}"),
            (None, None) => unreachable!("an entry is a function or is located"),
        });

        lines.extend(entry.excerpt.map(|excerpt| format!("  {excerpt}")));
        lines.extend(
            entry
                .details
                .into_iter()
                .map(|detail| format!("  {detail}")),
        );
    }

    indent(lines.into_iter())
}

/// Whether the frame is known to be a single function, so that the expressions after it are in it.
/// The renderer names the others `<unknown:...>` or `<ambiguous...>`, and an expression after one
/// of those can be anywhere.
fn is_known(name: &str) -> bool {
    !name.starts_with("<unknown") && !name.starts_with("<ambiguous")
}

fn indent(lines: impl Iterator<Item = String>) -> String {
    lines
        .map(|line| format!("  {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parses what the renderer writes, or returns `None` if it's in another shape.
fn parse(trace: &str) -> Option<Vec<Item>> {
    let mut items = Vec::new();
    let mut lines = trace.lines();

    while let Some(line) = lines.next() {
        if let Some(header) = line.strip_prefix("File \"") {
            // `File "<path>", line <line>, column <column>[, in <name>[  # bound: <names>]]`,
            // followed by the source line, and a line of carets under the location
            let (path, rest) = header.rsplit_once("\", line ")?;
            let (line_number, rest) = rest.split_once(", column ")?;
            let (column, name) = match rest.split_once(", in ") {
                Some((column, name)) => (column, Some(name)),
                None => (rest, None),
            };
            let line_number = line_number.parse::<usize>().ok()?;
            let column = column.parse::<usize>().ok()?;

            let excerpt = lines.next()?.strip_prefix("  ")?.to_string();
            let carets = lines.next()?.strip_prefix("  ")?;

            if !carets.trim_start().starts_with('^') {
                return None;
            }

            let location = format!("{path}:{line_number}:{column}");

            items.push(match name {
                Some(name) => Item::Function {
                    location: Some(location),
                    name: function_name(name),
                    excerpt: Some(excerpt),
                    details: Vec::new(),
                },
                None => Item::Expression { location, excerpt },
            });
        } else if let Some(name) = line.strip_prefix("in ") {
            items.push(Item::Function {
                location: None,
                name: function_name(name),
                excerpt: None,
                details: Vec::new(),
            });
        } else if let Some(detail) = line.strip_prefix("  ") {
            let Some(Item::Function { details, .. }) = items.last_mut() else {
                return None;
            };
            details.push(detail.to_string());
        } else if line.starts_with("<...") {
            items.push(Item::Note(line.to_string()));
        } else {
            return None;
        }
    }

    Some(items)
}

/// The name of a function, without the names of the values that were bound to it by a curry,
/// which are shown with the other values anyway.
fn function_name(name: &str) -> String {
    name.split_once("  # bound: ")
        .map_or(name, |(name, _)| name)
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn functions_are_shown_once_where_they_were() {
        let trace = "\
File \"main.rue\", line 5, column 21
  test fn test_main() {
                      ^
File \"main.rue\", line 5, column 21, in test_main
  test fn test_main() {
                      ^
File \"main.rue\", line 6, column 5
  do_the_test();
  ^^^^^^^^^^^^^
File \"main.rue\", line 10, column 5, in do_the_test
  assert value > 10;
  ^^^^^^^^^^^^^^^^^^
  value = 3
File \"main.rue\", line 10, column 5
  assert value > 10;
  ^^^^^^^^^^^^^^^^^^";

        assert_eq!(
            compact_trace(trace),
            "  main.rue:6:5, in test_main
    do_the_test();
  main.rue:10:5, in do_the_test
    assert value > 10;
    value = 3"
        );
    }

    #[test]
    fn notes_and_frames_without_locations_are_kept() {
        let trace = "\
<... 2 older frames omitted ...>
File \"main.rue\", line 3, column 5, in <lambda>  # bound: limit
  item < limit
  ^^^^^^^^^^^^
  limit = 10
  item = 30
<... 3 identical frames repeated ...>
in <ambiguous: first | second>
  arguments = [-1]
in <ambiguous>
  arguments = [-1]";

        assert_eq!(
            compact_trace(trace),
            "  <... 2 older frames omitted ...>
  main.rue:3:5, in <lambda>
    item < limit
    limit = 10
    item = 30
    <... 3 identical frames repeated ...>
  in <ambiguous: first | second>
    arguments = [-1]
  in <ambiguous>
    arguments = [-1]"
        );
    }

    #[test]
    fn expressions_after_unknown_frames_are_their_own() {
        // A program that no sidecar describes runs one that does, whose setup is first
        let trace = "\
in <unknown:12d4>
File \"main.rue\", line 3, column 29
  fn main(amount: Int) -> Int {
                              ^
File \"main.rue\", line 4, column 5, in main
  outer(amount) + 1
  ^^^^^^^^^^^^^^^^^
  amount = 10";

        assert_eq!(
            compact_trace(trace),
            "  in <unknown:12d4>
  main.rue:4:5, in main
    outer(amount) + 1
    amount = 10"
        );

        // If no function follows, the expression is the innermost place the program was at
        let trace = "\
in <ambiguous: first | second>
  arguments = [-1]
File \"main.rue\", line 6, column 5
  assert a > 0;
  ^^^^^^^^^^^^^";

        assert_eq!(
            compact_trace(trace),
            "  in <ambiguous: first | second>
    arguments = [-1]
  main.rue:6:5
    assert a > 0;"
        );
    }

    #[test]
    fn inlined_bodies_are_shown_as_functions() {
        let trace = "\
File \"main.rue\", line 1, column 24, in main
  fn main(x: Int) -> Int {
                         ^
  x = 5
File \"main.rue\", line 6, column 46, in checked_sub (inlined)
  inline fn checked_sub(a: Int, b: Int) -> Int {
                                               ^
File \"main.rue\", line 8, column 9
  raise \"underflow\";
  ^^^^^^^^^^^^^^^^^^";

        assert_eq!(
            compact_trace(trace),
            "  main.rue:1:24, in main
    fn main(x: Int) -> Int {
    x = 5
  main.rue:8:9, in checked_sub (inlined)
    raise \"underflow\";"
        );
    }

    #[test]
    fn a_trace_without_functions_keeps_its_last_frame() {
        let trace = "\
File \"main.rue\", line 1, column 1
  raise;
  ^^^^^^";

        assert_eq!(compact_trace(trace), "  main.rue:1:1\n    raise;");
    }

    #[test]
    fn traces_in_another_shape_are_kept_as_they_are() {
        for trace in [
            "something else",
            "File \"main.rue\", line x, column 1\n  a\n  ^",
            // The line after the source line has no carets
            "File \"main.rue\", line 1, column 1\n  a\n  b",
            // Values are only written under a function
            "File \"main.rue\", line 1, column 1\n  a\n  ^\n  x = 1",
        ] {
            assert_eq!(
                compact_trace(trace),
                indent(trace.lines().map(str::to_string))
            );
        }
    }
}
