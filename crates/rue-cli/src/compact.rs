//! A more concise form of the stack traces that Chialisp's renderer writes in its Python style.
//!
//! The renderer writes each CLVM frame that it can symbolize, which includes several for each
//! call: the setup that binds a function's captures, the function itself (located at the start of
//! its body), and each expression that was being evaluated in it (such as the call to the next
//! function, or the assertion that failed). Here, each function is shown once, at the innermost
//! place it was at, which is where a debugger would say it was.

/// A frame of the rendered trace.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Frame {
    /// The file, line and column it's at, if it's located.
    location: Option<String>,
    /// The function it's in, if it's a function's frame.
    function: Option<String>,
    /// The source line at the location, without its indentation.
    excerpt: Option<String>,
    /// The values of the parameters, and notes such as repeated frames.
    details: Vec<String>,
}

/// Shows each function in the trace once, at the innermost location it was at, with the values of
/// its parameters. Frames that aren't in a function, such as the setup of a function's captures,
/// add their location to the function that's running, and are left out before the first one. If
/// the trace isn't in the shape the renderer writes, it's returned as it is.
pub fn compact_trace(trace: &str) -> String {
    let Some((preamble, frames)) = parse(trace) else {
        return trace.to_string();
    };

    let mut entries: Vec<Frame> = Vec::new();
    // A frame that isn't in a function, which is only kept if no function follows it
    let mut pending: Option<Frame> = None;

    for frame in frames {
        if frame.function.is_some() {
            pending = None;
            entries.push(frame);
        } else if let Some(entry) = entries.last_mut() {
            // The function was at this expression when it called the next frame
            if frame.location.is_some() {
                entry.location = frame.location;
                entry.excerpt = frame.excerpt;
            }
            entry.details.extend(frame.details);
        } else {
            pending = Some(frame);
        }
    }

    entries.extend(pending);

    let mut lines = preamble
        .iter()
        .map(|line| format!("  {line}"))
        .collect::<Vec<_>>();

    for entry in entries {
        lines.push(match (&entry.location, &entry.function) {
            (Some(location), Some(function)) => format!("  {location}, in {function}"),
            (Some(location), None) => format!("  {location}"),
            (None, Some(function)) => format!("  in {function}"),
            (None, None) => continue,
        });

        lines.extend(entry.excerpt.iter().map(|excerpt| format!("    {excerpt}")));
        lines.extend(entry.details.iter().map(|detail| format!("    {detail}")));
    }

    lines.join("\n")
}

/// Parses the lines before the first frame, and the frames.
fn parse(trace: &str) -> Option<(Vec<String>, Vec<Frame>)> {
    let mut preamble = Vec::new();
    let mut frames: Vec<Frame> = Vec::new();
    let mut lines = trace.lines().peekable();

    while let Some(line) = lines.next() {
        if let Some(header) = line.strip_prefix("File \"") {
            // `File "<path>", line <line>, column <column>[, in <function>[  # bound: <names>]]`,
            // followed by the source line, and a line of carets under the location
            let (path, rest) = header.split_once("\", line ")?;
            let (line_number, rest) = rest.split_once(", column ")?;
            let (column, function) = match rest.split_once(", in ") {
                Some((column, function)) => (column, Some(function)),
                None => (rest, None),
            };
            let line_number = line_number.parse::<usize>().ok()?;
            let column = column.parse::<usize>().ok()?;

            let excerpt = lines.next()?.strip_prefix("  ")?;
            let carets = lines.next()?.strip_prefix("  ")?;

            if !carets.trim_start().starts_with('^') {
                return None;
            }

            frames.push(Frame {
                location: Some(format!("{path}:{line_number}:{column}")),
                function: function.map(function_name),
                excerpt: Some(excerpt.to_string()),
                details: Vec::new(),
            });
        } else if let Some(function) = line.strip_prefix("in ") {
            frames.push(Frame {
                function: Some(function_name(function)),
                ..Frame::default()
            });
        } else if let Some(detail) = line.strip_prefix("  ") {
            frames.last_mut()?.details.push(detail.to_string());
        } else if line.starts_with("<...") {
            match frames.last_mut() {
                Some(frame) => frame.details.push(line.to_string()),
                None => preamble.push(line.to_string()),
            }
        } else {
            return None;
        }
    }

    Some((preamble, frames))
}

/// The name of a function, without the names of the values that were bound to it by a curry,
/// which are shown with the other values anyway.
fn function_name(function: &str) -> String {
    function
        .split_once("  # bound: ")
        .map_or(function, |(name, _)| name)
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
        ] {
            assert_eq!(compact_trace(trace), trace);
        }
    }
}
