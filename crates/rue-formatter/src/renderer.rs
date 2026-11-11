use crate::{
    FormatOptions,
    document::{Doc, LineKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Flat,
    Broken,
}

#[derive(Debug, Clone, Copy)]
struct Command<'a> {
    indent: usize,
    mode: Mode,
    doc: &'a Doc,
}

pub fn render(doc: &Doc, options: &FormatOptions) -> String {
    let mut output = String::new();
    let mut column = 0;
    // The end of the last text or line break, after which the output only contains the spaces
    // added for indentation and separators, which are removed at the end of a line
    let mut content_end = 0;
    let mut commands = vec![Command {
        indent: 0,
        mode: Mode::Broken,
        doc,
    }];

    while let Some(command) = commands.pop() {
        match command.doc {
            Doc::Nil => {}
            Doc::Text(text) => {
                output.push_str(text);
                column += text.chars().count();
                if !text.chars().all(|char| char == ' ') {
                    content_end = output.len();
                }
            }
            Doc::Concat(docs) => {
                for doc in docs.iter().rev() {
                    commands.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc,
                    });
                }
            }
            Doc::Line(LineKind::Soft) if command.mode == Mode::Flat => {
                output.push(' ');
                column += 1;
            }
            Doc::Fill {
                flat,
                broken,
                space_when_flat,
                indent_levels_on_break,
            } => {
                let separator_width = usize::from(*space_when_flat);
                if column + separator_width <= options.max_width
                    && fits(
                        options.max_width - column - separator_width,
                        command.indent,
                        flat,
                        &commands,
                        Mode::Broken,
                    )
                {
                    if *space_when_flat {
                        output.push(' ');
                        column += 1;
                    }
                    commands.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc: flat,
                    });
                } else {
                    output.truncate(content_end);
                    output.push('\n');
                    content_end = output.len();
                    let indent = command.indent + indent_levels_on_break * options.indent_width;
                    output.extend(std::iter::repeat_n(' ', indent));
                    column = indent;
                    commands.push(Command {
                        indent,
                        mode: command.mode,
                        doc: broken,
                    });
                }
            }
            Doc::Line(kind) => {
                output.truncate(content_end);
                output.push('\n');
                if *kind == LineKind::Empty && !output.ends_with("\n\n") {
                    output.push('\n');
                }
                content_end = output.len();
                output.extend(std::iter::repeat_n(' ', command.indent));
                column = command.indent;
            }
            Doc::Indent(doc) => commands.push(Command {
                indent: command.indent + options.indent_width,
                mode: command.mode,
                doc,
            }),
            Doc::Outdent(doc) => commands.push(Command {
                indent: command.indent.saturating_sub(options.indent_width),
                mode: command.mode,
                doc,
            }),
            Doc::Group(doc) => {
                let mode = if has_forced_line(doc)
                    || !fits(
                        options.max_width.saturating_sub(column),
                        command.indent,
                        doc,
                        &commands,
                        Mode::Flat,
                    ) {
                    Mode::Broken
                } else {
                    Mode::Flat
                };
                commands.push(Command {
                    indent: command.indent,
                    mode,
                    doc,
                });
            }
            Doc::IfBreak { broken, flat } => commands.push(Command {
                indent: command.indent,
                mode: command.mode,
                doc: if command.mode == Mode::Broken {
                    broken
                } else {
                    flat
                },
            }),
        }
    }

    normalize_output(&output)
}

fn has_forced_line(doc: &Doc) -> bool {
    match doc {
        Doc::Nil | Doc::Text(_) | Doc::Line(LineKind::Soft) => false,
        Doc::Line(LineKind::Hard | LineKind::Empty) => true,
        Doc::Concat(docs) => docs.iter().any(has_forced_line),
        Doc::Indent(doc) | Doc::Outdent(doc) | Doc::Group(doc) => has_forced_line(doc),
        Doc::Fill { flat, .. } => has_forced_line(flat),
        Doc::IfBreak { flat, .. } => has_forced_line(flat),
    }
}

fn fits(
    mut remaining: usize,
    initial_indent: usize,
    doc: &Doc,
    remaining_commands: &[Command<'_>],
    initial_mode: Mode,
) -> bool {
    let mut commands = remaining_commands.to_vec();
    commands.push(Command {
        indent: initial_indent,
        mode: initial_mode,
        doc,
    });

    while let Some(command) = commands.pop() {
        match command.doc {
            Doc::Nil => {}
            Doc::Text(text) => {
                let width = text.chars().count();
                if width > remaining {
                    return false;
                }
                remaining -= width;
            }
            Doc::Concat(docs) => {
                for doc in docs.iter().rev() {
                    commands.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc,
                    });
                }
            }
            Doc::Line(LineKind::Soft) if command.mode == Mode::Flat => {
                if remaining == 0 {
                    return false;
                }
                remaining -= 1;
            }
            Doc::Fill {
                flat,
                space_when_flat,
                ..
            } => {
                let separator_width = usize::from(*space_when_flat);
                if separator_width > remaining {
                    return false;
                }
                remaining -= separator_width;
                commands.push(Command {
                    indent: command.indent,
                    mode: command.mode,
                    doc: flat,
                });
            }
            Doc::Line(LineKind::Soft) => return true,
            Doc::Line(LineKind::Hard | LineKind::Empty) => return true,
            Doc::Indent(doc) | Doc::Outdent(doc) | Doc::Group(doc) => {
                commands.push(Command {
                    indent: command.indent,
                    mode: command.mode,
                    doc,
                });
            }
            Doc::IfBreak { broken, flat } => commands.push(Command {
                indent: command.indent,
                mode: command.mode,
                doc: if command.mode == Mode::Broken {
                    broken
                } else {
                    flat
                },
            }),
        }
    }

    true
}

fn normalize_output(output: &str) -> String {
    let mut normalized = output.to_string();

    while normalized.ends_with("\n\n\n") {
        normalized.pop();
    }

    if !normalized.ends_with('\n') {
        normalized.push('\n');
    }

    normalized
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fill_line_stays_flat_before_broken_suffix_when_prefix_fits() {
        let doc = Doc::concat([
            Doc::text("lhs"),
            Doc::fill(Doc::text("== rhs"), 0),
            Doc::hard_line(),
            Doc::text("tail"),
        ]);
        assert_eq!(
            render(
                &doc,
                &FormatOptions {
                    max_width: 10,
                    indent_width: 4,
                },
            ),
            "lhs == rhs\ntail\n"
        );
    }

    #[test]
    fn fill_line_breaks_when_broken_prefix_does_not_fit() {
        let doc = Doc::concat([
            Doc::text("long_lhs"),
            Doc::fill(Doc::text("== rhs"), 1),
            Doc::hard_line(),
            Doc::text("tail"),
        ]);
        assert_eq!(
            render(
                &doc,
                &FormatOptions {
                    max_width: 10,
                    indent_width: 4,
                },
            ),
            "long_lhs\n    == rhs\ntail\n"
        );
    }
}
