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
            Doc::Group(doc) | Doc::ReluctantGroup(doc) => {
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
            Doc::GroupWithTail { group, tail } => {
                // The tail is measured as it's laid out on its own, which is up to its first
                // line break
                commands.push(Command {
                    indent: command.indent,
                    mode: Mode::Broken,
                    doc: tail,
                });
                let broken = has_forced_line(group)
                    || !fits(
                        options.max_width.saturating_sub(column),
                        command.indent,
                        group,
                        &commands,
                        Mode::Flat,
                    );
                let tail_command = commands.last_mut().expect("the tail was pushed");

                if broken {
                    // A tail that's a group doesn't get to decide for itself
                    if let Doc::Group(doc) = &**tail {
                        tail_command.doc = doc;
                    }
                } else {
                    tail_command.mode = command.mode;
                }

                commands.push(Command {
                    indent: command.indent,
                    mode: if broken { Mode::Broken } else { Mode::Flat },
                    doc: group,
                });
            }
        }
    }

    // Spaces at the end of the document are removed like those at the end of any other line
    output.truncate(content_end);
    normalize_output(&output)
}

fn has_forced_line(doc: &Doc) -> bool {
    match doc {
        Doc::Nil | Doc::Text(_) | Doc::Line(LineKind::Soft) => false,
        Doc::Line(LineKind::Hard | LineKind::Empty) => true,
        Doc::Concat(docs) => docs.iter().any(has_forced_line),
        Doc::Indent(doc) | Doc::Outdent(doc) | Doc::Group(doc) | Doc::ReluctantGroup(doc) => {
            has_forced_line(doc)
        }
        Doc::Fill { flat, .. } => has_forced_line(flat),
        Doc::IfBreak { flat, .. } => has_forced_line(flat),
        Doc::GroupWithTail { group, tail } => has_forced_line(group) || has_forced_line(tail),
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
    // Spaces only take up width if text follows them on the same line, since `render` removes
    // them at the end of a line
    let mut pending_spaces = 0;

    while let Some(command) = commands.pop() {
        match command.doc {
            Doc::Nil => {}
            Doc::Text(text) if text.chars().all(|char| char == ' ') => {
                pending_spaces += text.len();
            }
            Doc::Text(text) => {
                let width = pending_spaces + text.chars().count();
                if width > remaining {
                    return false;
                }
                remaining -= width;
                pending_spaces = 0;
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
                pending_spaces += 1;
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
            // Measuring a reluctant group flat, even when it comes after the doc being measured
            // in a broken parent, keeps its lines from ending the current line early. Otherwise,
            // a parameter list followed by a return type with generic arguments would look like
            // it fits, and the generic arguments would break instead.
            Doc::ReluctantGroup(doc) => commands.push(Command {
                indent: command.indent,
                mode: if has_forced_line(doc) {
                    Mode::Broken
                } else {
                    Mode::Flat
                },
                doc,
            }),
            Doc::IfBreak { broken, flat } => commands.push(Command {
                indent: command.indent,
                mode: command.mode,
                doc: if command.mode == Mode::Broken {
                    broken
                } else {
                    flat
                },
            }),
            Doc::GroupWithTail { group, tail } => {
                for doc in [tail, group] {
                    commands.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc,
                    });
                }
            }
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
                    single_quote: false,
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
                    single_quote: false,
                },
            ),
            "long_lhs\n    == rhs\ntail\n"
        );
    }

    /// A group with `head`, a soft line and `more` in it, and after it a tail of `{`, a soft
    /// line, `tail`, a soft line and `}` that's a group of its own.
    fn group_with_tail(head: &str, more: &str, tail: &str, max_width: usize) -> String {
        let doc = Doc::concat([
            Doc::group_with_tail(
                Doc::concat([
                    Doc::text(head),
                    Doc::concat([Doc::soft_line(), Doc::text(more)]).indent(),
                    Doc::if_break(Doc::hard_line(), Doc::space()),
                ]),
                Doc::concat([
                    Doc::text("{"),
                    Doc::concat([Doc::soft_line(), Doc::text(tail)]).indent(),
                    Doc::soft_line(),
                    Doc::text("}"),
                ])
                .group(),
            ),
            Doc::text(";"),
        ]);

        render(
            &doc,
            &FormatOptions {
                max_width,
                indent_width: 4,
                single_quote: false,
            },
        )
    }

    #[test]
    fn group_with_tail_fits_on_one_line() {
        assert_eq!(
            group_with_tail("head", "more", "tail", 22),
            "head more { tail };\n"
        );
    }

    #[test]
    fn group_with_tail_only_measures_the_first_line_of_the_tail() {
        // The group and the `{` fit, so only the tail breaks, as it would on its own
        assert_eq!(
            group_with_tail("head", "more", "tail", 18),
            "head more {\n    tail\n};\n"
        );
        assert_eq!(
            group_with_tail("head", "more", "tail", 11),
            "head more {\n    tail\n};\n"
        );
    }

    #[test]
    fn group_with_tail_breaks_the_tail_with_the_group() {
        // The `{` doesn't fit after the group, so the group breaks, and the tail does too,
        // though it would fit on a line
        assert_eq!(
            group_with_tail("head", "more", "tail", 10),
            "head\n    more\n{\n    tail\n};\n"
        );
    }

    #[test]
    fn group_with_tail_breaks_the_group_around_it_if_its_tail_has_a_forced_line() {
        let inner = Doc::group_with_tail(
            Doc::concat([Doc::text("head"), Doc::space()]),
            Doc::concat([
                Doc::text("{"),
                Doc::concat([Doc::hard_line(), Doc::text("tail")]).indent(),
                Doc::hard_line(),
                Doc::text("}"),
            ]),
        );
        let doc = Doc::concat([
            Doc::text("["),
            Doc::concat([Doc::soft_line(), inner]).indent(),
            Doc::soft_line(),
            Doc::text("]"),
        ])
        .group();

        assert_eq!(
            render(
                &doc,
                &FormatOptions {
                    max_width: 80,
                    indent_width: 4,
                    single_quote: false,
                },
            ),
            "[\n    head {\n        tail\n    }\n]\n"
        );
    }

    #[test]
    fn group_with_tail_is_measured_flat_by_the_group_around_it() {
        let inner = Doc::group_with_tail(
            Doc::concat([Doc::text("head"), Doc::space()]),
            Doc::concat([
                Doc::text("{"),
                Doc::concat([Doc::soft_line(), Doc::text("tail")]).indent(),
                Doc::soft_line(),
                Doc::text("}"),
            ])
            .group(),
        );
        let doc = Doc::concat([
            Doc::text("["),
            Doc::concat([
                Doc::soft_line(),
                inner,
                Doc::text(","),
                Doc::soft_line(),
                Doc::text("x"),
            ])
            .indent(),
            Doc::soft_line(),
            Doc::text("]"),
        ])
        .group();
        let render = |max_width| {
            render(
                &doc,
                &FormatOptions {
                    max_width,
                    indent_width: 4,
                    single_quote: false,
                },
            )
        };

        assert_eq!(render(20), "[ head { tail }, x ]\n");
        // The group around it breaks before the tail does
        assert_eq!(render(19), "[\n    head { tail },\n    x\n]\n");
    }

    #[test]
    fn spaces_before_a_line_break_do_not_count_toward_the_width() {
        // The group's soft line and the space after it are removed at the end of the line, so the
        // group fits exactly
        let doc = Doc::concat([
            Doc::text("abc"),
            Doc::concat([Doc::text("de"), Doc::soft_line(), Doc::space()]).group(),
            Doc::hard_line(),
            Doc::text("tail"),
        ]);
        assert_eq!(
            render(
                &doc,
                &FormatOptions {
                    max_width: 5,
                    indent_width: 4,
                    single_quote: false,
                },
            ),
            "abcde\ntail\n"
        );
    }
}
