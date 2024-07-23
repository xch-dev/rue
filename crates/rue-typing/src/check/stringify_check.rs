use std::fmt;

use crate::TypePath;

use super::Check;

fn stringify_value(f: &mut fmt::Formatter<'_>, path: &[TypePath]) -> fmt::Result {
    for path in path.iter().rev() {
        match path {
            TypePath::First => write!(f, "(f ")?,
            TypePath::Rest => write!(f, "(r ")?,
        }
    }
    write!(f, "val")?;
    for _ in 0..path.len() {
        write!(f, ")")?;
    }
    Ok(())
}

pub(crate) fn stringify_check(
    check: &Check,
    f: &mut fmt::Formatter<'_>,
    path: &mut Vec<TypePath>,
) -> fmt::Result {
    match check {
        Check::None => write!(f, "1"),
        Check::IsPair => {
            write!(f, "(l ")?;
            stringify_value(f, path)?;
            write!(f, ")")
        }
        Check::IsAtom => {
            write!(f, "(not (l ")?;
            stringify_value(f, path)?;
            write!(f, "))")
        }
        Check::IsBool => {
            write!(f, "(any (= ")?;
            stringify_value(f, path)?;
            write!(f, " ()) (= ")?;
            stringify_value(f, path)?;
            write!(f, " 1))")
        }
        Check::IsNil => {
            write!(f, "(= ")?;
            stringify_value(f, path)?;
            write!(f, " ())")
        }
        Check::Length(len) => {
            write!(f, "(= (strlen ")?;
            stringify_value(f, path)?;
            write!(f, ") {len})")
        }
        Check::And(checks) => {
            write!(f, "(and")?;
            for check in checks {
                write!(f, " ")?;
                stringify_check(check, f, path)?;
            }
            write!(f, ")")
        }
        Check::Or(checks) => {
            write!(f, "(or")?;
            for check in checks {
                write!(f, " ")?;
                stringify_check(check, f, path)?;
            }
            write!(f, ")")
        }
        Check::If(cond, then, else_) => {
            write!(f, "(if ")?;
            stringify_check(cond, f, path)?;
            write!(f, " ")?;
            stringify_check(then, f, path)?;
            write!(f, " ")?;
            stringify_check(else_, f, path)?;
            write!(f, ")")
        }
        Check::Pair(first, rest) => {
            let has_first = first.as_ref() != &Check::None;
            let has_rest = rest.as_ref() != &Check::None;

            if has_first && has_rest {
                write!(f, "(all ")?;
                path.push(TypePath::First);
                stringify_check(first, f, path)?;
                path.pop().unwrap();
                write!(f, " ")?;
                path.push(TypePath::Rest);
                stringify_check(rest, f, path)?;
                path.pop().unwrap();
                write!(f, ")")
            } else if has_first {
                path.push(TypePath::First);
                stringify_check(first, f, path)?;
                path.pop().unwrap();
                Ok(())
            } else if has_rest {
                path.push(TypePath::Rest);
                stringify_check(rest, f, path)?;
                path.pop().unwrap();
                Ok(())
            } else {
                write!(f, "1")
            }
        }
    }
}
