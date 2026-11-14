use crate::token_stream::{CommentPlacement, Gap};

/// Splits trivia between two movable units without guessing that the whole gap
/// belongs to the unit on its right. Inline comments form the trailing prefix;
/// comments beginning on their own line form the leading suffix.
pub fn split_between(gap: &Gap) -> (Gap, Gap) {
    let split = gap
        .comments
        .iter()
        .position(|comment| comment.placement != CommentPlacement::Trailing)
        .unwrap_or(gap.comments.len());

    let mut trailing = Gap {
        comments: gap.comments[..split].to_vec(),
        newlines: 0,
    };
    let mut leading = Gap {
        comments: gap.comments[split..].to_vec(),
        newlines: gap.newlines,
    };

    if trailing.comments.is_empty() {
        return (Gap::default(), leading);
    }
    if leading.comments.is_empty() {
        trailing.newlines = gap.newlines.min(1);
        return (trailing, Gap::default());
    }

    let first_leading = leading
        .comments
        .first_mut()
        .expect("leading trivia is known to contain a comment");
    trailing.newlines = first_leading.newlines_before;
    first_leading.newlines_before = 0;
    (trailing, leading)
}

/// Separates comments that are visually a file banner from documentation
/// attached to the first item. The closest contiguous comment block is leading
/// trivia; earlier blocks remain anchored at the file head.
pub fn split_file_header(gap: &Gap) -> (Gap, Gap) {
    let (_, item_leading) = split_between(gap);
    let gap = &item_leading;
    if gap.comments.is_empty() || gap.newlines > 1 {
        return (gap.clone(), Gap::default());
    }

    let mut attached_start = gap.comments.len() - 1;
    while attached_start > 0 && gap.comments[attached_start].newlines_before <= 1 {
        attached_start -= 1;
    }

    let mut header = Gap {
        comments: gap.comments[..attached_start].to_vec(),
        newlines: 0,
    };
    let mut leading = Gap {
        comments: gap.comments[attached_start..].to_vec(),
        newlines: gap.newlines,
    };
    if let Some(first) = leading.comments.first_mut() {
        header.newlines = first.newlines_before;
        first.newlines_before = 0;
    }
    (header, leading)
}

/// Splits trivia after an import-group opener. Inline opener comments and
/// standalone banner blocks stay dangling on `{`; only the closest comment
/// block without a blank line before the first path becomes path documentation.
pub fn split_group_opening(gap: &Gap) -> (Gap, Gap) {
    let trailing_count = gap
        .comments
        .iter()
        .position(|comment| comment.placement != CommentPlacement::Trailing)
        .unwrap_or(gap.comments.len());
    let mut attached_start = if trailing_count == gap.comments.len() || gap.newlines > 1 {
        gap.comments.len()
    } else {
        gap.comments.len() - 1
    };
    // Nothing is attached if `attached_start` is past the end, since there's a blank line before
    // the first path
    while attached_start > trailing_count
        && attached_start < gap.comments.len()
        && gap.comments[attached_start].newlines_before <= 1
    {
        attached_start -= 1;
    }

    let mut opening = Gap {
        comments: gap.comments[..attached_start].to_vec(),
        newlines: 0,
    };
    let mut leading = Gap {
        comments: gap.comments[attached_start..].to_vec(),
        newlines: gap.newlines,
    };
    if let Some(first) = leading.comments.first_mut() {
        opening.newlines = first.newlines_before;
        first.newlines_before = 0;
    } else {
        opening.newlines = gap.newlines;
    }
    (opening, leading)
}
