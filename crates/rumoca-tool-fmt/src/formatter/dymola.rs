use crate::format_options::{FormatOptions, LineEnding};

pub(super) fn can_trim_trailing_whitespace(line: &str, state: &DymolaFormatState) -> bool {
    if state.in_string || state.in_quoted_identifier || state.in_block_comment {
        return false;
    }

    let mut probe = state.clone();
    update_format_state(line, &mut probe);
    !probe.in_string && !probe.in_quoted_identifier && !probe.in_block_comment
}

#[derive(Clone, Default)]
pub(super) struct DymolaFormatState {
    section: FormatSection,
    in_string: bool,
    in_quoted_identifier: bool,
    in_block_comment: bool,
    previous_line_continues: bool,
    paren_depth: usize,
    brace_depth: usize,
    bracket_depth: usize,
    /// True when the previous character inside a string / quoted identifier was
    /// an unconsumed `\`, so the current character is escaped (MLS §2.2).
    escaped: bool,
    statement_continuation: bool,
    comment_anchored_layout: bool,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
enum FormatSection {
    #[default]
    Declaration,
    Equation,
    Algorithm,
}

pub(super) fn target_line_ending(source: &str, setting: LineEnding) -> &'static str {
    match setting {
        LineEnding::Lf => "\n",
        LineEnding::Crlf => "\r\n",
        LineEnding::Auto => {
            let crlf_count = source
                .as_bytes()
                .windows(2)
                .filter(|w| *w == b"\r\n")
                .count();
            let lf_count = source.as_bytes().iter().filter(|&&b| b == b'\n').count();
            if crlf_count > lf_count.saturating_sub(crlf_count) {
                "\r\n"
            } else {
                "\n"
            }
        }
    }
}

pub(super) fn format_dymola_line(
    line: &str,
    options: &FormatOptions,
    indent_level: &mut usize,
    state: &mut DymolaFormatState,
) -> String {
    if line.trim().is_empty() {
        if state.in_string || state.in_quoted_identifier || state.in_block_comment {
            update_format_state(line, state);
            return line.to_string();
        }
        state.previous_line_continues = false;
        state.comment_anchored_layout = false;
        return String::new();
    }

    let line = if options.normalize_indentation {
        normalized_indented_line(line, options, indent_level, state)
    } else if options.repair_missing_indentation {
        repair_missing_indented_line(line, options, indent_level, state)
    } else {
        line.to_string()
    };

    update_format_state(&line, state);
    line
}

fn normalized_indented_line(
    line: &str,
    options: &FormatOptions,
    indent_level: &mut usize,
    state: &DymolaFormatState,
) -> String {
    let trimmed = line.trim_start_matches([' ', '\t']);

    if should_preserve_leading_whitespace(trimmed, state) {
        return line.to_string();
    }

    if should_preserve_comment_anchored_layout(trimmed, state) {
        let _ = update_indent_level_for_line(trimmed, indent_level);
        return line.to_string();
    }

    let visual_indent = update_indent_level_for_line(trimmed, indent_level);
    indented_line(trimmed, options, visual_indent)
}

fn repair_missing_indented_line(
    line: &str,
    options: &FormatOptions,
    indent_level: &mut usize,
    state: &DymolaFormatState,
) -> String {
    let trimmed = line.trim_start_matches([' ', '\t']);

    if should_preserve_leading_whitespace(trimmed, state) {
        return line.to_string();
    }

    let visual_indent = update_indent_level_for_line(trimmed, indent_level);
    let has_existing_indent = trimmed.len() != line.len();
    if has_existing_indent
        || visual_indent == 0
        || should_preserve_unindented_layout_line(trimmed)
        || !should_repair_missing_indentation_line(trimmed, state)
    {
        return line.to_string();
    }

    indented_line(trimmed, options, visual_indent)
}

fn should_repair_missing_indentation_line(trimmed: &str, state: &DymolaFormatState) -> bool {
    match state.section {
        FormatSection::Equation => false,
        FormatSection::Declaration => is_builtin_scalar_declaration(trimmed),
        FormatSection::Algorithm => false,
    }
}

fn is_builtin_scalar_declaration(trimmed: &str) -> bool {
    let mut rest = trimmed;
    loop {
        let Some((word, tail)) = split_first_word(rest) else {
            return false;
        };
        if matches!(
            word,
            "final"
                | "parameter"
                | "constant"
                | "discrete"
                | "input"
                | "output"
                | "flow"
                | "stream"
        ) {
            rest = tail.trim_start();
            continue;
        }
        return matches!(word, "Real" | "Integer" | "Boolean" | "String");
    }
}

fn split_first_word(input: &str) -> Option<(&str, &str)> {
    let trimmed = input.trim_start();
    if trimmed.is_empty() {
        return None;
    }
    let end = trimmed
        .char_indices()
        .find_map(|(index, c)| {
            if c.is_whitespace() || matches!(c, '(' | '[' | ';') {
                Some(index)
            } else {
                None
            }
        })
        .unwrap_or(trimmed.len());
    Some((&trimmed[..end], &trimmed[end..]))
}

fn should_preserve_unindented_layout_line(trimmed: &str) -> bool {
    let lower = trimmed.to_ascii_lowercase();
    is_class_open_keyword(&lower)
        || is_dedent_keyword(&lower)
        || is_section_keyword(&lower)
        || lower.starts_with("extends ")
        || lower.starts_with("import ")
        || lower.starts_with("within ")
        || starts_with_keyword(&lower, "annotation")
        || lower.starts_with("connect(")
        || starts_with_keyword(&lower, "public")
        || starts_with_keyword(&lower, "protected")
}

fn indented_line(trimmed: &str, options: &FormatOptions, visual_indent: usize) -> String {
    let mut normalized = String::new();
    let unit = if options.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(options.indent_size)
    };
    for _ in 0..visual_indent {
        normalized.push_str(&unit);
    }
    normalized.push_str(trimmed);
    normalized
}

fn update_indent_level_for_line(trimmed: &str, indent_level: &mut usize) -> usize {
    let lower = trimmed.to_ascii_lowercase();
    let section = is_section_keyword(&lower);
    let branch = is_branch_keyword(&lower);
    if is_dedent_keyword(&lower) || branch {
        *indent_level = indent_level.saturating_sub(1);
    }

    let visual_indent = if section {
        indent_level.saturating_sub(1)
    } else {
        *indent_level
    };

    if is_indent_keyword(&lower) || branch {
        *indent_level += 1;
    }
    visual_indent
}

fn should_preserve_leading_whitespace(trimmed: &str, state: &DymolaFormatState) -> bool {
    state.in_string
        || state.in_quoted_identifier
        || state.in_block_comment
        || state.previous_line_continues
        || state.paren_depth > 0
        || state.brace_depth > 0
        || state.bracket_depth > 0
        || trimmed.starts_with('"')
        || starts_with_keyword(&trimmed.to_ascii_lowercase(), "annotation")
        || trimmed.starts_with("/*")
        || trimmed.starts_with("//")
}

fn should_preserve_comment_anchored_layout(trimmed: &str, state: &DymolaFormatState) -> bool {
    state.comment_anchored_layout && !is_comment_anchor_boundary(trimmed)
}

fn is_comment_anchor_boundary(trimmed: &str) -> bool {
    let lower = trimmed.to_ascii_lowercase();
    trimmed.is_empty()
        || is_section_keyword(&lower)
        || is_class_open_keyword(&lower)
        || is_branch_keyword(&lower)
        || lower.starts_with("end if")
        || lower.starts_with("end when")
        || starts_with_keyword(&lower, "annotation")
}

fn update_format_state(line: &str, state: &mut DymolaFormatState) {
    let was_in_block_comment = state.in_block_comment;
    update_format_section(line, state);
    let started_as_layout_comment = is_layout_comment_line(line, state);
    scan_line_lexical_state(line, state);
    finish_line_state(line, state);
    update_comment_anchored_layout(line, state, was_in_block_comment, started_as_layout_comment);
}

/// Advance the string / comment / delimiter state machine over one source line.
///
/// Quoted regions consume `\` escapes so that a literal trailing backslash
/// (`"a\\"`) does not leave `in_string` latched for the rest of the file.
fn scan_line_lexical_state(line: &str, state: &mut DymolaFormatState) {
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        if state.in_block_comment {
            if c == '*' && matches!(chars.peek(), Some('/')) {
                let _ = chars.next();
                state.in_block_comment = false;
            }
            continue;
        }
        if state.in_string || state.in_quoted_identifier {
            advance_quoted_state(c, state);
            continue;
        }
        if c == '"' {
            state.in_string = true;
            state.escaped = false;
            continue;
        }
        if c == '\'' {
            state.in_quoted_identifier = true;
            state.escaped = false;
            continue;
        }
        if c == '/' && matches!(chars.peek(), Some('/')) {
            break;
        }
        if c == '/' && matches!(chars.peek(), Some('*')) {
            let _ = chars.next();
            state.in_block_comment = true;
            continue;
        }
        update_delimiter_depth(c, state);
    }
}

/// Consume one character inside a string literal or quoted identifier.
fn advance_quoted_state(c: char, state: &mut DymolaFormatState) {
    if state.escaped {
        state.escaped = false;
        return;
    }
    match c {
        '\\' => state.escaped = true,
        '"' if state.in_string => state.in_string = false,
        '\'' if state.in_quoted_identifier => state.in_quoted_identifier = false,
        _ => {}
    }
}

fn update_delimiter_depth(c: char, state: &mut DymolaFormatState) {
    match c {
        '(' => state.paren_depth += 1,
        ')' => state.paren_depth = state.paren_depth.saturating_sub(1),
        '{' => state.brace_depth += 1,
        '}' => state.brace_depth = state.brace_depth.saturating_sub(1),
        '[' => state.bracket_depth += 1,
        ']' => state.bracket_depth = state.bracket_depth.saturating_sub(1),
        _ => {}
    }
}

fn finish_line_state(line: &str, state: &mut DymolaFormatState) {
    let was_statement_continuation = state.statement_continuation;
    let continues = line_continues(line, state);
    let lower = line.trim().to_ascii_lowercase();
    state.statement_continuation =
        continues && (was_statement_continuation || starts_statement_continuation(&lower));
    state.previous_line_continues = continues;
}

/// True when `lower` opens with `keyword` as a whole Modelica keyword.
///
/// Guards against identifier prefixes: `equationCount a;` declares a component
/// of type `equationCount` and must not switch the formatter into the equation
/// section (MLS §2.3.3 reserves `equation`, but `equationCount` is a legal
/// identifier).
fn starts_with_keyword(lower: &str, keyword: &str) -> bool {
    lower
        .strip_prefix(keyword)
        .is_some_and(|rest| !rest.starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_'))
}

fn update_format_section(line: &str, state: &mut DymolaFormatState) {
    if state.in_string || state.in_quoted_identifier || state.in_block_comment {
        return;
    }
    let lower = line.trim_start().to_ascii_lowercase();
    if starts_with_keyword(&lower, "initial equation") || starts_with_keyword(&lower, "equation") {
        state.section = FormatSection::Equation;
    } else if starts_with_keyword(&lower, "initial algorithm")
        || starts_with_keyword(&lower, "algorithm")
    {
        state.section = FormatSection::Algorithm;
    } else if starts_with_keyword(&lower, "public")
        || starts_with_keyword(&lower, "protected")
        || is_class_end_keyword(&lower)
    {
        state.section = FormatSection::Declaration;
    }
}

fn is_class_end_keyword(lower: &str) -> bool {
    (lower.starts_with("end ") || lower == "end;") && !is_control_end_keyword(lower)
}

fn is_control_end_keyword(lower: &str) -> bool {
    lower.starts_with("end if")
        || lower.starts_with("end for")
        || lower.starts_with("end while")
        || lower.starts_with("end when")
}

fn is_layout_comment_line(line: &str, state: &DymolaFormatState) -> bool {
    if !matches!(
        state.section,
        FormatSection::Equation | FormatSection::Algorithm
    ) {
        return false;
    }
    let trimmed = line.trim_start();
    trimmed.starts_with("//") || trimmed.starts_with("/*")
}

fn update_comment_anchored_layout(
    line: &str,
    state: &mut DymolaFormatState,
    was_in_block_comment: bool,
    started_as_layout_comment: bool,
) {
    if state.in_string || state.in_quoted_identifier {
        return;
    }

    let trimmed = line.trim_start();
    if is_comment_anchor_boundary(trimmed) {
        state.comment_anchored_layout = false;
        return;
    }

    if started_as_layout_comment && !state.in_block_comment {
        state.comment_anchored_layout = true;
        return;
    }

    if was_in_block_comment && !state.in_block_comment {
        state.comment_anchored_layout = true;
    }
}

fn line_continues(line: &str, state: &DymolaFormatState) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return false;
    }
    if state.in_string || state.in_block_comment {
        return true;
    }

    let lower = trimmed.to_ascii_lowercase();
    if is_section_keyword(&lower)
        || is_class_open_keyword(&lower)
        || is_dedent_keyword(&lower)
        || is_statement_branch(&lower, state)
        || lower.ends_with(';')
        || is_statement_then(&lower, state)
        || is_statement_loop(&lower, state)
        || is_statement_else(&lower, state)
        || lower.starts_with("/*")
        || lower.starts_with('*')
        || lower.ends_with("*/")
        || lower.starts_with("//")
    {
        return false;
    }

    true
}

fn is_dedent_keyword(lower: &str) -> bool {
    lower.starts_with("end ") || lower == "end;"
}

fn is_indent_keyword(lower: &str) -> bool {
    is_class_open_keyword(lower)
        || lower.starts_with("if ")
        || lower.starts_with("for ")
        || lower.starts_with("while ")
        || lower.starts_with("when ")
}

fn is_class_open_keyword(lower: &str) -> bool {
    let mut lowered = lower;
    if lowered.starts_with("operator ") {
        return true;
    }
    for prefix in [
        "encapsulated ",
        "partial ",
        "operator ",
        "expandable ",
        "pure ",
        "impure ",
    ] {
        if let Some(stripped) = lowered.strip_prefix(prefix) {
            lowered = stripped;
            if lowered.starts_with("operator ") {
                return true;
            }
        }
    }
    lowered.starts_with("model ")
        || lowered.starts_with("class ")
        || lowered.starts_with("block ")
        || lowered.starts_with("connector ")
        || lowered.starts_with("record ")
        || lowered.starts_with("type ")
        || lowered.starts_with("package ")
        || lowered.starts_with("function ")
}

fn is_section_keyword(lower: &str) -> bool {
    matches!(
        lower,
        "equation"
            | "algorithm"
            | "initial equation"
            | "initial algorithm"
            | "public"
            | "protected"
    )
}

fn is_branch_keyword(lower: &str) -> bool {
    lower.starts_with("else") || lower.starts_with("elseif ")
}

fn is_statement_branch(lower: &str, state: &DymolaFormatState) -> bool {
    !state.previous_line_continues
        && (lower.starts_with("else") && !lower.starts_with("elseif ")
            || lower.starts_with("elseif ") && lower.ends_with(" then"))
}

fn is_statement_then(lower: &str, state: &DymolaFormatState) -> bool {
    (!state.previous_line_continues || state.statement_continuation)
        && lower.ends_with(" then")
        && (state.statement_continuation
            || lower.starts_with("if ")
            || lower.starts_with("elseif "))
}

fn is_statement_loop(lower: &str, state: &DymolaFormatState) -> bool {
    (!state.previous_line_continues || state.statement_continuation)
        && lower.ends_with(" loop")
        && (state.statement_continuation
            || lower.starts_with("for ")
            || lower.starts_with("while "))
}

fn is_statement_else(lower: &str, state: &DymolaFormatState) -> bool {
    !state.previous_line_continues && lower.ends_with(" else")
}

fn starts_statement_continuation(lower: &str) -> bool {
    ((lower.starts_with("if ") || lower.starts_with("elseif ")) && !lower.ends_with(" then"))
        || ((lower.starts_with("for ") || lower.starts_with("while ")) && !lower.ends_with(" loop"))
}
