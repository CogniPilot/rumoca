//! Built-in Jinja semantic-fallback and schedule-construction ratchet.

use super::content_fingerprint;
use std::collections::{BTreeMap, BTreeSet};

pub(super) fn template_source_set_finding(templates: &[(String, String)]) -> String {
    let mut ordered = templates.iter().collect::<Vec<_>>();
    ordered.sort_by(|left, right| left.0.cmp(&right.0));
    let mut hasher = blake3::Hasher::new();
    for (path, source) in &ordered {
        hasher.update(&(path.len() as u64).to_le_bytes());
        hasher.update(path.as_bytes());
        hasher.update(&(source.len() as u64).to_le_bytes());
        hasher.update(source.as_bytes());
    }
    format!(
        "<phase-codegen>::<template-source-set>:source-review:templates-{}:{}",
        ordered.len(),
        hasher.finalize().to_hex()
    )
}

pub(super) fn analyze_template_source(path: &str, source: &str) -> BTreeSet<String> {
    let mut kinds = Vec::<(usize, String, String)>::new();
    let blocks = match jinja_code_blocks(source) {
        Ok(blocks) => blocks,
        Err(error) => {
            return BTreeSet::from([format!(
                "{path}::<template-file>:template-lex-error:{error}"
            )]);
        }
    };
    let logic_inventory = (!blocks.is_empty()).then(|| {
        format!(
            "{path}::<template-file>:template-reviewed-logic:{}:{}",
            blocks.len(),
            template_logic_fingerprint(&blocks)
        )
    });
    let trim_inventory = (!blocks.is_empty()).then(|| {
        format!(
            "{path}::<template-file>:template-trim-policy:{}:{}",
            blocks.len(),
            template_trim_fingerprint(&blocks)
        )
    });
    for block in &blocks {
        collect_template_block_kinds(block, &mut kinds);
    }
    let control_blocks = blocks
        .iter()
        .filter(|block| block.control)
        .collect::<Vec<_>>();
    for pair in control_blocks.windows(2) {
        let (first, second) = (&pair[0].code, &pair[1].code);
        let binding_then_start = first.starts_with("if")
            && first.contains("binding")
            && second.starts_with("elif")
            && second.contains("start");
        let start_then_binding = first.starts_with("if")
            && first.contains("start")
            && second.starts_with("elif")
            && second.contains("binding");
        if binding_then_start || start_then_binding {
            record_template_kind(
                &mut kinds,
                &[pair[0], pair[1]],
                "template-fallback:binding-from-start",
            );
        }
    }
    for triple in control_blocks.windows(3) {
        let first = &triple[0].code;
        let middle = &triple[1].code;
        let last = &triple[2].code;
        let nested_binding_then_start = first.starts_with("if")
            && first.contains("binding")
            && middle == "else"
            && last.starts_with("if")
            && last.contains("start");
        let nested_start_then_binding = first.starts_with("if")
            && first.contains("start")
            && middle == "else"
            && last.starts_with("if")
            && last.contains("binding");
        if nested_binding_then_start || nested_start_then_binding {
            record_template_kind(
                &mut kinds,
                &[triple[0], triple[1], triple[2]],
                "template-fallback:binding-from-start",
            );
        }
    }

    let mut ordinals = BTreeMap::<(usize, String, String), usize>::new();
    let mut findings = BTreeSet::new();
    findings.extend(logic_inventory);
    findings.extend(trim_inventory);
    for (line, fingerprint, kind) in kinds {
        let ordinal = ordinals
            .entry((line, fingerprint.clone(), kind.clone()))
            .or_default();
        *ordinal += 1;
        findings.insert(format!(
            "{path}::<template-line-{line}-{fingerprint}>:{kind}#{ordinal}"
        ));
    }
    findings
}

fn collect_template_block_kinds(block: &JinjaBlock, kinds: &mut Vec<(usize, String, String)>) {
    for _ in block.code.match_indices("range(") {
        record_template_kind(kinds, &[block], "template-domain-expansion:range");
    }
    for (offset, _) in block.code.match_indices("default(") {
        let receiver = block.code[..offset].strip_suffix('|').unwrap_or("");
        let allowed_final_flag =
            receiver.starts_with("flat.variable_final_flags[") && receiver.ends_with(']');
        if receiver != "model_name" && !allowed_final_flag {
            record_template_kind(kinds, &[block], "template-fallback:default");
        }
    }
    for needle in ["|sum", "reduce(", "fold("] {
        for _ in block.code.match_indices(needle) {
            record_template_kind(kinds, &[block], "template-semantic-reduction");
        }
    }
    if block.code.starts_with("set")
        && let Some((target, _)) = block.code.split_once('=')
        && target.contains('.')
    {
        record_template_kind(kinds, &[block], "template-namespace-state-mutation");
    }
    if [
        "or0",
        "or0.0",
        "orfalse",
        "ortrue",
        "or[]",
        "or{}",
        "or(0)",
        "or(0.0)",
        "or(false)",
        "or(true)",
        "or([])",
        "or({})",
    ]
    .iter()
    .any(|needle| block.code.contains(needle))
    {
        record_template_kind(kinds, &[block], "template-fallback:literal");
    }
    if block.code.starts_with("set")
        && block.code.split_once('=').is_some_and(|(_, value)| {
            matches!(value.split('|').next(), Some("range" | "default" | "sum"))
        })
    {
        record_template_kind(kinds, &[block], "template-semantic-operation-alias");
    }
    if block.code.contains("binding")
        && block.code.contains("start")
        && block.code.contains("if")
        && block.code.contains("else")
    {
        record_template_kind(kinds, &[block], "template-fallback:binding-from-start");
    }
}

fn template_logic_fingerprint(blocks: &[JinjaBlock]) -> String {
    let mut hasher = blake3::Hasher::new();
    for block in blocks {
        hasher.update(&[u8::from(block.control)]);
        let payload = block.fingerprint_payload.as_bytes();
        hasher.update(&(payload.len() as u64).to_le_bytes());
        hasher.update(payload);
    }
    hasher.finalize().to_hex().to_string()
}

fn template_trim_fingerprint(blocks: &[JinjaBlock]) -> String {
    let mut hasher = blake3::Hasher::new();
    for block in blocks {
        hasher.update(&[u8::from(block.trim_left), u8::from(block.trim_right)]);
    }
    hasher.finalize().to_hex().to_string()
}

fn record_template_kind(
    kinds: &mut Vec<(usize, String, String)>,
    blocks: &[&JinjaBlock],
    kind: &str,
) {
    let owner = blocks
        .iter()
        .map(|block| block.fingerprint_payload.as_str())
        .collect::<Vec<_>>()
        .join("\u{0}");
    kinds.push((
        blocks.first().expect("template finding has an owner").line,
        content_fingerprint(&owner),
        kind.to_string(),
    ));
}

struct JinjaBlock {
    control: bool,
    line: usize,
    trim_left: bool,
    trim_right: bool,
    fingerprint_payload: String,
    code: String,
}

fn jinja_code_blocks(source: &str) -> Result<Vec<JinjaBlock>, &'static str> {
    let mut blocks = Vec::new();
    let mut consumed = 0usize;
    while let Some((open, kind)) = next_opening_delimiter(source, consumed) {
        if kind == OpeningDelimiter::Comment {
            let body_start = open + 2;
            let end = source[body_start..].find("#}").ok_or("unclosed-comment")?;
            consumed = body_start + end + 2;
            continue;
        }
        let is_control = kind == OpeningDelimiter::Control;
        let close = if is_control { "%}" } else { "}}" };
        let body_start = open + 2;
        let end = find_unquoted_close(source, body_start, close).ok_or(if is_control {
            "unclosed-control"
        } else {
            "unclosed-expression"
        })?;
        let block = make_jinja_block(source, open, body_start, end, is_control);
        let is_raw = is_control && block.code == "raw";
        blocks.push(block);
        consumed = end + 2;
        if is_raw {
            let (raw_end, after_raw) = find_endraw(source, consumed)?;
            blocks.push(raw_end);
            consumed = after_raw;
        }
    }
    Ok(blocks)
}

pub(super) fn template_uses_registered_command(
    source: &str,
    kind: &str,
    public_name: &str,
) -> Result<bool, &'static str> {
    let blocks = jinja_code_blocks(source)?;
    let code = blocks
        .iter()
        .map(|block| compact_usage_code(&block.fingerprint_payload))
        .collect::<Vec<_>>();
    if kind == "function"
        && blocks
            .iter()
            .any(|block| block_binds_name(&block.fingerprint_payload, public_name))
    {
        return Ok(false);
    }
    Ok(code
        .iter()
        .any(|code| code_uses_command(code, kind, public_name)))
}

fn compact_usage_code(code: &str) -> String {
    code.chars()
        .filter(|character| !character.is_whitespace())
        .collect()
}

fn block_binds_name(code: &str, public_name: &str) -> bool {
    let identifiers = unquoted_identifiers(code);
    let Some((_, keyword)) = identifiers.first() else {
        return false;
    };
    match *keyword {
        "macro" => identifiers
            .get(1)
            .is_some_and(|(_, identifier)| *identifier == public_name),
        "set" => {
            let target_end = first_unquoted_separator(code, &['=', '|']).unwrap_or(code.len());
            identifiers
                .iter()
                .skip(1)
                .take_while(|(offset, _)| *offset < target_end)
                .any(|(_, identifier)| *identifier == public_name)
        }
        "for" => identifiers
            .iter()
            .skip(1)
            .take_while(|(_, identifier)| *identifier != "in")
            .any(|(_, identifier)| *identifier == public_name),
        "with" => identifiers
            .iter()
            .skip(1)
            .any(|(_, identifier)| *identifier == public_name),
        "import" | "from" => identifiers
            .iter()
            .skip(1)
            .any(|(_, identifier)| *identifier == public_name),
        _ => false,
    }
}

fn unquoted_identifiers(code: &str) -> Vec<(usize, &str)> {
    let mut identifiers = Vec::new();
    let mut cursor = 0;
    let mut quote = None;
    let mut escaped = false;
    while cursor < code.len() {
        let character = code[cursor..]
            .chars()
            .next()
            .expect("cursor remains on a character boundary");
        let width = character.len_utf8();
        if let Some(active_quote) = quote {
            if escaped {
                escaped = false;
            } else if character == '\\' {
                escaped = true;
            } else if character == active_quote {
                quote = None;
            }
            cursor += width;
            continue;
        }
        if matches!(character, '\'' | '"') {
            quote = Some(character);
            cursor += width;
            continue;
        }
        if character == '_' || character.is_ascii_alphabetic() {
            let start = cursor;
            cursor = identifier_end(code, cursor + width);
            identifiers.push((start, &code[start..cursor]));
        } else {
            cursor += width;
        }
    }
    identifiers
}

fn identifier_end(code: &str, mut cursor: usize) -> usize {
    while cursor < code.len() {
        let next = code[cursor..]
            .chars()
            .next()
            .expect("cursor remains on a character boundary");
        if !is_jinja_identifier(next) {
            break;
        }
        cursor += next.len_utf8();
    }
    cursor
}

fn first_unquoted_separator(code: &str, separators: &[char]) -> Option<usize> {
    let mut quote = None;
    let mut escaped = false;
    for (offset, character) in code.char_indices() {
        if let Some(active_quote) = quote {
            if escaped {
                escaped = false;
            } else if character == '\\' {
                escaped = true;
            } else if character == active_quote {
                quote = None;
            }
        } else if matches!(character, '\'' | '"') {
            quote = Some(character);
        } else if separators.contains(&character) {
            return Some(offset);
        }
    }
    None
}

fn code_uses_command(code: &str, kind: &str, public_name: &str) -> bool {
    let mut quote = None;
    let mut escaped = false;
    for (offset, character) in code.char_indices() {
        if let Some(active_quote) = quote {
            if escaped {
                escaped = false;
            } else if character == '\\' {
                escaped = true;
            } else if character == active_quote {
                quote = None;
            }
            continue;
        }
        if matches!(character, '\'' | '"') {
            quote = Some(character);
            continue;
        }
        if command_starts_at(code, offset, kind, public_name) {
            return true;
        }
    }
    false
}

fn command_starts_at(code: &str, offset: usize, kind: &str, public_name: &str) -> bool {
    match kind {
        "filter" if code[offset..].starts_with('|') => {
            let name_start = offset + 1;
            let name_end = name_start + public_name.len();
            exact_identifier_at(code, name_start, public_name) && !code[name_end..].starts_with('.')
        }
        "function" if exact_identifier_at(code, offset, public_name) => {
            let before = code[..offset].chars().next_back();
            let after = offset + public_name.len();
            !before.is_some_and(|character| is_jinja_identifier(character) || character == '.')
                && code[after..].starts_with('(')
        }
        _ => false,
    }
}

fn exact_identifier_at(code: &str, offset: usize, identifier: &str) -> bool {
    let Some(tail) = code.get(offset..) else {
        return false;
    };
    if !tail.starts_with(identifier) {
        return false;
    }
    tail[identifier.len()..]
        .chars()
        .next()
        .is_none_or(|character| !is_jinja_identifier(character))
}

fn is_jinja_identifier(character: char) -> bool {
    character == '_' || character.is_ascii_alphanumeric()
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OpeningDelimiter {
    Expression,
    Control,
    Comment,
}

fn next_opening_delimiter(source: &str, start: usize) -> Option<(usize, OpeningDelimiter)> {
    let rest = &source[start..];
    [
        ("{{", OpeningDelimiter::Expression),
        ("{%", OpeningDelimiter::Control),
        ("{#", OpeningDelimiter::Comment),
    ]
    .into_iter()
    .filter_map(|(needle, kind)| rest.find(needle).map(|offset| (start + offset, kind)))
    .min_by_key(|(offset, _)| *offset)
}

fn find_unquoted_close(source: &str, start: usize, close: &str) -> Option<usize> {
    let mut quote = None;
    let mut escaped = false;
    for (relative, character) in source[start..].char_indices() {
        let offset = start + relative;
        if let Some(active_quote) = quote {
            if escaped {
                escaped = false;
            } else if character == '\\' {
                escaped = true;
            } else if character == active_quote {
                quote = None;
            }
            continue;
        }
        if matches!(character, '\'' | '"') {
            quote = Some(character);
        } else if source[offset..].starts_with(close) {
            return Some(offset);
        }
    }
    None
}

fn find_endraw(source: &str, mut cursor: usize) -> Result<(JinjaBlock, usize), &'static str> {
    loop {
        let Some(relative) = source[cursor..].find("{%") else {
            return Err("unclosed-raw");
        };
        let open = cursor + relative;
        let body_start = open + 2;
        let end = find_unquoted_close(source, body_start, "%}").ok_or("unclosed-raw-control")?;
        let block = make_jinja_block(source, open, body_start, end, true);
        if block.code == "endraw" {
            return Ok((block, end + 2));
        }
        cursor = end + 2;
    }
}

fn make_jinja_block(
    source: &str,
    open: usize,
    body_start: usize,
    end: usize,
    is_control: bool,
) -> JinjaBlock {
    let raw_body = &source[body_start..end];
    let trim_left = raw_body.starts_with('-');
    let trim_right = raw_body.ends_with('-');
    let without_leading_marker = if trim_left { &raw_body[1..] } else { raw_body };
    let without_markers = if trim_right && !without_leading_marker.is_empty() {
        &without_leading_marker[..without_leading_marker.len() - 1]
    } else {
        without_leading_marker
    };
    let fingerprint_payload = without_markers.trim();
    JinjaBlock {
        control: is_control,
        line: source[..open].bytes().filter(|byte| *byte == b'\n').count() + 1,
        trim_left,
        trim_right,
        fingerprint_payload: fingerprint_payload.to_string(),
        code: fingerprint_payload
            .chars()
            .filter(|character| !character.is_whitespace())
            .collect::<String>()
            .to_ascii_lowercase(),
    }
}
