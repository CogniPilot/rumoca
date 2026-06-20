//! Shared scanning helpers for the span/size validation suites.

use super::super::*;

pub(crate) fn is_line_count_checked_source_file(path: &Path) -> bool {
    let rel = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    !rel.contains("/generated/")
}

pub(crate) fn expression_span_dummy_fallback_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_expression_span_dummy_fallback(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn line_has_expression_span_dummy_fallback(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && line.contains(".span().unwrap_or(")
        && (line.contains("Span::DUMMY") || line.contains("rumoca_core::Span::DUMMY"))
}

pub(crate) fn is_dummy_span_fallback_counted_file(path: &Path) -> bool {
    is_production_span_debt_counted_file(path)
}

pub(crate) fn is_production_span_debt_counted_file(path: &Path) -> bool {
    let rel = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    !rel.contains("/generated/")
        && !rel.contains("/tests/")
        && !rel.ends_with("/tests.rs")
        && !rel.ends_with("_tests.rs")
        && !rel.ends_with("/test_support.rs")
        && !rel.ends_with("architecture_hardening_test.rs")
        && !rel.contains("/architecture_hardening/")
}

pub(crate) fn dummy_span_fallback_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_dummy_span_fallback(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn source_map_location_to_span_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_source_map_location_to_span_call(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn line_has_source_map_location_to_span_call(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains(".location_to_span(") || line.contains("fn location_to_span(&self"))
}

pub(crate) fn line_has_dummy_span_fallback(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    code.contains("Span::DUMMY")
        && (code.contains("unwrap_or(")
            || code.contains("unwrap_or_else(")
            || code.contains("map_or("))
}

pub(crate) fn default_span_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_default_span_construction(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn line_has_default_span_construction(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains("Span::default()")
            || line.contains("span: Default::default()")
            || line.contains("span: std::default::Default::default()")
            || line.contains("span: core::default::Default::default()"))
}

pub(crate) fn dummy_span_use_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_dummy_span_use(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn source_free_span_escape_hatch_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_source_free_span_escape_hatch(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn generated_dummy_subscript_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_generated_dummy_subscript(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn generated_span_helper_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_generated_span_helper(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn raw_generated_subscript_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_raw_generated_subscript_constructor(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn line_has_generated_span_helper(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//") && trimmed.contains("fn generated_") && trimmed.contains("_span")
}

pub(crate) fn line_has_raw_generated_subscript_constructor(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains("Subscript::generated_index(")
            || line.contains("Subscript::generated_colon(")
            || line.contains("Subscript::generated_expr("))
}

pub(crate) fn line_has_generated_dummy_subscript(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && line.contains("Span::DUMMY")
        && (line.contains("Subscript::generated_index(")
            || line.contains("Subscript::generated_colon(")
            || line.contains("Subscript::generated_expr(")
            || line.contains("Subscript::colon("))
}

pub(crate) fn line_has_dummy_span_use(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (code.contains("Span::DUMMY") || code.contains("rumoca_core::Span::DUMMY"))
}

pub(crate) fn line_has_unspanned_test_span_use(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (code.contains("unspanned_test_span()")
            || code.contains("unspanned_solve_test_span()")
            || code.contains("unspanned_event_action_test_span()")
            || code.contains("unspanned_residual_test_span()")
            || code.contains("unspanned_runtime_assignment_test_span()")
            || code.contains("unspanned_dynamic_event_test_span()")
            || code.contains("unspanned_layout_test_span()")
            || code.contains("unspanned_solve_model_test_span()")
            || code.contains("unspanned_ad_test_span()")
            || code.contains("unspanned_fft_test_span()")
            || code.contains("unspanned_compile_time_test_span()")
            || code.contains("unspanned_random_test_span()")
            || code.contains("unspanned_root_condition_test_span()")
            || code.contains("unspanned_derivative_rhs_test_span()")
            || code.contains("unspanned_projection_selection_test_span()")
            || code.contains("unspanned_linear_parts_test_span()")
            || code.contains("unspanned_runtime_intrinsics_test_span()")
            || code.contains("unspanned_statement_test_span()")
            || code.contains("unspanned_function_projection_test_span()")
            || code.contains("unspanned_discrete_update_test_span()")
            || code.contains("unspanned_complex_operator_test_span()")
            || code.contains("unspanned_stencil_test_span()")
            || code.contains("unspanned_array_values_test_span()"))
}

pub(crate) fn line_has_named_unspanned_test_span_use(line: &str, helper_call: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let trimmed = line.trim_start();
    !trimmed.starts_with("//") && code.contains(helper_call)
}

pub(crate) fn line_has_dummy_source_id_use(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (code.contains("SourceId::DUMMY") || code.contains("rumoca_core::SourceId::DUMMY"))
}

pub(crate) fn line_has_source_free_span_escape_hatch(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    code.contains("Span::source_free_serde_default(")
        || code.contains("source_free_span()")
        || code.contains("ScalarProgramBlock::source_free(")
        || code.contains("source_free_generated_index(")
}

pub(crate) fn line_without_literals_or_line_comment(line: &str) -> String {
    let mut code = String::with_capacity(line.len());
    let mut idx = 0;
    while idx < line.len() {
        if line[idx..].starts_with("//") {
            break;
        }

        if let Some(end) = raw_string_literal_end(line, idx) {
            code.push('"');
            idx = end;
            continue;
        }

        let Some(ch) = line[idx..].chars().next() else {
            break;
        };
        if ch == '"' {
            code.push('"');
            idx = cooked_string_literal_end(line, idx + ch.len_utf8());
            continue;
        }

        code.push(ch);
        idx += ch.len_utf8();
    }
    code
}

pub(crate) fn cooked_string_literal_end(line: &str, mut idx: usize) -> usize {
    let mut escaped = false;
    while idx < line.len() {
        let Some(ch) = line[idx..].chars().next() else {
            break;
        };
        idx += ch.len_utf8();
        if escaped {
            escaped = false;
            continue;
        }
        match ch {
            '\\' => escaped = true,
            '"' => break,
            _ => {}
        }
    }
    idx
}

pub(crate) fn raw_string_literal_end(line: &str, start: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut idx = match bytes.get(start..) {
        Some([b'r', ..]) => start + 1,
        Some([b'b', b'r', ..]) => start + 2,
        _ => return None,
    };

    let mut hashes = 0;
    while bytes.get(idx) == Some(&b'#') {
        idx += 1;
        hashes += 1;
    }
    if bytes.get(idx) != Some(&b'"') {
        return None;
    }

    let mut search = idx + 1;
    while search < bytes.len() {
        if raw_string_closes_at(bytes, search, hashes) {
            return Some(search + 1 + hashes);
        }
        search += 1;
    }
    Some(bytes.len())
}

pub(crate) fn raw_string_closes_at(bytes: &[u8], quote_idx: usize, hashes: usize) -> bool {
    if bytes.get(quote_idx) != Some(&b'"') {
        return false;
    }
    let close_end = quote_idx + 1 + hashes;
    close_end <= bytes.len()
        && bytes[quote_idx + 1..close_end]
            .iter()
            .all(|byte| *byte == b'#')
}

pub(crate) fn production_source_lines(content: &str) -> impl Iterator<Item = (usize, &str)> {
    let mut pending_cfg_test = false;
    let mut skipped_cfg_test_depth = 0i32;
    content.lines().enumerate().filter(move |(_, line)| {
        if skipped_cfg_test_depth > 0 {
            skipped_cfg_test_depth += brace_delta(line);
            return false;
        }

        let trimmed = line.trim_start();
        if pending_cfg_test {
            if trimmed.starts_with("#[") {
                return false;
            }
            pending_cfg_test = false;
            if starts_cfg_test_item(trimmed) {
                skipped_cfg_test_depth = brace_delta(line).max(0);
                return false;
            }
        }

        if trimmed.starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            return false;
        }
        true
    })
}

pub(crate) fn starts_cfg_test_item(trimmed: &str) -> bool {
    trimmed.starts_with("mod ")
        || trimmed.starts_with("pub mod ")
        || trimmed.starts_with("fn ")
        || trimmed.starts_with("pub fn ")
        || trimmed.starts_with("use ")
}

pub(crate) fn brace_delta(line: &str) -> i32 {
    line.chars().fold(0, |delta, ch| match ch {
        '{' => delta + 1,
        '}' => delta - 1,
        _ => delta,
    })
}

pub(crate) fn numeric_source_id_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    content
        .lines()
        .enumerate()
        .filter(|(_, line)| line_has_numeric_source_id_constructor(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn line_has_numeric_source_id_constructor(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let mut remaining = code.as_str();
    while let Some(start) = remaining.find("SourceId(") {
        let after = &remaining[start + "SourceId(".len()..];
        if after.chars().next().is_some_and(|ch| ch.is_ascii_digit()) {
            return true;
        }
        remaining = after;
    }
    false
}

pub(crate) fn source_zero_span_offenders(phase_src_dir: &Path, banned: &[&str]) -> Vec<String> {
    let mut files = Vec::new();
    collect_rs_files(phase_src_dir, &mut files);
    files
        .into_iter()
        .flat_map(|path| source_zero_span_offenders_in_file(&path, banned))
        .collect()
}

pub(crate) fn source_zero_span_offenders_in_file(path: &Path, banned: &[&str]) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read phase source file");
    let production_content = strip_cfg_test_modules(&content);
    production_content
        .lines()
        .enumerate()
        .filter(|(_, line)| banned.iter().any(|needle| line.contains(needle)))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

pub(crate) fn strip_cfg_test_modules(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut pending_cfg_test = false;
    let mut skipping_depth: Option<usize> = None;

    for line in content.lines() {
        let trimmed = line.trim_start();
        if let Some(depth) = skipping_depth.as_mut() {
            *depth = depth.saturating_add(line.matches('{').count());
            *depth = depth.saturating_sub(line.matches('}').count());
            if *depth == 0 {
                skipping_depth = None;
            }
            out.push('\n');
            continue;
        }

        if trimmed.starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            out.push('\n');
            continue;
        }

        if pending_cfg_test && trimmed.starts_with("mod tests") {
            let depth = line
                .matches('{')
                .count()
                .saturating_sub(line.matches('}').count());
            if depth > 0 {
                skipping_depth = Some(depth);
            }
            pending_cfg_test = false;
            out.push('\n');
            continue;
        }

        pending_cfg_test = false;
        out.push_str(line);
        out.push('\n');
    }

    out
}
