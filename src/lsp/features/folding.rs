//! Folding Ranges handler for Modelica files.
//!
//! Provides code folding support for:
//! - Class/model/function/record definitions
//! - Equation sections
//! - Algorithm sections
//! - If/when/for blocks
//! - Comments
//! - Annotations

use std::collections::HashMap;

use lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams, Uri};

use crate::ir::ast::{ClassDefinition, Equation, Statement};
use crate::lsp::utils::parse_document;

/// Handle folding range request
pub fn handle_folding_range(
    documents: &HashMap<Uri, String>,
    params: FoldingRangeParams,
) -> Option<Vec<FoldingRange>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let mut ranges = Vec::new();

    // Add comment folding ranges
    collect_comment_ranges(text, &mut ranges);

    // Add annotation folding ranges
    collect_annotation_ranges(text, &mut ranges);

    // Parse and add AST-based folding ranges
    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            collect_class_ranges(class, text, &mut ranges);
        }
    }

    Some(ranges)
}

/// Collect folding ranges for comments (multi-line comments and consecutive single-line comments)
fn collect_comment_ranges(text: &str, ranges: &mut Vec<FoldingRange>) {
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Multi-line comment /* ... */
        if line.contains("/*") && !line.contains("*/") {
            let start_line = i as u32;
            let mut end_line = start_line;

            // Find the closing */
            for (j, line) in lines.iter().enumerate().skip(i + 1) {
                if line.contains("*/") {
                    end_line = j as u32;
                    break;
                }
            }

            if end_line > start_line {
                ranges.push(FoldingRange {
                    start_line,
                    start_character: None,
                    end_line,
                    end_character: None,
                    kind: Some(FoldingRangeKind::Comment),
                    collapsed_text: None,
                });
            }
            i = end_line as usize + 1;
            continue;
        }

        // Consecutive single-line comments //
        if line.starts_with("//") {
            let start_line = i as u32;
            let mut end_line = start_line;

            // Count consecutive comment lines
            for (j, line) in lines.iter().enumerate().skip(i + 1) {
                if line.trim().starts_with("//") {
                    end_line = j as u32;
                } else {
                    break;
                }
            }

            // Only create a range if we have multiple lines
            if end_line > start_line {
                ranges.push(FoldingRange {
                    start_line,
                    start_character: None,
                    end_line,
                    end_character: None,
                    kind: Some(FoldingRangeKind::Comment),
                    collapsed_text: None,
                });
            }
            i = end_line as usize + 1;
            continue;
        }

        i += 1;
    }
}

/// Collect folding ranges for annotation() blocks that span multiple lines
/// We use FoldingRangeKind::Imports so annotations auto-fold on file open
/// (via VSCode's "editor.foldingImportsByDefault" setting)
fn collect_annotation_ranges(text: &str, ranges: &mut Vec<FoldingRange>) {
    let lines: Vec<&str> = text.lines().collect();

    for (line_idx, line) in lines.iter().enumerate() {
        // Find "annotation" keyword followed by optional whitespace and "("
        if let Some(ann_pos) = find_annotation_start(line) {
            // Check if annotation is inside a string or comment
            let before_ann = &line[..ann_pos];
            if before_ann.contains("//") || is_inside_string(line, ann_pos) {
                continue;
            }

            let start_line = line_idx as u32;

            // Find position after the opening parenthesis
            let after_ann = &line[ann_pos..];
            if let Some(paren_offset) = after_ann.find('(') {
                let paren_pos = ann_pos + paren_offset + 1; // Position after '('

                // Find the matching closing parenthesis
                if let Some(end_line) = find_matching_paren(&lines, line_idx, paren_pos) {
                    // Only create a folding range if it spans multiple lines
                    if end_line > start_line {
                        ranges.push(FoldingRange {
                            start_line,
                            start_character: Some(ann_pos as u32),
                            end_line,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Imports),
                            collapsed_text: Some("annotation(...)".to_string()),
                        });
                    }
                }
            }
        }
    }
}

/// Find the start position of "annotation" keyword followed by "(" (with optional whitespace)
fn find_annotation_start(line: &str) -> Option<usize> {
    let mut search_start = 0;
    while let Some(pos) = line[search_start..].find("annotation") {
        let abs_pos = search_start + pos;

        // Make sure it's a word boundary (not part of another identifier)
        if abs_pos > 0 {
            let prev_char = line[..abs_pos].chars().last().unwrap();
            if prev_char.is_alphanumeric() || prev_char == '_' {
                search_start = abs_pos + 10; // Skip past "annotation"
                continue;
            }
        }

        // Check what comes after "annotation"
        let after = &line[abs_pos + 10..]; // 10 = len("annotation")

        // Skip whitespace and check for '('
        let trimmed = after.trim_start();
        if trimmed.starts_with('(') {
            return Some(abs_pos);
        }

        // Not followed by '(', keep searching
        search_start = abs_pos + 10;
    }
    None
}

/// Check if position is inside a string literal (simple heuristic)
fn is_inside_string(line: &str, pos: usize) -> bool {
    let before = &line[..pos];
    // Count unescaped quotes before this position
    let quote_count = before.matches('"').count() - before.matches("\\\"").count();
    quote_count % 2 == 1
}

/// Find the matching closing parenthesis for an annotation starting at given position
fn find_matching_paren(lines: &[&str], start_line: usize, start_col: usize) -> Option<u32> {
    let mut depth = 1; // We start after the opening paren of annotation(
    let mut in_string = false;

    // Start from the position after "annotation("
    let first_line = lines.get(start_line)?;
    for ch in first_line[start_col..].chars() {
        match ch {
            '"' => in_string = !in_string,
            '(' if !in_string => depth += 1,
            ')' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    return Some(start_line as u32);
                }
            }
            _ => {}
        }
    }

    // Continue on subsequent lines
    for (offset, line) in lines.iter().enumerate().skip(start_line + 1) {
        for ch in line.chars() {
            match ch {
                '"' => in_string = !in_string,
                '(' if !in_string => depth += 1,
                ')' if !in_string => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(offset as u32);
                    }
                }
                _ => {}
            }
        }
    }

    None
}

/// Collect folding ranges for a class definition
fn collect_class_ranges(class: &ClassDefinition, text: &str, ranges: &mut Vec<FoldingRange>) {
    let class_name = &class.name.text;

    // Use the class location (spans from class keyword to end statement)
    let start_line = class.location.start_line.saturating_sub(1);
    let end_line = class.location.end_line.saturating_sub(1);

    // Add class folding range
    if end_line > start_line {
        ranges.push(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(format!("{:?} {} ...", class.class_type, class_name)),
        });
    }

    // Fold equation section using keyword token location
    if let Some(ref eq_kw) = class.equation_keyword {
        // Find the end of the equation section (next section or class end)
        let eq_start = eq_kw.location.start_line.saturating_sub(1);
        let eq_end = find_section_end(class, eq_start, end_line);
        if eq_end > eq_start {
            ranges.push(FoldingRange {
                start_line: eq_start,
                start_character: None,
                end_line: eq_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: Some("equation ...".to_string()),
            });
        }
    }

    // Fold initial equation section
    if let Some(ref eq_kw) = class.initial_equation_keyword {
        let eq_start = eq_kw.location.start_line.saturating_sub(1);
        let eq_end = find_section_end(class, eq_start, end_line);
        if eq_end > eq_start {
            ranges.push(FoldingRange {
                start_line: eq_start,
                start_character: None,
                end_line: eq_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: Some("initial equation ...".to_string()),
            });
        }
    }

    // Fold algorithm section
    if let Some(ref alg_kw) = class.algorithm_keyword {
        let alg_start = alg_kw.location.start_line.saturating_sub(1);
        let alg_end = find_section_end(class, alg_start, end_line);
        if alg_end > alg_start {
            ranges.push(FoldingRange {
                start_line: alg_start,
                start_character: None,
                end_line: alg_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: Some("algorithm ...".to_string()),
            });
        }
    }

    // Fold initial algorithm section
    if let Some(ref alg_kw) = class.initial_algorithm_keyword {
        let alg_start = alg_kw.location.start_line.saturating_sub(1);
        let alg_end = find_section_end(class, alg_start, end_line);
        if alg_end > alg_start {
            ranges.push(FoldingRange {
                start_line: alg_start,
                start_character: None,
                end_line: alg_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: Some("initial algorithm ...".to_string()),
            });
        }
    }

    // Collect ranges for equations with blocks (if, for, when)
    for eq in &class.equations {
        collect_equation_ranges(eq, text, ranges);
    }

    for eq in &class.initial_equations {
        collect_equation_ranges(eq, text, ranges);
    }

    // Collect ranges for statements with blocks
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_ranges(stmt, text, ranges);
        }
    }

    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_ranges(stmt, text, ranges);
        }
    }

    // Recursively process nested classes
    for nested_class in class.classes.values() {
        collect_class_ranges(nested_class, text, ranges);
    }
}

/// Find the end of a section by looking for the next section keyword or class end
/// Uses AST keyword tokens to determine section boundaries
fn find_section_end(class: &ClassDefinition, section_start: u32, class_end: u32) -> u32 {
    // Collect all section start lines (0-indexed)
    let mut section_starts: Vec<u32> = vec![];

    if let Some(ref kw) = class.equation_keyword {
        section_starts.push(kw.location.start_line.saturating_sub(1));
    }
    if let Some(ref kw) = class.initial_equation_keyword {
        section_starts.push(kw.location.start_line.saturating_sub(1));
    }
    if let Some(ref kw) = class.algorithm_keyword {
        section_starts.push(kw.location.start_line.saturating_sub(1));
    }
    if let Some(ref kw) = class.initial_algorithm_keyword {
        section_starts.push(kw.location.start_line.saturating_sub(1));
    }

    // Sort section starts
    section_starts.sort();

    // Find the next section after our current section
    for &start in &section_starts {
        if start > section_start {
            // End one line before the next section
            return start.saturating_sub(1);
        }
    }

    // No next section found, end at class end - 1 (before "end ClassName;")
    class_end.saturating_sub(1)
}

/// Collect folding ranges for equations with blocks
fn collect_equation_ranges(eq: &Equation, text: &str, ranges: &mut Vec<FoldingRange>) {
    match eq {
        Equation::If {
            cond_blocks,
            else_block: _,
        } => {
            if let Some(first_block) = cond_blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                let start_line = loc.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "if") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("if ... end if".to_string()),
                    });
                }
            }

            // Recursively process inner equations
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    collect_equation_ranges(inner_eq, text, ranges);
                }
            }
        }
        Equation::For { indices, equations } => {
            if let Some(first_index) = indices.first() {
                let start_line = first_index.ident.location.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "for") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("for ... end for".to_string()),
                    });
                }
            }

            for inner_eq in equations {
                collect_equation_ranges(inner_eq, text, ranges);
            }
        }
        Equation::When(blocks) => {
            if let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                let start_line = loc.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "when") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("when ... end when".to_string()),
                    });
                }
            }

            for block in blocks {
                for inner_eq in &block.eqs {
                    collect_equation_ranges(inner_eq, text, ranges);
                }
            }
        }
        _ => {}
    }
}

/// Collect folding ranges for statements with blocks
fn collect_statement_ranges(stmt: &Statement, text: &str, ranges: &mut Vec<FoldingRange>) {
    match stmt {
        Statement::For { indices, equations } => {
            if let Some(first_index) = indices.first() {
                let start_line = first_index.ident.location.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "for") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("for ... end for".to_string()),
                    });
                }
            }

            for inner_stmt in equations {
                collect_statement_ranges(inner_stmt, text, ranges);
            }
        }
        Statement::While(block) => {
            if let Some(loc) = block.cond.get_location() {
                let start_line = loc.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "while") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("while ... end while".to_string()),
                    });
                }
            }

            for inner_stmt in &block.stmts {
                collect_statement_ranges(inner_stmt, text, ranges);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            if let Some(first_block) = cond_blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                let start_line = loc.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "if") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("if ... end if".to_string()),
                    });
                }
            }

            for block in cond_blocks {
                for inner_stmt in &block.stmts {
                    collect_statement_ranges(inner_stmt, text, ranges);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    collect_statement_ranges(inner_stmt, text, ranges);
                }
            }
        }
        Statement::When(blocks) => {
            if let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                let start_line = loc.start_line.saturating_sub(1);
                if let Some(end_line) = find_end_keyword(text, start_line, "when") {
                    ranges.push(FoldingRange {
                        start_line,
                        start_character: None,
                        end_line,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: Some("when ... end when".to_string()),
                    });
                }
            }

            for block in blocks {
                for inner_stmt in &block.stmts {
                    collect_statement_ranges(inner_stmt, text, ranges);
                }
            }
        }
        _ => {}
    }
}

/// Find the line containing "end <keyword>" starting from a given line
fn find_end_keyword(text: &str, start_line: u32, keyword: &str) -> Option<u32> {
    let lines: Vec<&str> = text.lines().collect();
    let end_pattern = format!("end {}", keyword);

    let mut depth = 0;
    for (i, line) in lines.iter().enumerate().skip(start_line as usize) {
        let trimmed = line.trim();

        // Track nesting depth
        if trimmed.starts_with(keyword) && !trimmed.starts_with("end") {
            depth += 1;
        }

        if trimmed.starts_with(&end_pattern) || trimmed == &end_pattern[..end_pattern.len()] {
            if depth <= 1 {
                return Some(i as u32);
            }
            depth -= 1;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collect_comment_ranges_multiline() {
        let text = "/* This is\na multi-line\ncomment */\ncode here";
        let mut ranges = Vec::new();
        collect_comment_ranges(text, &mut ranges);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 2);
    }

    #[test]
    fn test_collect_comment_ranges_consecutive() {
        let text = "// Comment 1\n// Comment 2\n// Comment 3\ncode";
        let mut ranges = Vec::new();
        collect_comment_ranges(text, &mut ranges);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 2);
    }

    #[test]
    fn test_find_end_keyword() {
        let text = "for i in 1:10 loop\n  x := i;\nend for;";
        let end_line = find_end_keyword(text, 0, "for");
        assert_eq!(end_line, Some(2));
    }

    #[test]
    fn test_collect_annotation_ranges_single_line() {
        let text = "x = 1 annotation(Evaluate=true);";
        let mut ranges = Vec::new();
        collect_annotation_ranges(text, &mut ranges);
        // Single-line annotation should not create a folding range
        assert_eq!(ranges.len(), 0);
    }

    #[test]
    fn test_collect_annotation_ranges_multiline() {
        let text = r#"  annotation(
    Documentation(info="<html>
      <p>Test</p>
    </html>"));"#;
        let mut ranges = Vec::new();
        collect_annotation_ranges(text, &mut ranges);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
        assert_eq!(
            ranges[0].collapsed_text,
            Some("annotation(...)".to_string())
        );
    }

    #[test]
    fn test_collect_annotation_ranges_nested_parens() {
        let text = r#"annotation(
  Placement(transformation(extent={{-80,70},{-60,90}})),
  Documentation(info="text"))"#;
        let mut ranges = Vec::new();
        collect_annotation_ranges(text, &mut ranges);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 2);
    }

    #[test]
    fn test_collect_annotation_ranges_with_whitespace() {
        // Test annotation with space before parenthesis
        let text = r#"  annotation (
    Documentation(info="<html>
      <p>Test</p>
    </html>"));"#;
        let mut ranges = Vec::new();
        collect_annotation_ranges(text, &mut ranges);
        assert_eq!(
            ranges.len(),
            1,
            "Expected 1 folding range for 'annotation ('"
        );
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn test_find_annotation_start() {
        // No space
        assert_eq!(find_annotation_start("  annotation(x=1)"), Some(2));
        // With space
        assert_eq!(find_annotation_start("  annotation (x=1)"), Some(2));
        // With multiple spaces
        assert_eq!(find_annotation_start("annotation   (x=1)"), Some(0));
        // With tab
        assert_eq!(find_annotation_start("annotation\t(x=1)"), Some(0));
        // Not followed by paren
        assert_eq!(find_annotation_start("annotation x"), None);
        // Part of another word
        assert_eq!(find_annotation_start("myannotation(x=1)"), None);
    }

    #[test]
    fn test_algorithm_section_folding() {
        use lsp_types::Uri;
        use std::collections::HashMap;

        let text = r#"model Test
  Real x;
algorithm
  x := 1;
  x := x + 1;
  x := x * 2;
end Test;
"#;
        let uri: Uri = "file:///tmp/test.mo".parse().unwrap();
        let mut documents = HashMap::new();
        documents.insert(uri.clone(), text.to_string());

        let params = FoldingRangeParams {
            text_document: lsp_types::TextDocumentIdentifier { uri },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let ranges = handle_folding_range(&documents, params).unwrap();

        // Should have at least: model fold + algorithm fold
        let algo_folds: Vec<_> = ranges
            .iter()
            .filter(|r| r.collapsed_text == Some("algorithm ...".to_string()))
            .collect();

        assert!(
            !algo_folds.is_empty(),
            "Expected algorithm folding range, got: {:?}",
            ranges
        );
    }
}
