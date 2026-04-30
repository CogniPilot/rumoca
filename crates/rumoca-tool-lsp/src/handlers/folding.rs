//! Folding ranges handler for Modelica files.

use lsp_types::{FoldingRange, FoldingRangeKind};
use rumoca_compile::parsing::ast;

/// Handle folding ranges request.
pub fn handle_folding_ranges(ast: &ast::StoredDefinition, source: &str) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();

    // Class body folding
    for (_, class) in &ast.classes {
        collect_class_folds(class, &mut ranges);
    }

    // Comment folding from text
    collect_comment_folds(source, &mut ranges);

    ranges
}

fn collect_class_folds(class: &ast::ClassDef, ranges: &mut Vec<FoldingRange>) {
    let loc = &class.location;
    if loc.start_line > 0 && loc.end_line > loc.start_line {
        ranges.push(FoldingRange {
            start_line: loc.start_line.saturating_sub(1),
            start_character: None,
            end_line: loc.end_line.saturating_sub(1),
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: None,
        });
    }

    // Equation section
    if let Some(kw) = &class.equation_keyword {
        let start = kw.location.start_line.saturating_sub(1);
        let end = find_section_end(class, start);
        if end > start {
            ranges.push(FoldingRange {
                start_line: start,
                start_character: None,
                end_line: end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            });
        }
    }

    // Algorithm section
    if let Some(kw) = &class.algorithm_keyword {
        let start = kw.location.start_line.saturating_sub(1);
        let end = find_section_end(class, start);
        if end > start {
            ranges.push(FoldingRange {
                start_line: start,
                start_character: None,
                end_line: end,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            });
        }
    }

    // Nested classes
    for (_, nested) in &class.classes {
        collect_class_folds(nested, ranges);
    }
}

fn find_section_end(class: &ast::ClassDef, section_start: u32) -> u32 {
    // Section ends at the class end or at the next section keyword
    let class_end = class.location.end_line.saturating_sub(1);
    let mut end = class_end;

    // Check if another section starts after this one
    let keywords = [
        class.equation_keyword.as_ref(),
        class.initial_equation_keyword.as_ref(),
        class.algorithm_keyword.as_ref(),
        class.initial_algorithm_keyword.as_ref(),
    ];
    for kw in keywords.into_iter().flatten() {
        let kw_line = kw.location.start_line.saturating_sub(1);
        if kw_line > section_start && kw_line < end {
            end = kw_line.saturating_sub(1);
        }
    }

    end
}

fn make_comment_fold(start: u32, end: u32) -> Option<FoldingRange> {
    if end <= start {
        return None;
    }
    Some(FoldingRange {
        start_line: start,
        start_character: None,
        end_line: end,
        end_character: None,
        kind: Some(FoldingRangeKind::Comment),
        collapsed_text: None,
    })
}

fn collect_comment_folds(source: &str, ranges: &mut Vec<FoldingRange>) {
    let lines: Vec<&str> = source.lines().collect();

    // Block comments: /* ... */
    let mut block_start: Option<u32> = None;
    for (i, line) in lines.iter().enumerate() {
        let line_num = i as u32;
        if block_start.is_none() && line.contains("/*") {
            block_start = Some(line_num);
        }
        if let Some(start) = block_start.filter(|_| line.contains("*/")) {
            ranges.extend(make_comment_fold(start, line_num));
            block_start = None;
        }
    }

    // Consecutive single-line comments
    let mut comment_start: Option<u32> = None;
    let mut comment_end: u32 = 0;

    for (i, line) in lines.iter().enumerate() {
        let line_num = i as u32;
        if line.trim().starts_with("//") {
            if comment_start.is_none() {
                comment_start = Some(line_num);
            }
            comment_end = line_num;
        } else if let Some(start) = comment_start.take() {
            ranges.extend(make_comment_fold(start, comment_end));
        }
    }

    // Handle trailing comments at end of file
    if let Some(start) = comment_start {
        ranges.extend(make_comment_fold(start, comment_end));
    }
}
