//! AST trivia helpers for the Modelica formatter.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct TextReplacement {
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) text: &'static str,
}

pub(super) fn apply_ast_trivia_rules(
    source: &str,
    options: &FormatOptions,
    ast: &ast::StoredDefinition,
) -> String {
    let component_layout = ComponentDeclarationLayout::collect(ast);
    let mut collector = AstTriviaReplacementCollector::new(source, options, &component_layout);
    let _ = ast::Visitor::visit_stored_definition(&mut collector, ast);
    let mut replacements = collector.into_replacements();
    if replacements.is_empty() {
        return source.to_string();
    }

    replacements.sort_by_key(|replacement| replacement.start);
    replacements.dedup();

    let mut formatted = source.to_string();
    let mut previous_start = source.len() + 1;
    for replacement in replacements.into_iter().rev() {
        if replacement.end > previous_start {
            continue;
        }
        formatted.replace_range(replacement.start..replacement.end, replacement.text);
        previous_start = replacement.start;
    }
    formatted
}

pub(super) struct SourceTrivia<'source> {
    pub(super) source: &'source str,
}

impl<'source> SourceTrivia<'source> {
    pub(super) fn new(source: &'source str) -> Self {
        Self { source }
    }

    pub(super) fn gap_between_spans(&self, left: Span, right: Span) -> Option<TriviaGap<'source>> {
        if left.is_dummy() || right.is_dummy() || left.source != right.source {
            return None;
        }
        let start = left.end.0;
        let end = right.start.0;
        if start > end {
            return None;
        }
        self.source
            .get(start..end)
            .map(|text| TriviaGap { start, text })
    }

    pub(super) fn gap_between_expressions(
        &self,
        left: &ast::Expression,
        right: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(left.span(), right.span())
    }

    pub(super) fn gap_between_expression_list_items(
        &self,
        left: &ast::Expression,
        right: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        let gap = self.gap_between_spans(
            expression_list_item_span(left),
            expression_list_item_span(right),
        )?;
        Some(adjust_expression_list_separator_gap(gap))
    }

    pub(super) fn gap_after_call_open_paren(
        &self,
        target: &ast::ComponentReference,
        first_arg: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(target.span, first_arg.span())
            .and_then(opening_parenthesis_padding_gap)
    }

    pub(super) fn gap_after_token_open_paren(
        &self,
        target: &Token,
        first_arg: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(token_span(target), first_arg.span())
            .and_then(opening_parenthesis_padding_gap)
    }

    pub(super) fn gap_after_name_open_paren(
        &self,
        target: &ast::Name,
        first_arg: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(token_span(target.name.last()?), first_arg.span())
            .and_then(opening_parenthesis_padding_gap)
    }

    pub(super) fn gap_after_keyword_open_paren_before_expression(
        &self,
        keyword: &str,
        expr: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        let span = expr.span();
        if span.is_dummy() {
            return None;
        }
        let expr_start = span.start.0;
        let line_start = self
            .source
            .get(..expr_start)?
            .rfind('\n')
            .map_or(0, |index| index + '\n'.len_utf8());
        let before_expr = self.source.get(line_start..expr_start)?;
        let keyword_start = before_expr.find(keyword)?;
        let keyword_end = keyword_start + keyword.len();
        if keyword_start > 0
            && before_expr[..keyword_start]
                .chars()
                .next_back()
                .is_some_and(is_identifier_char)
        {
            return None;
        }
        if before_expr[keyword_end..]
            .chars()
            .next()
            .is_some_and(is_identifier_char)
        {
            return None;
        }
        opening_parenthesis_padding_gap(TriviaGap {
            start: line_start + keyword_end,
            text: before_expr.get(keyword_end..)?,
        })
    }

    pub(super) fn gap_between_tokens(
        &self,
        left: &Token,
        right: &Token,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(token_span(left), token_span(right))
    }

    pub(super) fn gap_between_name_and_token(
        &self,
        left: &ast::Name,
        right: &Token,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_tokens(left.name.last()?, right)
    }

    pub(super) fn gap_between_token_and_expression(
        &self,
        left: &Token,
        right: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(token_span(left), right.span())
    }

    pub(super) fn gap_between_expression_list_item_and_expression(
        &self,
        left: &ast::Expression,
        right: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(expression_list_item_span(left), right.span())
    }

    pub(super) fn gap_between_component_reference_and_expression(
        &self,
        left: &ast::ComponentReference,
        right: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(left.span, right.span())
    }

    pub(super) fn gap_between_component_references(
        &self,
        left: &ast::ComponentReference,
        right: &ast::ComponentReference,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(left.span, right.span)
    }

    pub(super) fn gap_between_expression_and_component_reference(
        &self,
        left: &ast::Expression,
        right: &ast::ComponentReference,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(left.span(), right.span)
    }

    pub(super) fn gap_between_subscripts(
        &self,
        left: &ast::Subscript,
        right: &ast::Subscript,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_spans(subscript_span(left)?, subscript_span(right)?)
    }

    pub(super) fn gap_between_extend_modifications(
        &self,
        left: &ast::ExtendModification,
        right: &ast::ExtendModification,
    ) -> Option<TriviaGap<'source>> {
        let gap = self.gap_between_spans(
            expression_list_item_span(&left.expr),
            expression_list_item_span(&right.expr),
        )?;
        Some(adjust_expression_list_separator_gap(gap))
    }

    pub(super) fn gap_between_token_and_name(
        &self,
        left: &Token,
        right: &ast::Name,
    ) -> Option<TriviaGap<'source>> {
        self.gap_between_tokens(left, right.name.first()?)
    }

    pub(super) fn gap_after_keyword_before_name(
        &self,
        keyword: &str,
        name: &ast::Name,
    ) -> Option<TriviaGap<'source>> {
        self.gap_after_keyword_before_token(keyword, name.name.first()?)
    }

    pub(super) fn gap_after_keyword_before_token(
        &self,
        keyword: &str,
        token: &Token,
    ) -> Option<TriviaGap<'source>> {
        let span = token_span(token);
        if span.is_dummy() {
            return None;
        }
        let token_start = span.start.0;
        let line_start = self
            .source
            .get(..token_start)?
            .rfind('\n')
            .map_or(0, |index| index + '\n'.len_utf8());
        let before_token = self.source.get(line_start..token_start)?;
        let keyword_end = before_token.trim_end_matches([' ', '\t']).len();
        let keyword_start = keyword_end.checked_sub(keyword.len())?;
        if before_token.get(keyword_start..keyword_end)? != keyword {
            return None;
        }
        if keyword_start > 0
            && before_token[..keyword_start]
                .chars()
                .next_back()
                .is_some_and(is_identifier_char)
        {
            return None;
        }
        Some(TriviaGap {
            start: line_start + keyword_end,
            text: before_token.get(keyword_end..)?,
        })
    }

    pub(super) fn line_before_token(&self, token: &Token) -> Option<LineBeforeToken<'source>> {
        let span = token_span(token);
        if span.is_dummy() {
            return None;
        }
        let token_start = span.start.0;
        let line_start = self
            .source
            .get(..token_start)?
            .rfind('\n')
            .map_or(0, |index| index + '\n'.len_utf8());
        let text = self.source.get(line_start..token_start)?;
        Some(LineBeforeToken {
            start: line_start,
            text,
        })
    }
}

pub(super) struct LineBeforeToken<'source> {
    pub(super) start: usize,
    text: &'source str,
}

pub(super) struct TriviaGap<'source> {
    pub(super) start: usize,
    pub(super) text: &'source str,
}

impl TriviaGap<'_> {
    pub(super) fn single_horizontal_space_replacement(&self) -> Option<TextReplacement> {
        if self.text == " " || self.text.contains(['\n', '\r']) {
            return None;
        }
        self.text
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
            .then_some(TextReplacement {
                start: self.start,
                end: self.start + self.text.len(),
                text: " ",
            })
    }

    pub(super) fn is_multiple_spaces(&self) -> bool {
        self.text.len() > 1 && self.text.chars().all(|c| c == ' ')
    }

    pub(super) fn dot_separator_replacement(&self) -> Option<TextReplacement> {
        if self.text == "." || self.text.contains(['\n', '\r']) {
            return None;
        }
        let dot_count = self.text.chars().filter(|&c| c == '.').count();
        (dot_count == 1 && self.text.chars().all(|c| matches!(c, '.' | ' ' | '\t'))).then_some(
            TextReplacement {
                start: self.start,
                end: self.start + self.text.len(),
                text: ".",
            },
        )
    }

    pub(super) fn assignment_operator_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let assignment_offset = self.text.rfind('=')?;
        let start = self.start + trim_horizontal_whitespace_before(self.text, assignment_offset);
        let end = self.start
            + trim_horizontal_whitespace_after(self.text, assignment_offset + '='.len_utf8());
        Some(TextReplacement {
            start,
            end,
            text: " = ",
        })
    }

    pub(super) fn compact_assignment_operator_replacement(&self) -> Option<TextReplacement> {
        if self.text == "=" || self.text.contains(['\n', '\r']) {
            return None;
        }
        let assignment_offset = self.text.rfind('=')?;
        let start = self.start + trim_horizontal_whitespace_before(self.text, assignment_offset);
        let end = self.start
            + trim_horizontal_whitespace_after(self.text, assignment_offset + '='.len_utf8());
        Some(TextReplacement {
            start,
            end,
            text: "=",
        })
    }

    pub(super) fn statement_assignment_operator_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let assignment_offset = self.text.rfind(":=")?;
        let start = self.start + trim_horizontal_whitespace_before(self.text, assignment_offset);
        let end = self.start
            + trim_horizontal_whitespace_after(self.text, assignment_offset + ":=".len());
        Some(TextReplacement {
            start,
            end,
            text: " := ",
        })
    }

    pub(super) fn keyword_separator_replacement(
        &self,
        keyword: &'static str,
    ) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        (self.text.trim_matches([' ', '\t']) == keyword).then_some(TextReplacement {
            start: self.start,
            end: self.start + self.text.len(),
            text: match keyword {
                "in" => " in ",
                _ => return None,
            },
        })
    }

    pub(super) fn comma_separator_replacement(&self) -> Option<TextReplacement> {
        if self.text == ", " || self.text.contains(['\n', '\r']) {
            return None;
        }
        let comma_count = self.text.chars().filter(|&c| c == ',').count();
        (comma_count == 1 && self.text.chars().all(|c| matches!(c, ',' | ' ' | '\t'))).then_some(
            TextReplacement {
                start: self.start,
                end: self.start + self.text.len(),
                text: ", ",
            },
        )
    }

    pub(super) fn opening_parenthesis_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let paren_offset = self.text.find('(')?;
        if !self.text[..paren_offset]
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
        {
            return None;
        }
        let after_paren = paren_offset + '('.len_utf8();
        let padding_len = self.text[after_paren..]
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .map(char::len_utf8)
            .sum::<usize>();
        if paren_offset == 0 && padding_len == 0 {
            return None;
        }
        Some(TextReplacement {
            start: self.start,
            end: self.start + after_paren + padding_len,
            text: "(",
        })
    }

    pub(super) fn opening_delimiter_padding_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let delimiter_len = self
            .text
            .chars()
            .take_while(|c| matches!(c, '(' | '[' | '{'))
            .map(char::len_utf8)
            .sum::<usize>();
        if delimiter_len == 0 {
            return None;
        }
        let padding = self.text.get(delimiter_len..)?;
        if padding.is_empty() || !padding.chars().all(|c| matches!(c, ' ' | '\t')) {
            return None;
        }
        Some(TextReplacement {
            start: self.start + delimiter_len,
            end: self.start + self.text.len(),
            text: "",
        })
    }

    pub(super) fn closing_expression_separator_replacement(&self) -> Option<TextReplacement> {
        if self.text == ", " || self.text.contains(['\n', '\r']) {
            return None;
        }
        let comma_offset = self.text.find(',')?;
        if !self.text[..comma_offset]
            .chars()
            .all(|c| matches!(c, ')' | '}' | ']' | ' ' | '\t'))
            || !self.text[comma_offset + ','.len_utf8()..]
                .chars()
                .all(|c| matches!(c, ' ' | '\t'))
        {
            return None;
        }
        Some(TextReplacement {
            start: self.start + comma_offset,
            end: self.start + self.text.len(),
            text: ", ",
        })
    }

    pub(super) fn binary_operator_replacement(&self, op: &OpBinary) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let operator = binary_operator_text(op)?;
        (self.text.trim_matches([' ', '\t']) == operator).then_some(TextReplacement {
            start: self.start,
            end: self.start + self.text.len(),
            text: spaced_binary_operator_text(op)?,
        })
    }

    pub(super) fn closing_delimiter_name_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let delimiter_len = self
            .text
            .chars()
            .take_while(|c| matches!(c, ')' | ']' | '}'))
            .map(char::len_utf8)
            .sum::<usize>();
        if delimiter_len == 0 {
            return None;
        }
        let padding = self.text.get(delimiter_len..)?;
        if padding == " " || !padding.chars().all(|c| matches!(c, ' ' | '\t')) {
            return None;
        }
        Some(TextReplacement {
            start: self.start + delimiter_len,
            end: self.start + self.text.len(),
            text: " ",
        })
    }

    pub(super) fn closing_delimiter_binary_operator_replacement(&self) -> Option<TextReplacement> {
        let operator = closing_delimiter_binary_operator(self.text)?;
        let delimiter_len = closing_delimiter_prefix_len(self.text);
        let replacement_text = spaced_token_operator_text(operator);
        if self.text.get(delimiter_len..) == Some(replacement_text) {
            return None;
        }
        Some(TextReplacement {
            start: self.start + delimiter_len,
            end: self.start + self.text.len(),
            text: replacement_text,
        })
    }

    pub(super) fn closing_delimiter_opening_parenthesis_replacement(
        &self,
    ) -> Option<TextReplacement> {
        let delimiter_len = closing_delimiter_prefix_len(self.text);
        if delimiter_len == 0 || self.text.contains(['\n', '\r']) {
            return None;
        }
        let after_delimiters = self.text.get(delimiter_len..)?;
        let paren_offset = after_delimiters.find('(')?;
        if !after_delimiters[..paren_offset]
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
        {
            return None;
        }
        let after_paren = paren_offset + '('.len_utf8();
        let padding_len = after_delimiters[after_paren..]
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .map(char::len_utf8)
            .sum::<usize>();
        if paren_offset == 0 && padding_len == 0 {
            return None;
        }
        Some(TextReplacement {
            start: self.start + delimiter_len,
            end: self.start + delimiter_len + after_paren + padding_len,
            text: "(",
        })
    }

    pub(super) fn statement_separator_replacement(&self) -> Option<TextReplacement> {
        if self.text.contains(['\n', '\r']) {
            return None;
        }
        let semicolon_offset = self.text.find(';')?;
        if !self.text[..semicolon_offset]
            .chars()
            .all(|c| matches!(c, ')' | ']' | '}' | ' ' | '\t'))
            || !self.text[semicolon_offset + ';'.len_utf8()..]
                .chars()
                .all(|c| matches!(c, ' ' | '\t'))
            || self.text.matches(';').count() != 1
        {
            return None;
        }
        (self.text.get(semicolon_offset..) != Some("; ")).then_some(TextReplacement {
            start: self.start + semicolon_offset,
            end: self.start + self.text.len(),
            text: "; ",
        })
    }
}

pub(super) fn adjust_expression_list_separator_gap(gap: TriviaGap<'_>) -> TriviaGap<'_> {
    let Some(comma_offset) = gap.text.find(',') else {
        return gap;
    };
    if !gap.text[..comma_offset]
        .chars()
        .all(|c| matches!(c, ')' | '}' | ']' | ' ' | '\t'))
    {
        return gap;
    }
    TriviaGap {
        start: gap.start + comma_offset,
        text: &gap.text[comma_offset..],
    }
}

pub(super) fn opening_parenthesis_padding_gap(gap: TriviaGap<'_>) -> Option<TriviaGap<'_>> {
    let paren_offset = gap.text.find('(')?;
    if !gap.text[..paren_offset]
        .chars()
        .all(|c| matches!(c, ' ' | '\t'))
    {
        return None;
    }
    let padding_start = paren_offset + '('.len_utf8();
    let padding_len = gap
        .text
        .get(padding_start..)?
        .chars()
        .take_while(|c| matches!(c, ' ' | '\t'))
        .map(char::len_utf8)
        .sum::<usize>();
    let text = gap.text.get(..padding_start + padding_len)?;
    (paren_offset > 0 || padding_len > 0).then_some(TriviaGap {
        start: gap.start,
        text,
    })
}

pub(super) fn opening_delimiter_padding_gap(gap: &TriviaGap<'_>) -> bool {
    let delimiter_len = gap
        .text
        .chars()
        .take_while(|c| matches!(c, '(' | '[' | '{'))
        .map(char::len_utf8)
        .sum::<usize>();
    delimiter_len > 0
        && gap.text.get(delimiter_len..).is_some_and(|padding| {
            !padding.is_empty() && padding.chars().all(|c| matches!(c, ' ' | '\t'))
        })
}

pub(super) fn closing_delimiter_prefix_len(text: &str) -> usize {
    text.chars()
        .take_while(|c| matches!(c, ')' | ']' | '}'))
        .map(char::len_utf8)
        .sum()
}

pub(super) fn closing_delimiter_binary_operator(text: &str) -> Option<&'static str> {
    if text.contains(['\n', '\r']) {
        return None;
    }
    let delimiter_len = closing_delimiter_prefix_len(text);
    if delimiter_len == 0 {
        return None;
    }
    let after_delimiters = text.get(delimiter_len..)?;
    let trimmed = after_delimiters.trim_matches([' ', '\t']);
    let operator = match trimmed {
        "+" => "+",
        "-" => "-",
        "*" => "*",
        "/" => "/",
        "<" => "<",
        "<=" => "<=",
        ">" => ">",
        ">=" => ">=",
        "==" => "==",
        "<>" => "<>",
        _ => return None,
    };
    if !after_delimiters
        .chars()
        .all(|c| matches!(c, ' ' | '\t' | '+' | '-' | '*' | '/' | '<' | '>' | '='))
    {
        return None;
    }
    Some(operator)
}

pub(super) fn is_closing_delimiter_opening_parenthesis_gap(text: &str) -> bool {
    let delimiter_len = closing_delimiter_prefix_len(text);
    if delimiter_len == 0 || text.contains(['\n', '\r']) {
        return false;
    }
    let Some(after_delimiters) = text.get(delimiter_len..) else {
        return false;
    };
    let Some(paren_offset) = after_delimiters.find('(') else {
        return false;
    };
    after_delimiters[..paren_offset]
        .chars()
        .all(|c| matches!(c, ' ' | '\t'))
        && after_delimiters[paren_offset + '('.len_utf8()..]
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
}

pub(super) fn is_statement_separator_gap(text: &str) -> bool {
    let Some(semicolon_offset) = text.find(';') else {
        return false;
    };
    !text.contains(['\n', '\r'])
        && text.matches(';').count() == 1
        && text[..semicolon_offset]
            .chars()
            .all(|c| matches!(c, ')' | ']' | '}' | ' ' | '\t'))
        && text[semicolon_offset + ';'.len_utf8()..]
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
}

pub(super) fn spaced_token_operator_text(operator: &str) -> &'static str {
    match operator {
        "+" => " + ",
        "-" => " - ",
        "*" => " * ",
        "/" => " / ",
        "<" => " < ",
        "<=" => " <= ",
        ">" => " > ",
        ">=" => " >= ",
        "==" => " == ",
        "<>" => " <> ",
        _ => unreachable!("operator filtered by closing_delimiter_binary_operator"),
    }
}

pub(super) fn token_span(token: &Token) -> Span {
    Span::from_offsets(
        ir_core::SourceId::from_source_name(&token.location.file_name),
        token.location.start as usize,
        token.location.end as usize,
    )
}

pub(super) fn expression_list_item_span(expr: &ast::Expression) -> Span {
    match expr {
        ast::Expression::NamedArgument { name, value, span } => {
            span_from_start_to_end(token_span(name), value.span()).unwrap_or(*span)
        }
        ast::Expression::Modification {
            target,
            value,
            span,
        } => span_from_start_to_end(target.span, value.span()).unwrap_or(*span),
        _ => expr.span(),
    }
}

pub(super) fn subscript_span(subscript: &ast::Subscript) -> Option<Span> {
    match subscript {
        ast::Subscript::Expression(expr) => Some(expr.span()),
        ast::Subscript::Range { token } => Some(token_span(token)),
        ast::Subscript::Empty => None,
    }
}

pub(super) fn span_from_start_to_end(start: Span, end: Span) -> Option<Span> {
    if start.is_dummy() || end.is_dummy() || start.source != end.source {
        return None;
    }
    Some(Span::from_offsets(start.source, start.start.0, end.end.0))
}

#[derive(Default)]
pub(super) struct ComponentDeclarationLayout {
    declaration_lines: Vec<u32>,
}

impl ComponentDeclarationLayout {
    pub(super) fn collect(ast: &ast::StoredDefinition) -> Self {
        let mut collector = ComponentDeclarationLayoutCollector::default();
        let _ = ast::Visitor::visit_stored_definition(&mut collector, ast);
        collector.into_layout()
    }

    pub(super) fn has_nearby_declaration_line(&self, line: u32) -> bool {
        self.declaration_lines
            .iter()
            .copied()
            .any(|other| other != line && other.abs_diff(line) <= 2)
    }
}

#[derive(Default)]
pub(super) struct ComponentDeclarationLayoutCollector {
    declaration_lines: Vec<u32>,
}

impl ComponentDeclarationLayoutCollector {
    pub(super) fn into_layout(mut self) -> ComponentDeclarationLayout {
        self.declaration_lines.sort_unstable();
        self.declaration_lines.dedup();
        ComponentDeclarationLayout {
            declaration_lines: self.declaration_lines,
        }
    }
}

impl ast::Visitor for ComponentDeclarationLayoutCollector {
    fn enter_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        self.declaration_lines
            .push(component.name_token.location.start_line);
        Continue(())
    }
}

pub(super) fn is_builtin_scalar_type_name(name: &ast::Name) -> bool {
    let [token] = name.name.as_slice() else {
        return false;
    };
    matches!(
        token.text.as_ref(),
        "Real" | "Integer" | "Boolean" | "String"
    )
}

pub(super) fn type_alias_base_name(class: &ast::ClassDef) -> Option<&ast::Name> {
    if class.extends.len() != 1 || class.end_name_token.is_some() {
        return None;
    }
    Some(&class.extends[0].base_name)
}

pub(super) fn redeclare_modification_class_target(
    expr: &ast::Expression,
) -> Option<(&ast::ComponentReference, &[ast::Expression])> {
    let ast::Expression::Modification { value, .. } = expr else {
        return None;
    };
    let ast::Expression::ClassModification {
        target,
        modifications,
        ..
    } = value.as_ref()
    else {
        return None;
    };
    Some((target, modifications))
}

pub(super) fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

pub(super) fn component_prefix_keywords(component: &ast::Component) -> Vec<&'static str> {
    let mut keywords = Vec::new();
    if component.is_redeclare {
        keywords.push("redeclare");
    }
    if component.is_final {
        keywords.push("final");
    }
    if component.inner {
        keywords.push("inner");
    }
    if component.outer {
        keywords.push("outer");
    }
    if component.is_replaceable {
        keywords.push("replaceable");
    }
    if let Some(keyword) = component_connection_keyword(component) {
        keywords.push(keyword);
    }
    if let Some(keyword) = component_variability_keyword(component) {
        keywords.push(keyword);
    }
    if let Some(keyword) = component_causality_keyword(component) {
        keywords.push(keyword);
    }
    keywords
}

pub(super) fn component_connection_keyword(component: &ast::Component) -> Option<&'static str> {
    match &component.connection {
        ast::Connection::Flow(_) => Some("flow"),
        ast::Connection::Stream(_) => Some("stream"),
        ast::Connection::Empty => None,
    }
}

pub(super) fn component_variability_keyword(component: &ast::Component) -> Option<&'static str> {
    match &component.variability {
        Variability::Constant(_) => Some("constant"),
        Variability::Parameter(_) => Some("parameter"),
        Variability::Discrete(_) => Some("discrete"),
        Variability::Continuous(_) => Some("continuous"),
        Variability::Empty => None,
    }
}

pub(super) fn component_causality_keyword(component: &ast::Component) -> Option<&'static str> {
    match &component.causality {
        Causality::Input(_) => Some("input"),
        Causality::Output(_) => Some("output"),
        Causality::Empty => None,
    }
}

pub(super) fn class_prefix_keywords(class: &ast::ClassDef) -> Vec<&'static str> {
    let mut keywords = Vec::new();
    if class.is_redeclare {
        keywords.push("redeclare");
    }
    if class.is_final {
        keywords.push("final");
    }
    if class.is_inner {
        keywords.push("inner");
    }
    if class.is_outer {
        keywords.push("outer");
    }
    if class.is_replaceable {
        keywords.push("replaceable");
    }
    if class.encapsulated {
        keywords.push("encapsulated");
    }
    if class.partial {
        keywords.push("partial");
    }
    if class.expandable {
        keywords.push("expandable");
    }
    if class.operator_record {
        keywords.push("operator");
    }
    if is_function_class_type_token(&class.class_type_token) {
        if class.pure {
            keywords.push("pure");
        } else {
            keywords.push("impure");
        }
    }
    keywords
}

pub(super) fn declaration_prefix_gaps_before_token<'source>(
    line: LineBeforeToken<'source>,
    allowed_keywords: &[&str],
) -> Vec<TriviaGap<'source>> {
    let words = declaration_prefix_words_before_token(line.text, allowed_keywords);
    words
        .windows(2)
        .filter_map(|adjacent| {
            let left = &adjacent[0];
            let right = &adjacent[1];
            let text = line.text.get(left.end..right.start)?;
            Some(TriviaGap {
                start: line.start + left.end,
                text,
            })
        })
        .collect()
}

fn declaration_prefix_words_before_token<'source>(
    text_before_declaration_token: &'source str,
    allowed_keywords: &[&str],
) -> Vec<WordSpan<'source>> {
    let mut words = leading_words(text_before_declaration_token);
    let mut selected = vec![WordSpan {
        text: "",
        start: text_before_declaration_token.len(),
        end: text_before_declaration_token.len(),
    }];
    while let Some(word) = words.pop() {
        if !allowed_keywords.contains(&word.text) {
            break;
        }
        selected.push(word);
    }
    selected.reverse();
    selected
}

#[derive(Debug)]
struct WordSpan<'source> {
    text: &'source str,
    start: usize,
    end: usize,
}

fn leading_words(text: &str) -> Vec<WordSpan<'_>> {
    let mut words = Vec::new();
    let mut cursor = 0usize;
    while let Some((start, end)) = next_word_span(text, cursor) {
        words.push(WordSpan {
            text: &text[start..end],
            start,
            end,
        });
        cursor = end;
    }
    words
}

pub(super) fn next_word_span(text: &str, cursor: usize) -> Option<(usize, usize)> {
    let start = text
        .get(cursor..)?
        .char_indices()
        .find_map(|(offset, c)| is_identifier_char(c).then_some(cursor + offset))?;
    let end = text
        .get(start..)?
        .char_indices()
        .find_map(|(offset, c)| (!is_identifier_char(c)).then_some(start + offset))
        .unwrap_or(text.len());
    Some((start, end))
}

pub(super) fn is_function_class_type_token(token: &Token) -> bool {
    token.text.as_ref() == "function"
}

pub(super) fn binary_operator_text(op: &OpBinary) -> Option<&'static str> {
    match op {
        OpBinary::Add => Some("+"),
        OpBinary::Sub => Some("-"),
        OpBinary::Mul => Some("*"),
        OpBinary::Div => Some("/"),
        OpBinary::Eq => Some("=="),
        OpBinary::Neq => Some("<>"),
        OpBinary::Lt => Some("<"),
        OpBinary::Le => Some("<="),
        OpBinary::Gt => Some(">"),
        OpBinary::Ge => Some(">="),
        OpBinary::And => Some("and"),
        OpBinary::Or => Some("or"),
        OpBinary::Exp => Some("^"),
        OpBinary::ExpElem => Some(".^"),
        OpBinary::AddElem => Some(".+"),
        OpBinary::SubElem => Some(".-"),
        OpBinary::MulElem => Some(".*"),
        OpBinary::DivElem => Some("./"),
        OpBinary::Assign | OpBinary::Empty => None,
    }
}

pub(super) fn spaced_binary_operator_text(op: &OpBinary) -> Option<&'static str> {
    match op {
        OpBinary::Add => Some(" + "),
        OpBinary::Sub => Some(" - "),
        OpBinary::Mul => Some(" * "),
        OpBinary::Div => Some(" / "),
        OpBinary::Eq => Some(" == "),
        OpBinary::Neq => Some(" <> "),
        OpBinary::Lt => Some(" < "),
        OpBinary::Le => Some(" <= "),
        OpBinary::Gt => Some(" > "),
        OpBinary::Ge => Some(" >= "),
        OpBinary::And => Some(" and "),
        OpBinary::Or => Some(" or "),
        OpBinary::Exp => Some(" ^ "),
        OpBinary::ExpElem => Some(" .^ "),
        OpBinary::AddElem => Some(" .+ "),
        OpBinary::SubElem => Some(" .- "),
        OpBinary::MulElem => Some(" .* "),
        OpBinary::DivElem => Some(" ./ "),
        OpBinary::Assign | OpBinary::Empty => None,
    }
}

pub(super) fn escape_example_gap(text: &str) -> String {
    text.replace('\t', "\\t").replace(' ', "·")
}

pub(super) fn truncate_example(text: &str) -> String {
    const MAX_CHARS: usize = 32;
    let mut chars = text.chars();
    let truncated: String = chars.by_ref().take(MAX_CHARS).collect();
    if chars.next().is_some() {
        format!("{truncated}...")
    } else {
        truncated
    }
}

pub(super) fn is_unclassified_gap_candidate(text: &str) -> bool {
    text.contains([' ', '\t'])
        && !text.contains(['\n', '\r'])
        && text.chars().all(|c| {
            matches!(
                c,
                ' ' | '\t'
                    | '('
                    | ')'
                    | '['
                    | ']'
                    | '{'
                    | '}'
                    | ','
                    | ';'
                    | ':'
                    | '='
                    | '+'
                    | '-'
                    | '*'
                    | '/'
                    | '^'
                    | '<'
                    | '>'
                    | '.'
            )
        })
}

pub(super) fn is_closing_expression_separator_gap(text: &str) -> bool {
    let Some(comma_offset) = text.find(',') else {
        return false;
    };
    text[..comma_offset]
        .chars()
        .all(|c| matches!(c, ')' | '}' | ']' | ' ' | '\t'))
        && text[comma_offset + ','.len_utf8()..]
            .chars()
            .all(|c| matches!(c, ' ' | '\t'))
}

pub(super) fn trim_horizontal_whitespace_before(text: &str, offset: usize) -> usize {
    let mut start = offset;
    while start > 0 {
        let Some((previous_offset, previous_char)) = text[..start].char_indices().next_back()
        else {
            break;
        };
        if !matches!(previous_char, ' ' | '\t') {
            break;
        }
        start = previous_offset;
    }
    start
}

pub(super) fn trim_horizontal_whitespace_after(text: &str, offset: usize) -> usize {
    let mut end = offset;
    while end < text.len() {
        let Some(next_char) = text[end..].chars().next() else {
            break;
        };
        if !matches!(next_char, ' ' | '\t') {
            break;
        }
        end += next_char.len_utf8();
    }
    end
}
