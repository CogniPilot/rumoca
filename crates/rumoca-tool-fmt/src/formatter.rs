//! Modelica formatter implementation.

use crate::format_errors::FormatError;
use crate::format_options::{FormatOptions, FormatProfile};

use rumoca_compile::parsing::{
    Causality, OpBinary, Span, Token, Variability, ast, ir_core, parse_source_to_ast,
};
use std::ops::ControlFlow::{self, Continue};

mod dymola;

mod coverage;
mod trivia;

pub use coverage::{FormatCoverageCategory, FormatCoverageCategoryReport, FormatCoverageReport};

use trivia::*;

use dymola::{
    DymolaFormatState, can_trim_trailing_whitespace, format_dymola_line, target_line_ending,
};

/// Format Modelica source code.
pub fn format(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    format_with_source_name(source, options, "<format>")
}

/// Format Modelica source code with explicit source name.
pub fn format_with_source_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> Result<String, FormatError> {
    let ast = parse_source_to_ast(source, source_name)
        .map_err(|e| FormatError::SyntaxError(e.to_string()))?;
    Ok(format_source(source, options, &ast))
}

/// Format source, returning original on syntax error.
pub fn format_or_original(source: &str, options: &FormatOptions) -> String {
    format(source, options).unwrap_or_else(|_| source.to_string())
}

/// Format source with explicit source name, returning original on syntax error.
pub fn format_or_original_with_source_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> String {
    format_with_source_name(source, options, source_name).unwrap_or_else(|_| source.to_string())
}

/// Report formatter rule coverage over eligible AST trivia gaps.
pub fn format_coverage_report(
    source: &str,
    options: &FormatOptions,
) -> Result<FormatCoverageReport, FormatError> {
    format_coverage_report_with_source_name(source, options, "<coverage>")
}

/// Report formatter rule coverage with an explicit source name.
pub fn format_coverage_report_with_source_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> Result<FormatCoverageReport, FormatError> {
    let ast = parse_source_to_ast(source, source_name)
        .map_err(|e| FormatError::SyntaxError(e.to_string()))?;
    let component_layout = ComponentDeclarationLayout::collect(&ast);
    let mut collector = AstTriviaCoverageCollector::new(source, options, &component_layout);
    let _ = ast::Visitor::visit_stored_definition(&mut collector, &ast);
    Ok(collector.into_report())
}

fn format_source(source: &str, options: &FormatOptions, ast: &ast::StoredDefinition) -> String {
    match options.profile {
        FormatProfile::Dymola | FormatProfile::Canonical => {
            format_dymola_source(source, options, ast)
        }
    }
}

fn format_dymola_source(
    source: &str,
    options: &FormatOptions,
    ast: &ast::StoredDefinition,
) -> String {
    let source = apply_ast_trivia_rules(source, options, ast);
    let mut line_options = options.clone();
    line_options.normalize_equation_spacing = false;

    let line_ending = target_line_ending(&source, options.line_ending);
    let mut result = String::with_capacity(source.len());
    let mut indent_level = 0usize;
    let mut state = DymolaFormatState::default();
    let mut wrote_line = false;

    for raw_line in source.lines() {
        let line = raw_line.strip_suffix('\r').unwrap_or(raw_line);
        let line = if line_options.trim_trailing_whitespace
            && can_trim_trailing_whitespace(line, &state)
        {
            line.trim_end_matches([' ', '\t'])
        } else {
            line
        };
        let formatted_line = format_dymola_line(line, &line_options, &mut indent_level, &mut state);
        result.push_str(&formatted_line);
        result.push_str(line_ending);
        wrote_line = true;
    }

    if !line_options.insert_final_newline && wrote_line {
        for _ in 0..line_ending.len() {
            let _ = result.pop();
        }
    }

    result
}

struct AstTriviaReplacementCollector<'source, 'options> {
    trivia: SourceTrivia<'source>,
    options: &'options FormatOptions,
    component_layout: &'options ComponentDeclarationLayout,
    replacements: Vec<TextReplacement>,
    token_spans: Vec<Span>,
}

impl<'source, 'options> AstTriviaReplacementCollector<'source, 'options> {
    fn new(
        source: &'source str,
        options: &'options FormatOptions,
        component_layout: &'options ComponentDeclarationLayout,
    ) -> Self {
        Self {
            trivia: SourceTrivia::new(source),
            options,
            component_layout,
            replacements: Vec::new(),
            token_spans: Vec::new(),
        }
    }

    fn into_replacements(mut self) -> Vec<TextReplacement> {
        self.collect_token_gap_replacements();
        self.replacements
    }

    fn remember_token(&mut self, token: &Token) {
        let span = token_span(token);
        if !span.is_dummy() {
            self.token_spans.push(span);
        }
    }

    fn remember_component_reference_tokens(&mut self, reference: &ast::ComponentReference) {
        for part in &reference.parts {
            self.remember_token(&part.ident);
        }
    }

    fn collect_token_gap_replacements(&mut self) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        self.token_spans
            .sort_by_key(|span| (span.start.0, span.end.0));
        self.token_spans
            .dedup_by_key(|span| (span.start.0, span.end.0));
        let spans = self.token_spans.clone();
        for adjacent in spans.windows(2) {
            let Some(gap) = self.trivia.gap_between_spans(adjacent[0], adjacent[1]) else {
                continue;
            };
            if let Some(replacement) = gap.closing_expression_separator_replacement() {
                self.replacements.push(replacement);
            }
            if let Some(replacement) = gap.opening_parenthesis_replacement() {
                self.replacements.push(replacement);
            }
            if let Some(replacement) = gap.opening_delimiter_padding_replacement() {
                self.replacements.push(replacement);
            }
            if let Some(replacement) = gap.closing_delimiter_binary_operator_replacement() {
                self.replacements.push(replacement);
            }
            if let Some(replacement) = gap.closing_delimiter_opening_parenthesis_replacement() {
                self.replacements.push(replacement);
            }
            if let Some(replacement) = gap.statement_separator_replacement() {
                self.replacements.push(replacement);
            }
        }
    }

    fn collect_class_keyword_spacing(&mut self, class: &ast::ClassDef) {
        let Some(replacement) = self
            .trivia
            .gap_between_tokens(&class.class_type_token, &class.name)
            .and_then(|gap| gap.single_horizontal_space_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_class_prefix_spacing(&mut self, class: &ast::ClassDef) {
        let Some(line) = self.trivia.line_before_token(&class.class_type_token) else {
            return;
        };
        let allowed = class_prefix_keywords(class);
        for gap in declaration_prefix_gaps_before_token(line, &allowed) {
            let Some(replacement) = gap.single_horizontal_space_replacement() else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_class_end_name_spacing(&mut self, class: &ast::ClassDef) {
        let Some(replacement) = class
            .end_name_token
            .as_ref()
            .and_then(|end_name| self.trivia.gap_after_keyword_before_token("end", end_name))
            .and_then(|gap| gap.single_horizontal_space_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_type_alias_assignment_spacing(&mut self, class: &ast::ClassDef) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(base) = type_alias_base_name(class) else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_between_token_and_name(&class.name, base)
            .and_then(|gap| gap.assignment_operator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_keyword_name_spacing(&mut self, keyword: &str, name: &ast::Name) {
        let Some(replacement) = self
            .trivia
            .gap_after_keyword_before_name(keyword, name)
            .and_then(|gap| gap.single_horizontal_space_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_name_separators(&mut self, name: &ast::Name) {
        for tokens in name.name.windows(2) {
            self.collect_dot_separator_between_tokens(&tokens[0], &tokens[1]);
        }
    }

    fn collect_component_reference_separators(&mut self, reference: &ast::ComponentReference) {
        self.remember_component_reference_tokens(reference);
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        for parts in reference.parts.windows(2) {
            self.collect_dot_separator_between_tokens(&parts[0].ident, &parts[1].ident);
        }
    }

    fn collect_import_spacing(&mut self, import: &ast::Import) {
        match import {
            ast::Import::Renamed { alias, path, .. } => {
                self.collect_renamed_import_assignment_spacing(alias, path);
            }
            ast::Import::Selective { names, .. } => {
                self.collect_selective_import_name_separators(names);
            }
            ast::Import::Qualified { .. } | ast::Import::Unqualified { .. } => {}
        }
    }

    fn collect_renamed_import_assignment_spacing(&mut self, alias: &Token, path: &ast::Name) {
        let Some(replacement) = self
            .trivia
            .gap_between_token_and_name(alias, path)
            .and_then(|gap| gap.assignment_operator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_selective_import_name_separators(&mut self, names: &[Token]) {
        for adjacent in names.windows(2) {
            let Some(replacement) = self
                .trivia
                .gap_between_tokens(&adjacent[0], &adjacent[1])
                .and_then(|gap| gap.comma_separator_replacement())
            else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_expression_list_commas(&mut self, expressions: &[ast::Expression]) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        for adjacent in expressions.windows(2) {
            let Some(replacement) = self
                .trivia
                .gap_between_expression_list_items(&adjacent[0], &adjacent[1])
                .and_then(|gap| gap.comma_separator_replacement())
            else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_opening_parenthesis_padding(
        &mut self,
        target: &ast::ComponentReference,
        args: &[ast::Expression],
    ) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(first_arg) = args.first() else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_after_call_open_paren(target, first_arg)
            .and_then(|gap| gap.opening_parenthesis_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_assert_opening_parenthesis_padding(&mut self, condition: &ast::Expression) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(replacement) = self
            .trivia
            .gap_after_keyword_open_paren_before_expression("assert", condition)
            .and_then(|gap| gap.opening_parenthesis_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_component_modifier_opening_parenthesis_padding(
        &mut self,
        component: &ast::Component,
    ) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(first_modification) = component.source_modifications.first() else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_after_token_open_paren(&component.name_token, first_modification)
            .and_then(|gap| gap.opening_parenthesis_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_subscript_commas(&mut self, subscripts: &[ast::Subscript]) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        for adjacent in subscripts.windows(2) {
            let Some(replacement) = self
                .trivia
                .gap_between_subscripts(&adjacent[0], &adjacent[1])
                .and_then(|gap| gap.comma_separator_replacement())
            else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_extend_modification_commas(&mut self, modifications: &[ast::ExtendModification]) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        for adjacent in modifications.windows(2) {
            let Some(replacement) = self
                .trivia
                .gap_between_extend_modifications(&adjacent[0], &adjacent[1])
                .and_then(|gap| gap.comma_separator_replacement())
            else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_argument_assignment_spacing(&mut self, expr: &ast::Expression) {
        if !self.options.normalize_argument_assignment_spacing {
            return;
        }
        let replacement = match expr {
            ast::Expression::NamedArgument { name, value, .. } => self
                .trivia
                .gap_between_token_and_expression(name, value)
                .and_then(|gap| gap.compact_assignment_operator_replacement()),
            ast::Expression::Modification { target, value, .. } => self
                .trivia
                .gap_between_component_reference_and_expression(target, value)
                .and_then(|gap| gap.compact_assignment_operator_replacement()),
            ast::Expression::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } => self
                .trivia
                .gap_between_expressions(lhs, rhs)
                .and_then(|gap| gap.compact_assignment_operator_replacement()),
            _ => None,
        };
        if let Some(replacement) = replacement {
            self.replacements.push(replacement);
        }
    }

    fn collect_dot_separator_between_tokens(&mut self, left: &Token, right: &Token) {
        let Some(replacement) = self
            .trivia
            .gap_between_tokens(left, right)
            .and_then(|gap| gap.dot_separator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_component_prefix_spacing(&mut self, component: &ast::Component) {
        let Some(first_type_part) = component.type_name.name.first() else {
            return;
        };
        let Some(line) = self.trivia.line_before_token(first_type_part) else {
            return;
        };
        let allowed = component_prefix_keywords(component);
        for gap in declaration_prefix_gaps_before_token(line, &allowed) {
            let Some(replacement) = gap.single_horizontal_space_replacement() else {
                continue;
            };
            self.replacements.push(replacement);
        }
    }

    fn collect_component_type_name_spacing(&mut self, component: &ast::Component) {
        let Some(gap) = self.component_type_name_gap(component) else {
            return;
        };
        if self.should_preserve_component_alignment(component, &gap) {
            return;
        }
        let replacement = if component.shape_expr.is_empty() {
            gap.single_horizontal_space_replacement()
        } else {
            gap.closing_delimiter_name_replacement()
        };
        let Some(replacement) = replacement else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn component_type_name_gap(&self, component: &ast::Component) -> Option<TriviaGap<'source>> {
        if let Some(last_shape) = component.shape_expr.last()
            && let Some(shape_span) = subscript_span(last_shape)
            && shape_span.end.0 <= token_span(&component.name_token).start.0
        {
            return self
                .trivia
                .gap_between_spans(shape_span, token_span(&component.name_token));
        }
        self.trivia
            .gap_between_name_and_token(&component.type_name, &component.name_token)
    }

    fn collect_redeclare_modification_type_name_spacing(&mut self, expr: &ast::Expression) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(replacement) = self
            .redeclare_modification_type_name_gap(expr)
            .and_then(|gap| gap.single_horizontal_space_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_redeclare_modification_opening_parenthesis_padding(
        &mut self,
        expr: &ast::Expression,
    ) {
        let Some((target, modifications)) = redeclare_modification_class_target(expr) else {
            return;
        };
        self.collect_opening_parenthesis_padding(target, modifications);
    }

    fn collect_class_modification_redeclare_spacing(
        &mut self,
        modifications: &[ast::Expression],
        redeclare_flags: &[bool],
    ) {
        for (modification, redeclare) in modifications.iter().zip(redeclare_flags) {
            if !*redeclare {
                continue;
            }
            self.collect_redeclare_modification_type_name_spacing(modification);
            self.collect_redeclare_modification_opening_parenthesis_padding(modification);
        }
    }

    fn redeclare_modification_type_name_gap(
        &self,
        expr: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        let ast::Expression::Modification { target, value, .. } = expr else {
            return None;
        };
        let ast::Expression::ClassModification {
            target: replacement_type,
            ..
        } = value.as_ref()
        else {
            return None;
        };
        self.trivia.gap_between_tokens(
            &replacement_type.parts.last()?.ident,
            &target.parts.first()?.ident,
        )
    }

    fn collect_component_binding_assignment(&mut self, component: &ast::Component) {
        if !self.options.normalize_equation_spacing {
            return;
        }
        let Some(binding) = &component.binding else {
            return;
        };
        let gap = component
            .source_modifications
            .last()
            .and_then(|last| {
                self.trivia
                    .gap_between_expression_list_item_and_expression(last, binding)
            })
            .or_else(|| {
                self.trivia
                    .gap_between_token_and_expression(&component.name_token, binding)
            });
        let Some(replacement) = gap.and_then(|gap| gap.assignment_operator_replacement()) else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_external_function_spacing(&mut self, external: &ast::ExternalFunction) {
        if let Some(function_name) = &external.function_name {
            self.collect_external_opening_parenthesis_padding(function_name, &external.args);
        }
        self.collect_expression_list_commas(&external.args);
    }

    fn collect_external_opening_parenthesis_padding(
        &mut self,
        function_name: &Token,
        args: &[ast::Expression],
    ) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(first_arg) = args.first() else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_after_token_open_paren(function_name, first_arg)
            .and_then(|gap| gap.opening_parenthesis_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn should_preserve_component_alignment(
        &self,
        component: &ast::Component,
        gap: &TriviaGap<'_>,
    ) -> bool {
        gap.is_multiple_spaces()
            && (!is_builtin_scalar_type_name(&component.type_name)
                || self
                    .component_layout
                    .has_nearby_declaration_line(component.name_token.location.start_line))
    }

    fn collect_simple_equation_assignment(&mut self, lhs: &ast::Expression, rhs: &ast::Expression) {
        if !self.options.normalize_equation_spacing {
            return;
        }
        let Some(replacement) = self
            .trivia
            .gap_between_expressions(lhs, rhs)
            .and_then(|gap| gap.assignment_operator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_statement_assignment(
        &mut self,
        target: &ast::ComponentReference,
        value: &ast::Expression,
    ) {
        if !self.options.normalize_equation_spacing {
            return;
        }
        let Some(replacement) = self
            .trivia
            .gap_between_component_reference_and_expression(target, value)
            .and_then(|gap| gap.statement_assignment_operator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_connect_comma(
        &mut self,
        lhs: &ast::ComponentReference,
        rhs: &ast::ComponentReference,
    ) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(replacement) = self
            .trivia
            .gap_between_component_references(lhs, rhs)
            .and_then(|gap| gap.comma_separator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_statement_call_output_assignment(
        &mut self,
        outputs: &[ast::Expression],
        target: &ast::ComponentReference,
    ) {
        if !self.options.normalize_equation_spacing {
            return;
        }
        let Some(last_output) = outputs.last() else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_between_expression_and_component_reference(last_output, target)
            .and_then(|gap| gap.statement_assignment_operator_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_binary_operator_spacing(
        &mut self,
        op: &OpBinary,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) {
        if !self.options.normalize_operator_spacing || matches!(op, OpBinary::Assign) {
            return;
        }
        let Some(replacement) = self
            .trivia
            .gap_between_expressions(lhs, rhs)
            .and_then(|gap| gap.binary_operator_replacement(op))
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_extends_base_spacing(&mut self, ext: &ast::Extend) {
        self.collect_keyword_name_spacing("extends", &ext.base_name);
    }

    fn collect_extends_opening_parenthesis_padding(&mut self, ext: &ast::Extend) {
        if !matches!(self.options.profile, FormatProfile::Canonical) {
            return;
        }
        let Some(first_modification) = ext.modifications.first() else {
            return;
        };
        let Some(replacement) = self
            .trivia
            .gap_after_name_open_paren(&ext.base_name, &first_modification.expr)
            .and_then(|gap| gap.opening_parenthesis_replacement())
        else {
            return;
        };
        self.replacements.push(replacement);
    }

    fn collect_constrainedby_spacing(&mut self, name: &ast::Name) {
        self.collect_keyword_name_spacing("constrainedby", name);
    }

    fn collect_for_index_spacing(&mut self, index: &ast::ForIndex) {
        let Some(replacement) = self
            .trivia
            .gap_between_token_and_expression(&index.ident, &index.range)
            .and_then(|gap| gap.keyword_separator_replacement("in"))
        else {
            return;
        };
        self.replacements.push(replacement);
    }
}

struct AstTriviaCoverageCollector<'source, 'options> {
    trivia: SourceTrivia<'source>,
    options: &'options FormatOptions,
    component_layout: &'options ComponentDeclarationLayout,
    report: FormatCoverageReport,
    recorded_ranges: Vec<(usize, usize)>,
    token_spans: Vec<Span>,
}

impl<'source, 'options> AstTriviaCoverageCollector<'source, 'options> {
    fn new(
        source: &'source str,
        options: &'options FormatOptions,
        component_layout: &'options ComponentDeclarationLayout,
    ) -> Self {
        Self {
            trivia: SourceTrivia::new(source),
            options,
            component_layout,
            report: FormatCoverageReport::new(),
            recorded_ranges: Vec::new(),
            token_spans: Vec::new(),
        }
    }

    fn into_report(mut self) -> FormatCoverageReport {
        self.record_unclassified_token_gaps();
        self.report
    }

    fn record_gap(&mut self, category: FormatCoverageCategory, gap: Option<TriviaGap<'source>>) {
        let Some(gap) = gap else {
            return;
        };
        if gap.text.contains(['\n', '\r']) {
            return;
        }
        self.recorded_ranges
            .push((gap.start, gap.start + gap.text.len()));
        self.report
            .record(category, self.category_is_covered(category));
    }

    fn category_is_covered(&self, category: FormatCoverageCategory) -> bool {
        match category {
            FormatCoverageCategory::SimpleEquationAssignment
            | FormatCoverageCategory::StatementAssignment
            | FormatCoverageCategory::StatementCallAssignment
            | FormatCoverageCategory::ComponentBindingAssignment => {
                self.options.normalize_equation_spacing
            }
            FormatCoverageCategory::ArgumentAssignment => {
                self.options.normalize_argument_assignment_spacing
            }
            FormatCoverageCategory::BinaryOperator => self.options.normalize_operator_spacing,
            FormatCoverageCategory::ExpressionListComma
            | FormatCoverageCategory::ConnectComma
            | FormatCoverageCategory::OpeningParenthesisPadding
            | FormatCoverageCategory::ArraySubscriptComma
            | FormatCoverageCategory::StatementSeparator => {
                matches!(self.options.profile, FormatProfile::Canonical)
            }
            FormatCoverageCategory::Unclassified => false,
            FormatCoverageCategory::ClassPrefix
            | FormatCoverageCategory::ClassKeywordName
            | FormatCoverageCategory::ClassEndName
            | FormatCoverageCategory::QualifiedNameDot
            | FormatCoverageCategory::RenamedImportAssignment
            | FormatCoverageCategory::SelectiveImportComma
            | FormatCoverageCategory::ComponentPrefix
            | FormatCoverageCategory::ComponentTypeName
            | FormatCoverageCategory::ComponentDeclarationAlignment
            | FormatCoverageCategory::ExtendsBase
            | FormatCoverageCategory::ConstrainedBy
            | FormatCoverageCategory::ForIndexIn => true,
            FormatCoverageCategory::TypeAliasAssignment => {
                matches!(self.options.profile, FormatProfile::Canonical)
            }
        }
    }

    fn remember_token(&mut self, token: &Token) {
        let span = token_span(token);
        if !span.is_dummy() {
            self.token_spans.push(span);
        }
    }

    fn remember_name_tokens(&mut self, name: &ast::Name) {
        for token in &name.name {
            self.remember_token(token);
        }
    }

    fn remember_component_reference_tokens(&mut self, reference: &ast::ComponentReference) {
        for part in &reference.parts {
            self.remember_token(&part.ident);
        }
    }

    fn record_unclassified_token_gaps(&mut self) {
        self.token_spans
            .sort_by_key(|span| (span.start.0, span.end.0));
        self.token_spans
            .dedup_by_key(|span| (span.start.0, span.end.0));
        self.recorded_ranges.sort_unstable();
        self.recorded_ranges.dedup();

        let spans = self.token_spans.clone();
        for adjacent in spans.windows(2) {
            let left = adjacent[0];
            let right = adjacent[1];
            let Some(gap) = self.trivia.gap_between_spans(left, right) else {
                continue;
            };
            if !is_unclassified_gap_candidate(gap.text) {
                continue;
            }
            let range = (gap.start, gap.start + gap.text.len());
            if self.recorded_ranges_overlap(range) {
                continue;
            }
            if is_closing_expression_separator_gap(gap.text) {
                self.report.record(
                    FormatCoverageCategory::ExpressionListComma,
                    self.category_is_covered(FormatCoverageCategory::ExpressionListComma),
                );
                continue;
            }
            if opening_parenthesis_padding_gap(TriviaGap {
                start: gap.start,
                text: gap.text,
            })
            .is_some()
            {
                self.report.record(
                    FormatCoverageCategory::OpeningParenthesisPadding,
                    self.category_is_covered(FormatCoverageCategory::OpeningParenthesisPadding),
                );
                continue;
            }
            if opening_delimiter_padding_gap(&gap) {
                self.report.record(
                    FormatCoverageCategory::OpeningParenthesisPadding,
                    self.category_is_covered(FormatCoverageCategory::OpeningParenthesisPadding),
                );
                continue;
            }
            if closing_delimiter_binary_operator(gap.text).is_some() {
                self.report.record(
                    FormatCoverageCategory::BinaryOperator,
                    self.category_is_covered(FormatCoverageCategory::BinaryOperator),
                );
                continue;
            }
            if is_closing_delimiter_opening_parenthesis_gap(gap.text) {
                self.report.record(
                    FormatCoverageCategory::OpeningParenthesisPadding,
                    self.category_is_covered(FormatCoverageCategory::OpeningParenthesisPadding),
                );
                continue;
            }
            if is_statement_separator_gap(gap.text) {
                self.report.record(
                    FormatCoverageCategory::StatementSeparator,
                    self.category_is_covered(FormatCoverageCategory::StatementSeparator),
                );
                continue;
            }
            self.report
                .record(FormatCoverageCategory::Unclassified, false);
            self.report
                .record_unclassified_example(self.unclassified_gap_example(left, &gap, right));
        }
    }

    fn recorded_ranges_overlap(&self, range: (usize, usize)) -> bool {
        self.recorded_ranges
            .iter()
            .any(|recorded| recorded.0 < range.1 && range.0 < recorded.1)
    }

    fn unclassified_gap_example(&self, left: Span, gap: &TriviaGap<'_>, right: Span) -> String {
        let left_text = self.source_span_text(left).unwrap_or("<left>");
        let right_text = self.source_span_text(right).unwrap_or("<right>");
        format!(
            "{}{}{}",
            truncate_example(left_text),
            escape_example_gap(gap.text),
            truncate_example(right_text)
        )
    }

    fn source_span_text(&self, span: Span) -> Option<&'source str> {
        self.trivia.source.get(span.start.0..span.end.0)
    }

    fn record_class_gaps(&mut self, class: &ast::ClassDef) {
        self.remember_token(&class.class_type_token);
        self.remember_token(&class.name);
        if let Some(end_name) = &class.end_name_token {
            self.remember_token(end_name);
        }
        self.record_gap(
            FormatCoverageCategory::ClassKeywordName,
            self.trivia
                .gap_between_tokens(&class.class_type_token, &class.name),
        );
        if let Some(end_name) = &class.end_name_token {
            self.record_gap(
                FormatCoverageCategory::ClassEndName,
                self.trivia.gap_after_keyword_before_token("end", end_name),
            );
        }
        if let Some(base) = type_alias_base_name(class) {
            self.record_gap(
                FormatCoverageCategory::TypeAliasAssignment,
                self.trivia.gap_between_token_and_name(&class.name, base),
            );
        }
        let Some(line) = self.trivia.line_before_token(&class.class_type_token) else {
            return;
        };
        for gap in declaration_prefix_gaps_before_token(line, &class_prefix_keywords(class)) {
            self.record_gap(FormatCoverageCategory::ClassPrefix, Some(gap));
        }
    }

    fn record_name_separators(&mut self, name: &ast::Name) {
        self.remember_name_tokens(name);
        for tokens in name.name.windows(2) {
            self.record_gap(
                FormatCoverageCategory::QualifiedNameDot,
                self.trivia.gap_between_tokens(&tokens[0], &tokens[1]),
            );
        }
    }

    fn record_component_reference_separators(&mut self, reference: &ast::ComponentReference) {
        self.remember_component_reference_tokens(reference);
        for parts in reference.parts.windows(2) {
            self.record_gap(
                FormatCoverageCategory::QualifiedNameDot,
                self.trivia
                    .gap_between_tokens(&parts[0].ident, &parts[1].ident),
            );
        }
    }

    fn record_import_gaps(&mut self, import: &ast::Import) {
        match import {
            ast::Import::Renamed { alias, path, .. } => {
                self.remember_token(alias);
                self.record_gap(
                    FormatCoverageCategory::RenamedImportAssignment,
                    self.trivia.gap_between_token_and_name(alias, path),
                );
            }
            ast::Import::Selective { names, .. } => {
                for name in names {
                    self.remember_token(name);
                }
                for adjacent in names.windows(2) {
                    self.record_gap(
                        FormatCoverageCategory::SelectiveImportComma,
                        self.trivia.gap_between_tokens(&adjacent[0], &adjacent[1]),
                    );
                }
            }
            ast::Import::Qualified { .. } | ast::Import::Unqualified { .. } => {}
        }
    }

    fn record_expression_list_commas(&mut self, expressions: &[ast::Expression]) {
        for adjacent in expressions.windows(2) {
            self.record_gap(
                FormatCoverageCategory::ExpressionListComma,
                self.trivia
                    .gap_between_expression_list_items(&adjacent[0], &adjacent[1]),
            );
        }
    }

    fn record_opening_parenthesis_padding(
        &mut self,
        target: &ast::ComponentReference,
        args: &[ast::Expression],
    ) {
        let Some(first_arg) = args.first() else {
            return;
        };
        self.record_gap(
            FormatCoverageCategory::OpeningParenthesisPadding,
            self.trivia.gap_after_call_open_paren(target, first_arg),
        );
    }

    fn record_assert_opening_parenthesis_padding(&mut self, condition: &ast::Expression) {
        self.record_gap(
            FormatCoverageCategory::OpeningParenthesisPadding,
            self.trivia
                .gap_after_keyword_open_paren_before_expression("assert", condition),
        );
    }

    fn record_component_modifier_opening_parenthesis_padding(
        &mut self,
        component: &ast::Component,
    ) {
        let Some(first_modification) = component.source_modifications.first() else {
            return;
        };
        self.record_gap(
            FormatCoverageCategory::OpeningParenthesisPadding,
            self.trivia
                .gap_after_token_open_paren(&component.name_token, first_modification),
        );
    }

    fn record_subscript_commas(&mut self, subscripts: &[ast::Subscript]) {
        for adjacent in subscripts.windows(2) {
            self.record_gap(
                FormatCoverageCategory::ArraySubscriptComma,
                self.trivia
                    .gap_between_subscripts(&adjacent[0], &adjacent[1]),
            );
        }
    }

    fn record_extend_modification_commas(&mut self, modifications: &[ast::ExtendModification]) {
        for adjacent in modifications.windows(2) {
            self.record_gap(
                FormatCoverageCategory::ExpressionListComma,
                self.trivia
                    .gap_between_extend_modifications(&adjacent[0], &adjacent[1]),
            );
        }
    }

    fn record_component_gaps(&mut self, component: &ast::Component) {
        self.remember_token(&component.name_token);
        if let Some(first_type_part) = component.type_name.name.first()
            && let Some(line) = self.trivia.line_before_token(first_type_part)
        {
            for gap in
                declaration_prefix_gaps_before_token(line, &component_prefix_keywords(component))
            {
                self.record_gap(FormatCoverageCategory::ComponentPrefix, Some(gap));
            }
        }
        let Some(gap) = self.component_type_name_gap(component) else {
            return;
        };
        if self.should_preserve_component_alignment(component, &gap) {
            self.record_gap(
                FormatCoverageCategory::ComponentDeclarationAlignment,
                Some(gap),
            );
        } else {
            self.record_gap(FormatCoverageCategory::ComponentTypeName, Some(gap));
        }
        if let Some(binding) = &component.binding {
            let gap = component
                .source_modifications
                .last()
                .and_then(|last| {
                    self.trivia
                        .gap_between_expression_list_item_and_expression(last, binding)
                })
                .or_else(|| {
                    self.trivia
                        .gap_between_token_and_expression(&component.name_token, binding)
                });
            self.record_gap(FormatCoverageCategory::ComponentBindingAssignment, gap);
        }
    }

    fn component_type_name_gap(&self, component: &ast::Component) -> Option<TriviaGap<'source>> {
        if let Some(last_shape) = component.shape_expr.last()
            && let Some(shape_span) = subscript_span(last_shape)
            && shape_span.end.0 <= token_span(&component.name_token).start.0
        {
            return self
                .trivia
                .gap_between_spans(shape_span, token_span(&component.name_token));
        }
        self.trivia
            .gap_between_name_and_token(&component.type_name, &component.name_token)
    }

    fn record_redeclare_modification_type_name_spacing(&mut self, expr: &ast::Expression) {
        self.record_gap(
            FormatCoverageCategory::ComponentTypeName,
            self.redeclare_modification_type_name_gap(expr),
        );
    }

    fn record_redeclare_modification_opening_parenthesis_padding(
        &mut self,
        expr: &ast::Expression,
    ) {
        let Some((target, modifications)) = redeclare_modification_class_target(expr) else {
            return;
        };
        self.record_opening_parenthesis_padding(target, modifications);
    }

    fn record_class_modification_redeclare_gaps(
        &mut self,
        modifications: &[ast::Expression],
        redeclare_flags: &[bool],
    ) {
        for (modification, redeclare) in modifications.iter().zip(redeclare_flags) {
            if !*redeclare {
                continue;
            }
            self.record_redeclare_modification_type_name_spacing(modification);
            self.record_redeclare_modification_opening_parenthesis_padding(modification);
        }
    }

    fn redeclare_modification_type_name_gap(
        &self,
        expr: &ast::Expression,
    ) -> Option<TriviaGap<'source>> {
        let ast::Expression::Modification { target, value, .. } = expr else {
            return None;
        };
        let ast::Expression::ClassModification {
            target: replacement_type,
            ..
        } = value.as_ref()
        else {
            return None;
        };
        self.trivia.gap_between_tokens(
            &replacement_type.parts.last()?.ident,
            &target.parts.first()?.ident,
        )
    }

    fn record_external_function_spacing(&mut self, external: &ast::ExternalFunction) {
        if let Some(function_name) = &external.function_name
            && let Some(first_arg) = external.args.first()
        {
            self.record_gap(
                FormatCoverageCategory::OpeningParenthesisPadding,
                self.trivia
                    .gap_after_token_open_paren(function_name, first_arg),
            );
        }
        self.record_expression_list_commas(&external.args);
    }

    fn record_extends_opening_parenthesis_padding(&mut self, ext: &ast::Extend) {
        let Some(first_modification) = ext.modifications.first() else {
            return;
        };
        self.record_gap(
            FormatCoverageCategory::OpeningParenthesisPadding,
            self.trivia
                .gap_after_name_open_paren(&ext.base_name, &first_modification.expr),
        );
    }

    fn should_preserve_component_alignment(
        &self,
        component: &ast::Component,
        gap: &TriviaGap<'_>,
    ) -> bool {
        gap.is_multiple_spaces()
            && (!is_builtin_scalar_type_name(&component.type_name)
                || self
                    .component_layout
                    .has_nearby_declaration_line(component.name_token.location.start_line))
    }

    fn record_binary_operator(
        &mut self,
        op: &OpBinary,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) {
        if matches!(op, OpBinary::Assign | OpBinary::Empty) {
            return;
        }
        self.record_gap(
            FormatCoverageCategory::BinaryOperator,
            self.trivia.gap_between_expressions(lhs, rhs),
        );
    }

    fn record_argument_assignment(&mut self, expr: &ast::Expression) {
        match expr {
            ast::Expression::NamedArgument { name, value, .. } => {
                self.record_gap(
                    FormatCoverageCategory::ArgumentAssignment,
                    self.trivia.gap_between_token_and_expression(name, value),
                );
            }
            ast::Expression::Modification { target, value, .. } => {
                self.record_gap(
                    FormatCoverageCategory::ArgumentAssignment,
                    self.trivia
                        .gap_between_component_reference_and_expression(target, value),
                );
            }
            ast::Expression::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } => {
                self.record_gap(
                    FormatCoverageCategory::ArgumentAssignment,
                    self.trivia.gap_between_expressions(lhs, rhs),
                );
            }
            _ => {}
        }
    }
}

impl ast::Visitor for AstTriviaCoverageCollector<'_, '_> {
    fn enter_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        self.record_subscript_commas(&class.array_subscripts);
        self.record_expression_list_commas(&class.annotation);
        Continue(())
    }

    fn enter_scope(&mut self, scope: ast::VisitScope<'_>) -> ControlFlow<()> {
        if let ast::VisitScope::Class(class) = scope {
            self.record_class_gaps(class);
        }
        Continue(())
    }

    fn visit_type_name(&mut self, name: &ast::Name, ctx: ast::TypeNameContext) -> ControlFlow<()> {
        if matches!(
            ctx,
            ast::TypeNameContext::ClassConstrainedBy | ast::TypeNameContext::ComponentConstrainedBy
        ) {
            self.record_gap(
                FormatCoverageCategory::ConstrainedBy,
                self.trivia
                    .gap_after_keyword_before_name("constrainedby", name),
            );
        }
        self.record_name_separators(name);
        Continue(())
    }

    fn visit_name_ctx(&mut self, name: &ast::Name, _ctx: ast::NameContext) -> ControlFlow<()> {
        self.record_name_separators(name);
        Continue(())
    }

    fn enter_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        match expr {
            ast::Expression::Terminal { token, .. } => {
                self.remember_token(token);
            }
            ast::Expression::ComponentReference(_) | ast::Expression::FunctionCall { .. } => {}
            ast::Expression::ClassModification {
                target,
                modifications,
                redeclare_flags,
                ..
            } => {
                self.record_opening_parenthesis_padding(target, modifications);
                self.record_expression_list_commas(modifications);
                self.record_class_modification_redeclare_gaps(modifications, redeclare_flags);
            }
            ast::Expression::NamedArgument { name, .. } => {
                self.remember_token(name);
                self.record_argument_assignment(expr);
            }
            ast::Expression::Modification { .. } => {
                self.record_argument_assignment(expr);
            }
            ast::Expression::Binary { op, lhs, rhs, .. } => {
                self.record_binary_operator(op, lhs, rhs);
                if matches!(op, OpBinary::Assign) {
                    self.record_argument_assignment(expr);
                }
            }
            ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
                self.record_expression_list_commas(elements);
            }
            ast::Expression::ArrayIndex { subscripts, .. } => {
                self.record_subscript_commas(subscripts);
            }
            _ => {}
        }
        Continue(())
    }

    fn enter_component_reference(
        &mut self,
        reference: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        self.record_component_reference_separators(reference);
        for part in &reference.parts {
            if let Some(subscripts) = &part.subs {
                self.record_subscript_commas(subscripts);
            }
        }
        Continue(())
    }

    fn enter_expr_function_call(
        &mut self,
        target: &ast::ComponentReference,
        args: &[ast::Expression],
        _ctx: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        self.record_opening_parenthesis_padding(target, args);
        self.record_expression_list_commas(args);
        Continue(())
    }

    fn enter_equation_assert(
        &mut self,
        condition: &ast::Expression,
        _message: &ast::Expression,
        _level: Option<&ast::Expression>,
    ) -> ControlFlow<()> {
        self.record_assert_opening_parenthesis_padding(condition);
        Continue(())
    }

    fn enter_statement_assert(
        &mut self,
        condition: &ast::Expression,
        _message: &ast::Expression,
        _level: Option<&ast::Expression>,
    ) -> ControlFlow<()> {
        self.record_assert_opening_parenthesis_padding(condition);
        Continue(())
    }

    fn enter_external_function(&mut self, external: &ast::ExternalFunction) -> ControlFlow<()> {
        self.record_external_function_spacing(external);
        Continue(())
    }

    fn enter_import(&mut self, import: &ast::Import) -> ControlFlow<()> {
        self.record_import_gaps(import);
        Continue(())
    }

    fn enter_extend(&mut self, ext: &ast::Extend) -> ControlFlow<()> {
        self.record_extend_modification_commas(&ext.modifications);
        self.record_expression_list_commas(&ext.annotation);
        self.record_extends_opening_parenthesis_padding(ext);
        for modification in &ext.modifications {
            if modification.redeclare {
                self.record_redeclare_modification_type_name_spacing(&modification.expr);
                self.record_redeclare_modification_opening_parenthesis_padding(&modification.expr);
            }
        }
        self.record_gap(
            FormatCoverageCategory::ExtendsBase,
            self.trivia
                .gap_after_keyword_before_name("extends", &ext.base_name),
        );
        Continue(())
    }

    fn enter_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        self.record_subscript_commas(&component.shape_expr);
        self.record_component_modifier_opening_parenthesis_padding(component);
        self.record_expression_list_commas(&component.annotation);
        self.record_expression_list_commas(&component.source_modifications);
        for (modification, redeclare) in component
            .source_modifications
            .iter()
            .zip(&component.source_modification_redeclare_flags)
        {
            if *redeclare {
                self.record_redeclare_modification_type_name_spacing(modification);
                self.record_redeclare_modification_opening_parenthesis_padding(modification);
            }
        }
        self.record_component_gaps(component);
        Continue(())
    }

    fn enter_simple_equation(
        &mut self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> ControlFlow<()> {
        self.record_gap(
            FormatCoverageCategory::SimpleEquationAssignment,
            self.trivia.gap_between_expressions(lhs, rhs),
        );
        Continue(())
    }

    fn enter_connect(
        &mut self,
        lhs: &ast::ComponentReference,
        rhs: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        self.remember_component_reference_tokens(lhs);
        self.remember_component_reference_tokens(rhs);
        self.record_gap(
            FormatCoverageCategory::ConnectComma,
            self.trivia.gap_between_component_references(lhs, rhs),
        );
        Continue(())
    }

    fn enter_for_index(&mut self, index: &ast::ForIndex) -> ControlFlow<()> {
        self.remember_token(&index.ident);
        self.record_gap(
            FormatCoverageCategory::ForIndexIn,
            self.trivia
                .gap_between_token_and_expression(&index.ident, &index.range),
        );
        Continue(())
    }

    fn enter_assignment(
        &mut self,
        target: &ast::ComponentReference,
        value: &ast::Expression,
    ) -> ControlFlow<()> {
        self.remember_component_reference_tokens(target);
        self.record_gap(
            FormatCoverageCategory::StatementAssignment,
            self.trivia
                .gap_between_component_reference_and_expression(target, value),
        );
        Continue(())
    }

    fn enter_statement_function_call(
        &mut self,
        target: &ast::ComponentReference,
        _args: &[ast::Expression],
        outputs: &[ast::Expression],
    ) -> ControlFlow<()> {
        self.remember_component_reference_tokens(target);
        if let Some(last_output) = outputs.last() {
            self.record_gap(
                FormatCoverageCategory::StatementCallAssignment,
                self.trivia
                    .gap_between_expression_and_component_reference(last_output, target),
            );
        }
        self.record_expression_list_commas(outputs);
        Continue(())
    }
}

impl ast::Visitor for AstTriviaReplacementCollector<'_, '_> {
    fn enter_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        self.collect_subscript_commas(&class.array_subscripts);
        self.collect_expression_list_commas(&class.annotation);
        Continue(())
    }

    fn enter_scope(&mut self, scope: ast::VisitScope<'_>) -> ControlFlow<()> {
        if let ast::VisitScope::Class(class) = scope {
            self.collect_class_prefix_spacing(class);
            self.collect_class_keyword_spacing(class);
            self.collect_class_end_name_spacing(class);
            self.collect_type_alias_assignment_spacing(class);
        }
        Continue(())
    }

    fn visit_type_name(&mut self, name: &ast::Name, ctx: ast::TypeNameContext) -> ControlFlow<()> {
        if matches!(
            ctx,
            ast::TypeNameContext::ClassConstrainedBy | ast::TypeNameContext::ComponentConstrainedBy
        ) {
            self.collect_constrainedby_spacing(name);
        }
        self.collect_name_separators(name);
        Continue(())
    }

    fn visit_name_ctx(&mut self, name: &ast::Name, _ctx: ast::NameContext) -> ControlFlow<()> {
        self.collect_name_separators(name);
        Continue(())
    }

    fn enter_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        match expr {
            ast::Expression::Terminal { token, .. } => {
                self.remember_token(token);
            }
            ast::Expression::Binary { op, lhs, rhs, .. } => {
                self.collect_binary_operator_spacing(op, lhs, rhs);
                if matches!(op, OpBinary::Assign) {
                    self.collect_argument_assignment_spacing(expr);
                }
            }
            ast::Expression::NamedArgument { name, .. } => {
                self.remember_token(name);
                self.collect_argument_assignment_spacing(expr);
            }
            ast::Expression::Modification { .. } => {
                self.collect_argument_assignment_spacing(expr);
            }
            ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
                self.collect_expression_list_commas(elements);
            }
            ast::Expression::ArrayIndex { subscripts, .. } => {
                self.collect_subscript_commas(subscripts);
            }
            ast::Expression::ClassModification {
                target,
                modifications,
                redeclare_flags,
                ..
            } => {
                self.collect_opening_parenthesis_padding(target, modifications);
                self.collect_expression_list_commas(modifications);
                self.collect_class_modification_redeclare_spacing(modifications, redeclare_flags);
            }
            _ => {}
        }
        Continue(())
    }

    fn enter_component_reference(
        &mut self,
        reference: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        self.collect_component_reference_separators(reference);
        for part in &reference.parts {
            if let Some(subscripts) = &part.subs {
                self.collect_subscript_commas(subscripts);
            }
        }
        Continue(())
    }

    fn enter_expr_function_call(
        &mut self,
        target: &ast::ComponentReference,
        args: &[ast::Expression],
        _ctx: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        self.collect_opening_parenthesis_padding(target, args);
        self.collect_expression_list_commas(args);
        Continue(())
    }

    fn enter_equation_assert(
        &mut self,
        condition: &ast::Expression,
        _message: &ast::Expression,
        _level: Option<&ast::Expression>,
    ) -> ControlFlow<()> {
        self.collect_assert_opening_parenthesis_padding(condition);
        Continue(())
    }

    fn enter_statement_assert(
        &mut self,
        condition: &ast::Expression,
        _message: &ast::Expression,
        _level: Option<&ast::Expression>,
    ) -> ControlFlow<()> {
        self.collect_assert_opening_parenthesis_padding(condition);
        Continue(())
    }

    fn enter_external_function(&mut self, external: &ast::ExternalFunction) -> ControlFlow<()> {
        self.collect_external_function_spacing(external);
        Continue(())
    }

    fn enter_import(&mut self, import: &ast::Import) -> ControlFlow<()> {
        self.collect_import_spacing(import);
        Continue(())
    }

    fn enter_extend(&mut self, ext: &ast::Extend) -> ControlFlow<()> {
        self.collect_extend_modification_commas(&ext.modifications);
        self.collect_expression_list_commas(&ext.annotation);
        self.collect_extends_opening_parenthesis_padding(ext);
        for modification in &ext.modifications {
            if modification.redeclare {
                self.collect_redeclare_modification_type_name_spacing(&modification.expr);
                self.collect_redeclare_modification_opening_parenthesis_padding(&modification.expr);
            }
        }
        self.collect_extends_base_spacing(ext);
        Continue(())
    }

    fn enter_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        self.collect_subscript_commas(&component.shape_expr);
        self.collect_component_modifier_opening_parenthesis_padding(component);
        self.collect_expression_list_commas(&component.annotation);
        self.collect_expression_list_commas(&component.source_modifications);
        for (modification, redeclare) in component
            .source_modifications
            .iter()
            .zip(&component.source_modification_redeclare_flags)
        {
            if *redeclare {
                self.collect_redeclare_modification_type_name_spacing(modification);
                self.collect_redeclare_modification_opening_parenthesis_padding(modification);
            }
        }
        self.collect_component_prefix_spacing(component);
        self.collect_component_type_name_spacing(component);
        self.collect_component_binding_assignment(component);
        Continue(())
    }

    fn enter_simple_equation(
        &mut self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> ControlFlow<()> {
        self.collect_simple_equation_assignment(lhs, rhs);
        Continue(())
    }

    fn enter_connect(
        &mut self,
        lhs: &ast::ComponentReference,
        rhs: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        self.collect_connect_comma(lhs, rhs);
        Continue(())
    }

    fn enter_for_index(&mut self, index: &ast::ForIndex) -> ControlFlow<()> {
        self.collect_for_index_spacing(index);
        Continue(())
    }

    fn enter_assignment(
        &mut self,
        target: &ast::ComponentReference,
        value: &ast::Expression,
    ) -> ControlFlow<()> {
        self.collect_statement_assignment(target, value);
        Continue(())
    }

    fn enter_statement_function_call(
        &mut self,
        target: &ast::ComponentReference,
        _args: &[ast::Expression],
        outputs: &[ast::Expression],
    ) -> ControlFlow<()> {
        self.collect_statement_call_output_assignment(outputs, target);
        self.collect_expression_list_commas(outputs);
        Continue(())
    }
}

#[cfg(test)]
mod tests;
