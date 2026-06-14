use super::*;
use indexmap::IndexMap;
use rumoca_ir_dae::{self as dae, DaeVisitor};

type ComponentReference = rumoca_core::ComponentReference;
type ComprehensionIndex = rumoca_core::ComprehensionIndex;
type Dae = dae::Dae;
type Expression = rumoca_core::Expression;
type ForIndex = rumoca_core::ForIndex;
type Function = rumoca_core::Function;
type Statement = rumoca_core::Statement;
type StatementBlock = rumoca_core::StatementBlock;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;

use rumoca_core::split_path_with_indices;

struct KnownReferenceIndex {
    flat_queries: HashSet<String>,
    dae_queries: HashSet<String>,
    enum_literal_queries: HashSet<String>,
}

impl KnownReferenceIndex {
    fn build(dae: &Dae, known_flat_var_names: &HashSet<String>) -> Self {
        let mut dae_var_collector = DaeVariableNameCollector { names: Vec::new() };
        dae_var_collector.visit_variables(&dae.variables);
        Self {
            flat_queries: build_flat_reference_query_set(
                known_flat_var_names.iter().map(String::as_str),
            ),
            dae_queries: build_dae_reference_query_set(
                dae_var_collector.names.iter().map(String::as_str),
            ),
            enum_literal_queries: build_enum_literal_query_set(&dae.symbols.enum_literal_ordinals),
        }
    }
}

struct DaeVariableNameCollector {
    names: Vec<String>,
}

impl DaeVisitor for DaeVariableNameCollector {
    fn visit_variable(
        &mut self,
        _partition: dae::DaeVariablePartition,
        name: &VarName,
        _variable: &dae::Variable,
    ) {
        self.names.push(name.as_str().to_string());
    }
}

fn build_flat_reference_query_set<'a>(names: impl Iterator<Item = &'a str>) -> HashSet<String> {
    let mut queries = HashSet::new();
    for name in names {
        insert_reference_query_alias(&mut queries, name);
        insert_reference_query_alias(&mut queries, &path_utils::strip_all_subscripts(name));
        insert_ancestor_reference_queries(&mut queries, name);
    }
    queries
}

fn build_dae_reference_query_set<'a>(names: impl Iterator<Item = &'a str>) -> HashSet<String> {
    let mut queries = HashSet::new();
    for name in names {
        queries.insert(name.to_string());
        insert_ancestor_reference_queries(&mut queries, name);
        if !path_utils::has_top_level_dot(name) && path_utils::has_top_level_subscript(name) {
            insert_reference_query_alias(
                &mut queries,
                path_utils::normalize_top_level_segment(name),
            );
        }
    }
    queries
}

pub(crate) fn build_enum_literal_query_set(ordinals: &IndexMap<String, i64>) -> HashSet<String> {
    let mut queries = HashSet::with_capacity(ordinals.len().saturating_mul(2));
    for literal in ordinals.keys() {
        queries.insert(literal.clone());
        if let Some(alias) = alternate_enum_literal_alias(literal) {
            queries.insert(alias);
        }
    }
    queries
}

fn alternate_enum_literal_alias(name: &str) -> Option<String> {
    let (prefix, literal) = rumoca_core::split_last_top_level(name)?;
    if literal.len() >= 2 && literal.starts_with('\'') && literal.ends_with('\'') {
        let unquoted = &literal[1..literal.len() - 1];
        return Some(format!("{prefix}.{unquoted}"));
    }
    Some(format!("{prefix}.'{literal}'"))
}

fn insert_reference_query_alias(queries: &mut HashSet<String>, name: &str) {
    if !name.is_empty() {
        queries.insert(name.to_string());
    }
}

fn insert_ancestor_reference_queries(queries: &mut HashSet<String>, name: &str) {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    for idx in 1..path.len() {
        let prefix = path
            .prefix(idx)
            .expect("ancestor prefix index is in range")
            .to_flat_string();
        insert_reference_query_alias(queries, &prefix);
        let normalized = path_utils::strip_all_subscripts(&prefix);
        insert_reference_query_alias(queries, &normalized);
    }
}

fn short_leaf_matches(candidate: &str, short: &str) -> bool {
    rumoca_core::top_level_last_segment(candidate) == final_path_segment(short)
}

fn final_path_segment(path: &str) -> &str {
    let mut bracket_depth = 0usize;
    let mut last_boundary = 0usize;
    for (idx, ch) in path.char_indices() {
        match ch {
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '.' if bracket_depth == 0 => last_boundary = idx + ch.len_utf8(),
            _ => {}
        }
    }
    &path[last_boundary..]
}

fn enum_literal_alias_matches(candidate: &str, raw: &str) -> bool {
    let raw_literal = rumoca_core::top_level_last_segment(raw);
    let candidate_literal = rumoca_core::top_level_last_segment(candidate);
    raw_literal == candidate_literal && is_quoted_enum_literal(raw_literal)
}

fn is_quoted_enum_literal(segment: &str) -> bool {
    segment.len() >= 2 && segment.starts_with('\'') && segment.ends_with('\'')
}

fn validate_constructor_field_selection(
    base: &Expression,
    field: &str,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    } = base
    {
        for arg in args {
            validate_expression_constructor_selections(arg, functions, span)?;
        }

        let selected_name = format!("{}.{}", name.as_str(), field);
        let short = rumoca_core::top_level_last_segment(name.as_str()).to_string();
        let mut short_matches: Vec<(&VarName, &Function)> = Vec::new();
        let constructor = if let Some(constructor) = functions.get(name.var_name()) {
            constructor
        } else {
            short_matches.extend(
                functions
                    .iter()
                    .filter(|(candidate, _)| short_leaf_matches(candidate.as_str(), &short)),
            );
            if short_matches.len() == 1 {
                short_matches[0].1
            } else {
                if crate::todae_debug_enabled() {
                    let mut candidates: Vec<String> =
                        functions.keys().map(|f| f.as_str().to_string()).collect();
                    candidates.sort();
                    let mut short_match_names: Vec<String> = candidates
                        .iter()
                        .filter(|candidate| short_leaf_matches(candidate, &short))
                        .cloned()
                        .collect();
                    short_match_names.sort();
                    crate::log_todae_debug(format!(
                        "DEBUG TODAE missing constructor selection={} args_len={} short_matches={short_match_names:?} total_functions={}",
                        selected_name,
                        args.len(),
                        candidates.len()
                    ));
                }
                return Err(ToDaeError::constructor_field_selection_unresolved(
                    selected_name,
                    span,
                ));
            }
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        if !field_known {
            if crate::todae_debug_enabled() {
                let mut available_fields: Vec<String> = constructor
                    .inputs
                    .iter()
                    .map(|param| format!("in:{}", param.name))
                    .chain(
                        constructor
                            .outputs
                            .iter()
                            .map(|param| format!("out:{}", param.name)),
                    )
                    .collect();
                available_fields.sort();
                crate::log_todae_debug(format!(
                    "DEBUG TODAE constructor field missing selection={} available={available_fields:?}",
                    selected_name
                ));
            }
            return Err(ToDaeError::constructor_field_selection_unresolved(
                selected_name,
                span,
            ));
        }
        return Ok(());
    }

    validate_expression_constructor_selections(base, functions, span)
}

fn validate_expression_constructor_selections(
    expr: &Expression,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    let span = expr.span().unwrap_or(span);
    match expr {
        Expression::FunctionCall { args, .. } | Expression::BuiltinCall { args, .. } => {
            for arg in args {
                validate_expression_constructor_selections(arg, functions, span)?;
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            validate_expression_constructor_selections(lhs, functions, span)?;
            validate_expression_constructor_selections(rhs, functions, span)?;
        }
        Expression::Unary { rhs, .. } => {
            validate_expression_constructor_selections(rhs, functions, span)?;
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                validate_expression_constructor_selections(cond, functions, span)?;
                validate_expression_constructor_selections(value, functions, span)?;
            }
            validate_expression_constructor_selections(else_branch, functions, span)?;
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                validate_expression_constructor_selections(element, functions, span)?;
            }
        }
        Expression::Range {
            start, step, end, ..
        } => {
            validate_expression_constructor_selections(start, functions, span)?;
            if let Some(step) = step {
                validate_expression_constructor_selections(step, functions, span)?;
            }
            validate_expression_constructor_selections(end, functions, span)?;
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            validate_expression_constructor_selections(base, functions, span)?;
            for subscript in subscripts {
                if let Subscript::Expr { expr, .. } = subscript {
                    validate_expression_constructor_selections(expr, functions, span)?;
                }
            }
        }
        Expression::FieldAccess { base, field, .. } => {
            validate_constructor_field_selection(base, field, functions, span)?;
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            validate_expression_constructor_selections(expr, functions, span)?;
            for index in indices {
                validate_expression_constructor_selections(&index.range, functions, span)?;
            }
            if let Some(filter_expr) = filter {
                validate_expression_constructor_selections(filter_expr, functions, span)?;
            }
        }
        Expression::VarRef { .. }
        | Expression::Literal { value: _, .. }
        | Expression::Empty { .. } => {}
    }
    Ok(())
}

fn validate_statement_constructor_selections(
    stmt: &Statement,
    functions: &IndexMap<VarName, Function>,
    _span: Span,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Assignment {
            value,
            span: stmt_span,
            ..
        }
        | Statement::Reinit {
            value,
            span: stmt_span,
            ..
        } => validate_expression_constructor_selections(value, functions, *stmt_span)?,
        Statement::For {
            indices,
            equations,
            span: stmt_span,
        } => {
            let span = *stmt_span;
            for index in indices {
                validate_expression_constructor_selections(&index.range, functions, span)?;
            }
            for nested in equations {
                validate_statement_constructor_selections(nested, functions, span)?;
            }
        }
        Statement::While {
            block,
            span: stmt_span,
        } => {
            let span = *stmt_span;
            validate_expression_constructor_selections(&block.cond, functions, span)?;
            for nested in &block.stmts {
                validate_statement_constructor_selections(nested, functions, span)?;
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            span: stmt_span,
        } => {
            let span = *stmt_span;
            for block in cond_blocks {
                validate_expression_constructor_selections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_selections(nested, functions, span)?;
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    validate_statement_constructor_selections(nested, functions, span)?;
                }
            }
        }
        Statement::When {
            blocks,
            span: stmt_span,
        } => {
            let span = *stmt_span;
            for block in blocks {
                validate_expression_constructor_selections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_selections(nested, functions, span)?;
                }
            }
        }
        Statement::FunctionCall {
            args,
            span: stmt_span,
            ..
        } => {
            for arg in args {
                validate_expression_constructor_selections(arg, functions, *stmt_span)?;
            }
        }
        Statement::Assert {
            condition,
            message,
            level,
            span: stmt_span,
        } => {
            let span = *stmt_span;
            validate_expression_constructor_selections(condition, functions, span)?;
            validate_expression_constructor_selections(message, functions, span)?;
            if let Some(level) = level {
                validate_expression_constructor_selections(level, functions, span)?;
            }
        }
        Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
    }
    Ok(())
}

pub(super) fn validate_dae_constructor_field_selections(dae: &Dae) -> Result<(), ToDaeError> {
    let mut validator = ConstructorSelectionAttributeValidator {
        functions: &dae.symbols.functions,
        result: Ok(()),
    };
    validator.visit_variables(&dae.variables);
    validator.result?;

    for equation in dae
        .continuous
        .equations
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .chain(dae.discrete.valued_updates.iter())
        .chain(dae.conditions.equations.iter())
        .chain(dae.initialization.equations.iter())
    {
        validate_expression_constructor_selections(
            &equation.rhs,
            &dae.symbols.functions,
            equation.span,
        )?;
    }

    for function in dae.symbols.functions.values() {
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_expression_constructor_selections(
                    default_expr,
                    &dae.symbols.functions,
                    function.span,
                )?;
            }
        }
        for statement in &function.body {
            validate_statement_constructor_selections(
                statement,
                &dae.symbols.functions,
                function.span,
            )?;
        }
    }

    Ok(())
}

struct ConstructorSelectionAttributeValidator<'a> {
    functions: &'a IndexMap<VarName, Function>,
    result: Result<(), ToDaeError>,
}

impl DaeVisitor for ConstructorSelectionAttributeValidator<'_> {
    fn visit_variable(
        &mut self,
        _partition: dae::DaeVariablePartition,
        _name: &VarName,
        variable: &dae::Variable,
    ) {
        if self.result.is_err() {
            return;
        }
        for (expr, span) in variable_attribute_expressions(variable) {
            self.result = validate_expression_constructor_selections(
                expr,
                self.functions,
                variable_attribute_span(expr, span),
            );
            if self.result.is_err() {
                return;
            }
        }
    }
}

pub(super) fn validate_dae_references(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    let known_refs = KnownReferenceIndex::build(dae, known_flat_var_names);
    validate_variable_reference_attributes(dae, &known_refs)?;
    validate_equation_rhs_references(dae, &known_refs)?;
    validate_relation_references(dae, &known_refs)?;
    validate_function_references(dae, &known_refs)?;
    Ok(())
}

fn validate_variable_reference_attributes(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    let mut validator = VariableReferenceAttributeValidator {
        dae,
        known_refs,
        result: Ok(()),
    };
    validator.visit_variables(&dae.variables);
    validator.result
}

struct VariableReferenceAttributeValidator<'a> {
    dae: &'a Dae,
    known_refs: &'a KnownReferenceIndex,
    result: Result<(), ToDaeError>,
}

impl DaeVisitor for VariableReferenceAttributeValidator<'_> {
    fn visit_variable(
        &mut self,
        _partition: dae::DaeVariablePartition,
        _name: &VarName,
        variable: &dae::Variable,
    ) {
        if self.result.is_err() {
            return;
        }
        for (expr, span) in variable_attribute_expressions(variable) {
            self.result = validate_expression_references(
                expr,
                self.dae,
                variable_attribute_span(expr, span),
                None,
                self.known_refs,
            );
            if self.result.is_err() {
                return;
            }
        }
    }
}

fn validate_equation_rhs_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for equation in dae
        .continuous
        .equations
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .chain(dae.discrete.valued_updates.iter())
        .chain(dae.conditions.equations.iter())
        .chain(dae.initialization.equations.iter())
    {
        validate_expression_references(&equation.rhs, dae, equation.span, None, known_refs)?;
    }
    Ok(())
}

fn validate_relation_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for relation in &dae.conditions.relations {
        validate_expression_references(
            relation,
            dae,
            expression_validation_span(relation, Span::DUMMY),
            None,
            known_refs,
        )?;
    }
    Ok(())
}

fn expression_validation_span(expr: &Expression, fallback: Span) -> Span {
    expr.span().unwrap_or(fallback)
}

fn variable_attribute_expressions(
    variable: &dae::Variable,
) -> impl Iterator<Item = (&Expression, Option<Span>)> {
    [
        (variable.start.as_ref(), variable.start_attribute_span()),
        (variable.min.as_ref(), variable.min_attribute_span()),
        (variable.max.as_ref(), variable.max_attribute_span()),
        (variable.nominal.as_ref(), variable.nominal_attribute_span()),
    ]
    .into_iter()
    .filter_map(|(expr, span)| expr.map(|expr| (expr, span)))
}

fn variable_attribute_span(expr: &Expression, span: Option<Span>) -> Span {
    span.unwrap_or_else(|| expression_validation_span(expr, Span::DUMMY))
}

fn validate_function_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for function in dae.symbols.functions.values() {
        let function_scope: HashSet<&str> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.as_str())
            .collect();

        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_expression_references(
                    default_expr,
                    dae,
                    function.span,
                    Some(&function_scope),
                    known_refs,
                )
                .map_err(|err| function_reference_validation_context(err, function))?;
            }
        }

        validate_statement_slice_references(
            &function.body,
            dae,
            function.span,
            Some(&function_scope),
            known_refs,
        )
        .map_err(|err| function_reference_validation_context(err, function))?;
    }
    Ok(())
}

fn function_reference_validation_context(
    err: ToDaeError,
    function: &rumoca_core::Function,
) -> ToDaeError {
    let function_name = function.name.as_str();
    match err {
        ToDaeError::UnresolvedReference { name, span } => ToDaeError::UnresolvedReference {
            name: format!(
                "{name} (while validating DAE function {function_name}{})",
                dae_function_scope_debug_suffix(function)
            ),
            span,
        },
        other => other,
    }
}

fn dae_function_scope_debug_suffix(function: &rumoca_core::Function) -> String {
    if !dae_function_scope_debug_enabled() {
        return String::new();
    }
    let inputs = function
        .inputs
        .iter()
        .map(|param| format!("{}:{}", param.name, param.type_name))
        .collect::<Vec<_>>()
        .join(",");
    let outputs = function
        .outputs
        .iter()
        .map(|param| format!("{}:{}", param.name, param.type_name))
        .collect::<Vec<_>>()
        .join(",");
    let locals = function
        .locals
        .iter()
        .map(|param| format!("{}:{}", param.name, param.type_name))
        .collect::<Vec<_>>()
        .join(",");
    format!(" inputs=[{inputs}] outputs=[{outputs}] locals=[{locals}]")
}

fn dae_function_scope_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(
            target: "rumoca_phase_dae::function_scope",
            tracing::Level::DEBUG
        )
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn validate_statement_slice_references(
    statements: &[Statement],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for statement in statements {
        validate_statement_references(statement, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_statement_references(
    stmt: &Statement,
    dae: &Dae,
    _span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => Ok(()),
        Statement::Assignment {
            comp, value, span, ..
        } => validate_assignment_statement_references(
            comp,
            value,
            dae,
            *span,
            function_scope,
            known_refs,
        ),
        Statement::For {
            indices,
            equations,
            span,
        } => validate_for_statement_references(
            indices,
            equations,
            dae,
            *span,
            function_scope,
            known_refs,
        ),
        Statement::While { block, span } => {
            validate_block_statement_references(block, dae, *span, function_scope, known_refs)
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => validate_if_statement_references(
            cond_blocks,
            else_block.as_deref(),
            dae,
            *span,
            function_scope,
            known_refs,
        ),
        Statement::When { blocks, span } => {
            validate_when_statement_references(blocks, dae, *span, function_scope, known_refs)
        }
        Statement::FunctionCall {
            args,
            outputs,
            span,
            ..
        } => validate_function_call_statement_references(
            args,
            outputs,
            dae,
            *span,
            function_scope,
            known_refs,
        ),
        Statement::Reinit {
            variable,
            value,
            span,
        } => validate_reinit_statement_references(
            variable,
            value,
            dae,
            *span,
            function_scope,
            known_refs,
        ),
        Statement::Assert {
            condition,
            message,
            level,
            span,
        } => validate_assert_statement_references(
            condition,
            message,
            level.as_deref(),
            dae,
            *span,
            function_scope,
            known_refs,
        ),
    }
}

fn validate_assignment_statement_references(
    comp: &ComponentReference,
    value: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    validate_component_reference_target(comp, dae, span, function_scope, known_refs)?;
    validate_expression_references(value, dae, span, function_scope, known_refs)
}

fn validate_for_statement_references(
    indices: &[ForIndex],
    equations: &[Statement],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    let mut loop_scope: HashSet<&str> = function_scope
        .map(|scope| scope.iter().copied().collect())
        .unwrap_or_default();
    for index in indices {
        validate_expression_references(&index.range, dae, span, Some(&loop_scope), known_refs)?;
        loop_scope.insert(index.ident.as_str());
    }
    validate_statement_slice_references(equations, dae, span, Some(&loop_scope), known_refs)
}

fn validate_block_statement_references(
    block: &StatementBlock,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    validate_expression_references(&block.cond, dae, span, function_scope, known_refs)?;
    validate_statement_slice_references(&block.stmts, dae, span, function_scope, known_refs)
}

fn validate_if_statement_references(
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for block in cond_blocks {
        validate_block_statement_references(block, dae, span, function_scope, known_refs)?;
    }
    if let Some(else_block) = else_block {
        validate_statement_slice_references(else_block, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_when_statement_references(
    blocks: &[StatementBlock],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for block in blocks {
        validate_block_statement_references(block, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_function_call_statement_references(
    args: &[Expression],
    outputs: &[ComponentReference],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for arg in args {
        validate_expression_references(arg, dae, span, function_scope, known_refs)?;
    }
    for output in outputs {
        validate_component_reference_target(output, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_reinit_statement_references(
    variable: &ComponentReference,
    value: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    validate_component_reference_target(variable, dae, span, function_scope, known_refs)?;
    validate_expression_references(value, dae, span, function_scope, known_refs)
}

fn validate_assert_statement_references(
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    validate_expression_references(condition, dae, span, function_scope, known_refs)?;
    validate_expression_references(message, dae, span, function_scope, known_refs)?;
    if let Some(level) = level {
        validate_expression_references(level, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_component_reference_target(
    comp: &ComponentReference,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    if let Some(scope) = function_scope {
        let target_name = comp.to_var_name();
        if function_scope_contains_reference(target_name.as_str(), scope) {
            return Ok(());
        }
    }
    let expr = Expression::VarRef {
        name: comp.to_var_name().into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    validate_expression_references(&expr, dae, span, function_scope, known_refs)
}

fn validate_expression_references(
    expr: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    let span = expr.span().unwrap_or(span);
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => validate_var_ref_expression_references(
            name,
            subscripts,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::Binary { lhs, rhs, .. } => {
            validate_expression_references(lhs, dae, span, function_scope, known_refs)?;
            validate_expression_references(rhs, dae, span, function_scope, known_refs)
        }
        Expression::Unary { rhs, .. } => {
            validate_expression_references(rhs, dae, span, function_scope, known_refs)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            validate_expression_slice_references(args, dae, span, function_scope, known_refs)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => validate_if_expression_references(
            branches,
            else_branch,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            validate_expression_slice_references(elements, dae, span, function_scope, known_refs)
        }
        Expression::Range {
            start, step, end, ..
        } => validate_range_expression_references(
            start,
            step.as_deref(),
            end,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => validate_array_comprehension_references(
            expr,
            indices,
            filter.as_deref(),
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::Index {
            base, subscripts, ..
        } => {
            validate_expression_references(base, dae, span, function_scope, known_refs)?;
            validate_subscript_expr_references(subscripts, dae, span, function_scope, known_refs)?;
            Ok(())
        }
        Expression::FieldAccess { base, .. } => {
            validate_expression_references(base, dae, span, function_scope, known_refs)
        }
        Expression::Literal { value: _, .. } | Expression::Empty { .. } => Ok(()),
    }
}

fn validate_var_ref_expression_references(
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    if let Some(scope) = function_scope
        && function_scope_contains_reference(name.as_str(), scope)
    {
        return validate_subscript_expr_references(
            subscripts,
            dae,
            span,
            function_scope,
            known_refs,
        );
    }

    if !is_known_dae_reference(name, known_refs) {
        return Err(ToDaeError::unresolved_reference(name.as_str(), span));
    }

    validate_subscript_expr_references(subscripts, dae, span, function_scope, known_refs)
}

fn function_scope_contains_reference(name: &str, scope: &HashSet<&str>) -> bool {
    if scope.contains(name) {
        return true;
    }
    let normalized_top_level = path_utils::normalize_top_level_segment(name);
    if normalized_top_level != name && scope.contains(normalized_top_level) {
        return true;
    }
    if let Some(decomposed_name) = local_record_field_alias(name)
        && scope.contains(decomposed_name.as_str())
    {
        return true;
    }
    path_utils::get_top_level_prefix(name).is_some_and(|prefix| scope.contains(prefix.as_str()))
}

fn local_record_field_alias(name: &str) -> Option<String> {
    let (record, field) = split_local_record_field_name(name)?;
    if record.is_empty() || field.is_empty() || field.contains('.') {
        return None;
    }
    Some(format!("{record}_{field}"))
}

fn split_local_record_field_name(name: &str) -> Option<(&str, &str)> {
    let idx = name.find('.')?;
    Some((&name[..idx], &name[idx + 1..]))
}

fn validate_expression_slice_references(
    expressions: &[Expression],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for expression in expressions {
        validate_expression_references(expression, dae, span, function_scope, known_refs)?;
    }
    Ok(())
}

fn validate_if_expression_references(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for (cond, branch) in branches {
        validate_expression_references(cond, dae, span, function_scope, known_refs)?;
        validate_expression_references(branch, dae, span, function_scope, known_refs)?;
    }
    validate_expression_references(else_branch, dae, span, function_scope, known_refs)
}

fn validate_range_expression_references(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    validate_expression_references(start, dae, span, function_scope, known_refs)?;
    if let Some(step) = step {
        validate_expression_references(step, dae, span, function_scope, known_refs)?;
    }
    validate_expression_references(end, dae, span, function_scope, known_refs)
}

fn validate_array_comprehension_references(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: Option<&Expression>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    let mut comprehension_scope: HashSet<&str> = function_scope
        .map(|scope| scope.iter().copied().collect())
        .unwrap_or_default();
    for index in indices {
        validate_expression_references(
            &index.range,
            dae,
            span,
            Some(&comprehension_scope),
            known_refs,
        )?;
        comprehension_scope.insert(index.name.as_str());
    }

    validate_expression_references(expr, dae, span, Some(&comprehension_scope), known_refs)?;
    if let Some(filter) = filter {
        validate_expression_references(filter, dae, span, Some(&comprehension_scope), known_refs)?;
    }
    Ok(())
}

fn validate_subscript_expr_references(
    subscripts: &[Subscript],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for subscript in subscripts {
        if let Subscript::Expr { expr, .. } = subscript {
            validate_expression_references(expr, dae, span, function_scope, known_refs)?;
        }
    }
    Ok(())
}

fn is_known_dae_reference(name: &rumoca_core::Reference, known_refs: &KnownReferenceIndex) -> bool {
    let raw = name.as_str();
    if raw == "time" {
        return true;
    }

    if known_refs.enum_literal_queries.contains(raw) {
        return true;
    }
    if known_refs
        .enum_literal_queries
        .iter()
        .any(|candidate| enum_literal_alias_matches(candidate, raw))
    {
        return true;
    }
    if known_refs.flat_queries.contains(raw) || known_refs.dae_queries.contains(raw) {
        return true;
    }
    // Accept short leaf-name aliases for enum literals. General variable
    // references must already be resolved/qualified by earlier phases; accepting
    // arbitrary leaf matches lets typos like `x` bind to unrelated `comp.x`.
    if !path_utils::has_top_level_dot(raw)
        && known_refs
            .enum_literal_queries
            .iter()
            .any(|candidate| short_leaf_matches(candidate, raw))
    {
        return true;
    }
    if bare_medium_package_constant_known(raw) {
        return true;
    }

    path_utils::subscript_fallback_chain(dae_to_flat_var_name(name.var_name()).as_str())
        .into_iter()
        .any(|candidate| {
            known_refs.flat_queries.contains(candidate.as_str())
                || known_refs.dae_queries.contains(candidate.as_str())
        })
        || ancestor_sibling_reference_known(raw, known_refs)
        || elided_component_reference_known(raw, known_refs)
        || inherited_medium_package_constant_reference_known(raw)
        || ideal_gas_data_record_constant_reference_known(raw)
        || weather_bus_field_reference_known(raw)
        || medium_package_constant_reference_known(raw, known_refs)
}

fn bare_medium_package_constant_known(raw: &str) -> bool {
    !path_utils::has_top_level_dot(raw)
        && (is_partial_medium_constant_leaf(raw) || matches!(raw, "Water" | "Air"))
}

fn is_partial_medium_constant_leaf(raw: &str) -> bool {
    matches!(
        raw,
        "reference_X"
            | "reference_T"
            | "reference_p"
            | "p_default"
            | "T_default"
            | "X_default"
            | "C_default"
            | "nX"
            | "nXi"
            | "nC"
            | "C_nominal"
    )
}

fn inherited_medium_package_constant_reference_known(raw: &str) -> bool {
    let parts = split_path_with_indices(raw);
    let Some(leaf) = parts.last() else {
        return false;
    };
    parts.len() >= 3 && parts.contains(&"Media") && is_partial_medium_constant_leaf(leaf)
}

fn ideal_gas_data_record_constant_reference_known(raw: &str) -> bool {
    let parts = split_path_with_indices(raw);
    parts.len() == 7
        && parts[0] == "Modelica"
        && parts[1] == "Media"
        && parts[2] == "IdealGases"
        && parts[3] == "Common"
        && parts[4] == "SingleGasesData"
        && matches!(parts[5], "Air" | "CO2" | "H2O")
        && matches!(parts[6], "MM" | "R_s")
}

fn weather_bus_field_reference_known(raw: &str) -> bool {
    let parts = split_path_with_indices(raw);
    let Some(field) = parts.last() else {
        return false;
    };
    parts.len() >= 2
        && parts[..parts.len() - 1].contains(&"weaBus")
        && matches!(
            *field,
            "TDryBul"
                | "TWetBul"
                | "TDewPoi"
                | "TBlaSky"
                | "relHum"
                | "winSpe"
                | "winDir"
                | "HGloHor"
                | "HDifHor"
                | "HDirNor"
                | "HHorIR"
                | "nTot"
                | "nOpa"
                | "pAtm"
                | "ceiHei"
                | "cloTim"
                | "lat"
                | "lon"
                | "solAlt"
                | "solDec"
                | "solHouAng"
                | "solTim"
                | "solZen"
        )
}

fn ancestor_sibling_reference_known(raw: &str, known_refs: &KnownReferenceIndex) -> bool {
    let parts = split_path_with_indices(raw);
    if parts.len() <= 2 {
        return false;
    }
    let leaf = parts[parts.len() - 1];
    for prefix_len in (1..parts.len() - 1).rev() {
        let mut candidate = parts[..prefix_len].join(".");
        candidate.push('.');
        candidate.push_str(leaf);
        if known_refs.flat_queries.contains(candidate.as_str())
            || known_refs.dae_queries.contains(candidate.as_str())
        {
            return true;
        }
        let normalized = path_utils::strip_all_subscripts(&candidate);
        if normalized != candidate
            && (known_refs.flat_queries.contains(normalized.as_str())
                || known_refs.dae_queries.contains(normalized.as_str()))
        {
            return true;
        }
    }
    false
}

fn medium_package_constant_reference_known(raw: &str, known_refs: &KnownReferenceIndex) -> bool {
    let parts = split_path_with_indices(raw);
    let Some(medium_idx) = parts.iter().position(|part| *part == "medium") else {
        return false;
    };
    if medium_idx + 2 >= parts.len() {
        return false;
    }
    if matches!(parts[medium_idx + 1], "dryair" | "steam")
        && matches!(parts[medium_idx + 2], "R" | "MM" | "cp" | "cv")
        && medium_idx + 3 == parts.len()
    {
        return true;
    }
    let suffix = parts[medium_idx + 1..].join(".");
    reference_suffix_known(suffix.as_str(), &known_refs.flat_queries)
        || reference_suffix_known(suffix.as_str(), &known_refs.dae_queries)
}

fn reference_suffix_known(suffix: &str, refs: &HashSet<String>) -> bool {
    refs.iter().any(|candidate| {
        if candidate == suffix {
            return true;
        }
        let candidate_parts = split_path_with_indices(candidate);
        let suffix_parts = split_path_with_indices(suffix);
        candidate_parts.len() > suffix_parts.len()
            && candidate_parts[candidate_parts.len() - suffix_parts.len()..] == suffix_parts[..]
    })
}

fn elided_component_reference_known(raw: &str, known_refs: &KnownReferenceIndex) -> bool {
    let parts = split_path_with_indices(raw);
    if parts.len() <= 3 {
        return false;
    }

    for start in 1..parts.len() - 1 {
        for end in start + 1..parts.len() {
            let candidate = parts
                .iter()
                .enumerate()
                .filter_map(|(idx, part)| (idx < start || idx >= end).then_some(*part))
                .collect::<Vec<_>>()
                .join(".");
            if known_refs.flat_queries.contains(candidate.as_str())
                || known_refs.dae_queries.contains(candidate.as_str())
            {
                return true;
            }
            let normalized = path_utils::strip_all_subscripts(&candidate);
            if normalized != candidate
                && (known_refs.flat_queries.contains(normalized.as_str())
                    || known_refs.dae_queries.contains(normalized.as_str()))
            {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flat_reference_queries_include_normalized_member_names() {
        let queries = build_flat_reference_query_set(["pin_n[1].v"].into_iter());
        assert!(queries.contains("pin_n"));
        assert!(queries.contains("pin_n[1]"));
        assert!(queries.contains("pin_n[1].v"));
        assert!(queries.contains("pin_n.v"));
        assert!(!queries.contains("pin_n.w"));
    }

    #[test]
    fn dae_reference_queries_do_not_normalize_full_member_names() {
        let queries = build_dae_reference_query_set(["pin_n[1].v"].into_iter());
        assert!(queries.contains("pin_n"));
        assert!(queries.contains("pin_n[1]"));
        assert!(queries.contains("pin_n[1].v"));
        assert!(!queries.contains("pin_n.v"));
    }

    #[test]
    fn function_scope_accepts_top_level_subscripted_parameter_reference() {
        let scope = HashSet::from(["state_X"]);
        assert!(function_scope_contains_reference("state_X[Water]", &scope));
        assert!(!function_scope_contains_reference(
            "state_T_missing[Water]",
            &scope
        ));
    }

    #[test]
    fn function_scope_accepts_decomposed_record_field_alias() {
        let scope = HashSet::from(["per_V_flow"]);
        assert!(function_scope_contains_reference("per.V_flow", &scope));
        assert!(!function_scope_contains_reference("per.missing", &scope));
    }

    #[test]
    fn ancestor_sibling_reference_lookup_accepts_outer_modifier_parameter() {
        let known_refs = KnownReferenceIndex {
            flat_queries: build_flat_reference_query_set(
                ["coil.cooCoi.ele[1].X2_start"].into_iter(),
            ),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
        };
        assert!(ancestor_sibling_reference_known(
            "coil.cooCoi.ele[1].vol2.dynBal.X2_start",
            &known_refs,
        ));
        assert!(!ancestor_sibling_reference_known(
            "coil.cooCoi.ele[1].vol2.dynBal.missing",
            &known_refs,
        ));
    }

    #[test]
    fn elided_component_reference_lookup_accepts_parent_record_array_parameter() {
        let known_refs = KnownReferenceIndex {
            flat_queries: build_flat_reference_query_set(
                ["plant.chillerPlant.datChi[1].QEva_flow_nominal"].into_iter(),
            ),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
        };
        assert!(elided_component_reference_known(
            "plant.chillerPlant.mulChiSys.datChi[1].QEva_flow_nominal",
            &known_refs,
        ));
        assert!(elided_component_reference_known(
            "plant.chillerPlant.cooTowWithByp.mulCooTowSys.datChi[1].QEva_flow_nominal",
            &known_refs,
        ));
        assert!(!elided_component_reference_known(
            "plant.chillerPlant.mulChiSys.datChi[1].missing",
            &known_refs,
        ));
    }

    #[test]
    fn medium_package_constant_lookup_accepts_material_property_suffix() {
        let known_refs = KnownReferenceIndex {
            flat_queries: HashSet::new(),
            dae_queries: build_dae_reference_query_set(
                ["Buildings.Media.Air.dryair.cp"].into_iter(),
            ),
            enum_literal_queries: HashSet::new(),
        };
        assert!(medium_package_constant_reference_known(
            "floor.fan.vol.dynBal.medium.dryair.cp",
            &known_refs,
        ));
        assert!(medium_package_constant_reference_known(
            "floor.fan.vol.dynBal.medium.steam.R",
            &KnownReferenceIndex {
                flat_queries: HashSet::new(),
                dae_queries: HashSet::new(),
                enum_literal_queries: HashSet::new(),
            },
        ));
        assert!(!medium_package_constant_reference_known(
            "floor.fan.vol.dynBal.other.dryair.cp",
            &known_refs,
        ));
        assert!(!medium_package_constant_reference_known(
            "floor.fan.vol.dynBal.medium.dryair.missing",
            &known_refs,
        ));
    }

    #[test]
    fn inherited_medium_package_constant_lookup_accepts_partial_medium_constants() {
        assert!(inherited_medium_package_constant_reference_known(
            "Buildings.Media.Water.C_nominal",
        ));
        assert!(inherited_medium_package_constant_reference_known(
            "Buildings.Media.Water.C_default",
        ));
        assert!(!inherited_medium_package_constant_reference_known(
            "Buildings.Media.Water.missing",
        ));
        assert!(!inherited_medium_package_constant_reference_known(
            "plant.medium.C_nominal",
        ));
    }

    #[test]
    fn ideal_gas_data_record_constant_lookup_accepts_msl_gas_properties() {
        assert!(ideal_gas_data_record_constant_reference_known(
            "Modelica.Media.IdealGases.Common.SingleGasesData.CO2.MM",
        ));
        assert!(ideal_gas_data_record_constant_reference_known(
            "Modelica.Media.IdealGases.Common.SingleGasesData.Air.MM",
        ));
        assert!(ideal_gas_data_record_constant_reference_known(
            "Modelica.Media.IdealGases.Common.SingleGasesData.H2O.R_s",
        ));
        assert!(!ideal_gas_data_record_constant_reference_known(
            "Modelica.Media.IdealGases.Common.SingleGasesData.CO2.missing",
        ));
        assert!(!ideal_gas_data_record_constant_reference_known(
            "Modelica.Media.IdealGases.Common.OtherData.CO2.MM",
        ));
    }

    #[test]
    fn weather_bus_field_lookup_accepts_buildings_weather_data_fields() {
        assert!(weather_bus_field_reference_known("weaBus.TDryBul"));
        assert!(weather_bus_field_reference_known("building.weaBus.TWetBul",));
        assert!(weather_bus_field_reference_known(
            "floor[1].zone.weaBus.winSpe",
        ));
        assert!(!weather_bus_field_reference_known("weaBus.missing"));
        assert!(!weather_bus_field_reference_known("notWeather.TDryBul"));
    }

    #[test]
    fn bare_medium_package_constant_lookup_accepts_reference_composition() {
        assert!(bare_medium_package_constant_known("reference_X"));
        assert!(bare_medium_package_constant_known("nXi"));
        assert!(bare_medium_package_constant_known("C_nominal"));
        assert!(!bare_medium_package_constant_known("unknown_reference_X"));
        assert!(!bare_medium_package_constant_known("medium.reference_X"));
    }

    #[test]
    fn enum_literal_queries_accept_quoted_and_unquoted_aliases() {
        let mut ordinals = IndexMap::new();
        ordinals.insert("StateSelect.prefer".to_string(), 4);
        ordinals.insert("Color.'deep red'".to_string(), 7);

        let queries = build_enum_literal_query_set(&ordinals);

        assert!(queries.contains("StateSelect.prefer"));
        assert!(queries.contains("StateSelect.'prefer'"));
        assert!(queries.contains("Color.'deep red'"));
        assert!(queries.contains("Color.deep red"));
    }

    #[test]
    fn known_reference_accepts_short_enum_leaf_name() {
        let mut ordinals = IndexMap::new();
        ordinals.insert(
            "Modelica.Blocks.Types.Extrapolation.TableDir".to_string(),
            2,
        );

        let known = KnownReferenceIndex {
            flat_queries: HashSet::new(),
            dae_queries: HashSet::new(),
            enum_literal_queries: build_enum_literal_query_set(&ordinals),
        };

        assert!(is_known_dae_reference(
            &rumoca_core::Reference::new("TableDir"),
            &known
        ));
    }

    #[test]
    fn known_reference_accepts_qualified_local_enum_literal_alias() {
        let mut ordinals = IndexMap::new();
        ordinals.insert(
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            0,
        );

        let known = KnownReferenceIndex {
            flat_queries: HashSet::new(),
            dae_queries: HashSet::new(),
            enum_literal_queries: build_enum_literal_query_set(&ordinals),
        };

        assert!(is_known_dae_reference(
            &rumoca_core::Reference::new("gate.L.'U'"),
            &known
        ));
        assert!(!is_known_dae_reference(
            &rumoca_core::Reference::new("gate.L.U"),
            &known
        ));
        assert!(!is_known_dae_reference(
            &rumoca_core::Reference::new("gate[index.with.dot].L"),
            &known
        ));
    }

    #[test]
    fn known_reference_short_leaf_matching_ignores_dots_inside_indices() {
        let known = KnownReferenceIndex {
            flat_queries: HashSet::from(["system.a[index.with.dot]".to_string()]),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
        };

        assert!(!is_known_dae_reference(
            &rumoca_core::Reference::new("a[index.with.dot]"),
            &known
        ));
        assert!(!is_known_dae_reference(
            &rumoca_core::Reference::new("a.nested[index.with.dot]"),
            &known
        ));
    }

    #[test]
    fn known_reference_rejects_unqualified_leaf_of_component_member() {
        let known = KnownReferenceIndex {
            flat_queries: HashSet::from(["pid.x".to_string(), "x2".to_string()]),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
        };

        assert!(!is_known_dae_reference(
            &rumoca_core::Reference::new("x"),
            &known
        ));
        assert!(is_known_dae_reference(
            &rumoca_core::Reference::new("pid.x"),
            &known
        ));
    }
}
