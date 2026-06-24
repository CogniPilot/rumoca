use super::*;
use indexmap::{IndexMap, IndexSet};
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

struct KnownReferenceIndex {
    flat_queries: IndexSet<String>,
    dae_queries: IndexSet<String>,
    enum_literal_queries: IndexSet<String>,
}

impl KnownReferenceIndex {
    fn build(dae: &Dae, known_flat_var_names: &HashSet<String>) -> Result<Self, ToDaeError> {
        let mut dae_var_collector = DaeVariableNameCollector { names: Vec::new() };
        dae_var_collector.visit_variables(&dae.variables);
        Ok(Self {
            flat_queries: build_flat_reference_query_set(
                known_flat_var_names.iter().map(String::as_str),
            )?,
            dae_queries: build_dae_reference_query_set(
                dae_var_collector.names.iter().map(String::as_str),
            )?,
            enum_literal_queries: build_enum_literal_query_set(&dae.symbols.enum_literal_ordinals),
        })
    }
}

struct DaeVariableNameCollector {
    names: Vec<String>,
}

fn copied_scope<'a>(function_scope: Option<&HashSet<&'a str>>) -> HashSet<&'a str> {
    match function_scope {
        Some(scope) => scope.iter().copied().collect(),
        None => HashSet::new(),
    }
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

fn build_flat_reference_query_set<'a>(
    names: impl Iterator<Item = &'a str>,
) -> Result<IndexSet<String>, ToDaeError> {
    let mut queries = IndexSet::new();
    for name in names {
        insert_reference_query_alias(&mut queries, name);
        insert_reference_query_alias(&mut queries, &path_utils::strip_all_subscripts(name));
        insert_ancestor_reference_queries(&mut queries, name)?;
    }
    Ok(queries)
}

fn build_dae_reference_query_set<'a>(
    names: impl Iterator<Item = &'a str>,
) -> Result<IndexSet<String>, ToDaeError> {
    let mut queries = IndexSet::new();
    for name in names {
        queries.insert(name.to_string());
        insert_ancestor_reference_queries(&mut queries, name)?;
        if !path_utils::is_nested_name(name) && path_utils::has_top_level_subscript(name) {
            insert_reference_query_alias(
                &mut queries,
                path_utils::normalize_top_level_segment(name),
            );
        }
    }
    Ok(queries)
}

pub(crate) fn build_enum_literal_query_set(ordinals: &IndexMap<String, i64>) -> IndexSet<String> {
    ordinals.keys().cloned().collect()
}

fn insert_reference_query_alias(queries: &mut IndexSet<String>, name: &str) {
    if !name.is_empty() {
        queries.insert(name.to_string());
    }
}

fn insert_ancestor_reference_queries(
    queries: &mut IndexSet<String>,
    name: &str,
) -> Result<(), ToDaeError> {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    for idx in 1..path.len() {
        let Some(prefix) = path.prefix(idx) else {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "reference `{name}` could not produce ancestor prefix {idx} from {} path segments",
                path.len()
            )));
        };
        let prefix = prefix.to_flat_string();
        insert_reference_query_alias(queries, &prefix);
        let normalized = path_utils::strip_all_subscripts(&prefix);
        insert_reference_query_alias(queries, &normalized);
    }
    Ok(())
}

fn short_leaf_matches(candidate: &str, short: &str) -> bool {
    path_utils::leaf_segment(candidate) == short
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
        span: call_span,
    } = base
    {
        let span = if call_span.is_dummy() {
            span
        } else {
            *call_span
        };
        for arg in args {
            validate_expression_constructor_selections(arg, functions, span)?;
        }

        let selected_name = format!("{}.{}", name.as_str(), field);
        let Some(constructor) = functions.get(name.var_name()) else {
            if crate::todae_debug_enabled() {
                let mut candidates: Vec<String> =
                    functions.keys().map(|f| f.as_str().to_string()).collect();
                candidates.sort();
                let short = name.last_segment().to_string();
                let mut short_matches: Vec<String> = candidates
                    .iter()
                    .filter(|candidate| short_leaf_matches(candidate, &short))
                    .cloned()
                    .collect();
                short_matches.sort();
                crate::log_todae_debug(format!(
                    "DEBUG TODAE missing constructor selection={} args_len={} short_matches={short_matches:?} total_functions={}",
                    selected_name,
                    args.len(),
                    candidates.len()
                ));
            }
            return Err(ToDaeError::constructor_field_selection_unresolved(
                selected_name,
                span,
            ));
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        let field_resolves_from_positional =
            crate::constructor_field_selection::positional_constructor_arg_for_field(args, field)
                .is_some();
        if !field_known && !field_resolves_from_positional {
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
        name: &VarName,
        variable: &dae::Variable,
    ) {
        if self.result.is_err() {
            return;
        }
        for attribute in variable_attribute_expressions(variable) {
            self.result = variable_attribute_span(name, variable, &attribute).and_then(|span| {
                validate_expression_constructor_selections(attribute.expr, self.functions, span)
            });
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
    let known_refs = KnownReferenceIndex::build(dae, known_flat_var_names)?;
    validate_variable_provenance(dae)?;
    validate_variable_reference_attributes(dae, &known_refs)?;
    validate_equation_rhs_references(dae, &known_refs)?;
    validate_relation_references(dae, &known_refs)?;
    validate_function_references(dae, &known_refs)?;
    Ok(())
}

/// Phase contract: every DAE variable carries a structured component
/// reference. Source variables keep the reference produced by flatten;
/// generated variables (event conditions, `__pre__` parameters) attach a
/// generated reference at their creation site. Downstream passes consume this
/// provenance instead of re-deriving structure from rendered names.
fn validate_variable_provenance(dae: &Dae) -> Result<(), ToDaeError> {
    struct ProvenanceValidator {
        result: Result<(), ToDaeError>,
    }
    impl rumoca_ir_dae::DaeVisitor for ProvenanceValidator {
        fn visit_variable(
            &mut self,
            _partition: rumoca_ir_dae::DaeVariablePartition,
            name: &rumoca_core::VarName,
            variable: &rumoca_ir_dae::Variable,
        ) {
            if self.result.is_ok() && variable.component_ref.is_none() {
                self.result = Err(ToDaeError::runtime_contract_violation_at(
                    format!(
                        "DAE variable `{}` has no structured component reference; its producer must attach one (generated variables included)",
                        name.as_str()
                    ),
                    variable.source_span,
                ));
            }
        }
    }
    let mut validator = ProvenanceValidator { result: Ok(()) };
    validator.visit_variables(&dae.variables);
    validator.result
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
        name: &VarName,
        variable: &dae::Variable,
    ) {
        if self.result.is_err() {
            return;
        }
        for attribute in variable_attribute_expressions(variable) {
            self.result = variable_attribute_span(name, variable, &attribute).and_then(|span| {
                validate_expression_references(
                    attribute.expr,
                    self.dae,
                    span,
                    None,
                    self.known_refs,
                )
            });
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
            relation_validation_span(relation)?,
            None,
            known_refs,
        )?;
    }
    Ok(())
}

fn relation_validation_span(expr: &Expression) -> Result<Span, ToDaeError> {
    expr.span().ok_or_else(|| {
        ToDaeError::runtime_metadata_violation(
            "condition relation expression is missing source provenance",
        )
    })
}

fn variable_attribute_expressions(
    variable: &dae::Variable,
) -> impl Iterator<Item = VariableAttributeExpression<'_>> {
    [
        (
            "start",
            variable.start.as_ref(),
            variable.start_attribute_span(),
        ),
        ("min", variable.min.as_ref(), variable.min_attribute_span()),
        ("max", variable.max.as_ref(), variable.max_attribute_span()),
        (
            "nominal",
            variable.nominal.as_ref(),
            variable.nominal_attribute_span(),
        ),
    ]
    .into_iter()
    .filter_map(|(name, expr, span)| {
        expr.map(|expr| VariableAttributeExpression { name, expr, span })
    })
}

struct VariableAttributeExpression<'a> {
    name: &'static str,
    expr: &'a Expression,
    span: Option<Span>,
}

fn variable_attribute_span(
    variable_name: &VarName,
    variable: &dae::Variable,
    attribute: &VariableAttributeExpression<'_>,
) -> Result<Span, ToDaeError> {
    attribute
        .span
        .or_else(|| attribute.expr.span())
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation_at(
                format!(
                    "variable attribute `{}.{}` expression is missing source provenance",
                    variable_name.as_str(),
                    attribute.name
                ),
                variable.source_span,
            )
        })
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
                )?;
            }
        }

        validate_statement_slice_references(
            &function.body,
            dae,
            function.span,
            Some(&function_scope),
            known_refs,
        )?;
    }
    Ok(())
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
    let mut loop_scope = copied_scope(function_scope);
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
        span: component_reference_target_span(comp, span)?,
    };
    validate_expression_references(&expr, dae, span, function_scope, known_refs)
}

fn component_reference_target_span(
    comp: &ComponentReference,
    context_span: Span,
) -> Result<Span, ToDaeError> {
    (!comp.span.is_dummy())
        .then_some(comp.span)
        .or_else(|| (!context_span.is_dummy()).then_some(context_span))
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "component reference target `{}` is missing source provenance",
                comp.to_var_name().as_str()
            ))
        })
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
    path_utils::get_top_level_prefix(name).is_some_and(|prefix| scope.contains(prefix.as_str()))
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
    let mut comprehension_scope = copied_scope(function_scope);
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
    if known_refs.flat_queries.contains(raw) || known_refs.dae_queries.contains(raw) {
        return true;
    }
    // Accept short leaf-name aliases for enum literals. General variable
    // references must already be resolved/qualified by earlier phases; accepting
    // arbitrary leaf matches lets typos like `x` bind to unrelated `comp.x`.
    if !path_utils::is_nested_name(raw)
        && known_refs
            .enum_literal_queries
            .iter()
            .any(|candidate| short_leaf_matches(candidate, raw))
    {
        return true;
    }

    path_utils::subscript_fallback_chain(dae_to_flat_var_name(name.var_name()).as_str())
        .into_iter()
        .any(|candidate| {
            known_refs.flat_queries.contains(candidate.as_str())
                || known_refs.dae_queries.contains(candidate.as_str())
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flat_reference_queries_include_normalized_member_names() {
        let queries = build_flat_reference_query_set(["pin_n[1].v"].into_iter())
            .unwrap_or_else(|err| panic!("flat reference query set should build: {err}"));
        assert!(queries.contains("pin_n"));
        assert!(queries.contains("pin_n[1]"));
        assert!(queries.contains("pin_n[1].v"));
        assert!(queries.contains("pin_n.v"));
        assert!(!queries.contains("pin_n.w"));
    }

    #[test]
    fn dae_reference_queries_do_not_normalize_full_member_names() {
        let queries = build_dae_reference_query_set(["pin_n[1].v"].into_iter())
            .unwrap_or_else(|err| panic!("DAE reference query set should build: {err}"));
        assert!(queries.contains("pin_n"));
        assert!(queries.contains("pin_n[1]"));
        assert!(queries.contains("pin_n[1].v"));
        assert!(!queries.contains("pin_n.v"));
    }

    #[test]
    fn enum_literal_queries_preserve_flattened_aliases() {
        let mut ordinals = IndexMap::new();
        ordinals.insert("StateSelect.prefer".to_string(), 4);
        ordinals.insert("StateSelect.'prefer'".to_string(), 4);
        ordinals.insert("Color.'deep red'".to_string(), 7);
        ordinals.insert("Color.deep red".to_string(), 7);

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
            flat_queries: IndexSet::new(),
            dae_queries: IndexSet::new(),
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
        ordinals.insert(
            "Modelica.Electrical.Digital.Interfaces.Logic.U".to_string(),
            0,
        );
        ordinals.insert("gate.L.'U'".to_string(), 0);

        let known = KnownReferenceIndex {
            flat_queries: IndexSet::new(),
            dae_queries: IndexSet::new(),
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
            flat_queries: IndexSet::from(["system.a[index.with.dot]".to_string()]),
            dae_queries: IndexSet::new(),
            enum_literal_queries: IndexSet::new(),
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
            flat_queries: IndexSet::from(["pid.x".to_string(), "x2".to_string()]),
            dae_queries: IndexSet::new(),
            enum_literal_queries: IndexSet::new(),
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

    fn component_ref(name: &str, span: Span) -> ComponentReference {
        ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: None,
        }
    }

    fn test_span(offset: usize) -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("reference_validation_test.mo"),
            offset,
            offset + 3,
        )
    }

    #[test]
    fn component_reference_target_span_prefers_component_span() {
        let component_span = test_span(10);
        let context_span = test_span(20);
        let comp = component_ref("x", component_span);

        assert_eq!(
            component_reference_target_span(&comp, context_span)
                .unwrap_or_else(|err| panic!("component span should resolve: {err}")),
            component_span
        );
    }

    #[test]
    fn component_reference_target_span_uses_context_span_when_component_is_unspanned() {
        let context_span = test_span(30);
        let comp = component_ref("x", Span::DUMMY);

        assert_eq!(
            component_reference_target_span(&comp, context_span)
                .unwrap_or_else(|err| panic!("context span should resolve: {err}")),
            context_span
        );
    }

    #[test]
    fn component_reference_target_span_rejects_missing_provenance() {
        let comp = component_ref("x", Span::DUMMY);
        let err = component_reference_target_span(&comp, Span::DUMMY)
            .expect_err("unspanned component reference should fail");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }
}
