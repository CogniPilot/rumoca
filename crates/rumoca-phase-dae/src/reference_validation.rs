use super::*;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;

type ComponentReference = dae::ComponentReference;
type ComprehensionIndex = dae::ComprehensionIndex;
type Dae = dae::Dae;
type Expression = dae::Expression;
type ForIndex = dae::ForIndex;
type Function = dae::Function;
type Statement = dae::Statement;
type StatementBlock = dae::StatementBlock;
type Subscript = dae::Subscript;
type VarName = dae::VarName;

struct KnownReferenceIndex {
    flat_queries: HashSet<String>,
    dae_queries: HashSet<String>,
    enum_literal_queries: HashSet<String>,
    scoped_index_names: HashSet<String>,
}

impl KnownReferenceIndex {
    fn build(dae: &Dae, known_flat_var_names: &HashSet<String>) -> Self {
        Self {
            flat_queries: build_flat_reference_query_set(
                known_flat_var_names.iter().map(String::as_str),
            ),
            dae_queries: build_dae_reference_query_set(
                dae.states
                    .keys()
                    .chain(dae.algebraics.keys())
                    .chain(dae.inputs.keys())
                    .chain(dae.outputs.keys())
                    .chain(dae.parameters.keys())
                    .chain(dae.constants.keys())
                    .chain(dae.discrete_reals.keys())
                    .chain(dae.discrete_valued.keys())
                    .chain(dae.derivative_aliases.keys())
                    .map(VarName::as_str),
            ),
            enum_literal_queries: build_enum_literal_query_set(&dae.enum_literal_ordinals),
            scoped_index_names: collect_scoped_index_names(dae),
        }
    }
}

fn collect_scoped_index_names(dae: &Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for equation in dae
        .f_x
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
        .chain(dae.f_c.iter())
        .chain(dae.initial_equations.iter())
    {
        collect_indices_from_expression(&equation.rhs, &mut names);
    }
    for expr in dae
        .relation
        .iter()
        .chain(dae.synthetic_root_conditions.iter())
        .chain(dae.triggered_clock_conditions.iter())
    {
        collect_indices_from_expression(expr, &mut names);
    }
    for function in dae.functions.values() {
        for stmt in &function.body {
            collect_indices_from_statement(stmt, &mut names);
        }
    }
    names
}

fn collect_indices_from_statement(stmt: &Statement, names: &mut HashSet<String>) {
    match stmt {
        Statement::Assignment { comp, value } => {
            collect_indices_from_component_reference(comp, names);
            collect_indices_from_expression(value, names);
        }
        Statement::For { indices, equations } => {
            for index in indices {
                names.insert(index.ident.to_string());
                collect_indices_from_expression(&index.range, names);
            }
            for nested in equations {
                collect_indices_from_statement(nested, names);
            }
        }
        Statement::While(block) => {
            collect_indices_from_expression(&block.cond, names);
            for nested in &block.stmts {
                collect_indices_from_statement(nested, names);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_indices_from_expression(&block.cond, names);
                for nested in &block.stmts {
                    collect_indices_from_statement(nested, names);
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    collect_indices_from_statement(nested, names);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                collect_indices_from_expression(&block.cond, names);
                for nested in &block.stmts {
                    collect_indices_from_statement(nested, names);
                }
            }
        }
        Statement::FunctionCall { comp, args, outputs } => {
            collect_indices_from_component_reference(comp, names);
            for arg in args {
                collect_indices_from_expression(arg, names);
            }
            for output in outputs {
                collect_indices_from_expression(output, names);
            }
        }
        Statement::Reinit { variable, value } => {
            collect_indices_from_component_reference(variable, names);
            collect_indices_from_expression(value, names);
        }
        Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_indices_from_expression(condition, names);
            collect_indices_from_expression(message, names);
            if let Some(level) = level {
                collect_indices_from_expression(level, names);
            }
        }
        Statement::Return | Statement::Break | Statement::Empty => {}
    }
}

fn collect_indices_from_expression(expr: &Expression, names: &mut HashSet<String>) {
    match expr {
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
        Expression::Binary { lhs, rhs, .. } => {
            collect_indices_from_expression(lhs, names);
            collect_indices_from_expression(rhs, names);
        }
        Expression::Unary { rhs, .. } => collect_indices_from_expression(rhs, names),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_indices_from_expression(arg, names);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_indices_from_expression(cond, names);
                collect_indices_from_expression(value, names);
            }
            collect_indices_from_expression(else_branch, names);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_indices_from_expression(element, names);
            }
        }
        Expression::Range { start, step, end } => {
            collect_indices_from_expression(start, names);
            if let Some(step) = step {
                collect_indices_from_expression(step, names);
            }
            collect_indices_from_expression(end, names);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                names.insert(index.name.to_string());
                collect_indices_from_expression(&index.range, names);
            }
            collect_indices_from_expression(expr, names);
            if let Some(filter) = filter {
                collect_indices_from_expression(filter, names);
            }
        }
        Expression::Index { base, subscripts } => {
            collect_indices_from_expression(base, names);
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    collect_indices_from_expression(expr, names);
                }
            }
        }
        Expression::FieldAccess { base, .. } => collect_indices_from_expression(base, names),
    }
}

fn collect_indices_from_component_reference(
    comp: &ComponentReference,
    names: &mut HashSet<String>,
) {
    for part in &comp.parts {
        for subscript in &part.subs {
            if let Subscript::Expr(expr) = subscript {
                collect_indices_from_expression(expr, names);
            }
        }
    }
}

fn reference_query_matches(known_refs: &KnownReferenceIndex, candidate: &str) -> bool {
    known_refs.flat_queries.contains(candidate)
        || known_refs.dae_queries.contains(candidate)
        || enum_literal_query_matches(known_refs, candidate)
}

fn enum_literal_query_matches(known_refs: &KnownReferenceIndex, candidate: &str) -> bool {
    if known_refs.enum_literal_queries.contains(candidate) {
        return true;
    }
    if !candidate.contains('.') {
        return false;
    }

    let suffix = format!(".{candidate}");
    known_refs
        .enum_literal_queries
        .iter()
        .any(|known| known.ends_with(&suffix))
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
        if !path_utils::has_top_level_dot(name) && name.contains('[') {
            insert_reference_query_alias(
                &mut queries,
                path_utils::normalize_top_level_segment(name),
            );
        }
    }
    queries
}

fn build_enum_literal_query_set(ordinals: &IndexMap<String, i64>) -> HashSet<String> {
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
    let (prefix, literal) = name.rsplit_once('.')?;
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
    let parts = path_utils::split_path_with_indices(name);
    if parts.len() <= 1 {
        return;
    }

    let mut prefix = String::new();
    for part in parts.iter().take(parts.len() - 1) {
        if !prefix.is_empty() {
            prefix.push('.');
        }
        prefix.push_str(part);
        insert_reference_query_alias(queries, &prefix);
        let normalized = path_utils::strip_all_subscripts(&prefix);
        insert_reference_query_alias(queries, &normalized);
    }
}

fn short_leaf_matches(candidate: &str, short: &str) -> bool {
    candidate
        .rsplit('.')
        .next()
        .is_some_and(|leaf| leaf == short)
}

fn validate_constructor_field_projection(
    base: &Expression,
    field: &str,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
    } = base
    {
        for arg in args {
            validate_expression_constructor_projections(arg, functions, span)?;
        }

        if matches!(field, "re" | "im") {
            return Ok(());
        }

        let projected_name = format!("{}.{}", name.as_str(), field);
        let Some(constructor) = resolve_constructor_function(name, functions) else {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
                let mut candidates: Vec<String> =
                    functions.keys().map(|f| f.as_str().to_string()).collect();
                candidates.sort();
                let short = name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .unwrap_or(name.as_str())
                    .to_string();
                let mut short_matches: Vec<String> = candidates
                    .iter()
                    .filter(|candidate| short_leaf_matches(candidate, &short))
                    .cloned()
                    .collect();
                short_matches.sort();
                eprintln!(
                    "DEBUG TODAE missing constructor projection={} args_len={} short_matches={short_matches:?} total_functions={}",
                    projected_name,
                    args.len(),
                    candidates.len()
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        if !field_known {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
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
                eprintln!(
                    "DEBUG TODAE constructor field missing projection={} available={available_fields:?}",
                    projected_name
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        }
        return Ok(());
    }

    validate_expression_constructor_projections(base, functions, span)
}

fn resolve_constructor_function<'a>(
    name: &VarName,
    functions: &'a IndexMap<VarName, Function>,
) -> Option<&'a Function> {
    if let Some(function) = functions.get(name) {
        return Some(function);
    }

    // MLS §5.3/§7.3: short or partially qualified type names may be visible
    // in local scope; if ToDae sees such a name here, recover by unique suffix.
    let mut matches = functions
        .iter()
        .filter(|(candidate, _)| {
            let text = candidate.as_str();
            text == name.as_str() || text.ends_with(&format!(".{}", name.as_str()))
        })
        .map(|(_, function)| function);

    let first = matches.next()?;
    if matches.next().is_some() {
        return None;
    }
    Some(first)
}

fn validate_expression_constructor_projections(
    expr: &Expression,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    match expr {
        Expression::FunctionCall { args, .. } | Expression::BuiltinCall { args, .. } => {
            for arg in args {
                validate_expression_constructor_projections(arg, functions, span)?;
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            validate_expression_constructor_projections(lhs, functions, span)?;
            validate_expression_constructor_projections(rhs, functions, span)?;
        }
        Expression::Unary { rhs, .. } => {
            validate_expression_constructor_projections(rhs, functions, span)?;
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                validate_expression_constructor_projections(cond, functions, span)?;
                validate_expression_constructor_projections(value, functions, span)?;
            }
            validate_expression_constructor_projections(else_branch, functions, span)?;
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                validate_expression_constructor_projections(element, functions, span)?;
            }
        }
        Expression::Range { start, step, end } => {
            validate_expression_constructor_projections(start, functions, span)?;
            if let Some(step) = step {
                validate_expression_constructor_projections(step, functions, span)?;
            }
            validate_expression_constructor_projections(end, functions, span)?;
        }
        Expression::Index { base, subscripts } => {
            validate_expression_constructor_projections(base, functions, span)?;
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    validate_expression_constructor_projections(expr, functions, span)?;
                }
            }
        }
        Expression::FieldAccess { base, field } => {
            validate_constructor_field_projection(base, field, functions, span)?;
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            validate_expression_constructor_projections(expr, functions, span)?;
            for index in indices {
                validate_expression_constructor_projections(&index.range, functions, span)?;
            }
            if let Some(filter_expr) = filter {
                validate_expression_constructor_projections(filter_expr, functions, span)?;
            }
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
    Ok(())
}

fn validate_statement_constructor_projections(
    stmt: &Statement,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Assignment { value, .. } | Statement::Reinit { value, .. } => {
            validate_expression_constructor_projections(value, functions, span)?
        }
        Statement::For { indices, equations } => {
            for index in indices {
                validate_expression_constructor_projections(&index.range, functions, span)?;
            }
            for nested in equations {
                validate_statement_constructor_projections(nested, functions, span)?;
            }
        }
        Statement::While(block) => {
            validate_expression_constructor_projections(&block.cond, functions, span)?;
            for nested in &block.stmts {
                validate_statement_constructor_projections(nested, functions, span)?;
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                validate_expression_constructor_projections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                validate_expression_constructor_projections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
        }
        Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                validate_expression_constructor_projections(arg, functions, span)?;
            }
            for output in outputs {
                validate_expression_constructor_projections(output, functions, span)?;
            }
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            validate_expression_constructor_projections(condition, functions, span)?;
            validate_expression_constructor_projections(message, functions, span)?;
            if let Some(level) = level {
                validate_expression_constructor_projections(level, functions, span)?;
            }
        }
        Statement::Empty | Statement::Return | Statement::Break => {}
    }
    Ok(())
}

pub(super) fn validate_dae_constructor_field_projections(dae: &Dae) -> Result<(), ToDaeError> {
    for variable in dae
        .states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.inputs.values())
        .chain(dae.outputs.values())
        .chain(dae.parameters.values())
        .chain(dae.constants.values())
        .chain(dae.discrete_reals.values())
        .chain(dae.discrete_valued.values())
        .chain(dae.derivative_aliases.values())
    {
        for expr in [
            variable.start.as_ref(),
            variable.min.as_ref(),
            variable.max.as_ref(),
            variable.nominal.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            validate_expression_constructor_projections(expr, &dae.functions, Span::DUMMY)?;
        }
    }

    for equation in dae
        .f_x
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
        .chain(dae.f_c.iter())
        .chain(dae.initial_equations.iter())
    {
        validate_expression_constructor_projections(&equation.rhs, &dae.functions, equation.span)?;
    }

    for function in dae.functions.values() {
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_expression_constructor_projections(
                    default_expr,
                    &dae.functions,
                    function.span,
                )?;
            }
        }
        for statement in &function.body {
            validate_statement_constructor_projections(statement, &dae.functions, function.span)?;
        }
    }

    Ok(())
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
    for variable in dae
        .states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.inputs.values())
        .chain(dae.outputs.values())
        .chain(dae.parameters.values())
        .chain(dae.constants.values())
        .chain(dae.discrete_reals.values())
        .chain(dae.discrete_valued.values())
        .chain(dae.derivative_aliases.values())
    {
        for expr in [
            variable.start.as_ref(),
            variable.min.as_ref(),
            variable.max.as_ref(),
            variable.nominal.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            validate_expression_references(expr, dae, Span::DUMMY, None, known_refs)?;
        }
    }
    Ok(())
}

fn validate_equation_rhs_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for equation in dae
        .f_x
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
        .chain(dae.f_c.iter())
        .chain(dae.initial_equations.iter())
    {
        validate_expression_references(&equation.rhs, dae, equation.span, None, known_refs)?;
    }
    Ok(())
}

fn validate_relation_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for relation in &dae.relation {
        validate_expression_references(relation, dae, Span::DUMMY, None, known_refs)?;
    }
    Ok(())
}

fn validate_function_references(
    dae: &Dae,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for function in dae.functions.values() {
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
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Empty | Statement::Return | Statement::Break => Ok(()),
        Statement::Assignment { comp, value } => validate_assignment_statement_references(
            comp,
            value,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Statement::For { indices, equations } => validate_for_statement_references(
            indices,
            equations,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Statement::While(block) => {
            validate_block_statement_references(block, dae, span, function_scope, known_refs)
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => validate_if_statement_references(
            cond_blocks,
            else_block.as_deref(),
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Statement::When(blocks) => {
            validate_when_statement_references(blocks, dae, span, function_scope, known_refs)
        }
        Statement::FunctionCall { args, outputs, .. } => {
            validate_function_call_statement_references(
                args,
                outputs,
                dae,
                span,
                function_scope,
                known_refs,
            )
        }
        Statement::Reinit { variable, value } => validate_reinit_statement_references(
            variable,
            value,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Statement::Assert {
            condition,
            message,
            level,
        } => validate_assert_statement_references(
            condition,
            message,
            level.as_ref(),
            dae,
            span,
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
    outputs: &[Expression],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    for arg in args {
        validate_expression_references(arg, dae, span, function_scope, known_refs)?;
    }
    for output in outputs {
        validate_expression_references(output, dae, span, function_scope, known_refs)?;
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
        if function_scope_contains_reference(&target_name, scope) {
            return Ok(());
        }
    }
    let expr = Expression::VarRef {
        name: comp.to_var_name(),
        subscripts: vec![],
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
    match expr {
        Expression::VarRef { name, subscripts } => validate_var_ref_expression_references(
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
        } => validate_if_expression_references(
            branches,
            else_branch,
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            validate_expression_slice_references(elements, dae, span, function_scope, known_refs)
        }
        Expression::Range { start, step, end } => validate_range_expression_references(
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
        } => validate_array_comprehension_references(
            expr,
            indices,
            filter.as_deref(),
            dae,
            span,
            function_scope,
            known_refs,
        ),
        Expression::Index { base, subscripts } => {
            validate_expression_references(base, dae, span, function_scope, known_refs)?;
            validate_subscript_expr_references(subscripts, dae, span, function_scope, known_refs)?;
            Ok(())
        }
        Expression::FieldAccess { base, .. } => {
            validate_expression_references(base, dae, span, function_scope, known_refs)
        }
        Expression::Literal(_) | Expression::Empty => Ok(()),
    }
}

fn validate_var_ref_expression_references(
    name: &VarName,
    subscripts: &[Subscript],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_refs: &KnownReferenceIndex,
) -> Result<(), ToDaeError> {
    if let Some(scope) = function_scope
        && function_scope_contains_reference(name, scope)
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
        if is_scoped_index_reference(name, known_refs) {
            return validate_subscript_expr_references(
                subscripts,
                dae,
                span,
                function_scope,
                known_refs,
            );
        }
        return Err(ToDaeError::unresolved_reference(name.as_str(), span));
    }

    validate_subscript_expr_references(subscripts, dae, span, function_scope, known_refs)
}

fn is_scoped_index_reference(name: &VarName, known_refs: &KnownReferenceIndex) -> bool {
    let Some(leaf) = name.as_str().rsplit('.').next() else {
        return false;
    };
    name.as_str().contains('.') && known_refs.scoped_index_names.contains(leaf)
}

fn function_scope_contains_reference(name: &VarName, scope: &HashSet<&str>) -> bool {
    if scope.contains(name.as_str()) {
        return true;
    }
    if name
        .as_str()
        .rsplit('.')
        .next()
        .is_some_and(|leaf| scope.contains(leaf))
    {
        return true;
    }
    path_utils::get_top_level_prefix(name.as_str())
        .is_some_and(|prefix| scope.contains(prefix.as_str()))
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
        if let Subscript::Expr(expr) = subscript {
            validate_expression_references(expr, dae, span, function_scope, known_refs)?;
        }
    }
    Ok(())
}

fn is_known_dae_reference(name: &VarName, known_refs: &KnownReferenceIndex) -> bool {
    let raw = name.as_str();
    if raw == "time" {
        return true;
    }

    if enum_literal_query_matches(known_refs, raw) {
        return true;
    }
    if known_refs.flat_queries.contains(raw) || known_refs.dae_queries.contains(raw) {
        return true;
    }
    // Accept short leaf-name aliases for enum literals and known references.
    // Libraries often use imported aliases and unqualified enum names (e.g. `TableDir`).
    if !raw.contains('.') {
        if known_refs
            .enum_literal_queries
            .iter()
            .any(|candidate| short_leaf_matches(candidate, raw))
        {
            return true;
        }
        if known_refs
            .flat_queries
            .iter()
            .any(|candidate| short_leaf_matches(candidate, raw))
            || known_refs
                .dae_queries
                .iter()
                .any(|candidate| short_leaf_matches(candidate, raw))
        {
            return true;
        }
    }

    let known_via_subscript = path_utils::subscript_fallback_chain(&dae_to_flat_var_name(name))
        .into_iter()
        .any(|candidate| {
            known_refs.flat_queries.contains(candidate.as_str())
                || known_refs.dae_queries.contains(candidate.as_str())
        });
    let known_via_outer_scope =
        resolves_via_outer_scope_component_modifier_fallback(raw, known_refs);
    let known_via_leading_scope =
        resolves_via_leading_component_scope_fallback(raw, known_refs);
    let known_via_member_value_scope =
        resolves_via_component_member_value_fallback(raw, known_refs);
    let known_via_nested_member_value_scope =
        resolves_via_component_nested_member_value_fallback(raw, known_refs);

    known_via_subscript
        || known_via_outer_scope
        || known_via_leading_scope
        || known_via_member_value_scope
        || known_via_nested_member_value_scope
}

fn resolves_via_outer_scope_component_modifier_fallback(
    raw: &str,
    known_refs: &KnownReferenceIndex,
) -> bool {
    let segments = path_utils::split_path_with_indices(raw);
    if segments.len() < 3 {
        return false;
    }

    // MLS §7.2/§7.3: names used in component modifiers are looked up in the
    // enclosing lexical scope. If an earlier phase over-qualifies that name
    // with the modified component segment (e.g. `a.b.k` vs `a.k`), allow
    // fallback to the enclosing scope spelling.
    let mut collapsed = Vec::with_capacity(segments.len() - 1);
    collapsed.extend(segments.iter().take(segments.len() - 2).copied());
    collapsed.push(segments[segments.len() - 1]);
    let candidate = collapsed.join(".");

    known_refs.flat_queries.contains(candidate.as_str())
        || known_refs.dae_queries.contains(candidate.as_str())
}

fn resolves_via_leading_component_scope_fallback(raw: &str, known_refs: &KnownReferenceIndex) -> bool {
    let segments = path_utils::split_path_with_indices(raw);
    if segments.len() < 2 {
        return false;
    }

    // Some expressions are over-qualified with a leading instance segment
    // (`inst.Type.EnumLiteral`). Try dropping that leading segment only.
    let candidate = segments[1..].join(".");
    reference_query_matches(known_refs, &candidate)
}

fn resolves_via_component_member_value_fallback(raw: &str, known_refs: &KnownReferenceIndex) -> bool {
    let segments = path_utils::split_path_with_indices(raw);
    if segments.len() != 3 {
        return false;
    }

    // Guarded fallback for over-qualified modifier values like
    // `component.member.valueSymbol`, where `valueSymbol` belongs to the
    // enclosing scope (MLS §7.2/§7.3 name lookup in modifications).
    if !segments[2].starts_with(segments[1]) {
        return false;
    }
    if reference_query_matches(known_refs, segments[2]) {
        return true;
    }

    let suffix = format!(".{}", segments[2]);
    known_refs
        .flat_queries
        .iter()
        .chain(known_refs.dae_queries.iter())
        .any(|known| known.ends_with(&suffix))
}

fn resolves_via_component_nested_member_value_fallback(
    raw: &str,
    known_refs: &KnownReferenceIndex,
) -> bool {
    let segments = path_utils::split_path_with_indices(raw);
    if segments.len() < 4 {
        return false;
    }

    // Guarded fallback for nested over-qualified modifier values like
    // `inst.scope.arr[i].scope`, where the trailing scope symbol refers to
    // an enclosing declaration rather than the indexed member chain.
    let scope_name = segments[1];
    let trailing = segments[segments.len() - 1];
    if trailing != scope_name && !trailing.starts_with(scope_name) {
        return false;
    }

    if reference_query_matches(known_refs, trailing) {
        return true;
    }
    let suffix = format!(".{trailing}");
    known_refs
        .flat_queries
        .iter()
        .chain(known_refs.dae_queries.iter())
        .any(|known| known.ends_with(&suffix))
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
            scoped_index_names: HashSet::new(),
        };

        assert!(is_known_dae_reference(&VarName::from("TableDir"), &known));
    }

    #[test]
    fn known_reference_accepts_outer_scope_modifier_fallback() {
        let known_refs = KnownReferenceIndex {
            flat_queries: HashSet::from([String::from("rectifier.RonThyristor")]),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
            scoped_index_names: HashSet::new(),
        };

        assert!(is_known_dae_reference(
            &VarName::new("rectifier.thyristor_p.RonThyristor"),
            &known_refs
        ));
    }

    #[test]
    fn known_reference_accepts_leading_component_scope_fallback() {
        let known_refs = KnownReferenceIndex {
            flat_queries: HashSet::new(),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::from([String::from(
                "Modelica.Electrical.PowerConverters.Types.PWMType.SVPWM",
            )]),
            scoped_index_names: HashSet::new(),
        };

        assert!(is_known_dae_reference(
            &VarName::new("pwm.PowerConverters.Types.PWMType.SVPWM"),
            &known_refs
        ));
    }

    #[test]
    fn resolve_constructor_function_accepts_unique_suffix_match() {
        let name = VarName::new("Machines.Losses.BrushParameters");
        let full = VarName::new("Modelica.Electrical.Machines.Losses.BrushParameters");
        let mut functions = IndexMap::new();
        functions.insert(full.clone(), Function::new(full.as_str(), Span::DUMMY));

        let resolved = resolve_constructor_function(&name, &functions);
        assert!(resolved.is_some());
    }

    #[test]
    fn known_reference_accepts_component_member_value_fallback() {
        let known_refs = KnownReferenceIndex {
            flat_queries: HashSet::from([String::from("cellData2")]),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
            scoped_index_names: HashSet::new(),
        };

        assert!(is_known_dae_reference(
            &VarName::new("battery2.cellData.cellData2"),
            &known_refs
        ));
    }

    #[test]
    fn known_reference_accepts_component_nested_member_value_fallback() {
        let known_refs = KnownReferenceIndex {
            flat_queries: HashSet::from([String::from("stackData")]),
            dae_queries: HashSet::new(),
            enum_literal_queries: HashSet::new(),
            scoped_index_names: HashSet::new(),
        };

        assert!(is_known_dae_reference(
            &VarName::new("stack.stackData.cellData[1,1].stackData"),
            &known_refs
        ));
    }

    #[test]
    fn function_scope_contains_reference_accepts_overqualified_leaf() {
        let mut scope = HashSet::new();
        scope.insert("k");
        assert!(function_scope_contains_reference(
            &VarName::new("symmetricalComponents_1.k"),
            &scope
        ));
    }
}
