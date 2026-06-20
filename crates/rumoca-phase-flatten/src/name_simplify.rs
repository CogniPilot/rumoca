use indexmap::IndexMap;
use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

/// Optional MLS §5.6-compatible shortening for export-oriented flat models.
///
/// Use the shortest readable suffix that stays unique, adding parent hierarchy
/// only when needed. Connector/overconstrained paths remain qualified because
/// later balance logic still needs their public interface grouping.
pub(crate) fn simplify_flat_names(
    flat: &mut flat::Model,
) -> Result<(), crate::errors::FlattenError> {
    let mut working = flat.clone();
    simplify_flat_names_in_place(&mut working)?;
    *flat = working;
    Ok(())
}

fn simplify_flat_names_in_place(flat: &mut flat::Model) -> Result<(), crate::errors::FlattenError> {
    let protected_prefixes = protected_semantic_prefixes(flat);
    let rename_map = build_rename_map(flat, &protected_prefixes);
    let ctx = RenameContext {
        projection_map: build_projection_map(flat, &rename_map)?,
        rename_map: &rename_map,
    };
    if rename_map
        .iter()
        .all(|(old_name, new_name)| old_name == new_name)
        && ctx.projection_map.is_empty()
    {
        return Ok(());
    }

    remap_variables(flat, &ctx)?;
    remap_equations(&mut flat.equations, &ctx)?;
    remap_equations(&mut flat.initial_equations, &ctx)?;
    remap_assert_equations(&mut flat.assert_equations, &ctx)?;
    remap_assert_equations(&mut flat.initial_assert_equations, &ctx)?;
    remap_algorithms(&mut flat.algorithms, &ctx)?;
    remap_algorithms(&mut flat.initial_algorithms, &ctx)?;
    remap_when_clauses(&mut flat.when_clauses, &ctx)?;
    Ok(())
}

struct RenameContext<'a> {
    rename_map: &'a HashMap<String, String>,
    projection_map: HashMap<String, Vec<ProjectionElement>>,
}

struct ProjectionElement {
    name: String,
    span: rumoca_core::Span,
}

fn protected_semantic_prefixes(flat: &flat::Model) -> HashSet<String> {
    flat.top_level_connectors
        .iter()
        .chain(flat.top_level_input_components.iter())
        .cloned()
        .collect()
}

fn build_rename_map(
    flat: &flat::Model,
    protected_prefixes: &HashSet<String>,
) -> HashMap<String, String> {
    let mut requests = Vec::new();
    let mut used = HashSet::new();
    let mut out = HashMap::new();

    for (name, var) in &flat.variables {
        let old_name = name.as_str().to_string();
        if should_preserve_name(&old_name, var, protected_prefixes) {
            used.insert(old_name.clone());
            out.insert(old_name.clone(), old_name);
        } else {
            requests.push((old_name.clone(), name_candidates(&old_name)));
        }
    }

    let candidate_counts = candidate_counts(&requests);
    for (old_name, candidates) in requests {
        let new_name = allocate_name(&mut used, &candidates, &candidate_counts);
        out.insert(old_name, new_name);
    }
    out
}

fn build_projection_map(
    flat: &flat::Model,
    rename_map: &HashMap<String, String>,
) -> Result<HashMap<String, Vec<ProjectionElement>>, crate::errors::FlattenError> {
    let mut groups: HashMap<String, Vec<(Vec<i64>, String, rumoca_core::Span)>> = HashMap::new();

    for (name, variable) in &flat.variables {
        let Some((projection, indices)) = projection_key(name.as_str()) else {
            continue;
        };
        if indices.len() != 1 || projection == name.as_str() {
            continue;
        }
        groups.entry(projection).or_default().push((
            indices,
            remap_name_string(name.as_str(), rename_map),
            variable.source_span,
        ));
    }

    let mut projection_map = HashMap::new();
    for (projection, mut entries) in groups {
        if entries.len() <= 1 || has_duplicate_projection_index(&entries) {
            continue;
        }
        entries.sort_by(|(lhs, _, _), (rhs, _, _)| lhs.cmp(rhs));
        let mut elements = Vec::with_capacity(entries.len());
        for (_, name, span) in entries {
            if span.is_dummy() {
                return Err(crate::errors::FlattenError::missing_source_context(
                    format!(
                        "cannot build projection `{projection}` from `{name}` without a variable source span"
                    ),
                ));
            }
            elements.push(ProjectionElement { name, span });
        }
        projection_map.insert(projection, elements);
    }
    Ok(projection_map)
}

fn has_duplicate_projection_index(entries: &[(Vec<i64>, String, rumoca_core::Span)]) -> bool {
    let mut seen = HashSet::new();
    entries.iter().any(|(indices, _, _)| !seen.insert(indices))
}

fn projection_expression(
    projection: &str,
    entries: &[ProjectionElement],
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, crate::errors::FlattenError> {
    if owner_span.is_dummy() {
        return Err(crate::errors::FlattenError::missing_source_context(
            format!(
                "cannot replace projection reference `{projection}` without the reference source span"
            ),
        ));
    }
    Ok(rumoca_core::Expression::Array {
        elements: entries
            .iter()
            .map(|element| rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(element.name.clone()),
                subscripts: Vec::new(),
                span: element.span,
            })
            .collect(),
        is_matrix: false,
        span: owner_span,
    })
}

fn projection_key(name: &str) -> Option<(String, Vec<i64>)> {
    let mut projection = String::with_capacity(name.len());
    let mut indices = Vec::new();
    let mut depth = 0i32;
    let mut group_start = None;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => {
                if depth == 0 {
                    group_start = Some(idx + 1);
                }
                depth += 1;
            }
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    let raw_index = name[group_start?..idx].trim();
                    indices.push(raw_index.parse::<i64>().ok()?);
                    group_start = None;
                }
            }
            _ if depth == 0 => projection.push(ch),
            _ => {}
        }
    }

    if depth != 0 || indices.is_empty() || projection.is_empty() {
        return None;
    }
    Some((projection, indices))
}

fn should_preserve_name(
    name: &str,
    var: &flat::Variable,
    protected_prefixes: &HashSet<String>,
) -> bool {
    // MLS §7.2.4: modification bindings are interpreted in the lexical
    // modifier scope. Keep those paths stable so projected record-field
    // bindings remain inspectable by their instantiated component paths.
    if var.binding_from_modification {
        return true;
    }

    matches!(var.causality, rumoca_core::Causality::Input(_))
        || var.oc_record_path.is_some()
        || protected_prefixes
            .iter()
            .any(|prefix| is_at_or_below_prefix(name, prefix))
}

fn is_at_or_below_prefix(name: &str, prefix: &str) -> bool {
    name == prefix
        || name
            .strip_prefix(prefix)
            .is_some_and(|tail| tail.starts_with('.') || tail.starts_with('['))
}

fn candidate_counts(requests: &[(String, Vec<String>)]) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    for (_, candidates) in requests {
        let mut seen = HashSet::new();
        for candidate in candidates {
            if seen.insert(candidate) {
                *counts.entry(candidate.clone()).or_insert(0) += 1;
            }
        }
    }
    counts
}

fn allocate_name(
    used: &mut HashSet<String>,
    candidates: &[String],
    candidate_counts: &HashMap<String, usize>,
) -> String {
    for candidate in candidates {
        if candidate_counts.get(candidate).copied().unwrap_or(0) <= 1
            && used.insert(candidate.clone())
        {
            return candidate.clone();
        }
    }

    for candidate in candidates {
        if used.insert(candidate.clone()) {
            return candidate.clone();
        }
    }

    let base = candidates
        .last()
        .cloned()
        .unwrap_or_else(|| "value".to_string());
    for suffix in 2.. {
        let candidate = format!("{base}_{suffix}");
        if used.insert(candidate.clone()) {
            return candidate;
        }
    }
    unreachable!("exhausted usize suffix range while allocating a unique simplified name")
}

fn name_candidates(name: &str) -> Vec<String> {
    let segments = split_modelica_path(name);
    let mut candidates = Vec::new();
    for start in (0..segments.len()).rev() {
        let candidate = segments[start..]
            .iter()
            .map(|segment| readable_identifier_segment(segment))
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
            .join("_");
        if !candidate.is_empty() {
            candidates.push(candidate);
        }
    }
    if candidates.is_empty() {
        candidates.push("value".to_string());
    }
    candidates.dedup();
    candidates
}

fn split_modelica_path(name: &str) -> Vec<&str> {
    let mut segments = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            '.' if depth == 0 => {
                segments.push(&name[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
    }
    segments.push(&name[start..]);
    segments
}

fn readable_identifier_segment(segment: &str) -> String {
    let mut out = String::with_capacity(segment.len());
    let mut last_was_underscore = false;
    for ch in segment.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            if out.is_empty() && ch.is_ascii_digit() {
                out.push('_');
            }
            out.push(ch);
            last_was_underscore = ch == '_';
        } else if !last_was_underscore {
            out.push('_');
            last_was_underscore = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    out
}

fn remap_variables(
    flat: &mut flat::Model,
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    let variable_count = flat.variables.len();
    let remapped_variables: flat::VarNameIndexMap<_> = flat
        .variables
        .iter()
        .map(
            |(old_name, var)| -> Result<_, crate::errors::FlattenError> {
                let mut var = var.clone();
                let new_name = remap_var_name(old_name, ctx.rename_map);
                var.name = new_name.clone();
                remap_variable(&mut var, ctx)?;
                Ok((new_name, var))
            },
        )
        .collect::<Result<_, _>>()?;
    // Downstream phases treat flat names as globally unique (semantic VarRef
    // equality compares names only), so a rename collision that collapses two
    // variables into one map entry must fail here, not corrupt the model.
    if remapped_variables.len() != variable_count {
        return Err(crate::errors::FlattenError::Internal(format!(
            "name simplification collapsed {} variables into {}: rename map produced \
             duplicate flat names",
            variable_count,
            remapped_variables.len()
        )));
    }
    flat.variables = remapped_variables;
    flat.variable_type_names = remap_index_map_keys(
        std::mem::take(&mut flat.variable_type_names),
        ctx.rename_map,
    );
    flat.variable_final_flags = remap_index_map_keys(
        std::mem::take(&mut flat.variable_final_flags),
        ctx.rename_map,
    );
    Ok(())
}

fn remap_index_map_keys<T, S>(
    values: IndexMap<rumoca_core::VarName, T, S>,
    rename_map: &HashMap<String, String>,
) -> IndexMap<rumoca_core::VarName, T, S>
where
    S: Default + std::hash::BuildHasher,
{
    values
        .into_iter()
        .map(|(name, value)| (remap_var_name(&name, rename_map), value))
        .collect()
}

fn remap_variable(
    var: &mut flat::Variable,
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    remap_optional_expression(&mut var.start, ctx)?;
    remap_optional_expression(&mut var.min, ctx)?;
    remap_optional_expression(&mut var.max, ctx)?;
    remap_optional_expression(&mut var.nominal, ctx)?;
    remap_optional_expression(&mut var.binding, ctx)?;
    Ok(())
}

fn remap_equations(
    equations: &mut [flat::Equation],
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    for equation in equations {
        remap_expression(&mut equation.residual, ctx)?;
        remap_equation_origin(&mut equation.origin, ctx.rename_map);
    }
    Ok(())
}

fn remap_assert_equations(
    equations: &mut [flat::AssertEquation],
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    for equation in equations {
        remap_expression(&mut equation.condition, ctx)?;
        remap_expression(&mut equation.message, ctx)?;
        remap_optional_expression(&mut equation.level, ctx)?;
    }
    Ok(())
}

fn remap_algorithms(
    algorithms: &mut [flat::Algorithm],
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    for algorithm in algorithms {
        remap_statements(&mut algorithm.statements, ctx, &HashSet::new())?;
        algorithm.outputs = algorithm
            .outputs
            .iter()
            .map(|name| remap_reference(name, ctx.rename_map))
            .collect();
    }
    Ok(())
}

fn remap_when_clauses(
    clauses: &mut [flat::WhenClause],
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    for clause in clauses {
        remap_expression(&mut clause.condition, ctx)?;
        remap_when_equations(&mut clause.equations, ctx)?;
    }
    Ok(())
}

fn remap_when_equations(
    equations: &mut [flat::WhenEquation],
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, value, .. } => {
                *target = remap_var_name(target, ctx.rename_map);
                remap_expression(value, ctx)?;
            }
            flat::WhenEquation::Reinit { state, value, .. } => {
                *state = remap_var_name(state, ctx.rename_map);
                remap_expression(value, ctx)?;
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                remap_expression(condition, ctx)?;
                remap_expression(message, ctx)?;
            }
            flat::WhenEquation::Terminate { message, .. } => {
                remap_expression(message, ctx)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch) in branches {
                    remap_expression(condition, ctx)?;
                    remap_when_equations(branch, ctx)?;
                }
                remap_when_equations(else_branch, ctx)?;
            }
            flat::WhenEquation::FunctionCallOutputs {
                outputs, function, ..
            } => {
                for output in outputs {
                    *output = remap_var_name(output, ctx.rename_map);
                }
                remap_expression(function, ctx)?;
            }
        }
    }
    Ok(())
}

fn remap_optional_expression(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    if let Some(expr) = expr {
        remap_expression(expr, ctx)?;
    }
    Ok(())
}

fn remap_expression(
    expr: &mut rumoca_core::Expression,
    ctx: &RenameContext<'_>,
) -> Result<(), crate::errors::FlattenError> {
    remap_expression_with_locals(expr, ctx, &HashSet::new())
}

fn remap_expression_with_locals(
    expr: &mut rumoca_core::Expression,
    ctx: &RenameContext<'_>,
    locals: &HashSet<String>,
) -> Result<(), crate::errors::FlattenError> {
    match expr {
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            remap_expression_with_locals(lhs, ctx, locals)?;
            remap_expression_with_locals(rhs, ctx, locals)?;
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            remap_expression_with_locals(rhs, ctx, locals)?;
        }
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => {
            if !locals.contains(name.as_str()) {
                if subscripts.is_empty()
                    && let Some(projection) = ctx.projection_map.get(name.as_str())
                {
                    *expr = projection_expression(name.as_str(), projection, *span)?;
                    return Ok(());
                }
                *name = remap_reference(name, ctx.rename_map);
            }
            remap_subscripts(subscripts, ctx, locals)?;
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                remap_expression_with_locals(arg, ctx, locals)?;
            }
        }
        rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => {}
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                remap_expression_with_locals(condition, ctx, locals)?;
                remap_expression_with_locals(value, ctx, locals)?;
            }
            remap_expression_with_locals(else_branch, ctx, locals)?;
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                remap_expression_with_locals(element, ctx, locals)?;
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            remap_expression_with_locals(start, ctx, locals)?;
            if let Some(step) = step {
                remap_expression_with_locals(step, ctx, locals)?;
            }
            remap_expression_with_locals(end, ctx, locals)?;
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            let mut nested_locals = locals.clone();
            for index in indices {
                remap_expression_with_locals(&mut index.range, ctx, locals)?;
                nested_locals.insert(index.name.clone());
            }
            remap_expression_with_locals(expr, ctx, &nested_locals)?;
            if let Some(filter) = filter {
                remap_expression_with_locals(filter, ctx, &nested_locals)?;
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            remap_expression_with_locals(base, ctx, locals)?;
            remap_subscripts(subscripts, ctx, locals)?;
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            remap_expression_with_locals(base, ctx, locals)?;
        }
    }
    Ok(())
}

fn remap_subscripts(
    subscripts: &mut [rumoca_core::Subscript],
    ctx: &RenameContext<'_>,
    locals: &HashSet<String>,
) -> Result<(), crate::errors::FlattenError> {
    for subscript in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            remap_expression_with_locals(expr, ctx, locals)?;
        }
    }
    Ok(())
}

fn remap_statements(
    statements: &mut [rumoca_core::Statement],
    ctx: &RenameContext<'_>,
    locals: &HashSet<String>,
) -> Result<(), crate::errors::FlattenError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. } => {}
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                remap_component_reference(comp, ctx, locals)?;
                remap_expression_with_locals(value, ctx, locals)?;
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                let mut nested_locals = locals.clone();
                for index in indices {
                    remap_expression_with_locals(&mut index.range, ctx, locals)?;
                    nested_locals.insert(index.ident.clone());
                }
                remap_statements(equations, ctx, &nested_locals)?;
            }
            rumoca_core::Statement::While { block, .. } => {
                remap_statement_block(block, ctx, locals)?
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    remap_statement_block(block, ctx, locals)?;
                }
                if let Some(block) = else_block {
                    remap_statements(block, ctx, locals)?;
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    remap_statement_block(block, ctx, locals)?;
                }
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                ..
            } => {
                remap_component_reference(comp, ctx, locals)?;
                for arg in args {
                    remap_expression_with_locals(arg, ctx, locals)?;
                }
                for output in outputs {
                    remap_component_reference(output, ctx, locals)?;
                }
            }
            rumoca_core::Statement::Reinit {
                variable, value, ..
            } => {
                remap_component_reference(variable, ctx, locals)?;
                remap_expression_with_locals(value, ctx, locals)?;
            }
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                remap_expression_with_locals(condition, ctx, locals)?;
                remap_expression_with_locals(message, ctx, locals)?;
                if let Some(level) = level {
                    remap_expression_with_locals(level, ctx, locals)?;
                }
            }
        }
    }
    Ok(())
}

fn remap_statement_block(
    block: &mut rumoca_core::StatementBlock,
    ctx: &RenameContext<'_>,
    locals: &HashSet<String>,
) -> Result<(), crate::errors::FlattenError> {
    remap_expression_with_locals(&mut block.cond, ctx, locals)?;
    remap_statements(&mut block.stmts, ctx, locals)?;
    Ok(())
}

fn remap_component_reference(
    comp: &mut rumoca_core::ComponentReference,
    ctx: &RenameContext<'_>,
    locals: &HashSet<String>,
) -> Result<(), crate::errors::FlattenError> {
    for part in &mut comp.parts {
        remap_subscripts(&mut part.subs, ctx, locals)?;
    }

    let old_name = comp.to_var_name();
    if locals.contains(old_name.as_str()) {
        return Ok(());
    }
    let Some(new_name) = ctx.rename_map.get(old_name.as_str()) else {
        return Ok(());
    };
    if new_name == old_name.as_str() {
        return Ok(());
    }

    let trailing_subs = comp
        .parts
        .last()
        .map(|part| part.subs.clone())
        .unwrap_or_default();
    comp.parts = vec![rumoca_core::ComponentRefPart {
        ident: new_name.clone(),
        span: comp.span,
        subs: trailing_subs,
    }];
    Ok(())
}

fn remap_equation_origin(origin: &mut flat::EquationOrigin, rename_map: &HashMap<String, String>) {
    match origin {
        flat::EquationOrigin::ComponentEquation { .. }
        | flat::EquationOrigin::FlowSum { .. }
        | flat::EquationOrigin::Algorithm { .. } => {}
        flat::EquationOrigin::Connection { lhs, rhs } => {
            *lhs = remap_name_string(lhs, rename_map);
            *rhs = remap_name_string(rhs, rename_map);
        }
        flat::EquationOrigin::UnconnectedFlow { variable }
        | flat::EquationOrigin::Reinit { state: variable }
        | flat::EquationOrigin::WhenAssignment { target: variable }
        | flat::EquationOrigin::Binding { variable } => {
            *variable = remap_name_string(variable, rename_map);
        }
    }
}

fn remap_var_name(
    name: &rumoca_core::VarName,
    rename_map: &HashMap<String, String>,
) -> rumoca_core::VarName {
    rumoca_core::VarName::new(remap_name_string(name.as_str(), rename_map))
}

fn remap_reference(
    name: &rumoca_core::Reference,
    rename_map: &HashMap<String, String>,
) -> rumoca_core::Reference {
    name.with_var_name(remap_var_name(name.var_name(), rename_map))
}

fn remap_name_string(name: &str, rename_map: &HashMap<String, String>) -> String {
    if let Some(new_name) = rename_map.get(name) {
        return new_name.clone();
    }

    if let Some((base, suffix)) = split_trailing_subscript(name)
        && let Some(new_base) = rename_map.get(base)
    {
        return format!("{new_base}{suffix}");
    }

    name.to_string()
}

fn split_trailing_subscript(name: &str) -> Option<(&str, &str)> {
    let mut depth = 0i32;
    let mut group_start = None;
    let mut last_group = None;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => {
                if depth == 0 {
                    group_start = Some(idx);
                }
                depth += 1;
            }
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    last_group = Some((group_start?, idx));
                }
            }
            _ => {}
        }
    }

    let (start, end) = last_group?;
    (depth == 0 && end + 1 == name.len() && start > 0).then_some((&name[..start], &name[start..]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_flatten_name_simplify_source_7.mo"),
            0,
            1,
        )
    }

    fn var(name: &str) -> flat::Variable {
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn structured_ref(path: &[&str]) -> rumoca_core::Reference {
        rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: path
                .iter()
                .map(|ident| rumoca_core::ComponentRefPart {
                    ident: (*ident).to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                })
                .collect(),
            def_id: Some(rumoca_core::DefId::new(17)),
        })
    }

    #[test]
    fn uses_short_names_when_leaf_names_are_unique() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("body.position.x"),
            var("body.position.x"),
        );
        flat.add_variable(
            rumoca_core::VarName::new("body.velocity.vx"),
            var("body.velocity.vx"),
        );
        flat.add_equation(flat::Equation::new(
            var_ref("body.position.x"),
            Span::DUMMY,
            flat::EquationOrigin::Binding {
                variable: "body.position.x".to_string(),
            },
        ));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(flat.variables.contains_key(&rumoca_core::VarName::new("x")));
        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("vx"))
        );
        let rumoca_core::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "x");
        assert_eq!(flat.equations[0].origin.binding_variable(), Some("x"));
    }

    #[test]
    fn adds_hierarchy_for_conflicting_leaf_names() {
        let mut flat = flat::Model::new();
        flat.add_variable(rumoca_core::VarName::new("body.x"), var("body.x"));
        flat.add_variable(rumoca_core::VarName::new("other.x"), var("other.x"));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("body_x"))
        );
        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("other_x"))
        );
    }

    #[test]
    fn preserves_connector_hierarchy_for_later_interface_analysis() {
        let mut flat = flat::Model::new();
        flat.top_level_connectors.insert("frame_a".to_string());
        flat.add_variable(rumoca_core::VarName::new("frame_a.R.T"), var("frame_a.R.T"));
        flat.add_variable(rumoca_core::VarName::new("body.x"), var("body.x"));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("frame_a.R.T"))
        );
        assert!(flat.variables.contains_key(&rumoca_core::VarName::new("x")));
    }

    #[test]
    fn preserves_input_hierarchy_for_later_causality_analysis() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("plant.omega_cmd"),
            flat::Variable {
                name: rumoca_core::VarName::new("plant.omega_cmd"),
                causality: rumoca_core::Causality::Input(Default::default()),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_variable(rumoca_core::VarName::new("body.x"), var("body.x"));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("plant.omega_cmd"))
        );
        assert!(flat.variables.contains_key(&rumoca_core::VarName::new("x")));
    }

    #[test]
    fn preserves_modifier_bound_names_for_source_scope_bindings() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("p.c.rp.a"),
            flat::Variable {
                name: rumoca_core::VarName::new("p.c.rp.a"),
                binding: Some(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                }),
                binding_from_modification: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_variable(rumoca_core::VarName::new("other.a"), var("other.a"));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("p.c.rp.a"))
        );
        assert!(flat.variables.contains_key(&rumoca_core::VarName::new("a")));
    }

    #[test]
    fn disambiguates_indexed_component_arrays() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("motor[1].omega"),
            var("motor[1].omega"),
        );
        flat.add_variable(
            rumoca_core::VarName::new("motor[2].omega"),
            var("motor[2].omega"),
        );

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("motor_1_omega"))
        );
        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("motor_2_omega"))
        );
    }

    #[test]
    fn remaps_indexed_var_ref_string_suffixes() {
        let mut flat = flat::Model::new();
        flat.add_variable(rumoca_core::VarName::new("attitude.q"), var("attitude.q"));
        flat.add_variable(rumoca_core::VarName::new("q"), var("q"));
        flat.add_equation(flat::Equation::new(
            var_ref("attitude.q[1]"),
            Span::DUMMY,
            flat::EquationOrigin::Binding {
                variable: "attitude.q[1]".to_string(),
            },
        ));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        let rumoca_core::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "attitude_q[1]");
        assert_eq!(
            flat.equations[0].origin.binding_variable(),
            Some("attitude_q[1]")
        );
    }

    #[test]
    fn remaps_component_array_member_projection_to_array_expression() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("motor[1].omega"),
            var("motor[1].omega"),
        );
        flat.add_variable(
            rumoca_core::VarName::new("motor[2].omega"),
            var("motor[2].omega"),
        );
        flat.add_equation(flat::Equation::new(
            var_ref("motor.omega"),
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "motor".to_string(),
            },
        ));

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        let rumoca_core::Expression::Array { elements, .. } = &flat.equations[0].residual else {
            panic!("expected array projection");
        };
        let names = elements
            .iter()
            .map(|element| match element {
                rumoca_core::Expression::VarRef { name, .. } => name.as_str(),
                _ => panic!("expected projected var ref"),
            })
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["motor_1_omega", "motor_2_omega"]);
    }

    #[test]
    fn rejects_projection_without_reference_span() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("motor[1].omega"),
            var("motor[1].omega"),
        );
        flat.add_variable(
            rumoca_core::VarName::new("motor[2].omega"),
            var("motor[2].omega"),
        );
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("motor.omega"),
                subscripts: Vec::new(),
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "motor".to_string(),
            },
        ));

        let err = simplify_flat_names(&mut flat).expect_err("missing span should be rejected");
        assert!(matches!(
            err,
            crate::errors::FlattenError::MissingSourceContext { .. }
        ));
    }

    #[test]
    fn remaps_references_without_losing_component_reference_structure() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("body.position.x"),
            var("body.position.x"),
        );
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::VarRef {
                name: structured_ref(&["body", "position", "x"]),
                subscripts: Vec::new(),
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::Binding {
                variable: "body.position.x".to_string(),
            },
        ));
        flat.algorithms.push(flat::Algorithm {
            statements: Vec::new(),
            outputs: vec![structured_ref(&["body", "position", "x"])],
            span: Span::DUMMY,
            origin: "test".to_string(),
        });

        simplify_flat_names(&mut flat).expect("name simplification should succeed");

        let rumoca_core::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "x");
        assert_eq!(name.target_def_id(), Some(rumoca_core::DefId::new(17)));
        assert!(name.has_structure());
        assert_eq!(flat.algorithms[0].outputs[0].as_str(), "x");
        assert_eq!(
            flat.algorithms[0].outputs[0].target_def_id(),
            Some(rumoca_core::DefId::new(17))
        );
        assert!(flat.algorithms[0].outputs[0].has_structure());
    }
}
