//! Appendix B invariant checks on solver-facing DAE.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;

use crate::{ToDaeError, path_utils::subscript_fallback_chain};

pub(super) fn validate_appendix_b_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    validate_runtime_contract_invariants(dae_model)?;
    validate_condition_partition(dae_model)?;
    validate_discrete_real_solved_form(dae_model)?;
    validate_discrete_valued_solved_form(dae_model)?;
    validate_strict_solver_expression_invariants(dae_model)?;
    validate_runtime_metadata_invariants(dae_model)?;
    Ok(())
}

fn validate_strict_solver_expression_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for (index, equation) in dae_model.f_x.iter().enumerate() {
        validate_solver_expression(
            &equation.rhs,
            &format!("f_x[{index}] (origin='{}')", equation.origin),
            equation.span,
        )?;
    }

    for (index, equation) in dae_model.initial_equations.iter().enumerate() {
        validate_solver_expression(
            &equation.rhs,
            &format!("initial_equations[{index}] (origin='{}')", equation.origin),
            equation.span,
        )?;
    }

    Ok(())
}

fn validate_runtime_contract_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut seen_partition_for_name: HashMap<dae::VarName, &'static str> = HashMap::new();
    register_partition_names(&mut seen_partition_for_name, "x", dae_model.states.keys())?;
    register_partition_names(
        &mut seen_partition_for_name,
        "y",
        dae_model.algebraics.keys(),
    )?;
    register_partition_names(&mut seen_partition_for_name, "u", dae_model.inputs.keys())?;
    register_partition_names(&mut seen_partition_for_name, "w", dae_model.outputs.keys())?;
    register_partition_names(
        &mut seen_partition_for_name,
        "p",
        dae_model.parameters.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "constants",
        dae_model.constants.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "z",
        dae_model.discrete_reals.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "m",
        dae_model.discrete_valued.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "x_dot_alias",
        dae_model.derivative_aliases.keys(),
    )?;
    Ok(())
}

fn register_partition_names<'a, I>(
    seen_partition_for_name: &mut HashMap<dae::VarName, &'static str>,
    partition: &'static str,
    names: I,
) -> Result<(), ToDaeError>
where
    I: Iterator<Item = &'a dae::VarName>,
{
    for name in names {
        if let Some(previous_partition) = seen_partition_for_name.insert(name.clone(), partition) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "variable `{name}` appears in multiple partitions: `{previous_partition}` and `{partition}`"
            )));
        }
    }
    Ok(())
}

fn validate_solver_expression(
    expr: &dae::Expression,
    context: &str,
    span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    if let Some(construct) = forbidden_synchronous_construct(expr) {
        return Err(ToDaeError::strict_solver_dae_violation(
            format!("{context} contains unlowered synchronous construct `{construct}`"),
            span,
        ));
    }

    match expr {
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                validate_solver_expression(arg, context, span)?;
            }
        }
        dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                validate_solver_expression(arg, context, span)?;
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            validate_solver_expression(lhs, context, span)?;
            validate_solver_expression(rhs, context, span)?;
        }
        dae::Expression::Unary { rhs, .. } => {
            validate_solver_expression(rhs, context, span)?;
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                validate_solver_expression(condition, context, span)?;
                validate_solver_expression(value, context, span)?;
            }
            validate_solver_expression(else_branch, context, span)?;
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                validate_solver_expression(element, context, span)?;
            }
        }
        dae::Expression::Range { start, step, end } => {
            validate_solver_expression(start, context, span)?;
            if let Some(step_expr) = step {
                validate_solver_expression(step_expr, context, span)?;
            }
            validate_solver_expression(end, context, span)?;
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            validate_solver_expression(expr, context, span)?;
            for index in indices {
                validate_solver_expression(&index.range, context, span)?;
            }
            if let Some(filter_expr) = filter {
                validate_solver_expression(filter_expr, context, span)?;
            }
        }
        dae::Expression::Index { base, subscripts } => {
            validate_solver_expression(base, context, span)?;
            for subscript in subscripts {
                if let dae::Subscript::Expr(subscript_expr) = subscript {
                    validate_solver_expression(subscript_expr, context, span)?;
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            validate_solver_expression(base, context, span)?;
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }

    Ok(())
}

fn forbidden_synchronous_construct(expr: &dae::Expression) -> Option<&'static str> {
    match expr {
        dae::Expression::BuiltinCall { function, .. } => {
            if *function == dae::BuiltinFunction::Sample {
                return Some("sample");
            }
            None
        }
        dae::Expression::FunctionCall { name, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            forbidden_sync_name(short)
        }
        _ => None,
    }
}

fn forbidden_sync_name(name: &str) -> Option<&'static str> {
    match name {
        "sample" => Some("sample"),
        "hold" => Some("hold"),
        "Clock" => Some("Clock"),
        "subSample" => Some("subSample"),
        "superSample" => Some("superSample"),
        "shiftSample" => Some("shiftSample"),
        "backSample" => Some("backSample"),
        "noClock" => Some("noClock"),
        "firstTick" => Some("firstTick"),
        "previous" => Some("previous"),
        _ => None,
    }
}

fn validate_condition_partition(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    if dae_model.f_c.len() != dae_model.relation.len() {
        return Err(ToDaeError::condition_partition_violation(format!(
            "f_c count ({}) does not match relation count ({})",
            dae_model.f_c.len(),
            dae_model.relation.len()
        )));
    }

    for (index, (equation, relation)) in dae_model
        .f_c
        .iter()
        .zip(dae_model.relation.iter())
        .enumerate()
    {
        let expected_lhs = dae::VarName::new(format!("c[{}]", index + 1));
        if equation.lhs.as_ref() != Some(&expected_lhs) {
            return Err(ToDaeError::condition_partition_violation(format!(
                "f_c[{index}] must define `{expected_lhs}` but found lhs={:?}",
                equation.lhs
            )));
        }

        if format!("{:?}", equation.rhs) != format!("{:?}", relation) {
            return Err(ToDaeError::condition_partition_violation(format!(
                "f_c[{index}] rhs does not match relation[{index}]"
            )));
        }
    }

    Ok(())
}

fn validate_discrete_valued_solved_form(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut assignments: HashMap<dae::VarName, &dae::Equation> = HashMap::new();
    for equation in &dae_model.f_m {
        let lhs = equation.lhs.as_ref().ok_or_else(|| {
            ToDaeError::discrete_solved_form_violation(
                format!(
                    "f_m equation is not in explicit assignment form (origin='{}', rhs={:?})",
                    equation.origin, equation.rhs
                ),
                equation.span,
            )
        })?;

        if !is_discrete_valued_name(dae_model, lhs) {
            return Err(ToDaeError::discrete_solved_form_violation(
                format!("f_m lhs `{lhs}` is not a discrete-valued variable"),
                equation.span,
            ));
        }

        if let Some(previous) = assignments.insert(lhs.clone(), equation) {
            return Err(ToDaeError::discrete_solved_form_violation(
                format!(
                    "duplicate f_m assignment target `{lhs}` (new origin='{}', prior origin='{}')",
                    equation.origin, previous.origin
                ),
                equation.span,
            ));
        }
    }

    let targets: HashSet<dae::VarName> = assignments.keys().cloned().collect();
    let mut deps: HashMap<dae::VarName, Vec<dae::VarName>> = HashMap::new();

    for (lhs, equation) in &assignments {
        let mut refs = HashSet::new();
        collect_current_var_refs(&equation.rhs, &mut refs);

        let mut lhs_deps = Vec::new();
        for var_ref in refs {
            let Some(dep_target) = resolve_assignment_target(&var_ref, &targets) else {
                continue;
            };
            if dep_target == *lhs {
                return Err(ToDaeError::discrete_solved_form_violation(
                    format!("f_m assignment `{lhs}` has direct self-dependency"),
                    equation.span,
                ));
            }
            lhs_deps.push(dep_target);
        }

        lhs_deps.sort_unstable_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
        lhs_deps.dedup();
        deps.insert(lhs.clone(), lhs_deps);
    }

    if let Some(cycle) = find_cycle(&deps) {
        let cycle_str = cycle
            .iter()
            .map(dae::VarName::as_str)
            .collect::<Vec<_>>()
            .join(" -> ");
        let span = assignments
            .get(&cycle[0])
            .map(|equation| equation.span)
            .unwrap_or(rumoca_core::Span::DUMMY);
        return Err(ToDaeError::discrete_solved_form_violation(
            format!("f_m assignments have cyclic dependency: {cycle_str}"),
            span,
        ));
    }

    Ok(())
}

fn validate_discrete_real_solved_form(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut assignments: HashMap<dae::VarName, &dae::Equation> = HashMap::new();
    for equation in &dae_model.f_z {
        // f_z may contain residual-style lowered equations in current ToDae
        // output. Enforce partition integrity when an explicit target exists.
        let Some(lhs) = equation.lhs.as_ref() else {
            continue;
        };

        if !is_discrete_real_or_state_name(dae_model, lhs) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "f_z lhs `{lhs}` is not an event-updated Real target (state or discrete Real)"
            )));
        }

        if let Some(previous) = assignments.insert(lhs.clone(), equation) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "duplicate f_z assignment target `{lhs}` (new origin='{}', prior origin='{}')",
                equation.origin, previous.origin
            )));
        }
    }
    Ok(())
}

fn is_discrete_valued_name(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.discrete_valued.contains_key(name)
        || subscript_fallback_chain(name)
            .into_iter()
            .any(|candidate| dae_model.discrete_valued.contains_key(&candidate))
}

fn is_discrete_real_or_state_name(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.discrete_reals.contains_key(name)
        || dae_model.states.contains_key(name)
        || subscript_fallback_chain(name).into_iter().any(|candidate| {
            dae_model.discrete_reals.contains_key(&candidate)
                || dae_model.states.contains_key(&candidate)
        })
}

fn resolve_assignment_target(
    name: &dae::VarName,
    targets: &HashSet<dae::VarName>,
) -> Option<dae::VarName> {
    if targets.contains(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name)
        .into_iter()
        .find(|candidate| targets.contains(candidate))
}

fn collect_current_var_refs(expr: &dae::Expression, out: &mut HashSet<dae::VarName>) {
    match expr {
        dae::Expression::VarRef { name, .. } => {
            out.insert(name.clone());
        }
        dae::Expression::BuiltinCall { function, args } => {
            if matches!(function, dae::BuiltinFunction::Pre) {
                return;
            }
            for arg in args {
                collect_current_var_refs(arg, out);
            }
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if short_name == "previous" {
                return;
            }
            for arg in args {
                collect_current_var_refs(arg, out);
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_current_var_refs(lhs, out);
            collect_current_var_refs(rhs, out);
        }
        dae::Expression::Unary { rhs, .. } => collect_current_var_refs(rhs, out),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_current_var_refs(cond, out);
                collect_current_var_refs(value, out);
            }
            collect_current_var_refs(else_branch, out);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_current_var_refs(element, out);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_current_var_refs(start, out);
            if let Some(step_expr) = step {
                collect_current_var_refs(step_expr, out);
            }
            collect_current_var_refs(end, out);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_current_var_refs(expr, out);
            for index in indices {
                collect_current_var_refs(&index.range, out);
            }
            if let Some(filter_expr) = filter {
                collect_current_var_refs(filter_expr, out);
            }
        }
        dae::Expression::Index { base, subscripts } => {
            collect_current_var_refs(base, out);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_current_var_refs(expr, out);
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => collect_current_var_refs(base, out),
        dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn find_cycle(deps: &HashMap<dae::VarName, Vec<dae::VarName>>) -> Option<Vec<dae::VarName>> {
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    let mut stack = Vec::new();

    for target in deps.keys() {
        if let Some(cycle) = dfs_cycle(target, deps, &mut visiting, &mut visited, &mut stack) {
            return Some(cycle);
        }
    }

    None
}

fn dfs_cycle(
    node: &dae::VarName,
    deps: &HashMap<dae::VarName, Vec<dae::VarName>>,
    visiting: &mut HashSet<dae::VarName>,
    visited: &mut HashSet<dae::VarName>,
    stack: &mut Vec<dae::VarName>,
) -> Option<Vec<dae::VarName>> {
    if visited.contains(node) {
        return None;
    }
    if let Some(pos) = stack.iter().position(|entry| entry == node) {
        let mut cycle = stack[pos..].to_vec();
        cycle.push(node.clone());
        return Some(cycle);
    }
    if !visiting.insert(node.clone()) {
        return None;
    }

    stack.push(node.clone());
    if let Some(children) = deps.get(node) {
        for child in children {
            if let Some(cycle) = dfs_cycle(child, deps, visiting, visited, stack) {
                return Some(cycle);
            }
        }
    }
    stack.pop();
    visiting.remove(node);
    visited.insert(node.clone());
    None
}

fn validate_runtime_metadata_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for schedule in &dae_model.clock_schedules {
        if !schedule.period_seconds.is_finite() || schedule.period_seconds <= 0.0 {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock period must be finite and positive, got {}",
                schedule.period_seconds
            )));
        }
        if !schedule.phase_seconds.is_finite() {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock phase must be finite, got {}",
                schedule.phase_seconds
            )));
        }
    }

    for pair in dae_model.scheduled_time_events.windows(2) {
        if pair[1] <= pair[0] {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "scheduled_time_events must be strictly increasing; got [{}, {}]",
                pair[0], pair[1]
            )));
        }
    }

    let mut seen_roots = HashSet::new();
    for root in &dae_model.synthetic_root_conditions {
        let key = format!("{root:?}");
        if !seen_roots.insert(key) {
            return Err(ToDaeError::runtime_metadata_violation(
                "synthetic_root_conditions contain duplicates",
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;

    fn bool_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: dae::VarName::new(name),
            ..Default::default()
        }
    }

    fn var_ref(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn pre_var(name: &str) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![var_ref(name)],
        }
    }

    fn lit(value: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(value))
    }

    fn call(name: &str, args: Vec<dae::Expression>) -> dae::Expression {
        dae::Expression::FunctionCall {
            name: dae::VarName::new(name),
            args,
            is_constructor: false,
        }
    }

    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[test]
    fn condition_partition_mismatch_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model.relation.push(var_ref("u"));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("missing f_c entry must fail condition partition validation");
        assert!(matches!(
            err,
            ToDaeError::ConditionPartitionViolation { .. }
        ));
    }

    #[test]
    fn runtime_contract_rejects_partition_overlap() {
        let mut dae_model = dae::Dae::default();
        let shared = dae::VarName::new("x");
        dae_model.states.insert(shared.clone(), bool_var("x"));
        dae_model.algebraics.insert(shared, bool_var("x"));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("overlapping partition variable must fail runtime contract validation");
        assert!(matches!(err, ToDaeError::RuntimeContractViolation { .. }));
    }

    #[test]
    fn runtime_contract_rejects_non_assignment_fz_equation() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("a"), bool_var("a"));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("a"),
            lit(1.0),
            rumoca_core::Span::DUMMY,
            "test explicit f_z non-discrete-real target",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("non-discrete-real f_z target must fail runtime contract validation");
        assert!(matches!(err, ToDaeError::RuntimeContractViolation { .. }));
    }

    #[test]
    fn fm_requires_explicit_assignments() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("m"), bool_var("m"));
        dae_model.f_m.push(dae::Equation::residual(
            sub(var_ref("m"), pre_var("m")),
            rumoca_core::Span::DUMMY,
            "test residual f_m",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("residual f_m equation must fail solved-form validation");
        assert!(matches!(
            err,
            ToDaeError::DiscreteSolvedFormViolation { .. }
        ));
    }

    #[test]
    fn fm_cycle_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("a"), bool_var("a"));
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("b"), bool_var("b"));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("a"),
            var_ref("b"),
            rumoca_core::Span::DUMMY,
            "a = b",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("b"),
            var_ref("a"),
            rumoca_core::Span::DUMMY,
            "b = a",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("cyclic f_m dependencies must fail validation");
        assert!(matches!(
            err,
            ToDaeError::DiscreteSolvedFormViolation { .. }
        ));
    }

    #[test]
    fn fm_acyclic_with_pre_is_accepted() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("a"), bool_var("a"));
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("b"), bool_var("b"));
        dae_model.relation.push(var_ref("a"));
        dae_model.f_c.push(dae::Equation::explicit(
            dae::VarName::new("c[1]"),
            var_ref("a"),
            rumoca_core::Span::DUMMY,
            "condition",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("a"),
            pre_var("a"),
            rumoca_core::Span::DUMMY,
            "a = pre(a)",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("b"),
            var_ref("a"),
            rumoca_core::Span::DUMMY,
            "b = a",
        ));

        validate_appendix_b_invariants(&dae_model)
            .expect("acyclic f_m with pre() dependency should pass validation");
    }

    #[test]
    fn strict_solver_dae_rejects_sample_in_fx() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![lit(0.0), lit(0.1)],
            },
            rumoca_core::Span::DUMMY,
            "sample in f_x",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("sample() in f_x must fail strict solver DAE validation");
        assert!(matches!(err, ToDaeError::StrictSolverDaeViolation { .. }));
    }

    #[test]
    fn strict_solver_dae_rejects_clock_in_initial_equation() {
        let mut dae_model = dae::Dae::default();
        dae_model.initial_equations.push(dae::Equation::residual(
            call("Clock", vec![lit(0.1)]),
            rumoca_core::Span::DUMMY,
            "clock in initial equation",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("Clock() in initial equation must fail strict solver DAE validation");
        assert!(matches!(err, ToDaeError::StrictSolverDaeViolation { .. }));
    }

    #[test]
    fn strict_solver_dae_allows_clock_constructor_metadata() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .clock_constructor_exprs
            .push(call("Clock", vec![lit(0.1)]));

        validate_appendix_b_invariants(&dae_model).expect(
            "clock constructor metadata should not violate strict solver expression checks",
        );
    }
}
