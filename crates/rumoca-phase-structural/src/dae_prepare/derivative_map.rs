use super::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DerivativeClosureState {
    Unresolved,
    Resolved,
    Blocked,
}

#[derive(Clone, Copy, Default)]
pub(super) struct RelaxedDerivativeMapOptions<'a> {
    pub(super) canonical_state_derivative: Option<&'a VarName>,
    pub(super) rejected_state_derivative: Option<&'a VarName>,
    pub(super) excluded_equation: Option<usize>,
}

fn continuous_variable<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.inputs.get(name))
}

fn derivative_closure_names(
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
) -> IndexSet<VarName> {
    let mut names = IndexSet::new();
    for expr in seed_exprs {
        names.extend(collect_rhs_var_refs(expr));
    }

    let mut next = 0;
    while next < names.len() {
        let name = names
            .get_index(next)
            .expect("closure index is bounded by the set length")
            .clone();
        for defining_expr in defining_expr_candidates(defining_expr_index, &name) {
            names.extend(collect_rhs_var_refs(defining_expr));
        }
        next += 1;
    }
    names
}

fn derivative_dependency_is_resolved(
    dae: &Dae,
    states: &IndexMap<String, DerivativeClosureState>,
    name: &VarName,
) -> bool {
    name.as_str() == "time"
        || dae.variables.parameters.contains_key(name)
        || dae.variables.constants.contains_key(name)
        || states.get(name.as_str()) == Some(&DerivativeClosureState::Resolved)
}

fn resolved_derivative_candidate(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    closure_states: &IndexMap<String, DerivativeClosureState>,
    state_name_set: &HashSet<String>,
    options: RelaxedDerivativeMapOptions<'_>,
    map: &HashMap<String, Expression>,
    name: &VarName,
) -> Option<Expression> {
    defining_expr_index
        .get(name.as_str())
        .into_iter()
        .flatten()
        .filter(|candidate| Some(candidate.equation_index) != options.excluded_equation)
        .find_map(|candidate| {
            let defining_expr = &candidate.expr;
            let dependencies_resolved =
                collect_rhs_var_refs(defining_expr)
                    .iter()
                    .all(|dependency| {
                        derivative_dependency_is_resolved(dae, closure_states, dependency)
                    });
            if !dependencies_resolved {
                return None;
            }

            let derivative = symbolic_time_derivative(defining_expr, dae, map)?;
            let self_derivative = expr_contains_der_of(&derivative, name);
            let excluded_derivative = options
                .rejected_state_derivative
                .is_some_and(|state_name| expr_contains_der_of(&derivative, state_name));
            let non_state_derivative = expr_contains_der_of_non_state(&derivative, state_name_set);
            (!self_derivative && !excluded_derivative && !non_state_derivative)
                .then_some(derivative)
        })
}

pub(super) fn build_relaxed_derivative_map_for_exprs(
    dae: &Dae,
    seed_exprs: &[Expression],
) -> Result<HashMap<String, Expression>, StructuralError> {
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    build_relaxed_derivative_map_for_exprs_with_index(
        dae,
        &defining_expr_index,
        seed_exprs,
        RelaxedDerivativeMapOptions::default(),
    )
}

pub(super) fn build_relaxed_derivative_map_for_state_definition(
    dae: &Dae,
    seed_exprs: &[Expression],
    state_name: &VarName,
) -> Result<HashMap<String, Expression>, StructuralError> {
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    build_relaxed_derivative_map_for_exprs_with_index(
        dae,
        &defining_expr_index,
        seed_exprs,
        RelaxedDerivativeMapOptions {
            canonical_state_derivative: Some(state_name),
            rejected_state_derivative: Some(state_name),
            excluded_equation: None,
        },
    )
}

pub(super) fn build_relaxed_derivative_map_for_exprs_with_index(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
    options: RelaxedDerivativeMapOptions<'_>,
) -> Result<HashMap<String, Expression>, StructuralError> {
    let mut map = build_der_value_map(dae);
    if let Some(state_name) = options.canonical_state_derivative
        && let Some(variable) = dae.variables.states.get(state_name)
    {
        map.insert(
            state_name.as_str().to_string(),
            symbolic_der_var_ref_for_variable(variable)?,
        );
    }
    let state_name_set = dae
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let reachable = derivative_closure_names(defining_expr_index, seed_exprs);
    let mut closure_states = reachable
        .iter()
        .filter_map(|name| {
            if name.as_str() == "time"
                || dae.variables.parameters.contains_key(name)
                || dae.variables.constants.contains_key(name)
            {
                return None;
            }
            let state = if dae.variables.states.contains_key(name) {
                DerivativeClosureState::Resolved
            } else {
                DerivativeClosureState::Unresolved
            };
            Some((name.as_str().to_string(), state))
        })
        .collect::<IndexMap<_, _>>();

    for name in &reachable {
        if !dae.variables.states.contains_key(name) || map.contains_key(name.as_str()) {
            continue;
        }
        let variable = dae.variables.states.get(name).ok_or_else(|| {
            StructuralError::UnspannedContractViolation {
                reason: format!("state `{name}` disappeared while building derivative closure"),
            }
        })?;
        map.insert(
            name.as_str().to_string(),
            symbolic_der_var_ref_for_variable(variable)?,
        );
    }

    loop {
        let newly_resolved = reachable
            .iter()
            .filter(|name| {
                closure_states.get(name.as_str()) == Some(&DerivativeClosureState::Unresolved)
            })
            .filter_map(|name| {
                resolved_derivative_candidate(
                    dae,
                    defining_expr_index,
                    &closure_states,
                    &state_name_set,
                    options,
                    &map,
                    name,
                )
                .map(|derivative| (name.clone(), derivative))
            })
            .collect::<Vec<_>>();
        if newly_resolved.is_empty() {
            break;
        }
        for (name, derivative) in newly_resolved {
            map.insert(name.as_str().to_string(), derivative);
            closure_states.insert(name.as_str().to_string(), DerivativeClosureState::Resolved);
        }
    }

    for name in &reachable {
        if closure_states.get(name.as_str()) != Some(&DerivativeClosureState::Unresolved) {
            continue;
        }
        closure_states.insert(name.as_str().to_string(), DerivativeClosureState::Blocked);
        let Some(variable) = continuous_variable(dae, name) else {
            continue;
        };
        map.insert(
            name.as_str().to_string(),
            symbolic_der_var_ref_for_variable(variable)?,
        );
    }
    Ok(map)
}

/// Iteratively resolve time derivatives for algebraic variables.
///
/// Starting from known state derivatives (from `build_der_value_map`), this
/// function iteratively resolves derivatives for algebraic variables by:
/// 1. Finding the algebraic equation that defines each variable: `z = expr`
/// 2. Differentiating `expr` using the chain rule with known derivatives
/// 3. Adding the resolved derivative to the map and repeating
///
/// This avoids promoting algebraic variables to states, which would create
/// redundant degrees of freedom and conflicting ODE/algebraic constraints.
pub fn compute_full_derivative_map(dae: &Dae) -> HashMap<String, Expression> {
    let mut der_map = build_der_value_map(dae);
    let defining_expr_index = collect_residual_defining_expr_index(dae);

    // Iteratively resolve algebraic variable derivatives
    // Each pass may resolve new variables that enable further resolution
    let max_iters = 20; // prevent infinite loops
    for _ in 0..max_iters {
        let mut new_entries = Vec::new();

        // Outputs are causal algebraics defined by their own block equations, so
        // their time derivatives are differentiable just like algebraics. They
        // must be resolved too: a `Modelica.Blocks.Continuous.Der` chain reads
        // `der(output)` (e.g. `der1.y = der(der1.u)` with `der1.u = Bessel.y`),
        // which only expands once `der(Bessel.y)` is in the map.
        for alg_name in dae
            .variables
            .algebraics
            .keys()
            .chain(dae.variables.outputs.keys())
        {
            if der_map.contains_key(alg_name.as_str()) {
                continue; // Already resolved
            }
            let derivative = defining_expr_candidates(&defining_expr_index, alg_name)
                .find_map(|expr| symbolic_time_derivative(expr, dae, &der_map));
            if let Some(d) = derivative {
                new_entries.push((alg_name.as_str().to_string(), d));
            }
        }

        if new_entries.is_empty() {
            break; // Fixed point reached
        }

        for (name, deriv) in new_entries {
            der_map.insert(name, deriv);
        }
    }

    der_map
}

/// Expand all `der()` calls in the DAE equations using chain-rule derivatives.
///
/// This pass:
/// 1. Builds a full derivative map (states + resolved algebraics)
/// 2. Substitutes `der(algebraic_var)` with its chain-rule derivative
/// 3. Expands compound `der(non-VarRef)` using the chain rule
///
/// After this pass, only `der(state)` calls remain (needed for mass matrix).
/// All `der(algebraic)` and `der(compound)` calls are replaced with algebraic
/// expressions. This prevents spurious state promotion.
pub fn expand_compound_derivatives(dae: &mut Dae) {
    if !needs_compound_derivative_expansion(dae) {
        return;
    }

    let der_map = compute_full_derivative_map(dae);
    if der_map.is_empty() {
        return;
    }

    // Build set of state names — we keep der(state) intact
    let state_names: HashSet<String> = dae
        .variables
        .states
        .keys()
        .map(|n| n.as_str().to_string())
        .collect();

    let expanded_continuous: Vec<Expression> = dae
        .continuous
        .equations
        .iter()
        .map(|eq| expand_der_in_expr_full(&eq.rhs, dae, &der_map, &state_names))
        .collect();
    let expanded_initialization: Vec<Expression> = dae
        .initialization
        .equations
        .iter()
        .map(|eq| expand_der_in_expr_full(&eq.rhs, dae, &der_map, &state_names))
        .collect();
    for (eq, new_rhs) in dae.continuous.equations.iter_mut().zip(expanded_continuous) {
        eq.rhs = new_rhs;
    }
    for (eq, new_rhs) in dae
        .initialization
        .equations
        .iter_mut()
        .zip(expanded_initialization)
    {
        eq.rhs = new_rhs;
    }
}

pub(super) fn needs_compound_derivative_expansion(dae: &Dae) -> bool {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let matcher = DerivativeNameMatcher::from_var_names(&state_names);
    dae.continuous
        .equations
        .iter()
        .chain(&dae.initialization.equations)
        .any(|eq| expr_contains_expandable_derivative(&eq.rhs, &matcher))
}

fn expr_contains_expandable_derivative(expr: &Expression, matcher: &DerivativeNameMatcher) -> bool {
    let mut checker = ExpandableDerivativeChecker {
        matcher,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct ExpandableDerivativeChecker<'a> {
    matcher: &'a DerivativeNameMatcher,
    found: bool,
}

impl ExpressionVisitor for ExpandableDerivativeChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            self.found = match args.first() {
                Some(arg) => !self.matcher.expression_refers_to_match(arg),
                None => true,
            };
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}
