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
    /// Rows the closure may not use as a *definition* of a variable.
    ///
    /// A caller that is about to rewrite a row must exclude it, or the closure
    /// resolves that row's own variables through the row itself and hands back a
    /// circular derivative. On `Fourbar1` the loop-closure row
    /// `b0.frame_b.r_0 = j2.frame_a.r_0` was the only definition the index
    /// offered for `j2.frame_a.r_0`, so its derivative came back as
    /// `der(b0.frame_b.r_0)` — world-fixed, hence zero — and the differentiated
    /// constraint collapsed to `0 = 0`, silently deleting the loop closure.
    ///
    /// It is a set rather than one index because a caller may nominate a group
    /// of rows at once. Excluding a row that genuinely defines nothing removes
    /// no legitimate definition; excluding one that does — a row an earlier
    /// prolongation round rewrote and re-nominated, which may since have been
    /// matched — withholds a definition the closure could have used, and the
    /// name it needed then blocks. That is the safe direction: a blocked name
    /// makes the caller decline its candidate, where a circular one would make
    /// the caller accept a constraint that had quietly collapsed.
    pub(super) excluded_equations: &'a [usize],
    /// Time-derivative values of states already demoted by an earlier
    /// prolongation step (`phi := w` once `phi` became a dummy state).
    ///
    /// A demoted state is no longer in `dae.variables.states`, and the
    /// constraint row that now defines it is not an explicit definition, so the
    /// closure below cannot rediscover its derivative. Without these values the
    /// second prolongation of a holonomic chain blocks on the first algebraic
    /// that reads the demoted state — for a multibody joint, the orientation
    /// matrix `R_rel.T` — and every level-2 candidate is rejected as an
    /// unclosed derivative. Their derivative is known and they behave exactly
    /// like states here.
    pub(super) selected_derivatives: Option<&'a HashMap<String, Expression>>,
}

fn continuous_variable<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.inputs.get(name))
}

/// Names an expression contributes to the derivative closure.
///
/// Plain variable references stop at the record-valued base of a field
/// selection, which is not a DAE variable at all. Record-linear selections are
/// projected onto their scalar components so the closure reaches the variables
/// the row actually reads (MLS 4.7 / 10.6).
fn closure_names_of_expr(dae: &Dae, expr: &Expression) -> IndexSet<VarName> {
    let mut names = collect_rhs_var_refs(expr);
    names.extend(super::record_projection::record_field_projection_names(
        dae, expr,
    ));
    names
}

/// Transitive variable closure of `seed_exprs` through defining expressions.
pub(super) fn derivative_closure_names(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
) -> IndexSet<VarName> {
    let mut names = IndexSet::new();
    for expr in seed_exprs {
        names.extend(closure_names_of_expr(dae, expr));
    }

    let mut next = 0;
    while next < names.len() {
        let name = names
            .get_index(next)
            .expect("closure index is bounded by the set length")
            .clone();
        for defining_expr in defining_expr_candidates(defining_expr_index, &name) {
            names.extend(closure_names_of_expr(dae, defining_expr));
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

/// How much of the closure a resolution pass is willing to accept.
///
/// A variable that a connector network defines through several rows at once has
/// several valid inversions, and their derivatives are not equally useful. On a
/// DC-machine excitation node the airgap row `psi_e = Le*ie` inverts to
/// `ie = psi_e/Le`, whose derivative is the symbolic `der(psi_e)/Le`, while the
/// pin row `ie = pin_ep.i` carries a closed-form value across the flow sum to
/// the constant-current source. Taking the first candidate resolves `der(ie)`
/// back through the very state index reduction is trying to demote, which then
/// looks like a self-referential cycle. Resolving closed-form candidates first
/// takes the acyclic route; the symbolic pass afterwards still reaches the
/// genuine differentiator chains, where `der(state)` is the answer.
#[derive(Clone, Copy, Eq, PartialEq)]
enum DerivativeClosurePass {
    ClosedFormOnly,
    SymbolicAllowed,
}

struct DerivativeClosureResolver<'a> {
    dae: &'a Dae,
    defining_expr_index: &'a DefiningExprIndex,
    state_name_set: &'a HashSet<String>,
    options: RelaxedDerivativeMapOptions<'a>,
    pass: DerivativeClosurePass,
}

impl DerivativeClosureResolver<'_> {
    fn resolved_derivative_candidate(
        &self,
        closure_states: &IndexMap<String, DerivativeClosureState>,
        map: &HashMap<String, Expression>,
        name: &VarName,
    ) -> Option<Expression> {
        self.defining_expr_index
            .get(name.as_str())
            .into_iter()
            .flatten()
            .filter(|candidate| {
                !self
                    .options
                    .excluded_equations
                    .contains(&candidate.equation_index)
            })
            .find_map(|candidate| {
                let defining_expr = &candidate.expr;
                let dependencies_resolved =
                    collect_rhs_var_refs(defining_expr)
                        .iter()
                        .all(|dependency| {
                            derivative_dependency_is_resolved(self.dae, closure_states, dependency)
                        });
                if !dependencies_resolved {
                    return None;
                }

                let derivative = symbolic_time_derivative(defining_expr, self.dae, map)?;
                self.accepts(&derivative, name).then_some(derivative)
            })
    }

    fn accepts(&self, derivative: &Expression, name: &VarName) -> bool {
        if self.pass == DerivativeClosurePass::ClosedFormOnly
            && expression_contains_any_der_call(derivative)
        {
            return false;
        }
        let self_derivative = expr_contains_der_of(derivative, name);
        let excluded_derivative = self
            .options
            .rejected_state_derivative
            .is_some_and(|state_name| expr_contains_der_of(derivative, state_name));
        let non_state_derivative = expr_contains_der_of_non_state(derivative, self.state_name_set);
        !self_derivative && !excluded_derivative && !non_state_derivative
    }
}

pub(super) fn build_relaxed_derivative_map_for_exprs(
    dae: &Dae,
    seed_exprs: &[Expression],
) -> Result<HashMap<String, Expression>, StructuralError> {
    let defining_expr_index = collect_differentiable_defining_expr_index(dae);
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
    let dummy_derivatives = dummy_derivative_group::generated_dummy_derivative_values(dae);
    build_relaxed_derivative_map_for_exprs_with_index(
        dae,
        &defining_expr_index,
        seed_exprs,
        RelaxedDerivativeMapOptions {
            canonical_state_derivative: Some(state_name),
            rejected_state_derivative: Some(state_name),
            excluded_equations: &[],
            selected_derivatives: Some(&dummy_derivatives),
        },
    )
}

/// Drop extracted `der(state)` values that are not explicit ODE right-hand
/// sides.
///
/// [`build_der_value_map`] solves each derivative row for its target. A row that
/// mentions several state derivatives at once — MLS 3.7's linear-implicit
/// (mass-matrix) form, e.g. the fundamental-wave power balance
/// `-v = N.re*der(Phi.re) + N.im*der(Phi.im)` — only yields
/// `der(Phi.re) := (-v - N.im*der(Phi.im))/N.re`, which still contains a state
/// derivative.
///
/// Substituting such a value while differentiating a constraint reintroduces the
/// very derivative the caller is trying to eliminate, so a perfectly valid
/// Pantelides candidate looks like a self-referential cycle and is rejected.
/// Retained state derivatives are independent symbolic coordinates; the rest of
/// this builder falls back to a plain `der(state)` reference for every entry
/// removed here, which is what chain-rule differentiation needs. Mass-matrix
/// solving owns the coupled values later.
///
/// [`compute_full_derivative_map`] already refuses to seed itself from extracted
/// right-hand sides for exactly this reason; this keeps the relaxed builder
/// consistent with it while still using genuinely explicit ODE rows.
///
/// An exact derivative alias `der(x) = der(q)` is kept. Its extracted value is a
/// bare `der(q)`, which names one independent coordinate instead of re-entering
/// the row it came from, and it is what lets a chain like `x = q` differentiate
/// to a closed derivative.
fn retain_closed_state_derivative_values(map: &mut HashMap<String, Expression>) {
    map.retain(|_, value| {
        !expression_contains_any_der_call(value) || is_plain_derivative_reference(value)
    });
}

/// True for a bare `der(name)` on an unsubscripted reference.
fn is_plain_derivative_reference(expr: &Expression) -> bool {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        ..
    } = expr
    else {
        return false;
    };
    matches!(
        args.as_slice(),
        [Expression::VarRef { subscripts, .. }] if subscripts.is_empty()
    )
}

/// Starting point of the derivative closure fixpoint.
struct ClosureSeed {
    map: HashMap<String, Expression>,
    state_name_set: HashSet<String>,
    closure_states: IndexMap<String, DerivativeClosureState>,
}

/// Seed the closure with everything whose time derivative is already known.
///
/// That is: the ODE right-hand sides of the current states, the caller's
/// canonical state derivative, and the values chosen for states an earlier
/// prolongation demoted. All three behave identically here — a name with a
/// known derivative is `Resolved` and counts as a state for the
/// non-state-derivative rejection in [`resolved_derivative_candidate`].
fn seed_derivative_closure(
    dae: &Dae,
    options: RelaxedDerivativeMapOptions<'_>,
    reachable: &IndexSet<VarName>,
) -> Result<ClosureSeed, StructuralError> {
    let mut map = build_der_value_map(dae);
    retain_closed_state_derivative_values(&mut map);
    let selected_derivatives = options.selected_derivatives;
    if let Some(selected) = selected_derivatives {
        for (name, value) in selected {
            map.insert(name.clone(), value.clone());
        }
    }
    if let Some(state_name) = options.canonical_state_derivative
        && let Some(variable) = dae.variables.states.get(state_name)
    {
        map.insert(
            state_name.as_str().to_string(),
            symbolic_der_var_ref_for_variable(variable)?,
        );
    }
    let mut state_name_set = dae
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
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
    if let Some(selected) = selected_derivatives {
        for name in selected.keys() {
            state_name_set.insert(name.clone());
            closure_states.insert(name.clone(), DerivativeClosureState::Resolved);
        }
    }
    Ok(ClosureSeed {
        map,
        state_name_set,
        closure_states,
    })
}

fn run_derivative_closure_pass(
    resolver: &DerivativeClosureResolver<'_>,
    reachable: &IndexSet<VarName>,
    closure_states: &mut IndexMap<String, DerivativeClosureState>,
    map: &mut HashMap<String, Expression>,
) {
    loop {
        let newly_resolved = reachable
            .iter()
            .filter(|name| {
                closure_states.get(name.as_str()) == Some(&DerivativeClosureState::Unresolved)
            })
            .filter_map(|name| {
                resolver
                    .resolved_derivative_candidate(closure_states, map, name)
                    .map(|derivative| (name.clone(), derivative))
            })
            .collect::<Vec<_>>();
        if newly_resolved.is_empty() {
            return;
        }
        for (name, derivative) in newly_resolved {
            map.insert(name.as_str().to_string(), derivative);
            closure_states.insert(name.as_str().to_string(), DerivativeClosureState::Resolved);
        }
    }
}

pub(super) fn build_relaxed_derivative_map_for_exprs_with_index(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
    options: RelaxedDerivativeMapOptions<'_>,
) -> Result<HashMap<String, Expression>, StructuralError> {
    let reachable = derivative_closure_names(dae, defining_expr_index, seed_exprs);
    let ClosureSeed {
        mut map,
        state_name_set,
        mut closure_states,
    } = seed_derivative_closure(dae, options, &reachable)?;

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

    for pass in [
        DerivativeClosurePass::ClosedFormOnly,
        DerivativeClosurePass::SymbolicAllowed,
    ] {
        let resolver = DerivativeClosureResolver {
            dae,
            defining_expr_index,
            state_name_set: &state_name_set,
            options,
            pass,
        };
        run_derivative_closure_pass(&resolver, &reachable, &mut closure_states, &mut map);
    }

    let mut blocked = Vec::new();
    for name in &reachable {
        if closure_states.get(name.as_str()) != Some(&DerivativeClosureState::Unresolved) {
            continue;
        }
        closure_states.insert(name.as_str().to_string(), DerivativeClosureState::Blocked);
        blocked.push(name.as_str().to_string());
        let Some(variable) = continuous_variable(dae, name) else {
            continue;
        };
        map.insert(
            name.as_str().to_string(),
            symbolic_der_var_ref_for_variable(variable)?,
        );
    }
    trace_blocked_closure_names(dae, defining_expr_index, &blocked);
    Ok(map)
}

/// Names the closure could not differentiate, and why each one stopped.
///
/// A blocked name leaves a literal `der(name)` in every expression built from
/// this map, which downstream index reduction must reject. Naming the blocked
/// set, and saying whether the name has a defining row at all, is the
/// difference between "the constraint is not differentiable" and "the
/// constraint depends on a variable whose own definition is missing".
fn trace_blocked_closure_names(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    blocked: &[String],
) {
    if blocked.is_empty() || !crate::structural_trace_enabled() {
        return;
    }
    for name in blocked.iter().take(BLOCKED_CLOSURE_TRACE_LIMIT) {
        let cause = if defining_expr_index.contains_key(name) {
            "dependency_unresolved"
        } else if continuous_variable(dae, &VarName::new(name.as_str())).is_some() {
            "no_defining_row"
        } else {
            "not_a_continuous_variable"
        };
        crate::structural_trace!(
            "[sim-trace] derivative closure blocked name={name} cause={cause}"
        );
    }
}

/// Blocked names reported per closure build.
///
/// A blocked set can span every algebraic in a multibody chain; the first few
/// are the ones a reader acts on, and the rest are their consequences.
const BLOCKED_CLOSURE_TRACE_LIMIT: usize = 12;

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
    // Chain-rule normalization needs retained state derivatives as independent
    // symbolic coordinates. Seeding this map with an extracted ODE right-hand
    // side can create a false cycle in coupled mass-matrix rows: after demoting
    // a dummy state `y` from `der(x) + der(y) = 0`, the solved value for
    // `der(x)` still contains `der(y)`, so differentiating an alias `y = x`
    // can never eliminate the newly algebraic derivative. Mass-matrix solving
    // owns those derivative values later; this pass owns only normalization.
    let mut der_map = dae
        .variables
        .states
        .iter()
        .map(|(name, variable)| {
            (
                name.as_str().to_string(),
                symbolic_der_var_ref(name, variable.source_span),
            )
        })
        .collect::<HashMap<_, _>>();
    let defining_expr_index = collect_residual_defining_expr_index(dae);

    // Iteratively resolve algebraic variable derivatives. Each successful pass
    // strictly grows `der_map`, so the finite variable set is the termination
    // bound. Do not impose an arbitrary depth cap: long connector/alias chains
    // are valid Modelica and must reach the same fixed point as short chains.
    loop {
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
