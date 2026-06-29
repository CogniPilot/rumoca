use std::collections::{BTreeSet, HashSet};

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_dae::expr_contains_var;

use super::{
    Dae, Equation, Expression, Literal, OpBinary, OpUnary, Span, Subscript, VarName, add_expr,
    direct_demotion_round_context, expression_contains_any_der_call,
    extract_state_direct_assignment_equation, state_has_standalone_der_equation, state_select_rank,
    sub_expr,
};

const LINEAR_EPSILON: f64 = 1.0e-12;

#[derive(Debug, Clone)]
struct LinearRow {
    span: Span,
    terms: IndexMap<String, f64>,
    /// Parameters whose compile-time values were baked into the coefficients.
    /// When a demotion derived from this row is applied, these must be pinned
    /// as structural (no longer runtime-tunable) or a tuned value would
    /// silently disagree with the baked coefficient.
    structural_params: BTreeSet<String>,
}

impl LinearRow {
    fn new(span: Span) -> Self {
        Self {
            span,
            terms: IndexMap::new(),
            structural_params: BTreeSet::new(),
        }
    }

    fn add_term(&mut self, name: String, coeff: f64) {
        let next = self.terms.get(&name).copied().unwrap_or(0.0) + coeff;
        if next.abs() <= LINEAR_EPSILON {
            self.terms.shift_remove(&name);
        } else {
            self.terms.insert(name, next);
        }
    }

    fn scale(&mut self, factor: f64) {
        for coeff in self.terms.values_mut() {
            *coeff *= factor;
        }
        self.remove_near_zero_terms();
    }

    fn add_scaled(&mut self, other: &LinearRow, factor: f64) {
        if self.span.is_dummy() && !other.span.is_dummy() {
            self.span = other.span;
        }
        for (name, coeff) in &other.terms {
            self.add_term(name.clone(), coeff * factor);
        }
        self.structural_params
            .extend(other.structural_params.iter().cloned());
    }

    fn remove_near_zero_terms(&mut self) {
        self.terms.retain(|_, coeff| coeff.abs() > LINEAR_EPSILON);
    }

    fn coefficient(&self, name: &str) -> f64 {
        self.terms.get(name).copied().unwrap_or(0.0)
    }

    fn contains_continuous_non_state(
        &self,
        state_name_set: &HashSet<String>,
        non_state_unknown_names: &HashSet<String>,
    ) -> bool {
        self.terms.keys().any(|name| {
            !state_name_set.contains(name.as_str())
                && non_state_unknown_names.contains(name.as_str())
        })
    }

    fn state_terms<'a>(
        &'a self,
        state_name_set: &'a HashSet<String>,
    ) -> impl Iterator<Item = &'a str> + 'a {
        self.terms
            .keys()
            .map(String::as_str)
            .filter(|name| state_name_set.contains(*name))
    }
}

fn direct_dummy_state_candidate(
    dae: &Dae,
    eq: &Equation,
    state_names: &[VarName],
    state_name_set: &HashSet<String>,
    when_assigned_states: &HashSet<String>,
) -> Option<VarName> {
    let (state_name, defining_expr) =
        extract_state_direct_assignment_equation(eq, state_names, state_name_set)?;
    if when_assigned_states.contains(state_name.as_str()) {
        return None;
    }
    if dae
        .variables
        .states
        .get(&state_name)
        .is_some_and(|state| state.state_select == rumoca_core::StateSelect::Always)
    {
        return None;
    }
    if expression_contains_any_der_call(&defining_expr) {
        return None;
    }
    if expr_contains_var(&defining_expr, &state_name) {
        return None;
    }
    let references_other_state = state_names
        .iter()
        .any(|other| other != &state_name && expr_contains_var(&defining_expr, other))
        || defining_expr_references_non_alias_output(dae, &defining_expr, &state_name);
    references_other_state.then_some(state_name)
}

/// True if `defining_expr` references a model output variable that is not just a
/// direct alias of `state_name`. A state defined by `S = E` where `E` depends on
/// a computed output is over-determined: outputs are causally computed by their
/// own block equations, so a differentiated input bound to one can be a dummy
/// derivative to deselect. Plain visualization aliases (`output = state`) must
/// not trigger demotion, or the state loses its ODE row.
fn defining_expr_references_non_alias_output(
    dae: &Dae,
    defining_expr: &Expression,
    state_name: &VarName,
) -> bool {
    let mut refs: Vec<VarName> = Vec::new();
    defining_expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| {
        dae.variables.outputs.contains_key(name)
            && !output_is_direct_alias_of_state(dae, name, state_name)
    })
}

fn output_is_direct_alias_of_state(dae: &Dae, output_name: &VarName, state_name: &VarName) -> bool {
    super::find_defining_expr_candidates(dae, output_name)
        .into_iter()
        .any(|expr| expression_is_plain_var_ref(&expr, state_name))
}

fn expression_is_plain_var_ref(expr: &Expression, expected: &VarName) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts, .. }
            if subscripts.is_empty() && name.var_name() == expected
    )
}

/// True when the constrained-dummy defining expression for `state_name`
/// references another state, i.e. the defining constraint genuinely couples
/// differential states (a high-index DAE such as `w1 = ratio * w2`).
fn defining_expr_couples_other_state(
    defining_expr: &Expression,
    state_name: &VarName,
    state_names: &[VarName],
) -> bool {
    state_names
        .iter()
        .any(|other| other != state_name && expr_contains_var(defining_expr, other))
}

fn candidate_is_self_integrating_non_state_alias(
    dae: &Dae,
    state_name: &VarName,
    definition: &ConstrainedDummyDefinition,
    state_names: &[VarName],
) -> bool {
    state_has_standalone_der_equation(dae, state_name, state_names).unwrap_or(false)
        && !defining_expr_couples_other_state(&definition.defining_expr, state_name, state_names)
}

fn numeric_constant(expr: &Expression) -> Option<f64> {
    match expr {
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value as f64),
        Expression::Unary { op, rhs, .. } => {
            let value = numeric_constant(rhs)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = numeric_constant(lhs)?;
            let rhs = numeric_constant(rhs)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
                OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
                OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
                OpBinary::Div | OpBinary::DivElem => Some(lhs / rhs),
                _ => None,
            }
        }
        _ => None,
    }
}

fn scalar_var_name(dae: &Dae, name: &VarName, subscripts: &[Subscript]) -> Option<String> {
    if !subscripts.is_empty()
        && dae
            .variables
            .states
            .get(name)
            .is_some_and(|state| state.size() == 1)
    {
        return Some(name.as_str().to_string());
    }
    let mut out = name.as_str().to_string();
    for subscript in subscripts {
        let index = match subscript {
            Subscript::Index { value: index, .. } => *index,
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(index),
                    ..
                } => *index,
                _ => return None,
            },
            Subscript::Colon { .. } => return None,
        };
        out.push('[');
        out.push_str(&index.to_string());
        out.push(']');
    }
    Some(out)
}

fn linear_terms(dae: &Dae, expr: &Expression) -> Option<LinearRow> {
    match expr {
        Expression::Literal { .. } => Some(LinearRow::new(expression_row_span(expr))),
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut row = LinearRow::new(expression_row_span(expr));
            row.add_term(scalar_var_name(dae, name.var_name(), subscripts)?, 1.0);
            Some(row)
        }
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => {
            let mut row = linear_terms(dae, rhs)?;
            row.scale(-1.0);
            Some(row)
        }
        Expression::Unary { .. } => None,
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                let mut row = linear_terms(dae, lhs)?;
                row.add_scaled(&linear_terms(dae, rhs)?, 1.0);
                Some(row)
            }
            OpBinary::Sub | OpBinary::SubElem => {
                let mut row = linear_terms(dae, lhs)?;
                row.add_scaled(&linear_terms(dae, rhs)?, -1.0);
                Some(row)
            }
            OpBinary::Mul | OpBinary::MulElem => {
                let mut used = BTreeSet::new();
                if let Some(scale) = structural_numeric_constant(dae, lhs, &mut used, 0) {
                    let mut row = linear_terms(dae, rhs)?;
                    row.scale(scale);
                    row.structural_params.extend(used);
                    Some(row)
                } else if let Some(scale) = {
                    used.clear();
                    structural_numeric_constant(dae, rhs, &mut used, 0)
                } {
                    let mut row = linear_terms(dae, lhs)?;
                    row.scale(scale);
                    row.structural_params.extend(used);
                    Some(row)
                } else {
                    None
                }
            }
            OpBinary::Div | OpBinary::DivElem => {
                let mut used = BTreeSet::new();
                let scale = structural_numeric_constant(dae, rhs, &mut used, 0)?;
                if scale.abs() <= LINEAR_EPSILON {
                    return None;
                }
                let mut row = linear_terms(dae, lhs)?;
                row.scale(1.0 / scale);
                row.structural_params.extend(used);
                Some(row)
            }
            _ => None,
        },
        Expression::BuiltinCall { .. }
        | Expression::FunctionCall { .. }
        | Expression::If { .. }
        | Expression::Array { .. }
        | Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::FieldAccess { .. }
        | Expression::Index { .. }
        | Expression::Empty { .. } => None,
    }
}

/// Resolve a numeric coefficient that may go through fixed parameter or
/// constant values: literals and literal arithmetic resolve as in
/// [`numeric_constant`]; an unsubscripted reference to a parameter/constant
/// resolves through its (transitively numeric) start value. Parameters
/// consumed this way are recorded in `used_params` so callers can pin them as
/// structural once their value is baked into a reduction.
fn structural_numeric_constant(
    dae: &Dae,
    expr: &Expression,
    used_params: &mut BTreeSet<String>,
    depth: usize,
) -> Option<f64> {
    if depth > 8 {
        return None;
    }
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let var_name = name.var_name();
            let is_parameter = dae.variables.parameters.contains_key(var_name);
            let var = dae
                .variables
                .parameters
                .get(var_name)
                .or_else(|| dae.variables.constants.get(var_name))?;
            let start = var.start.as_ref()?;
            let value = structural_numeric_constant(dae, start, used_params, depth + 1)?;
            if is_parameter {
                used_params.insert(var_name.as_str().to_string());
            }
            Some(value)
        }
        Expression::Unary { op, rhs, .. } => {
            let value = structural_numeric_constant(dae, rhs, used_params, depth + 1)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = structural_numeric_constant(dae, lhs, used_params, depth + 1)?;
            let rhs = structural_numeric_constant(dae, rhs, used_params, depth + 1)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
                OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
                OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
                OpBinary::Div | OpBinary::DivElem => Some(lhs / rhs),
                _ => None,
            }
        }
        _ => numeric_constant(expr),
    }
}

fn equation_linear_row(dae: &Dae, eq: &Equation) -> Option<LinearRow> {
    if expression_contains_any_der_call(&eq.rhs) {
        return None;
    }
    if let Some(lhs) = &eq.lhs {
        let lhs_expr = Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: eq.span,
        };
        return linear_terms(dae, &sub_expr(lhs_expr, eq.rhs.clone(), eq.span));
    }
    linear_terms(dae, &eq.rhs)
}

fn expression_row_span(expr: &Expression) -> Span {
    match expr {
        Expression::VarRef { name, span, .. } => {
            if !span.is_dummy() {
                *span
            } else {
                name.span().unwrap_or(*span)
            }
        }
        Expression::Binary { span, .. }
        | Expression::Unary { span, .. }
        | Expression::BuiltinCall { span, .. }
        | Expression::FunctionCall { span, .. }
        | Expression::Literal { span, .. }
        | Expression::If { span, .. }
        | Expression::Array { span, .. }
        | Expression::Tuple { span, .. }
        | Expression::Range { span, .. }
        | Expression::ArrayComprehension { span, .. }
        | Expression::Index { span, .. }
        | Expression::FieldAccess { span, .. }
        | Expression::Empty { span } => *span,
    }
}

fn candidate_from_state_constraint(
    dae: &Dae,
    row: &LinearRow,
    state_names: &[VarName],
    state_name_set: &HashSet<String>,
    when_assigned_states: &HashSet<String>,
) -> Option<VarName> {
    let mut candidates = row
        .state_terms(state_name_set)
        .filter(|name| !when_assigned_states.contains(*name))
        .filter_map(|name| {
            let state_name = VarName::new(name.to_string());
            dae.variables
                .states
                .get(&state_name)
                .filter(|state| state.state_select != rumoca_core::StateSelect::Always)
                .map(|state| (state_name, state.state_select))
        })
        .collect::<Vec<_>>();
    if candidates.len() < 2 {
        return None;
    }

    candidates.sort_by(|(a_name, a_select), (b_name, b_select)| {
        state_select_rank(*a_select)
            .cmp(&state_select_rank(*b_select))
            .then_with(|| a_name.as_str().cmp(b_name.as_str()))
    });
    let candidate = candidates.into_iter().next()?.0;
    state_names.contains(&candidate).then_some(candidate)
}

fn var_expr(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn real_expr(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn neg_expr(rhs: Expression, span: Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(rhs),
        span,
    }
}

fn mul_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn div_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn linear_term_expr(name: &str, coeff: f64, span: Span) -> Expression {
    let term = var_expr(name, span);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        term
    } else {
        mul_expr(real_expr(coeff, span), term, span)
    }
}

fn sum_linear_terms<'a>(
    terms: impl Iterator<Item = (&'a String, &'a f64)>,
    span: Span,
) -> Expression {
    terms
        .map(|(name, coeff)| linear_term_expr(name, *coeff, span))
        .reduce(|lhs, rhs| add_expr(lhs, rhs, span))
        .unwrap_or_else(|| real_expr(0.0, span))
}

fn solve_linear_row_for_state(row: &LinearRow, state_name: &VarName) -> Option<Expression> {
    let coeff = row.coefficient(state_name.as_str());
    if coeff.abs() <= LINEAR_EPSILON {
        return None;
    }
    let remainder = sum_linear_terms(
        row.terms
            .iter()
            .filter(|(name, _)| name.as_str() != state_name.as_str()),
        row.span,
    );
    let numerator = neg_expr(remainder, row.span);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        Some(numerator)
    } else {
        Some(div_expr(numerator, real_expr(coeff, row.span), row.span))
    }
}

fn canonical_singleton_state_term(dae: &Dae, name: &str) -> Option<String> {
    let scalar = rumoca_core::parse_scalar_name(name)?;
    let state = dae.variables.states.get(&VarName::new(scalar.base))?;
    (state.size() == 1).then(|| scalar.base.to_string())
}

fn canonicalize_singleton_state_terms(dae: &Dae, row: &LinearRow) -> LinearRow {
    let mut canonical = LinearRow::new(row.span);
    for (name, coeff) in &row.terms {
        let term_name = canonical_singleton_state_term(dae, name).unwrap_or_else(|| name.clone());
        canonical.add_term(term_name, *coeff);
    }
    canonical.structural_params = row.structural_params.clone();
    canonical
}

fn linear_constraint_dummy_state_definitions(
    dae: &Dae,
    state_names: &[VarName],
    state_name_set: &HashSet<String>,
    when_assigned_states: &HashSet<String>,
) -> Vec<(VarName, ConstrainedDummyDefinition)> {
    let non_state_unknown_names = continuous_non_state_unknown_names(dae);
    let mut rows = dae
        .continuous
        .equations
        .iter()
        .filter_map(|eq| equation_linear_row(dae, eq))
        .map(|row| canonicalize_singleton_state_terms(dae, &row))
        .filter(|row| !row.terms.is_empty())
        .collect::<Vec<_>>();
    let non_state_names = sorted_non_state_names(&rows, state_name_set, &non_state_unknown_names);

    for pivot_name in non_state_names {
        let Some(pivot_idx) = rows
            .iter()
            .position(|row| row.coefficient(&pivot_name).abs() > LINEAR_EPSILON)
        else {
            continue;
        };
        let mut pivot = rows.remove(pivot_idx);
        let pivot_coeff = pivot.coefficient(&pivot_name);
        pivot.scale(1.0 / pivot_coeff);

        for row in &mut rows {
            let coeff = row.coefficient(&pivot_name);
            if coeff.abs() > LINEAR_EPSILON {
                row.add_scaled(&pivot, -coeff);
            }
        }
    }

    rows.iter()
        .filter(|row| !row.contains_continuous_non_state(state_name_set, &non_state_unknown_names))
        .filter_map(|row| {
            let candidate = candidate_from_state_constraint(
                dae,
                row,
                state_names,
                state_name_set,
                when_assigned_states,
            )?;
            let expr = solve_linear_row_for_state(row, &candidate)?;
            Some((
                candidate,
                ConstrainedDummyDefinition {
                    defining_expr: expr,
                    structural_params: row.structural_params.iter().cloned().collect(),
                },
            ))
        })
        .collect()
}

fn continuous_non_state_unknown_names(dae: &Dae) -> HashSet<String> {
    dae.variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .map(|name| name.as_str().to_string())
        .collect()
}

fn sorted_non_state_names(
    rows: &[LinearRow],
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
) -> Vec<String> {
    let mut names = rows
        .iter()
        .flat_map(|row| row.terms.keys())
        .filter(|name| {
            !state_name_set.contains(name.as_str())
                && non_state_unknown_names.contains(name.as_str())
        })
        .cloned()
        .collect::<Vec<_>>();
    names.sort();
    names.dedup();
    names
}

/// Identify state variables that are constrained dummy-state candidates.
///
/// This detects direct state constraints such as `x1 = -x2 - x3`, including
/// constraints that become visible through linear connector/current aliases.
/// The result is ordered by state selection and name, and is used both for
/// simulation metadata and constrained dummy-derivative reduction.
pub fn constrained_dummy_state_names(dae: &Dae) -> IndexSet<String> {
    constrained_dummy_state_defining_exprs(dae)
        .keys()
        .cloned()
        .collect()
}

/// One constrained dummy-state definition: the expression that defines the
/// candidate state, plus the parameters whose compile-time values were baked
/// into it by the linear constraint reduction.
#[derive(Debug, Clone)]
pub struct ConstrainedDummyDefinition {
    pub defining_expr: Expression,
    pub structural_params: Vec<String>,
}

pub fn constrained_dummy_state_defining_exprs(
    dae: &Dae,
) -> IndexMap<String, ConstrainedDummyDefinition> {
    let Some((state_names, state_name_set, when_assigned_states)) =
        direct_demotion_round_context(dae)
    else {
        return IndexMap::new();
    };

    let mut definitions = dae
        .continuous
        .equations
        .iter()
        .filter_map(|eq| {
            let candidate = direct_dummy_state_candidate(
                dae,
                eq,
                &state_names,
                &state_name_set,
                &when_assigned_states,
            )?;
            let (state_name, defining_expr) =
                extract_state_direct_assignment_equation(eq, &state_names, &state_name_set)?;
            (state_name == candidate).then_some((
                candidate,
                ConstrainedDummyDefinition {
                    defining_expr,
                    structural_params: Vec::new(),
                },
            ))
        })
        .collect::<Vec<_>>();
    definitions.extend(linear_constraint_dummy_state_definitions(
        dae,
        &state_names,
        &state_name_set,
        &when_assigned_states,
    ));
    // A state with its own assignable `der(state) = ...` row genuinely
    // integrates unless the defining constraint couples it to another state.
    // Filter that case here, at constrained-dummy classification time, so
    // metadata (`constrained_dummy_state_names`) and actual demotion agree.
    definitions.retain(|(state_name, definition)| {
        !candidate_is_self_integrating_non_state_alias(dae, state_name, definition, &state_names)
    });

    definitions.sort_by(|(a, _), (b, _)| {
        let a_var = &dae.variables.states[a];
        let b_var = &dae.variables.states[b];
        state_select_rank(a_var.state_select)
            .cmp(&state_select_rank(b_var.state_select))
            .then_with(|| a.as_str().cmp(b.as_str()))
    });
    let mut result = IndexMap::new();
    for (name, expr) in definitions {
        result.entry(name.as_str().to_string()).or_insert(expr);
    }
    result
}
