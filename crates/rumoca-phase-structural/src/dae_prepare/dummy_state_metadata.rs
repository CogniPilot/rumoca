use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_dae::expr_contains_var;

use super::{
    Dae, Equation, Expression, Literal, OpBinary, OpUnary, Subscript, VarName, add_expr,
    direct_demotion_round_context, expression_contains_any_der_call,
    extract_state_direct_assignment_equation, state_select_rank, sub_expr,
};

const LINEAR_EPSILON: f64 = 1.0e-12;

#[derive(Debug, Clone, Default)]
struct LinearRow {
    terms: IndexMap<String, f64>,
}

impl LinearRow {
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
        for (name, coeff) in &other.terms {
            self.add_term(name.clone(), coeff * factor);
        }
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
        Expression::Literal { .. } => Some(LinearRow::default()),
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut row = LinearRow::default();
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
                if let Some(scale) = numeric_constant(lhs) {
                    let mut row = linear_terms(dae, rhs)?;
                    row.scale(scale);
                    Some(row)
                } else if let Some(scale) = numeric_constant(rhs) {
                    let mut row = linear_terms(dae, lhs)?;
                    row.scale(scale);
                    Some(row)
                } else {
                    None
                }
            }
            OpBinary::Div | OpBinary::DivElem => {
                let scale = numeric_constant(rhs)?;
                if scale.abs() <= LINEAR_EPSILON {
                    return None;
                }
                let mut row = linear_terms(dae, lhs)?;
                row.scale(1.0 / scale);
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

fn equation_linear_row(dae: &Dae, eq: &Equation) -> Option<LinearRow> {
    if expression_contains_any_der_call(&eq.rhs) {
        return None;
    }
    if let Some(lhs) = &eq.lhs {
        let lhs_expr = Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        return linear_terms(dae, &sub_expr(lhs_expr, eq.rhs.clone()));
    }
    linear_terms(dae, &eq.rhs)
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

fn var_expr(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn neg_expr(rhs: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn mul_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn div_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn linear_term_expr(name: &str, coeff: f64) -> Expression {
    let term = var_expr(name);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        term
    } else {
        mul_expr(real_expr(coeff), term)
    }
}

fn sum_linear_terms<'a>(terms: impl Iterator<Item = (&'a String, &'a f64)>) -> Expression {
    terms
        .map(|(name, coeff)| linear_term_expr(name, *coeff))
        .reduce(add_expr)
        .unwrap_or_else(|| real_expr(0.0))
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
    );
    let numerator = neg_expr(remainder);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        Some(numerator)
    } else {
        Some(div_expr(numerator, real_expr(coeff)))
    }
}

fn canonical_singleton_state_term(dae: &Dae, name: &str) -> Option<String> {
    let scalar = rumoca_core::parse_scalar_name(name)?;
    let state = dae.variables.states.get(&VarName::new(scalar.base))?;
    (state.size() == 1).then(|| scalar.base.to_string())
}

fn canonicalize_singleton_state_terms(dae: &Dae, row: &LinearRow) -> LinearRow {
    let mut canonical = LinearRow::default();
    for (name, coeff) in &row.terms {
        let term_name = canonical_singleton_state_term(dae, name).unwrap_or_else(|| name.clone());
        canonical.add_term(term_name, *coeff);
    }
    canonical
}

fn linear_constraint_dummy_state_definitions(
    dae: &Dae,
    state_names: &[VarName],
    state_name_set: &HashSet<String>,
    when_assigned_states: &HashSet<String>,
) -> Vec<(VarName, Expression)> {
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
            Some((candidate, expr))
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

pub fn constrained_dummy_state_defining_exprs(dae: &Dae) -> IndexMap<String, Expression> {
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
            (state_name == candidate).then_some((candidate, defining_expr))
        })
        .collect::<Vec<_>>();
    definitions.extend(linear_constraint_dummy_state_definitions(
        dae,
        &state_names,
        &state_name_set,
        &when_assigned_states,
    ));

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
