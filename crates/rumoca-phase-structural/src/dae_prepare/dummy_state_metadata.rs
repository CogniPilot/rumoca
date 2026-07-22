use std::collections::{BTreeSet, HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_dae::expr_contains_var;

use super::{
    Dae, DefiningExprIndex, Equation, Expression, Literal, OpBinary, OpUnary, Span, Subscript,
    VarName, add_expr, choose_exact_alias_state_representative,
    collect_non_derivative_defining_expr_index, collect_non_state_continuous_unknown_names,
    direct_demotion_round_context, expression_contains_any_der_call,
    extract_state_direct_assignment_equation, state_select_rank, sub_expr,
    try_extract_derivative_alias,
};

const LINEAR_EPSILON: f64 = 1.0e-12;

#[derive(Debug, Clone)]
struct LinearRow {
    span: Span,
    terms: IndexMap<VarName, f64>,
    /// Parameters whose compile-time values were baked into the coefficients.
    /// When a demotion derived from this row is applied, these must be pinned
    /// as structural (no longer runtime-tunable) or a tuned value would
    /// silently disagree with the baked coefficient.
    structural_params: BTreeSet<VarName>,
}

impl LinearRow {
    fn new(span: Span) -> Self {
        Self {
            span,
            terms: IndexMap::new(),
            structural_params: BTreeSet::new(),
        }
    }

    fn add_term(&mut self, name: VarName, coeff: f64) {
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

    fn coefficient(&self, name: &VarName) -> f64 {
        self.terms.get(name).copied().unwrap_or(0.0)
    }

    fn contains_continuous_non_state(
        &self,
        state_components: &IndexMap<VarName, VarName>,
        non_state_unknown_names: &HashSet<VarName>,
    ) -> bool {
        self.terms.keys().any(|name| {
            !state_components.contains_key(name) && non_state_unknown_names.contains(name)
        })
    }

    fn state_terms<'a>(
        &'a self,
        state_components: &'a IndexMap<VarName, VarName>,
    ) -> impl Iterator<Item = &'a VarName> + 'a {
        self.terms
            .keys()
            .filter(|name| state_components.contains_key(*name))
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
    Some(state_name)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateDependency {
    None,
    OtherState,
    Cycle,
}

impl StateDependency {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::OtherState, _) | (_, Self::OtherState) => Self::OtherState,
            (Self::Cycle, _) | (_, Self::Cycle) => Self::Cycle,
            (Self::None, Self::None) => Self::None,
        }
    }
}

struct StateDependencyClosure {
    definitions: DefiningExprIndex,
    states: HashSet<VarName>,
    non_states: HashSet<String>,
}

impl StateDependencyClosure {
    fn new(dae: &Dae) -> Self {
        Self {
            definitions: collect_non_derivative_defining_expr_index(dae),
            states: dae.variables.states.keys().cloned().collect(),
            non_states: collect_non_state_continuous_unknown_names(dae),
        }
    }

    fn classify(
        &self,
        expr: &Expression,
        candidate: &VarName,
        excluded_equation: usize,
    ) -> StateDependency {
        self.classify_expr(
            expr,
            candidate,
            excluded_equation,
            &mut HashSet::new(),
            &mut HashMap::new(),
        )
    }

    fn classify_expr(
        &self,
        expr: &Expression,
        candidate: &VarName,
        excluded_equation: usize,
        active: &mut HashSet<VarName>,
        complete: &mut HashMap<VarName, StateDependency>,
    ) -> StateDependency {
        let mut refs = Vec::new();
        expr.collect_var_refs(&mut refs);
        refs.sort_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
        refs.dedup();

        let mut result = StateDependency::None;
        for name in refs {
            result = result.merge(self.classify_reference(
                &name,
                candidate,
                excluded_equation,
                active,
                complete,
            ));
            if result == StateDependency::OtherState {
                return result;
            }
        }
        result
    }

    fn classify_reference(
        &self,
        name: &VarName,
        candidate: &VarName,
        excluded_equation: usize,
        active: &mut HashSet<VarName>,
        complete: &mut HashMap<VarName, StateDependency>,
    ) -> StateDependency {
        if self.states.contains(name) {
            return if name == candidate {
                StateDependency::None
            } else {
                StateDependency::OtherState
            };
        }
        self.classify_non_state(name, candidate, excluded_equation, active, complete)
    }

    fn classify_non_state(
        &self,
        name: &VarName,
        candidate: &VarName,
        excluded_equation: usize,
        active: &mut HashSet<VarName>,
        complete: &mut HashMap<VarName, StateDependency>,
    ) -> StateDependency {
        if !self.non_states.contains(name.as_str()) {
            return StateDependency::None;
        }
        if let Some(result) = complete.get(name) {
            return *result;
        }
        if !active.insert(name.clone()) {
            return StateDependency::Cycle;
        }

        let mut result = StateDependency::None;
        for definition in self
            .definitions
            .get(name.as_str())
            .into_iter()
            .flatten()
            .filter(|definition| definition.equation_index != excluded_equation)
        {
            result = result.merge(self.classify_expr(
                &definition.expr,
                candidate,
                excluded_equation,
                active,
                complete,
            ));
            if result == StateDependency::OtherState {
                break;
            }
        }
        active.remove(name);
        complete.insert(name.clone(), result);
        result
    }
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

fn scalar_var_name(dae: &Dae, name: &VarName, subscripts: &[Subscript]) -> Option<VarName> {
    if !subscripts.is_empty()
        && dae
            .variables
            .states
            .get(name)
            .is_some_and(|state| state.size() == 1)
    {
        let state = &dae.variables.states[name];
        return Some(rumoca_ir_dae::scalar_name_for_flat_index(
            name,
            &state.dims,
            0,
        ));
    }
    crate::scalarize::scalarization_var_ref_name(name, subscripts).map(VarName::new)
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
        Expression::Binary { op, lhs, rhs, .. } => linear_binary_terms(dae, op, lhs, rhs),
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones,
            ..
        } => Some(LinearRow::new(expression_row_span(expr))),
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            ..
        } => {
            let mut structural_params = BTreeSet::new();
            structural_numeric_constant(dae, args.first()?, &mut structural_params)?;
            let mut row = LinearRow::new(expression_row_span(expr));
            row.structural_params = structural_params;
            Some(row)
        }
        Expression::Index { base, .. }
            if matches!(
                base.as_ref(),
                Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Zeros
                        | rumoca_core::BuiltinFunction::Ones
                        | rumoca_core::BuiltinFunction::Fill,
                    ..
                }
            ) =>
        {
            linear_terms(dae, base)
        }
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

fn linear_binary_terms(
    dae: &Dae,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
) -> Option<LinearRow> {
    match op {
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
            let (mut row, scale) =
                if let Some(scale) = structural_numeric_constant(dae, lhs, &mut used) {
                    (linear_terms(dae, rhs)?, scale)
                } else {
                    used.clear();
                    (
                        linear_terms(dae, lhs)?,
                        structural_numeric_constant(dae, rhs, &mut used)?,
                    )
                };
            row.scale(scale);
            row.structural_params.extend(used);
            Some(row)
        }
        OpBinary::Div | OpBinary::DivElem => {
            let mut used = BTreeSet::new();
            let scale = structural_numeric_constant(dae, rhs, &mut used)?;
            if scale.abs() <= LINEAR_EPSILON {
                return None;
            }
            let mut row = linear_terms(dae, lhs)?;
            row.scale(1.0 / scale);
            row.structural_params.extend(used);
            Some(row)
        }
        _ => None,
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
    used_params: &mut BTreeSet<VarName>,
) -> Option<f64> {
    structural_numeric_constant_inner(dae, expr, used_params, &mut HashSet::new())
}

fn structural_numeric_constant_inner(
    dae: &Dae,
    expr: &Expression,
    used_params: &mut BTreeSet<VarName>,
    visiting: &mut HashSet<VarName>,
) -> Option<f64> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let var_name = name.var_name();
            if !visiting.insert(var_name.clone()) {
                return None;
            }
            let is_parameter = dae.variables.parameters.contains_key(var_name);
            let var = dae
                .variables
                .parameters
                .get(var_name)
                .or_else(|| dae.variables.constants.get(var_name))?;
            let start = var.start.as_ref()?;
            let value = structural_numeric_constant_inner(dae, start, used_params, visiting)?;
            visiting.remove(var_name);
            if is_parameter {
                used_params.insert(var_name.clone());
            }
            Some(value)
        }
        Expression::Unary { op, rhs, .. } => {
            let value = structural_numeric_constant_inner(dae, rhs, used_params, visiting)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = structural_numeric_constant_inner(dae, lhs, used_params, visiting)?;
            let rhs = structural_numeric_constant_inner(dae, rhs, used_params, visiting)?;
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

fn equation_residual_expression(eq: &Equation) -> Expression {
    let Some(lhs) = &eq.lhs else {
        return eq.rhs.clone();
    };
    let lhs_expr = Expression::VarRef {
        name: lhs.clone(),
        subscripts: Vec::new(),
        span: eq.span,
    };
    sub_expr(lhs_expr, eq.rhs.clone(), eq.span)
}

fn scalar_linear_rows(dae: &Dae) -> Result<Vec<LinearRow>, crate::StructuralError> {
    let scalarization = crate::scalarize::build_expression_scalarization_context(dae)?;
    let mut rows = Vec::new();
    for equation in &dae.continuous.equations {
        let residual = equation_residual_expression(equation);
        if expression_contains_any_der_call(&residual) {
            continue;
        }
        for scalar in crate::scalarize::scalarize_expression_rows(
            &residual,
            equation.scalar_count,
            &scalarization,
        )? {
            if let Some(row) = linear_terms(dae, &scalar)
                && !row.terms.is_empty()
            {
                rows.push(row);
            }
        }
    }
    Ok(rows)
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
    state_components: &IndexMap<VarName, VarName>,
    when_assigned_states: &HashSet<String>,
) -> Option<(VarName, VarName)> {
    let state_term_count = row.state_terms(state_components).count();
    let mut candidates = row
        .state_terms(state_components)
        .filter_map(|component_name| {
            let state_name = state_components.get(component_name)?;
            if when_assigned_states.contains(state_name.as_str()) {
                return None;
            }
            dae.variables
                .states
                .get(state_name)
                .filter(|state| state.state_select != rumoca_core::StateSelect::Always)
                .map(|state| {
                    (
                        component_name.clone(),
                        state_name.clone(),
                        state.state_select,
                    )
                })
        })
        .collect::<Vec<_>>();
    if candidates.is_empty() || (state_term_count > 1 && candidates.len() < 2) {
        return None;
    }

    candidates.sort_by(
        |(a_component, a_name, a_select), (b_component, b_name, b_select)| {
            state_select_rank(*a_select)
                .cmp(&state_select_rank(*b_select))
                .then_with(|| a_name.as_str().cmp(b_name.as_str()))
                .then_with(|| a_component.as_str().cmp(b_component.as_str()))
        },
    );
    let (component_name, state_name, _) = candidates.into_iter().next()?;
    Some((component_name, state_name))
}

fn var_expr(name: &VarName, span: Span) -> Expression {
    let (reference, subscripts) = rumoca_core::component_reference_from_flat_name(name, span)
        .map(|mut component_ref| {
            let subscripts = component_ref
                .parts
                .last_mut()
                .map(|part| std::mem::take(&mut part.subs))
                .unwrap_or_default();
            (
                rumoca_core::Reference::from_component_reference(component_ref),
                subscripts,
            )
        })
        .unwrap_or_else(|| {
            (
                rumoca_core::Reference::from_var_name(name.clone()),
                Vec::new(),
            )
        });
    Expression::VarRef {
        name: reference,
        subscripts,
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

fn linear_term_expr(name: &VarName, coeff: f64, span: Span) -> Expression {
    let term = var_expr(name, span);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        term
    } else {
        mul_expr(real_expr(coeff, span), term, span)
    }
}

fn sum_linear_terms<'a>(
    terms: impl Iterator<Item = (&'a VarName, &'a f64)>,
    span: Span,
) -> Expression {
    terms
        .map(|(name, coeff)| linear_term_expr(name, *coeff, span))
        .reduce(|lhs, rhs| add_expr(lhs, rhs, span))
        .unwrap_or_else(|| real_expr(0.0, span))
}

fn solve_linear_row_for_state(row: &LinearRow, state_name: &VarName) -> Option<Expression> {
    let coeff = row.coefficient(state_name);
    if coeff.abs() <= LINEAR_EPSILON {
        return None;
    }
    let remainder = sum_linear_terms(
        row.terms.iter().filter(|(name, _)| *name != state_name),
        row.span,
    );
    let numerator = neg_expr(remainder, row.span);
    if (coeff - 1.0).abs() <= LINEAR_EPSILON {
        Some(numerator)
    } else {
        Some(div_expr(numerator, real_expr(coeff, row.span), row.span))
    }
}

fn linear_constraint_dummy_state_definitions(
    dae: &Dae,
    when_assigned_states: &HashSet<String>,
) -> Result<Vec<(VarName, ConstrainedDummyDefinition)>, crate::StructuralError> {
    let state_components = scalar_component_bases(&dae.variables.states);
    let non_state_unknown_names = continuous_non_state_unknown_names(dae);
    let mut rows = scalar_linear_rows(dae)?;
    let non_state_names =
        sorted_non_state_names(&rows, &state_components, &non_state_unknown_names);

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

    let mut definitions = IndexMap::<VarName, ConstrainedDummyDefinition>::new();
    for row in rows.iter().filter(|row| {
        !row.contains_continuous_non_state(&state_components, &non_state_unknown_names)
    }) {
        let Some((component_name, state_name)) =
            candidate_from_state_constraint(dae, row, &state_components, when_assigned_states)
        else {
            continue;
        };
        let Some(expr) = solve_linear_row_for_state(row, &component_name) else {
            continue;
        };
        let definition = definitions.entry(state_name).or_default();
        definition
            .component_defining_exprs
            .entry(component_name)
            .or_insert(expr);
        definition
            .structural_params
            .extend(row.structural_params.iter().cloned());
    }

    definitions.retain(|state_name, definition| {
        let Some(state) = dae.variables.states.get(state_name) else {
            return false;
        };
        let expected = scalar_names_for_variable(state_name, state);
        expected.len() == definition.component_defining_exprs.len()
            && expected
                .iter()
                .all(|name| definition.component_defining_exprs.contains_key(name))
    });
    Ok(definitions.into_iter().collect())
}

fn scalar_names_for_variable(name: &VarName, var: &rumoca_ir_dae::Variable) -> Vec<VarName> {
    if var.dims.is_empty() {
        return vec![name.clone()];
    }
    (0..var.size())
        .map(|flat_index| rumoca_ir_dae::scalar_name_for_flat_index(name, &var.dims, flat_index))
        .collect()
}

fn scalar_component_bases(
    variables: &IndexMap<VarName, rumoca_ir_dae::Variable>,
) -> IndexMap<VarName, VarName> {
    let mut components = IndexMap::new();
    for (name, var) in variables {
        for component_name in scalar_names_for_variable(name, var) {
            components.insert(component_name, name.clone());
        }
    }
    components
}

fn continuous_non_state_unknown_names(dae: &Dae) -> HashSet<VarName> {
    scalar_component_bases(&dae.variables.algebraics)
        .into_keys()
        .chain(scalar_component_bases(&dae.variables.outputs).into_keys())
        .collect()
}

fn sorted_non_state_names(
    rows: &[LinearRow],
    state_components: &IndexMap<VarName, VarName>,
    non_state_unknown_names: &HashSet<VarName>,
) -> Vec<VarName> {
    let mut names = rows
        .iter()
        .flat_map(|row| row.terms.keys())
        .filter(|name| {
            !state_components.contains_key(*name) && non_state_unknown_names.contains(*name)
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
pub fn constrained_dummy_state_names(
    dae: &Dae,
) -> Result<IndexSet<String>, crate::StructuralError> {
    let mut names = IndexSet::new();
    for (state_name, definition) in constrained_dummy_state_defining_exprs(dae)? {
        if super::constrained_dummy_derivative_plan_for_definition(dae, &state_name, &definition)?
            .is_some()
        {
            names.insert(state_name.as_str().to_string());
        }
    }
    Ok(names)
}

/// One constrained dummy-state definition: the expression that defines the
/// candidate state, plus the parameters whose compile-time values were baked
/// into it by the linear constraint reduction.
#[derive(Debug, Clone, Default)]
pub struct ConstrainedDummyDefinition {
    pub component_defining_exprs: IndexMap<VarName, Expression>,
    pub aggregate_defining_expr: Option<Expression>,
    pub structural_params: BTreeSet<VarName>,
}

fn direct_dummy_definition(
    dae: &Dae,
    state_name: VarName,
    defining_expr: Expression,
    scalarization: &crate::scalarize::ExpressionScalarizationContext,
) -> Result<Option<ConstrainedDummyDefinition>, crate::StructuralError> {
    let Some(state) = dae.variables.states.get(&state_name) else {
        return Ok(None);
    };
    let component_defining_exprs = if state.dims.is_empty() {
        IndexMap::from_iter([(state_name, defining_expr.clone())])
    } else {
        if super::row_shape::expression_dims_for_row_count(dae, &defining_expr)?
            != Some(state.dims.clone())
        {
            return Ok(None);
        }
        let rows = crate::scalarize::scalarize_expression_rows(
            &defining_expr,
            state.size(),
            scalarization,
        )?;
        if rows.len() != state.size() {
            return Ok(None);
        }
        rows.into_iter()
            .enumerate()
            .map(|(flat_index, expr)| {
                (
                    rumoca_ir_dae::scalar_name_for_flat_index(&state_name, &state.dims, flat_index),
                    expr,
                )
            })
            .collect()
    };
    Ok(Some(ConstrainedDummyDefinition {
        component_defining_exprs,
        aggregate_defining_expr: Some(defining_expr),
        structural_params: BTreeSet::new(),
    }))
}

fn duplicate_state_derivative_alias_definitions(
    dae: &Dae,
    scalarization: &crate::scalarize::ExpressionScalarizationContext,
) -> Result<Vec<(VarName, ConstrainedDummyDefinition)>, crate::StructuralError> {
    let mut aliases_by_derivative = IndexMap::<VarName, IndexMap<VarName, Span>>::new();
    for derivative_state in dae.variables.states.keys() {
        for equation in &dae.continuous.equations {
            let Some(alias) = try_extract_derivative_alias(equation, derivative_state) else {
                continue;
            };
            if alias == *derivative_state || !dae.variables.states.contains_key(&alias) {
                continue;
            }
            aliases_by_derivative
                .entry(derivative_state.clone())
                .or_default()
                .entry(alias)
                .or_insert(equation.span);
        }
    }

    let mut definitions = Vec::new();
    for aliases in aliases_by_derivative.values() {
        if aliases.len() < 2 {
            continue;
        }
        let alias_names = aliases.keys().cloned().collect::<Vec<_>>();
        let Some(canonical) = choose_exact_alias_state_representative(dae, &alias_names) else {
            continue;
        };
        for (alias, span) in aliases {
            if alias == canonical
                || dae
                    .variables
                    .states
                    .get(alias)
                    .is_some_and(|state| state.state_select == rumoca_core::StateSelect::Always)
            {
                continue;
            }
            let Some(definition) = direct_dummy_definition(
                dae,
                alias.clone(),
                var_expr(canonical, *span),
                scalarization,
            )?
            else {
                continue;
            };
            definitions.push((alias.clone(), definition));
        }
    }
    Ok(definitions)
}

pub fn constrained_dummy_state_defining_exprs(
    dae: &Dae,
) -> Result<IndexMap<VarName, ConstrainedDummyDefinition>, crate::StructuralError> {
    let Some((state_names, state_name_set, when_assigned_states)) =
        direct_demotion_round_context(dae)
    else {
        return Ok(IndexMap::new());
    };

    let scalarization = crate::scalarize::build_expression_scalarization_context(dae)?;
    let state_dependency = StateDependencyClosure::new(dae);
    let mut definitions = Vec::new();
    for (equation_index, equation) in dae.continuous.equations.iter().enumerate() {
        let Some(candidate) = direct_dummy_state_candidate(
            dae,
            equation,
            &state_names,
            &state_name_set,
            &when_assigned_states,
        ) else {
            continue;
        };
        let Some((state_name, defining_expr)) =
            extract_state_direct_assignment_equation(equation, &state_names, &state_name_set)
        else {
            continue;
        };
        if state_name != candidate {
            continue;
        }
        let reaches_other_state =
            state_dependency.classify(&defining_expr, &candidate, equation_index)
                == StateDependency::OtherState;
        let Some(definition) =
            direct_dummy_definition(dae, candidate.clone(), defining_expr, &scalarization)?
        else {
            continue;
        };
        let has_closed_derivative = !reaches_other_state
            && super::constrained_dummy_derivative_plan_for_definition(
                dae,
                &candidate,
                &definition,
            )?
            .is_some();
        if !reaches_other_state && !has_closed_derivative {
            continue;
        }
        definitions.push((candidate, definition));
    }
    definitions.extend(linear_constraint_dummy_state_definitions(
        dae,
        &when_assigned_states,
    )?);
    definitions.extend(duplicate_state_derivative_alias_definitions(
        dae,
        &scalarization,
    )?);

    definitions.sort_by(|(a, _), (b, _)| {
        let a_var = &dae.variables.states[a];
        let b_var = &dae.variables.states[b];
        state_select_rank(a_var.state_select)
            .cmp(&state_select_rank(b_var.state_select))
            .then_with(|| a.as_str().cmp(b.as_str()))
    });
    let mut result = IndexMap::new();
    for (name, definition) in definitions {
        insert_preferred_dummy_definition(dae, &mut result, name, definition)?;
    }
    Ok(result)
}

fn insert_preferred_dummy_definition(
    dae: &Dae,
    definitions: &mut IndexMap<VarName, ConstrainedDummyDefinition>,
    name: VarName,
    candidate: ConstrainedDummyDefinition,
) -> Result<(), crate::StructuralError> {
    let Some(existing) = definitions.get_mut(&name) else {
        definitions.insert(name, candidate);
        return Ok(());
    };
    let existing_is_viable =
        super::constrained_dummy_derivative_plan_for_definition(dae, &name, existing)?.is_some();
    if !existing_is_viable
        && super::constrained_dummy_derivative_plan_for_definition(dae, &name, &candidate)?
            .is_some()
    {
        *existing = candidate;
    }
    Ok(())
}
