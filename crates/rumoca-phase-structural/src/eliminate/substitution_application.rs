use std::collections::HashMap;

use rumoca_core::{ExpressionRewriter, Literal, OpUnary};

use super::{
    Dae, Expression, OpBinary, Reference, Substitution, SubstitutionApplicationPlan, VarName,
    apply_substitutions_to_expr_with_plan,
};
use crate::StructuralError;

pub(super) fn equation_analysis_expr(eq: &rumoca_ir_dae::Equation) -> Expression {
    let Some(lhs) = eq.lhs.as_ref() else {
        return eq.rhs.clone();
    };
    subtraction(
        Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: eq.span,
        },
        eq.rhs.clone(),
        eq.span,
    )
}

/// New contents computed for one equation while the DAE is still pristine.
struct RewrittenEquation {
    lhs: Option<Reference>,
    rhs: Expression,
}

pub(super) fn apply_substitutions_to_remaining_once(
    dae: &mut Dae,
    eliminated_eq_flags: &[bool],
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let plan = SubstitutionApplicationPlan::new(substitutions, aggregate_constructor.as_ref());
    // Symbolic derivative expansion reads the DAE as it stood before this pass
    // started. Computing every replacement while only a shared borrow is held
    // keeps that guarantee without snapshotting the whole DAE.
    let (rewritten, canonical_touched_families) =
        rewrite_remaining_equations(dae, eliminated_eq_flags, substitutions, &plan)?;
    let touched_equations = commit_rewritten_equations(dae, rewritten);
    super::drop_structured_families_touching_equations(
        dae,
        &touched_equations,
        &canonical_touched_families,
    )
}

fn rewrite_remaining_equations(
    dae: &Dae,
    eliminated_eq_flags: &[bool],
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<(Vec<Option<RewrittenEquation>>, Vec<usize>), StructuralError> {
    let canonical_touched_families =
        canonical_structured_families_touched_by_substitutions(dae, substitutions, plan)?;
    let mut derivative_replacements = DerivativeReplacementCache::new(dae);
    let mut rewritten = Vec::with_capacity(dae.continuous.equations.len());
    for (i, eq) in dae.continuous.equations.iter().enumerate() {
        let eliminated = eliminated_eq_flags.get(i).ok_or_else(|| {
            StructuralError::ContractViolation {
                reason: format!(
                    "eliminated equation flags have length {} but continuous equation {i} exists",
                    eliminated_eq_flags.len()
                ),
                span: eq.span,
            }
        })?;
        if *eliminated {
            rewritten.push(None);
            continue;
        }
        rewritten.push(Some(rewritten_equation(
            eq,
            substitutions,
            plan,
            &mut derivative_replacements,
        )?));
    }
    Ok((rewritten, canonical_touched_families))
}

fn rewritten_equation(
    eq: &rumoca_ir_dae::Equation,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
    derivative_replacements: &mut DerivativeReplacementCache<'_>,
) -> Result<RewrittenEquation, StructuralError> {
    let rhs = apply_substitutions_in_order_with_derivatives(
        &eq.rhs,
        substitutions,
        plan,
        derivative_replacements,
    )?;
    let Some(lhs) = eq.lhs.as_ref() else {
        return Ok(RewrittenEquation { lhs: None, rhs });
    };
    let lhs_expr = Expression::VarRef {
        name: lhs.clone(),
        subscripts: Vec::new(),
        span: eq.span,
    };
    let substituted_lhs = apply_substitutions_in_order_with_derivatives(
        &lhs_expr,
        substitutions,
        plan,
        derivative_replacements,
    )?;
    if substituted_lhs == lhs_expr {
        Ok(RewrittenEquation {
            lhs: eq.lhs.clone(),
            rhs,
        })
    } else {
        Ok(RewrittenEquation {
            lhs: None,
            rhs: subtraction(substituted_lhs, rhs, eq.span),
        })
    }
}

fn commit_rewritten_equations(
    dae: &mut Dae,
    rewritten: Vec<Option<RewrittenEquation>>,
) -> Vec<usize> {
    let mut touched = Vec::new();
    for (i, (eq, replacement)) in dae
        .continuous
        .equations
        .iter_mut()
        .zip(rewritten)
        .enumerate()
    {
        let Some(replacement) = replacement else {
            continue;
        };
        if eq.lhs != replacement.lhs || eq.rhs != replacement.rhs {
            touched.push(i);
        }
        eq.lhs = replacement.lhs;
        eq.rhs = replacement.rhs;
    }
    touched
}

pub(super) fn apply_substitutions_to_dae_partitions(
    dae: &mut Dae,
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let plan = SubstitutionApplicationPlan::new(substitutions, aggregate_constructor.as_ref());
    rewrite_dae_partitions(dae, substitutions, &plan)
}

pub(super) fn apply_aggregate_substitutions_to_dae_partitions(
    dae: &mut Dae,
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let plan = SubstitutionApplicationPlan::new(substitutions, aggregate_constructor.as_ref());
    // Only the aggregate/complex reconstruction groups replay here; the
    // sequential list is intentionally empty.
    rewrite_dae_partitions(dae, &[], &plan)
}

/// Failure atomicity: every partition is rewritten and the structured-family
/// contract is decided while only a shared borrow of `dae` is held, so any
/// error leaves the DAE exactly as it was found. Nothing is written back until
/// the whole rewrite is known to succeed.
fn rewrite_dae_partitions(
    dae: &mut Dae,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<(), StructuralError> {
    let (rewritten, surviving_families) = plan_partition_rewrite(dae, substitutions, plan)?;
    rewritten.commit(dae);
    if let Some(surviving_families) = surviving_families {
        dae.continuous.structured_equations = surviving_families;
    }
    Ok(())
}

/// Compute every partition's new contents plus the structured families that
/// survive, without touching `dae`.
///
/// Error ordering matches the pre-batching pass: the structured-family contract
/// is decided against the continuous rewrite *before* the initialization,
/// event and clock partitions are rewritten, so a family violation still
/// outranks a rewrite error raised by one of those later partitions.
fn plan_partition_rewrite(
    dae: &Dae,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<
    (
        RewrittenPartitions,
        Option<Vec<rumoca_ir_dae::StructuredEquationFamily>>,
    ),
    StructuralError,
> {
    let canonical_touched_families =
        canonical_structured_families_touched_by_substitutions(dae, substitutions, plan)?;
    let mut rewriter = SubstitutionDaeRewriter {
        substitutions,
        derivative_replacements: DerivativeReplacementCache::new(dae),
        plan,
    };
    let continuous = rewriter.rewrite_equations(&dae.continuous.equations)?;
    let touched_equations = changed_equation_indices(&dae.continuous.equations, &continuous);
    let surviving_families = super::structured_families_surviving_substitution(
        dae,
        &touched_equations,
        &canonical_touched_families,
    )?;
    let rewritten = rewriter.rewrite_remaining_partitions(dae, continuous)?;
    Ok((rewritten, surviving_families))
}

fn canonical_structured_families_touched_by_substitutions(
    dae: &Dae,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<Vec<usize>, StructuralError> {
    let mut touched = Vec::new();
    let mut derivative_replacements = DerivativeReplacementCache::new(dae);
    for (family_index, family) in dae.continuous.structured_equations.iter().enumerate() {
        if family.interiors_materialized {
            continue;
        }
        let Some(template) = &family.template else {
            // A non-materialized family without its canonical body has no
            // authority against which a derived-row rewrite can be checked.
            touched.push(family_index);
            continue;
        };
        let mut changed = false;
        for expression in &template.body {
            let rewritten =
                apply_substitutions_to_expr_with_plan(expression, substitutions, plan, |sub| {
                    derivative_replacements.replacement_for(sub)
                })?;
            if rewritten != *expression {
                changed = true;
                break;
            }
        }
        if changed {
            touched.push(family_index);
        }
    }
    Ok(touched)
}

/// Apply a cumulative substitution list whose plan is already built.
///
/// Callers that walk many equations against the same growing list must use
/// this entry point: rebuilding the plan per equation re-scans the function
/// table and deep-clones every replacement expression.
pub(super) fn apply_substitutions_in_order_with_plan(
    expr: &Expression,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<Expression, StructuralError> {
    let substituted =
        apply_substitutions_to_expr_with_plan(expr, substitutions, plan, |_| Ok(None))?;
    Ok(simplify_arithmetic_identities(substituted))
}

pub(super) fn apply_substitutions_to_expressions_in_order(
    dae: &Dae,
    expressions: &mut [Expression],
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let plan = SubstitutionApplicationPlan::new(substitutions, aggregate_constructor.as_ref());
    let mut derivative_replacements = DerivativeReplacementCache::new(dae);
    for expression in expressions {
        *expression = apply_substitutions_in_order_with_derivatives(
            expression,
            substitutions,
            &plan,
            &mut derivative_replacements,
        )?;
        *expression = simplify_arithmetic_identities(expression.clone());
    }
    Ok(())
}

fn apply_substitutions_in_order_with_derivatives(
    expr: &Expression,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
    derivative_replacements: &mut DerivativeReplacementCache<'_>,
) -> Result<Expression, StructuralError> {
    let substituted = apply_substitutions_to_expr_with_plan(expr, substitutions, plan, |sub| {
        derivative_replacements.replacement_for(sub)
    })?;
    Ok(simplify_arithmetic_identities(substituted))
}

pub(super) fn aggregate_constructor_reference(dae: &Dae) -> Option<rumoca_core::Reference> {
    let candidates = dae
        .symbols
        .functions
        .values()
        .filter(|function| {
            function.is_constructor
                && matches!(function.inputs.as_slice(), [re, im] if re.name == "re" && im.name == "im")
        })
        .collect::<Vec<_>>();
    let first = *candidates.first()?;
    if !candidates
        .iter()
        .all(|candidate| constructors_share_field_declarations(first, candidate))
    {
        return None;
    }
    // Derived operator-record types retain the source field DefIds. Pick the
    // shortest qualified constructor deterministically when every candidate
    // therefore proves it constructs the same inherited record value.
    let constructor = candidates.into_iter().min_by_key(|function| {
        (
            function.name.as_str().matches('.').count(),
            function.name.as_str().len(),
            function.name.as_str(),
        )
    })?;
    let instance_id = constructor.instance_id?;
    Some(
        rumoca_core::Reference::new(constructor.name.as_str()).with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id,
                base_part_count: 0,
            },
        ),
    )
}

fn constructors_share_field_declarations(
    lhs: &rumoca_core::Function,
    rhs: &rumoca_core::Function,
) -> bool {
    lhs.inputs.len() == rhs.inputs.len()
        && lhs.inputs.iter().zip(&rhs.inputs).all(|(lhs, rhs)| {
            lhs.name == rhs.name
                && lhs.def_id.is_some()
                && lhs.def_id == rhs.def_id
                && lhs.type_def_id == rhs.type_def_id
                && lhs.dims == rhs.dims
        })
}

/// Fold exact arithmetic identities introduced by substitution.
///
/// Substituting a literal 0 into `x - s_support` produces `x - 0`, which the
/// symbolic solver in `try_solve_for_unknown` cannot see through (it only
/// matches the unknown when it's the direct lhs/rhs of a top-level Sub). This
/// pass canonicalises the expression so equation residues like `x - (y - 0)`
/// reduce to the solvable `x - y` form.
///
/// Only handles identities that are mathematically exact across all numeric
/// types — division-by-zero and `0^0` are intentionally not folded.
pub(crate) fn simplify_arithmetic_identities(expr: Expression) -> Expression {
    ArithmeticIdentitySimplifier.rewrite_expression(&expr)
}

struct ArithmeticIdentitySimplifier;

impl rumoca_core::ExpressionRewriter for ArithmeticIdentitySimplifier {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        simplify_arithmetic_identity_root(self.walk_expression(expr))
    }
}

fn simplify_arithmetic_identity_root(expr: Expression) -> Expression {
    match expr {
        Expression::Binary { op, lhs, rhs, span } => {
            let lhs = *lhs;
            let rhs = *rhs;
            match op {
                OpBinary::Add => {
                    if is_numeric_zero(&lhs) {
                        return rhs;
                    }
                    if is_numeric_zero(&rhs) {
                        return lhs;
                    }
                }
                OpBinary::Sub => {
                    if lhs == rhs && identity_operand_is_total(&lhs) {
                        return zero_literal(span);
                    }
                    if is_numeric_zero(&rhs) {
                        return lhs;
                    }
                    if is_numeric_zero(&lhs) {
                        return negate(rhs, span);
                    }
                }
                OpBinary::Mul => {
                    if is_numeric_zero(&lhs) || is_numeric_zero(&rhs) {
                        return zero_literal(span);
                    }
                    if is_numeric_one(&lhs) {
                        return rhs;
                    }
                    if is_numeric_one(&rhs) {
                        return lhs;
                    }
                }
                OpBinary::Div if is_numeric_one(&rhs) => {
                    return lhs;
                }
                _ => {}
            }
            Expression::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            }
        }
        Expression::Unary { op, rhs, span } => {
            let inner = *rhs;
            if matches!(op, OpUnary::Minus) {
                // -(-x) → x
                if let Expression::Unary {
                    op: OpUnary::Minus,
                    rhs: inner_inner,
                    ..
                } = inner
                {
                    return *inner_inner;
                }
                // -0 → 0
                if is_numeric_zero(&inner) {
                    return inner;
                }
            }
            Expression::Unary {
                op,
                rhs: Box::new(inner),
                span,
            }
        }
        _ => expr,
    }
}

fn identity_operand_is_total(expr: &Expression) -> bool {
    match expr {
        Expression::Literal { .. } | Expression::VarRef { .. } => true,
        Expression::Unary { op, rhs, .. } => {
            matches!(op, OpUnary::Plus | OpUnary::Minus) && identity_operand_is_total(rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            matches!(op, OpBinary::Add | OpBinary::Sub)
                && identity_operand_is_total(lhs)
                && identity_operand_is_total(rhs)
        }
        _ => false,
    }
}

fn is_numeric_zero(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: Literal::Real(v),
            ..
        } if *v == 0.0
    ) || matches!(
        expr,
        Expression::Literal {
            value: Literal::Integer(0),
            ..
        }
    )
}

fn is_numeric_one(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: Literal::Real(v),
            ..
        } if *v == 1.0
    ) || matches!(
        expr,
        Expression::Literal {
            value: Literal::Integer(1),
            ..
        }
    )
}

fn negate(expr: Expression, span: rumoca_core::Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span,
    }
}

fn zero_literal(span: rumoca_core::Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

/// New contents for every DAE partition the substitution rewrite touches.
///
/// Computing all of them before any of them is written back keeps symbolic
/// derivative expansion anchored to the pre-pass DAE without cloning it.
#[derive(Default)]
struct RewrittenPartitions {
    continuous: Vec<RewrittenEquation>,
    initialization: Vec<RewrittenEquation>,
    discrete_real_updates: Vec<RewrittenEquation>,
    discrete_valued_updates: Vec<RewrittenEquation>,
    condition_equations: Vec<RewrittenEquation>,
    condition_relations: Vec<Expression>,
    synthetic_root_conditions: Vec<Expression>,
    event_action_conditions: Vec<Expression>,
    delay_channels: Vec<(Expression, Expression, Option<Expression>)>,
    clock_constructor_exprs: Vec<Expression>,
    clock_triggered_conditions: Vec<Expression>,
}

impl RewrittenPartitions {
    /// Write every partition back. Infallible by construction: all fallible
    /// work happened in [`plan_partition_rewrite`], before this point.
    fn commit(self, dae: &mut Dae) {
        commit_equations(&mut dae.continuous.equations, self.continuous);
        commit_equations(&mut dae.initialization.equations, self.initialization);
        commit_equations(&mut dae.discrete.real_updates, self.discrete_real_updates);
        commit_equations(
            &mut dae.discrete.valued_updates,
            self.discrete_valued_updates,
        );
        commit_equations(&mut dae.conditions.equations, self.condition_equations);
        commit_expressions(&mut dae.conditions.relations, self.condition_relations);
        commit_expressions(
            &mut dae.events.synthetic_root_conditions,
            self.synthetic_root_conditions,
        );
        for (action, condition) in dae
            .events
            .event_actions
            .iter_mut()
            .zip(self.event_action_conditions)
        {
            action.condition = condition;
        }
        for (channel, (source, delay_time, delay_max)) in dae
            .events
            .delay_channels
            .iter_mut()
            .zip(self.delay_channels)
        {
            channel.source = source;
            channel.delay_time = delay_time;
            channel.delay_max = delay_max;
        }
        commit_expressions(
            &mut dae.clocks.constructor_exprs,
            self.clock_constructor_exprs,
        );
        commit_expressions(
            &mut dae.clocks.triggered_conditions,
            self.clock_triggered_conditions,
        );
    }
}

fn commit_equations(equations: &mut [rumoca_ir_dae::Equation], rewritten: Vec<RewrittenEquation>) {
    for (equation, replacement) in equations.iter_mut().zip(rewritten) {
        equation.lhs = replacement.lhs;
        equation.rhs = replacement.rhs;
    }
}

/// Indices whose rewritten contents differ from the equations still in the DAE.
fn changed_equation_indices(
    equations: &[rumoca_ir_dae::Equation],
    rewritten: &[RewrittenEquation],
) -> Vec<usize> {
    equations
        .iter()
        .zip(rewritten)
        .enumerate()
        .filter_map(|(index, (equation, replacement))| {
            (equation.lhs != replacement.lhs || equation.rhs != replacement.rhs).then_some(index)
        })
        .collect()
}

fn commit_expressions(expressions: &mut [Expression], rewritten: Vec<Expression>) {
    for (expression, replacement) in expressions.iter_mut().zip(rewritten) {
        *expression = replacement;
    }
}

struct SubstitutionDaeRewriter<'a> {
    substitutions: &'a [Substitution],
    derivative_replacements: DerivativeReplacementCache<'a>,
    plan: &'a SubstitutionApplicationPlan,
}

impl SubstitutionDaeRewriter<'_> {
    /// Rewrite every partition except `continuous`, which the caller already
    /// rewrote so the structured-family contract could be decided first.
    fn rewrite_remaining_partitions(
        &mut self,
        dae: &Dae,
        continuous: Vec<RewrittenEquation>,
    ) -> Result<RewrittenPartitions, StructuralError> {
        Ok(RewrittenPartitions {
            continuous,
            initialization: self.rewrite_equations(&dae.initialization.equations)?,
            discrete_real_updates: self.rewrite_equations(&dae.discrete.real_updates)?,
            discrete_valued_updates: self.rewrite_equations(&dae.discrete.valued_updates)?,
            condition_equations: self.rewrite_equations(&dae.conditions.equations)?,
            condition_relations: self.rewrite_expression_slots(&dae.conditions.relations)?,
            synthetic_root_conditions: self
                .rewrite_expression_slots(&dae.events.synthetic_root_conditions)?,
            event_action_conditions: self.rewrite_event_actions(&dae.events.event_actions)?,
            delay_channels: self.rewrite_delay_channels(&dae.events.delay_channels)?,
            clock_constructor_exprs: self
                .rewrite_expression_slots(&dae.clocks.constructor_exprs)?,
            clock_triggered_conditions: self
                .rewrite_expression_slots(&dae.clocks.triggered_conditions)?,
        })
    }

    fn rewrite_equations(
        &mut self,
        equations: &[rumoca_ir_dae::Equation],
    ) -> Result<Vec<RewrittenEquation>, StructuralError> {
        equations
            .iter()
            .map(|equation| {
                rewritten_equation(
                    equation,
                    self.substitutions,
                    self.plan,
                    &mut self.derivative_replacements,
                )
            })
            .collect()
    }

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, StructuralError> {
        apply_substitutions_in_order_with_derivatives(
            expr,
            self.substitutions,
            self.plan,
            &mut self.derivative_replacements,
        )
    }

    fn rewrite_expression_slots(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Vec<Expression>, StructuralError> {
        expressions
            .iter()
            .map(|expression| self.rewrite_expression(expression))
            .collect()
    }

    fn rewrite_event_actions(
        &mut self,
        actions: &[rumoca_ir_dae::DaeEventAction],
    ) -> Result<Vec<Expression>, StructuralError> {
        actions
            .iter()
            .map(|action| self.rewrite_expression(&action.condition))
            .collect()
    }

    fn rewrite_delay_channels(
        &mut self,
        channels: &[rumoca_ir_dae::DaeDelayChannel],
    ) -> Result<Vec<(Expression, Expression, Option<Expression>)>, StructuralError> {
        channels
            .iter()
            .map(|channel| {
                let source = self.rewrite_expression(&channel.source)?;
                let delay_time = self.rewrite_expression(&channel.delay_time)?;
                let delay_max = channel
                    .delay_max
                    .as_ref()
                    .map(|delay_max| self.rewrite_expression(delay_max))
                    .transpose()?;
                Ok((source, delay_time, delay_max))
            })
            .collect()
    }
}

/// Lazily computed symbolic derivatives of substitution replacement expressions.
///
/// The relaxed derivative map is a fixpoint over the whole DAE, so it is built
/// at most once per cache instead of once per cache miss.
struct DerivativeReplacementCache<'a> {
    dae: &'a Dae,
    derivative_map: Option<HashMap<String, Expression>>,
    replacements: HashMap<VarName, Option<Expression>>,
}

impl<'a> DerivativeReplacementCache<'a> {
    fn new(dae: &'a Dae) -> Self {
        Self {
            dae,
            derivative_map: None,
            replacements: HashMap::new(),
        }
    }

    fn replacement_for(
        &mut self,
        substitution: &Substitution,
    ) -> Result<Option<Expression>, StructuralError> {
        if !substitution.var_dims.is_empty() {
            return Ok(None);
        }
        if !self.replacements.contains_key(&substitution.var_name) {
            let derivative_map = match &self.derivative_map {
                Some(derivative_map) => derivative_map,
                None => self
                    .derivative_map
                    .insert(crate::dae_prepare::build_relaxed_derivative_map(self.dae)?),
            };
            let derivative = crate::dae_prepare::symbolic_time_derivative_for_expr_with_map(
                self.dae,
                &substitution.expr,
                derivative_map,
            );
            self.replacements
                .insert(substitution.var_name.clone(), derivative);
        }
        Ok(self
            .replacements
            .get(&substitution.var_name)
            .and_then(Clone::clone))
    }
}

fn subtraction(lhs: Expression, rhs: Expression, span: rumoca_core::Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("delay_substitution_test.mo"),
            1,
            2,
        )
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn constructor(
        name: &str,
        instance_id: u32,
        re_def: u32,
        im_def: u32,
    ) -> rumoca_core::Function {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("aggregate_constructor_test.mo"),
            1,
            2,
        );
        let mut function = rumoca_core::Function::new(name, span);
        function.instance_id = Some(rumoca_core::FunctionInstanceId::new(instance_id));
        function.is_constructor = true;
        for (field, def_id) in [("re", re_def), ("im", im_def)] {
            let mut input = rumoca_core::FunctionParam::new(field, "Real", span);
            input.def_id = Some(rumoca_core::DefId::new(def_id));
            input.type_def_id = Some(rumoca_core::DefId::new(1));
            function.add_input(input);
        }
        function
    }

    #[test]
    fn inherited_operator_record_constructors_share_canonical_base() {
        let mut dae = Dae::default();
        for function in [
            constructor("Modelica.Units.SI.ComplexImpedance", 4, 90, 91),
            constructor("Complex", 8, 90, 91),
        ] {
            dae.symbols
                .functions
                .insert(function.name.clone(), function);
        }

        let reference = aggregate_constructor_reference(&dae)
            .expect("shared field declarations prove one inherited record identity");
        assert_eq!(reference.as_str(), "Complex");
        assert_eq!(
            reference
                .resolved_function()
                .map(|resolved| resolved.instance_id),
            Some(rumoca_core::FunctionInstanceId::new(8))
        );
    }

    #[test]
    fn unrelated_record_constructors_remain_ambiguous() {
        let mut dae = Dae::default();
        for function in [
            constructor("Complex", 8, 90, 91),
            constructor("Other", 9, 190, 191),
        ] {
            dae.symbols
                .functions
                .insert(function.name.clone(), function);
        }

        assert!(aggregate_constructor_reference(&dae).is_none());
    }

    fn scalar_substitution(name: &str, value: f64) -> Substitution {
        Substitution {
            var_name: VarName::new(name),
            var_ref: Some(rumoca_core::Reference::new(name)),
            expr: Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                span: test_span(),
            },
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![name.to_string()],
        }
    }

    fn equation_with_rhs(rhs: Expression) -> rumoca_ir_dae::Equation {
        rumoca_ir_dae::Equation {
            lhs: None,
            rhs,
            span: test_span(),
            origin: "test".to_string(),
            scalar_count: 1,
        }
    }

    /// Rewriting the DAE partitions is all-or-nothing: the structured-family
    /// contract is decided before the initialization/event/clock partitions are
    /// rewritten and before anything is written back, so a family violation
    /// must leave every partition exactly as it was found.
    #[test]
    fn structured_family_conflict_leaves_every_partition_untouched() {
        let mut dae = Dae::default();
        dae.continuous
            .equations
            .push(equation_with_rhs(var_ref("alias")));
        dae.initialization
            .equations
            .push(equation_with_rhs(var_ref("alias")));
        dae.continuous
            .structured_equations
            .push(rumoca_ir_dae::StructuredEquationFamily {
                domain: rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                first_equation_index: 0,
                equations_per_point: 1,
                span: test_span(),
                origin: "for i loop".to_string(),
                regular: None,
                template: None,
                interiors_materialized: false,
            });

        let err =
            apply_substitutions_to_dae_partitions(&mut dae, &[scalar_substitution("alias", 5.0)])
                .expect_err(
                    "substituting into a cheapened structured family's rows must be rejected",
                );

        assert!(
            format!("{err}").contains("placeholder interior rows"),
            "unexpected error: {err}"
        );
        assert_eq!(
            dae.continuous.equations[0].rhs,
            var_ref("alias"),
            "the continuous partition must not be left rewritten after an abort"
        );
        assert_eq!(
            dae.initialization.equations[0].rhs,
            var_ref("alias"),
            "the initialization partition must not be left rewritten after an abort"
        );
        assert_eq!(dae.continuous.structured_equations.len(), 1);
    }

    #[test]
    fn elimination_rewrites_all_delay_channel_expressions() {
        let mut dae = Dae::default();
        dae.events
            .delay_channels
            .push(rumoca_ir_dae::DaeDelayChannel {
                value_parameter: VarName::new("__runtime__.delay.0"),
                source: var_ref("source_alias"),
                delay_time: var_ref("time_alias"),
                delay_max: Some(var_ref("max_alias")),
                source_is_discrete: false,
                span: test_span(),
            });
        let substitutions = [
            ("source_alias", 4.0),
            ("time_alias", 2.0),
            ("max_alias", 3.0),
        ]
        .into_iter()
        .map(|(name, value)| Substitution {
            var_name: VarName::new(name),
            var_ref: Some(rumoca_core::Reference::new(name)),
            expr: Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                span: test_span(),
            },
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![name.to_string()],
        })
        .collect::<Vec<_>>();

        apply_substitutions_to_dae_partitions(&mut dae, &substitutions)
            .expect("delay expressions participate in structural substitution");

        let channel = &dae.events.delay_channels[0];
        assert!(matches!(
            channel.source,
            Expression::Literal {
                value: rumoca_core::Literal::Real(4.0),
                ..
            }
        ));
        assert!(matches!(
            channel.delay_time,
            Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                ..
            }
        ));
        assert!(matches!(
            channel.delay_max,
            Some(Expression::Literal {
                value: rumoca_core::Literal::Real(3.0),
                ..
            })
        ));
    }
}
