use std::collections::HashSet;

use indexmap::IndexSet;
use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, OpBinary, VarName};
use rumoca_ir_dae::{self as dae, Dae};

use super::{StructuralError, equation_analysis_expr, expr_contains_var};

pub(super) fn runtime_protected_unknown_names(
    dae: &Dae,
) -> Result<IndexSet<String>, StructuralError> {
    let mut protected = crate::runtime_defined::runtime_defined_continuous_unknown_names(dae);
    protected.extend(pre_source_protected_unknown_names(dae));
    protected.extend(branch_local_analog_protected_unknown_names(dae));
    protected.extend(clocked_value_source_protected_unknown_names(dae));
    protected.extend(delay_channel_input_protected_unknown_names(dae));
    protected.extend(compact_family_protected_unknown_names(dae)?);
    Ok(protected)
}

fn compact_family_protected_unknown_names(dae: &Dae) -> Result<HashSet<String>, StructuralError> {
    let mut protected = HashSet::new();
    for family in &dae.continuous.structured_equations {
        if family.interiors_materialized {
            continue;
        }
        if let Some(template) = &family.template {
            for expression in &template.body {
                protect_structured_expression_unknowns(dae, &mut protected, expression);
            }
        }
        let row_count = family.scalar_view_row_count().map_err(|error| {
            structured_family_contract_error(
                family.span,
                format!("invalid compact family scalar view: {error}"),
            )
        })?;
        let row_end = family
            .first_equation_index
            .checked_add(row_count)
            .ok_or_else(|| {
                structured_family_contract_error(
                    family.span,
                    "compact family row range overflows host index space".to_string(),
                )
            })?;
        let equations = dae
            .continuous
            .equations
            .get(family.first_equation_index..row_end)
            .ok_or_else(|| {
                structured_family_contract_error(
                    family.span,
                    format!(
                        "compact family row range {}..{} exceeds {} continuous equations",
                        family.first_equation_index,
                        row_end,
                        dae.continuous.equations.len()
                    ),
                )
            })?;
        for equation in equations {
            protect_structured_expression_unknowns(
                dae,
                &mut protected,
                &equation_analysis_expr(equation),
            );
        }
    }
    Ok(protected)
}

fn structured_family_contract_error(span: rumoca_core::Span, reason: String) -> StructuralError {
    if span.is_dummy() {
        StructuralError::UnspannedContractViolation { reason }
    } else {
        StructuralError::ContractViolation { reason, span }
    }
}

fn protect_structured_expression_unknowns(
    dae: &Dae,
    protected: &mut HashSet<String>,
    expression: &Expression,
) {
    let mut references = HashSet::new();
    expression.collect_var_refs(&mut references);
    for name in references {
        maybe_protect_structured_unknown(dae, protected, &name);
    }
}

fn maybe_protect_structured_unknown(dae: &Dae, protected: &mut HashSet<String>, name: &VarName) {
    if dae.variables.states.contains_key(name)
        || dae.variables.algebraics.contains_key(name)
        || dae.variables.outputs.contains_key(name)
    {
        protected.insert(name.as_str().to_string());
    }

    let Some(base) = dae::component_base_name(name.as_str()) else {
        return;
    };
    let base_name = VarName::new(&base);
    if dae.variables.states.contains_key(&base_name)
        || dae.variables.algebraics.contains_key(&base_name)
        || dae.variables.outputs.contains_key(&base_name)
        || continuous_unknown_has_component_base(dae, &base)
    {
        protected.insert(base);
    }
}

fn continuous_unknown_has_component_base(dae: &Dae, expected_base: &str) -> bool {
    dae.variables
        .states
        .keys()
        .chain(dae.variables.algebraics.keys())
        .chain(dae.variables.outputs.keys())
        .any(|candidate| {
            dae::component_base_name(candidate.as_str())
                .is_some_and(|candidate_base| candidate_base == expected_base)
        })
}

fn delay_channel_input_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for channel in &dae.events.delay_channels {
        let mut refs = HashSet::new();
        channel.source.collect_var_refs(&mut refs);
        for name in refs {
            if continuous_definition_references_delay_value(dae, &name) {
                // A delay source whose defining row reads a delay value closes
                // a runtime-memory feedback loop. Keep that carrier named so
                // symbolic elimination cannot hide the loop from structural
                // incidence behind runtime parameter slots.
                maybe_protect_continuous_unknown(dae, &mut protected, &name);
            }
        }
    }
    protected
}

fn continuous_definition_references_delay_value(dae: &Dae, name: &VarName) -> bool {
    dae.continuous.equations.iter().any(|equation| {
        let expression = equation_analysis_expr(equation);
        assignment_target_name(&expression).as_ref() == Some(name)
            && dae
                .events
                .delay_channels
                .iter()
                .any(|channel| expr_contains_var(&expression, &channel.value_parameter))
    })
}

fn pre_source_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for name in dae.variables.parameters.keys() {
        let Some(source) = rumoca_core::pre_slot_base(name.as_str()) else {
            continue;
        };
        maybe_protect_continuous_unknown(dae, &mut protected, &VarName::new(source));
    }
    protected
}

pub(super) fn runtime_defined_discrete_target_names(dae: &Dae) -> HashSet<String> {
    let mut targets = HashSet::default();
    for lhs in dae
        .discrete
        .valued_updates
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .filter_map(|eq| eq.lhs.as_ref())
    {
        targets.insert(lhs.as_str().to_string());
        if let Some(base) = dae::component_base_name(lhs.as_str()) {
            targets.insert(base);
        }
    }
    targets
}

pub(super) fn is_runtime_protected_unknown(name: &VarName, protected: &IndexSet<String>) -> bool {
    protected.contains(name.as_str())
        || dae::component_base_name(name.as_str()).is_some_and(|base| protected.contains(&base))
}

fn branch_local_analog_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for eq in &dae.continuous.equations {
        let analysis_expr = equation_analysis_expr(eq);
        if !expr_contains_branch_local_analog_operator(&analysis_expr) {
            continue;
        }

        // MLS §3.3 / §3.7.5: noEvent/smooth preserve the value semantics of
        // the enclosed expression while only changing event generation or
        // differentiability treatment. Keep continuous helper unknowns that
        // appear inside those branch-local analog rows so structural
        // elimination does not rewrite them away into a numerically wider
        // solve than the original Modelica equation system.
        let mut refs = HashSet::new();
        analysis_expr.collect_var_refs(&mut refs);
        for name in refs {
            maybe_protect_continuous_unknown(dae, &mut protected, &name);
        }
        if let Some(target) = assignment_target_name(&analysis_expr) {
            maybe_protect_continuous_unknown(dae, &mut protected, &target);
        }
    }
    protected
}

fn clocked_value_source_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for eq in dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
    {
        collect_clocked_value_source_unknowns(dae, &eq.rhs, &mut protected);
    }
    protected
}

fn collect_clocked_value_source_unknowns(
    dae: &Dae,
    expr: &Expression,
    protected: &mut HashSet<String>,
) {
    ClockedValueSourceCollector { dae, protected }.visit_expression(expr);
}

struct ClockedValueSourceCollector<'a> {
    dae: &'a Dae,
    protected: &'a mut HashSet<String>,
}

impl ClockedValueSourceCollector<'_> {
    fn protect_source_refs(&mut self, source: &Expression) {
        let mut refs = HashSet::new();
        source.collect_var_refs(&mut refs);
        for name in refs {
            maybe_protect_continuous_unknown(self.dae, self.protected, &name);
        }
    }
}

impl ExpressionVisitor for ClockedValueSourceCollector<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                args,
                ..
            } => {
                if let Some(source) = args.first() {
                    // MLS §16.5.1 sampled-value equations read the source at
                    // clock ticks, so preserve helper unknowns feeding it.
                    self.protect_source_refs(source);
                }
                self.walk_expression(expr);
            }
            Expression::FunctionCall { name, args, .. } => {
                if matches!(
                    crate::runtime_defined::clocked_or_event_function_short_name(name),
                    Some(
                        "hold"
                            | "previous"
                            | "noClock"
                            | "subSample"
                            | "superSample"
                            | "shiftSample"
                            | "backSample"
                    )
                ) && let Some(source) = args.first()
                {
                    self.protect_source_refs(source);
                }
                self.walk_expression(expr);
            }
            _ => self.walk_expression(expr),
        }
    }
}

fn maybe_protect_continuous_unknown(dae: &Dae, protected: &mut HashSet<String>, name: &VarName) {
    if dae.variables.algebraics.contains_key(name) || dae.variables.outputs.contains_key(name) {
        protected.insert(name.as_str().to_string());
    }

    let Some(base) = dae::component_base_name(name.as_str()) else {
        return;
    };
    let base = VarName::new(base);
    if dae.variables.algebraics.contains_key(&base) || dae.variables.outputs.contains_key(&base) {
        protected.insert(base.as_str().to_string());
    }
}

fn expr_contains_branch_local_analog_operator(expr: &Expression) -> bool {
    let mut checker = BranchLocalAnalogOperatorChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

struct BranchLocalAnalogOperatorChecker {
    found: bool,
}

impl ExpressionVisitor for BranchLocalAnalogOperatorChecker {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if matches!(
            function,
            BuiltinFunction::Smooth | BuiltinFunction::NoEvent | BuiltinFunction::Homotopy
        ) {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

pub(super) fn assignment_target_name(expr: &Expression) -> Option<VarName> {
    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    if !matches!(op, OpBinary::Sub) {
        return None;
    }
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
        && let Some(target) = assignment_var_ref_name(name.var_name(), subscripts)
    {
        return Some(target);
    }
    if let Expression::VarRef {
        name, subscripts, ..
    } = rhs.as_ref()
        && let Some(target) = assignment_var_ref_name(name.var_name(), subscripts)
    {
        return Some(target);
    }
    None
}

pub(super) fn assignment_var_ref_name(
    name: &VarName,
    subscripts: &[rumoca_core::Subscript],
) -> Option<VarName> {
    if subscripts.is_empty() {
        return Some(name.clone());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let idx = match subscript {
            rumoca_core::Subscript::Index { value: idx, .. } => *idx,
            rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(idx),
                    ..
                } => *idx,
                Expression::Literal {
                    value: rumoca_core::Literal::Real(value),
                    ..
                } if value.is_finite() && value.fract() == 0.0 => *value as i64,
                _ => return None,
            },
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        indices.push(idx.to_string());
    }
    Some(VarName::new(format!(
        "{}[{}]",
        name.as_str(),
        indices.join(",")
    )))
}

pub(super) fn runtime_partition_or_event_refs_var(dae: &Dae, var_name: &VarName) -> bool {
    dae.discrete
        .real_updates
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .discrete
            .valued_updates
            .iter()
            .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .conditions
            .equations
            .iter()
            .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .conditions
            .relations
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
        || dae
            .events
            .synthetic_root_conditions
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
        || dae
            .clocks
            .constructor_exprs
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
}

pub(super) fn should_preserve_runtime_known_assignment(dae: &Dae, eq_rhs: &Expression) -> bool {
    let Some(target) = assignment_target_name(eq_rhs) else {
        return false;
    };
    dae.variables.discrete_reals.contains_key(&target)
        || dae.variables.discrete_valued.contains_key(&target)
        || runtime_partition_or_event_refs_var(dae, &target)
}

pub(super) fn expr_references_any_runtime_discrete_target(
    expr: &Expression,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    if runtime_defined_discrete_targets.is_empty() {
        return false;
    }

    let mut refs: HashSet<VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| {
        let raw = name.as_str();
        runtime_defined_discrete_targets.contains(raw)
            || dae::component_base_name(raw)
                .is_some_and(|base| runtime_defined_discrete_targets.contains(base.as_str()))
    })
}

pub(super) fn expr_references_any_discrete_name(dae: &Dae, expr: &Expression) -> bool {
    let mut refs: HashSet<VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| {
        dae.variables.discrete_reals.contains_key(name)
            || dae.variables.discrete_valued.contains_key(name)
            || dae::component_base_name(name.as_str()).is_some_and(|base| {
                let base = VarName::new(base.as_str());
                dae.variables.discrete_reals.contains_key(&base)
                    || dae.variables.discrete_valued.contains_key(&base)
            })
    })
}

// ── Expression Helpers ──────────────────────────────────────────────────
