//! Lower `pre()` operator references into dedicated parameter symbols.
//!
//! MLS §3.7.5: `pre(y)` returns the left limit of a discrete variable y at an
//! event instant. CasADi and other numeric backends don't have a native `pre()`
//! function. This pass replaces `pre(x)` with a parameter variable `__pre__.x`
//! that the simulation driver updates at each event boundary.

use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, ExpressionVisitor};
use rumoca_ir_dae::{self as dae, DaeVisitor};

use crate::{ToDaeError, reference_validation::build_enum_literal_query_set};

/// Lower every `pre()` call in any DAE equation partition to a parameter
/// reference. SPEC_0007 Stage 3 Contract: no `pre()` survives into f_x, f_z,
/// f_m, or f_c — every `pre(v)` becomes a reference to a `__pre__.v` parameter
/// slot before the DAE stage exits. The runtime writes those slots at event
/// entry via `SolveLayout::pre_param_bindings` (see SPEC_0007 Stage 4).
///
/// For each unique `pre(x)` reference:
/// 1. Creates a parameter `__pre__.x` with the same dimensions and start value as `x`
/// 2. Replaces the `BuiltinCall { Pre, [VarRef(x)] }` with `VarRef("__pre__.x")`
pub(crate) fn lower_pre_operator(dae: &mut dae::Dae) -> Result<(), ToDaeError> {
    // Collect pre() targets across every equation partition uniformly.
    // SPEC_0007 §Stage 3 Contract: applies to f_x, f_z, f_m, f_c without
    // exception. MLS Appendix B canonical vector v := [p; t; ẋ; x; y; z; m;
    // pre(z); pre(m)] treats pre(z) and pre(m) as parameter slots in p, not
    // as runtime operators. Eliminating pre() uniformly converts the DAE
    // into a system of pure mathematical functions over v.
    // IndexMap for deterministic parameter insertion order (SPEC_0021).
    let mut pre_targets: IndexMap<rumoca_core::VarName, PreTarget> = IndexMap::new();
    collect_pre_targets_from_equations(&dae.continuous.equations, &mut pre_targets, false)?;
    collect_pre_targets_from_equations(&dae.discrete.real_updates, &mut pre_targets, true)?;
    collect_pre_targets_from_equations(&dae.discrete.valued_updates, &mut pre_targets, true)?;
    collect_pre_targets_from_equations(&dae.conditions.equations, &mut pre_targets, true)?;
    for expr in &dae.conditions.relations {
        collect_pre_targets_from_expr(expr, &mut pre_targets, false)?;
    }
    for expr in &dae.clocks.triggered_conditions {
        collect_pre_targets_from_expr(expr, &mut pre_targets, true)?;
    }
    collect_pre_targets_from_event_actions(&dae.events.event_actions, &mut pre_targets)?;
    collect_pre_targets_from_equations(&dae.initialization.equations, &mut pre_targets, true)?;

    discard_enum_literal_pre_targets(dae, &mut pre_targets);
    resolve_pre_targets(dae, &mut pre_targets)?;
    let mut pre_params: IndexMap<rumoca_core::VarName, dae::Variable> = IndexMap::new();

    for (target_name, target) in &pre_targets {
        let pre_param_name =
            rumoca_core::VarName::new(format!("__pre__.{}", target.source_name.as_str()));
        let Some(var) = find_variable(dae, &target.source_name) else {
            return Err(ToDaeError::runtime_contract_violation_at(
                format!(
                    "pre() target `{}` was not declared in DAE variables",
                    target_name.as_str()
                ),
                target.span,
            ));
        };
        let pre_var = build_pre_parameter(&pre_param_name, var, target.source_name.as_str());
        pre_params.insert(pre_param_name, pre_var);
    }

    // Add pre-parameters to the DAE. This pass intentionally runs twice: once
    // before temporal-finalization and once after canonical condition variables
    // may have introduced new pre() references. Existing __pre__ variables keep
    // their first-pass metadata instead of being silently re-snapshotted.
    for (name, var) in pre_params {
        if !dae.variables.parameters.contains_key(&name) {
            dae.variables.parameters.insert(name, var);
        }
    }

    let relation_memories = relation_memory_exprs(dae)?;

    rewrite_equations(
        &mut dae.continuous.equations,
        &pre_targets,
        &relation_memories,
    )?;
    rewrite_equations(
        &mut dae.discrete.real_updates,
        &pre_targets,
        &relation_memories,
    )?;
    rewrite_equations(
        &mut dae.discrete.valued_updates,
        &pre_targets,
        &relation_memories,
    )?;
    rewrite_equations(
        &mut dae.conditions.equations,
        &pre_targets,
        &relation_memories,
    )?;
    for expr in &mut dae.conditions.relations {
        *expr = rewrite_pre_expr(expr, &pre_targets, &relation_memories)?;
    }
    for expr in &mut dae.clocks.triggered_conditions {
        *expr = rewrite_pre_expr(expr, &pre_targets, &relation_memories)?;
    }
    rewrite_event_actions(
        &mut dae.events.event_actions,
        &pre_targets,
        &relation_memories,
    )?;
    rewrite_equations(
        &mut dae.initialization.equations,
        &pre_targets,
        &relation_memories,
    )?;
    Ok(())
}

struct PreTarget {
    span: rumoca_core::Span,
    source_name: rumoca_core::VarName,
    require_discrete: bool,
    allow_continuous_target: bool,
}

fn collect_pre_targets_from_equations(
    equations: &[dae::Equation],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
    allow_continuous_target: bool,
) -> Result<(), ToDaeError> {
    for eq in equations {
        collect_pre_targets_from_expr(&eq.rhs, targets, allow_continuous_target)?;
    }
    Ok(())
}

fn collect_pre_targets_from_expr(
    expr: &rumoca_core::Expression,
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
    allow_continuous_target: bool,
) -> Result<(), ToDaeError> {
    let mut collector = PreTargetCollector {
        targets,
        allow_continuous_target,
        error: None,
    };
    ExpressionVisitor::visit_expression(&mut collector, expr);
    match collector.error {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

fn collect_pre_targets_from_event_actions(
    actions: &[dae::DaeEventAction],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    for action in actions {
        collect_pre_targets_from_expr(&action.condition, targets, true)?;
    }
    Ok(())
}

struct PreTargetCollector<'a> {
    targets: &'a mut IndexMap<rumoca_core::VarName, PreTarget>,
    allow_continuous_target: bool,
    error: Option<ToDaeError>,
}

impl PreTargetCollector<'_> {
    fn collect_pre_call_arg(&mut self, args: &[rumoca_core::Expression]) {
        let Some(arg) = args.first() else {
            self.collect_args_as_pre_values(args);
            return;
        };
        let (name, subscripts) = match pre_target_parts(arg) {
            Ok(Some(parts)) => parts,
            Ok(None) => {
                self.collect_args_as_pre_values(args);
                return;
            }
            Err(err) => {
                self.error = Some(err);
                return;
            }
        };
        let span = match pre_target_span(arg, &subscripts, "pre() target") {
            Ok(span) => span,
            Err(err) => {
                self.error = Some(err);
                return;
            }
        };
        insert_pre_target(self.targets, name, span, true, self.allow_continuous_target);
    }

    fn record_collection_result(&mut self, result: Result<(), ToDaeError>) {
        if let Err(err) = result {
            self.error = Some(err);
        }
    }

    fn collect_args_as_pre_values(&mut self, args: &[rumoca_core::Expression]) {
        let result = collect_args_as_pre_values(args, self.targets);
        self.record_collection_result(result);
    }

    fn collect_first_arg_as_pre_value(&mut self, args: &[rumoca_core::Expression]) {
        let result = collect_first_arg_as_pre_value(args, self.targets);
        self.record_collection_result(result);
    }

    fn collect_sample_value_as_pre_value(&mut self, args: &[rumoca_core::Expression]) {
        let result = collect_sample_value_as_pre_value(args, self.targets);
        self.record_collection_result(result);
    }
}

impl ExpressionVisitor for PreTargetCollector<'_> {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if self.error.is_some() {
            return;
        }
        match function {
            rumoca_core::BuiltinFunction::Pre => {
                self.collect_pre_call_arg(args);
            }
            rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change => {
                self.collect_first_arg_as_pre_value(args);
            }
            rumoca_core::BuiltinFunction::Sample => {
                self.collect_sample_value_as_pre_value(args);
            }
            _ => {}
        }
        if self.error.is_some() {
            return;
        }
        self.walk_builtin_call(function, args);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if self.error.is_some() {
            return;
        }
        match rumoca_core::source_temporal_function_short_name(name.as_str()) {
            Some("pre") => {
                self.collect_pre_call_arg(args);
            }
            Some("edge" | "change" | "previous") => {
                self.collect_first_arg_as_pre_value(args);
            }
            Some("sample") => {
                self.collect_sample_value_as_pre_value(args);
            }
            _ => {}
        }
        if self.error.is_some() {
            return;
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

fn collect_args_as_pre_values(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    for arg in args {
        collect_pre_targets_from_pre_expr(arg, targets)?;
    }
    Ok(())
}

fn collect_first_arg_as_pre_value(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    if let Some(arg) = args.first() {
        collect_pre_targets_from_pre_expr(arg, targets)?;
    }
    Ok(())
}

fn collect_sample_value_as_pre_value(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    if matches!(args.len(), 1 | 2) && !args.first().is_some_and(is_time_var_ref) {
        collect_first_arg_as_pre_value(args, targets)?;
    }
    Ok(())
}

fn is_time_var_ref(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn collect_pre_targets_from_pre_expr(
    expr: &rumoca_core::Expression,
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    let mut collector = PreValueCollector {
        targets,
        error: None,
    };
    ExpressionVisitor::visit_expression(&mut collector, expr);
    match collector.error {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

struct PreValueCollector<'a> {
    targets: &'a mut IndexMap<rumoca_core::VarName, PreTarget>,
    error: Option<ToDaeError>,
}

impl ExpressionVisitor for PreValueCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.error.is_some() {
            return;
        }
        let parts = match pre_target_parts(expr) {
            Ok(parts) => parts,
            Err(err) => {
                self.error = Some(err);
                return;
            }
        };
        if let Some((name, subscripts)) = parts {
            match pre_target_span(expr, &subscripts, "pre() value target") {
                Ok(span) => insert_pre_target(self.targets, name, span, false, true),
                Err(err) => self.error = Some(err),
            }
            return;
        }
        self.walk_expression(expr);
    }

    fn visit_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
    ) {
        if let Some(selected_branch) = selected_static_if_branch(branches, else_branch) {
            self.visit_expression(selected_branch);
            return;
        }
        for (condition, value) in branches {
            self.visit_expression(condition);
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }
}

fn insert_pre_target(
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
    name: rumoca_core::VarName,
    span: rumoca_core::Span,
    require_discrete: bool,
    allow_continuous_target: bool,
) {
    if name.as_str() == "time" {
        return;
    }
    targets
        .entry(name.clone())
        .and_modify(|target| {
            target.require_discrete |= require_discrete;
            target.allow_continuous_target &= allow_continuous_target;
        })
        .or_insert_with(|| PreTarget {
            span,
            source_name: name,
            require_discrete,
            allow_continuous_target,
        });
}

fn discard_enum_literal_pre_targets(
    dae: &dae::Dae,
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) {
    let enum_literals = build_enum_literal_query_set(&dae.symbols.enum_literal_ordinals);
    targets.retain(|name, _| !enum_literals.contains(name.as_str()));
}

fn resolve_pre_targets(
    dae: &dae::Dae,
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<(), ToDaeError> {
    // The scalarized-field fallback used to scan (and path-parse) every
    // variable per unresolved target, which is quadratic when many
    // parameter-dependent relations create pre() targets. Build the
    // (prefix, field) -> candidates index once, on first need.
    let mut scalarized_index = None;
    for (target_name, target) in targets {
        if let Some((partition, _)) = find_variable_partition(dae, target_name) {
            if target.require_discrete {
                validate_pre_target_partition(
                    partition,
                    target_name,
                    target.span,
                    target.allow_continuous_target,
                )?;
            }
            target.source_name = target_name.clone();
            continue;
        }
        if let Some((source_name, partition)) =
            repeated_indexed_component_pre_target(dae, target_name)
        {
            if target.require_discrete {
                validate_pre_target_partition(
                    partition,
                    &source_name,
                    target.span,
                    target.allow_continuous_target,
                )?;
            }
            target.source_name = source_name;
            continue;
        }
        let index = scalarized_index.get_or_insert_with(|| build_scalarized_field_index(dae));
        if let Some(source_name) =
            singleton_scalarized_field_name(index, target_name, target.require_discrete)
        {
            target.source_name = source_name;
            continue;
        }
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "pre() target `{}` was not declared in DAE variables",
                target_name.as_str()
            ),
            target.span,
        ));
    }
    Ok(())
}

fn repeated_indexed_component_pre_target(
    dae: &dae::Dae,
    target_name: &rumoca_core::VarName,
) -> Option<(rumoca_core::VarName, dae::DaeVariablePartition)> {
    for candidate in
        crate::path_utils::repeated_indexed_component_path_candidates(target_name.as_str())
    {
        let candidate = rumoca_core::VarName::new(candidate);
        if let Some((partition, _)) = find_variable_partition(dae, &candidate) {
            return Some((candidate, partition));
        }
    }
    None
}

type ScalarizedFieldIndex =
    std::collections::HashMap<(String, String), Vec<(rumoca_core::VarName, bool)>>;

/// One pass over all variables: candidates keyed by (subscript-stripped
/// prefix base name, last field segment), tagged with whether their
/// partition is a valid pre() target.
fn build_scalarized_field_index(dae: &dae::Dae) -> ScalarizedFieldIndex {
    struct Collector {
        index: ScalarizedFieldIndex,
    }
    impl DaeVisitor for Collector {
        fn visit_variable(
            &mut self,
            partition: dae::DaeVariablePartition,
            name: &rumoca_core::VarName,
            _variable: &dae::Variable,
        ) {
            let path = rumoca_core::ComponentPath::from_flat_path(name.as_str());
            let Some((field, prefix_parts)) = path.parts().split_last() else {
                return;
            };
            let candidate_prefix =
                rumoca_core::ComponentPath::from_parts(prefix_parts.iter().cloned());
            let Some(prefix_base) =
                rumoca_core::component_path_base_name(candidate_prefix.as_str())
            else {
                return;
            };
            self.index
                .entry((prefix_base, field.as_str().to_string()))
                .or_default()
                .push((name.clone(), is_pre_target_partition(partition)));
        }
    }
    let mut collector = Collector {
        index: ScalarizedFieldIndex::new(),
    };
    collector.visit_variables(&dae.variables);
    collector.index
}

fn singleton_scalarized_field_name(
    index: &ScalarizedFieldIndex,
    target_name: &rumoca_core::VarName,
    require_discrete: bool,
) -> Option<rumoca_core::VarName> {
    let (prefix, field) = target_name.scope_split()?;
    let candidates = index.get(&(prefix.to_string(), field.to_string()))?;
    let mut matches = candidates
        .iter()
        .filter(|(_, pre_ok)| !require_discrete || *pre_ok)
        .map(|(name, _)| name);
    match (matches.next(), matches.next()) {
        (Some(name), None) => Some(name.clone()),
        _ => None,
    }
}

/// Snapshot parameter for `pre(<source>)`: copies the source variable's
/// shape/start metadata and attaches a generated `__pre__.<source>`
/// structured reference (DAE provenance contract).
fn build_pre_parameter(
    pre_param_name: &rumoca_core::VarName,
    var: &dae::Variable,
    source_name: &str,
) -> dae::Variable {
    let pre_ref = crate::condition_lowering::generated_pre_component_ref(
        var.component_ref.as_ref(),
        source_name,
        var.source_span,
    );
    dae::Variable {
        name: pre_param_name.clone(),
        component_ref: Some(pre_ref),
        source_span: var.source_span,
        dims: var.dims.clone(),
        start: var.start.clone(),
        start_span: var.start_attribute_span(),
        fixed: Some(true),
        min: None,
        min_span: None,
        max: None,
        max_span: None,
        nominal: None,
        nominal_span: None,
        unit: None,
        state_select: rumoca_core::StateSelect::Default,
        description: Some(format!("pre() of {source_name}")),
        causality: dae::VariableCausality::CalculatedParameter,
        is_tunable: false,
        origin: dae::VariableOrigin::Generated,
    }
}

fn find_variable<'a>(dae: &'a dae::Dae, name: &rumoca_core::VarName) -> Option<&'a dae::Variable> {
    find_variable_partition(dae, name).map(|(_, variable)| variable)
}

fn find_variable_partition<'a>(
    dae: &'a dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<(dae::DaeVariablePartition, &'a dae::Variable)> {
    dae.variables
        .states
        .get(name)
        .map(|variable| (dae::DaeVariablePartition::State, variable))
        .or_else(|| {
            dae.variables
                .algebraics
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::Algebraic, variable))
        })
        .or_else(|| {
            dae.variables
                .inputs
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::Input, variable))
        })
        .or_else(|| {
            dae.variables
                .outputs
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::Output, variable))
        })
        .or_else(|| {
            dae.variables
                .parameters
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::Parameter, variable))
        })
        .or_else(|| {
            dae.variables
                .constants
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::Constant, variable))
        })
        .or_else(|| {
            dae.variables
                .discrete_reals
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::DiscreteReal, variable))
        })
        .or_else(|| {
            dae.variables
                .discrete_valued
                .get(name)
                .map(|variable| (dae::DaeVariablePartition::DiscreteValued, variable))
        })
}

fn validate_pre_target_partition(
    partition: dae::DaeVariablePartition,
    name: &rumoca_core::VarName,
    span: rumoca_core::Span,
    allow_continuous_target: bool,
) -> Result<(), ToDaeError> {
    if is_allowed_explicit_pre_target_partition(partition, name, allow_continuous_target) {
        return Ok(());
    }
    Err(ToDaeError::runtime_contract_violation_at(
        format!(
            "pre() target `{}` is not a discrete-time variable",
            name.as_str()
        ),
        span,
    ))
}

fn is_allowed_explicit_pre_target_partition(
    partition: dae::DaeVariablePartition,
    name: &rumoca_core::VarName,
    allow_continuous_target: bool,
) -> bool {
    if is_pre_target_partition(partition) || name.as_str().starts_with("__pre__.") {
        return true;
    }

    allow_continuous_target || !matches!(partition, dae::DaeVariablePartition::State)
}

fn is_pre_target_partition(partition: dae::DaeVariablePartition) -> bool {
    matches!(
        partition,
        dae::DaeVariablePartition::DiscreteReal | dae::DaeVariablePartition::DiscreteValued
    )
}

fn rewrite_equations(
    equations: &mut [dae::Equation],
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &[(rumoca_core::Expression, rumoca_core::Expression)],
) -> Result<(), ToDaeError> {
    for eq in equations {
        eq.rhs = rewrite_pre_expr(&eq.rhs, targets, relation_memories)?;
    }
    Ok(())
}

fn relation_memory_exprs(
    dae_model: &dae::Dae,
) -> Result<Vec<(rumoca_core::Expression, rumoca_core::Expression)>, ToDaeError> {
    let mut memories = Vec::new();
    let mut relation_offset = 0usize;
    for equation in &dae_model.conditions.equations {
        let scalar_count = equation.scalar_count.max(1);
        let Some(lhs) = equation.lhs.as_ref() else {
            relation_offset += scalar_count;
            continue;
        };
        for scalar_index in 0..scalar_count {
            let Some(relation) = dae_model
                .conditions
                .relations
                .get(relation_offset + scalar_index)
            else {
                continue;
            };
            memories.push((
                relation.clone(),
                relation_memory_scalar_expr(
                    dae_model,
                    lhs,
                    scalar_index,
                    scalar_count,
                    equation.span,
                )?,
            ));
        }
        relation_offset += scalar_count;
    }
    Ok(memories)
}

fn relation_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::Reference,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let target = relation_memory_target(lhs, span)?;
    let var = dae_model
        .variables
        .discrete_valued
        .get(&target.variable_name)
        .or_else(|| {
            dae_model
                .variables
                .discrete_reals
                .get(&target.variable_name)
        })
        .ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "relation memory target `{}` has scalar_count={} but no DAE variable metadata",
                    lhs.as_str(),
                    scalar_count
                ),
                span,
            )
        })?;
    let name = reference_for_variable_origin(&target.variable_name, var);
    if scalar_count <= 1 {
        return Ok(rumoca_core::Expression::VarRef {
            name,
            subscripts: target.subscripts,
            span,
        });
    }
    if !target.subscripts.is_empty() {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "relation memory target `{}` has both explicit lhs subscripts and scalar_count={}",
                lhs.as_str(),
                scalar_count
            ),
            span,
        ));
    }
    let dims = var.dims.as_slice();
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)
        .ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "relation memory target `{}` cannot map flat index {} of {} through dims {:?}",
                    lhs.as_str(),
                    flat_index,
                    scalar_count,
                    dims
                ),
                span,
            )
        })?
        .into_iter()
        .map(|index| {
            let index = i64::try_from(index).map_err(|_| {
                ToDaeError::runtime_contract_violation_at(
                    format!(
                        "relation memory target `{}` flat index component {index} exceeds i64 range",
                        lhs.as_str()
                    ),
                    span,
                )
            })?;
            generated_index_subscript(index, span, "relation memory target subscript")
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    })
}

struct RelationMemoryTarget {
    variable_name: rumoca_core::VarName,
    subscripts: Vec<rumoca_core::Subscript>,
}

fn relation_memory_target(
    lhs: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Result<RelationMemoryTarget, ToDaeError> {
    let Some(component_ref) = lhs.component_ref() else {
        return Ok(RelationMemoryTarget {
            variable_name: lhs.var_name().clone(),
            subscripts: Vec::new(),
        });
    };
    let Some(last_part) = component_ref.parts.last() else {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "relation memory target `{}` has empty component reference",
                lhs.as_str()
            ),
            span,
        ));
    };
    let mut base_ref = component_ref.clone();
    let Some(base_part) = base_ref.parts.last_mut() else {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "relation memory target `{}` has empty component reference",
                lhs.as_str()
            ),
            span,
        ));
    };
    let subscripts = std::mem::take(&mut base_part.subs);
    let variable_name = if base_ref.parts.len() == 1 {
        rumoca_core::VarName::new(last_part.ident.clone())
    } else {
        rumoca_core::VarName::new(
            rumoca_core::ComponentPath::from_component_reference(&base_ref).to_flat_string(),
        )
    };
    Ok(RelationMemoryTarget {
        variable_name,
        subscripts,
    })
}

fn reference_for_variable_origin(
    name: &rumoca_core::VarName,
    var: &dae::Variable,
) -> rumoca_core::Reference {
    match var.origin {
        dae::VariableOrigin::Generated => rumoca_core::Reference::generated(name.as_str()),
        dae::VariableOrigin::Source => name.clone().into(),
    }
}

fn relation_memory_expr_for_condition(
    relation_memories: &[(rumoca_core::Expression, rumoca_core::Expression)],
    condition: &rumoca_core::Expression,
) -> Option<rumoca_core::Expression> {
    relation_memories
        .iter()
        .find_map(|(relation, memory)| (relation == condition).then(|| memory.clone()))
}

fn rewrite_event_actions(
    actions: &mut [dae::DaeEventAction],
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &[(rumoca_core::Expression, rumoca_core::Expression)],
) -> Result<(), ToDaeError> {
    for action in actions {
        action.condition = rewrite_pre_expr(&action.condition, targets, relation_memories)?;
    }
    Ok(())
}

fn rewrite_pre_expr(
    expr: &rumoca_core::Expression,
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &[(rumoca_core::Expression, rumoca_core::Expression)],
) -> Result<rumoca_core::Expression, ToDaeError> {
    let mut rewriter = PreExpressionRewriter {
        targets,
        relation_memories,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(err) => Err(err),
        None => Ok(rewritten),
    }
}

struct PreExpressionRewriter<'a> {
    targets: &'a IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &'a [(rumoca_core::Expression, rumoca_core::Expression)],
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for PreExpressionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Pre,
                args,
                span,
            } => self.rewrite_pre_builtin(args, *span),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Edge,
                args,
                span,
            } => self.rewrite_edge_builtin(args, *span),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Change,
                args,
                span,
            } => self.rewrite_change_builtin(args, *span),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args,
                span,
            } => self.rewrite_sample_builtin(args, *span),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => match rumoca_core::source_temporal_function_short_name(name.as_str()) {
                Some("pre") => self.rewrite_pre_builtin(args, *span),
                Some("edge") => self.rewrite_edge_builtin(args, *span),
                Some("change") => self.rewrite_change_builtin(args, *span),
                Some("sample") => self.rewrite_sample_builtin(args, *span),
                Some("previous") => self.rewrite_previous_function(args, *is_constructor, *span),
                _ => self.walk_expression(expr),
            },
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(self.rewrite_expression(expr)),
                indices: indices.clone(),
                filter: filter
                    .as_deref()
                    .map(|filter| Box::new(self.rewrite_expression(filter))),
                span: *span,
            },
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => {
                if let Some(selected_branch) = selected_static_if_branch(branches, else_branch) {
                    return self.rewrite_expression(selected_branch);
                }
                self.walk_if_expression(branches, else_branch, *span)
            }
            _ => self.walk_expression(expr),
        }
    }
}

impl PreExpressionRewriter<'_> {
    fn rewrite_pre_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if let Some(arg) = args.first()
            && let Some(memory) = relation_memory_expr_for_condition(self.relation_memories, arg)
        {
            return self.rewrite_relation_memory_pre_expr(&memory, span);
        }
        let rewritten_args = self.rewrite_expressions(args);
        let parts = match rewritten_args.first().map(pre_target_parts) {
            Some(Ok(parts)) => parts,
            Some(Err(err)) => {
                self.error = Some(err);
                return rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: rewritten_args,
                    span,
                };
            }
            None => None,
        };
        if let Some((name, subscripts)) = parts
            && let Some(target) = self.targets.get(&name)
        {
            return pre_var_ref(&target.source_name, &subscripts, span);
        }
        if let [arg] = rewritten_args.as_slice() {
            return self.rewrite_pre_value(arg);
        }
        // Preserve unsupported pre(...) shapes so validation/errors can surface
        // instead of silently dropping expression structure.
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: rewritten_args,
            span,
        }
    }

    fn rewrite_edge_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let Some(arg) = args.first() else {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span,
            };
        };
        if let Some(memory) = relation_memory_expr_for_condition(self.relation_memories, arg) {
            return rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::And,
                lhs: Box::new(memory.clone()),
                rhs: Box::new(rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs: Box::new(self.rewrite_relation_memory_pre_expr(&memory, span)),
                    span,
                }),
                span,
            };
        }
        // A source `edge(relation)` that reaches this pass without condition
        // memory must still keep Modelica edge semantics instead of becoming a
        // level-sensitive relation.
        let current = self.rewrite_expression(arg);
        let previous = self.rewrite_pre_value(&current);
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(current),
            rhs: Box::new(rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs: Box::new(previous),
                span,
            }),
            span,
        }
    }

    fn rewrite_change_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let Some(arg) = args.first() else {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span,
            };
        };
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Neq,
            lhs: Box::new(self.rewrite_expression(arg)),
            rhs: Box::new(self.rewrite_pre_value(arg)),
            span,
        }
    }

    fn rewrite_sample_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if rumoca_core::sample_call_is_inferred_clock_value_form(args)
            && let Some(value) = args.first()
        {
            return self.rewrite_pre_value(value);
        }
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
            args: self.rewrite_expressions(args),
            is_constructor: false,
            span,
        }
    }

    fn rewrite_previous_function(
        &mut self,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if let Some(arg) = args.first() {
            return self.rewrite_pre_value(arg);
        }
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated("previous"),
            args: Vec::new(),
            is_constructor,
            span,
        }
    }

    fn rewrite_pre_value(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match rewrite_pre_expr_value(expr, self.targets) {
            Ok(expr) => expr,
            Err(err) => {
                self.error = Some(err);
                expr.clone()
            }
        }
    }

    fn rewrite_relation_memory_pre_expr(
        &mut self,
        memory: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        match relation_memory_pre_expr(memory, span) {
            Ok(expr) => expr,
            Err(err) => {
                self.error = Some(err);
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![memory.clone()],
                    span,
                }
            }
        }
    }
}

fn relation_memory_pre_expr(
    memory: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let Some((name, subscripts)) = pre_target_parts(memory)? {
        return Ok(pre_var_ref(&name, &subscripts, span));
    }
    Ok(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![memory.clone()],
        span,
    })
}

fn rewrite_pre_expr_value(
    expr: &rumoca_core::Expression,
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let mut rewriter = PreValueRewriter {
        targets,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(err) => Err(err),
        None => Ok(rewritten),
    }
}

struct PreValueRewriter<'a> {
    targets: &'a IndexMap<rumoca_core::VarName, PreTarget>,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for PreValueRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        let parts = match pre_target_parts(expr) {
            Ok(parts) => parts,
            Err(err) => {
                self.error = Some(err);
                return expr.clone();
            }
        };
        if let Some((name, subscripts)) = parts
            && let Some(target) = self.targets.get(&name)
        {
            return pre_var_ref(
                &target.source_name,
                &subscripts,
                expr.span().unwrap_or(target.span),
            );
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args,
                span,
            } if !rumoca_core::sample_call_is_inferred_clock_value_form(args) => {
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(false),
                    span: *span,
                }
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(self.rewrite_expression(expr)),
                indices: indices.clone(),
                filter: filter
                    .as_deref()
                    .map(|filter| Box::new(self.rewrite_expression(filter))),
                span: *span,
            },
            _ => self.walk_expression(expr),
        }
    }
}

fn pre_var_ref(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::generated(format!("__pre__.{}", name.as_str())),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn pre_target_parts(
    expr: &rumoca_core::Expression,
) -> Result<Option<(rumoca_core::VarName, Vec<rumoca_core::Subscript>)>, ToDaeError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
            ..
        } if !is_pre_parameter_name(name.var_name()) => {
            pre_target_parts_from_name(name.var_name(), subscripts, *span).map(Some)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some((name, mut target_subscripts)) = pre_target_parts(base)? else {
                return Ok(None);
            };
            target_subscripts.extend(subscripts.clone());
            Ok(Some((name, target_subscripts)))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let Some((base_name, subscripts)) = pre_target_parts(base)? else {
                return Ok(None);
            };
            if subscripts.is_empty() {
                Ok(Some((
                    rumoca_core::VarName::new(format!("{}.{}", base_name.as_str(), field)),
                    Vec::new(),
                )))
            } else {
                Ok(indexed_field_target_name(&base_name, &subscripts, field)
                    .map(|name| (name, Vec::new())))
            }
        }
        _ => Ok(None),
    }
}

fn pre_target_parts_from_name(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Result<(rumoca_core::VarName, Vec<rumoca_core::Subscript>), ToDaeError> {
    let (base_name, mut encoded_subscripts) =
        split_encoded_integer_subscripts(name.as_str(), span)?;
    encoded_subscripts.extend_from_slice(subscripts);
    Ok((rumoca_core::VarName::new(base_name), encoded_subscripts))
}

fn split_encoded_integer_subscripts(
    path: &str,
    span: rumoca_core::Span,
) -> Result<(String, Vec<rumoca_core::Subscript>), ToDaeError> {
    let mut base = path;
    let mut reversed_groups = Vec::new();
    while let Some(scalar) = rumoca_core::parse_scalar_name(base) {
        reversed_groups.push(scalar.indices);
        base = scalar.base;
    }
    reversed_groups.reverse();
    let mut subscripts = Vec::new();
    for index in reversed_groups.into_iter().flatten() {
        subscripts.push(generated_index_subscript(
            index,
            span,
            "encoded pre() target subscript",
        )?);
    }
    Ok((base.to_string(), subscripts))
}

fn generated_index_subscript(
    index: i64,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, ToDaeError> {
    rumoca_core::Subscript::try_generated_index(index, span, context).map_err(|err| {
        if span.is_dummy() {
            ToDaeError::runtime_metadata_violation(err.to_string())
        } else {
            ToDaeError::runtime_metadata_violation_at(err.to_string(), span)
        }
    })
}

fn pre_target_span(
    expr: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    context: &'static str,
) -> Result<rumoca_core::Span, ToDaeError> {
    expr.span()
        .or_else(|| subscripts.iter().find_map(subscript_source_span))
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "{context} is missing source provenance"
            ))
        })
}

fn subscript_source_span(subscript: &rumoca_core::Subscript) -> Option<rumoca_core::Span> {
    let span = subscript.span();
    if !span.is_dummy() {
        return Some(span);
    }
    let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
        return None;
    };
    expr.span()
}

fn is_pre_parameter_name(name: &rumoca_core::VarName) -> bool {
    name.as_str().starts_with("__pre__.")
}

fn indexed_field_target_name(
    base_name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
) -> Option<rumoca_core::VarName> {
    let rendered = render_static_subscripts(subscripts)?;
    Some(rumoca_core::VarName::new(format!(
        "{}[{rendered}].{field}",
        base_name.as_str()
    )))
}

fn render_static_subscripts(subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    let mut rendered = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        rendered.push(static_subscript_index(subscript)?.to_string());
    }
    Some(rendered.join(","))
}

fn static_subscript_index(subscript: &rumoca_core::Subscript) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => Some(*value),
            _ => None,
        },
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn selected_static_if_branch<'a>(
    branches: &'a [(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &'a rumoca_core::Expression,
) -> Option<&'a rumoca_core::Expression> {
    for (condition, value) in branches {
        if static_bool_expr(condition)? {
            return Some(value);
        }
    }
    Some(else_branch)
}

fn static_bool_expr(expr: &rumoca_core::Expression) -> Option<bool> {
    rumoca_eval_dae::constant::eval_const_expr_with(expr, &|_, _| None)?.as_bool()
}

#[cfg(test)]
mod tests;
