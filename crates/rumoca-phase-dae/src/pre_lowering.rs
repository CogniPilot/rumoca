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
    collect_pre_targets_from_equations(&dae.continuous.equations, &mut pre_targets, false);
    collect_pre_targets_from_equations(&dae.discrete.real_updates, &mut pre_targets, true);
    collect_pre_targets_from_equations(&dae.discrete.valued_updates, &mut pre_targets, true);
    collect_pre_targets_from_equations(&dae.conditions.equations, &mut pre_targets, true);
    for expr in &dae.conditions.relations {
        collect_pre_targets_from_expr(expr, &mut pre_targets, false);
    }
    for expr in &dae.clocks.triggered_conditions {
        collect_pre_targets_from_expr(expr, &mut pre_targets, true);
    }
    collect_pre_targets_from_event_actions(&dae.events.event_actions, &mut pre_targets);
    collect_pre_targets_from_equations(&dae.initialization.equations, &mut pre_targets, true);

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
        let (source_span, dims, start, start_span) = (
            var.source_span,
            var.dims.clone(),
            var.start.clone(),
            var.start_attribute_span(),
        );

        let pre_var = dae::Variable {
            name: pre_param_name.clone(),
            component_ref: None,
            source_span,
            dims,
            start,
            start_span,
            fixed: Some(true),
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: Some(format!("pre() of {}", target.source_name.as_str())),
            is_tunable: false,
        };
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

    let relation_memories = relation_memory_exprs(dae);

    rewrite_equations(
        &mut dae.continuous.equations,
        &pre_targets,
        &relation_memories,
    );
    rewrite_equations(
        &mut dae.discrete.real_updates,
        &pre_targets,
        &relation_memories,
    );
    rewrite_equations(
        &mut dae.discrete.valued_updates,
        &pre_targets,
        &relation_memories,
    );
    rewrite_equations(
        &mut dae.conditions.equations,
        &pre_targets,
        &relation_memories,
    );
    for expr in &mut dae.conditions.relations {
        *expr = rewrite_pre_expr(expr, &pre_targets, &relation_memories);
    }
    for expr in &mut dae.clocks.triggered_conditions {
        *expr = rewrite_pre_expr(expr, &pre_targets, &relation_memories);
    }
    rewrite_event_actions(
        &mut dae.events.event_actions,
        &pre_targets,
        &relation_memories,
    );
    rewrite_equations(
        &mut dae.initialization.equations,
        &pre_targets,
        &relation_memories,
    );
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
) {
    for eq in equations {
        collect_pre_targets_from_expr(&eq.rhs, targets, allow_continuous_target);
    }
}

fn collect_pre_targets_from_expr(
    expr: &rumoca_core::Expression,
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
    allow_continuous_target: bool,
) {
    ExpressionVisitor::visit_expression(
        &mut PreTargetCollector {
            targets,
            allow_continuous_target,
        },
        expr,
    );
}

fn collect_pre_targets_from_event_actions(
    actions: &[dae::DaeEventAction],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) {
    for action in actions {
        collect_pre_targets_from_expr(&action.condition, targets, true);
    }
}

struct PreTargetCollector<'a> {
    targets: &'a mut IndexMap<rumoca_core::VarName, PreTarget>,
    allow_continuous_target: bool,
}

impl ExpressionVisitor for PreTargetCollector<'_> {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        match function {
            rumoca_core::BuiltinFunction::Pre => {
                if let Some(arg) = args.first()
                    && let Some((name, _)) = pre_target_parts(arg)
                {
                    let span = arg.span().unwrap_or(rumoca_core::Span::DUMMY);
                    insert_pre_target(self.targets, name, span, true, self.allow_continuous_target);
                } else {
                    collect_args_as_pre_values(args, self.targets);
                }
            }
            rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change => {
                collect_first_arg_as_pre_value(args, self.targets);
            }
            rumoca_core::BuiltinFunction::Sample => {
                collect_sample_value_as_pre_value(args, self.targets);
            }
            _ => {}
        }
        self.walk_builtin_call(function, args);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        match rumoca_core::source_temporal_function_short_name(name.as_str()) {
            Some("pre") => {
                if let Some(arg) = args.first()
                    && let Some((name, _)) = pre_target_parts(arg)
                {
                    let span = arg.span().unwrap_or(rumoca_core::Span::DUMMY);
                    insert_pre_target(self.targets, name, span, true, self.allow_continuous_target);
                } else {
                    collect_args_as_pre_values(args, self.targets);
                }
            }
            Some("edge" | "change" | "previous") => {
                collect_first_arg_as_pre_value(args, self.targets);
            }
            Some("sample") => {
                collect_sample_value_as_pre_value(args, self.targets);
            }
            _ => {}
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

fn collect_args_as_pre_values(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) {
    for arg in args {
        collect_pre_targets_from_pre_expr(arg, targets);
    }
}

fn collect_first_arg_as_pre_value(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) {
    if let Some(arg) = args.first() {
        collect_pre_targets_from_pre_expr(arg, targets);
    }
}

fn collect_sample_value_as_pre_value(
    args: &[rumoca_core::Expression],
    targets: &mut IndexMap<rumoca_core::VarName, PreTarget>,
) {
    if matches!(args.len(), 1 | 2) && !args.first().is_some_and(is_time_var_ref) {
        collect_first_arg_as_pre_value(args, targets);
    }
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
) {
    ExpressionVisitor::visit_expression(&mut PreValueCollector { targets }, expr);
}

struct PreValueCollector<'a> {
    targets: &'a mut IndexMap<rumoca_core::VarName, PreTarget>,
}

impl ExpressionVisitor for PreValueCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if let Some((name, _)) = pre_target_parts(expr) {
            let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
            insert_pre_target(self.targets, name, span, false, true);
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
        if let Some(source_name) =
            singleton_scalarized_field_name(dae, target_name, target.require_discrete)
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

fn singleton_scalarized_field_name(
    dae: &dae::Dae,
    target_name: &rumoca_core::VarName,
    require_discrete: bool,
) -> Option<rumoca_core::VarName> {
    let (prefix, field) = rumoca_core::split_last_top_level(target_name.as_str())?;
    let mut collector = ScalarizedFieldCandidateCollector {
        prefix,
        field,
        require_discrete,
        candidates: Vec::new(),
    };
    collector.visit_variables(&dae.variables);
    match collector.candidates.as_slice() {
        [name] => Some(name.clone()),
        _ => None,
    }
}

struct ScalarizedFieldCandidateCollector<'a> {
    prefix: &'a str,
    field: &'a str,
    require_discrete: bool,
    candidates: Vec<rumoca_core::VarName>,
}

impl DaeVisitor for ScalarizedFieldCandidateCollector<'_> {
    fn visit_variable(
        &mut self,
        partition: dae::DaeVariablePartition,
        name: &rumoca_core::VarName,
        _variable: &dae::Variable,
    ) {
        if (!self.require_discrete || is_pre_target_partition(partition))
            && scalarized_field_name_matches(name.as_str(), self.prefix, self.field)
        {
            self.candidates.push(name.clone());
        }
    }
}

fn scalarized_field_name_matches(candidate: &str, prefix: &str, field: &str) -> bool {
    let Some(rest) = candidate.strip_prefix(prefix) else {
        return false;
    };
    let Some(rest) = rest.strip_prefix('[') else {
        return false;
    };
    let Some(subscripts) = rest.strip_suffix(&format!(".{field}")) else {
        return false;
    };
    subscripts.ends_with(']')
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
) {
    for eq in equations {
        eq.rhs = rewrite_pre_expr(&eq.rhs, targets, relation_memories);
    }
}

fn relation_memory_exprs(
    dae_model: &dae::Dae,
) -> Vec<(rumoca_core::Expression, rumoca_core::Expression)> {
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
                ),
            ));
        }
        relation_offset += scalar_count;
    }
    memories
}

fn relation_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if scalar_count <= 1 {
        return rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span,
        };
    }
    let dims = dae_model
        .variables
        .discrete_valued
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .map(|var| var.dims.as_slice())
        .unwrap_or(&[]);
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)
        .unwrap_or_else(|| vec![flat_index.saturating_add(1)])
        .into_iter()
        .map(|index| rumoca_core::Subscript::generated_index(index as i64, span))
        .collect();
    rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span,
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
) {
    for action in actions {
        action.condition = rewrite_pre_expr(&action.condition, targets, relation_memories);
    }
}

fn rewrite_pre_expr(
    expr: &rumoca_core::Expression,
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &[(rumoca_core::Expression, rumoca_core::Expression)],
) -> rumoca_core::Expression {
    PreExpressionRewriter {
        targets,
        relation_memories,
    }
    .rewrite_expression(expr)
}

struct PreExpressionRewriter<'a> {
    targets: &'a IndexMap<rumoca_core::VarName, PreTarget>,
    relation_memories: &'a [(rumoca_core::Expression, rumoca_core::Expression)],
}

impl ExpressionRewriter for PreExpressionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
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
            return relation_memory_pre_expr(&memory, span);
        }
        let rewritten_args = self.rewrite_expressions(args);
        if let Some((name, subscripts)) = rewritten_args.first().and_then(pre_target_parts)
            && let Some(target) = self.targets.get(&name)
        {
            return pre_var_ref(&target.source_name, &subscripts, span);
        }
        if let [arg] = rewritten_args.as_slice() {
            return rewrite_pre_expr_value(arg, self.targets);
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
                    rhs: Box::new(relation_memory_pre_expr(&memory, span)),
                    span,
                }),
                span,
            };
        }
        // A source `edge(relation)` that reaches this pass without condition
        // memory must still keep Modelica edge semantics instead of becoming a
        // level-sensitive relation.
        let current = self.rewrite_expression(arg);
        let previous = rewrite_pre_expr_value(&current, self.targets);
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
            rhs: Box::new(rewrite_pre_expr_value(arg, self.targets)),
            span,
        }
    }

    fn rewrite_sample_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
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
            return rewrite_pre_expr_value(arg, self.targets);
        }
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("previous"),
            args: Vec::new(),
            is_constructor,
            span,
        }
    }
}

fn relation_memory_pre_expr(
    memory: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if let Some((name, subscripts)) = pre_target_parts(memory) {
        return pre_var_ref(&name, &subscripts, span);
    }
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![memory.clone()],
        span,
    }
}

fn rewrite_pre_expr_value(
    expr: &rumoca_core::Expression,
    targets: &IndexMap<rumoca_core::VarName, PreTarget>,
) -> rumoca_core::Expression {
    PreValueRewriter { targets }.rewrite_expression(expr)
}

struct PreValueRewriter<'a> {
    targets: &'a IndexMap<rumoca_core::VarName, PreTarget>,
}

impl ExpressionRewriter for PreValueRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let Some((name, subscripts)) = pre_target_parts(expr)
            && let Some(target) = self.targets.get(&name)
        {
            return pre_var_ref(
                &target.source_name,
                &subscripts,
                expr.span().unwrap_or(rumoca_core::Span::DUMMY),
            );
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                span,
                ..
            } => rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: *span,
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
        name: rumoca_core::Reference::new(format!("__pre__.{}", name.as_str())),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn pre_target_parts(
    expr: &rumoca_core::Expression,
) -> Option<(rumoca_core::VarName, Vec<rumoca_core::Subscript>)> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some(pre_target_parts_from_name(name.var_name(), subscripts)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let (name, mut target_subscripts) = pre_target_parts(base)?;
            target_subscripts.extend(subscripts.clone());
            Some((name, target_subscripts))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let (base_name, subscripts) = pre_target_parts(base)?;
            if subscripts.is_empty() {
                Some((
                    rumoca_core::VarName::new(format!("{}.{}", base_name.as_str(), field)),
                    Vec::new(),
                ))
            } else {
                Some((
                    indexed_field_target_name(&base_name, &subscripts, field)?,
                    Vec::new(),
                ))
            }
        }
        _ => None,
    }
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

fn pre_target_parts_from_name(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
) -> (rumoca_core::VarName, Vec<rumoca_core::Subscript>) {
    let (base_name, mut encoded_subscripts) = split_encoded_integer_subscripts(name.as_str());
    encoded_subscripts.extend_from_slice(subscripts);
    (rumoca_core::VarName::new(base_name), encoded_subscripts)
}

fn split_encoded_integer_subscripts(path: &str) -> (String, Vec<rumoca_core::Subscript>) {
    let mut base = path;
    let mut reversed_groups = Vec::new();
    while let Some(stripped) = base.strip_suffix(']') {
        let Some(open_idx) = stripped.rfind('[') else {
            break;
        };
        let index_text = &stripped[open_idx + 1..];
        let Some(group) = parse_encoded_integer_subscript_group(index_text) else {
            break;
        };
        reversed_groups.push(group);
        base = &stripped[..open_idx];
    }
    reversed_groups.reverse();
    (
        base.to_string(),
        reversed_groups.into_iter().flatten().collect(),
    )
}

fn parse_encoded_integer_subscript_group(index_text: &str) -> Option<Vec<rumoca_core::Subscript>> {
    index_text
        .split(',')
        .map(|part| {
            let index = part.trim().parse::<i64>().ok()?;
            Some(rumoca_core::Subscript::generated_index(
                index,
                rumoca_core::Span::DUMMY,
            ))
        })
        .collect()
}

#[cfg(test)]
#[cfg(test)]
mod tests;
