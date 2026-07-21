//! Inline hidden scalar component-output algebraics into their consumers.
//!
//! Flattening preserves component block outputs such as `gain.y` as ordinary
//! residual equations even when the output itself is not a DAE runtime variable:
//! `gain.y - gain.k * gain.u = 0`. If a sampled parent reads that output, the
//! GALEC targets see an otherwise discrete model with one leftover continuous
//! residual and reject it before projection. This pass removes that producer
//! artifact by substituting the explicit scalar definition into discrete/event
//! contexts, then dropping the defining residual when the hidden output is not
//! needed by the continuous or initialization system.

use std::collections::{HashMap, HashSet};

use rumoca_core::{
    Expression, ExpressionRewriter, Literal, OpBinary, OpUnary, Reference, Span, Subscript, VarName,
};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::DaeExpressionRewriter;

#[derive(Clone)]
struct HiddenDefinition {
    rhs: Expression,
    equation_index: usize,
}

pub(crate) fn inline_hidden_component_algebraics(dae: &mut dae::Dae) {
    let protected_indices = structured_equation_indices(dae);
    let declared = declared_runtime_names(dae);
    let removable = removable_hidden_variables(dae);
    let definitions = hidden_scalar_definitions(dae, &declared, &removable, &protected_indices);
    let definitions = removable_context_definitions(dae, definitions);
    if definitions.is_empty() {
        return;
    }
    let definitions = expanded_acyclic_definitions(definitions);
    if definitions.is_empty() {
        return;
    }
    let definition_expressions = definitions
        .iter()
        .map(|(target, definition)| (target.clone(), definition.rhs.clone()))
        .collect::<HashMap<_, _>>();

    remove_inlined_definitions(
        dae,
        definitions
            .values()
            .map(|definition| definition.equation_index)
            .collect(),
        definitions.keys().cloned().collect(),
    );
    rewrite_non_continuous_contexts(dae, &definition_expressions);
}

fn hidden_scalar_definitions(
    dae: &dae::Dae,
    declared: &HashSet<String>,
    removable: &HashSet<String>,
    protected_indices: &HashSet<usize>,
) -> HashMap<String, HiddenDefinition> {
    let mut definitions = HashMap::new();
    let mut duplicates = HashSet::new();
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        if equation.scalar_count != 1 || protected_indices.contains(&index) {
            continue;
        }
        let Some((target, rhs)) = residual_scalar_definition(&equation.rhs) else {
            continue;
        };
        if !is_component_owned_definition(equation, target.as_str()) {
            continue;
        }
        if runtime_metadata_requires_variable(dae, target.as_str()) {
            continue;
        }
        if (declared.contains(target.as_str()) && !removable.contains(target.as_str()))
            || references_target(&rhs, target.as_str())
        {
            continue;
        }
        if definitions
            .insert(
                target.as_str().to_owned(),
                HiddenDefinition {
                    rhs,
                    equation_index: index,
                },
            )
            .is_some()
        {
            duplicates.insert(target.as_str().to_owned());
        }
    }
    for duplicate in duplicates {
        definitions.remove(&duplicate);
    }
    definitions
}

fn removable_context_definitions(
    dae: &dae::Dae,
    definitions: HashMap<String, HiddenDefinition>,
) -> HashMap<String, HiddenDefinition> {
    definitions
        .into_iter()
        .filter(|(target, definition)| {
            !continuous_equations_reference_target(dae, target, definition.equation_index)
                && !equations_reference_target(&dae.initialization.equations, target)
                && !variable_attributes_reference_target(&dae.variables, target)
                && !event_or_clock_contexts_reference_target(dae, target)
        })
        .collect()
}

fn continuous_equations_reference_target(
    dae: &dae::Dae,
    target: &str,
    definition_index: usize,
) -> bool {
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .any(|(index, equation)| {
            index != definition_index && references_target(&equation.rhs, target)
        })
}

fn equations_reference_target(equations: &[dae::Equation], target: &str) -> bool {
    equations
        .iter()
        .any(|equation| references_target(&equation.rhs, target))
}

fn variable_attributes_reference_target(variables: &dae::DaeVariables, target: &str) -> bool {
    for partition in [
        &variables.states,
        &variables.algebraics,
        &variables.inputs,
        &variables.outputs,
        &variables.parameters,
        &variables.constants,
        &variables.discrete_reals,
        &variables.discrete_valued,
    ] {
        if partition
            .values()
            .any(|variable| variable_references_target(variable, target))
        {
            return true;
        }
    }
    false
}

fn variable_references_target(variable: &dae::Variable, target: &str) -> bool {
    [
        variable.start.as_ref(),
        variable.min.as_ref(),
        variable.max.as_ref(),
        variable.nominal.as_ref(),
    ]
    .into_iter()
    .flatten()
    .any(|expression| references_target(expression, target))
}

fn event_or_clock_contexts_reference_target(dae: &dae::Dae, target: &str) -> bool {
    expression_slots_reference_target(&dae.events.synthetic_root_conditions, target)
        || event_actions_reference_target(&dae.events.event_actions, target)
        || expression_slots_reference_target(&dae.clocks.constructor_exprs, target)
        || expression_slots_reference_target(&dae.clocks.triggered_conditions, target)
}

fn expression_slots_reference_target(slots: &[Expression], target: &str) -> bool {
    slots
        .iter()
        .any(|expression| references_target(expression, target))
}

fn event_actions_reference_target(actions: &[dae::DaeEventAction], target: &str) -> bool {
    actions.iter().any(|action| {
        references_target(&action.condition, target)
            || match &action.kind {
                dae::DaeEventActionKind::Assert { message }
                | dae::DaeEventActionKind::Terminate { message } => {
                    references_target(message, target)
                }
            }
    })
}

fn is_component_owned_definition(equation: &dae::Equation, target: &str) -> bool {
    let Some(component) = definition_origin_component(&equation.origin) else {
        return false;
    };
    !component.is_empty()
        && target
            .strip_prefix(component)
            .is_some_and(|suffix| suffix.starts_with('.') || suffix.starts_with('['))
}

fn runtime_metadata_requires_variable(dae: &dae::Dae, target: &str) -> bool {
    dae.clocks.intervals.contains_key(target) || dae.clocks.timings.contains_key(target)
}

fn definition_origin_component(origin: &str) -> Option<&str> {
    if let Some(component) = origin.strip_prefix("equation from ") {
        return Some(component);
    }
    if let Some(component) = origin.strip_prefix("explicit equation from ") {
        return Some(component);
    }
    origin
        .strip_prefix("algorithm assignment (algorithm from ")
        .map(|component| component.split(')').next().unwrap_or(component))
}

fn residual_scalar_definition(residual: &Expression) -> Option<(VarName, Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    subscripts
        .is_empty()
        .then(|| (name.var_name().clone(), rhs.as_ref().clone()))
}

fn expanded_acyclic_definitions(
    mut definitions: HashMap<String, HiddenDefinition>,
) -> HashMap<String, HiddenDefinition> {
    let keys = definitions.keys().cloned().collect::<HashSet<_>>();
    for _ in 0..keys.len() {
        let previous = definitions
            .iter()
            .map(|(target, definition)| (target.clone(), definition.rhs.clone()))
            .collect::<HashMap<_, _>>();
        for (target, definition) in &mut definitions {
            definition.rhs = DefinitionExpander {
                definitions: &previous,
                skip: target,
            }
            .rewrite_expression(&definition.rhs);
        }
    }
    definitions.retain(|target, definition| {
        !keys
            .iter()
            .any(|candidate| candidate != target && references_target(&definition.rhs, candidate))
    });
    definitions
}

fn remove_inlined_definitions(
    dae: &mut dae::Dae,
    definition_indices: HashSet<usize>,
    targets: HashSet<String>,
) {
    let mut removed = Vec::new();
    let mut retained = Vec::with_capacity(dae.continuous.equations.len());
    for (index, equation) in std::mem::take(&mut dae.continuous.equations)
        .into_iter()
        .enumerate()
    {
        if definition_indices.contains(&index) {
            removed.push(index);
        } else {
            retained.push(equation);
        }
    }
    dae.continuous.equations = retained;
    if removed.is_empty() {
        return;
    }
    remove_hidden_variables(dae, &targets);
    for family in &mut dae.continuous.structured_equations {
        let before = removed
            .iter()
            .filter(|index| **index < family.first_equation_index)
            .count();
        family.first_equation_index = family.first_equation_index.saturating_sub(before);
    }
}

fn remove_hidden_variables(dae: &mut dae::Dae, targets: &HashSet<String>) {
    dae.variables
        .outputs
        .retain(|name, _| !targets.contains(name.as_str()));
    dae.variables
        .algebraics
        .retain(|name, _| !targets.contains(name.as_str()));
}

fn rewrite_non_continuous_contexts(dae: &mut dae::Dae, definitions: &HashMap<String, Expression>) {
    let mut rewriter = HiddenAlgebraicSubstituter { definitions };
    rewriter.rewrite_equations(&mut dae.initialization.equations);
    rewriter.rewrite_equations(&mut dae.discrete.real_updates);
    rewriter.rewrite_equations(&mut dae.discrete.valued_updates);
    rewriter.rewrite_equations(&mut dae.conditions.equations);
    rewriter.rewrite_expression_slots(&mut dae.conditions.relations);
}

fn declared_runtime_names(dae: &dae::Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for partition in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        names.extend(partition.keys().map(|name| name.as_str().to_owned()));
    }
    names
}

fn removable_hidden_variables(dae: &dae::Dae) -> HashSet<String> {
    dae.variables
        .outputs
        .iter()
        .chain(dae.variables.algebraics.iter())
        .filter(|(_, variable)| {
            variable.causality == dae::VariableCausality::Output
                && variable
                    .component_ref
                    .as_ref()
                    .is_some_and(|reference| reference.parts.len() > 1)
        })
        .map(|(name, _)| name.as_str().to_owned())
        .collect()
}

fn structured_equation_indices(dae: &dae::Dae) -> HashSet<usize> {
    let mut indices = HashSet::new();
    for family in &dae.continuous.structured_equations {
        let count = family
            .scalar_view_row_count()
            .expect("validated structured family row count");
        indices.extend(family.first_equation_index..family.first_equation_index + count);
    }
    indices
}

struct HiddenAlgebraicSubstituter<'a> {
    definitions: &'a HashMap<String, Expression>,
}

impl ExpressionRewriter for HiddenAlgebraicSubstituter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if let Some(target) = reference_target_name(name, subscripts)
            && let Some(replacement) = self.definitions.get(target.as_str())
        {
            return replacement.clone();
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

impl DaeExpressionRewriter for HiddenAlgebraicSubstituter<'_> {}

struct DefinitionExpander<'a> {
    definitions: &'a HashMap<String, Expression>,
    skip: &'a str,
}

impl ExpressionRewriter for DefinitionExpander<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if let Some(target) = reference_target_name(name, subscripts)
            && target != self.skip
            && let Some(replacement) = self.definitions.get(target.as_str())
        {
            return replacement.clone();
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

fn reference_target_name(name: &Reference, subscripts: &[Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_owned());
    }
    let rendered = render_static_subscripts(subscripts)?;
    Some(format!("{}[{rendered}]", name.as_str()))
}

fn render_static_subscripts(subscripts: &[Subscript]) -> Option<String> {
    let mut rendered = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        rendered.push(static_subscript_index(subscript)?.to_string());
    }
    Some(rendered.join(","))
}

fn static_subscript_index(subscript: &Subscript) -> Option<i64> {
    match subscript {
        Subscript::Index { value, .. } => Some(*value),
        Subscript::Expr { expr, .. } => static_integer_expression(expr),
        Subscript::Colon { .. } => None,
    }
}

fn static_integer_expression(expression: &Expression) -> Option<i64> {
    let value = match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => *value as f64,
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => *value,
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => -(static_integer_expression(rhs)? as f64),
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => static_integer_expression(rhs)? as f64,
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = static_integer_expression(lhs)? as f64;
            let rhs = static_integer_expression(rhs)? as f64;
            match op {
                OpBinary::Add => lhs + rhs,
                OpBinary::Sub => lhs - rhs,
                OpBinary::Mul => lhs * rhs,
                OpBinary::Div => lhs / rhs,
                _ => return None,
            }
        }
        _ => return None,
    };

    (value.is_finite() && value.fract() == 0.0).then_some(value as i64)
}

fn references_target(expression: &Expression, wanted: &str) -> bool {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => reference_target_name(name, subscripts).is_some_and(|target| target == wanted),
        Expression::Binary { lhs, rhs, .. } => {
            references_target(lhs, wanted) || references_target(rhs, wanted)
        }
        Expression::Unary { rhs, .. } => references_target(rhs, wanted),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| references_target(arg, wanted))
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                references_target(condition, wanted) || references_target(value, wanted)
            }) || references_target(else_branch, wanted)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| references_target(element, wanted)),
        Expression::Range {
            start, step, end, ..
        } => {
            references_target(start, wanted)
                || step
                    .as_deref()
                    .is_some_and(|step| references_target(step, wanted))
                || references_target(end, wanted)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            references_target(expr, wanted)
                || indices
                    .iter()
                    .any(|index| references_target(&index.range, wanted))
                || filter
                    .as_deref()
                    .is_some_and(|filter| references_target(filter, wanted))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            references_target(base, wanted)
                || subscripts.iter().any(|subscript| match subscript {
                    Subscript::Expr { expr, .. } => references_target(expr, wanted),
                    Subscript::Index { .. } | Subscript::Colon { .. } => false,
                })
        }
        Expression::FieldAccess { base, .. } => references_target(base, wanted),
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aggregate_residuals_are_not_treated_as_hidden_scalar_definitions() {
        let mut dae = dae::Dae::default();
        dae.continuous.equations.push(dae::Equation::residual_array(
            sub(var_ref("comp.x"), var_ref("comp.y")),
            Span::DUMMY,
            "aggregate record equation",
            2,
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert_eq!(dae.continuous.equations.len(), 1);
        assert_eq!(dae.continuous.equations[0].scalar_count, 2);
    }

    #[test]
    fn top_level_output_field_definitions_are_not_hidden_component_definitions() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("y.re"),
            output_var("y.re", dae::VariableCausality::Output),
        );
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("y.re"), var_ref("source.re")),
            Span::DUMMY,
            "top-level model equation",
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert!(dae.variables.outputs.contains_key(&VarName::new("y.re")));
        assert_eq!(dae.continuous.equations.len(), 1);
    }

    #[test]
    fn nested_component_output_definitions_are_hidden() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("gain.y"),
            output_var("gain.y", dae::VariableCausality::Output),
        );
        dae.variables.discrete_reals.insert(
            VarName::new("out"),
            output_var("out", dae::VariableCausality::Output),
        );
        dae.discrete.real_updates.push(dae::Equation::explicit(
            VarName::new("out"),
            var_ref("gain.y"),
            Span::DUMMY,
            "sampled parent assignment",
        ));
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("gain.y"), var_ref("gain.u")),
            Span::DUMMY,
            "equation from gain",
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert!(!dae.variables.outputs.contains_key(&VarName::new("gain.y")));
        assert!(dae.continuous.equations.is_empty());
        assert!(!references_target(
            &dae.discrete.real_updates[0].rhs,
            "gain.y"
        ));
    }

    #[test]
    fn indexed_component_output_references_are_rewritten() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("replicator.y[1]"),
            output_var("replicator.y[1]", dae::VariableCausality::Output),
        );
        dae.variables.discrete_reals.insert(
            VarName::new("out"),
            output_var("out", dae::VariableCausality::Output),
        );
        dae.discrete.real_updates.push(dae::Equation::explicit(
            VarName::new("out"),
            indexed_var_ref("replicator.y", 1),
            Span::DUMMY,
            "sampled parent assignment",
        ));
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("replicator.y[1]"), var_ref("replicator.u[1]")),
            Span::DUMMY,
            "equation from replicator",
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert!(
            !dae.variables
                .outputs
                .contains_key(&VarName::new("replicator.y[1]"))
        );
        assert!(dae.continuous.equations.is_empty());
        assert!(!references_target(
            &dae.discrete.real_updates[0].rhs,
            "replicator.y[1]"
        ));
    }

    #[test]
    fn nested_component_algorithm_output_definitions_are_hidden() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("alg.y"),
            output_var("alg.y", dae::VariableCausality::Output),
        );
        dae.variables.discrete_reals.insert(
            VarName::new("out"),
            output_var("out", dae::VariableCausality::Output),
        );
        dae.discrete.real_updates.push(dae::Equation::explicit(
            VarName::new("out"),
            var_ref("alg.y"),
            Span::DUMMY,
            "algorithm when-assignment (algorithm from )",
        ));
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("alg.y"), var_ref("alg.u")),
            Span::DUMMY,
            "algorithm assignment (algorithm from alg)",
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert!(!dae.variables.outputs.contains_key(&VarName::new("alg.y")));
        assert!(dae.continuous.equations.is_empty());
        assert!(!references_target(
            &dae.discrete.real_updates[0].rhs,
            "alg.y"
        ));
    }

    #[test]
    fn clocked_component_output_definitions_remain_runtime_variables() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("ramp.y"),
            output_var("ramp.y", dae::VariableCausality::Output),
        );
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("ramp.y"), var_ref("ramp.simTime")),
            Span::DUMMY,
            "equation from ramp",
        ));
        dae.clocks.intervals.insert("ramp.y".to_string(), 0.1);

        inline_hidden_component_algebraics(&mut dae);

        assert!(dae.variables.outputs.contains_key(&VarName::new("ramp.y")));
        assert_eq!(dae.continuous.equations.len(), 1);
        assert!(references_target(
            &dae.continuous.equations[0].rhs,
            "ramp.y"
        ));
    }

    #[test]
    fn continuous_consumers_keep_hidden_target_definitions() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            VarName::new("gain.y"),
            output_var("gain.y", dae::VariableCausality::Output),
        );
        dae.variables.algebraics.insert(
            VarName::new("out"),
            output_var("out", dae::VariableCausality::Local),
        );
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("gain.y"), var_ref("gain.u")),
            Span::DUMMY,
            "equation from gain",
        ));
        dae.continuous.equations.push(dae::Equation::residual(
            sub(var_ref("gain.y"), var_ref("out")),
            Span::DUMMY,
            "connection equation: gain.y = out",
        ));

        inline_hidden_component_algebraics(&mut dae);

        assert!(dae.variables.outputs.contains_key(&VarName::new("gain.y")));
        assert_eq!(dae.continuous.equations.len(), 2);
        assert_eq!(
            dae.continuous.equations[1].origin,
            "connection equation: gain.y = out"
        );
        assert!(references_target(
            &dae.continuous.equations[1].rhs,
            "gain.y"
        ));
    }

    fn output_var(name: &str, causality: dae::VariableCausality) -> dae::Variable {
        let mut variable = dae::Variable::new(VarName::new(name), Span::DUMMY);
        variable.causality = causality;
        variable.component_ref =
            rumoca_core::component_reference_from_flat_name(&VarName::new(name), Span::DUMMY);
        variable
    }

    fn sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: Span::DUMMY,
        }
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn indexed_var_ref(name: &str, index: i64) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: vec![Subscript::Expr {
                expr: Box::new(Expression::Literal {
                    value: Literal::Integer(index),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        }
    }
}
