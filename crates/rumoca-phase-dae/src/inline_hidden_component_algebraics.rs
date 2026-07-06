//! Inline hidden scalar component-output algebraics into their consumers.
//!
//! Flattening preserves component block outputs such as `gain.y` as ordinary
//! residual equations even when the output itself is not a DAE runtime variable:
//! `gain.y - gain.k * gain.u = 0`. If a sampled parent reads that output, the
//! GALEC targets see an otherwise discrete model with one leftover continuous
//! residual and reject it before projection. This pass removes that producer
//! artifact by substituting the explicit scalar definition into every DAE
//! expression slot, then dropping the defining residual.

use std::collections::{HashMap, HashSet};

use rumoca_core::{Expression, ExpressionRewriter, OpBinary, Reference, Span, Subscript, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::DaeExpressionRewriter;

pub(crate) fn inline_hidden_component_algebraics(dae: &mut dae::Dae) {
    let protected_indices = structured_equation_indices(dae);
    let declared = declared_runtime_names(dae);
    let removable = removable_hidden_variables(dae);
    let definitions = hidden_scalar_definitions(dae, &declared, &removable, &protected_indices);
    if definitions.is_empty() {
        return;
    }
    let definitions = expanded_acyclic_definitions(definitions);
    if definitions.is_empty() {
        return;
    }

    remove_inlined_definitions(
        dae,
        definitions.keys().cloned().collect(),
        &protected_indices,
    );
    HiddenAlgebraicSubstituter {
        definitions: &definitions,
    }
    .rewrite_dae(dae);
}

fn hidden_scalar_definitions(
    dae: &dae::Dae,
    declared: &HashSet<String>,
    removable: &HashSet<String>,
    protected_indices: &HashSet<usize>,
) -> HashMap<String, Expression> {
    let mut definitions = HashMap::new();
    let mut duplicates = HashSet::new();
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        if equation.scalar_count != 1 || protected_indices.contains(&index) {
            continue;
        }
        let Some((target, rhs)) = residual_scalar_definition(&equation.rhs) else {
            continue;
        };
        if (declared.contains(target.as_str()) && !removable.contains(target.as_str()))
            || references_scalar(&rhs, target.as_str())
        {
            continue;
        }
        if definitions
            .insert(target.as_str().to_owned(), rhs)
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
    mut definitions: HashMap<String, Expression>,
) -> HashMap<String, Expression> {
    let keys = definitions.keys().cloned().collect::<HashSet<_>>();
    for _ in 0..keys.len() {
        let previous = definitions.clone();
        for (target, rhs) in &mut definitions {
            *rhs = DefinitionExpander {
                definitions: &previous,
                skip: target,
            }
            .rewrite_expression(rhs);
        }
    }
    definitions.retain(|target, rhs| {
        !keys
            .iter()
            .any(|candidate| candidate != target && references_scalar(rhs, candidate))
    });
    definitions
}

fn remove_inlined_definitions(
    dae: &mut dae::Dae,
    targets: HashSet<String>,
    protected_indices: &HashSet<usize>,
) {
    let mut removed = Vec::new();
    let mut retained = Vec::with_capacity(dae.continuous.equations.len());
    for (index, equation) in std::mem::take(&mut dae.continuous.equations)
        .into_iter()
        .enumerate()
    {
        let remove = equation.scalar_count == 1
            && !protected_indices.contains(&index)
            && residual_scalar_definition(&equation.rhs)
                .is_some_and(|(target, _)| targets.contains(target.as_str()));
        if remove {
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
        let count = family.equation_counts.iter().sum::<usize>();
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
        if subscripts.is_empty()
            && let Some(replacement) = self.definitions.get(name.as_str())
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
        if subscripts.is_empty()
            && name.as_str() != self.skip
            && let Some(replacement) = self.definitions.get(name.as_str())
        {
            return replacement.clone();
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

fn references_scalar(expression: &Expression, wanted: &str) -> bool {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => subscripts.is_empty() && name.as_str() == wanted,
        Expression::Binary { lhs, rhs, .. } => {
            references_scalar(lhs, wanted) || references_scalar(rhs, wanted)
        }
        Expression::Unary { rhs, .. } => references_scalar(rhs, wanted),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| references_scalar(arg, wanted))
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                references_scalar(condition, wanted) || references_scalar(value, wanted)
            }) || references_scalar(else_branch, wanted)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| references_scalar(element, wanted)),
        Expression::Range {
            start, step, end, ..
        } => {
            references_scalar(start, wanted)
                || step
                    .as_deref()
                    .is_some_and(|step| references_scalar(step, wanted))
                || references_scalar(end, wanted)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            references_scalar(expr, wanted)
                || indices
                    .iter()
                    .any(|index| references_scalar(&index.range, wanted))
                || filter
                    .as_deref()
                    .is_some_and(|filter| references_scalar(filter, wanted))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            references_scalar(base, wanted)
                || subscripts.iter().any(|subscript| match subscript {
                    Subscript::Expr { expr, .. } => references_scalar(expr, wanted),
                    Subscript::Index { .. } | Subscript::Colon { .. } => false,
                })
        }
        Expression::FieldAccess { base, .. } => references_scalar(base, wanted),
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
}
