//! Flatten-exit pass: attach structured component references to every
//! variable reference the phase emitted by rendered name.
//!
//! Flatten owns its name encoding. Connector/array expansion historically
//! rendered element references (`sum.u[2]`) as plain strings and downstream
//! phases re-derived the structure (the resolver scalar-name parse, balance
//! prefix matching). This pass resolves each rendered reference against the
//! flat variable table once, at the producing phase's boundary, so consumers
//! receive structured references and the string-parsing bridges can reject
//! unstructured leftovers instead of guessing.

use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(crate) fn attach_structured_references(flat: &mut flat::Model) {
    let index = StructuredRefIndex::build(flat);

    let mut rewriter = StructuredRefRewriter { index: &index };
    let mut variables = std::mem::take(&mut flat.variables);
    for var in variables.values_mut() {
        rewrite_opt_expr(&mut var.binding, &mut rewriter);
        rewrite_opt_expr(&mut var.start, &mut rewriter);
        rewrite_opt_expr(&mut var.min, &mut rewriter);
        rewrite_opt_expr(&mut var.max, &mut rewriter);
        rewrite_opt_expr(&mut var.nominal, &mut rewriter);
    }
    flat.variables = variables;

    for equation in &mut flat.equations {
        equation.residual = rewriter.rewrite_expression(&equation.residual);
    }
    for equation in &mut flat.initial_equations {
        equation.residual = rewriter.rewrite_expression(&equation.residual);
    }
    for assert_eq in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        assert_eq.condition = rewriter.rewrite_expression(&assert_eq.condition);
        assert_eq.message = rewriter.rewrite_expression(&assert_eq.message);
        rewrite_opt_expr(&mut assert_eq.level, &mut rewriter);
    }
    for when_clause in &mut flat.when_clauses {
        when_clause.condition = rewriter.rewrite_expression(&when_clause.condition);
        for equation in &mut when_clause.equations {
            rewrite_when_equation(equation, &mut rewriter);
        }
    }
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
}

fn rewrite_when_equation(
    equation: &mut flat::WhenEquation,
    rewriter: &mut StructuredRefRewriter<'_>,
) {
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            *value = rewriter.rewrite_expression(value);
        }
        flat::WhenEquation::Assert {
            condition, message, ..
        } => {
            *condition = rewriter.rewrite_expression(condition);
            *message = rewriter.rewrite_expression(message);
        }
        flat::WhenEquation::Terminate { message, .. } => {
            *message = rewriter.rewrite_expression(message);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches.iter_mut() {
                *condition = rewriter.rewrite_expression(condition);
                for nested in equations {
                    rewrite_when_equation(nested, rewriter);
                }
            }
            for nested in else_branch {
                rewrite_when_equation(nested, rewriter);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            *function = rewriter.rewrite_expression(function);
        }
    }
}

fn rewrite_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    rewriter: &mut StructuredRefRewriter<'_>,
) {
    if let Some(inner) = expr.as_mut() {
        *inner = rewriter.rewrite_expression(inner);
    }
}

/// Structured references for every flat variable, plus element references for
/// array variables (`base[i]` -> base reference + literal index subscripts).
struct StructuredRefIndex {
    by_name: std::collections::HashMap<rumoca_core::VarNameId, rumoca_core::ComponentReference>,
}

impl StructuredRefIndex {
    fn build(flat: &flat::Model) -> Self {
        let mut by_name = std::collections::HashMap::new();
        for (name, var) in &flat.variables {
            let Some(reference) = var.component_ref.as_ref() else {
                continue;
            };
            by_name.insert(name.id(), reference.clone());
        }
        Self { by_name }
    }

    fn structured_for(
        &self,
        name: &rumoca_core::VarName,
    ) -> Option<rumoca_core::ComponentReference> {
        if let Some(reference) = self.by_name.get(&name.id()) {
            return Some(reference.clone());
        }
        // Element of an array variable: recover `(base, indices)` once, here
        // at the producing boundary, and compose the element reference from
        // the base variable's structured reference.
        let scalar = rumoca_core::parse_scalar_name(name.as_str())?;
        let base = rumoca_core::VarName::new(scalar.base);
        let base_ref = self.by_name.get(&base.id())?;
        let mut reference = base_ref.clone();
        let part = reference.parts.last_mut()?;
        part.subs.extend(
            scalar
                .indices
                .iter()
                .map(|index| rumoca_core::Subscript::generated_index(*index, reference.span)),
        );
        Some(reference)
    }
}

struct StructuredRefRewriter<'a> {
    index: &'a StructuredRefIndex,
}

impl ExpressionRewriter for StructuredRefRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if name.has_structure() || name.is_generated() {
            return self.walk_expression(expr);
        }
        let Some(reference) = self.index.structured_for(name.var_name()) else {
            return self.walk_expression(expr);
        };
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(name.as_str(), reference),
            subscripts: subscripts
                .iter()
                .map(|sub| self.rewrite_subscript(sub))
                .collect(),
            span: *span,
        }
    }
}

impl StatementRewriter for StructuredRefRewriter<'_> {}
