use std::collections::BTreeSet;

use rumoca_core::{
    BuiltinFunction, Expression, ExpressionRewriter, ExpressionVisitor, Reference, Span, Subscript,
    Variability,
};
use rumoca_ir_flat as flat;

pub(crate) fn freeze_current_discrete_selections(flat: &mut flat::Model) {
    let mut chains = std::mem::take(&mut flat.when_chains);
    let selections = {
        let mut rewriter = SelectedReads {
            flat,
            selections: Vec::new(),
        };
        for chain in &mut chains {
            for branch in chain.branches_mut() {
                branch.condition = rewriter.rewrite_expression(&branch.condition);
                rewrite_equations(&mut rewriter, &mut branch.equations);
            }
        }
        rewriter.selections
    };
    flat.when_chains = chains;
    flat.parameter_branch_selections.extend(selections);
}

fn rewrite_equations(rewriter: &mut SelectedReads<'_>, equations: &mut [flat::WhenEquation]) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. }
            | flat::WhenEquation::AssignElement { value, .. }
            | flat::WhenEquation::Reinit { value, .. } => {
                *value = rewriter.rewrite_expression(value)
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, nested) in branches {
                    *condition = rewriter.rewrite_expression(condition);
                    rewrite_equations(rewriter, nested);
                }
                if let Some(nested) = else_branch {
                    rewrite_equations(rewriter, nested);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function)
            }
            _ => {}
        }
    }
}

struct SelectedReads<'a> {
    flat: &'a flat::Model,
    selections: Vec<flat::ParameterBranchSelection>,
}

impl SelectedReads<'_> {
    fn selection(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Option<Vec<Subscript>> {
        let variable = self.flat.variables.get(name.var_name())?;
        if !variable.is_discrete_type
            || matches!(variable.causality, rumoca_core::Causality::Input(_))
            || name.instance_id() != Some(variable.instance_id)
            || subscripts.is_empty()
            || subscripts.len() != variable.dims.len()
        {
            return None;
        }
        let mut parameters = ParameterReads {
            flat: self.flat,
            names: BTreeSet::new(),
            constants: rustc_hash::FxHashSet::default(),
        };
        let indices = subscripts
            .iter()
            .map(|subscript| match subscript {
                Subscript::Index { value, span } => Some(Subscript::Index {
                    value: *value,
                    span: *span,
                }),
                Subscript::Expr { expr, span } => {
                    parameters.visit_expression(expr);
                    rumoca_eval_flat::flat_int::try_eval_flat_expr_i64(expr, self.flat, 0)
                        .map(|value| Subscript::Index { value, span: *span })
                }
                _ => None,
            })
            .collect::<Option<Vec<_>>>()?;
        if !parameters.names.is_empty() {
            self.selections.push(flat::ParameterBranchSelection {
                span,
                kind: flat::StructuralParameterUse::DiscreteDependencySubscript,
                references: parameters
                    .names
                    .into_iter()
                    .map(|name| vec![name])
                    .collect(),
            });
        }
        Some(indices)
    }
}

impl ExpressionRewriter for SelectedReads<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        match expression {
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre | BuiltinFunction::Previous | BuiltinFunction::Sample,
                ..
            } => expression.clone(),
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => match self.selection(name, subscripts, *span) {
                Some(subscripts) => Expression::VarRef {
                    name: name.clone(),
                    subscripts,
                    span: *span,
                },
                None => self.walk_expression(expression),
            },
            Expression::Index {
                base,
                subscripts,
                span,
            } => {
                if let Expression::VarRef {
                    name,
                    subscripts: inner,
                    ..
                } = base.as_ref()
                    && inner.is_empty()
                    && let Some(subscripts) = self.selection(name, subscripts, *span)
                {
                    return Expression::VarRef {
                        name: name.clone(),
                        subscripts,
                        span: *span,
                    };
                }
                self.walk_expression(expression)
            }
            _ => self.walk_expression(expression),
        }
    }
}

struct ParameterReads<'a> {
    flat: &'a flat::Model,
    names: BTreeSet<String>,
    constants: rustc_hash::FxHashSet<rumoca_core::InstanceId>,
}

impl ParameterReads<'_> {
    fn visit_constant_binding(&mut self, variable: &flat::Variable) {
        if self.constants.insert(variable.instance_id)
            && let Some(binding) = &variable.binding
        {
            self.visit_expression(binding);
        }
    }
}

impl ExpressionVisitor for ParameterReads<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        if let Some(variable) = self.flat.variables.get(name.var_name()) {
            match variable.variability {
                Variability::Parameter(_) => {
                    self.names.insert(variable.name.to_string());
                }
                Variability::Constant(_) => self.visit_constant_binding(variable),
                _ => {}
            }
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        let skip = usize::from(matches!(
            function,
            BuiltinFunction::Size | BuiltinFunction::Ndims
        ));
        for argument in args.iter().skip(skip) {
            self.visit_expression(argument);
        }
    }
}

#[cfg(test)]
mod tests;
