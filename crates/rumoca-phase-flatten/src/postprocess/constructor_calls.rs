use std::collections::HashSet;

use crate::{ast, flat};
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(crate) fn mark_record_constructor_calls(flat: &mut flat::Model, tree: &ast::ClassTree) {
    let constructor_def_ids = tree
        .def_map
        .keys()
        .copied()
        .filter(|def_id| {
            tree.get_class_by_def_id(*def_id)
                .is_some_and(|class_def| class_def.class_type == rumoca_core::ClassType::Record)
        })
        .collect::<HashSet<_>>();
    let mut constructor_names: HashSet<String> = tree
        .def_map
        .iter()
        .filter(|&(def_id, _qualified_name)| constructor_def_ids.contains(def_id))
        .map(|(_def_id, qualified_name)| qualified_name.clone())
        .collect();
    constructor_names.extend(
        flat.functions
            .values()
            .filter(|function| function.is_constructor)
            .map(|function| function.name.as_str().to_string()),
    );
    if constructor_names.is_empty() && constructor_def_ids.is_empty() {
        return;
    }

    let marker = ConstructorMarker {
        constructor_names: &constructor_names,
        constructor_def_ids: &constructor_def_ids,
    };
    for var in flat.variables.values_mut() {
        marker.mark_opt_expr(&mut var.binding);
        marker.mark_opt_expr(&mut var.start);
        marker.mark_opt_expr(&mut var.min);
        marker.mark_opt_expr(&mut var.max);
        marker.mark_opt_expr(&mut var.nominal);
    }
    for eq in &mut flat.equations {
        marker.mark_expr(&mut eq.residual);
    }
    for eq in &mut flat.initial_equations {
        marker.mark_expr(&mut eq.residual);
    }
    for assert_eq in &mut flat.assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for assert_eq in &mut flat.initial_assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for algorithm in &mut flat.algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for algorithm in &mut flat.initial_algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            marker.mark_expr(&mut branch.condition);
            marker.mark_when_equations(&mut branch.equations);
        }
    }
    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            marker.mark_opt_expr(&mut input.default);
        }
        for output in &mut function.outputs {
            marker.mark_opt_expr(&mut output.default);
        }
        for local in &mut function.locals {
            marker.mark_opt_expr(&mut local.default);
        }
        marker.mark_statements(&mut function.body);
    }
}

#[derive(Clone, Copy)]
struct ConstructorMarker<'a> {
    constructor_names: &'a HashSet<String>,
    constructor_def_ids: &'a HashSet<rumoca_core::DefId>,
}

impl ConstructorMarker<'_> {
    fn mark_opt_expr(self, expr: &mut Option<rumoca_core::Expression>) {
        if let Some(expr) = expr {
            self.mark_expr(expr);
        }
    }

    fn mark_expr(mut self, expr: &mut rumoca_core::Expression) {
        *expr = self.rewrite_expression(expr);
    }

    fn mark_statements(mut self, statements: &mut [rumoca_core::Statement]) {
        for statement in statements {
            *statement = self.rewrite_statement(statement);
        }
    }

    fn mark_when_equations(self, equations: &mut [flat::WhenEquation]) {
        for equation in equations {
            match equation {
                flat::WhenEquation::Assign { value, .. }
                | flat::WhenEquation::Reinit { value, .. } => self.mark_expr(value),
                flat::WhenEquation::Assert {
                    condition,
                    message,
                    level,
                    ..
                } => self.mark_assert(condition, message, level),
                flat::WhenEquation::Conditional {
                    branches,
                    else_branch,
                    ..
                } => self.mark_conditional_when_equation(branches, else_branch),
                flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                    self.mark_expr(function);
                }
                flat::WhenEquation::Terminate { message, .. } => self.mark_expr(message),
            }
        }
    }

    fn mark_assert(
        self,
        condition: &mut rumoca_core::Expression,
        message: &mut rumoca_core::Expression,
        level: &mut Option<Box<rumoca_core::Expression>>,
    ) {
        self.mark_expr(condition);
        self.mark_expr(message);
        if let Some(level) = level {
            self.mark_expr(level);
        }
    }

    fn mark_conditional_when_equation(
        self,
        branches: &mut [(rumoca_core::Expression, Vec<flat::WhenEquation>)],
        else_branch: &mut Option<Vec<flat::WhenEquation>>,
    ) {
        for (condition, branch_equations) in branches {
            self.mark_expr(condition);
            self.mark_when_equations(branch_equations);
        }
        if let Some(else_branch) = else_branch {
            self.mark_when_equations(else_branch);
        }
    }

    fn is_constructor_call(self, name: &rumoca_core::Reference) -> bool {
        name.target_def_id()
            .is_some_and(|def_id| self.constructor_def_ids.contains(&def_id))
            || self.constructor_names.contains(name.as_str())
    }
}

impl ExpressionRewriter for ConstructorMarker<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: self.rewrite_expressions(args),
                is_constructor: *is_constructor || self.is_constructor_call(name),
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for ConstructorMarker<'_> {}
