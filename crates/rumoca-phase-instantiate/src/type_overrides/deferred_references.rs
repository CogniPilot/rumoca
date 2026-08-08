//! Resolution of references that Resolve deferred across replaceable edges.
//!
//! Resolve keeps the replaceable declaration in a deferred reference because
//! the concrete selection is instance dependent. Instantiation owns that
//! selection, so it can prove the final member declarations of equations,
//! statements, and expressions without rewriting source aliases.

use super::override_map::TypeOverrideMap;
use super::selected_class_members::resolve_member_reference_in_class;
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::visitor::ExpressionTransformer;
use rustc_hash::FxHashMap;

/// Selected class of each component declaration in one instantiation scope.
///
/// Keys are the component declaration identities Resolve records on a reference
/// root; values are the classes instantiation actually selected for those
/// occurrences.
pub(crate) type SelectedComponentTypes = FxHashMap<DefId, DefId>;

/// Resolve a reference deferred by Resolve across a replaceable class edge.
///
/// Resolve deliberately retains the replaceable declaration in `def_id`.
/// Instantiation owns the concrete selection, so it can prove the final member
/// declaration without rewriting the source alias into a rendered target name.
pub(crate) fn resolve_dynamic_expression_targets(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    expression: ast::Expression,
) -> InstantiateResult<ast::Expression> {
    let mut batch = DynamicExpressionTargetBatch::new(tree, overrides, selected_component_types);
    let expression = batch.transform_expression(expression);
    batch.finish(expression)
}

pub(crate) fn resolve_dynamic_equation_targets(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    equation: ast::Equation,
) -> InstantiateResult<ast::Equation> {
    let mut batch = DynamicExpressionTargetBatch::new(tree, overrides, selected_component_types);
    let equation = batch.transform_equation(equation);
    batch.finish(equation)
}

pub(crate) fn resolve_dynamic_statement_targets(
    tree: &ast::ClassTree,
    overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    statement: ast::Statement,
) -> InstantiateResult<ast::Statement> {
    let mut batch = DynamicExpressionTargetBatch::new(tree, overrides, selected_component_types);
    let statement = batch.transform_statement(statement);
    batch.finish(statement)
}

/// One exact-selection resolver shared by all expression surfaces that belong
/// to the same source occurrence.
///
/// Post-materialization repair groups component bindings, attributes, and
/// dimensions by their structured source scope. Reusing the resolver avoids a
/// fresh traversal owner for every optional field while preserving the first
/// exact member-proof error.
pub(crate) struct DynamicExpressionTargetBatch<'a> {
    resolver: DynamicExpressionTargetResolver<'a>,
}

impl<'a> DynamicExpressionTargetBatch<'a> {
    pub(crate) fn new(
        tree: &'a ast::ClassTree,
        overrides: &'a TypeOverrideMap,
        selected_component_types: &'a SelectedComponentTypes,
    ) -> Self {
        Self {
            resolver: DynamicExpressionTargetResolver::new(
                tree,
                overrides,
                selected_component_types,
            ),
        }
    }

    pub(crate) fn transform_expression(&mut self, expression: ast::Expression) -> ast::Expression {
        self.resolver.transform_expression(expression)
    }

    pub(crate) fn transform_optional_expression(
        &mut self,
        expression: &mut Option<ast::Expression>,
    ) {
        if let Some(value) = expression.take() {
            *expression = Some(self.resolver.transform_expression(value));
        }
    }

    pub(crate) fn transform_subscripts(&mut self, subscripts: &mut Vec<ast::Subscript>) {
        *subscripts = subscripts
            .drain(..)
            .map(|subscript| self.resolver.transform_subscript(subscript))
            .collect();
    }

    pub(crate) fn transform_equation(&mut self, equation: ast::Equation) -> ast::Equation {
        self.resolver.transform_equation(equation)
    }

    pub(crate) fn transform_statement(&mut self, statement: ast::Statement) -> ast::Statement {
        self.resolver.transform_statement(statement)
    }

    pub(crate) fn finish<T>(self, value: T) -> InstantiateResult<T> {
        self.resolver.finish(value)
    }
}

struct DynamicExpressionTargetResolver<'a> {
    tree: &'a ast::ClassTree,
    overrides: &'a TypeOverrideMap,
    selected_component_types: &'a SelectedComponentTypes,
    error: Option<Box<InstantiateError>>,
}

impl ExpressionTransformer for DynamicExpressionTargetResolver<'_> {
    fn transform_component_ref_inner(
        &mut self,
        mut reference: ast::ComponentReference,
    ) -> ast::ComponentReference {
        reference = self.walk_component_reference_subscripts(reference);
        if self.error.is_some() || reference.target_def_id().is_some() {
            return reference;
        }
        let Some(root_def_id) = reference.root_def_id() else {
            return reference;
        };
        // A replaceable class alias selects a class directly; a replaceable
        // component selects one through the type of its instantiated occurrence.
        let Some(target_class_def_id) = self
            .overrides
            .target_for_alias_def_id(root_def_id)
            .or_else(|| self.selected_component_types.get(&root_def_id).copied())
        else {
            return reference;
        };
        match resolve_member_reference_in_class(self.tree, target_class_def_id, &reference, 1) {
            Ok(identities) => {
                for (part, def_id) in reference.parts.iter_mut().skip(1).zip(identities) {
                    part.def_id = Some(def_id);
                }
            }
            Err(error) => self.error = Some(error),
        }
        reference
    }
}

impl DynamicExpressionTargetResolver<'_> {
    fn new<'a>(
        tree: &'a ast::ClassTree,
        overrides: &'a TypeOverrideMap,
        selected_component_types: &'a SelectedComponentTypes,
    ) -> DynamicExpressionTargetResolver<'a> {
        DynamicExpressionTargetResolver {
            tree,
            overrides,
            selected_component_types,
            error: None,
        }
    }

    fn finish<T>(self, value: T) -> InstantiateResult<T> {
        match self.error {
            Some(error) => Err(error),
            None => Ok(value),
        }
    }

    fn walk_component_reference_subscripts(
        &mut self,
        mut reference: ast::ComponentReference,
    ) -> ast::ComponentReference {
        for part in &mut reference.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        reference
    }

    fn transform_equation(&mut self, equation: ast::Equation) -> ast::Equation {
        match equation {
            ast::Equation::Empty => ast::Equation::Empty,
            ast::Equation::Simple { lhs, rhs } => ast::Equation::Simple {
                lhs: self.transform_expression(lhs),
                rhs: self.transform_expression(rhs),
            },
            ast::Equation::Connect { lhs, rhs } => ast::Equation::Connect {
                lhs: self.transform_component_ref_inner(lhs),
                rhs: self.transform_component_ref_inner(rhs),
            },
            ast::Equation::For { indices, equations } => ast::Equation::For {
                indices: indices
                    .into_iter()
                    .map(|index| self.transform_for_index(index))
                    .collect(),
                equations: equations
                    .into_iter()
                    .map(|equation| self.transform_equation(equation))
                    .collect(),
            },
            ast::Equation::When(blocks) => ast::Equation::When(
                blocks
                    .into_iter()
                    .map(|block| self.transform_equation_block(block))
                    .collect(),
            ),
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => ast::Equation::If {
                cond_blocks: cond_blocks
                    .into_iter()
                    .map(|block| self.transform_equation_block(block))
                    .collect(),
                else_block: else_block.map(|equations| {
                    equations
                        .into_iter()
                        .map(|equation| self.transform_equation(equation))
                        .collect()
                }),
            },
            ast::Equation::FunctionCall { comp, args, span } => ast::Equation::FunctionCall {
                comp: self.transform_component_ref_inner(comp),
                args: args
                    .into_iter()
                    .map(|argument| self.transform_expression(argument))
                    .collect(),
                span,
            },
            ast::Equation::Assert {
                condition,
                message,
                level,
            } => ast::Equation::Assert {
                condition: self.transform_expression(condition),
                message: self.transform_expression(message),
                level: level.map(|level| self.transform_expression(level)),
            },
        }
    }

    fn transform_equation_block(&mut self, block: ast::EquationBlock) -> ast::EquationBlock {
        ast::EquationBlock {
            cond: self.transform_expression(block.cond),
            eqs: block
                .eqs
                .into_iter()
                .map(|equation| self.transform_equation(equation))
                .collect(),
        }
    }

    fn transform_statement(&mut self, statement: ast::Statement) -> ast::Statement {
        match statement {
            ast::Statement::Empty => ast::Statement::Empty,
            ast::Statement::Assignment { comp, value } => ast::Statement::Assignment {
                comp: self.transform_component_ref_inner(comp),
                value: self.transform_expression(value),
            },
            ast::Statement::Return { token } => ast::Statement::Return { token },
            ast::Statement::Break { token } => ast::Statement::Break { token },
            ast::Statement::For { indices, equations } => ast::Statement::For {
                indices: indices
                    .into_iter()
                    .map(|index| self.transform_for_index(index))
                    .collect(),
                equations: equations
                    .into_iter()
                    .map(|statement| self.transform_statement(statement))
                    .collect(),
            },
            ast::Statement::While(block) => {
                ast::Statement::While(self.transform_statement_block(block))
            }
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => ast::Statement::If {
                cond_blocks: cond_blocks
                    .into_iter()
                    .map(|block| self.transform_statement_block(block))
                    .collect(),
                else_block: else_block.map(|statements| {
                    statements
                        .into_iter()
                        .map(|statement| self.transform_statement(statement))
                        .collect()
                }),
            },
            ast::Statement::When(blocks) => ast::Statement::When(
                blocks
                    .into_iter()
                    .map(|block| self.transform_statement_block(block))
                    .collect(),
            ),
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => ast::Statement::FunctionCall {
                comp: self.transform_component_ref_inner(comp),
                args: args
                    .into_iter()
                    .map(|argument| self.transform_expression(argument))
                    .collect(),
                outputs: outputs
                    .into_iter()
                    .map(|output| self.transform_expression(output))
                    .collect(),
            },
            ast::Statement::Reinit { variable, value } => ast::Statement::Reinit {
                variable: self.transform_component_ref_inner(variable),
                value: self.transform_expression(value),
            },
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => ast::Statement::Assert {
                condition: self.transform_expression(condition),
                message: self.transform_expression(message),
                level: level.map(|level| Box::new(self.transform_expression(*level))),
            },
        }
    }

    fn transform_statement_block(&mut self, block: ast::StatementBlock) -> ast::StatementBlock {
        ast::StatementBlock {
            cond: self.transform_expression(block.cond),
            stmts: block
                .stmts
                .into_iter()
                .map(|statement| self.transform_statement(statement))
                .collect(),
        }
    }
}
