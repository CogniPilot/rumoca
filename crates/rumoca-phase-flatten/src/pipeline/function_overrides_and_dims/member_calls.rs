use super::*;
use rumoca_ir_ast::visitor::{
    CalleeSite, ComponentReferenceSite, SemanticReferenceEditor, transform_callee_in_place,
    transform_expression_in_place, transform_for_index_in_place,
};

pub(super) struct QualifyReplaceableFunctionModifier<'a> {
    pub(super) receiver_alias: &'a ComponentPath,
}

impl QualifyReplaceableFunctionModifier<'_> {
    /// The receiver-qualified display spelling of a single-part reference, or
    /// `None` when the reference is not receiver-qualifiable.
    fn receiver_qualified_name(
        &self,
        cr: rumoca_ir_ast::ComponentReferenceView<'_>,
    ) -> Option<String> {
        if cr.part_count() != 1 || cr.local() || self.receiver_alias.is_root() {
            return None;
        }
        let root = cr.parts().next()?;
        let display_name = self
            .receiver_alias
            .join(&ComponentPath::from_parts([root.ident_text()]));
        Some(display_name.to_flat_string())
    }
}

impl ExpressionTransformer for QualifyReplaceableFunctionModifier<'_> {
    fn transform_component_reference(
        &mut self,
        mut cr: SemanticReferenceEditor<'_>,
        _site: ComponentReferenceSite,
    ) {
        let Some(display_name) = self.receiver_qualified_name(cr.view()) else {
            return;
        };
        cr.set_qualified_display_name(display_name);
    }

    // Callees keep the default no-op hook: a function name is not a receiver
    // member, so it is never receiver-qualified. The kernel still traverses
    // callee part subscripts and call arguments, so references inside an
    // indexed callee's subscripts are receiver-qualified like any other value
    // reference.
}

pub(crate) fn mark_member_function_calls_in_instance_equation(
    inst_eq: &rumoca_ir_ast::InstanceEquation,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> rumoca_ir_ast::InstanceEquation {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    let mut equation = inst_eq.equation.clone();
    marker.mark_equation(&mut equation);
    rumoca_ir_ast::InstanceEquation {
        equation,
        origin: inst_eq.origin.clone(),
        source_scope: inst_eq.source_scope.clone(),
        source_scope_id: inst_eq.source_scope_id,
        span: inst_eq.span,
    }
}

pub(crate) fn mark_member_function_calls_in_instance_statements(
    statements: &[rumoca_ir_ast::InstanceStatement],
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> Vec<rumoca_ir_ast::InstanceStatement> {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    statements
        .iter()
        .map(|statement| {
            let mut marked = statement.statement.clone();
            marker.mark_statement(&mut marked);
            rumoca_ir_ast::InstanceStatement {
                statement: marked,
                origin: statement.origin.clone(),
                source_scope: statement.source_scope.clone(),
                source_scope_id: statement.source_scope_id,
                span: statement.span,
            }
        })
        .collect()
}

pub(super) struct MemberFunctionCallMarker<'a> {
    pub(super) tree: &'a ClassTree,
    pub(super) class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    pub(super) override_functions: &'a OverrideFunctionMap,
}

impl MemberFunctionCallMarker<'_> {
    fn resolve_member_function_def_id(
        &self,
        comp: rumoca_ir_ast::ComponentReferenceView<'_>,
    ) -> Option<rumoca_core::DefId> {
        let mut parts = comp.parts();
        let (receiver, member) = match (parts.next(), parts.next(), parts.next()) {
            (Some(receiver), Some(member), None) => (receiver, member),
            _ => return None,
        };
        // The receiver's recorded declaration identity selects the alias
        // slot; a same-spelled alias at another nesting depth is a different
        // slot and must never answer for it.
        let receiver_type = self.override_functions.get(&receiver.def_id()?)?;
        let function_name = resolve_function_in_package_chain(
            self.tree,
            self.class_index,
            receiver_type,
            member.ident_text(),
        )?;
        self.tree.get_def_id_by_name(&function_name)
    }

    fn mark_equation(&mut self, equation: &mut rumoca_ir_ast::Equation) {
        match equation {
            rumoca_ir_ast::Equation::Empty | rumoca_ir_ast::Equation::Connect { .. } => {}
            rumoca_ir_ast::Equation::Simple { lhs, rhs } => {
                transform_expression_in_place(self, lhs);
                transform_expression_in_place(self, rhs);
            }
            rumoca_ir_ast::Equation::For { indices, equations } => {
                for index in indices {
                    transform_for_index_in_place(self, index);
                }
                self.mark_equations(equations);
            }
            rumoca_ir_ast::Equation::When(blocks) => {
                for block in blocks {
                    self.mark_equation_block(block);
                }
            }
            rumoca_ir_ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.mark_equation_block(block);
                }
                if let Some(equations) = else_block {
                    self.mark_equations(equations);
                }
            }
            rumoca_ir_ast::Equation::FunctionCall { comp, args, .. } => {
                transform_callee_in_place(self, comp);
                for argument in args {
                    transform_expression_in_place(self, argument);
                }
            }
            rumoca_ir_ast::Equation::Assert {
                condition,
                message,
                level,
            } => {
                transform_expression_in_place(self, condition);
                transform_expression_in_place(self, message);
                if let Some(level) = level {
                    transform_expression_in_place(self, level);
                }
            }
        }
    }

    fn mark_equation_block(&mut self, block: &mut rumoca_ir_ast::EquationBlock) {
        transform_expression_in_place(self, &mut block.cond);
        self.mark_equations(&mut block.eqs);
    }

    fn mark_equations(&mut self, equations: &mut [rumoca_ir_ast::Equation]) {
        for equation in equations {
            self.mark_equation(equation);
        }
    }

    fn mark_statement(&mut self, statement: &mut rumoca_ir_ast::Statement) {
        match statement {
            rumoca_ir_ast::Statement::Empty
            | rumoca_ir_ast::Statement::Return { .. }
            | rumoca_ir_ast::Statement::Break { .. } => {}
            rumoca_ir_ast::Statement::Assignment { value, .. } => {
                transform_expression_in_place(self, value);
            }
            rumoca_ir_ast::Statement::For { indices, equations } => {
                for index in indices {
                    transform_for_index_in_place(self, index);
                }
                self.mark_statements(equations);
            }
            rumoca_ir_ast::Statement::While(block) => self.mark_statement_block(block),
            rumoca_ir_ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.mark_statement_block(block);
                }
                if let Some(statements) = else_block {
                    self.mark_statements(statements);
                }
            }
            rumoca_ir_ast::Statement::When(blocks) => {
                for block in blocks {
                    self.mark_statement_block(block);
                }
            }
            rumoca_ir_ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => {
                transform_callee_in_place(self, comp);
                for argument in args {
                    transform_expression_in_place(self, argument);
                }
                for output in outputs {
                    transform_expression_in_place(self, output);
                }
            }
            rumoca_ir_ast::Statement::Reinit { value, .. } => {
                transform_expression_in_place(self, value);
            }
            rumoca_ir_ast::Statement::Assert {
                condition,
                message,
                level,
            } => {
                transform_expression_in_place(self, condition);
                transform_expression_in_place(self, message);
                if let Some(level) = level {
                    transform_expression_in_place(self, level.as_mut());
                }
            }
        }
    }

    fn mark_statement_block(&mut self, block: &mut rumoca_ir_ast::StatementBlock) {
        transform_expression_in_place(self, &mut block.cond);
        self.mark_statements(&mut block.stmts);
    }

    fn mark_statements(&mut self, statements: &mut [rumoca_ir_ast::Statement]) {
        for statement in statements {
            self.mark_statement(statement);
        }
    }
}

impl ExpressionTransformer for MemberFunctionCallMarker<'_> {
    fn transform_callee(&mut self, mut callee: SemanticReferenceEditor<'_>, _site: CalleeSite) {
        let Some(def_id) = self.resolve_member_function_def_id(callee.view()) else {
            return;
        };
        if let Some(mut target) = callee.part_identity_slots().last() {
            target.set_def_id(def_id);
        }
    }
}
