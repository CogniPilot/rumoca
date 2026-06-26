use super::*;

pub(super) struct QualifyReplaceableFunctionModifier<'a> {
    pub(super) receiver_alias: &'a ComponentPath,
}

impl ExpressionTransformer for QualifyReplaceableFunctionModifier<'_> {
    fn transform_component_ref_inner(
        &mut self,
        mut cr: rumoca_ir_ast::ComponentReference,
    ) -> rumoca_ir_ast::ComponentReference {
        if cr.parts.len() == 1 && !cr.local && !self.receiver_alias.is_root() {
            let location = cr.parts[0].ident.location.clone();
            let mut prefixed_parts: Vec<_> = self
                .receiver_alias
                .parts()
                .iter()
                .map(|part| rumoca_ir_ast::ComponentRefPart {
                    ident: Token {
                        text: std::sync::Arc::from(part.as_str()),
                        location: location.clone(),
                        token_number: 0,
                        token_type: 0,
                    },
                    subs: None,
                })
                .collect();
            prefixed_parts.extend(cr.parts);
            cr.parts = prefixed_parts;
        } else if cr.parts.len() > 1
            && !cr.local
            && !self.receiver_alias.is_root()
            && let Some(receiver_leaf) = self.receiver_alias.parts().last()
        {
            let already_scoped = cr.parts.len() >= self.receiver_alias.parts().len()
                && cr
                    .parts
                    .iter()
                    .zip(self.receiver_alias.parts())
                    .all(|(part, receiver_part)| part.ident.text.as_ref() == receiver_part);
            if !already_scoped && cr.parts[0].ident.text.as_ref() == receiver_leaf {
                let location = cr.parts[0].ident.location.clone();
                let mut scoped_parts: Vec<_> = self
                    .receiver_alias
                    .parts()
                    .iter()
                    .map(|part| rumoca_ir_ast::ComponentRefPart {
                        ident: Token {
                            text: std::sync::Arc::from(part.as_str()),
                            location: location.clone(),
                            token_number: 0,
                            token_type: 0,
                        },
                        subs: None,
                    })
                    .collect();
                scoped_parts.extend(cr.parts.into_iter().skip(1));
                cr.parts = scoped_parts;
            }
        }
        for part in &mut cr.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        cr
    }

    fn transform_function_call(
        &mut self,
        comp: rumoca_ir_ast::ComponentReference,
        args: Vec<rumoca_ir_ast::Expression>,
        span: rumoca_core::Span,
    ) -> rumoca_ir_ast::Expression {
        rumoca_ir_ast::Expression::FunctionCall {
            comp,
            args: args
                .into_iter()
                .map(|arg| self.transform_expression(arg))
                .collect(),
            span,
        }
    }
}

pub(crate) fn mark_member_function_calls_in_instance_equation(
    inst_eq: &rumoca_ir_ast::InstanceEquation,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> rumoca_ir_ast::InstanceEquation {
    rumoca_ir_ast::InstanceEquation {
        equation: mark_member_function_calls_in_equation(
            inst_eq.equation.clone(),
            tree,
            class_index,
            override_functions,
        ),
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
    statements
        .iter()
        .map(|statement| rumoca_ir_ast::InstanceStatement {
            statement: mark_member_function_calls_in_statement(
                statement.statement.clone(),
                tree,
                class_index,
                override_functions,
            ),
            origin: statement.origin.clone(),
            source_scope: statement.source_scope.clone(),
            source_scope_id: statement.source_scope_id,
            span: statement.span,
        })
        .collect()
}

pub(super) fn mark_member_function_calls_in_equation(
    equation: rumoca_ir_ast::Equation,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> rumoca_ir_ast::Equation {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    match equation {
        rumoca_ir_ast::Equation::Empty => rumoca_ir_ast::Equation::Empty,
        rumoca_ir_ast::Equation::Simple { lhs, rhs } => rumoca_ir_ast::Equation::Simple {
            lhs: marker.transform_expression(lhs),
            rhs: marker.transform_expression(rhs),
        },
        rumoca_ir_ast::Equation::Connect { lhs, rhs } => {
            rumoca_ir_ast::Equation::Connect { lhs, rhs }
        }
        rumoca_ir_ast::Equation::For { indices, equations } => rumoca_ir_ast::Equation::For {
            indices: indices
                .into_iter()
                .map(|index| marker.transform_for_index(index))
                .collect(),
            equations: mark_member_function_calls_in_equations(
                equations,
                tree,
                class_index,
                override_functions,
            ),
        },
        rumoca_ir_ast::Equation::When(blocks) => {
            rumoca_ir_ast::Equation::When(mark_member_function_calls_in_equation_blocks(
                blocks,
                tree,
                class_index,
                override_functions,
            ))
        }
        rumoca_ir_ast::Equation::If {
            cond_blocks,
            else_block,
        } => rumoca_ir_ast::Equation::If {
            cond_blocks: mark_member_function_calls_in_equation_blocks(
                cond_blocks,
                tree,
                class_index,
                override_functions,
            ),
            else_block: else_block.map(|equations| {
                mark_member_function_calls_in_equations(
                    equations,
                    tree,
                    class_index,
                    override_functions,
                )
            }),
        },
        rumoca_ir_ast::Equation::FunctionCall { comp, args } => {
            rumoca_ir_ast::Equation::FunctionCall {
                comp: marker.mark_component_function_call(comp),
                args: args
                    .into_iter()
                    .map(|arg| marker.transform_expression(arg))
                    .collect(),
            }
        }
        rumoca_ir_ast::Equation::Assert {
            condition,
            message,
            level,
        } => rumoca_ir_ast::Equation::Assert {
            condition: marker.transform_expression(condition),
            message: marker.transform_expression(message),
            level: level.map(|expr| marker.transform_expression(expr)),
        },
    }
}

pub(super) fn mark_member_function_calls_in_equations(
    equations: Vec<rumoca_ir_ast::Equation>,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> Vec<rumoca_ir_ast::Equation> {
    equations
        .into_iter()
        .map(|equation| {
            mark_member_function_calls_in_equation(equation, tree, class_index, override_functions)
        })
        .collect()
}

pub(super) fn mark_member_function_calls_in_equation_blocks(
    blocks: Vec<rumoca_ir_ast::EquationBlock>,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> Vec<rumoca_ir_ast::EquationBlock> {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    blocks
        .into_iter()
        .map(|block| rumoca_ir_ast::EquationBlock {
            cond: marker.transform_expression(block.cond),
            eqs: mark_member_function_calls_in_equations(
                block.eqs,
                tree,
                class_index,
                override_functions,
            ),
        })
        .collect()
}

pub(super) fn mark_member_function_calls_in_statement(
    statement: rumoca_ir_ast::Statement,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> rumoca_ir_ast::Statement {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    match statement {
        rumoca_ir_ast::Statement::Empty => rumoca_ir_ast::Statement::Empty,
        rumoca_ir_ast::Statement::Assignment { comp, value } => {
            rumoca_ir_ast::Statement::Assignment {
                comp,
                value: marker.transform_expression(value),
            }
        }
        rumoca_ir_ast::Statement::Return { token } => rumoca_ir_ast::Statement::Return { token },
        rumoca_ir_ast::Statement::Break { token } => rumoca_ir_ast::Statement::Break { token },
        rumoca_ir_ast::Statement::For { indices, equations } => rumoca_ir_ast::Statement::For {
            indices: indices
                .into_iter()
                .map(|index| marker.transform_for_index(index))
                .collect(),
            equations: mark_member_function_calls_in_statements(
                equations,
                tree,
                class_index,
                override_functions,
            ),
        },
        rumoca_ir_ast::Statement::While(block) => {
            rumoca_ir_ast::Statement::While(mark_member_function_calls_in_statement_block(
                block,
                tree,
                class_index,
                override_functions,
            ))
        }
        rumoca_ir_ast::Statement::If {
            cond_blocks,
            else_block,
        } => rumoca_ir_ast::Statement::If {
            cond_blocks: mark_member_function_calls_in_statement_blocks(
                cond_blocks,
                tree,
                class_index,
                override_functions,
            ),
            else_block: else_block.map(|statements| {
                mark_member_function_calls_in_statements(
                    statements,
                    tree,
                    class_index,
                    override_functions,
                )
            }),
        },
        rumoca_ir_ast::Statement::When(blocks) => {
            rumoca_ir_ast::Statement::When(mark_member_function_calls_in_statement_blocks(
                blocks,
                tree,
                class_index,
                override_functions,
            ))
        }
        rumoca_ir_ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => rumoca_ir_ast::Statement::FunctionCall {
            comp: marker.mark_component_function_call(comp),
            args: args
                .into_iter()
                .map(|arg| marker.transform_expression(arg))
                .collect(),
            outputs: outputs
                .into_iter()
                .map(|output| marker.transform_expression(output))
                .collect(),
        },
        rumoca_ir_ast::Statement::Reinit { variable, value } => rumoca_ir_ast::Statement::Reinit {
            variable,
            value: marker.transform_expression(value),
        },
        rumoca_ir_ast::Statement::Assert {
            condition,
            message,
            level,
        } => rumoca_ir_ast::Statement::Assert {
            condition: marker.transform_expression(condition),
            message: marker.transform_expression(message),
            level: level.map(|expr| Box::new(marker.transform_expression(*expr))),
        },
    }
}

pub(super) fn mark_member_function_calls_in_statements(
    statements: Vec<rumoca_ir_ast::Statement>,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> Vec<rumoca_ir_ast::Statement> {
    statements
        .into_iter()
        .map(|statement| {
            mark_member_function_calls_in_statement(
                statement,
                tree,
                class_index,
                override_functions,
            )
        })
        .collect()
}

pub(super) fn mark_member_function_calls_in_statement_block(
    block: rumoca_ir_ast::StatementBlock,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> rumoca_ir_ast::StatementBlock {
    let mut marker = MemberFunctionCallMarker {
        tree,
        class_index,
        override_functions,
    };
    rumoca_ir_ast::StatementBlock {
        cond: marker.transform_expression(block.cond),
        stmts: mark_member_function_calls_in_statements(
            block.stmts,
            tree,
            class_index,
            override_functions,
        ),
    }
}

pub(super) fn mark_member_function_calls_in_statement_blocks(
    blocks: Vec<rumoca_ir_ast::StatementBlock>,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_functions: &OverrideFunctionMap,
) -> Vec<rumoca_ir_ast::StatementBlock> {
    blocks
        .into_iter()
        .map(|block| {
            mark_member_function_calls_in_statement_block(
                block,
                tree,
                class_index,
                override_functions,
            )
        })
        .collect()
}

pub(super) struct MemberFunctionCallMarker<'a> {
    pub(super) tree: &'a ClassTree,
    pub(super) class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    pub(super) override_functions: &'a OverrideFunctionMap,
}

impl MemberFunctionCallMarker<'_> {
    pub(in crate::pipeline::function_overrides_and_dims) fn mark_component_function_call(
        &self,
        mut comp: rumoca_ir_ast::ComponentReference,
    ) -> rumoca_ir_ast::ComponentReference {
        if let Some(def_id) = self.resolve_member_function_def_id(&comp) {
            comp.def_id = Some(def_id);
        }
        comp
    }

    fn resolve_member_function_def_id(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
    ) -> Option<rumoca_core::DefId> {
        let [receiver, member] = comp.parts.as_slice() else {
            return None;
        };
        let receiver_type = self.override_functions.get(receiver.ident.text.as_ref())?;
        let function_name = resolve_function_in_package_chain(
            self.tree,
            self.class_index,
            receiver_type,
            member.ident.text.as_ref(),
        )?;
        self.tree.get_def_id_by_name(&function_name)
    }
}

impl ExpressionTransformer for MemberFunctionCallMarker<'_> {
    fn transform_function_call(
        &mut self,
        comp: rumoca_ir_ast::ComponentReference,
        args: Vec<rumoca_ir_ast::Expression>,
        span: rumoca_core::Span,
    ) -> rumoca_ir_ast::Expression {
        let comp = self.transform_component_ref_inner(comp);
        let comp = self.mark_component_function_call(comp);
        rumoca_ir_ast::Expression::FunctionCall {
            comp,
            args: args
                .into_iter()
                .map(|arg| self.transform_expression(arg))
                .collect(),
            span,
        }
    }
}
