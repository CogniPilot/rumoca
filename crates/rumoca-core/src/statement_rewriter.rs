use crate::{
    ComponentRefPart, ComponentReference, ExpressionRewriter, FallibleExpressionRewriter, ForIndex,
    Statement, StatementBlock,
};

pub trait StatementRewriter: ExpressionRewriter {
    fn rewrite_statements(&mut self, statements: &[Statement]) -> Vec<Statement> {
        statements
            .iter()
            .map(|statement| self.rewrite_statement(statement))
            .collect()
    }

    fn rewrite_statement(&mut self, statement: &Statement) -> Statement {
        self.walk_statement(statement)
    }

    fn walk_statement(&mut self, statement: &Statement) -> Statement {
        match statement {
            Statement::Empty { span } => Statement::Empty { span: *span },
            Statement::Assignment { comp, value, span } => Statement::Assignment {
                comp: self.rewrite_component_reference(comp),
                value: self.rewrite_expression(value),
                span: *span,
            },
            Statement::Return { span } => Statement::Return { span: *span },
            Statement::Break { span } => Statement::Break { span: *span },
            Statement::For {
                indices,
                equations,
                span,
            } => Statement::For {
                indices: self.rewrite_for_indices(indices),
                equations: self.rewrite_statements(equations),
                span: *span,
            },
            Statement::While { block, span } => Statement::While {
                block: self.rewrite_statement_block(block),
                span: *span,
            },
            Statement::If {
                cond_blocks,
                else_block,
                span,
            } => Statement::If {
                cond_blocks: self.rewrite_statement_blocks(cond_blocks),
                else_block: else_block
                    .as_deref()
                    .map(|statements| self.rewrite_statements(statements)),
                span: *span,
            },
            Statement::When { blocks, span } => Statement::When {
                blocks: self.rewrite_statement_blocks(blocks),
                span: *span,
            },
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => Statement::FunctionCall {
                comp: self.rewrite_component_reference(comp),
                args: self.rewrite_expressions(args),
                outputs: outputs
                    .iter()
                    .map(|output| self.rewrite_component_reference(output))
                    .collect(),
                span: *span,
            },
            Statement::Reinit {
                variable,
                value,
                span,
            } => Statement::Reinit {
                variable: self.rewrite_component_reference(variable),
                value: self.rewrite_expression(value),
                span: *span,
            },
            Statement::Assert {
                condition,
                message,
                level,
                span,
            } => Statement::Assert {
                condition: self.rewrite_expression(condition),
                message: Box::new(self.rewrite_expression(message)),
                level: level
                    .as_ref()
                    .map(|expr| Box::new(self.rewrite_expression(expr))),
                span: *span,
            },
        }
    }

    fn rewrite_statement_blocks(&mut self, blocks: &[StatementBlock]) -> Vec<StatementBlock> {
        blocks
            .iter()
            .map(|block| self.rewrite_statement_block(block))
            .collect()
    }

    fn rewrite_statement_block(&mut self, block: &StatementBlock) -> StatementBlock {
        StatementBlock {
            cond: self.rewrite_expression(&block.cond),
            stmts: self.rewrite_statements(&block.stmts),
        }
    }

    fn rewrite_for_indices(&mut self, indices: &[ForIndex]) -> Vec<ForIndex> {
        indices
            .iter()
            .map(|index| ForIndex {
                ident: index.ident.clone(),
                range: self.rewrite_expression(&index.range),
            })
            .collect()
    }

    fn rewrite_component_reference(
        &mut self,
        reference: &ComponentReference,
    ) -> ComponentReference {
        ComponentReference {
            local: reference.local,
            span: reference.span,
            parts: reference
                .parts
                .iter()
                .map(|part| self.rewrite_component_ref_part(part))
                .collect(),
            def_id: reference.def_id,
        }
    }

    fn rewrite_component_ref_part(&mut self, part: &ComponentRefPart) -> ComponentRefPart {
        ComponentRefPart {
            ident: part.ident.clone(),
            span: part.span,
            subs: self.rewrite_subscripts(&part.subs),
        }
    }
}

pub trait FallibleStatementRewriter: FallibleExpressionRewriter {
    fn rewrite_statements(
        &mut self,
        statements: &[Statement],
    ) -> Result<Vec<Statement>, Self::Error> {
        statements
            .iter()
            .map(|statement| self.rewrite_statement(statement))
            .collect()
    }

    fn rewrite_statement(&mut self, statement: &Statement) -> Result<Statement, Self::Error> {
        self.walk_statement(statement)
    }

    fn walk_statement(&mut self, statement: &Statement) -> Result<Statement, Self::Error> {
        match statement {
            Statement::Empty { span } => Ok(Statement::Empty { span: *span }),
            Statement::Assignment { comp, value, span } => Ok(Statement::Assignment {
                comp: self.rewrite_component_reference(comp)?,
                value: self.rewrite_expression(value)?,
                span: *span,
            }),
            Statement::Return { span } => Ok(Statement::Return { span: *span }),
            Statement::Break { span } => Ok(Statement::Break { span: *span }),
            Statement::For {
                indices,
                equations,
                span,
            } => Ok(Statement::For {
                indices: self.rewrite_for_indices(indices)?,
                equations: self.rewrite_statements(equations)?,
                span: *span,
            }),
            Statement::While { block, span } => Ok(Statement::While {
                block: self.rewrite_statement_block(block)?,
                span: *span,
            }),
            Statement::If {
                cond_blocks,
                else_block,
                span,
            } => Ok(Statement::If {
                cond_blocks: self.rewrite_statement_blocks(cond_blocks)?,
                else_block: else_block
                    .as_deref()
                    .map(|statements| self.rewrite_statements(statements))
                    .transpose()?,
                span: *span,
            }),
            Statement::When { blocks, span } => Ok(Statement::When {
                blocks: self.rewrite_statement_blocks(blocks)?,
                span: *span,
            }),
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => Ok(Statement::FunctionCall {
                comp: self.rewrite_component_reference(comp)?,
                args: self.rewrite_expressions(args)?,
                outputs: outputs
                    .iter()
                    .map(|output| self.rewrite_component_reference(output))
                    .collect::<Result<Vec<_>, Self::Error>>()?,
                span: *span,
            }),
            Statement::Reinit {
                variable,
                value,
                span,
            } => Ok(Statement::Reinit {
                variable: self.rewrite_component_reference(variable)?,
                value: self.rewrite_expression(value)?,
                span: *span,
            }),
            Statement::Assert {
                condition,
                message,
                level,
                span,
            } => Ok(Statement::Assert {
                condition: self.rewrite_expression(condition)?,
                message: Box::new(self.rewrite_expression(message)?),
                level: level
                    .as_ref()
                    .map(|expr| self.rewrite_expression(expr).map(Box::new))
                    .transpose()?,
                span: *span,
            }),
        }
    }

    fn rewrite_statement_blocks(
        &mut self,
        blocks: &[StatementBlock],
    ) -> Result<Vec<StatementBlock>, Self::Error> {
        blocks
            .iter()
            .map(|block| self.rewrite_statement_block(block))
            .collect()
    }

    fn rewrite_statement_block(
        &mut self,
        block: &StatementBlock,
    ) -> Result<StatementBlock, Self::Error> {
        Ok(StatementBlock {
            cond: self.rewrite_expression(&block.cond)?,
            stmts: self.rewrite_statements(&block.stmts)?,
        })
    }

    fn rewrite_for_indices(&mut self, indices: &[ForIndex]) -> Result<Vec<ForIndex>, Self::Error> {
        indices
            .iter()
            .map(|index| {
                Ok(ForIndex {
                    ident: index.ident.clone(),
                    range: self.rewrite_expression(&index.range)?,
                })
            })
            .collect()
    }

    fn rewrite_component_reference(
        &mut self,
        reference: &ComponentReference,
    ) -> Result<ComponentReference, Self::Error> {
        Ok(ComponentReference {
            local: reference.local,
            span: reference.span,
            parts: reference
                .parts
                .iter()
                .map(|part| self.rewrite_component_ref_part(part))
                .collect::<Result<Vec<_>, Self::Error>>()?,
            def_id: reference.def_id,
        })
    }

    fn rewrite_component_ref_part(
        &mut self,
        part: &ComponentRefPart,
    ) -> Result<ComponentRefPart, Self::Error> {
        Ok(ComponentRefPart {
            ident: part.ident.clone(),
            span: part.span,
            subs: self.rewrite_subscripts(&part.subs)?,
        })
    }
}
