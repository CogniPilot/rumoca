//! Conversion for equations and statements.

use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Ident> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Ident::BasicIdent(tok) => Ok(ir::ast::Token {
                location: tok.basic_ident.location.clone(),
                text: tok.basic_ident.text.clone(),
                token_number: tok.basic_ident.token_number,
                token_type: tok.basic_ident.token_type,
            }),
            modelica_grammar_trait::Ident::QIdent(tok) => Ok(ir::ast::Token {
                location: tok.q_ident.location.clone(),
                text: tok.q_ident.text.clone(),
                token_number: tok.q_ident.token_number,
                token_type: tok.q_ident.token_type,
            }),
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedInteger> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedInteger,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::Token {
            location: ast.unsigned_integer.location.clone(),
            text: ast.unsigned_integer.text.clone(),
            token_number: ast.unsigned_integer.token_number,
            token_type: ast.unsigned_integer.token_type,
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedReal> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedReal,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::UnsignedReal::Decimal(num) => Ok(num.decimal.clone()),
            modelica_grammar_trait::UnsignedReal::Scientific(num) => Ok(num.scientific.clone()),
            modelica_grammar_trait::UnsignedReal::Scientific2(num) => Ok(num.scientific2.clone()),
            modelica_grammar_trait::UnsignedReal::ScientificInt(num) => {
                Ok(num.scientific_int.clone())
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::EquationBlock> for ir::ast::EquationBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::EquationBlock {
            cond: ast.expression.clone(),
            eqs: ast
                .equation_block_list
                .iter()
                .map(|x| x.some_equation.clone())
                .collect(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::StatementBlock> for ir::ast::StatementBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StatementBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::StatementBlock {
            cond: ast.expression.clone(),
            stmts: ast
                .statement_block_list
                .iter()
                .map(|x| x.statement.clone())
                .collect(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::SomeEquation> for ir::ast::Equation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.some_equation_option {
            modelica_grammar_trait::SomeEquationOption::SimpleEquation(eq) => {
                match &eq.simple_equation.simple_equation_opt {
                    Some(rhs) => Ok(ir::ast::Equation::Simple {
                        lhs: eq.simple_equation.simple_expression.clone(),
                        rhs: rhs.expression.clone(),
                    }),
                    None => {
                        // this is a function call eq (reinit, assert, terminate, etc.)
                        // see 8.3.6-8.3.8
                        match &eq.simple_equation.simple_expression {
                            ir::ast::Expression::FunctionCall { comp, args } => {
                                Ok(ir::ast::Equation::FunctionCall {
                                    comp: comp.clone(),
                                    args: args.clone(),
                                })
                            }
                            _ => Err(anyhow::anyhow!(
                                "Modelica only allows functional call statement as equation: {:?}",
                                ast
                            )),
                        }
                    }
                }
            }
            modelica_grammar_trait::SomeEquationOption::ConnectEquation(eq) => {
                Ok(ir::ast::Equation::Connect {
                    lhs: eq.connect_equation.component_reference.clone(),
                    rhs: eq.connect_equation.component_reference0.clone(),
                })
            }
            modelica_grammar_trait::SomeEquationOption::ForEquation(eq) => {
                // Convert for indices
                let mut indices = Vec::new();

                // First index
                let first_idx = &eq.for_equation.for_indices.for_index;
                let range = first_idx
                    .for_index_opt
                    .as_ref()
                    .map(|opt| opt.expression.clone())
                    .unwrap_or_default();
                indices.push(ir::ast::ForIndex {
                    ident: first_idx.ident.clone(),
                    range,
                });

                // Additional indices
                for idx_item in &eq.for_equation.for_indices.for_indices_list {
                    let idx = &idx_item.for_index;
                    let range = idx
                        .for_index_opt
                        .as_ref()
                        .map(|opt| opt.expression.clone())
                        .unwrap_or_default();
                    indices.push(ir::ast::ForIndex {
                        ident: idx.ident.clone(),
                        range,
                    });
                }

                // Convert equations in the loop body
                let equations: Vec<ir::ast::Equation> = eq
                    .for_equation
                    .for_equation_list
                    .iter()
                    .map(|eq_item| eq_item.some_equation.clone())
                    .collect();

                Ok(ir::ast::Equation::For { indices, equations })
            }
            modelica_grammar_trait::SomeEquationOption::IfEquation(eq) => {
                let mut blocks = vec![eq.if_equation.if0.clone()];
                for when in &eq.if_equation.if_equation_list {
                    blocks.push(when.elseif0.clone());
                }
                Ok(ir::ast::Equation::If {
                    cond_blocks: blocks,
                    else_block: eq.if_equation.if_equation_opt.as_ref().map(|opt| {
                        opt.if_equation_opt_list
                            .iter()
                            .map(|x| x.some_equation.clone())
                            .collect()
                    }),
                })
            }
            modelica_grammar_trait::SomeEquationOption::WhenEquation(eq) => {
                let mut cond_blocks = vec![eq.when_equation.when0.clone()];
                for when in &eq.when_equation.when_equation_list {
                    cond_blocks.push(when.elsewhen0.clone());
                }
                Ok(ir::ast::Equation::When(cond_blocks))
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Statement> for ir::ast::Statement {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Statement) -> std::result::Result<Self, Self::Error> {
        match &ast.statement_option {
            modelica_grammar_trait::StatementOption::ComponentStatement(stmt) => {
                match &stmt.component_statement.component_statement_group {
                    modelica_grammar_trait::ComponentStatementGroup::ColonEquExpression(assign) => {
                        Ok(ir::ast::Statement::Assignment {
                            comp: stmt.component_statement.component_reference.clone(),
                            value: assign.expression.clone(),
                        })
                    }
                    modelica_grammar_trait::ComponentStatementGroup::FunctionCallArgs(args) => {
                        Ok(ir::ast::Statement::FunctionCall {
                            comp: stmt.component_statement.component_reference.clone(),
                            args: args.function_call_args.args.clone(),
                        })
                    }
                }
            }
            modelica_grammar_trait::StatementOption::Break(tok) => Ok(ir::ast::Statement::Break {
                token: tok.r#break.r#break.clone(),
            }),
            modelica_grammar_trait::StatementOption::Return(tok) => {
                Ok(ir::ast::Statement::Return {
                    token: tok.r#return.r#return.clone(),
                })
            }
            modelica_grammar_trait::StatementOption::ForStatement(stmt) => {
                // Convert for indices
                let mut indices = Vec::new();

                // First index
                let first_idx = &stmt.for_statement.for_indices.for_index;
                let range = first_idx
                    .for_index_opt
                    .as_ref()
                    .map(|opt| opt.expression.clone())
                    .unwrap_or_default();
                indices.push(ir::ast::ForIndex {
                    ident: first_idx.ident.clone(),
                    range,
                });

                // Additional indices
                for idx_item in &stmt.for_statement.for_indices.for_indices_list {
                    let idx = &idx_item.for_index;
                    let range = idx
                        .for_index_opt
                        .as_ref()
                        .map(|opt| opt.expression.clone())
                        .unwrap_or_default();
                    indices.push(ir::ast::ForIndex {
                        ident: idx.ident.clone(),
                        range,
                    });
                }

                // Convert statements in the loop body
                let equations: Vec<ir::ast::Statement> = stmt
                    .for_statement
                    .for_statement_list
                    .iter()
                    .map(|stmt_item| stmt_item.statement.clone())
                    .collect();

                Ok(ir::ast::Statement::For { indices, equations })
            }
            modelica_grammar_trait::StatementOption::IfStatement(stmt) => {
                let if_stmt = &stmt.if_statement;

                // Build cond_blocks: first the if block, then all elseif blocks
                let mut cond_blocks = vec![if_stmt.r#if0.clone()];
                for elseif_item in &if_stmt.if_statement_list {
                    cond_blocks.push(elseif_item.elseif0.clone());
                }

                // Build else_block if present
                let else_block = if_stmt.if_statement_opt.as_ref().map(|else_opt| {
                    else_opt
                        .if_statement_opt_list
                        .iter()
                        .map(|item| item.r#else.clone())
                        .collect()
                });

                Ok(ir::ast::Statement::If {
                    cond_blocks,
                    else_block,
                })
            }
            modelica_grammar_trait::StatementOption::WhenStatement(stmt) => {
                let when_stmt = &stmt.when_statement;

                // Build blocks: first the when block, then all elsewhen blocks
                let mut blocks = vec![when_stmt.when0.clone()];
                for elsewhen_item in &when_stmt.when_statement_list {
                    blocks.push(elsewhen_item.elsewhen0.clone());
                }

                Ok(ir::ast::Statement::When(blocks))
            }
            modelica_grammar_trait::StatementOption::WhileStatement(stmt) => {
                let while_stmt = &stmt.while_statement;

                // Collect all statements in the while body
                let stmts: Vec<ir::ast::Statement> = while_stmt
                    .while_statement_list
                    .iter()
                    .map(|item| item.statement.clone())
                    .collect();

                Ok(ir::ast::Statement::While(ir::ast::StatementBlock {
                    cond: while_stmt.expression.clone(),
                    stmts,
                }))
            }
            modelica_grammar_trait::StatementOption::FunctionCallOutputStatement(stmt) => {
                // Handle '(a, b) := func(x)' - multi-output function call
                // For now, we convert this to a function call statement
                // (the output bindings are preserved in the args for later processing)
                let fcall = &stmt.function_call_output_statement;

                Ok(ir::ast::Statement::FunctionCall {
                    comp: fcall.component_reference.clone(),
                    args: fcall.function_call_args.args.clone(),
                })
            }
        }
    }
}
