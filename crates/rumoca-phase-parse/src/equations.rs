//! Conversion for equations and statements.

use crate::errors::semantic_error_from_expression;
use crate::generated::modelica_grammar_trait;

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Ident> for rumoca_ir_core::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Ident::BasicIdent(tok) => Ok((&tok.basic_ident).into()),
            modelica_grammar_trait::Ident::QIdent(tok) => Ok((&tok.q_ident).into()),
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedInteger> for rumoca_ir_core::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedInteger,
    ) -> std::result::Result<Self, Self::Error> {
        Ok((&ast.unsigned_integer).into())
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedReal> for rumoca_ir_core::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedReal,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::UnsignedReal::Decimal(num) => Ok((&num.decimal).into()),
            modelica_grammar_trait::UnsignedReal::Scientific(num) => Ok((&num.scientific).into()),
            modelica_grammar_trait::UnsignedReal::Scientific2(num) => Ok((&num.scientific2).into()),
            modelica_grammar_trait::UnsignedReal::ScientificInt(num) => {
                Ok((&num.scientific_int).into())
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::EquationBlock> for rumoca_ir_ast::EquationBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(rumoca_ir_ast::EquationBlock {
            cond: ast.expression.clone(),
            eqs: ast
                .equation_block_list
                .iter()
                .map(|x| x.some_equation.clone())
                .collect(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::StatementBlock> for rumoca_ir_ast::StatementBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StatementBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(rumoca_ir_ast::StatementBlock {
            cond: ast.expression.clone(),
            stmts: ast
                .statement_block_list
                .iter()
                .map(|x| x.statement.clone())
                .collect(),
        })
    }
}

/// Convert a simple equation (may be assignment or function call).
fn convert_simple_equation(
    simple_eq: &modelica_grammar_trait::SimpleEquation,
) -> anyhow::Result<rumoca_ir_ast::Equation> {
    // If there's an RHS, it's a simple assignment equation
    if let Some(rhs) = &simple_eq.simple_equation_opt {
        return Ok(rumoca_ir_ast::Equation::Simple {
            lhs: simple_eq.simple_expression.clone(),
            rhs: rhs.expression.clone(),
        });
    }

    // No RHS means function call equation (reinit, assert, terminate, etc.)
    // See MLS 8.3.6-8.3.8
    let rumoca_ir_ast::Expression::FunctionCall { comp, args } = &simple_eq.simple_expression
    else {
        return Err(semantic_error_from_expression(
            format!(
                "Modelica only allows functional call statement as equation: {:?}",
                simple_eq
            ),
            &simple_eq.simple_expression,
        ));
    };

    Ok(rumoca_ir_ast::Equation::FunctionCall {
        comp: comp.clone(),
        args: args.clone(),
    })
}

impl TryFrom<&modelica_grammar_trait::SomeEquation> for rumoca_ir_ast::Equation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.some_equation_option {
            modelica_grammar_trait::SomeEquationOption::SimpleEquation(eq) => {
                convert_simple_equation(&eq.simple_equation)
            }
            modelica_grammar_trait::SomeEquationOption::ConnectEquation(eq) => {
                Ok(rumoca_ir_ast::Equation::Connect {
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
                indices.push(rumoca_ir_ast::ForIndex {
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
                    indices.push(rumoca_ir_ast::ForIndex {
                        ident: idx.ident.clone(),
                        range,
                    });
                }

                // Convert equations in the loop body
                let equations: Vec<rumoca_ir_ast::Equation> = eq
                    .for_equation
                    .for_equation_list
                    .iter()
                    .map(|eq_item| eq_item.some_equation.clone())
                    .collect();

                Ok(rumoca_ir_ast::Equation::For { indices, equations })
            }
            modelica_grammar_trait::SomeEquationOption::IfEquation(eq) => {
                let mut blocks = vec![eq.if_equation.if0.clone()];
                for when in &eq.if_equation.if_equation_list {
                    blocks.push(when.elseif0.clone());
                }
                Ok(rumoca_ir_ast::Equation::If {
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
                Ok(rumoca_ir_ast::Equation::When(cond_blocks))
            }
        }
    }
}

/// Convert grammar ForIndices to AST ForIndex vector.
fn convert_for_indices(
    for_indices: &modelica_grammar_trait::ForIndices,
) -> Vec<rumoca_ir_ast::ForIndex> {
    let mut indices = Vec::new();

    // First index
    let first_idx = &for_indices.for_index;
    let range = first_idx
        .for_index_opt
        .as_ref()
        .map(|opt| opt.expression.clone())
        .unwrap_or_default();
    indices.push(rumoca_ir_ast::ForIndex {
        ident: first_idx.ident.clone(),
        range,
    });

    // Additional indices
    for idx_item in &for_indices.for_indices_list {
        let idx = &idx_item.for_index;
        let range = idx
            .for_index_opt
            .as_ref()
            .map(|opt| opt.expression.clone())
            .unwrap_or_default();
        indices.push(rumoca_ir_ast::ForIndex {
            ident: idx.ident.clone(),
            range,
        });
    }

    indices
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Statement> for rumoca_ir_ast::Statement {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Statement) -> std::result::Result<Self, Self::Error> {
        match &ast.statement_option {
            modelica_grammar_trait::StatementOption::ComponentStatement(stmt) => {
                match &stmt.component_statement.component_statement_group {
                    modelica_grammar_trait::ComponentStatementGroup::ColonEquExpression(assign) => {
                        Ok(rumoca_ir_ast::Statement::Assignment {
                            comp: stmt.component_statement.component_reference.clone(),
                            value: assign.expression.clone(),
                        })
                    }
                    modelica_grammar_trait::ComponentStatementGroup::FunctionCallArgs(args) => {
                        Ok(rumoca_ir_ast::Statement::FunctionCall {
                            comp: stmt.component_statement.component_reference.clone(),
                            args: args.function_call_args.args.clone(),
                            outputs: vec![],
                        })
                    }
                }
            }
            modelica_grammar_trait::StatementOption::Break(tok) => {
                Ok(rumoca_ir_ast::Statement::Break {
                    token: tok.r#break.r#break.clone().into(),
                })
            }
            modelica_grammar_trait::StatementOption::Return(tok) => {
                Ok(rumoca_ir_ast::Statement::Return {
                    token: tok.r#return.r#return.clone().into(),
                })
            }
            modelica_grammar_trait::StatementOption::ForStatement(stmt) => {
                let indices = convert_for_indices(&stmt.for_statement.for_indices);
                let equations: Vec<rumoca_ir_ast::Statement> = stmt
                    .for_statement
                    .for_statement_list
                    .iter()
                    .map(|stmt_item| stmt_item.statement.clone())
                    .collect();
                Ok(rumoca_ir_ast::Statement::For { indices, equations })
            }
            modelica_grammar_trait::StatementOption::IfStatement(stmt) => {
                let if_stmt = &stmt.if_statement;
                let mut cond_blocks = vec![if_stmt.r#if0.clone()];
                for elseif_item in &if_stmt.if_statement_list {
                    cond_blocks.push(elseif_item.elseif0.clone());
                }
                let else_block = if_stmt.if_statement_opt.as_ref().map(|else_opt| {
                    else_opt
                        .if_statement_opt_list
                        .iter()
                        .map(|item| item.r#else.clone())
                        .collect()
                });
                Ok(rumoca_ir_ast::Statement::If {
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

                Ok(rumoca_ir_ast::Statement::When(blocks))
            }
            modelica_grammar_trait::StatementOption::WhileStatement(stmt) => {
                let while_stmt = &stmt.while_statement;

                // Collect all statements in the while body
                let stmts: Vec<rumoca_ir_ast::Statement> = while_stmt
                    .while_statement_list
                    .iter()
                    .map(|item| item.statement.clone())
                    .collect();

                Ok(rumoca_ir_ast::Statement::While(
                    rumoca_ir_ast::StatementBlock {
                        cond: while_stmt.expression.clone(),
                        stmts,
                    },
                ))
            }
            modelica_grammar_trait::StatementOption::FunctionCallOutputStatement(stmt) => {
                // Handle '(a, b) := func(x)' - multi-output function call
                let fcall = &stmt.function_call_output_statement;

                Ok(rumoca_ir_ast::Statement::FunctionCall {
                    comp: fcall.component_reference.clone(),
                    args: fcall.function_call_args.args.clone(),
                    outputs: fcall.output_expression_list.args.clone(),
                })
            }
        }
    }
}
