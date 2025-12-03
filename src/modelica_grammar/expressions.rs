//! Conversion for expressions.

use super::helpers::{collect_array_elements, expr_loc_info, loc_info};
use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ArraySubscripts {
    pub subscripts: Vec<ir::ast::Subscript>,
}

impl TryFrom<&modelica_grammar_trait::ArraySubscripts> for ArraySubscripts {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArraySubscripts,
    ) -> std::result::Result<Self, Self::Error> {
        let mut subscripts = vec![ast.subscript.clone()];
        for subscript in &ast.array_subscripts_list {
            subscripts.push(subscript.subscript.clone());
        }
        Ok(ArraySubscripts { subscripts })
    }
}

impl TryFrom<&modelica_grammar_trait::Subscript> for ir::ast::Subscript {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Subscript) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Subscript::Colon(tok) => Ok(ir::ast::Subscript::Range {
                token: tok.colon.clone(),
            }),
            modelica_grammar_trait::Subscript::Expression(expr) => {
                Ok(ir::ast::Subscript::Expression(expr.expression.clone()))
            }
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ExpressionList {
    pub args: Vec<ir::ast::Expression>,
}

impl TryFrom<&modelica_grammar_trait::FunctionArgument> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArgument,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArgument::Expression(expr) => {
                Ok(expr.expression.clone())
            }
            modelica_grammar_trait::FunctionArgument::FunctionPartialApplication(fpa) => {
                let loc = &fpa.function_partial_application.function.function.location;
                anyhow::bail!(
                    "Function partial application is not supported at line {}, column {}. \
                     This may indicate a syntax error in your Modelica code - \
                     check for stray text or missing semicolons near function calls.",
                    loc.start_line,
                    loc.start_column
                )
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionArguments> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArguments,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArguments::ExpressionFunctionArgumentsOpt(def) => {
                let mut args = vec![def.expression.clone()];
                if let Some(opt) = &def.function_arguments_opt {
                    match &opt.function_arguments_opt_group {
                        modelica_grammar_trait::FunctionArgumentsOptGroup::CommaFunctionArgumentsNonFirst(
                            expr,
                        ) => {
                            args.append(&mut expr.function_arguments_non_first.args.clone());
                        }
                        modelica_grammar_trait::FunctionArgumentsOptGroup::ForForIndices(..) => {
                            anyhow::bail!(
                                "Array comprehensions with 'for' are not yet supported."
                            )
                        }
                    }
                }
                Ok(ExpressionList { args })
            }
            modelica_grammar_trait::FunctionArguments::FunctionPartialApplicationFunctionArgumentsOpt0(fpa) => {
                let loc = &fpa.function_partial_application.function.function.location;
                anyhow::bail!(
                    "Function partial application is not supported at line {}, column {}. \
                     This may indicate a syntax error in your Modelica code - \
                     check for stray text or missing semicolons near function calls.",
                    loc.start_line, loc.start_column
                )
            }
            modelica_grammar_trait::FunctionArguments::NamedArguments(..) => {
                anyhow::bail!(
                    "Named function arguments are not yet supported. \
                     Use positional arguments instead."
                )
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionArgumentsNonFirst> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArgumentsNonFirst,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArgumentsNonFirst::FunctionArgumentFunctionArgumentsNonFirstOpt(expr) => {
                let mut args = vec![expr.function_argument.clone()];
                if let Some(opt) = &expr.function_arguments_non_first_opt {
                    args.append(&mut opt.function_arguments_non_first.args.clone());
                }
                Ok(ExpressionList { args })
            }
            modelica_grammar_trait::FunctionArgumentsNonFirst::NamedArguments(args) => {
                anyhow::bail!(
                    "Named arguments like 'func(x=1, y=2)' are not yet supported{}. \
                     Use positional arguments instead.",
                    loc_info(&args.named_arguments.named_argument.ident)
                )
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ArgumentList> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArgumentList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut args = vec![ast.argument.clone()];
        for arg in &ast.argument_list_list {
            args.push(arg.argument.clone())
        }
        Ok(ExpressionList { args })
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                match &modif.element_modification_or_replaceable.element_modification_or_replaceable_group {
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementModification(elem) => {
                        let name_loc = elem
                            .element_modification
                            .name
                            .name
                            .first()
                            .map(loc_info)
                            .unwrap_or_default();
                        match &elem.element_modification.element_modification_opt {
                            Some(opt) => {
                                match &opt.modification {
                                    modelica_grammar_trait::Modification::ClassModificationModificationOpt(_modif) => {
                                        anyhow::bail!(
                                            "Class modification in argument is not yet supported{}",
                                            name_loc
                                        )
                                    }
                                    modelica_grammar_trait::Modification::EquModificationExpression(modif) => {
                                        match &modif.modification_expression {
                                            modelica_grammar_trait::ModificationExpression::Break(brk) => {
                                                anyhow::bail!(
                                                    "'break' in modification expression is not yet supported{}",
                                                    loc_info(&brk.r#break.r#break)
                                                )
                                            }
                                            modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                                                // Create a Binary expression to preserve the name=value structure
                                                // LHS = name (as ComponentReference), RHS = value
                                                let name = &elem.element_modification.name;
                                                let parts = name.name.iter().map(|token| {
                                                    ir::ast::ComponentRefPart {
                                                        ident: token.clone(),
                                                        subs: None,
                                                    }
                                                }).collect();
                                                let name_expr = ir::ast::Expression::ComponentReference(
                                                    ir::ast::ComponentReference {
                                                        local: false,
                                                        parts,
                                                    }
                                                );
                                                Ok(ir::ast::Expression::Binary {
                                                    op: ir::ast::OpBinary::Eq(ir::ast::Token::default()),
                                                    lhs: Box::new(name_expr),
                                                    rhs: Box::new(expr.expression.clone()),
                                                })
                                            }
                                        }
                                    }
                                }
                            }
                            None => {
                                Ok(ir::ast::Expression::Empty)
                            }
                        }
                    }
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementReplaceable(repl) => {
                        anyhow::bail!(
                            "'replaceable' element in modification is not yet supported{}",
                            loc_info(&repl.element_replaceable.replaceable.replaceable)
                        )
                    }
                }
            }
            modelica_grammar_trait::Argument::ElementRedeclaration(redcl) => {
                anyhow::bail!(
                    "'redeclare' in argument is not yet supported{}",
                    loc_info(&redcl.element_redeclaration.redeclare.redeclare)
                )
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::OutputExpressionList> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::OutputExpressionList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut v = Vec::new();
        if let Some(opt) = &ast.output_expression_list_opt {
            v.push(opt.expression.clone());
        }
        for expr in &ast.output_expression_list_list {
            if let Some(opt) = &expr.output_expression_list_opt0 {
                v.push(opt.expression.clone());
            }
        }
        Ok(ExpressionList { args: v })
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionCallArgs> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionCallArgs,
    ) -> std::result::Result<Self, Self::Error> {
        if let Some(opt) = &ast.function_call_args_opt {
            Ok(ExpressionList {
                args: opt.function_arguments.args.clone(),
            })
        } else {
            Ok(ExpressionList { args: vec![] })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Primary> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Primary::ComponentPrimary(comp) => {
                match &comp.component_primary.component_primary_opt {
                    Some(args) => Ok(ir::ast::Expression::FunctionCall {
                        comp: comp.component_primary.component_reference.clone(),
                        args: args.function_call_args.args.clone(),
                    }),
                    None => Ok(ir::ast::Expression::ComponentReference(
                        comp.component_primary.component_reference.clone(),
                    )),
                }
            }
            modelica_grammar_trait::Primary::UnsignedNumber(unsigned_num) => {
                match &unsigned_num.unsigned_number {
                    modelica_grammar_trait::UnsignedNumber::UnsignedInteger(unsigned_int) => {
                        Ok(ir::ast::Expression::Terminal {
                            terminal_type: ir::ast::TerminalType::UnsignedInteger,
                            token: unsigned_int.unsigned_integer.clone(),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(unsigned_real) => {
                        Ok(ir::ast::Expression::Terminal {
                            terminal_type: ir::ast::TerminalType::UnsignedReal,
                            token: unsigned_real.unsigned_real.clone(),
                        })
                    }
                }
            }
            modelica_grammar_trait::Primary::String(string) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::String,
                token: string.string.clone(),
            }),
            modelica_grammar_trait::Primary::True(bool) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::Bool,
                token: bool.r#true.r#true.clone(),
            }),
            modelica_grammar_trait::Primary::False(bool) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::Bool,
                token: bool.r#false.r#false.clone(),
            }),
            modelica_grammar_trait::Primary::End(end) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::End,
                token: end.end.end.clone(),
            }),
            modelica_grammar_trait::Primary::ArrayPrimary(arr) => {
                let elements = collect_array_elements(&arr.array_primary.array_arguments)?;
                Ok(ir::ast::Expression::Array { elements })
            }
            modelica_grammar_trait::Primary::RangePrimary(range) => {
                anyhow::bail!(
                    "Range primary like '{{1:10}}' is not yet supported{}",
                    expr_loc_info(&range.range_primary.expression_list.expression)
                )
            }
            modelica_grammar_trait::Primary::OutputPrimary(output) => {
                let primary = &output.output_primary;
                let location_info = primary
                    .output_expression_list
                    .args
                    .first()
                    .and_then(|e| e.get_location())
                    .map(|loc| {
                        format!(
                            " at {}:{}:{}",
                            loc.file_name, loc.start_line, loc.start_column
                        )
                    })
                    .unwrap_or_default();

                if primary.output_primary_opt.is_some() {
                    anyhow::bail!(
                        "Output primary with array subscripts or identifiers is not yet supported{}. \
                         This may indicate a syntax error - check for stray text near parenthesized expressions.",
                        location_info
                    );
                };
                if primary.output_expression_list.args.len() > 1 {
                    // Multiple outputs like (a, b) = func() - create a Tuple
                    Ok(ir::ast::Expression::Tuple {
                        elements: primary.output_expression_list.args.clone(),
                    })
                } else if primary.output_expression_list.args.len() == 1 {
                    // Single expression in parentheses - preserve with Parenthesized wrapper
                    Ok(ir::ast::Expression::Parenthesized {
                        inner: Box::new(primary.output_expression_list.args[0].clone()),
                    })
                } else {
                    // Empty parentheses - return Empty expression
                    Ok(ir::ast::Expression::Empty)
                }
            }
            modelica_grammar_trait::Primary::GlobalFunctionCall(expr) => {
                let tok = match &expr.global_function_call.global_function_call_group {
                    modelica_grammar_trait::GlobalFunctionCallGroup::Der(expr) => {
                        expr.der.der.clone()
                    }
                    modelica_grammar_trait::GlobalFunctionCallGroup::Initial(expr) => {
                        expr.initial.initial.clone()
                    }
                    modelica_grammar_trait::GlobalFunctionCallGroup::Pure(expr) => {
                        expr.pure.pure.clone()
                    }
                };
                let part = ir::ast::ComponentRefPart {
                    ident: tok,
                    subs: None,
                };
                Ok(ir::ast::Expression::FunctionCall {
                    comp: ir::ast::ComponentReference {
                        local: false,
                        parts: vec![part],
                    },
                    args: expr.global_function_call.function_call_args.args.clone(),
                })
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Factor> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        if ast.factor_list.is_empty() {
            Ok(ast.primary.clone())
        } else {
            Ok(ir::ast::Expression::Binary {
                op: ir::ast::OpBinary::Exp(ir::ast::Token::default()),
                lhs: Box::new(ast.primary.clone()),
                rhs: Box::new(ast.factor_list[0].primary.clone()),
            })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        if ast.term_list.is_empty() {
            Ok(ast.factor.clone())
        } else {
            let mut lhs = ast.factor.clone();
            for factor in &ast.term_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match &factor.mul_operator {
                        modelica_grammar_trait::MulOperator::Star(op) => {
                            ir::ast::OpBinary::Mul(op.star.clone())
                        }
                        modelica_grammar_trait::MulOperator::Slash(op) => {
                            ir::ast::OpBinary::Div(op.slash.clone())
                        }
                        modelica_grammar_trait::MulOperator::DotSlash(op) => {
                            ir::ast::OpBinary::DivElem(op.dot_slash.clone())
                        }
                        modelica_grammar_trait::MulOperator::DotStar(op) => {
                            ir::ast::OpBinary::MulElem(op.dot_star.clone())
                        }
                    },
                    rhs: Box::new(factor.factor.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArithmeticExpression,
    ) -> std::result::Result<Self, Self::Error> {
        // handle first term
        let mut lhs = match &ast.arithmetic_expression_opt {
            Some(opt) => ir::ast::Expression::Unary {
                op: match &opt.add_operator {
                    modelica_grammar_trait::AddOperator::Minus(tok) => {
                        ir::ast::OpUnary::Minus(tok.minus.clone())
                    }
                    modelica_grammar_trait::AddOperator::Plus(tok) => {
                        ir::ast::OpUnary::Plus(tok.plus.clone())
                    }
                    modelica_grammar_trait::AddOperator::DotMinus(tok) => {
                        ir::ast::OpUnary::DotMinus(tok.dot_minus.clone())
                    }
                    modelica_grammar_trait::AddOperator::DotPlus(tok) => {
                        ir::ast::OpUnary::DotPlus(tok.dot_plus.clone())
                    }
                },
                rhs: Box::new(ast.term.clone()),
            },
            None => ast.term.clone(),
        };

        // if has term list, process expressions
        if !ast.arithmetic_expression_list.is_empty() {
            for term in &ast.arithmetic_expression_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match &term.add_operator {
                        modelica_grammar_trait::AddOperator::Plus(tok) => {
                            ir::ast::OpBinary::Add(tok.plus.clone())
                        }
                        modelica_grammar_trait::AddOperator::Minus(tok) => {
                            ir::ast::OpBinary::Sub(tok.minus.clone())
                        }
                        modelica_grammar_trait::AddOperator::DotPlus(tok) => {
                            ir::ast::OpBinary::AddElem(tok.dot_plus.clone())
                        }
                        modelica_grammar_trait::AddOperator::DotMinus(tok) => {
                            ir::ast::OpBinary::SubElem(tok.dot_minus.clone())
                        }
                    },
                    rhs: Box::new(term.term.clone()),
                };
            }
        }
        Ok(lhs)
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        match &ast.relation_opt {
            Some(relation) => Ok(ir::ast::Expression::Binary {
                lhs: Box::new(ast.arithmetic_expression.clone()),
                op: match &relation.relational_operator {
                    modelica_grammar_trait::RelationalOperator::EquEqu(tok) => {
                        ir::ast::OpBinary::Eq(tok.equ_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::GT(tok) => {
                        ir::ast::OpBinary::Gt(tok.g_t.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LT(tok) => {
                        ir::ast::OpBinary::Lt(tok.l_t.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::GTEqu(tok) => {
                        ir::ast::OpBinary::Ge(tok.g_t_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LTEqu(tok) => {
                        ir::ast::OpBinary::Le(tok.l_t_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LTGT(tok) => {
                        ir::ast::OpBinary::Neq(tok.l_t_g_t.clone())
                    }
                },
                rhs: Box::new(relation.arithmetic_expression.clone()),
            }),
            None => Ok(ast.arithmetic_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalFactor> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalFactor,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.logical_factor_opt {
            Some(opt) => {
                let not_tok = opt.not.not.clone();
                Ok(ir::ast::Expression::Unary {
                    op: ir::ast::OpUnary::Not(not_tok),
                    rhs: Box::new(ast.relation.clone()),
                })
            }
            None => Ok(ast.relation.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_term_list.is_empty() {
            Ok(ast.logical_factor.clone())
        } else {
            let mut lhs = ast.logical_factor.clone();
            for term in &ast.logical_term_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::ast::OpBinary::And(ir::ast::Token::default()),
                    rhs: Box::new(term.logical_factor.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalExpression,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_expression_list.is_empty() {
            Ok(ast.logical_term.clone())
        } else {
            let mut lhs = ast.logical_term.clone();
            for term in &ast.logical_expression_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::ast::OpBinary::Or(ir::ast::Token::default()),
                    rhs: Box::new(term.logical_term.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::SimpleExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SimpleExpression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.simple_expression_opt {
            Some(opt) => match &opt.simple_expression_opt0 {
                Some(opt0) => Ok(ir::ast::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: Some(Box::new(opt.logical_expression.clone())),
                    end: Box::new(opt0.logical_expression.clone()),
                }),
                None => Ok(ir::ast::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: None,
                    end: Box::new(opt.logical_expression.clone()),
                }),
            },
            None => Ok(ast.logical_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Expression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Expression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Expression::SimpleExpression(simple_expression) => {
                Ok(simple_expression.simple_expression.as_ref().clone())
            }
            modelica_grammar_trait::Expression::IfExpression(expr) => {
                let if_expr = &expr.if_expression;

                // Build the branches: first the main if, then any elseifs
                let mut branches = Vec::new();

                // The main if branch: condition is expression, result is expression0
                let condition = if_expr.expression.clone();
                let then_expr = if_expr.expression0.clone();
                branches.push((condition, then_expr));

                // Add any elseif branches from the list
                for elseif in &if_expr.if_expression_list {
                    let elseif_cond = elseif.expression.clone();
                    let elseif_expr = elseif.expression0.clone();
                    branches.push((elseif_cond, elseif_expr));
                }

                // The else branch is expression1
                let else_branch = Box::new(if_expr.expression1.clone());

                Ok(ir::ast::Expression::If {
                    branches,
                    else_branch,
                })
            }
        }
    }
}
