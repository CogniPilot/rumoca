// transformation.rs, does the conversion to GALEC after the analysis pass

use std::collections::{HashMap, HashSet};

use crate::analysis::AnalysisResult;
use crate::errors::TransformError;
use crate::galec::{
    DataRole, Galec, GalecBinaryOp, GalecExpr, GalecGroup, GalecLiteral, GalecStatement,
    GalecVariable,
};
use rumoca_core::{
    BuiltinFunction, Expression, ExpressionRewriter, Literal, OpBinary, OpUnary, Span, Statement,
    StatementBlock, Subscript, VarName,
};
use rumoca_ir_dae::{Dae, VariableCausality, VariableOrigin};

pub struct TransformationInput<'a> {
    pub dae: &'a Dae,
    pub analysis: AnalysisResult,
    pub model_name: String,
}

pub fn transform(input: TransformationInput) -> Result<Galec, TransformError> {
    let t = Transformer { dae: input.dae };
    let variables = t.transform_variables()?;
    let startup = t.build_startup()?;
    let recalibrate = t.build_recalibrate()?;
    let do_step = t.build_do_step(&input.analysis)?;
    let period = input.dae.clocks.schedules[0].period_seconds;

    Ok(Galec {
        name: input.model_name,
        period,
        variables,
        startup,
        recalibrate,
        do_step,
    })
}

struct Transformer<'a> {
    dae: &'a Dae,
}

impl<'a> Transformer<'a> {
    fn transform_variables(&self) -> Result<Vec<GalecVariable>, TransformError> {
        let mut vars: Vec<GalecVariable> = Vec::new();

        for (_, v) in &self.dae.variables.inputs {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            vars.push(self.make_var(v, DataRole::Input, GalecLiteral::Real(0.0))?);
        }
        for (_, v) in &self.dae.variables.outputs {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            vars.push(self.make_var(v, DataRole::Output, GalecLiteral::Real(0.0))?);
        }
        for (_, v) in &self.dae.variables.constants {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            vars.push(self.make_var(v, DataRole::Constant, GalecLiteral::Real(0.0))?);
        }
        for (_, v) in &self.dae.variables.discrete_reals {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            let role = discrete_role(v.causality);
            vars.push(self.make_var(v, role, GalecLiteral::Real(0.0))?);
        }
        for (_, v) in &self.dae.variables.discrete_valued {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            let role = discrete_role(v.causality);
            vars.push(self.make_var(v, role, GalecLiteral::Integer(0))?);
        }
        for (_, v) in &self.dae.variables.parameters {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            let role = if v.is_tunable {
                DataRole::TunableParameter
            } else {
                DataRole::DependentParameter
            };
            vars.push(self.make_var(v, role, GalecLiteral::Real(0.0))?);
        }

        Ok(vars)
    }

    fn make_var(
        &self,
        v: &rumoca_ir_dae::Variable,
        role: DataRole,
        ty: GalecLiteral,
    ) -> Result<GalecVariable, TransformError> {
        Ok(GalecVariable {
            name: v.name.to_string(),
            role,
            ty,
            dims: v.dims.iter().map(|&d| d as usize).collect(),
            start: v
                .start
                .as_ref()
                .map(|e| self.transform_expr(e))
                .transpose()?,
        })
    }

    fn build_startup(&self) -> Result<Vec<GalecStatement>, TransformError> {
        let mut stmts: Vec<GalecStatement> = Vec::new();

        for (_, v) in &self.dae.variables.constants {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            let Some(start) = &v.start else { continue };
            stmts.push(GalecStatement::Assign {
                lhs: v.name.to_string(),
                rhs: self.transform_expr(start)?,
            });
        }
        for (_, v) in &self.dae.variables.parameters {
            if v.origin == VariableOrigin::Generated {
                continue;
            }
            let Some(start) = &v.start else { continue };
            if matches!(start, Expression::Array { .. }) {
                continue;
            }
            stmts.push(GalecStatement::Assign {
                lhs: v.name.to_string(),
                rhs: self.transform_expr(start)?,
            });
        }

        let discrete = self
            .dae
            .variables
            .discrete_reals
            .values()
            .chain(self.dae.variables.discrete_valued.values())
            .chain(self.dae.variables.outputs.values())
            .filter(|v| v.origin != VariableOrigin::Generated);
        for v in discrete {
            let Some(start) = &v.start else { continue };
            stmts.push(GalecStatement::Assign {
                lhs: v.name.to_string(),
                rhs: self.transform_expr(start)?,
            });
        }

        Ok(stmts)
    }

    fn build_recalibrate(&self) -> Result<Vec<GalecStatement>, TransformError> {
        let d_param: HashSet<String> = self
            .dae
            .variables
            .parameters
            .values()
            .filter(|v| !v.is_tunable)
            .map(|v| v.name.to_string())
            .collect();

        self.dae
            .discrete
            .real_updates
            .iter()
            .chain(self.dae.discrete.valued_updates.iter())
            .filter(|eq| {
                eq.lhs
                    .as_ref()
                    .map(|l| d_param.contains(&l.to_string()))
                    .unwrap_or(false)
            })
            .map(|eq| {
                Ok(GalecStatement::Assign {
                    lhs: eq.lhs.as_ref().unwrap().to_string(),
                    rhs: self.transform_expr(&eq.rhs)?,
                })
            })
            .collect()
    }

    fn build_do_step(&self, analysis: &AnalysisResult) -> Result<Vec<GalecGroup>, TransformError> {
        let source_vars: HashSet<String> = self
            .dae
            .variables
            .discrete_reals
            .values()
            .chain(self.dae.variables.discrete_valued.values())
            .chain(self.dae.variables.outputs.values())
            .filter(|v| v.origin == VariableOrigin::Source)
            .map(|v| v.name.to_string())
            .collect();
        let equations: Vec<_> = self
            .dae
            .discrete
            .real_updates
            .iter()
            .chain(self.dae.discrete.valued_updates.iter())
            .filter(|eq| {
                eq.lhs
                    .as_ref()
                    .map(|lhs| source_vars.contains(&lhs.to_string()))
                    .unwrap_or(false)
            })
            .collect();

        analysis
            .groups
            .iter()
            .map(|group| {
                let statements = group
                    .statements
                    .iter()
                    .map(|id| {
                        let eq = equations[id.0];
                        let rhs = extract_true_branch(&eq.rhs);
                        Ok(GalecStatement::Assign {
                            lhs: eq.lhs.as_ref().unwrap().to_string(),
                            rhs: self.transform_expr(rhs)?,
                        })
                    })
                    .collect::<Result<Vec<_>, TransformError>>()?;

                Ok(GalecGroup {
                    condition: group
                        .condition
                        .as_ref()
                        .map(|e| self.transform_expr(e))
                        .transpose()?,
                    statements,
                })
            })
            .collect()
    }

    fn transform_expr(&self, expr: &Expression) -> Result<GalecExpr, TransformError> {
        match expr {
            Expression::VarRef {
                name, subscripts, ..
            } => self.transform_var_ref(name.as_str(), subscripts),

            Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args,
                ..
            } => match args.first() {
                Some(Expression::VarRef { name, .. }) => Ok(GalecExpr::PreviousRef {
                    name: name.as_str().to_string(),
                }),
                Some(_) => Err(TransformError::UnsupportedExpression(
                    "pre() argument must be a variable reference".to_string(),
                )),
                None => Err(TransformError::UnsupportedExpression(
                    "pre() requires exactly one argument".to_string(),
                )),
            },

            Expression::BuiltinCall { function, args, .. } => match remap_builtin(function) {
                Some(name) => Ok(GalecExpr::Call {
                    name: name.to_string(),
                    args: args
                        .iter()
                        .map(|e| self.transform_expr(e))
                        .collect::<Result<_, _>>()?,
                }),
                None => {
                    let builtin_name = function.name();
                    let args_str: Vec<String> = args.iter().map(format_expr_for_error).collect();
                    Err(TransformError::UnsupportedBuiltin {
                        feature: format!("builtin:{builtin_name}"),
                        source_expr: format!("{builtin_name}({})", args_str.join(", ")),
                    })
                }
            },

            Expression::FunctionCall { name, args, .. } => self.transform_function_call(name, args),

            Expression::Binary { op, lhs, rhs, .. } => Ok(GalecExpr::Binary {
                op: remap_op(op)?,
                lhs: Box::new(self.transform_expr(lhs)?),
                rhs: Box::new(self.transform_expr(rhs)?),
            }),

            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => Ok(GalecExpr::Negate(Box::new(self.transform_expr(rhs)?))),

            Expression::Literal {
                value: Literal::Real(v),
                ..
            } => Ok(GalecExpr::Literal(GalecLiteral::Real(*v))),
            Expression::Literal {
                value: Literal::Integer(v),
                ..
            } => Ok(GalecExpr::Literal(GalecLiteral::Integer(*v))),
            Expression::Literal {
                value: Literal::Boolean(v),
                ..
            } => Ok(GalecExpr::Literal(GalecLiteral::Boolean(*v))),

            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let mut result = self.transform_expr(else_branch)?;
                for (cond, then_val) in branches.iter().rev() {
                    result = GalecExpr::If {
                        condition: Box::new(self.transform_expr(cond)?),
                        then_branch: Box::new(self.transform_expr(then_val)?),
                        else_branch: Box::new(result),
                    };
                }
                Ok(result)
            }

            Expression::Index {
                base, subscripts, ..
            } => {
                let galec_subs = self.transform_subscripts(subscripts)?;
                if galec_subs.is_empty() {
                    self.transform_expr(base)
                } else {
                    Ok(GalecExpr::Index {
                        base: Box::new(self.transform_expr(base)?),
                        subscripts: galec_subs,
                    })
                }
            }

            Expression::Array { .. } => Ok(GalecExpr::Literal(GalecLiteral::Real(0.0))),

            _ => Err(TransformError::UnsupportedExpression(format!(
                "{expr:?} is not supported in GALEC"
            ))),
        }
    }

    fn transform_var_ref(
        &self,
        name_str: &str,
        subscripts: &[Subscript],
    ) -> Result<GalecExpr, TransformError> {
        let base = if let Some(base) = name_str.strip_prefix("__pre__.") {
            GalecExpr::PreviousRef {
                name: base.to_string(),
            }
        } else {
            GalecExpr::SelfRef {
                name: name_str.to_string(),
            }
        };
        let subs = self.transform_subscripts(subscripts)?;
        if subs.is_empty() {
            Ok(base)
        } else {
            Ok(GalecExpr::Index {
                base: Box::new(base),
                subscripts: subs,
            })
        }
    }

    fn transform_function_call(
        &self,
        name: &rumoca_core::Reference,
        args: &[Expression],
    ) -> Result<GalecExpr, TransformError> {
        let (func_name_str, output_slot) = if let Some((scope, last)) = name.scope_split() {
            (scope, Some(last))
        } else {
            (name.as_str(), None)
        };

        let func_key = VarName::new(func_name_str);
        let Some(func) = self.dae.symbols.functions.get(&func_key) else {
            return Ok(GalecExpr::Call {
                name: name.as_str().to_string(),
                args: args
                    .iter()
                    .map(|e| self.transform_expr(e))
                    .collect::<Result<_, _>>()?,
            });
        };

        let out_name = resolve_output_name(func, func_name_str, output_slot)?;
        self.inline_function(func, args, &out_name)
    }

    fn transform_subscripts(
        &self,
        subscripts: &[Subscript],
    ) -> Result<Vec<GalecExpr>, TransformError> {
        let mut result = Vec::new();
        for sub in subscripts {
            match sub {
                Subscript::Index { value, .. } => {
                    result.push(GalecExpr::Literal(GalecLiteral::Integer(*value)));
                }
                Subscript::Colon { .. } => {}
                Subscript::Expr { expr, .. }
                    if !matches!(expr.as_ref(), Expression::Range { .. }) =>
                {
                    result.push(self.transform_expr(expr)?);
                }
                Subscript::Expr { .. } => {}
            }
        }
        Ok(result)
    }

    fn inline_function(
        &self,
        func: &rumoca_core::Function,
        args: &[Expression],
        output_name: &str,
    ) -> Result<GalecExpr, TransformError> {
        let mut env: HashMap<String, Expression> = HashMap::new();

        for (i, input) in func.inputs.iter().enumerate() {
            let arg = match args.get(i) {
                Some(a) => a.clone(),
                None => input.default.clone().ok_or_else(|| {
                    TransformError::UnsupportedExpression(format!(
                        "function `{}` missing required argument `{}`",
                        func.name.as_str(),
                        input.name
                    ))
                })?,
            };
            env.insert(input.name.clone(), arg);
        }

        self.execute_body(&func.body, &mut env, func.name.as_str())?;

        let output_expr = env.remove(output_name).ok_or_else(|| {
            TransformError::UnsupportedExpression(format!(
                "function `{}` does not assign output `{output_name}`",
                func.name.as_str()
            ))
        })?;

        self.transform_expr(&output_expr)
    }

    fn execute_body(
        &self,
        stmts: &[Statement],
        env: &mut HashMap<String, Expression>,
        func_name: &str,
    ) -> Result<(), TransformError> {
        for stmt in stmts {
            match stmt {
                Statement::Empty { .. } | Statement::Return { .. } => {}

                Statement::Assignment { comp, value, .. } => {
                    execute_assignment(comp, value, env, func_name)?;
                }

                Statement::If {
                    cond_blocks,
                    else_block,
                    span,
                } => {
                    self.execute_if(cond_blocks, else_block, env, *span)?;
                }

                _ => {
                    return Err(TransformError::UnsupportedExpression(format!(
                        "function `{func_name}` contains an unsupported statement type"
                    )));
                }
            }
        }
        Ok(())
    }

    fn execute_if(
        &self,
        cond_blocks: &[StatementBlock],
        else_block: &Option<Vec<Statement>>,
        env: &mut HashMap<String, Expression>,
        span: Span,
    ) -> Result<(), TransformError> {
        let mut assigned: HashSet<String> = HashSet::new();
        for block in cond_blocks {
            collect_assigned(&block.stmts, &mut assigned);
        }
        if let Some(else_stmts) = else_block {
            collect_assigned(else_stmts, &mut assigned);
        }

        for var in assigned {
            let pre_val = env.get(&var).cloned().unwrap_or(Expression::Literal {
                value: Literal::Real(0.0),
                span,
            });
            let else_val = if let Some(else_stmts) = else_block {
                find_assignment_value(else_stmts, &var, env).unwrap_or(pre_val.clone())
            } else {
                pre_val.clone()
            };

            let mut result = else_val;
            for block in cond_blocks.iter().rev() {
                let cond = Substituter { env: &*env }.rewrite_expression(&block.cond);
                let then_val = find_assignment_value(&block.stmts, &var, env)
                    .unwrap_or_else(|| Substituter { env: &*env }.rewrite_expression(&pre_val));
                result = Expression::If {
                    branches: vec![(cond, then_val)],
                    else_branch: Box::new(result),
                    span,
                };
            }

            env.insert(var, result);
        }

        Ok(())
    }
}

fn discrete_role(causality: VariableCausality) -> DataRole {
    if causality == VariableCausality::Output {
        DataRole::Output
    } else {
        DataRole::State
    }
}

fn resolve_output_name(
    func: &rumoca_core::Function,
    func_name_str: &str,
    output_slot: Option<&str>,
) -> Result<String, TransformError> {
    if let Some(slot) = output_slot {
        return Ok(slot.to_string());
    }
    if func.outputs.len() == 1 {
        return Ok(func.outputs[0].name.clone());
    }
    Err(TransformError::UnsupportedExpression(format!(
        "function `{func_name_str}` has {} outputs; \
         use a qualified name (e.g. {func_name_str}.outputName) to select one",
        func.outputs.len(),
    )))
}

fn execute_assignment(
    comp: &rumoca_core::ComponentReference,
    value: &Expression,
    env: &mut HashMap<String, Expression>,
    func_name: &str,
) -> Result<(), TransformError> {
    let Some(target) = simple_assignment_target(comp) else {
        return Err(TransformError::UnsupportedExpression(format!(
            "function `{func_name}` has a complex assignment target"
        )));
    };
    let resolved = Substituter { env: &*env }.rewrite_expression(value);
    env.insert(target.to_string(), resolved);
    Ok(())
}

fn simple_assignment_target(comp: &rumoca_core::ComponentReference) -> Option<&str> {
    let [part] = comp.parts.as_slice() else {
        return None;
    };
    part.subs.is_empty().then_some(part.ident.as_str())
}

fn collect_assigned(stmts: &[Statement], out: &mut HashSet<String>) {
    for stmt in stmts {
        if let Statement::Assignment { comp, .. } = stmt
            && let Some(name) = simple_assignment_target(comp)
        {
            out.insert(name.to_string());
        }
    }
}

fn find_assignment_value(
    stmts: &[Statement],
    var: &str,
    env: &HashMap<String, Expression>,
) -> Option<Expression> {
    for stmt in stmts {
        if let Statement::Assignment { comp, value, .. } = stmt
            && simple_assignment_target(comp) == Some(var)
        {
            return Some(Substituter { env }.rewrite_expression(value));
        }
    }
    None
}

struct Substituter<'a> {
    env: &'a HashMap<String, Expression>,
}

impl ExpressionRewriter for Substituter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if let Some(value) = self.env.get(name.as_str()) {
            if subscripts.is_empty() {
                return value.clone();
            }
            return Expression::Index {
                base: Box::new(value.clone()),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

fn extract_true_branch(rhs: &Expression) -> &Expression {
    if let Expression::If { branches, .. } = rhs
        && let Some((_, true_val)) = branches.first()
    {
        return true_val;
    }
    rhs
}

fn format_expr_for_error(expr: &Expression) -> String {
    match expr {
        Expression::Literal {
            value: Literal::Integer(n),
            ..
        } => n.to_string(),
        Expression::Literal {
            value: Literal::Real(v),
            ..
        } => format!("{v}"),
        Expression::Literal {
            value: Literal::Boolean(b),
            ..
        } => b.to_string(),
        Expression::VarRef { name, .. } => name.as_str().to_string(),
        Expression::BuiltinCall { function, args, .. } => {
            let args_str: Vec<String> = args.iter().map(format_expr_for_error).collect();
            format!("{}({})", function.name(), args_str.join(", "))
        }
        _ => "...".to_string(),
    }
}

fn remap_builtin(f: &BuiltinFunction) -> Option<&'static str> {
    match f {
        BuiltinFunction::Abs => Some("absolute"),
        BuiltinFunction::Log => Some("ln"),
        BuiltinFunction::Log10 => Some("lg"),
        BuiltinFunction::Floor => Some("roundDown"),
        BuiltinFunction::Ceil => Some("roundUp"),
        BuiltinFunction::Min => Some("min"),
        BuiltinFunction::Max => Some("max"),
        BuiltinFunction::Sign => Some("sign"),
        BuiltinFunction::Sqrt => Some("sqrt"),
        BuiltinFunction::Sin => Some("sin"),
        BuiltinFunction::Cos => Some("cos"),
        BuiltinFunction::Tan => Some("tan"),
        BuiltinFunction::Asin => Some("asin"),
        BuiltinFunction::Acos => Some("acos"),
        BuiltinFunction::Atan => Some("atan"),
        BuiltinFunction::Atan2 => Some("atan2"),
        BuiltinFunction::Exp => Some("exp"),
        _ => None,
    }
}

fn remap_op(op: &OpBinary) -> Result<GalecBinaryOp, TransformError> {
    match op {
        OpBinary::Add | OpBinary::AddElem => Ok(GalecBinaryOp::Add),
        OpBinary::Sub | OpBinary::SubElem => Ok(GalecBinaryOp::Sub),
        OpBinary::Mul | OpBinary::MulElem => Ok(GalecBinaryOp::Mul),
        OpBinary::Div | OpBinary::DivElem => Ok(GalecBinaryOp::Div),
        OpBinary::Exp | OpBinary::ExpElem => Ok(GalecBinaryOp::Pow),
        OpBinary::Eq => Ok(GalecBinaryOp::Eq),
        OpBinary::Neq => Ok(GalecBinaryOp::Neq),
        OpBinary::Lt => Ok(GalecBinaryOp::Lt),
        OpBinary::Le => Ok(GalecBinaryOp::Lte),
        OpBinary::Gt => Ok(GalecBinaryOp::Gt),
        OpBinary::Ge => Ok(GalecBinaryOp::Gte),
        OpBinary::And => Ok(GalecBinaryOp::And),
        OpBinary::Or => Ok(GalecBinaryOp::Or),
        _ => Err(TransformError::UnsupportedExpression(format!(
            "binary operator {op:?} is not supported in GALEC"
        ))),
    }
}
