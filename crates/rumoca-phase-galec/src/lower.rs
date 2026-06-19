use core::result::Result::{self, Err};

use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Statement, Subscript, VarName};
use rumoca_ir_dae as dae;

use crate::admissibility::{GalecAdmissibleDae, fixed_sample_period_variable};
use crate::ir::{
    GalecBlock, GalecDecl, GalecDeclRole, GalecExpr, GalecInterface, GalecModel, GalecSampleTime,
    GalecStmt, GalecType, GalecVariable, GalecVariableRole,
};
use crate::prepare::{SNAPSHOT_ORIGIN, SNAPSHOT_PREFIX};

pub fn lower_to_galec(
    admissible: GalecAdmissibleDae<'_>,
    model_name: &str,
) -> Result<GalecModel, GalecLowerError> {
    let dae = admissible.dae;

    let mut interface = GalecInterface::default();
    let mut declarations = Vec::new();

    lower_variable_partition(
        dae.variables.inputs.values(),
        GalecVariableRole::Input,
        GalecType::Real,
        &mut interface,
        &mut declarations,
    )?;
    lower_variable_partition(
        dae.variables.outputs.values(),
        GalecVariableRole::Output,
        GalecType::Real,
        &mut interface,
        &mut declarations,
    )?;
    lower_parameter_variables(dae, &mut interface, &mut declarations)?;
    lower_variable_partition(
        dae.variables.constants.values(),
        GalecVariableRole::Constant,
        GalecType::Real,
        &mut interface,
        &mut declarations,
    )?;
    lower_variable_partition(
        dae.variables.algebraics.values(),
        GalecVariableRole::Local,
        GalecType::Real,
        &mut interface,
        &mut declarations,
    )?;
    lower_discrete_real_variables(dae, &mut interface, &mut declarations)?;
    lower_discrete_valued_variables(dae, &mut interface, &mut declarations)?;

    Ok(GalecModel {
        name: model_name.to_string(),
        interface,
        sample_time: lower_sample_time(dae)?,
        declarations,
        init: lower_equations_to_block(&dae.initialization.equations)?,
        recalibrate: lower_recalibrate_block(dae)?,
        step: lower_step_block(dae)?,
    })
}

fn lower_sample_time(dae: &dae::Dae) -> Result<GalecSampleTime, GalecLowerError> {
    let (name, _) = fixed_sample_period_variable(dae).ok_or_else(|| {
        GalecLowerError::Unsupported(
            "fixed sample period constant was not available after admissibility".to_string(),
        )
    })?;
    let schedule = dae.clocks.schedules.first().ok_or_else(|| {
        GalecLowerError::Unsupported(
            "fixed sample schedule was not available after admissibility".to_string(),
        )
    })?;

    Ok(GalecSampleTime {
        period_seconds: schedule.period_seconds,
        variable_name: name.as_str().to_string(),
    })
}

fn lower_parameter_variables(
    dae: &dae::Dae,
    interface: &mut GalecInterface,
    declarations: &mut Vec<GalecDecl>,
) -> Result<(), GalecLowerError> {
    for variable in dae.variables.parameters.values() {
        if !dae.algorithms.model.is_empty() && variable.name.as_str().starts_with("__pre__.") {
            continue;
        }
        let role = match variable.causality {
            dae::VariableCausality::CalculatedParameter => GalecVariableRole::DependentParameter,
            _ => GalecVariableRole::TunableParameter,
        };
        let scalar_ty = infer_scalar_type(variable).unwrap_or(GalecType::Real);
        let galec_var = lower_variable(variable, role, scalar_ty)?;
        place_variable(galec_var, interface, declarations);
    }
    Ok(())
}

fn lower_variable_partition<'a>(
    variables: impl IntoIterator<Item = &'a dae::Variable>,
    role: GalecVariableRole,
    default_scalar_ty: GalecType,
    interface: &mut GalecInterface,
    declarations: &mut Vec<GalecDecl>,
) -> Result<(), GalecLowerError> {
    for variable in variables {
        let scalar_ty = infer_scalar_type(variable).unwrap_or_else(|| default_scalar_ty.clone());
        let galec_var = lower_variable(variable, role, scalar_ty)?;
        place_variable(galec_var, interface, declarations);
    }
    Ok(())
}

fn lower_discrete_real_variables(
    dae: &dae::Dae,
    interface: &mut GalecInterface,
    declarations: &mut Vec<GalecDecl>,
) -> Result<(), GalecLowerError> {
    for variable in dae.variables.discrete_reals.values() {
        let role = if variable.name.as_str().starts_with(SNAPSHOT_PREFIX) {
            GalecVariableRole::Local
        } else {
            match variable.causality {
                dae::VariableCausality::Input => GalecVariableRole::Input,
                dae::VariableCausality::Output => GalecVariableRole::Output,
                _ => GalecVariableRole::State,
            }
        };
        let galec_var = lower_variable(variable, role, GalecType::Real)?;
        place_variable(galec_var, interface, declarations);
    }
    Ok(())
}

fn lower_discrete_valued_variables(
    dae: &dae::Dae,
    interface: &mut GalecInterface,
    declarations: &mut Vec<GalecDecl>,
) -> Result<(), GalecLowerError> {
    for variable in dae.variables.discrete_valued.values() {
        let scalar_ty = infer_scalar_type(variable).ok_or_else(|| {
            GalecLowerError::Unsupported(format!(
                "discrete-valued variable `{}` has no Boolean/Integer start value",
                variable.name.as_str()
            ))
        })?;
        let role = if variable.name.as_str().starts_with(SNAPSHOT_PREFIX) {
            GalecVariableRole::Local
        } else if dae
            .metadata
            .discrete_input_names
            .iter()
            .any(|name| name == variable.name.as_str())
        {
            GalecVariableRole::Input
        } else {
            match variable.causality {
                dae::VariableCausality::Input => GalecVariableRole::Input,
                dae::VariableCausality::Output => GalecVariableRole::Output,
                dae::VariableCausality::Parameter => GalecVariableRole::TunableParameter,
                dae::VariableCausality::CalculatedParameter => {
                    GalecVariableRole::DependentParameter
                }
                _ => GalecVariableRole::State,
            }
        };
        let galec_var = lower_variable(variable, role, scalar_ty)?;
        place_variable(galec_var, interface, declarations);
    }
    Ok(())
}

fn lower_variable(
    variable: &dae::Variable,
    role: GalecVariableRole,
    scalar_ty: GalecType,
) -> Result<GalecVariable, GalecLowerError> {
    Ok(GalecVariable {
        name: variable.name.as_str().to_string(),
        ty: lower_variable_type(variable, scalar_ty)?,
        role,
        description: variable.description.clone(),
        start: lower_optional_metadata_expr(variable.start.as_ref())?,
        min: lower_optional_metadata_expr(variable.min.as_ref())?,
        max: lower_optional_metadata_expr(variable.max.as_ref())?,
        nominal: lower_optional_metadata_expr(variable.nominal.as_ref())?,
        unit: variable.unit.clone(),
    })
}

fn place_variable(
    variable: GalecVariable,
    interface: &mut GalecInterface,
    declarations: &mut Vec<GalecDecl>,
) {
    match variable.role {
        GalecVariableRole::Input => interface.inputs.push(variable),
        GalecVariableRole::Output => interface.outputs.push(variable),
        GalecVariableRole::TunableParameter
        | GalecVariableRole::DependentParameter
        | GalecVariableRole::Constant => interface.parameters.push(variable),
        GalecVariableRole::State => interface.states.push(variable),
        GalecVariableRole::Local => declarations.push(GalecDecl {
            variable,
            role: GalecDeclRole::Local,
        }),
    }
}

fn lower_variable_type(
    variable: &dae::Variable,
    scalar_ty: GalecType,
) -> Result<GalecType, GalecLowerError> {
    if variable.dims.is_empty() {
        return Ok(scalar_ty);
    }

    let dims = variable
        .dims
        .iter()
        .map(|dim| {
            usize::try_from(*dim).map_err(|_| {
                GalecLowerError::Unsupported(format!("negative array dimension: {dim}"))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(GalecType::Array {
        element: Box::new(scalar_ty),
        dims,
    })
}

fn infer_scalar_type(variable: &dae::Variable) -> Option<GalecType> {
    match variable.start.as_ref()? {
        Expression::Literal {
            value: Literal::Boolean(_),
            ..
        } => Some(GalecType::Boolean),
        Expression::Literal {
            value: Literal::Integer(_),
            ..
        } => Some(GalecType::Integer),
        Expression::Literal {
            value: Literal::Real(_),
            ..
        } => Some(GalecType::Real),
        _ => None,
    }
}

fn lower_step_block(dae: &dae::Dae) -> Result<GalecBlock, GalecLowerError> {
    if !dae.algorithms.model.is_empty() {
        let statements = dae
            .algorithms
            .model
            .iter()
            .flat_map(|algorithm| algorithm.statements.iter())
            .map(lower_statement_to_galec)
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(GalecBlock { statements });
    }

    let equations = dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .collect::<Vec<_>>();
    let statements = equations
        .iter()
        .filter(|equation| equation.origin == SNAPSHOT_ORIGIN)
        .chain(
            equations
                .iter()
                .filter(|equation| equation.origin != SNAPSHOT_ORIGIN),
        )
        .map(|equation| lower_equation_to_assignment(equation))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(GalecBlock { statements })
}

fn lower_recalibrate_block(dae: &dae::Dae) -> Result<GalecBlock, GalecLowerError> {
    let dependent_parameters = dependent_parameter_names(dae).collect::<Vec<_>>();
    let statements = dae
        .initialization
        .equations
        .iter()
        .filter(|equation| {
            equation
                .lhs
                .as_ref()
                .is_some_and(|lhs| dependent_parameters.contains(&lhs.var_name()))
        })
        .map(lower_equation_to_assignment)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(GalecBlock { statements })
}

fn dependent_parameter_names(dae: &dae::Dae) -> impl Iterator<Item = &VarName> {
    dae.variables
        .parameters
        .iter()
        .filter_map(|(name, variable)| {
            (variable.causality == dae::VariableCausality::CalculatedParameter).then_some(name)
        })
}

fn lower_equations_to_block(equations: &[dae::Equation]) -> Result<GalecBlock, GalecLowerError> {
    let statements = equations
        .iter()
        .map(lower_equation_to_assignment)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(GalecBlock { statements })
}

pub fn lower_statements_to_galec_block(
    statements: &[Statement],
) -> Result<GalecBlock, GalecLowerError> {
    let statements = statements
        .iter()
        .map(lower_statement_to_galec)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(GalecBlock { statements })
}

pub fn lower_statement_to_galec(statement: &Statement) -> Result<GalecStmt, GalecLowerError> {
    lower_stmt(statement)
}

fn lower_equation_to_assignment(equation: &dae::Equation) -> Result<GalecStmt, GalecLowerError> {
    let lhs = equation.lhs.as_ref().ok_or_else(|| {
        GalecLowerError::Unsupported(
            "DAE equation must be explicit before GALEC lowering".to_string(),
        )
    })?;
    let rhs = lower_expr(&equation.rhs)?;

    Ok(GalecStmt::Assign {
        lhs: lhs.as_str().to_string(),
        rhs,
    })
}

fn lower_optional_metadata_expr(
    expr: Option<&Expression>,
) -> Result<Option<GalecExpr>, GalecLowerError> {
    expr.map(lower_expr).transpose()
}

fn lower_expr(expr: &Expression) -> Result<GalecExpr, GalecLowerError> {
    match expr {
        Expression::Literal { value, .. } => lower_literal(value),
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut expr = GalecExpr::Variable(name.as_str().to_string());
            for subscript in subscripts {
                let index = match subscript {
                    Subscript::Index { value, .. } => GalecExpr::IntegerLiteral(*value),
                    Subscript::Colon { .. } => {
                        return Err(GalecLowerError::Unsupported(
                            "colon subscripts are not supported yet".to_string(),
                        ));
                    }
                    Subscript::Expr { expr, .. } => lower_expr(expr)?,
                };
                expr = GalecExpr::BuiltinCall {
                    function: "index".to_string(),
                    args: vec![expr, index],
                };
            }
            Ok(expr)
        }
        Expression::Unary { op, rhs, .. } => {
            let rhs = Box::new(lower_expr(rhs)?);
            match op {
                OpUnary::Minus => Ok(GalecExpr::Neg(rhs)),
                OpUnary::Not => Ok(GalecExpr::Not(rhs)),
                other => Err(GalecLowerError::Unsupported(format!(
                    "unary operator `{other}` is not supported yet"
                ))),
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = Box::new(lower_expr(lhs)?);
            let rhs = Box::new(lower_expr(rhs)?);
            match op {
                OpBinary::Add => Ok(GalecExpr::Add(lhs, rhs)),
                OpBinary::Sub => Ok(GalecExpr::Sub(lhs, rhs)),
                OpBinary::Mul => Ok(GalecExpr::Mul(lhs, rhs)),
                OpBinary::Div => Ok(GalecExpr::Div(lhs, rhs)),
                OpBinary::Exp => Ok(GalecExpr::Pow(lhs, rhs)),
                OpBinary::Eq => Ok(GalecExpr::Eq(lhs, rhs)),
                OpBinary::Neq => Ok(GalecExpr::Neq(lhs, rhs)),
                OpBinary::Lt => Ok(GalecExpr::Lt(lhs, rhs)),
                OpBinary::Le => Ok(GalecExpr::Le(lhs, rhs)),
                OpBinary::Gt => Ok(GalecExpr::Gt(lhs, rhs)),
                OpBinary::Ge => Ok(GalecExpr::Ge(lhs, rhs)),
                OpBinary::And => Ok(GalecExpr::And(lhs, rhs)),
                OpBinary::Or => Ok(GalecExpr::Or(lhs, rhs)),
                other => Err(GalecLowerError::Unsupported(format!(
                    "binary operator `{other}` is not supported yet"
                ))),
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let branches = branches
                .iter()
                .map(|(condition, value)| Ok((lower_expr(condition)?, lower_expr(value)?)))
                .collect::<Result<Vec<_>, GalecLowerError>>()?;
            let else_expr = Box::new(lower_expr(else_branch)?);
            Ok(GalecExpr::If {
                branches,
                else_expr,
            })
        }
        Expression::BuiltinCall { function, args, .. } => {
            let fn_name = builtin_galec_name(*function);
            let args = args
                .iter()
                .map(|arg| lower_expr(arg))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(GalecExpr::BuiltinCall {
                function: fn_name,
                args,
            })
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            let elements = elements
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(GalecExpr::Array(elements))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let base = lower_expr(base)?;
            let mut expr = base;
            for subscript in subscripts {
                let index = match subscript {
                    Subscript::Index { value, .. } => GalecExpr::IntegerLiteral(*value),
                    Subscript::Colon { .. } => {
                        return Err(GalecLowerError::Unsupported(
                            "colon subscripts are not supported yet".to_string(),
                        ));
                    }
                    Subscript::Expr { expr, .. } => lower_expr(expr)?,
                };
                expr = GalecExpr::BuiltinCall {
                    function: "index".to_string(),
                    args: vec![expr, index],
                };
            }
            Ok(expr)
        }
        other => Err(GalecLowerError::Unsupported(format!(
            "expression form `{}` is not supported yet",
            expression_kind(other)
        ))),
    }
}

fn lower_literal(value: &Literal) -> Result<GalecExpr, GalecLowerError> {
    match value {
        Literal::Real(value) => Ok(GalecExpr::RealLiteral(*value)),
        Literal::Integer(value) => Ok(GalecExpr::IntegerLiteral(*value)),
        Literal::Boolean(value) => Ok(GalecExpr::BooleanLiteral(*value)),
        Literal::String(_) => Err(GalecLowerError::Unsupported(
            "string literals are not supported in GALEC expressions yet".to_string(),
        )),
    }
}

fn expression_kind(expr: &Expression) -> &'static str {
    match expr {
        Expression::Binary { .. } => "binary",
        Expression::Unary { .. } => "unary",
        Expression::VarRef { .. } => "variable reference",
        Expression::BuiltinCall { .. } => "builtin call",
        Expression::FunctionCall { .. } => "function call",
        Expression::Literal { .. } => "literal",
        Expression::If { .. } => "if expression",
        Expression::Array { .. } => "array",
        Expression::Tuple { .. } => "tuple",
        Expression::Range { .. } => "range",
        Expression::ArrayComprehension { .. } => "array comprehension",
        Expression::Index { .. } => "index",
        Expression::FieldAccess { .. } => "field access",
        Expression::Empty { .. } => "empty expression",
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GalecLowerError {
    #[error("unsupported GALEC lowering case: {0}")]
    Unsupported(String),
}

fn lower_stmt(statement: &Statement) -> Result<GalecStmt, GalecLowerError> {
    match statement {
        Statement::Assignment { comp, value, .. } => Ok(GalecStmt::Assign {
            lhs: comp.to_string(),
            rhs: lower_expr(&value)?,
        }),

        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let branches = cond_blocks
                .iter()
                .map(|block| {
                    let condition = lower_expr(&block.cond)?;
                    let statements = block
                        .stmts
                        .iter()
                        .map(lower_stmt)
                        .collect::<Result<Vec<_>, GalecLowerError>>()?;

                    Ok((condition, statements))
                })
                .collect::<Result<Vec<_>, GalecLowerError>>()?;

            let else_branch = else_block
                .as_deref()
                .unwrap_or(&[])
                .iter()
                .map(lower_stmt)
                .collect::<Result<Vec<_>, GalecLowerError>>()?;

            Ok(GalecStmt::If {
                branches,
                else_branch,
            })
        }

        other => Err(GalecLowerError::Unsupported(format!(
            "statement form `{}` is not supported yet",
            statement_kind(other),
        ))),
    }
}

fn builtin_galec_name(function: BuiltinFunction) -> String {
    match function {
        BuiltinFunction::Sin => "sin",
        BuiltinFunction::Cos => "cos",
        BuiltinFunction::Tan => "tan",
        BuiltinFunction::Asin => "asin",
        BuiltinFunction::Acos => "acos",
        BuiltinFunction::Atan => "atan",
        BuiltinFunction::Atan2 => "atan2",
        BuiltinFunction::Sqrt => "sqrt",
        BuiltinFunction::Exp => "exp",
        BuiltinFunction::Log => "log",
        BuiltinFunction::Log10 => "log10",
        BuiltinFunction::Abs => "abs",
        BuiltinFunction::Sign => "sign",
        BuiltinFunction::Min => "min",
        BuiltinFunction::Max => "max",
        BuiltinFunction::Div => "div",
        BuiltinFunction::Mod => "mod",
        BuiltinFunction::Rem => "rem",
        BuiltinFunction::Floor => "floor",
        BuiltinFunction::Ceil => "ceil",
        BuiltinFunction::Sinh => "sinh",
        BuiltinFunction::Cosh => "cosh",
        BuiltinFunction::Tanh => "tanh",
        BuiltinFunction::Integer => "integer",
        BuiltinFunction::NoEvent => "noEvent",
        BuiltinFunction::Smooth => "smooth",
        BuiltinFunction::Homotopy => "homotopy",
        _ => "unknown_builtin",
    }
    .to_string()
}

fn statement_kind(statement: &Statement) -> &'static str {
    match statement {
        Statement::Empty { .. } => "empty statement",
        Statement::Assignment { .. } => "assignment",
        Statement::Return { .. } => "return",
        Statement::Break { .. } => "break",
        Statement::For { .. } => "for statement",
        Statement::While { .. } => "while statement",
        Statement::If { .. } => "if statement",
        Statement::When { .. } => "when statement",
        Statement::FunctionCall { .. } => "function call",
        Statement::Reinit { .. } => "reinit",
        Statement::Assert { .. } => "assert",
    }
}
