use rumoca_ir_solve as solve;

use crate::lower::LowerError;

use super::stencil_contract_violation;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructuredAccessProof {
    pub(super) operands: Vec<StructuredAccessOperand>,
}

#[derive(Debug, Default)]
pub(crate) struct StructuredAccessProofBuilder {
    operands: Vec<StructuredAccessOperand>,
}

impl StructuredAccessProofBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn collect_expression_result<F, E>(
        &mut self,
        expression: &rumoca_core::Expression,
        collect_var_ref: F,
    ) -> Result<Option<()>, E>
    where
        F: FnMut(
            &str,
            &[rumoca_core::Subscript],
            rumoca_core::Span,
            &mut Vec<StructuredAccessOperand>,
        ) -> Result<Option<()>, E>,
    {
        collect_structured_access_operands_result(expression, &mut self.operands, collect_var_ref)
    }

    pub(crate) fn finish(self) -> StructuredAccessProof {
        StructuredAccessProof {
            operands: self.operands,
        }
    }

    pub(crate) fn append_to(
        self,
        operands: &mut Vec<StructuredAccessOperand>,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        operands
            .try_reserve_exact(self.operands.len())
            .map_err(|_| {
                stencil_contract_violation(
                    "structured access operand append capacity exceeds host memory limits",
                    span,
                )
            })?;
        operands.extend(self.operands);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StructuredAccessOperand {
    Const(f64),
    LoadY(usize),
    LoadP(usize),
}

fn collect_structured_access_operands_result<F, E>(
    expression: &rumoca_core::Expression,
    operands: &mut Vec<StructuredAccessOperand>,
    mut collect_var_ref: F,
) -> Result<Option<()>, E>
where
    F: FnMut(
        &str,
        &[rumoca_core::Subscript],
        rumoca_core::Span,
        &mut Vec<StructuredAccessOperand>,
    ) -> Result<Option<()>, E>,
{
    collect_structured_access_operands_result_inner(expression, operands, &mut collect_var_ref)
}

fn collect_structured_access_operands_result_inner<F, E>(
    expression: &rumoca_core::Expression,
    operands: &mut Vec<StructuredAccessOperand>,
    collect_var_ref: &mut F,
) -> Result<Option<()>, E>
where
    F: FnMut(
        &str,
        &[rumoca_core::Subscript],
        rumoca_core::Span,
        &mut Vec<StructuredAccessOperand>,
    ) -> Result<Option<()>, E>,
{
    match expression {
        rumoca_core::Expression::Literal { value, .. } => {
            let Some(value) = structured_access_literal_value(value) else {
                return Ok(None);
            };
            operands.push(StructuredAccessOperand::Const(value));
            Ok(Some(()))
        }
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => collect_var_ref(name.as_str(), subscripts, *span, operands),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            let Some(()) =
                collect_structured_access_operands_result_inner(lhs, operands, collect_var_ref)?
            else {
                return Ok(None);
            };
            collect_structured_access_operands_result_inner(rhs, operands, collect_var_ref)
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_structured_access_operands_result_inner(rhs, operands, collect_var_ref)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let [single] = elements.as_slice() else {
                return Ok(None);
            };
            collect_structured_access_operands_result_inner(single, operands, collect_var_ref)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            collect_structured_builtin_access_operands_result(
                *function,
                args,
                operands,
                collect_var_ref,
            )
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => collect_structured_if_access_operands_result(
            branches,
            else_branch,
            operands,
            collect_var_ref,
        ),
        _ => Ok(None),
    }
}

fn collect_structured_if_access_operands_result<F, E>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    operands: &mut Vec<StructuredAccessOperand>,
    collect_var_ref: &mut F,
) -> Result<Option<()>, E>
where
    F: FnMut(
        &str,
        &[rumoca_core::Subscript],
        rumoca_core::Span,
        &mut Vec<StructuredAccessOperand>,
    ) -> Result<Option<()>, E>,
{
    let Some(()) =
        collect_structured_access_operands_result_inner(else_branch, operands, collect_var_ref)?
    else {
        return Ok(None);
    };
    for (condition, value) in branches.iter().rev() {
        let Some(()) =
            collect_structured_access_operands_result_inner(condition, operands, collect_var_ref)?
        else {
            return Ok(None);
        };
        let Some(()) =
            collect_structured_access_operands_result_inner(value, operands, collect_var_ref)?
        else {
            return Ok(None);
        };
    }
    Ok(Some(()))
}

fn collect_structured_builtin_access_operands_result<F, E>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    operands: &mut Vec<StructuredAccessOperand>,
    collect_var_ref: &mut F,
) -> Result<Option<()>, E>
where
    F: FnMut(
        &str,
        &[rumoca_core::Subscript],
        rumoca_core::Span,
        &mut Vec<StructuredAccessOperand>,
    ) -> Result<Option<()>, E>,
{
    match function {
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Der => {
            operands.push(StructuredAccessOperand::Const(0.0));
            Ok(Some(()))
        }
        rumoca_core::BuiltinFunction::Ones | rumoca_core::BuiltinFunction::Identity => {
            operands.push(StructuredAccessOperand::Const(1.0));
            Ok(Some(()))
        }
        rumoca_core::BuiltinFunction::Fill => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            collect_structured_access_operands_result_inner(value, operands, collect_var_ref)
        }
        _ if function.is_unary_real_math() => {
            collect_structured_builtin_args_access_operands_result(args, operands, collect_var_ref)
        }
        rumoca_core::BuiltinFunction::Abs
        | rumoca_core::BuiltinFunction::Sign
        | rumoca_core::BuiltinFunction::Floor
        | rumoca_core::BuiltinFunction::Integer
        | rumoca_core::BuiltinFunction::Ceil
        | rumoca_core::BuiltinFunction::Atan2
        | rumoca_core::BuiltinFunction::Div
        | rumoca_core::BuiltinFunction::Mod
        | rumoca_core::BuiltinFunction::Rem
        | rumoca_core::BuiltinFunction::Min
        | rumoca_core::BuiltinFunction::Max => {
            collect_structured_builtin_args_access_operands_result(args, operands, collect_var_ref)
        }
        rumoca_core::BuiltinFunction::NoEvent => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            collect_structured_access_operands_result_inner(value, operands, collect_var_ref)
        }
        rumoca_core::BuiltinFunction::Smooth => {
            let Some(value) = args.get(1) else {
                return Ok(None);
            };
            collect_structured_access_operands_result_inner(value, operands, collect_var_ref)
        }
        _ => Ok(None),
    }
}

fn collect_structured_builtin_args_access_operands_result<F, E>(
    args: &[rumoca_core::Expression],
    operands: &mut Vec<StructuredAccessOperand>,
    collect_var_ref: &mut F,
) -> Result<Option<()>, E>
where
    F: FnMut(
        &str,
        &[rumoca_core::Subscript],
        rumoca_core::Span,
        &mut Vec<StructuredAccessOperand>,
    ) -> Result<Option<()>, E>,
{
    for arg in args {
        let Some(()) =
            collect_structured_access_operands_result_inner(arg, operands, collect_var_ref)?
        else {
            return Ok(None);
        };
    }
    Ok(Some(()))
}

fn structured_access_literal_value(value: &rumoca_core::Literal) -> Option<f64> {
    match value {
        rumoca_core::Literal::Real(value) => Some(*value),
        rumoca_core::Literal::Integer(value) => Some(*value as f64),
        rumoca_core::Literal::Boolean(value) => Some(u8::from(*value).into()),
        rumoca_core::Literal::String(_) => None,
    }
}

pub(crate) fn structured_access_operand_for_slot(
    slot: solve::ScalarSlot,
) -> Option<StructuredAccessOperand> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some(StructuredAccessOperand::LoadY(index)),
        solve::ScalarSlot::P { index, .. } => Some(StructuredAccessOperand::LoadP(index)),
        solve::ScalarSlot::Constant(value) => Some(StructuredAccessOperand::Const(value)),
        solve::ScalarSlot::Time => None,
    }
}
