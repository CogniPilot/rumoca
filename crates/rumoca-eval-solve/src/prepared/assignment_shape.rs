use std::collections::HashMap;

use rumoca_ir_solve::{BinaryOp, LinearOp, Reg, ScalarProgramBlock, UnaryOp, resolve_indexed_slot};

use super::dependency::reg_depends_on_y_index;
use super::{invalid_prepared_row, producer};
use crate::{EvalSolveError, RowEvalContext, eval_binary, eval_compare, eval_unary};

#[derive(Clone, Copy, Default)]
struct AffineScalar {
    coefficient: f64,
    offset: f64,
}

#[derive(Default)]
pub(super) struct AffineTargetScratch {
    registers: Vec<AffineScalar>,
}

/// Scalar Solve-IR row shape that can update one solver-Y slot directly.
///
/// Code generators use the same analysis as the interpreter so compiled
/// projection sweeps preserve assignment-row semantics.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TargetAssignmentShape {
    Direct {
        target_y_index: usize,
        expr_reg: u32,
        target_scale: f64,
        expr_eval_len: usize,
    },
    Affine {
        target_y_index: usize,
        offset_reg: u32,
        coefficient_reg: Option<u32>,
        offset_scale: f64,
        coefficient_scale: f64,
        expr_eval_len: usize,
    },
}

impl TargetAssignmentShape {
    pub fn target_y_index(self) -> usize {
        match self {
            Self::Direct { target_y_index, .. } | Self::Affine { target_y_index, .. } => {
                target_y_index
            }
        }
    }

    pub fn expr_eval_len(self) -> usize {
        match self {
            Self::Direct { expr_eval_len, .. } | Self::Affine { expr_eval_len, .. } => {
                expr_eval_len
            }
        }
    }

    pub(super) fn eval_value(
        self,
        row_idx: usize,
        regs: &[f64],
        span: Option<rumoca_core::Span>,
    ) -> Result<f64, EvalSolveError> {
        match self {
            Self::Direct { expr_reg, .. } => read_shape_reg(regs, expr_reg, span),
            Self::Affine {
                target_y_index,
                offset_reg,
                coefficient_reg,
                offset_scale,
                coefficient_scale,
                ..
            } => {
                let offset = offset_scale * read_shape_reg(regs, offset_reg, span)?;
                let coefficient = coefficient_scale
                    * coefficient_reg.map_or(Ok(1.0), |reg| read_shape_reg(regs, reg, span))?;
                if coefficient == 0.0 || !coefficient.is_finite() {
                    return Err(EvalSolveError::SingularTargetAssignment {
                        row: row_idx,
                        target_y_index,
                        coefficient,
                        span,
                    });
                }
                Ok(-offset / coefficient)
            }
        }
    }
}

/// Recognize a scalar row that can be evaluated as `target = expression`.
pub fn target_assignment_shape(
    row: &[LinearOp],
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    Ok(target_assignment_shapes(row)?.into_iter().next())
}

/// Prove that a scalar residual row is affine in one solver-Y variable.
///
/// Coefficients may depend on time, parameters, and other solver variables;
/// only dependence on `target_y_index` is classified. The conservative result
/// is used to authorize exact scalar pivoting during algebraic-loop tearing.
pub fn row_is_affine_in_y_index(row: &[LinearOp], target_y_index: usize) -> bool {
    if ScalarProgramBlock::program_output_count(row) != 1 {
        return false;
    }
    let mut degrees = HashMap::<Reg, TargetDegree>::new();
    let mut output = None;
    for op in row {
        if let Some((dst, degree)) = target_degree_for_op(op, &degrees, target_y_index) {
            degrees.insert(dst, degree);
        }
        if let LinearOp::StoreOutput { src } = *op {
            output = degrees.get(&src).copied();
        }
    }
    matches!(output, Some(TargetDegree::Affine))
}

/// Evaluate the exact scalar pivot of a row proven affine in `target_y_index`.
///
/// Every register carries `(a, b)` for the symbolic value `a * target + b`.
/// This fuses the residual and target-coefficient evaluations into one pass
/// while preserving the same guarded scalar operations as the canonical row
/// evaluator. The caller only invokes this for targets accepted by
/// [`row_is_affine_in_y_index`].
pub(super) struct AffineTargetRequest<'a> {
    pub(super) row_idx: usize,
    pub(super) row: &'a [LinearOp],
    pub(super) register_count: usize,
    pub(super) target_y_index: usize,
    pub(super) y: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) t: f64,
    pub(super) context: RowEvalContext<'a>,
}

pub(super) fn eval_affine_target_value(
    request: AffineTargetRequest<'_>,
    scratch: &mut AffineTargetScratch,
) -> Result<f64, EvalSolveError> {
    scratch.registers.clear();
    scratch
        .registers
        .resize(request.register_count, AffineScalar::default());
    let mut output = None;
    for op in request.row {
        eval_affine_op(
            *op,
            request.target_y_index,
            request.y,
            request.p,
            request.t,
            request.context,
            &mut scratch.registers,
            &mut output,
        )?;
    }
    let Some(AffineScalar {
        coefficient,
        offset,
    }) = output
    else {
        return Err(invalid_prepared_row(
            "affine target row has no scalar output",
        ));
    };
    if coefficient == 0.0 || !coefficient.is_finite() {
        return Err(EvalSolveError::SingularTargetAssignment {
            row: request.row_idx,
            target_y_index: request.target_y_index,
            coefficient,
            span: None,
        });
    }
    Ok(-offset / coefficient)
}

#[allow(clippy::too_many_arguments)]
fn eval_affine_op(
    op: LinearOp,
    target_y_index: usize,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    registers: &mut [AffineScalar],
    output: &mut Option<AffineScalar>,
) -> Result<(), EvalSolveError> {
    let value = match op {
        LinearOp::Const { value, .. } => affine_constant(value),
        LinearOp::LoadTime { .. } => affine_constant(t),
        LinearOp::LoadY { index, .. } if index == target_y_index => AffineScalar {
            coefficient: 1.0,
            offset: 0.0,
        },
        LinearOp::LoadY { index, .. } => affine_constant(read_input("y", y, index)?),
        LinearOp::LoadP { index, .. } => affine_constant(read_input("p", p, index)?),
        LinearOp::LoadSeed { index, .. } => affine_constant(read_optional_seed(context, index)?),
        LinearOp::LoadIndexedP {
            base, count, index, ..
        } => {
            let index = independent_register(registers, index)?;
            affine_constant(read_input(
                "p",
                p,
                resolve_indexed_slot(index, base, count),
            )?)
        }
        LinearOp::LoadIndexedSeed {
            base, count, index, ..
        } => {
            let index = independent_register(registers, index)?;
            affine_constant(read_optional_seed(
                context,
                resolve_indexed_slot(index, base, count),
            )?)
        }
        LinearOp::Move { src, .. } => read_register(registers, src)?,
        LinearOp::Unary { op, arg, .. } => affine_unary(op, read_register(registers, arg)?)?,
        LinearOp::Binary { op, lhs, rhs, .. } => affine_binary(
            op,
            read_register(registers, lhs)?,
            read_register(registers, rhs)?,
        )?,
        LinearOp::Compare { op, lhs, rhs, .. } => affine_constant(eval_compare(
            op,
            independent_register(registers, lhs)?,
            independent_register(registers, rhs)?,
        )),
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            if independent_register(registers, cond)? != 0.0 {
                read_register(registers, if_true)?
            } else {
                read_register(registers, if_false)?
            }
        }
        LinearOp::StoreOutput { src } => {
            *output = Some(read_register(registers, src)?);
            return Ok(());
        }
        _ => {
            return Err(invalid_prepared_row(
                "affine target row contains an unsupported operation",
            ));
        }
    };
    let dst = op
        .dst_register()
        .ok_or_else(|| invalid_prepared_row("affine target operation has no destination"))?;
    write_register(registers, dst, value)
}

fn affine_constant(offset: f64) -> AffineScalar {
    AffineScalar {
        coefficient: 0.0,
        offset,
    }
}

fn affine_unary(op: UnaryOp, value: AffineScalar) -> Result<AffineScalar, EvalSolveError> {
    if value.coefficient == 0.0 {
        return Ok(affine_constant(eval_unary(op, value.offset)));
    }
    if op == UnaryOp::Neg {
        return Ok(AffineScalar {
            coefficient: -value.coefficient,
            offset: -value.offset,
        });
    }
    Err(invalid_prepared_row(
        "nonlinear unary operation depends on affine target",
    ))
}

fn affine_binary(
    op: BinaryOp,
    lhs: AffineScalar,
    rhs: AffineScalar,
) -> Result<AffineScalar, EvalSolveError> {
    if lhs.coefficient == 0.0 && rhs.coefficient == 0.0 {
        return Ok(affine_constant(eval_binary(op, lhs.offset, rhs.offset)));
    }
    match op {
        BinaryOp::Add => Ok(AffineScalar {
            coefficient: lhs.coefficient + rhs.coefficient,
            offset: lhs.offset + rhs.offset,
        }),
        BinaryOp::Sub => Ok(AffineScalar {
            coefficient: lhs.coefficient - rhs.coefficient,
            offset: lhs.offset - rhs.offset,
        }),
        BinaryOp::Mul if lhs.coefficient == 0.0 => Ok(AffineScalar {
            coefficient: lhs.offset * rhs.coefficient,
            offset: lhs.offset * rhs.offset,
        }),
        BinaryOp::Mul if rhs.coefficient == 0.0 => Ok(AffineScalar {
            coefficient: lhs.coefficient * rhs.offset,
            offset: lhs.offset * rhs.offset,
        }),
        BinaryOp::Div if rhs.coefficient == 0.0 => Ok(AffineScalar {
            coefficient: lhs.coefficient / rhs.offset,
            offset: eval_binary(BinaryOp::Div, lhs.offset, rhs.offset),
        }),
        _ => Err(invalid_prepared_row(
            "nonlinear binary operation depends on affine target",
        )),
    }
}

fn read_register(
    registers: &[AffineScalar],
    register: Reg,
) -> Result<AffineScalar, EvalSolveError> {
    registers
        .get(register as usize)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register,
            len: registers.len(),
            span: None,
        })
}

fn independent_register(registers: &[AffineScalar], register: Reg) -> Result<f64, EvalSolveError> {
    let value = read_register(registers, register)?;
    if value.coefficient == 0.0 {
        return Ok(value.offset);
    }
    Err(invalid_prepared_row(
        "affine target unexpectedly controls a discrete operation",
    ))
}

fn write_register(
    registers: &mut [AffineScalar],
    register: Reg,
    value: AffineScalar,
) -> Result<(), EvalSolveError> {
    let len = registers.len();
    let slot = registers
        .get_mut(register as usize)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "write",
            register,
            len,
            span: None,
        })?;
    *slot = value;
    Ok(())
}

fn read_input(vector: &'static str, values: &[f64], index: usize) -> Result<f64, EvalSolveError> {
    values
        .get(index)
        .copied()
        .ok_or(EvalSolveError::MissingInput {
            vector,
            index,
            len: values.len(),
            span: None,
        })
}

fn read_optional_seed(context: RowEvalContext<'_>, index: usize) -> Result<f64, EvalSolveError> {
    let seed = context.seed.ok_or(EvalSolveError::MissingInput {
        vector: "seed",
        index,
        len: 0,
        span: None,
    })?;
    read_input("seed", seed, index)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TargetDegree {
    Independent,
    Affine,
    Nonlinear,
}

fn target_degree_for_op(
    op: &LinearOp,
    degrees: &HashMap<Reg, TargetDegree>,
    target_y_index: usize,
) -> Option<(Reg, TargetDegree)> {
    use TargetDegree::{Affine, Independent, Nonlinear};
    let degree = |reg| degrees.get(&reg).copied().unwrap_or(Nonlinear);
    let result = match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. } => (dst, Independent),
        LinearOp::LoadY { dst, index } => (
            dst,
            if index == target_y_index {
                Affine
            } else {
                Independent
            },
        ),
        LinearOp::Move { dst, src } => (dst, degree(src)),
        LinearOp::Unary { dst, op, arg } => (
            dst,
            match (op, degree(arg)) {
                (_, Independent) => Independent,
                (UnaryOp::Neg, Affine) => Affine,
                _ => Nonlinear,
            },
        ),
        LinearOp::Binary { dst, op, lhs, rhs } => {
            (dst, binary_target_degree(op, degree(lhs), degree(rhs)))
        }
        LinearOp::LoadIndexedP { dst, index, .. }
        | LinearOp::LoadIndexedSeed { dst, index, .. } => (
            dst,
            if degree(index) == Independent {
                Independent
            } else {
                Nonlinear
            },
        ),
        LinearOp::Compare { dst, lhs, rhs, .. } => (
            dst,
            if degree(lhs) == Independent && degree(rhs) == Independent {
                Independent
            } else {
                Nonlinear
            },
        ),
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => (
            dst,
            if degree(cond) == Independent {
                merge_affine_branches(degree(if_true), degree(if_false))
            } else {
                Nonlinear
            },
        ),
        _ => (op.dst_register()?, Nonlinear),
    };
    Some(result)
}

fn binary_target_degree(op: BinaryOp, lhs: TargetDegree, rhs: TargetDegree) -> TargetDegree {
    use TargetDegree::{Affine, Independent, Nonlinear};
    match op {
        BinaryOp::Add | BinaryOp::Sub => merge_affine_branches(lhs, rhs),
        BinaryOp::Mul => match (lhs, rhs) {
            (Independent, Independent) => Independent,
            (Independent, Affine) | (Affine, Independent) => Affine,
            _ => Nonlinear,
        },
        BinaryOp::Div => match (lhs, rhs) {
            (Independent, Independent) => Independent,
            (Affine, Independent) => Affine,
            _ => Nonlinear,
        },
        _ if lhs == Independent && rhs == Independent => Independent,
        _ => Nonlinear,
    }
}

fn merge_affine_branches(lhs: TargetDegree, rhs: TargetDegree) -> TargetDegree {
    use TargetDegree::{Affine, Independent, Nonlinear};
    match (lhs, rhs) {
        (Nonlinear, _) | (_, Nonlinear) => Nonlinear,
        (Affine, _) | (_, Affine) => Affine,
        (Independent, Independent) => Independent,
    }
}

pub fn target_assignment_shapes(
    row: &[LinearOp],
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    if ScalarProgramBlock::program_output_count(row) > 1 {
        return Ok(Vec::new());
    }
    let Some(output_reg) = store_output_reg(row) else {
        return Ok(Vec::new());
    };
    let mut shapes = Vec::new();
    if let Some(shape) = direct_assignment_shape(row, output_reg)? {
        shapes.push(shape);
    }
    for shape in affine_assignment_shapes(row, output_reg)? {
        if shapes
            .iter()
            .all(|existing| existing.target_y_index() != shape.target_y_index())
        {
            shapes.push(shape);
        }
    }
    Ok(shapes)
}

fn read_shape_reg(
    regs: &[f64],
    reg: u32,
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    regs.get(reg as usize)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: reg,
            len: regs.len(),
            span,
        })
}

fn direct_assignment_shape(
    row: &[LinearOp],
    output_reg: u32,
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    let Some((target_reg, expr_reg, target_scale)) = assignment_expr_reg(row, output_reg) else {
        return Ok(None);
    };
    let Some(target_y_index) = target_load_index(row, target_reg) else {
        return Ok(None);
    };
    if reg_depends_on_y_index(row, expr_reg, target_y_index) {
        return Ok(None);
    }
    let Some(expr_pos) = producer_pos(row, expr_reg) else {
        return Ok(None);
    };
    let expr_eval_len = checked_expr_eval_len(expr_pos)?;
    Ok(Some(TargetAssignmentShape::Direct {
        target_y_index,
        expr_reg,
        target_scale,
        expr_eval_len,
    }))
}

fn affine_assignment_shapes(
    row: &[LinearOp],
    output_reg: u32,
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    let Some(output_op) = producer(row, output_reg) else {
        return Ok(Vec::new());
    };
    let (lhs, rhs, lhs_scale, rhs_scale) = match *output_op {
        LinearOp::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, 1.0, 1.0),
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, 1.0, -1.0),
        _ => return Ok(Vec::new()),
    };
    let mut shapes = Vec::new();
    if let Some((target_reg, coefficient_reg)) = affine_target_term(row, lhs) {
        shapes.extend(affine_sum_side_shape(
            row,
            target_reg,
            coefficient_reg,
            lhs_scale,
            rhs,
            rhs_scale,
        )?);
    }
    if let Some((target_reg, coefficient_reg)) = affine_target_term(row, rhs) {
        shapes.extend(affine_sum_side_shape(
            row,
            target_reg,
            coefficient_reg,
            rhs_scale,
            lhs,
            lhs_scale,
        )?);
    }
    Ok(shapes)
}

fn affine_sum_side_shape(
    row: &[LinearOp],
    target_reg: u32,
    coefficient_reg: Option<u32>,
    coefficient_scale: f64,
    offset_reg: u32,
    offset_scale: f64,
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    let Some(target_y_index) = target_load_index(row, target_reg) else {
        return Ok(None);
    };
    if coefficient_reg.is_some_and(|reg| reg_depends_on_y_index(row, reg, target_y_index))
        || reg_depends_on_y_index(row, offset_reg, target_y_index)
    {
        return Ok(None);
    }
    let coefficient_pos = coefficient_reg
        .and_then(|reg| producer_pos(row, reg))
        .unwrap_or(0);
    let Some(offset_pos) = producer_pos(row, offset_reg) else {
        return Ok(None);
    };
    let expr_eval_len = checked_expr_eval_len(coefficient_pos.max(offset_pos))?;
    Ok(Some(TargetAssignmentShape::Affine {
        target_y_index,
        offset_reg,
        coefficient_reg,
        offset_scale,
        coefficient_scale,
        expr_eval_len,
    }))
}

pub(super) fn checked_expr_eval_len(pos: usize) -> Result<usize, EvalSolveError> {
    pos.checked_add(1)
        .ok_or_else(|| invalid_prepared_row("target assignment expression length overflows"))
}

fn affine_target_term(row: &[LinearOp], reg: u32) -> Option<(u32, Option<u32>)> {
    if is_y_load(row, reg) {
        return Some((reg, None));
    }
    let LinearOp::Binary {
        op: BinaryOp::Mul,
        lhs,
        rhs,
        ..
    } = *producer(row, reg)?
    else {
        return None;
    };
    match (is_y_load(row, lhs), is_y_load(row, rhs)) {
        (true, false) => Some((lhs, Some(rhs))),
        (false, true) => Some((rhs, Some(lhs))),
        _ => None,
    }
}

fn store_output_reg(row: &[LinearOp]) -> Option<u32> {
    row.iter().rev().find_map(|op| match *op {
        LinearOp::StoreOutput { src } => Some(src),
        _ => None,
    })
}

fn target_load_index(row: &[LinearOp], target_reg: u32) -> Option<usize> {
    row.iter().find_map(|op| match *op {
        LinearOp::LoadY { dst, index } if dst == target_reg => Some(index),
        _ => None,
    })
}

fn assignment_expr_reg(row: &[LinearOp], output_reg: u32) -> Option<(u32, u32, f64)> {
    let output_op = producer(row, output_reg)?;
    match *output_op {
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => sub_assignment_expr_reg(row, lhs, rhs, 1.0),
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => {
            let LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            } = *producer(row, arg)?
            else {
                return None;
            };
            sub_assignment_expr_reg(row, lhs, rhs, -1.0)
        }
        _ => None,
    }
}

fn producer_pos(row: &[LinearOp], dst_reg: u32) -> Option<usize> {
    row.iter()
        .rposition(|op| op.dst_register() == Some(dst_reg))
}

fn sub_assignment_expr_reg(
    row: &[LinearOp],
    lhs: u32,
    rhs: u32,
    output_scale: f64,
) -> Option<(u32, u32, f64)> {
    if is_y_load(row, lhs) {
        Some((lhs, rhs, output_scale))
    } else if is_y_load(row, rhs) {
        Some((rhs, lhs, -output_scale))
    } else {
        None
    }
}

fn is_y_load(row: &[LinearOp], reg: u32) -> bool {
    matches!(producer(row, reg), Some(LinearOp::LoadY { .. }))
}
