use crate::{LinearOp as Op, ScalarProgramBlock, SolvePureCallTable, TensorInputKind};

pub(super) fn outputs(
    table: &SolvePureCallTable,
    block: &ScalarProgramBlock,
    y: &[bool],
    p: &[bool],
) -> Option<Vec<bool>> {
    let mut outputs = Vec::new();
    for program in block.programs() {
        outputs.extend(program_outputs(table, program, y, p)?);
    }
    Some(outputs)
}

pub(in crate::fmi) fn program_outputs(
    table: &SolvePureCallTable,
    program: &[Op],
    y: &[bool],
    p: &[bool],
) -> Option<Vec<bool>> {
    let mut outputs = Vec::new();
    let mut registers = Vec::new();
    for op in program {
        let end = op
            .dst_register()
            .map_or(0, |dst| dst as usize + op.dst_register_count());
        registers.resize(registers.len().max(end), false);
        transfer(table, op, &mut registers, y, p, &mut outputs)?;
    }
    Some(outputs)
}

fn transfer(
    table: &SolvePureCallTable,
    op: &Op,
    r: &mut [bool],
    y: &[bool],
    p: &[bool],
    outputs: &mut Vec<bool>,
) -> Option<()> {
    match op {
        Op::Const { dst, .. } => r[*dst as usize] = true,
        Op::LoadY { dst, index } => r[*dst as usize] = *y.get(*index)?,
        Op::LoadP { dst, index } => r[*dst as usize] = *p.get(*index)?,
        Op::LoadTime { dst } => r[*dst as usize] = false,
        Op::Move { dst, src } => r[*dst as usize] = r[*src as usize],
        Op::Unary { dst, arg, .. } => r[*dst as usize] = r[*arg as usize],
        Op::Binary { dst, lhs, rhs, .. } | Op::Compare { dst, lhs, rhs, .. } => {
            r[*dst as usize] = r[*lhs as usize] && r[*rhs as usize]
        }
        Op::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => r[*dst as usize] = r[*cond as usize] && r[*if_true as usize] && r[*if_false as usize],
        Op::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            lanes: 1,
            ..
        } => {
            let source = match input {
                TensorInputKind::Y => y,
                TensorInputKind::P => p,
            };
            r.get_mut(*dst_start as usize..*dst_start as usize + count)?
                .copy_from_slice(source.get(*input_start..*input_start + count)?);
        }
        Op::PureCall {
            dst_start,
            input_starts,
            site,
        } => {
            let inputs = input_starts
                .iter()
                .zip(site.inputs())
                .map(|(start, ty)| {
                    r.get(*start as usize..*start as usize + ty.scalar_count() as usize)
                        .map(|v| v.iter().all(|x| *x))
                })
                .collect::<Option<Vec<_>>>()?;
            let owner = table.owner(site.owner())?;
            let values = super::typed_dependencies::outputs(table, owner.body(), &inputs);
            let mut start = *dst_start as usize;
            for (value, output) in values.iter().zip(owner.outputs()) {
                let end = start + output.value_type().scalar_count() as usize;
                r.get_mut(start..end)?.fill(*value);
                start = end;
            }
        }
        Op::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => {
            for i in 0..*count {
                r[*dst_start as usize + i] = r[*lhs_start as usize + i * lhs_stride]
                    && r[*rhs_start as usize + i * rhs_stride];
            }
        }
        Op::StoreOutput { src } => outputs.push(r[*src as usize]),
        Op::StoreOutputRange {
            start,
            count,
            stride,
        } => {
            for i in 0..*count {
                outputs.push(*r.get(*start as usize + i * stride)?);
            }
        }
        _ => {
            let dst = op.dst_register()? as usize;
            r.get_mut(dst..dst + op.dst_register_count())?.fill(false);
        }
    }
    Some(())
}
