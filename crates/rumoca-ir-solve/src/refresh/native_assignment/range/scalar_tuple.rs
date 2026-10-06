//! Exact scalar residuals packed by original immutable Move producers.

#[cfg(test)]
mod tests;

use super::*;

pub(super) fn is_packed(prefix: ProgramPrefix<'_>, output: Reg) -> bool {
    prefix
        .producer_position(output)
        .and_then(|position| prefix.operation(position))
        .is_some_and(|op| matches!(op, LinearOp::Move { dst, .. } if *dst == output))
}

pub(super) fn derive(
    prefix: ProgramPrefix<'_>,
    output: Reg,
    targets: &Range<usize>,
    dependencies: &dependency::Dependencies,
) -> Checked<Vec<Reg>> {
    targets
        .clone()
        .enumerate()
        .map(|(offset, target)| {
            let register = u32::try_from(offset)
                .ok()
                .and_then(|offset| output.checked_add(offset))
                .ok_or(NativeRefreshAssignmentRefusal(
                    "native scalar tuple output registers overflow",
                ))?;
            if !is_packed(prefix, register) {
                return refused("native scalar tuple has mixed output producers");
            }
            isolated(prefix, register, target, dependencies)
        })
        .collect()
}

fn definition<'a>(
    mut prefix: ProgramPrefix<'a>,
    mut register: Reg,
) -> Checked<(ProgramPrefix<'a>, Reg, usize)> {
    loop {
        let position = prefix
            .producer_position(register)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native scalar tuple has no exact producer",
            ))?;
        let before = prefix
            .before(position)
            .ok_or(NativeRefreshAssignmentRefusal(
                "invalid native scalar tuple prefix",
            ))?;
        match prefix.operation(position) {
            Some(LinearOp::Move { dst, src }) if *dst == register => {
                prefix = before;
                register = *src;
            }
            _ => return Ok((prefix, register, position)),
        }
    }
}

fn isolated(
    prefix: ProgramPrefix<'_>,
    output: Reg,
    target: usize,
    dependencies: &dependency::Dependencies,
) -> Checked<Reg> {
    let (prefix, register, position) = definition(prefix, output)?;
    let Some(LinearOp::Binary {
        dst,
        op: BinaryOp::Sub,
        lhs,
        rhs,
    }) = prefix.operation(position)
    else {
        return refused("native scalar tuple has no direct scalar isolator");
    };
    if *dst != register {
        return refused("native scalar tuple selects a partial producer");
    }
    let before = prefix
        .before(position)
        .ok_or(NativeRefreshAssignmentRefusal(
            "invalid native scalar tuple prefix",
        ))?;
    for (load, value) in [(*lhs, *rhs), (*rhs, *lhs)] {
        let (source, load, position) = definition(before, load)?;
        if matches!(source.operation(position), Some(LinearOp::LoadY { dst, index })
            if *dst == load && *index == target)
            && !dependencies.depends_on(before, value, 1, 1)?
        {
            return Ok(value);
        }
    }
    refused("native scalar tuple couples its owned target range")
}

pub(super) fn pack(operations: &mut Vec<LinearOp>, registers: &[Reg]) -> Checked<Reg> {
    let start = next_register(operations)?;
    let last = registers
        .len()
        .checked_sub(1)
        .and_then(|last| u32::try_from(last).ok());
    if last.and_then(|last| start.checked_add(last)).is_none() {
        return refused("native scalar tuple materialization overflows");
    }
    operations.extend(
        registers
            .iter()
            .enumerate()
            .map(|(offset, &src)| LinearOp::Move {
                dst: start + offset as u32,
                src,
            }),
    );
    Ok(start)
}
