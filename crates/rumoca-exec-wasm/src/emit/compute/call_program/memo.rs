//! A complete pure-call input coordinate determined by immutable P/time/constant cells.
//! Shared call results are reused only after successful complete-tuple evaluation.
use super::*;
use crate::typed_call::program::ProgramHelpers;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Input {
    Parameter { start: usize, count: usize },
    Constant(u64),
    Time,
}

pub(super) struct Memo {
    site: solve::SolvePureCallSite,
    inputs: Vec<Input>,
    pub flag: u32,
    pub tuple: u32,
    pub bytes: u32,
}

pub(super) fn derive(
    rows: &[Vec<LinearOp>],
    helpers: &ProgramHelpers,
    bytes: &mut u32,
) -> Result<Vec<Memo>, String> {
    let mut memos: Vec<Memo> = Vec::new();
    for row in rows {
        for (index, op) in row.iter().enumerate() {
            let Some((site, inputs)) = coordinate(&row[..index], op) else {
                continue;
            };
            if memos.iter().any(|m| m.site == *site && m.inputs == inputs) {
                continue;
            }
            let layout = helpers.layout(site).map_err(|e| e.to_string())?;
            let flag = *bytes;
            let tuple = flag.checked_add(8).ok_or("native memo flag overflow")?;
            *bytes = tuple
                .checked_add(layout.output)
                .filter(|&n| n <= 64 * 1024 * 1024)
                .ok_or("native whole-program memo exceeds 64 MiB")?;
            memos.push(Memo {
                site: site.clone(),
                inputs,
                flag,
                tuple,
                bytes: layout.output,
            });
        }
    }
    Ok(memos)
}

pub(super) fn find<'a>(memos: &'a [Memo], prefix: &[LinearOp], op: &LinearOp) -> Option<&'a Memo> {
    let (site, inputs) = coordinate(prefix, op)?;
    memos
        .iter()
        .find(|memo| memo.site == *site && memo.inputs == inputs)
}

fn coordinate<'a>(
    prefix: &[LinearOp],
    op: &'a LinearOp,
) -> Option<(&'a solve::SolvePureCallSite, Vec<Input>)> {
    let LinearOp::PureCall {
        input_starts, site, ..
    } = op
    else {
        return None;
    };
    let inputs = input_starts
        .iter()
        .zip(site.inputs())
        .map(|(&start, value)| source(prefix, start, value.scalar_count() as usize))
        .collect::<Option<Vec<_>>>()?;
    Some((site, inputs))
}

fn source(prefix: &[LinearOp], mut register: Reg, count: usize) -> Option<Input> {
    let mut end = prefix.len();
    loop {
        let position = prefix[..end].iter().rposition(|op| {
            let Some(start) = op.dst_register() else {
                return false;
            };
            u64::from(register) >= u64::from(start)
                && u64::from(register) < u64::from(start) + op.dst_register_count() as u64
        })?;
        match &prefix[position] {
            LinearOp::TensorLoad {
                dst_start,
                input: solve::TensorInputKind::P,
                input_start,
                count: width,
                seed_start: None,
                lanes: 1,
            } => {
                let offset = register.checked_sub(*dst_start)? as usize;
                if offset.checked_add(count)? > *width {
                    return None;
                }
                return Some(Input::Parameter {
                    start: input_start.checked_add(offset)?,
                    count,
                });
            }
            LinearOp::LoadP { index, .. } if count == 1 => {
                return Some(Input::Parameter {
                    start: *index,
                    count,
                });
            }
            LinearOp::Const { value, .. } if count == 1 => {
                return Some(Input::Constant(value.to_bits()));
            }
            LinearOp::LoadTime { .. } if count == 1 => return Some(Input::Time),
            LinearOp::Move { src, .. } if count == 1 => {
                register = *src;
                end = position;
            }
            _ => return None,
        }
    }
}
