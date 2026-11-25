//! Pull back through a typed call using its construction-issued directional body.
//! A single basis seed at a time keeps the local Jacobian implicit. The outer
//! program still visits each call site once on the reverse sweep.
use super::{EvalSolveError, Reg, add_adj, unsupported};
use crate::{RowEvalContext, eval_pure_call_directional_payload, eval_pure_call_payload};
use rumoca_ir_solve::{SolvePureCallSite, SolveScalarType};

pub(super) fn admitted(dst: Reg, starts: &[Reg], site: &SolvePureCallSite) -> bool {
    let Some(count) = site.output_scalar_count() else {
        return false;
    };
    site.directional().is_some()
        && starts.len() == site.inputs().len()
        && starts.iter().zip(site.inputs()).all(|(&start, ty)| {
            u64::from(start) + u64::from(ty.scalar_count()) <= u64::from(dst)
                || u64::from(dst) + count as u64 <= u64::from(start)
        })
}

pub(super) fn forward(
    dst: Reg,
    starts: &[Reg],
    site: &SolvePureCallSite,
    context: RowEvalContext<'_>,
    regs: &mut [f64],
) -> Result<(), EvalSolveError> {
    if !admitted(dst, starts, site) {
        return Err(unsupported(
            "reverse pure call requires disjoint results and a directional owner",
        ));
    }
    let values = eval_pure_call_payload(context, site, starts, |r| Ok(regs[r as usize]))?;
    regs[dst as usize..dst as usize + values.len()].copy_from_slice(&values);
    Ok(())
}

struct Arguments {
    values: Vec<f64>,
    starts: Vec<Reg>,
    seeds: Vec<(usize, Reg)>,
}

impl Arguments {
    fn pack(
        site: &SolvePureCallSite,
        starts: &[Reg],
        regs: &[f64],
    ) -> Result<Self, EvalSolveError> {
        let mut args = Self {
            values: vec![],
            starts: vec![],
            seeds: vec![],
        };
        for (&start, ty) in starts.iter().zip(site.inputs()) {
            let count = ty.scalar_count() as usize;
            args.begin_argument()?;
            args.values
                .extend_from_slice(&regs[start as usize..start as usize + count]);
            if !matches!(ty.element_type(), SolveScalarType::Real { .. }) {
                continue;
            }
            args.begin_argument()?;
            for i in 0..count {
                args.seeds.push((args.values.len(), start + i as Reg));
                args.values.push(0.0);
            }
        }
        Ok(args)
    }

    fn begin_argument(&mut self) -> Result<(), EvalSolveError> {
        self.starts.push(
            Reg::try_from(self.values.len())
                .map_err(|_| unsupported("directional call arguments exceed register range"))?,
        );
        Ok(())
    }
}

pub(super) fn reverse(
    dst: Reg,
    starts: &[Reg],
    site: &SolvePureCallSite,
    context: RowEvalContext<'_>,
    regs: &[f64],
    adj: &mut [f64],
    untaken: &mut [bool],
) -> Result<(), EvalSolveError> {
    let directional = site
        .directional()
        .ok_or_else(|| unsupported("missing directional call owner"))?;
    let mut weights = Vec::new();
    let mut primal = dst as usize;
    let mut dual = 0;
    let mut unselected = false;
    for out in site.outputs() {
        let width = out.value_type().scalar_count() as usize;
        dual += width;
        for i in 0..width {
            let weight = std::mem::take(&mut adj[primal + i]);
            unselected |= std::mem::take(&mut untaken[primal + i]);
            if out.carries_tangent() && weight != 0.0 {
                weights.push((dual + i, weight));
            }
        }
        primal += width;
        if out.carries_tangent() {
            dual += width;
        }
    }
    let mut args = Arguments::pack(site, starts, regs)?;
    for (slot, r) in args.seeds {
        if weights.is_empty() {
            if unselected && adj[r as usize] == 0.0 {
                untaken[r as usize] = true;
            }
            continue;
        }
        args.values[slot] = 1.0;
        let column =
            eval_pure_call_directional_payload(context, directional, &args.starts, |index| {
                Ok(args.values[index as usize])
            })?;
        args.values[slot] = 0.0;
        let contribution = weights.iter().map(|&(i, w)| w * column[i]).sum();
        add_adj(adj, r, contribution);
    }
    Ok(())
}
