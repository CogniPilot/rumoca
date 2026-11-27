//! Emit a partial scalar entry only when a verified scalar program uses it.
//! All callers keep the complete entry as a fallback, including generated
//! event, projection and directional rows absent from the source inventory.
use super::{CodegenError, Json, PureCallFamilies, owner_json, route_calls};
use rumoca_ir_solve::{self as solve, SolveVisitor};
use std::collections::BTreeSet;
use std::convert::Infallible;

pub(crate) fn demand_suffix(reads: &[bool]) -> String {
    let mut name = String::from("_reads");
    for (index, &read) in reads.iter().enumerate() {
        if read {
            name.push('_');
            name.push_str(&index.to_string());
        }
    }
    name
}

#[derive(Default)]
struct Demands(BTreeSet<(usize, bool, Vec<bool>)>);

impl SolveVisitor for Demands {
    type Error = Infallible;

    fn enter_linear_op_slice(
        &mut self,
        _: solve::LinearOpSliceKind,
        ops: &[solve::LinearOp],
    ) -> Result<(), Infallible> {
        for demand in solve::ScalarCallDemand::in_program(ops).into_values() {
            self.0.insert((
                demand.owner.index() as usize,
                demand.directional,
                demand.reads,
            ));
        }
        Ok(())
    }
}

impl PureCallFamilies {
    pub(in crate::codegen) fn for_model(
        table: &solve::SolvePureCallTable,
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
    ) -> Result<Self, CodegenError> {
        let mut families = Self::new(table)?;
        let mut demands = Demands::default();
        let Ok(()) = demands.visit_solve_problem(problem);
        let Ok(()) = demands.visit_solve_artifacts(artifacts);
        for (id, tangent, reads) in demands.0 {
            let id = *families
                .symbols
                .get(id)
                .ok_or_else(|| CodegenError::template("partial call has no family"))?
                as usize;
            let leaf = format!("{id}{}", demand_suffix(&reads));
            let name = format!(
                "rumoca_scalar_pure_{}{leaf}",
                if tangent { "directional_" } else { "" }
            );
            if families.variants.contains(&name) {
                continue;
            }
            let owner = &table.owners()[id];
            let mut view = owner_json(id, owner)?;
            let (body, json, target) = if tangent {
                let (Some(body), Some(json)) = (owner.directional(), view.directional.take())
                else {
                    continue;
                };
                (body.body(), json, &mut families.directional)
            } else {
                (owner.body(), view.primal, &mut families.owners)
            };
            let liveness = solve::SolveLiveness::of(
                body,
                &solve::SolveOutputDemand::of(reads.iter().copied()),
            );
            // Conditional arm capture pruning needs matching region ABIs. Keep
            // the complete entry when a discarded capture requires that step.
            if !liveness.dead().contains(&true)
                || (0..body.operations().len()).any(|i| !liveness.unused_captures(i).is_empty())
            {
                continue;
            }
            let mut json = route_calls(json, &families.symbols)?;
            json["symbol"] = Json::String(leaf);
            omit_dead(&mut json, &liveness, &reads)?;
            std::sync::Arc::make_mut(target).push(json);
            families.variants.insert(name);
        }
        Ok(families)
    }
}

fn omit_dead(
    owner: &mut Json,
    live: &solve::SolveLiveness,
    reads: &[bool],
) -> Result<(), CodegenError> {
    let body = &mut owner["body"];
    for (i, operation) in body["operations"]
        .as_array_mut()
        .ok_or_else(|| CodegenError::template("expected typed function array"))?
        .iter_mut()
        .enumerate()
    {
        if live.is_dead(i) {
            operation["omitted"] = Json::Bool(true);
        }
    }
    for (i, slot) in body["slots"]
        .as_array_mut()
        .ok_or_else(|| CodegenError::template("expected typed function slots"))?
        .iter_mut()
        .enumerate()
    {
        if live.drops(i) {
            slot["omitted"] = Json::Bool(true);
        }
    }
    for (result, read) in owner["outputs"]
        .as_array_mut()
        .ok_or_else(|| CodegenError::template("expected typed function results"))?
        .iter_mut()
        .zip(reads)
    {
        if !read {
            result["omitted"] = Json::Bool(true);
        }
    }
    Ok(())
}
