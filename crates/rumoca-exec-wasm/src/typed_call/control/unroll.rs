//! Loops with a small constant trip count and a small body are emitted as
//! straight-line passes. Each pass knows its binder as a constant, so the
//! index arithmetic and element addresses of its body fold at emission, and no
//! counter, test or branch remains.
use super::super::known::Facts;
use super::loops::{direct_outputs, slot_ids};
use super::*;

/// The binder value of pass `pass` of a one-binder domain.
fn binder_value(domain: &StructuredIndexDomain, pass: usize) -> Result<i64, TypedCallCompileError> {
    domain
        .index_tuple_at(pass)
        .map_err(|_| TypedCallCompileError::SizeLimit)?
        .and_then(|tuple| tuple.first().copied())
        .ok_or(TypedCallCompileError::SizeLimit)
}

/// The region of a loop and what its passes share.
pub(super) struct UnrolledLoop<'a, 'd> {
    pub index: usize,
    pub region: &'a solve::SolveProgramRegion,
    pub plan: &'a FramePlan,
    pub domain: &'d StructuredIndexDomain,
    pub count: usize,
    /// Known values of the input slots of the region.
    pub known: Facts,
}

impl<'a> Emitter<'a> {
    pub(super) fn unrolled_fold(
        &mut self,
        site: UnrolledLoop<'a, '_>,
        carried: &[CellRange],
        inputs: &[CellRange],
    ) -> Result<(), TypedCallCompileError> {
        for pass in 0..site.count {
            self.copy_pairs(&inputs[..carried.len()], carried);
            let mut state = RegionLoop::new(0, site.region.body(), site.domain);
            state.unrolled = Some((pass, binder_value(site.domain, pass)?));
            state.direct = direct_outputs(site.region.body(), site.plan, carried);
            self.pending_loop = Some(state);
            self.pending_known = Some(site.known.clone());
            self.region_body(site.index, 0, site.region, site.plan)?;
            self.copy_unwritten_results(site.region, site.plan, carried);
        }
        Ok(())
    }

    /// Copy each result of the pass just emitted that its body did not store
    /// straight into the carried cell.
    pub(super) fn copy_unwritten_results(
        &mut self,
        region: &solve::SolveProgramRegion,
        plan: &FramePlan,
        carried: &[CellRange],
    ) {
        let written = std::mem::take(&mut self.written_outputs);
        let outputs = interface(region.body(), plan, solve::SolveStorageClass::Output);
        let slots = slot_ids(region.body(), solve::SolveStorageClass::Output);
        let pending = carried
            .iter()
            .zip(outputs)
            .zip(slots)
            .filter(|(_, slot)| !written.contains(slot))
            .map(|((carried, output), _)| (*carried, output))
            .collect::<Vec<_>>();
        for (carried, output) in pending {
            self.copy(carried, output);
        }
    }

    pub(super) fn unrolled_map(
        &mut self,
        site: UnrolledLoop<'a, '_>,
        (target, output): (CellRange, CellRange),
    ) -> Result<(), TypedCallCompileError> {
        let slot = site
            .region
            .body()
            .slots()
            .iter()
            .find(|slot| slot.storage() == solve::SolveStorageClass::Output)
            .map(|slot| slot.id().index());
        for pass in 0..site.count {
            let mut state = RegionLoop::new(0, site.region.body(), site.domain);
            state.unrolled = Some((pass, binder_value(site.domain, pass)?));
            state.output = slot.map(|slot| MapOutput {
                slot,
                target,
                bytes: output.bytes,
                written: false,
            });
            self.pending_loop = Some(state);
            self.pending_known = Some(site.known.clone());
            self.region_body(site.index, 0, site.region, site.plan)?;
            let written = std::mem::take(&mut self.written_outputs);
            if !slot.is_some_and(|slot| written.contains(&slot)) {
                let element = CellRange {
                    offset: target.offset + pass as u32 * output.bytes,
                    bytes: output.bytes,
                    ..target
                };
                self.copy(element, output);
            }
        }
        Ok(())
    }
}
