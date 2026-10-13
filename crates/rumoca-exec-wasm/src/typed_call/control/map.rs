//! Ordered Map tuples pack one complete result into fresh private storage.
use super::*;

impl<'a> Emitter<'a> {
    pub(crate) fn map_operation(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::Map {
            domain,
            captures,
            destination,
            body,
        } = spanned.operation()
        else {
            unreachable!("checked map dispatch")
        };
        let child = &self.plan.regions[index][0];
        let inputs = interface(body.body(), child, solve::SolveStorageClass::Input);
        let outputs = interface(body.body(), child, solve::SolveStorageClass::Output);
        let [output] = outputs.as_slice() else {
            return Err(TypedCallCompileError::SizeLimit);
        };
        let target = self.reg(*destination);
        let count = domain
            .scalar_count()
            .map_err(|_| TypedCallCompileError::SizeLimit)?;
        let count = u64::try_from(count).map_err(|_| TypedCallCompileError::SizeLimit)?;
        if count.checked_mul(u64::from(output.bytes)) != Some(u64::from(target.bytes))
            || inputs.len() != captures.len() + domain.binders.len()
        {
            return Err(TypedCallCompileError::SizeLimit);
        }
        let sources = captures.iter().map(|r| self.reg(*r)).collect::<Vec<_>>();
        self.copy_pairs(&inputs[..captures.len()], &sources);
        let binders = &inputs[captures.len()..];
        if !Self::binders_in_locals(domain, false) {
            self.store_first_binders(binders, domain);
        }
        let known = self.known_inputs(0, captures, domain.binders.len());
        self.map_loop(index, (body, known), domain, (target, *output, binders))
    }

    fn map_loop(
        &mut self,
        index: usize,
        (region, known): (&'a solve::SolveProgramRegion, Facts),
        domain: &StructuredIndexDomain,
        (target, output, binders): (CellRange, CellRange, &[CellRange]),
    ) -> Result<(), TypedCallCompileError> {
        let plan = &self.plan.regions[index][0];
        let Some((count, last)) = loops::passes(domain)? else {
            return Ok(());
        };
        if let Some(count) = self.program.unrolled_passes(index, self.observed) {
            let site = unroll::UnrolledLoop {
                index,
                region,
                plan,
                domain,
                count,
                known,
            };
            return self.unrolled_map(site, (target, output));
        }
        let pass_loop = self.open_passes(domain, Self::binders_in_locals(domain, false));
        let counter = pass_loop.counter;
        let mut pass = RegionLoop::new(counter, region.body(), domain);
        pass.binder_locals = loops::binder_locals(region.body(), pass_loop, domain);
        pass.output = region
            .body()
            .slots()
            .iter()
            .find(|slot| slot.storage() == solve::SolveStorageClass::Output)
            .map(|slot| MapOutput {
                slot: slot.id().index(),
                target,
                bytes: output.bytes,
                written: false,
            });
        let pass_output = pass.output.as_ref().map(|output| output.slot);
        self.pending_loop = Some(pass);
        self.pending_known = Some(known);
        self.region_body(index, 0, region, plan)?;
        let written = std::mem::take(&mut self.written_outputs);
        if !pass_output.is_some_and(|slot| written.contains(&slot)) {
            // Checked count × body bytes equals the complete destination
            // extent; a pass index below the count keeps this tuple copy
            // within it.
            self.address(target);
            self.push(I::LocalGet(counter));
            self.push(I::I64Const(i64::from(output.bytes)));
            self.push(I::I64Mul);
            self.push(I::I32WrapI64);
            self.push(I::I32Add);
            self.address(output);
            self.copy_addresses(output.bytes);
        }
        self.next_pass(pass_loop, count, (binders, domain, &last));
        self.close_passes(pass_loop);
        Ok(())
    }
}
