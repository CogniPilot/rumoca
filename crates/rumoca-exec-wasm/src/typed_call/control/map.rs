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
        for (slot, binder) in binders.iter().zip(&domain.binders) {
            self.store_integer(*slot, binder.lower);
        }
        self.store_integer(child.counter, 0);
        self.map_loop(index, body, domain, target, *output, binders)
    }

    fn map_loop(
        &mut self,
        index: usize,
        region: &'a solve::SolveProgramRegion,
        domain: &StructuredIndexDomain,
        target: CellRange,
        output: CellRange,
        binders: &[CellRange],
    ) -> Result<(), TypedCallCompileError> {
        let plan = &self.plan.regions[index][0];
        let count = domain
            .scalar_count()
            .map_err(|_| TypedCallCompileError::SizeLimit)?;
        let last = if count == 0 {
            Vec::new()
        } else {
            domain
                .index_tuple_at(count - 1)
                .map_err(|_| TypedCallCompileError::SizeLimit)?
                .ok_or(TypedCallCompileError::SizeLimit)?
        };
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        self.address(plan.counter);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(count as i64));
        self.push(I::I64GeU);
        self.push(I::BrIf(1));
        self.region_body(index, 0, region, plan)?;
        // Checked count × body bytes equals the complete destination extent.
        // Counter < count proves this exact tuple copy stays within it.
        self.address(target);
        self.address(plan.counter);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(i64::from(output.bytes)));
        self.push(I::I64Mul);
        self.push(I::I32WrapI64);
        self.push(I::I32Add);
        self.address(output);
        self.copy_addresses(output.bytes);
        self.increment(plan.counter, 1);
        self.address(plan.counter);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(count as i64));
        self.push(I::I64GeU);
        self.push(I::BrIf(1));
        self.advance_binders(binders, domain, &last);
        self.push(I::Br(0));
        self.push(I::End);
        self.push(I::End);
        Ok(())
    }
}
