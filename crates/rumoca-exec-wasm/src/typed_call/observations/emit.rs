//! Atomic initialized records, independent of ordinary result publication.
use super::super::{
    TypedCallFaultKind,
    emit::{CELL, Emitter},
};
use super::*;
use wasm_encoder::{BlockType, Instruction as I};

impl<'a> Emitter<'a> {
    pub(in crate::typed_call) fn reset_observations(&mut self) {
        if let Some(plan) = &self.plan.observations {
            self.store_integer(plan.range, 0);
        }
    }

    fn observation_plan(&self) -> Result<&'a ObservationPlan, TypedCallCompileError> {
        self.linked
            .unit_plan(self.unit)?
            .observations
            .as_ref()
            .ok_or(TypedCallCompileError::SiteMismatch)
    }

    pub(in crate::typed_call) fn assertion_operation(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::CheckAssertion {
            predicate_output,
            condition,
            captures,
            destinations,
            message,
            ..
        } = spanned.operation()
        else {
            unreachable!("checked assertion dispatch")
        };
        if self.linked.unit_plan(self.unit)?.observations.is_none() {
            if self.linked.assertion_mode == super::super::link::AssertionMode::Value {
                return self.value_assertion(index, spanned);
            }
            return self.assigned_read_check(index, spanned);
        }
        let tag = self
            .observation_plan()?
            .direct_source(&self.region_path, index)
            .ok_or_else(|| self.unsupported(index, "CheckAssertion", spanned.provenance()))?;
        self.address(self.reg(*condition));
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::If(BlockType::Empty));
        self.assertion_failed_payload(index, captures, destinations, message)?;
        self.append_assertion(index, tag, destinations, spanned.provenance())?;
        if self.owner.outputs()[*predicate_output].assertion_level()
            == Some(solve::SolveAssertionLevel::Error)
        {
            let status = self.fault(
                Some(index),
                "CheckAssertion",
                TypedCallFaultKind::AssertionFailed,
                spanned.provenance(),
            );
            self.push(I::I32Const(status as i32));
            self.push(I::Return);
        }
        self.push(I::Else);
        self.assertion_inactive_payload(destinations)?;
        self.push(I::End);
        Ok(())
    }

    fn append_assertion(
        &mut self,
        operation: usize,
        tag: usize,
        destinations: &[solve::SolveRegisterId],
        provenance: rumoca_core::Span,
    ) -> Result<(), TypedCallCompileError> {
        self.begin_record(operation, provenance)?;
        self.record_integer(0, tag as i64);
        let mut offset = 8;
        for counter in self.loop_counters.clone() {
            self.record_copy(offset, counter);
            offset += 8;
        }
        for destination in destinations {
            let source = self.reg(*destination);
            self.record_copy(offset, source);
            offset += source.bytes;
        }
        self.commit_record()?;
        Ok(())
    }

    fn begin_record(
        &mut self,
        operation: usize,
        provenance: rumoca_core::Span,
    ) -> Result<(), TypedCallCompileError> {
        let plan = self.observation_plan()?;
        let status = self.fault(
            Some(operation),
            "ObservationCapacity",
            TypedCallFaultKind::ObservationCapacity,
            provenance,
        );
        self.address(plan.range);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(i64::from(plan.capacity)));
        self.push(I::I64GeU);
        self.fail_if(status);
        self.address(plan.range);
        self.push(I::I32Const(8));
        self.push(I::I32Add);
        self.address(plan.range);
        self.push(I::I64Load(CELL));
        self.push(I::I32WrapI64);
        self.push(I::I32Const(plan.stride as i32));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        self.push(I::LocalSet(10));
        Ok(())
    }

    fn record_integer(&mut self, offset: u32, value: i64) {
        self.record_address(offset);
        self.push(I::I64Const(value));
        self.push(I::I64Store(CELL));
    }

    fn record_address(&mut self, offset: u32) {
        self.push(I::LocalGet(10));
        self.push(I::I32Const(offset as i32));
        self.push(I::I32Add);
    }

    fn record_copy(&mut self, offset: u32, source: CellRange) {
        self.record_address(offset);
        self.address(source);
        self.push(I::I32Const(source.bytes as i32));
        self.push(I::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }

    fn commit_record(&mut self) -> Result<(), TypedCallCompileError> {
        self.increment(self.observation_plan()?.range, 1);
        Ok(())
    }

    pub(in crate::typed_call) fn forward_observations(
        &mut self,
        operation: usize,
        child: usize,
        input: CellRange,
        scratch: CellRange,
        provenance: rumoca_core::Span,
    ) -> Result<(), TypedCallCompileError> {
        let Some(child_plan) = &self.linked.unit_plan(child)?.observations else {
            return Ok(());
        };
        let parent = self.observation_plan()?;
        let header = CellRange {
            offset: scratch.offset + child_plan.range.offset,
            ..scratch
        };
        let mappings = parent
            .forwarding
            .iter()
            .filter(|mapping| mapping.regions == self.region_path && mapping.operation == operation)
            .cloned()
            .collect::<Vec<_>>();
        if mappings.len() != child_plan.sources.len() {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let integrity = self.fault(
            Some(operation),
            "ObservationForwarding",
            TypedCallFaultKind::ObservationIntegrity,
            provenance,
        );
        self.begin_forward_loop(header, child_plan, integrity);
        for mapping in &mappings {
            self.forward_record(operation, mapping, child_plan, input, provenance)?;
        }
        self.push(I::LocalGet(11));
        self.push(I::I32Const(1));
        self.push(I::I32Add);
        self.push(I::LocalSet(11));
        self.push(I::Br(0));
        self.push(I::End);
        self.push(I::End);
        Ok(())
    }

    fn begin_forward_loop(
        &mut self,
        header: CellRange,
        child_plan: &ObservationPlan,
        integrity: u32,
    ) {
        self.address(header);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(i64::from(child_plan.capacity)));
        self.push(I::I64GtU);
        self.fail_if(integrity);
        self.push(I::I32Const(0));
        self.push(I::LocalSet(11));
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        self.push(I::LocalGet(11));
        self.push(I::I64ExtendI32U);
        self.address(header);
        self.push(I::I64Load(CELL));
        self.push(I::I64GeU);
        self.push(I::BrIf(1));
        self.address(header);
        self.push(I::I32Const(8));
        self.push(I::I32Add);
        self.push(I::LocalGet(11));
        self.push(I::I32Const(child_plan.stride as i32));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        self.push(I::LocalSet(12));
        self.push(I::LocalGet(12));
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(child_plan.sources.len() as i64));
        self.push(I::I64GeU);
        self.fail_if(integrity);
    }

    fn forward_record(
        &mut self,
        operation: usize,
        mapping: &ObservationForwarding,
        child_plan: &ObservationPlan,
        input: CellRange,
        provenance: rumoca_core::Span,
    ) -> Result<(), TypedCallCompileError> {
        self.push(I::LocalGet(12));
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(mapping.child_tag as i64));
        self.push(I::I64Eq);
        self.push(I::If(BlockType::Empty));
        self.begin_record(operation, provenance)?;
        self.record_integer(0, mapping.parent_tag as i64);
        let mut offset = 8;
        for counter in self.loop_counters.clone() {
            self.record_copy(offset, counter);
            offset += 8;
        }
        self.record_copy(offset, input);
        offset += input.bytes;
        debug_assert_eq!(offset, 8 + mapping.prefix_bytes);
        self.record_address(offset);
        self.push(I::LocalGet(12));
        self.push(I::I32Const(8));
        self.push(I::I32Add);
        self.push(I::I32Const(
            child_plan.sources[mapping.child_tag].payload_bytes as i32,
        ));
        self.push(I::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        self.commit_record()?;
        self.push(I::End);
        Ok(())
    }
}
