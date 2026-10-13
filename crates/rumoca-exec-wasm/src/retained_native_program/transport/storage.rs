//! The single private buffer owner across preparation and resolved execution.

use super::{
    NativeTransportError, NativeTransportSnapshot, NativeTypedInput, NativeTypedInputBatch,
    presence::InputPresence,
};
use crate::retained_native_program::NativeCode;
use rumoca_ir_solve::{
    NativeOutputLane, ScalarSlot, SolveVariableStorageRole, SolveVariableValueKind,
};
use std::rc::Rc;

pub(in crate::retained_native_program) struct NativeStorage {
    pub(super) code: Rc<NativeCode>,
    instance_brand: Rc<()>,
    original_source: Option<Rc<NativeTransportSnapshot>>,
    pub(in crate::retained_native_program) y: Vec<f64>,
    #[cfg(any(
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32")),
        all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    ))]
    pub(in crate::retained_native_program) work_y: Vec<f64>,
    pub(in crate::retained_native_program) p: Vec<f64>,
    pub(in crate::retained_native_program) scratch: Vec<u64>,
    pub(in crate::retained_native_program) lanes: Vec<u64>,
    #[cfg(any(
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32")),
        all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    ))]
    pub(in crate::retained_native_program) work_lanes: Vec<u64>,
    pub(super) assigned: InputPresence,
    pub(in crate::retained_native_program) outputs_present: bool,
}

impl NativeStorage {
    pub(super) fn new(
        code: Rc<NativeCode>,
        y: &[f64],
        p: &[f64],
    ) -> Result<Self, NativeTransportError> {
        let layout = &code.inputs.root().model().problem.layout;
        if y.len() != layout.y_scalars() || p.len() != layout.p_scalars() {
            return Err(NativeTransportError::BufferShape);
        }
        let schedule = code.inputs.schedule();
        let bytes = schedule
            .input_lane_bytes()
            .checked_add(schedule.lane_bytes())
            .ok_or(NativeTransportError::BufferShape)?;
        Ok(Self {
            instance_brand: Rc::new(()),
            original_source: None,
            y: try_copy(y)?,
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32")),
                all(
                    feature = "fixed-native-guest-receiver",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                )
            ))]
            work_y: try_copy(y)?,
            p: try_copy(p)?,
            scratch: try_filled((code.compiled.scratch_bytes() as usize).div_ceil(8), 0)?,
            lanes: try_filled(bytes.div_ceil(8), 0)?,
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32")),
                all(
                    feature = "fixed-native-guest-receiver",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                )
            ))]
            work_lanes: try_filled(bytes.div_ceil(8), 0)?,
            assigned: InputPresence::default(),
            outputs_present: false,
            code,
        })
    }

    /// Only the no-coordinate source constructor may retain original defaults.
    pub(super) fn from_original_source(code: Rc<NativeCode>) -> Result<Self, NativeTransportError> {
        let root = code.inputs.root();
        if root.resolved_start_time().is_some() {
            return Err(NativeTransportError::SourcePristineUnavailable);
        }
        let model = root.model();
        let mut storage = Self::new(Rc::clone(&code), &model.initial_y, &model.parameters)?;
        storage.initialize_from_source()?;
        storage.original_source = Some(Rc::new(storage.snapshot()?));
        Ok(storage)
    }

    pub(super) fn restore_original_source(&mut self) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        let original = Rc::clone(
            self.original_source
                .as_ref()
                .ok_or(NativeTransportError::SourcePristineUnavailable)?,
        );
        self.restore(&original, None)
    }

    /// Exact input bits paired with the original descriptor; unwritten cells refuse.
    pub(super) fn input_values(
        &self,
    ) -> impl Iterator<
        Item = (
            rumoca_ir_solve::NativeInputLane,
            Result<rumoca_ir_solve::SolveValueKind, NativeTransportError>,
        ),
    > {
        self.code
            .inputs
            .schedule()
            .scalar_input_lanes()
            .map(|input| {
                let value = if self.code.entered.get() {
                    Err(NativeTransportError::Busy)
                } else if self
                    .assigned
                    .first_missing(input.p_index()..input.p_index() + 1)
                    .is_some()
                {
                    Err(NativeTransportError::MissingInput {
                        p_index: input.p_index(),
                    })
                } else {
                    Ok(read_cell(&self.lanes, input.lane_offset(), input.lane()))
                };
                (input, value)
            })
    }

    /// Update actual caller coordinates without inferring typed inputs from P.
    pub(super) fn set_coordinates(
        &mut self,
        y: &[f64],
        p: &[f64],
    ) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        if y.len() != self.y.len() || p.len() != self.p.len() {
            return Err(NativeTransportError::BufferShape);
        }
        self.y.clone_from_slice(y);
        self.p.clone_from_slice(p);
        Ok(())
    }

    pub(super) fn snapshot(&self) -> Result<NativeTransportSnapshot, NativeTransportError> {
        Ok(NativeTransportSnapshot {
            resolved_product: None,
            instance_brand: Rc::clone(&self.instance_brand),
            y: Rc::new(try_copy(&self.y)?),
            p: Rc::new(try_copy(&self.p)?),
            lanes: Rc::new(try_copy(&self.lanes)?),
            assigned: Rc::new(self.assigned.try_clone()?),
            outputs_present: self.outputs_present,
        })
    }

    pub(super) fn restore(
        &mut self,
        snapshot: &NativeTransportSnapshot,
        product: Option<&Rc<crate::CompiledRetainedNativeProgramWasm>>,
    ) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        if !Rc::ptr_eq(&self.instance_brand, &snapshot.instance_brand) {
            return Err(NativeTransportError::ForeignSnapshot);
        }
        if let Some(original) = &snapshot.resolved_product
            && !product.is_some_and(|actual| Rc::ptr_eq(original, actual))
        {
            return Err(NativeTransportError::SnapshotProductUnsupported);
        }
        let assigned = snapshot.assigned.try_clone()?;
        self.y.clone_from_slice(&snapshot.y);
        self.p.clone_from_slice(&snapshot.p);
        self.lanes.clone_from_slice(&snapshot.lanes);
        self.assigned = assigned;
        self.outputs_present = snapshot.outputs_present;
        Ok(())
    }

    /// Store bits only through the original constructor-issued input descriptor.
    pub(super) fn set_typed_input(
        &mut self,
        p_index: usize,
        value: NativeTypedInput,
    ) -> Result<(), NativeTransportError> {
        match value {
            NativeTypedInput::Integer(value) => self.set_typed_inputs(
                p_index,
                NativeTypedInputBatch::Integer(std::slice::from_ref(&value)),
            ),
            NativeTypedInput::Boolean(value) => self.set_typed_inputs(
                p_index,
                NativeTypedInputBatch::Boolean(std::slice::from_ref(&value)),
            ),
        }
    }

    pub(super) fn check_float64_range(
        &self,
        p_index: usize,
        count: usize,
    ) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        let end = p_index
            .checked_add(count)
            .ok_or(NativeTransportError::BufferShape)?;
        if end > self.p.len() {
            return Err(NativeTransportError::BufferShape);
        }
        if count == 0 {
            return Ok(());
        }
        for run in &self
            .code
            .inputs
            .root()
            .model()
            .problem
            .solve_layout
            .variable_storage_runs
        {
            let ScalarSlot::P { index, .. } = run.base else {
                continue;
            };
            let limit = index
                .checked_add(run.scalar_count)
                .ok_or(NativeTransportError::BufferShape)?;
            if !(index..limit).contains(&p_index) {
                continue;
            }
            if !matches!(
                run.role,
                SolveVariableStorageRole::ExternalInput
                    | SolveVariableStorageRole::Parameter
                    | SolveVariableStorageRole::Constant
            ) {
                return Err(NativeTransportError::UnknownInput { p_index });
            }
            if run.value_kind != SolveVariableValueKind::Real {
                return Err(NativeTransportError::InputType { p_index });
            }
            if end > limit {
                return Err(NativeTransportError::BufferShape);
            }
            return Ok(());
        }
        Err(NativeTransportError::UnknownInput { p_index })
    }

    pub(super) fn check_float64_inputs(
        &self,
        p_index: usize,
        count: usize,
    ) -> Result<(), NativeTransportError> {
        self.check_float64_range(p_index, count)?;
        if let Some(p_index) = self.assigned.first_missing(p_index..p_index + count) {
            return Err(NativeTransportError::MissingInput { p_index });
        }
        Ok(())
    }

    pub(super) fn check_float64_input_batches(
        &self,
        batches: &[(usize, &[f64])],
    ) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        for &(base, values) in batches {
            self.check_float64_range(base, values.len())?;
            if let Some(index) = values.iter().position(|value| !value.is_finite()) {
                return Err(NativeTransportError::InvalidInput {
                    p_index: base + index,
                });
            }
        }
        Ok(())
    }

    pub(super) fn set_float64_input_batches(
        &mut self,
        batches: &[(usize, &[f64])],
    ) -> Result<(), NativeTransportError> {
        self.check_float64_input_batches(batches)?;
        self.assigned.reserve_insertions(batches.len())?;
        for &(base, values) in batches {
            let end = base + values.len();
            self.p[base..end].copy_from_slice(values);
            self.assigned.insert(base..end);
        }
        Ok(())
    }

    /// Validate every original descriptor before writing any actual input bits.
    pub(super) fn check_typed_inputs(
        &self,
        p_index: usize,
        values: NativeTypedInputBatch<'_>,
    ) -> Result<(), NativeTransportError> {
        self.batch_inputs(p_index, values).map(|_| ())
    }

    pub(super) fn set_typed_inputs(
        &mut self,
        p_index: usize,
        values: NativeTypedInputBatch<'_>,
    ) -> Result<(), NativeTransportError> {
        self.set_typed_input_batches(&[(p_index, values)])
    }

    /// An FMI request may carry several array VRs. Check all of them before
    /// the first write; duplicate caller references retain request order.
    pub(super) fn set_typed_input_batches(
        &mut self,
        batches: &[(usize, NativeTypedInputBatch<'_>)],
    ) -> Result<(), NativeTransportError> {
        for &(p_index, values) in batches {
            self.batch_inputs(p_index, values)?;
        }
        self.assigned.reserve_insertions(batches.len())?;
        for &(p_index, values) in batches {
            self.write_batch(p_index, values);
        }
        Ok(())
    }

    pub(super) fn write_batch(&mut self, p_index: usize, values: NativeTypedInputBatch<'_>) {
        // Validate original run coverage before entering this value-write boundary.
        let end = p_index + values.len();
        for run in self.code.inputs.schedule().input_runs() {
            let storage = run.p_range();
            let first = storage.start.max(p_index);
            let limit = storage.end.min(end);
            for cell in first..limit {
                let offset = run.lane_offset() + (cell - storage.start) * run.lane().width();
                write_input(&mut self.lanes, offset, values.value(cell - p_index));
            }
        }
        self.assigned.insert(p_index..end);
    }

    /// A parameter or constant is fixed before execution, so a value the program's
    /// Real view would round is refused here instead of at run time.
    fn check_parameter_exactness(
        &self,
        run: &rumoca_ir_solve::NativeInputRun,
        cells: std::ops::Range<usize>,
        p_index: usize,
        values: NativeTypedInputBatch<'_>,
    ) -> Result<(), NativeTransportError> {
        if run.role() == SolveVariableStorageRole::ExternalInput {
            return Ok(());
        }
        let schedule = self.code.inputs.schedule();
        for cell in cells {
            let NativeTypedInput::Integer(value) = values.value(cell - p_index) else {
                continue;
            };
            let view_exact = value.unsigned_abs() <= 1 << 53;
            if !view_exact && schedule.input_lane(cell).is_some_and(|lane| lane.checked()) {
                return Err(NativeTransportError::InexactParameter { p_index: cell });
            }
        }
        Ok(())
    }

    fn batch_inputs(
        &self,
        p_index: usize,
        values: NativeTypedInputBatch<'_>,
    ) -> Result<(), NativeTransportError> {
        if self.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        let end = p_index
            .checked_add(values.len())
            .ok_or(NativeTransportError::BufferShape)?;
        let mut cursor = p_index;
        let runs = self.code.inputs.schedule().input_runs();
        let first = runs.partition_point(|run| run.p_range().end <= cursor);
        for run in &runs[first..] {
            if cursor == end {
                return Ok(());
            }
            let storage = run.p_range();
            if cursor < storage.start {
                return Err(NativeTransportError::UnknownInput { p_index: cursor });
            }
            if run.lane() != values.lane() {
                return Err(NativeTransportError::InputType { p_index: cursor });
            }
            self.check_parameter_exactness(run, cursor..storage.end.min(end), p_index, values)?;
            cursor = storage.end.min(end);
        }
        if cursor == end {
            Ok(())
        } else {
            Err(NativeTransportError::UnknownInput { p_index: cursor })
        }
    }
}

fn write_input(lanes: &mut [u64], offset: usize, value: NativeTypedInput) {
    match value {
        NativeTypedInput::Integer(value) => {
            lanes[offset / 8] = u64::from_le_bytes(value.to_le_bytes());
        }
        NativeTypedInput::Boolean(value) => {
            let shift = (offset % 8) * 8;
            let word = &mut lanes[offset / 8];
            *word = (*word & !(0xff << shift)) | (u64::from(value) << shift);
        }
    }
}

pub(super) fn read_cell(
    lanes: &[u64],
    offset: usize,
    lane: NativeOutputLane,
) -> rumoca_ir_solve::SolveValueKind {
    super::super::cells::decode(super::super::cells::bits(lanes, offset, lane), lane)
        .expect("private setters and checked outcomes retain valid Boolean cells")
}

pub(super) fn try_copy<T: Clone>(source: &[T]) -> Result<Vec<T>, NativeTransportError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(source.len())
        .map_err(|_| NativeTransportError::AllocationRefused)?;
    values.extend_from_slice(source);
    Ok(values)
}

fn try_filled<T: Clone>(count: usize, value: T) -> Result<Vec<T>, NativeTransportError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(count)
        .map_err(|_| NativeTransportError::AllocationRefused)?;
    values.resize(count, value);
    Ok(values)
}

#[cfg(test)]
mod allocation_tests {
    use super::*;

    #[test]
    fn owned_native_buffers_refuse_capacity_overflow_and_preserve_source_bits() {
        assert_eq!(
            try_filled::<u64>(usize::MAX, 0),
            Err(NativeTransportError::AllocationRefused)
        );
        let source = [f64::from_bits(0x7ff8000000000042), -0.0];
        let copied = try_copy(&source).unwrap();
        assert_eq!(
            copied
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            source
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>()
        );
    }
}
