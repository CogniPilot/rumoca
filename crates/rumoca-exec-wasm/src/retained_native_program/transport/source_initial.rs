//! Instantiation authority borrowed from the retained product's original inventory.

use super::{
    NativeTransportError, NativeTypedInputBatch, PreparedNativeStorage, PreparedNativeWasmCode,
    RetainedNativeWasmHost, storage::NativeStorage,
};
use crate::retained_native_program::NativeCode;
use rumoca_ir_solve::fmi::{FmiMetadata, FmiStorageColumn, FmiVariable};
use rumoca_ir_solve::{
    SolveDeclarationStart, SolveExactInitialRun, SolveExactInitialValues, SolveInitialValueRun,
    SolveVariableStorageRole, SolveVariableValueKind,
};

impl RetainedNativeWasmHost {
    /// Initialize inputs from this product's checked source/default authority.
    pub fn transport_from_source(
        &self,
        y: &[f64],
        p: &[f64],
    ) -> Result<super::NativeTransport, NativeTransportError> {
        let mut transport = self.transport(y, p)?;
        transport.storage.initialize_from_source()?;
        Ok(transport)
    }
}

impl PreparedNativeWasmCode {
    /// Retain original defaults only from the actual unresolved source, before host writes.
    pub fn original_storage_from_source(
        &self,
    ) -> Result<PreparedNativeStorage, NativeTransportError> {
        Ok(PreparedNativeStorage {
            storage: NativeStorage::from_original_source(std::rc::Rc::clone(&self.code))?,
        })
    }

    /// Use the same original starts and complete batch preflight before activation.
    pub fn storage_from_source(
        &self,
        y: &[f64],
        p: &[f64],
    ) -> Result<PreparedNativeStorage, NativeTransportError> {
        let mut prepared = self.storage(y, p)?;
        prepared.storage.initialize_from_source()?;
        Ok(prepared)
    }
}

impl NativeStorage {
    pub(super) fn initialize_from_source(&mut self) -> Result<(), NativeTransportError> {
        let code = std::rc::Rc::clone(&self.code);
        let root = code.inputs.root();
        let metadata = root.metadata();
        let evaluated = super::storage::try_copy(&self.p)?;
        let mut insertions = 0usize;
        visit_batches(&code, &evaluated, |base, batch| {
            self.check_typed_inputs(base, batch)?;
            insertions = insertions
                .checked_add(1)
                .ok_or(NativeTransportError::AllocationRefused)?;
            Ok(())
        })?;
        visit_real_runs(metadata, |base, run| {
            self.check_float64_range(base, run.count())?;
            insertions = insertions
                .checked_add(1)
                .ok_or(NativeTransportError::AllocationRefused)?;
            Ok(())
        })?;
        let parameters = real_parameter_cells(metadata);
        insertions = insertions
            .checked_add(parameters.len())
            .ok_or(NativeTransportError::AllocationRefused)?;
        self.assigned.reserve_insertions(insertions)?;
        visit_batches(&code, &evaluated, |base, batch| {
            self.write_batch(base, batch);
            Ok(())
        })?;
        visit_real_runs(metadata, |base, run| {
            self.write_real_source_run(base, run);
            Ok(())
        })?;
        // Real parameters and constants keep the prepared model's evaluated P cells.
        for cells in parameters {
            self.assigned.insert(cells);
        }
        Ok(())
    }

    fn write_real_source_run(&mut self, base: usize, run: SolveInitialValueRun<'_>) {
        let end = base + run.count();
        if let Some(bits) = run.repeated_bits() {
            self.p[base..end].fill(f64::from_bits(bits));
        } else {
            for (destination, &bits) in self.p[base..end]
                .iter_mut()
                .zip(run.literal_bits().expect("issued literal run"))
            {
                *destination = f64::from_bits(bits);
            }
        }
        self.assigned.insert(base..end);
    }
}

/// P cell ranges of Real parameters and constants, whose values the prepared
/// model already holds in P.
fn real_parameter_cells(metadata: &FmiMetadata) -> Vec<std::ops::Range<usize>> {
    let mut cells = Vec::new();
    for variable in metadata.variables() {
        if !matches!(
            variable.role(),
            Some(SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant)
        ) || variable.value_kind() != SolveVariableValueKind::Real
        {
            continue;
        }
        let Some(storage) = variable
            .storage()
            .filter(|storage| storage.column() == FmiStorageColumn::P)
        else {
            continue;
        };
        cells.push(storage.base()..storage.base() + storage.scalar_count());
    }
    cells
}

fn visit_real_runs(
    metadata: &FmiMetadata,
    mut visit: impl FnMut(usize, SolveInitialValueRun<'_>) -> Result<(), NativeTransportError>,
) -> Result<(), NativeTransportError> {
    for variable in metadata.variables() {
        if variable.role() != Some(SolveVariableStorageRole::ExternalInput) {
            continue;
        }
        let SolveDeclarationStart::Real(values) = variable.instantiation_start() else {
            continue;
        };
        let storage = variable
            .storage()
            .ok_or(NativeTransportError::BufferShape)?;
        if storage.column() != FmiStorageColumn::P || storage.scalar_count() != values.len() {
            return Err(NativeTransportError::BufferShape);
        }
        for run in values.runs() {
            let base = storage
                .base()
                .checked_add(run.start())
                .ok_or(NativeTransportError::BufferShape)?;
            visit(base, run)?;
        }
    }
    Ok(())
}

/// Source authority of one typed lane variable: the exact declared start, or,
/// for a parameter or constant without one, its evaluated P cells.
enum LaneSource {
    Declared(SolveDeclarationStart),
    Evaluated(SolveVariableValueKind),
}

fn lane_source(variable: &FmiVariable) -> Option<LaneSource> {
    let source = variable.instantiation_start();
    let kind = variable.value_kind();
    if source.value_kind() == kind && !source.is_missing() {
        return matches!(
            source,
            SolveDeclarationStart::Integer(_) | SolveDeclarationStart::Boolean(_)
        )
        .then(|| LaneSource::Declared(source.clone()));
    }
    (variable.role() != Some(SolveVariableStorageRole::ExternalInput))
        .then_some(LaneSource::Evaluated(kind))
}

fn visit_batches(
    code: &NativeCode,
    p: &[f64],
    mut visit: impl FnMut(usize, NativeTypedInputBatch<'_>) -> Result<(), NativeTransportError>,
) -> Result<(), NativeTransportError> {
    let schedule = code.inputs.schedule();
    for variable in code.inputs.root().metadata().variables() {
        if !rumoca_ir_solve::fmi::reads_input_lanes(variable)
            || !matches!(
                variable.value_kind(),
                SolveVariableValueKind::Integer | SolveVariableValueKind::Boolean
            )
        {
            continue;
        }
        let storage = variable
            .storage()
            .ok_or(NativeTransportError::BufferShape)?;
        if storage.column() != FmiStorageColumn::P
            || (storage.scalar_count() != 0 && schedule.input_lane(storage.base()).is_none())
        {
            continue;
        }
        let Some(source) = lane_source(variable) else {
            continue;
        };
        if let LaneSource::Declared(declared) = &source
            && storage.scalar_count() != declared.len()
        {
            return Err(NativeTransportError::BufferShape);
        }
        match source {
            LaneSource::Declared(SolveDeclarationStart::Integer(values)) => {
                visit_runs(storage.base(), &values, &mut visit)?
            }
            LaneSource::Declared(SolveDeclarationStart::Boolean(values)) => {
                visit_runs(storage.base(), &values, &mut visit)?
            }
            LaneSource::Declared(_) => return Err(NativeTransportError::BufferShape),
            LaneSource::Evaluated(kind) => visit_evaluated(storage, kind, p, &mut visit)?,
        }
    }
    Ok(())
}

/// Convert the evaluated P cells of a parameter or constant exactly: an
/// Integer must be integral and within `i64`, a Boolean exactly 0 or 1.
fn visit_evaluated(
    storage: rumoca_ir_solve::fmi::FmiStorageRun,
    kind: SolveVariableValueKind,
    p: &[f64],
    visit: &mut impl FnMut(usize, NativeTypedInputBatch<'_>) -> Result<(), NativeTransportError>,
) -> Result<(), NativeTransportError> {
    let cells = p
        .get(storage.base()..storage.base() + storage.scalar_count())
        .ok_or(NativeTransportError::BufferShape)?;
    let inexact = |offset: usize| NativeTransportError::InexactParameter {
        p_index: storage.base() + offset,
    };
    if kind == SolveVariableValueKind::Integer {
        let values = cells
            .iter()
            .enumerate()
            .map(|(offset, &value)| exact_integer(value).ok_or_else(|| inexact(offset)))
            .collect::<Result<Vec<_>, _>>()?;
        visit(storage.base(), NativeTypedInputBatch::Integer(&values))
    } else {
        let values = cells
            .iter()
            .enumerate()
            .map(|(offset, &value)| match value {
                0.0 => Ok(false),
                1.0 => Ok(true),
                _ => Err(inexact(offset)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        visit(storage.base(), NativeTypedInputBatch::Boolean(&values))
    }
}

fn exact_integer(value: f64) -> Option<i64> {
    // 2^63 is exactly representable and is the first value outside `i64`.
    (value.fract() == 0.0
        && (-9_223_372_036_854_775_808.0..9_223_372_036_854_775_808.0).contains(&value))
    .then_some(value as i64)
}

trait SourceScalar: Copy {
    fn literal(values: &[Self]) -> NativeTypedInputBatch<'_>;
    fn repeat(value: Self, count: usize) -> NativeTypedInputBatch<'static>;
}

impl SourceScalar for i64 {
    fn literal(values: &[Self]) -> NativeTypedInputBatch<'_> {
        NativeTypedInputBatch::Integer(values)
    }
    fn repeat(value: Self, count: usize) -> NativeTypedInputBatch<'static> {
        NativeTypedInputBatch::IntegerRepeat { value, count }
    }
}

impl SourceScalar for bool {
    fn literal(values: &[Self]) -> NativeTypedInputBatch<'_> {
        NativeTypedInputBatch::Boolean(values)
    }
    fn repeat(value: Self, count: usize) -> NativeTypedInputBatch<'static> {
        NativeTypedInputBatch::BooleanRepeat { value, count }
    }
}

fn visit_runs<T: SourceScalar>(
    base: usize,
    values: &SolveExactInitialValues<T>,
    visit: &mut impl FnMut(usize, NativeTypedInputBatch<'_>) -> Result<(), NativeTransportError>,
) -> Result<(), NativeTransportError> {
    for run in values.runs() {
        let start = base
            .checked_add(run.start())
            .ok_or(NativeTransportError::BufferShape)?;
        let batch = match run {
            SolveExactInitialRun::Literal { values, .. } => T::literal(values),
            SolveExactInitialRun::Repeat { value, count, .. } => T::repeat(*value, *count),
        };
        visit(start, batch)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::exact_integer;

    #[test]
    fn an_evaluated_integer_cell_must_be_integral_and_within_i64() {
        assert_eq!(exact_integer(-0.0), Some(0));
        assert_eq!(exact_integer(5.0), Some(5));
        assert_eq!(exact_integer(-9_223_372_036_854_775_808.0), Some(i64::MIN));
        assert_eq!(exact_integer(9_223_372_036_854_775_808.0), None);
        assert_eq!(exact_integer(2.5), None);
        assert_eq!(exact_integer(f64::NAN), None);
        assert_eq!(exact_integer(f64::INFINITY), None);
    }
}
