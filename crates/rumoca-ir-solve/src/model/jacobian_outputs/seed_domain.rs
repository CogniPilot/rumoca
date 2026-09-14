//! A projection-owned seed domain for a separately lowered numerical JVP.

use super::*;

/// Exact primal programs and seed coordinates issued by one projection.
#[derive(Clone, Debug)]
pub struct ProjectionJacobianSeedDomain {
    application: ProjectionJacobianApplication,
    canonical_primal: ScalarProgramBlock,
    primal: ScalarProgramBlock,
    active: Box<[usize]>,
}

impl ProjectionJacobianSeedDomain {
    pub fn derive(
        application: &ProjectionJacobianApplication,
        primal: &ScalarProgramBlock,
    ) -> Option<Self> {
        let rows = application
            .rows()
            .iter()
            .copied()
            .enumerate()
            .map(|(index, row)| (row, index))
            .collect::<Vec<_>>();
        let selection = ProgramOutputCatalog::new(primal).selection(&rows, rows.len())?;
        let mut active = application.y_indices().to_vec();
        active.sort_unstable();
        if active.windows(2).any(|pair| pair[0] == pair[1]) {
            return None;
        }
        let mut programs = Vec::new();
        let mut spans = Vec::new();
        let mut outputs = Vec::new();
        for selected in selection.programs() {
            let index = selected.program();
            let program = primal.programs().get(index)?;
            if !seed_reads_fit(program, &[]) {
                return None;
            }
            programs.push(program.clone());
            spans.push(*primal.program_spans().get(index)?);
            outputs.extend(
                primal
                    .output_bindings()
                    .filter(|binding| binding.program == index)
                    .map(|binding| binding.logical_index),
            );
        }
        Some(Self {
            application: application.clone(),
            canonical_primal: primal.clone(),
            primal: ScalarProgramBlock::with_output_indices(programs, spans, outputs).ok()?,
            active: active.into_boxed_slice(),
        })
    }

    pub const fn primal(&self) -> &ScalarProgramBlock {
        &self.primal
    }

    pub fn active_y(&self) -> &[usize] {
        &self.active
    }

    pub fn with_lowered_derivative(
        self,
        derivative: ScalarProgramBlock,
    ) -> Option<ProjectionJacobianApplication> {
        if derivative.output_indices() != self.primal.output_indices()
            || derivative.program_spans() != self.primal.program_spans()
            || derivative.programs().len() != self.primal.programs().len()
            || !derivative
                .programs()
                .iter()
                .all(|program| seed_reads_fit(program, &self.active))
        {
            return None;
        }
        self.application
            .with_specialized_source(&self.canonical_primal, derivative)
    }
}

fn seed_range_fits(active: &[usize], start: usize, count: usize) -> bool {
    let first = active.partition_point(|&index| index < start);
    let Some(last) = first.checked_add(count) else {
        return false;
    };
    active.get(first..last).is_some_and(|range| {
        range
            .iter()
            .copied()
            .enumerate()
            .all(|(offset, index)| start.checked_add(offset) == Some(index))
    })
}

fn seed_reads_fit(program: &[crate::LinearOp], active: &[usize]) -> bool {
    program
        .iter()
        .all(|operation| seed_operation_fits(operation, active))
}

fn seed_operation_fits(operation: &crate::LinearOp, active: &[usize]) -> bool {
    use crate::LinearOp as L;
    match operation {
        L::LoadSeed { index, .. } => active.binary_search(index).is_ok(),
        L::LoadIndexedSeed { base, count, .. } => seed_range_fits(active, *base, *count),
        L::TensorLoad {
            seed_start,
            count,
            lanes,
            ..
        } => *lanes != 2 || seed_start.is_none_or(|start| seed_range_fits(active, start, *count)),
        L::FunctionFold { program, .. }
        | L::GuardedFunctionFold { program, .. }
        | L::StoreOutputFunctionFold { program, .. } => seed_reads_fit(&program.update, active),
        L::FunctionConditional { program, .. } => {
            seed_reads_fit(&program.fallback, active)
                && program.arms.iter().all(|arm| {
                    seed_reads_fit(&arm.condition, active) && seed_reads_fit(&arm.result, active)
                })
        }
        L::Const { .. }
        | L::LoadTime { .. }
        | L::LoadY { .. }
        | L::LoadP { .. }
        | L::LoadIndexedP { .. }
        | L::LoadIndexedRegister { .. }
        | L::LoadIndexedFoldCarried { .. }
        | L::LoadIndexedFoldCapture { .. }
        | L::LoadFoldCarried { .. }
        | L::LoadFoldIndex { .. }
        | L::LoadFoldCapture { .. }
        | L::LoadFunctionConditionalCapture { .. }
        | L::LoadFunctionConditionalCaptureRange { .. }
        | L::Move { .. }
        | L::LinearSolveComponent { .. }
        | L::DotProduct { .. }
        | L::MatrixMultiply { .. }
        | L::TensorBinary { .. }
        | L::TensorCross { .. }
        | L::TensorTranspose { .. }
        | L::TensorConcatenate { .. }
        | L::TensorUpdate { .. }
        | L::TensorFill { .. }
        | L::TensorIdentity { .. }
        | L::TableBounds { .. }
        | L::TableLookup { .. }
        | L::TableLookupSlope { .. }
        | L::TableNextEvent { .. }
        | L::RandomInitialState { .. }
        | L::RandomResult { .. }
        | L::RandomState { .. }
        | L::ImpureRandomInit { .. }
        | L::ImpureRandom { .. }
        | L::ImpureRandomInteger { .. }
        | L::Unary { .. }
        | L::Binary { .. }
        | L::Compare { .. }
        | L::Select { .. }
        | L::PureCall { .. }
        | L::PureCallDirectional { .. }
        | L::StoreOutputFoldTensorUpdate { .. }
        | L::StoreOutputRange { .. }
        | L::StoreOutput { .. } => true,
    }
}
