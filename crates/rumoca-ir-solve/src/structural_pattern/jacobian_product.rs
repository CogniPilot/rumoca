//! One source-bound scalar graph and its construction-issued state dependencies.
//! This staged IR API is not yet consumed by the evaluator/native path. Compact
//! body receipts and the atomic consumer cutover are separate source slices.
mod construction;
#[cfg(test)]
mod tests;
mod value;

pub use construction::{StateJacobianProductConstruction, StateJacobianProductInput};
pub use value::StateJacobianProductValue;

use super::{PatternDerivation, PatternProvenance, StructuralPattern, StructuralPatternError};
use crate::{
    ContinuousNumericalSource, SolveModel, SolveProgramConstructionError, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallSite, SolvePureCallTable, SolveScalarType,
    SolveSpecializationIdentity, SolveTargetProfile, SolveValueType,
    StoredStateJacobianOutputOrder,
};
use rumoca_core::Span;
use std::num::NonZeroU64;

/// The declared coordinate and exceptional-value policy of this product.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StateJacobianProductDomain {
    /// State identity seeds, structural absence, finite inputs/primal/results.
    FiniteStoredState,
}

/// One immutable model-bound state-Jacobian graph and its presence relation.
/// Semantic source traversal remains with the evaluator producer. This local
/// constructor binds that producer's graph, opaque directions, stored axes and
/// pattern together; it accepts no caller-provided dependency rows or pattern.
/// ```compile_fail
/// use rumoca_ir_solve::{StateJacobianProduct, StructuralPattern};
/// fn replace_pattern<'s>(valid: StateJacobianProduct<'s>, arbitrary: StructuralPattern) {
///     let _ = StateJacobianProduct { dependencies: arbitrary, ..valid };
/// }
/// ```
#[derive(Debug)]
pub struct StateJacobianProduct<'source> {
    source: ContinuousNumericalSource<'source>,
    table: SolvePureCallTable,
    site: SolvePureCallSite,
    order: StoredStateJacobianOutputOrder<'source>,
    dependencies: StructuralPattern,
    canonical_dependencies: StructuralPattern,
    specialization: SolveSpecializationIdentity,
}

#[derive(Debug, thiserror::Error)]
pub enum StateJacobianProductConstructionError {
    #[error("state-Jacobian source and axes belong to different models")]
    ForeignSource,
    #[error(transparent)]
    Specialization(#[from] crate::SolveAlgorithmBlockDigestError),
    #[error(transparent)]
    Program(#[from] SolveProgramConstructionError),
    #[error(transparent)]
    Pattern(#[from] StructuralPatternError),
}

impl<'source> StateJacobianProduct<'source> {
    /// Construct graph and dependencies in the same branded owner closure.
    /// This is IR-local invariant construction, not a model analysis API.
    pub fn construct(
        source: ContinuousNumericalSource<'source>,
        order: StoredStateJacobianOutputOrder<'source>,
        target: &SolveTargetProfile,
        span: Span,
        build: impl for<'program, 'scope> FnOnce(
            &mut StateJacobianProductConstruction<'program, 'scope>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, StateJacobianProductConstructionError> {
        let model = order.source_model();
        if !std::ptr::eq(source.coordinates(), &model.problem.solve_layout)
            || !std::ptr::eq(source.layout(), &model.problem.layout)
            || !std::ptr::eq(source.continuous(), &model.problem.continuous)
        {
            return Err(StateJacobianProductConstructionError::ForeignSource);
        }
        let states = order.input_labels().len();
        let parameters = source.layout().p_scalars();
        let profile = source.calls().arithmetic();
        let width = u32::try_from(states).map_err(|_| invalid(span))?;
        let count = states
            .checked_add(parameters)
            .and_then(|n| n.checked_add(1))
            .ok_or_else(|| invalid(span))?;
        let real = SolveValueType::scalar(SolveScalarType::real(profile));
        let matrix = SolveValueType::tensor(SolveScalarType::real(profile), vec![width, width])
            .map_err(|_| invalid(span))?;
        let mut issued = None;
        let mut dependencies = None;
        let table = SolvePureCallTable::construct(profile, |table| {
            let owner = table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(1).expect("fixed local identity")),
                vec![real; count],
                vec![SolvePureCallOutput::result(matrix)],
                span,
                |builder, inputs, outputs| {
                    let mut construction = StateJacobianProductConstruction::new(
                        builder,
                        inputs,
                        outputs[0],
                        (states, parameters, profile),
                        span,
                    )?;
                    build(&mut construction)?;
                    dependencies = Some(construction.finish(&order, span)?);
                    Ok(())
                },
            )?;
            issued = table.call_site(owner);
            Ok(())
        })?;
        let canonical_dependencies = dependencies.ok_or_else(|| invalid(span))??;
        let dependencies =
            canonical_dependencies.project(order.row_coordinates(), order.column_coordinates())?;
        let specialization = target.select().bind(&table)?;
        Ok(Self {
            source,
            table,
            site: issued.ok_or_else(|| invalid(span))?,
            order,
            dependencies,
            canonical_dependencies,
            specialization,
        })
    }

    pub fn specialization(&self) -> &SolveSpecializationIdentity {
        &self.specialization
    }

    pub fn states(&self) -> usize {
        self.order.input_labels().len()
    }
    pub fn parameters(&self) -> usize {
        self.source.layout().p_scalars()
    }
    pub fn state_labels(&self) -> &[String] {
        self.order.input_labels()
    }
    pub fn output_order(&self) -> &StoredStateJacobianOutputOrder<'source> {
        &self.order
    }
    pub fn source_model(&self) -> &'source SolveModel {
        self.order.source_model()
    }
    pub fn source(&self) -> &ContinuousNumericalSource<'source> {
        &self.source
    }
    pub fn table(&self) -> &SolvePureCallTable {
        &self.table
    }
    pub fn call_site(&self) -> &SolvePureCallSite {
        &self.site
    }
    pub const fn domain(&self) -> StateJacobianProductDomain {
        StateJacobianProductDomain::FiniteStoredState
    }
    /// The relation in the emitted dense matrix's declared output axes.
    pub fn dependencies(&self) -> &StructuralPattern {
        &self.dependencies
    }
    /// The same issued relation in canonical state coordinates, before placement.
    pub fn canonical_dependencies(&self) -> &StructuralPattern {
        &self.canonical_dependencies
    }
}

fn invalid(provenance: Span) -> SolveProgramConstructionError {
    SolveProgramConstructionError::InvalidTensorAlgebra { provenance }
}

fn issue_dependencies(
    states: usize,
    cells: &[Option<crate::ProgramRegister<'_>>],
    span: Span,
) -> Result<StructuralPattern, StructuralPatternError> {
    let rows = cells
        .chunks(states)
        .map(|row| {
            row.iter()
                .enumerate()
                .filter_map(|(column, value)| value.is_some().then_some(column))
                .collect()
        })
        .collect::<Vec<Vec<usize>>>();
    let provenance = PatternProvenance::derived(PatternDerivation::DependencyPropagation, span)?;
    StructuralPattern::from_checked_row_dependencies(states, states, &rows, provenance)
}
