//! Opt-in full state Jacobian derived from one borrowed explicit ODE source.
//! Unsupported sources refuse before any entry is returned; the existing
//! general runtime and its algebraic-projection semantics are unchanged.
mod admission;
mod scalar;
#[cfg(test)]
mod tests;

use rumoca_core::Span;
use rumoca_ir_solve::{
    ContinuousNumericalSource, ContinuousNumericalSourceError, SolveModel,
    SolveProgramConstructionError, SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallSite,
    SolvePureCallTable, SolveScalarType, SolveSpecializationIdentity, SolveTargetProfile,
    SolveTargetSelection, SolveValueType, StoredStateJacobianOrderError,
    StoredStateJacobianOutputOrder,
};
use std::num::NonZeroU64;

/// Checked structural solver-state directions: absent cells are positive zero;
/// present numeric zeros stay active. All input/primal/public-result values
/// must be finite; original primal failures are diagnosed before AD masking.
/// Existing canonical local-partial/kink rules and unsupported subset remain.
///
/// No caller-supplied derivative, unchecked output permutation or detached helper
/// table enters this constructor. Output axes require the model-bound checked
/// stored-state descriptor; the model remains immutably borrowed for its life.
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveModel;
/// use rumoca_eval_solve::model_jacobian::ModelStateJacobian;
/// fn cannot_replace(model: &mut SolveModel) {
///     let graph = ModelStateJacobian::construct(model).unwrap();
///     model.problem = Default::default();
///     let _ = graph.states();
/// }
/// ```
#[derive(Debug)]
pub struct ModelStateJacobian<'source> {
    source: ContinuousNumericalSource<'source>,
    table: SolvePureCallTable,
    site: SolvePureCallSite,
    states: usize,
    parameters: usize,
    order: StoredStateJacobianOutputOrder<'source>,
    specialization: SolveSpecializationIdentity,
}

#[derive(Debug)]
pub enum ModelStateJacobianError {
    Source(ContinuousNumericalSourceError),
    Unsupported {
        reason: &'static str,
        span: Option<Span>,
    },
    Construction(SolveProgramConstructionError),
    OutputOrder(StoredStateJacobianOrderError),
    Specialization(rumoca_ir_solve::SolveAlgorithmBlockDigestError),
}

impl std::fmt::Display for ModelStateJacobianError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Source(error) => error.fmt(f),
            Self::Unsupported { reason, .. } => {
                write!(f, "shared state Jacobian unsupported: {reason}")
            }
            Self::Construction(error) => error.fmt(f),
            Self::OutputOrder(error) => error.fmt(f),
            Self::Specialization(error) => error.fmt(f),
        }
    }
}
impl std::error::Error for ModelStateJacobianError {}

impl<'source> ModelStateJacobian<'source> {
    pub fn states(&self) -> usize {
        self.states
    }
    pub fn parameters(&self) -> usize {
        self.parameters
    }
    /// Canonical state INPUT order; output axes are declared independently.
    pub fn state_labels(&self) -> &[String] {
        self.order.input_labels()
    }
    pub fn output_order(&self) -> &StoredStateJacobianOutputOrder<'source> {
        &self.order
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
    pub fn specialization(&self) -> &SolveSpecializationIdentity {
        &self.specialization
    }
}

impl<'source> ModelStateJacobian<'source> {
    /// Compile-domain construction only; performs no evaluation or native work.
    /// Supports checked scalar explicit ODE programs with real arithmetic.
    /// Calls, structured nodes, lazy regions, effects and refresh are refused.
    pub fn construct(model: &'source SolveModel) -> Result<Self, ModelStateJacobianError> {
        Self::construct_for_target(model, &SolveTargetProfile::current_dense_host())
    }

    /// Target selection precedes only final materialization. Source/AD issue
    /// order and compact placement facts remain owned by this constructor.
    pub fn construct_for_target(
        model: &'source SolveModel,
        target: &SolveTargetProfile,
    ) -> Result<Self, ModelStateJacobianError> {
        let source = model
            .continuous_numerical_source()
            .map_err(ModelStateJacobianError::Source)?;
        let order = StoredStateJacobianOutputOrder::canonical(model)
            .map_err(ModelStateJacobianError::OutputOrder)?;
        Self::construct_from_source(order, source, target.select())
    }

    /// Construct from one checked stored-coordinate axis request. The exact
    /// originating model is obtained from the descriptor, never supplied twice.
    pub fn construct_with_output_order(
        order: StoredStateJacobianOutputOrder<'source>,
    ) -> Result<Self, ModelStateJacobianError> {
        Self::construct_with_output_order_for_target(
            order,
            &SolveTargetProfile::current_dense_host(),
        )
    }

    pub fn construct_with_output_order_for_target(
        order: StoredStateJacobianOutputOrder<'source>,
        target: &SolveTargetProfile,
    ) -> Result<Self, ModelStateJacobianError> {
        let model = order.source_model();
        let source = model
            .continuous_numerical_source()
            .map_err(ModelStateJacobianError::Source)?;
        Self::construct_from_source(order, source, target.select())
    }

    fn construct_from_source(
        order: StoredStateJacobianOutputOrder<'source>,
        source: ContinuousNumericalSource<'source>,
        target: SolveTargetSelection<'_>,
    ) -> Result<Self, ModelStateJacobianError> {
        let model = order.source_model();
        let programs = admission::programs(model, &source)?;
        let states = model.state_scalar_count();
        let parameters = source.layout().p_scalars();
        let profile = source.calls().arithmetic();
        let span = programs[0].span;
        let width = u32::try_from(states)
            .map_err(|_| admission::unsupported("matrix extent exceeds typed shape", Some(span)))?;
        let input_count = states
            .checked_add(parameters)
            .and_then(|n| n.checked_add(1))
            .ok_or_else(|| admission::unsupported("input extent overflows", Some(span)))?;
        let real = SolveValueType::scalar(SolveScalarType::real(profile));
        let matrix = SolveValueType::tensor(SolveScalarType::real(profile), vec![width, width])
            .map_err(|_| admission::unsupported("matrix type overflows", Some(span)))?;
        let mut issued = None;
        let table = SolvePureCallTable::construct(profile, |table| {
            let owner = table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(1).expect("fixed local identity")),
                vec![real; input_count],
                vec![SolvePureCallOutput::result(matrix)],
                span,
                |builder, inputs, outputs| {
                    scalar::construct(
                        builder,
                        inputs,
                        outputs[0],
                        &programs,
                        &order,
                        scalar::Target {
                            arithmetic: profile,
                            selection: target,
                        },
                        span,
                    )
                },
            )?;
            issued = table.call_site(owner);
            Ok(())
        })
        .map_err(ModelStateJacobianError::Construction)?;
        let site = issued.ok_or_else(|| {
            admission::unsupported("constructor did not issue its owner", Some(span))
        })?;
        let specialization = target
            .bind(&table)
            .map_err(ModelStateJacobianError::Specialization)?;
        Ok(Self {
            source,
            table,
            site,
            states,
            parameters,
            order,
            specialization,
        })
    }
}
