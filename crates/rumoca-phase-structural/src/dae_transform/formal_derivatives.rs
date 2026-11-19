//! Coupled differentiated equations, prior to independent-state construction.

use std::collections::BTreeSet;

use rumoca_ir_dae as dae;

use super::variables::ReservedVariable;
use crate::StructuralError;
use crate::differential_structure::analyze_differential_structure_with_order_bounds;

mod slope_invariance;
mod stage_reads;
mod stages;
mod state_candidates;
#[cfg(test)]
mod tests;
pub(super) use stages::EquationProlongation;
pub use stages::{FormalDerivativeStage, FormalStageCoordinate, FormalStageEquation};
pub(super) use state_candidates::SelectedCoordinate;
pub use state_candidates::{
    FormalStateCandidate, FormalStateCandidateView, FormalStateCoordinate, ReducedSelectionChart,
    StateSelection,
};

/// An inspectable prolongation of one source DAE, not an executable state basis.
pub struct FormalDerivativeSystem<'source> {
    source: &'source dae::Dae,
    model: dae::Dae,
    coordinates: Vec<Vec<u32>>,
    equations: Vec<EquationProlongation>,
    dimension: usize,
    source_pins: Vec<super::InitialValuePin>,
    withheld: Vec<WithheldPreference>,
}

/// A `StateSelect.prefer` request that is not honored: the declaration and
/// why its selection as a state was refused. `prefer` is a request (MLS 3.7
/// §4.9.7.1), so the reducer's basis is integrated and this record is what
/// every consumer reports (ES017).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithheldPreference {
    pub variable: String,
    pub declaration: rumoca_core::Span,
    pub cause: WithheldCause,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WithheldCause {
    /// Selecting the declaration needs the time derivative of the operation
    /// written at `span`, which is refused.
    Operation {
        span: rumoca_core::Span,
        refusal: RefusedOperation,
    },
    /// Any other refusal of the declaration's derivative, of the requested
    /// basis, or of its checked selection, in the refusal's own words.
    Refusal(String),
}

/// Why the time derivative of an operation is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefusedOperation {
    /// It is written under `noEvent`: MLS §3.7.3 forbids the event its
    /// derivative would need.
    NoEvent,
    /// A conditional whose guard is neither fixed between events nor a
    /// relation that can own one.
    LiveGuard,
    /// The shared differentiation rules have none for it.
    NoRule,
}

impl WithheldPreference {
    /// One sentence naming the declaration, its site, and the cause, with the
    /// refused operation as the source writes it.
    #[must_use]
    pub fn message(&self, model: &dae::Dae) -> String {
        let declaration = source_site(model, self.declaration);
        let cause = match &self.cause {
            WithheldCause::Operation { span, refusal } => {
                let text = source_excerpt(model, *span).unwrap_or("an operation");
                let site = source_site(model, *span);
                let why = match refusal {
                    RefusedOperation::NoEvent => {
                        "which `noEvent` keeps from owning the event its derivative needs"
                    }
                    RefusedOperation::LiveGuard => {
                        "whose guard is neither fixed between events nor a relation that can \
                         own an event"
                    }
                    RefusedOperation::NoRule => "which has no differentiation rule",
                };
                format!("selecting it needs the time derivative of `{text}`{site}, {why}")
            }
            WithheldCause::Refusal(refusal) => refusal.clone(),
        };
        format!("`{}`{declaration} is not selected: {cause}", self.variable)
    }
}

/// ` (file:line:column)` of a span, or nothing when the model carries no
/// source text for it.
fn source_site(model: &dae::Dae, span: rumoca_core::Span) -> String {
    let Some((name, text)) = model.source_map().get_source(span.source) else {
        return String::new();
    };
    let position = rumoca_core::text_position::byte_offset_to_position(text, span.start.0);
    format!(" ({name}:{}:{})", position.line + 1, position.character + 1)
}

/// The first line of the source text a span covers.
fn source_excerpt(model: &dae::Dae, span: rumoca_core::Span) -> Option<&str> {
    let (_, text) = model.source_map().get_source(span.source)?;
    let excerpt = text.get(span.start.0..span.end.0)?.lines().next()?.trim();
    (!excerpt.is_empty()).then_some(excerpt)
}

/// Both source and constructed coordinate identities stay bound to their roots.
#[derive(Clone, Copy)]
pub struct FormalDerivativeView<'map, 'source, 'target> {
    pub source: dae::DaeView<'source>,
    pub view: dae::DaeView<'target>,
    /// Exact initial-value transfers, indexed only in `source`.
    pub source_pins: &'map [super::InitialValuePin],
    coordinates: &'map [Vec<u32>],
    equations: &'map [EquationProlongation],
    dimension: usize,
}

impl FormalDerivativeSystem<'_> {
    /// The `prefer` requests withheld to complete this construction, in
    /// declaration order; empty when every admitted request was constructed.
    pub fn withheld_preferences(&self) -> &[WithheldPreference] {
        &self.withheld
    }

    pub fn inspect<R>(
        &self,
        inspect: impl for<'source, 'target> FnOnce(FormalDerivativeView<'_, 'source, 'target>) -> R,
    ) -> R {
        self.source.inspect(|source| {
            self.model.inspect(|view| {
                inspect(FormalDerivativeView {
                    source,
                    view,
                    source_pins: &self.source_pins,
                    coordinates: &self.coordinates,
                    equations: &self.equations,
                    dimension: self.dimension,
                })
            })
        })
    }
}

impl<'source, 'target> FormalDerivativeView<'_, 'source, 'target> {
    /// Order zero denotes the unchanged source value; higher orders are tensors
    /// of formal derivatives with the same shape.
    pub fn coordinate(
        &self,
        variable: dae::VariableId<'source>,
        order: usize,
    ) -> Option<dae::VariableId<'target>> {
        self.coordinates[variable.index() as usize]
            .get(order)
            .and_then(|&id| self.view.variable_id(id as usize))
    }

    pub fn formal_dimension(&self) -> usize {
        self.dimension
    }
}

/// Preserve the source system and append formal derivatives at certified orders.
/// Regular independent coordinates and executable reconstruction are still needed.
///
/// Offsets start from the source signatures. When a prolonged owner reads a
/// coordinate above its own stage (a supplied derivative's `noDerivative`
/// input rate, see the `stage_reads` module), that variable's order is raised and the
/// offsets are certified again, so the read becomes a dependency its stage
/// determines. Each round strictly raises a bound and the shared
/// differentiation profile caps orders, so the refinement terminates; a read
/// no raise can place is refused. Withholding a preference changes only tensor
/// admission, so the source analysis is reused until an order bound rises.
pub fn construct_formal_derivatives(
    model: &dae::Dae,
) -> Result<FormalDerivativeSystem<'_>, StructuralError> {
    model.inspect(|source| {
        let invariance = crate::time_invariant::TimeInvariance::derive(source);
        let mut order_bounds = vec![0_u32; source.variables().count()];
        let mut withheld = BTreeSet::new();
        let mut refusals = Vec::new();
        let mut analysis = analyze_differential_structure_with_order_bounds(source, &order_bounds)?;
        loop {
            let offsets = analysis
                .tensor_offsets_withholding(source, &withheld)?
                .ok_or_else(|| StructuralError::UnspannedContractViolation {
                    reason:
                        "formal derivative construction requires compatible whole-tensor orders"
                            .into(),
                })?;
            let mut orders = vec![0; source.variables().count()];
            for (coordinate, &order) in analysis.variables().iter().zip(offsets.variable_orders()) {
                orders[coordinate.variable().index() as usize] = order;
            }
            let (rebuilt, coordinates, equations) = match super::reconstruction::rebuild_formal(
                model,
                source,
                &orders,
                offsets.equation_orders(),
            ) {
                Ok(rebuilt) => rebuilt,
                Err(refusal) => {
                    refusals.extend(withhold_refused_preferences(
                        source,
                        &mut withheld,
                        offsets.preferred(),
                        *refusal,
                    )?);
                    continue;
                }
            };
            let reads = rebuilt.inspect(|formal| {
                stage_reads::later_stage_reads(
                    formal,
                    &coordinates,
                    &equations,
                    invariance.algebraics(),
                )
            })?;
            if reads.is_empty() {
                return Ok(FormalDerivativeSystem {
                    source: model,
                    model: rebuilt,
                    coordinates,
                    equations,
                    dimension: analysis.formal_dimension(),
                    source_pins: super::transferred_initial_values(source)?,
                    withheld: in_declaration_order(refusals),
                });
            }
            if !raise_order_bounds(&mut order_bounds, reads) {
                return Err(StructuralError::UnspannedContractViolation {
                    reason: "a prolonged equation reads a coordinate that no certified stage \
                             order determines"
                        .into(),
                });
            }
            analysis = analyze_differential_structure_with_order_bounds(source, &order_bounds)?;
        }
    })
}

fn in_declaration_order(mut records: Vec<(u32, WithheldPreference)>) -> Vec<WithheldPreference> {
    records.sort_by_key(|(variable, _)| *variable);
    records.into_iter().map(|(_, record)| record).collect()
}

/// Withhold the `StateSelect.prefer` admissions that raised a refused owner
/// across the derivative level its prolongation could not construct. A `prefer`
/// request is optional (MLS 3.7 §4.9.7.1), so an owner the shared
/// differentiation rules cannot prolong to that level removes the admissions
/// that required it, and the construction repeats without them. A refusal no
/// admission caused is the source system's own and is returned unchanged. Each
/// repetition withholds at least one more declaration, so this terminates.
/// Every withheld declaration is returned with the refusal that caused it, so
/// the request is never dropped without a record.
fn withhold_refused_preferences(
    source: dae::DaeView<'_>,
    withheld: &mut BTreeSet<u32>,
    admitted: &[crate::PreferredAdmission],
    refusal: super::reconstruction::FormalRebuildRefusal,
) -> Result<Vec<(u32, WithheldPreference)>, StructuralError> {
    let Some((owner, level)) = refusal.owner else {
        return Err(refusal.error);
    };
    let cause = refusal
        .operation
        .and_then(|operation| refused_operation(source, operation))
        .unwrap_or_else(|| WithheldCause::Refusal(refusal.error.to_string()));
    let records = admitted
        .iter()
        .filter(|admission| {
            admission
                .raised_owners
                .iter()
                .any(|&(raised, from)| raised == owner && from < level)
        })
        .filter(|admission| withheld.insert(admission.variable))
        .filter_map(|admission| {
            let variable = source
                .variable_id(admission.variable as usize)
                .and_then(|id| source.variable(id))?;
            Some((
                admission.variable,
                WithheldPreference {
                    variable: variable.name().to_string(),
                    declaration: variable.declaration().span(),
                    cause: cause.clone(),
                },
            ))
        })
        .collect::<Vec<_>>();
    if records.is_empty() {
        return Err(refusal.error);
    }
    Ok(records)
}

/// The refused operation at source expression ordinal `operation`, with the
/// reason its own form gives.
fn refused_operation(source: dae::DaeView<'_>, operation: u32) -> Option<WithheldCause> {
    let node = source.expression(source.expression_id(operation as usize)?)?;
    let refusal = match node.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::NoEvent,
            ..
        } => RefusedOperation::NoEvent,
        dae::ExpressionOperation::Conditional(_) => RefusedOperation::LiveGuard,
        _ => RefusedOperation::NoRule,
    };
    let span = node.provenance().span();
    (!span.is_dummy()).then_some(WithheldCause::Operation { span, refusal })
}

/// Raise each bound to the order a later-stage read needs; `false` when no
/// bound rises, so another certification round cannot place the reads.
fn raise_order_bounds(
    order_bounds: &mut [u32],
    reads: std::collections::BTreeMap<usize, u32>,
) -> bool {
    let mut raised = false;
    for (variable, order) in reads {
        let bound = &mut order_bounds[variable];
        raised |= order > *bound;
        *bound = (*bound).max(order);
    }
    raised
}

pub(super) fn reserve_derivatives<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
    variables: &mut [ReservedVariable<'target>],
    orders: &[u32],
) -> Result<(), dae::DaeConstructionError> {
    let mut names = source.variables().map(|(_, v)| v.name().clone()).collect();
    for ((id, variable), reserved) in source.variables().zip(variables) {
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            variable.declaration().span(),
        )?;
        for order in 1..=orders[id.index() as usize] {
            let name = available_name(variable.name(), order, &mut names);
            let start = target.expressions(|expressions| {
                super::expressions::shaped_zero(
                    expressions,
                    variable.value_type().dimensions(),
                    provenance,
                )
            })?;
            let derivative = target.variables(|target| {
                let (id, reservation) = target.reserve_algebraic(
                    name,
                    types[variable.value_type_id().index() as usize],
                    provenance,
                )?;
                target.define(
                    reservation,
                    dae::VariableAttributes {
                        start: Some(start),
                        fixed: Some(vec![false]),
                        origin: dae::VariableOrigin::Generated,
                        ..Default::default()
                    },
                    provenance,
                )?;
                Ok(id)
            })?;
            reserved.formal_derivatives.push(derivative);
        }
        if variable.role() == dae::VariableRole::State {
            reserved.derivative_alias = reserved.formal_derivatives.first().copied();
            if reserved.derivative_alias.is_none() {
                return Err(dae::DaeConstructionError::IncompleteDefinition {
                    kind: "formal derivative for source state",
                    index: id.index(),
                    span: variable.declaration().span(),
                });
            }
        }
    }
    Ok(())
}

fn available_name(
    source: &rumoca_core::VarName,
    order: u32,
    names: &mut BTreeSet<rumoca_core::VarName>,
) -> rumoca_core::VarName {
    let base = format!("$formal_derivative.{order}.{source}");
    let mut candidate = rumoca_core::VarName::new(&base);
    let mut suffix = 0;
    while !names.insert(candidate.clone()) {
        suffix += 1;
        candidate = rumoca_core::VarName::new(format!("{base}.{suffix}"));
    }
    candidate
}
