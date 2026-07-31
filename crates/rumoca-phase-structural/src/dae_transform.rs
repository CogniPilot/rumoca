//! Constructor-only structural DAE-to-DAE lowering.
//!
//! Regular systems remain borrowed. A singular system is rebuilt only when a
//! scalar state is directly defined by a differentiable constraint. The
//! replacement DAE demotes that state and substitutes the exact symbolic
//! derivative of its definition at every derivative occurrence.

mod constraints;
mod declarations;
mod differentiation;
mod event_owners;
mod expressions;
mod functions;
mod reconstruction;
mod semantic_owners;
mod temporal;
#[cfg(test)]
mod tests;
mod variables;

use rumoca_ir_dae as dae;

use self::constraints::{direct_state_constraints, holonomic_constraints};
use self::reconstruction::{rebuild_holonomic_constraint, rebuild_with_state_demotion};
use crate::{StructuralError, sort};

/// A finalized DAE ready for Solve lowering.
pub enum PreparedDae<'source> {
    Borrowed(&'source dae::Dae),
    Transformed {
        dae: Box<dae::Dae>,
        manifold: Box<[u32]>,
    },
}

impl PreparedDae<'_> {
    pub fn as_dae(&self) -> &dae::Dae {
        match self {
            Self::Borrowed(dae) => dae,
            Self::Transformed { dae, .. } => dae,
        }
    }

    pub fn inspect<R>(
        &self,
        inspect: impl for<'dae> FnOnce(dae::DaeView<'dae>, &[dae::ExprId<'dae>]) -> R,
    ) -> R {
        match self {
            Self::Borrowed(dae) => dae.inspect(|view| inspect(view, &[])),
            Self::Transformed { dae, manifold } => dae.inspect(|view| {
                let expressions = manifold
                    .iter()
                    .map(|index| {
                        view.expression_id(*index as usize)
                            .expect("prepared manifold expression resolves")
                    })
                    .collect::<Vec<_>>();
                inspect(view, &expressions)
            }),
        }
    }
}

#[derive(Clone, Copy)]
struct DirectStateConstraint {
    state: u32,
    rhs: u32,
    owner: dae::DaeProvenance,
}

#[derive(Clone, Copy)]
struct HolonomicConstraint {
    residual: u32,
    owner: dae::DaeProvenance,
}

/// Prepare a finalized DAE for Solve without admitting a weaker intermediate.
///
/// The currently accepted index-one subset is intentionally narrow: primitive
/// scalar expressions, direct state definitions, and unstructured continuous
/// and initialization residuals. A model outside that subset retains its
/// original structural error instead of receiving a guessed transformation.
///
/// State demotions accumulate to a fixed point. A candidate that matches the
/// whole system wins outright; otherwise the first candidate that strictly
/// shrinks the unmatched residue is applied and the remaining candidates are
/// re-tested against that updated system. A singularity that only several
/// simultaneous demotions resolve therefore reduces, while a model that no
/// demotion improves still reports its original singularity.
pub fn prepare_for_solve(model: &dae::Dae) -> Result<PreparedDae<'_>, StructuralError> {
    let singular = match model.inspect(|view| sort(view).map(|_| ())) {
        Ok(_) => return Ok(PreparedDae::Borrowed(model)),
        Err(error @ StructuralError::Singular { .. }) => error,
        Err(StructuralError::EmptySystem) => return Ok(PreparedDae::Borrowed(model)),
        Err(error) => return Err(error),
    };
    let mut residue =
        unmatched_residue(&singular).expect("singular system reports its unmatched residue");
    let mut demoted: Option<dae::Dae> = None;
    loop {
        let step = demote_direct_state(demoted.as_ref().unwrap_or(model), residue)?;
        match step {
            None => break,
            Some(DemotionStep::Sorted(dae)) => {
                return Ok(PreparedDae::Transformed {
                    dae: Box::new(dae),
                    manifold: Box::new([]),
                });
            }
            Some(DemotionStep::Reduced { dae, residue: next }) => {
                residue = next;
                demoted = Some(dae);
            }
        }
    }
    let mut holonomic = reduce_holonomic_constraint(demoted.as_ref().unwrap_or(model))?;
    if holonomic.is_none() && demoted.is_some() {
        holonomic = reduce_holonomic_constraint(model)?;
    }
    match holonomic {
        Some((dae, manifold)) => Ok(PreparedDae::Transformed {
            dae: Box::new(dae),
            manifold: manifold.into_boxed_slice(),
        }),
        None => Err(singular),
    }
}

/// One accepted state demotion: either a fully matched replacement or a strict
/// reduction of the unmatched residue that the next round keeps working on.
enum DemotionStep {
    Sorted(dae::Dae),
    Reduced { dae: dae::Dae, residue: usize },
}

/// Demote one directly defined state of `model`.
///
/// Every candidate is tested against `model` itself, so an accumulated
/// demotion is re-tested against the system it produced rather than against a
/// stale pristine one. `residue` is the unmatched residue of `model`; only a
/// strict reduction of it is accepted as progress, which bounds the number of
/// accumulation rounds by the residue of the original singular system.
fn demote_direct_state(
    model: &dae::Dae,
    residue: usize,
) -> Result<Option<DemotionStep>, StructuralError> {
    let mut reduced = None;
    for candidate in model.inspect(direct_state_constraints) {
        let rebuilt = rebuild_with_state_demotion(model, candidate)?;
        match rebuilt.inspect(|view| sort(view).map(|_| ())) {
            Ok(()) => return Ok(Some(DemotionStep::Sorted(rebuilt))),
            Err(error) => {
                if reduced.is_none()
                    && let Some(next) = unmatched_residue(&error)
                    && next < residue
                {
                    reduced = Some(DemotionStep::Reduced {
                        dae: rebuilt,
                        residue: next,
                    });
                }
            }
        }
    }
    Ok(reduced)
}

/// Reduce one holonomic constraint of `model`, reporting the replacement DAE
/// and its manifold expressions once the differentiated system matches.
fn reduce_holonomic_constraint(
    model: &dae::Dae,
) -> Result<Option<(dae::Dae, Vec<u32>)>, StructuralError> {
    for constraint in model.inspect(holonomic_constraints) {
        let (rebuilt, manifold) = rebuild_holonomic_constraint(model, constraint)?;
        if rebuilt.inspect(|view| sort(view).map(|_| ())).is_ok() {
            return Ok(Some((rebuilt, manifold)));
        }
    }
    Ok(None)
}

/// Equations and unknowns that a maximum matching leaves unpaired, which is
/// zero exactly when the matching is perfect. Reported only for a singular
/// system; any other structural failure has no comparable residue.
fn unmatched_residue(error: &StructuralError) -> Option<usize> {
    let StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        ..
    } = error
    else {
        return None;
    };
    Some((n_equations - n_matched) + (n_unknowns - n_matched))
}
