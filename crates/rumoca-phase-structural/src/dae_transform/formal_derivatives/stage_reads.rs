//! STRUCT-T07 stage certification of the reads a prolongation actually makes.
//!
//! Differential offsets come from each owner's source signature, which follows
//! function bodies. A supplied derivative (MLS §12.7.1) may read coordinates the
//! body never reads: MSL `Frames.resolve1` reads `R.T`, while its derivative
//! `resolve1_der` (`noDerivative=R`) reads the rate `R.w`. Once an owner is
//! prolonged twice, that read sits one stage below the highest, and the rate
//! must be a coordinate of that stage or lower; otherwise the stage would depend
//! on a value only a later stage determines. This pass reports every such read
//! as the formal order its variable needs, so construction can certify it as a
//! stage dependency and keep the supplied derivative.

use std::collections::BTreeMap;

use rumoca_eval_dae::{ScalarCoordinateProjectionCache, for_each_scalar_coordinate_cached};
use rumoca_ir_dae as dae;

use super::EquationProlongation;
use crate::StructuralError;
use crate::incidence::projection::{projection_error, visit_owner_rows};

/// Source variable ordinal and formal order of a formal coordinate, plus the
/// highest formal order the variable has.
#[derive(Clone, Copy)]
struct FormalOrder {
    source: usize,
    order: u32,
    depth: u32,
}

/// Every source variable a prolonged owner reads above its own stage, with the
/// least formal order that places the read at or below that stage. Parameter
/// constant variables (`invariant`) have zero derivatives at every order and
/// never raise.
pub(super) fn later_stage_reads(
    formal: dae::DaeView<'_>,
    coordinates: &[Vec<u32>],
    equations: &[EquationProlongation],
    invariant: &[bool],
) -> Result<BTreeMap<usize, u32>, StructuralError> {
    let mut reads = StageReads {
        orders: formal_orders(coordinates),
        invariant,
        needed: BTreeMap::new(),
    };
    let mut cache = ScalarCoordinateProjectionCache::default();
    for equation in equations {
        let prolongation = equation.derivatives.len() as i64;
        for order in 0..=prolongation {
            let owner_index = if order == 0 {
                equation.original
            } else {
                equation.derivatives.start + order as usize - 1
            };
            let owner = formal.continuous_owner(owner_index).ok_or_else(|| {
                StructuralError::UnspannedContractViolation {
                    reason: "formal equation owner is missing from the prolonged system".into(),
                }
            })?;
            let stage = order - prolongation;
            visit_owner_rows(formal, owner, |row| {
                for_each_scalar_coordinate_cached(
                    formal,
                    row.expression,
                    row.scalar,
                    row.domain_point,
                    &mut cache,
                    |coordinate, _| reads.record(coordinate, stage),
                )
                .map_err(projection_error)
            })?;
        }
    }
    Ok(reads.needed)
}

struct StageReads<'a> {
    orders: BTreeMap<u32, FormalOrder>,
    invariant: &'a [bool],
    needed: BTreeMap<usize, u32>,
}

impl StageReads<'_> {
    /// Record the order a read coordinate needs to sit at or below `stage`.
    fn record(&mut self, coordinate: dae::CoordinateView<'_>, stage: i64) {
        let Some(read) = read_order(&self.orders, coordinate) else {
            return;
        };
        if self.invariant.get(read.source).copied().unwrap_or(false) {
            return;
        }
        let level = i64::from(read.order) - i64::from(read.depth);
        if level <= stage {
            return;
        }
        let depth = (i64::from(read.order) - stage) as u32;
        let entry = self.needed.entry(read.source).or_insert(0);
        *entry = (*entry).max(depth);
    }
}

fn formal_orders(coordinates: &[Vec<u32>]) -> BTreeMap<u32, FormalOrder> {
    coordinates
        .iter()
        .enumerate()
        .flat_map(|(source, formal)| {
            let depth = formal.len().saturating_sub(1) as u32;
            formal.iter().enumerate().map(move |(order, &id)| {
                (
                    id,
                    FormalOrder {
                        source,
                        order: order as u32,
                        depth,
                    },
                )
            })
        })
        .collect()
}

fn read_order(
    orders: &BTreeMap<u32, FormalOrder>,
    coordinate: dae::CoordinateView<'_>,
) -> Option<FormalOrder> {
    match coordinate {
        dae::CoordinateView::Algebraic(id) => orders.get(&id.index()).copied(),
        dae::CoordinateView::State(id) => orders.get(&id.index()).copied(),
        dae::CoordinateView::Derivative(id) => orders.get(&id.index()).map(|value| FormalOrder {
            order: value.order + 1,
            ..*value
        }),
        _ => None,
    }
}
