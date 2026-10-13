//! Typed provenance carried by the canonical register-version inventory.

use super::input_runs::{NativeInputCheck, NativeInputSelection};
use super::*;
use crate::register_versions::RegisterVersionInventory;
use std::sync::Arc;

#[derive(Clone)]
pub(super) enum RegisterSource {
    Other,
    Input { p_start: usize },
    Result { operation: usize, first_cell: u32 },
}

#[derive(Default)]
pub(super) struct RegisterSources {
    inventory: RegisterVersionInventory<RegisterSource>,
    pub(super) cells: BTreeMap<usize, crate::IndexIntervals>,
    pub(super) read_results: BTreeMap<usize, Vec<NativeInputCheck>>,
}

pub(super) struct RegisterSourcePiece {
    pub(super) selected: NativeInputSelection,
    pub(super) source: Arc<RegisterSource>,
    // Source-relative cell corresponding to selected.start.
    pub(super) source_offset: usize,
}

impl RegisterSources {
    pub(super) fn pieces(
        &self,
        selected: NativeInputSelection,
    ) -> Option<Vec<RegisterSourcePiece>> {
        let start = Reg::try_from(selected.start).ok()?;
        let view = self
            .inventory
            .strided_view(start, selected.count, selected.stride, 1)?;
        let mut pieces = Vec::new();
        for (low, high, piece) in view.pieces {
            let low = selected.start.checked_add(low)?;
            let high = selected.start.checked_add(high)?;
            let Some(clipped) = selected.clipped(low..high) else {
                continue;
            };
            pieces.push(RegisterSourcePiece {
                selected: clipped,
                source: piece.family,
                source_offset: piece.offset.checked_add(clipped.start - low)?,
            });
        }
        Some(pieces)
    }

    pub(super) fn has_integer_sources(&self, inputs: &input_runs::NativeInputInventory) -> bool {
        self.inventory
            .iter()
            .any(|(&register, piece)| match piece.family.as_ref() {
                RegisterSource::Other => false,
                RegisterSource::Result { .. } => true,
                RegisterSource::Input { p_start } => {
                    let start = p_start + piece.offset;
                    let count = (piece.end - u64::from(register)) as usize;
                    inputs.runs.iter().any(|run| {
                        run.lane() == NativeOutputLane::Integer
                            && run.p_range().start < start + count
                            && start < run.p_range().end
                    })
                }
            })
    }

    /// The same original operation reads first, then replaces its destinations.
    pub(super) fn define(&mut self, op: &LinearOp) {
        let Some(start) = op.dst_register() else {
            return;
        };
        self.inventory.install(
            start,
            op.dst_register_count(),
            Arc::new(RegisterSource::Other),
        );
        match op {
            LinearOp::LoadP { dst, index } => {
                self.inventory
                    .install(*dst, 1, Arc::new(RegisterSource::Input { p_start: *index }))
            }
            LinearOp::TensorLoad {
                dst_start,
                input: crate::TensorInputKind::P,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
            } => self.inventory.install(
                *dst_start,
                *count,
                Arc::new(RegisterSource::Input {
                    p_start: *input_start,
                }),
            ),
            _ => {}
        }
    }

    pub(super) fn call_results(
        &mut self,
        position: usize,
        dst_start: Reg,
        site: &crate::SolvePureCallSite,
    ) -> Option<()> {
        let mut flat = 0u32;
        for output in site.outputs() {
            let value = output.value_type();
            let count = value.scalar_count();
            let end = flat.checked_add(count)?;
            if matches!(value.element_type(), crate::SolveScalarType::Integer(_)) {
                self.inventory.install(
                    dst_start.checked_add(flat)?,
                    count as usize,
                    Arc::new(RegisterSource::Result {
                        operation: position,
                        first_cell: flat,
                    }),
                );
                let cells = self.cells.entry(position).or_default();
                *cells = cells.union(&crate::IndexIntervals::range(flat as usize, end as usize));
            }
            flat = end;
        }
        Some(())
    }

    /// Refine a validated read by its bound sinks without expanding either set.
    pub(super) fn read(
        &mut self,
        selected: NativeInputSelection,
        bound: &[NativeInputSelection],
        inputs: &mut input_runs::NativeInputInventory,
    ) -> Option<()> {
        for piece in self.pieces(selected)? {
            self.read_piece(piece, bound, inputs)?;
        }
        Some(())
    }

    fn read_piece(
        &mut self,
        piece: RegisterSourcePiece,
        bound: &[NativeInputSelection],
        inputs: &mut input_runs::NativeInputInventory,
    ) -> Option<()> {
        let map = |selection: NativeInputSelection,
                   source_start: usize|
         -> Option<NativeInputSelection> {
            NativeInputSelection::checked(
                source_start.checked_add(selection.start.checked_sub(piece.selected.start)?)?,
                selection.count,
                selection.stride,
            )
        };
        let high = piece
            .selected
            .start
            .checked_add((piece.selected.count - 1).checked_mul(piece.selected.stride)?)?
            .checked_add(1)?;
        let exclusions = bound
            .iter()
            .filter_map(|selection| selection.clipped(piece.selected.start..high))
            .collect::<Vec<_>>();
        match piece.source.as_ref() {
            RegisterSource::Other => {}
            RegisterSource::Input { p_start } => {
                let first = p_start.checked_add(piece.source_offset)?;
                let check = NativeInputCheck {
                    selected: map(piece.selected, first)?,
                    bound: exclusions
                        .into_iter()
                        .map(|selection| map(selection, first))
                        .collect::<Option<Vec<_>>>()?,
                };
                if inputs.selects_lane(NativeOutputLane::Integer, check.selected) {
                    inputs.checks.push(check);
                } else if inputs.selects_lane(NativeOutputLane::Boolean, check.selected) {
                    inputs.reads.push(check);
                }
            }
            RegisterSource::Result {
                operation,
                first_cell,
            } => {
                // Result checks retain bound exclusions as well; the exact
                // publication sink can leave a cell's Real view unread.
                let first = (*first_cell as usize).checked_add(piece.source_offset)?;
                self.read_results
                    .entry(*operation)
                    .or_default()
                    .push(NativeInputCheck {
                        selected: map(piece.selected, first)?,
                        bound: exclusions
                            .into_iter()
                            .map(|selection| map(selection, first))
                            .collect::<Option<Vec<_>>>()?,
                    });
            }
        }
        Some(())
    }
}
