//! The kinematic rows of the complete operation over its settled coordinate.
//!
//! A partitioned step evaluates the kinematic state derivatives again after
//! the other states moved, over the algebraics the last full evaluation
//! settled (see `structural_pattern::kinematic_states`). This issues the
//! derivative values restricted to those rows: every scalar program that
//! stores a kinematic row, and every tensor node with a kinematic cell, whole.
//! A node is evaluated exactly as the complete values evaluate it, so each
//! kept output has the bits the complete operation would give it; the other
//! outputs of a kept tensor node are evaluated and not read.
use super::*;
use crate::{ComputeNode, IndexIntervals};
use std::collections::BTreeSet;

/// The issued kinematic selection of one complete causal operation.
#[derive(Debug)]
pub struct FmiKinematicDerivativeOperation {
    rows: IndexIntervals,
    values: ComputeBlock,
}

/// Why a complete causal operation issues no kinematic entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FmiKinematicRefusal {
    /// The partition has no kinematic state.
    NoKinematicStates,
    /// Every state is kinematic: a partitioned step is one explicit step.
    EveryStateKinematic,
    /// A kinematic row is placed by no node of the derivative values.
    UnplacedRow,
    /// Dropping the other rows would move a node placed after the nodes
    /// before it.
    MovedPlacement,
    /// The derivative values' placements could not be read or rebuilt.
    Placement,
}

impl std::fmt::Display for FmiKinematicRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::NoKinematicStates => "the kinematic partition is empty",
            Self::EveryStateKinematic => "every state is kinematic",
            Self::UnplacedRow => "a kinematic row has no placed derivative value",
            Self::MovedPlacement => "dropping the other rows would move a placed derivative value",
            Self::Placement => "the derivative value placements could not be selected",
        })
    }
}

impl FmiKinematicDerivativeOperation {
    /// The selection fails closed: a source it cannot place issues no
    /// operation, with the reason.
    pub(super) fn issue(
        source: &FmiDerivativeOperationSource<'_>,
    ) -> Result<Self, FmiKinematicRefusal> {
        let kinematic = source
            .model
            .artifacts
            .continuous
            .structural
            .kinematic_states();
        if kinematic.is_empty() {
            return Err(FmiKinematicRefusal::NoKinematicStates);
        }
        if kinematic.len() >= source.vector_extent() {
            return Err(FmiKinematicRefusal::EveryStateKinematic);
        }
        let rows = kinematic.iter().copied().collect::<BTreeSet<_>>();
        let original = node_images(source.values()).map_err(|_| FmiKinematicRefusal::Placement)?;
        let mut nodes = Vec::new();
        let mut kept_images = Vec::new();
        for (node, image) in source.values().nodes.iter().zip(&original) {
            if image.is_disjoint(&rows) {
                continue;
            }
            let ComputeNode::ScalarPrograms(block) = node else {
                nodes.push(node.clone());
                kept_images.push(image.clone());
                continue;
            };
            let (programs, image) =
                kinematic_programs(block, image, &rows).ok_or(FmiKinematicRefusal::Placement)?;
            nodes.push(ComputeNode::ScalarPrograms(programs));
            kept_images.push(image);
        }
        if !kept_images
            .iter()
            .flatten()
            .copied()
            .collect::<BTreeSet<_>>()
            .is_superset(&rows)
        {
            return Err(FmiKinematicRefusal::UnplacedRow);
        }
        let values = ComputeBlock { nodes };
        // A matrix or dense node is placed after the nodes before it; dropping
        // those must leave every kept output where the complete values put it.
        if node_images(&values).map_err(|_| FmiKinematicRefusal::Placement)? != kept_images {
            return Err(FmiKinematicRefusal::MovedPlacement);
        }
        Ok(Self {
            rows: IndexIntervals::of(kinematic.iter().copied()),
            values,
        })
    }

    /// The kinematic states, ascending; the only outputs a caller may read.
    pub fn rows(&self) -> &IndexIntervals {
        &self.rows
    }

    /// The restricted values, placed in the complete operation's output order.
    pub fn values(&self) -> &ComputeBlock {
        &self.values
    }
}

fn node_images(
    block: &ComputeBlock,
) -> Result<Vec<BTreeSet<usize>>, crate::SolveProblemShapeContractError> {
    const CONTEXT: &str = "FMI kinematic derivative operation";
    let mut images = Vec::new();
    let visited = block
        .checked_source_view(CONTEXT)?
        .visit_output_placements(|placement| {
            let mut image = BTreeSet::new();
            match placement.append_indices(&mut image, CONTEXT) {
                Ok(()) => {
                    images.push(image);
                    std::ops::ControlFlow::Continue(())
                }
                Err(error) => std::ops::ControlFlow::Break(error),
            }
        })?;
    match visited {
        std::ops::ControlFlow::Continue(()) => Ok(images),
        std::ops::ControlFlow::Break(error) => Err(error),
    }
}

/// The programs of `block` that store a kinematic row, each with all of its
/// outputs, at the absolute positions `image` gives them.
fn kinematic_programs(
    block: &crate::ScalarProgramBlock,
    image: &BTreeSet<usize>,
    rows: &BTreeSet<usize>,
) -> Option<(crate::ScalarProgramBlock, BTreeSet<usize>)> {
    let absolute = match block.output_placements() {
        crate::ScalarOutputPlacementView::Dense { count } => {
            let start = *image.first()?;
            (start..start.checked_add(count)?).collect::<Vec<_>>()
        }
        _ => block.output_indices().to_vec(),
    };
    let (mut programs, mut spans, mut outputs) = (Vec::new(), Vec::new(), Vec::new());
    for (index, program) in block.programs().iter().enumerate() {
        let stored = absolute.get(block.program_output_ordinals(index)?)?;
        if stored.iter().any(|output| rows.contains(output)) {
            programs.push(program.clone());
            spans.push(*block.program_spans().get(index)?);
            outputs.extend_from_slice(stored);
        }
    }
    let kept = outputs.iter().copied().collect();
    let programs = crate::ScalarProgramBlock::with_output_indices(programs, spans, outputs).ok()?;
    Some((programs, kept))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LinearOp, ScalarProgramBlock};

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fmi_kinematic_operation.mo"),
            0,
            1,
        )
    }

    fn constant(values: &[f64]) -> Vec<LinearOp> {
        let mut program = Vec::new();
        for (register, &value) in values.iter().enumerate() {
            program.push(LinearOp::Const {
                dst: register as u32,
                value,
            });
        }
        for register in 0..values.len() {
            program.push(LinearOp::StoreOutput {
                src: register as u32,
            });
        }
        program
    }

    #[test]
    fn a_program_is_kept_whole_at_its_placement_when_it_stores_a_kinematic_row() {
        // Dense outputs placed after two earlier outputs: 2, then 3 and 4
        // from one program, then 5.
        let block = ScalarProgramBlock::with_program_spans(
            vec![constant(&[1.0]), constant(&[2.0, 3.0]), constant(&[4.0])],
            vec![span(); 3],
        )
        .unwrap();
        let image = (2..6).collect::<BTreeSet<_>>();
        let rows = [4].into_iter().collect::<BTreeSet<_>>();
        let (kept, outputs) = kinematic_programs(&block, &image, &rows).unwrap();
        assert_eq!(kept.programs(), &[constant(&[2.0, 3.0])]);
        assert_eq!(kept.output_indices(), &[3, 4]);
        assert_eq!(outputs, [3, 4].into_iter().collect());
        let images = node_images(&ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(kept)],
        })
        .unwrap();
        assert_eq!(images, vec![outputs]);
    }
}
