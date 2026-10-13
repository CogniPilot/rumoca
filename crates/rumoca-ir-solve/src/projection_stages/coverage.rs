//! Root coverage follows ordered correlated stages, never parallel row/Y sets.

use super::*;
use crate::{
    ComputeOutputImage, IndexIntervals, ScalarOutputSegment, ScalarProgramOutputPlacement,
    SolveProblem,
};
use std::ops::ControlFlow;

pub(crate) fn compact_source_rows(
    source: &crate::ComputeBlock,
) -> Result<Option<IndexIntervals>, SolveProblemShapeContractError> {
    let mut rows = IndexIntervals::default();
    let result = source
        .checked_source_view("continuous.implicit_rhs")?
        .visit_output_placements(|placement| {
            let image = match placement.image() {
                ComputeOutputImage::Contiguous(range) => {
                    IndexIntervals::range(range.start, range.end)
                }
                ComputeOutputImage::Explicit(indices) => {
                    IndexIntervals::of(indices.iter().copied())
                }
                ComputeOutputImage::ProgramRanges(entries) => {
                    let mut image = IndexIntervals::default();
                    for entry in *entries {
                        image = image.union(&placement_rows(entry));
                    }
                    image
                }
                ComputeOutputImage::Affine {
                    domain, output_map, ..
                } => {
                    let Ok(dense) =
                        crate::TensorOutputMap::dense_contiguous(output_map.start, domain)
                    else {
                        return ControlFlow::Break(());
                    };
                    if dense != **output_map {
                        return ControlFlow::Break(());
                    }
                    let Ok(count) = domain.scalar_count() else {
                        return ControlFlow::Break(());
                    };
                    let Some(end) = output_map.start.checked_add(count) else {
                        return ControlFlow::Break(());
                    };
                    IndexIntervals::range(output_map.start, end)
                }
            };
            rows = rows.union(&image);
            ControlFlow::Continue(())
        })?;
    Ok(matches!(result, ControlFlow::Continue(())).then_some(rows))
}

fn placement_rows(entry: &ScalarProgramOutputPlacement) -> IndexIntervals {
    match entry {
        ScalarProgramOutputPlacement::Range(range) => IndexIntervals::range(range.start, range.end),
        ScalarProgramOutputPlacement::Explicit(indices) => {
            IndexIntervals::of(indices.iter().copied())
        }
        ScalarProgramOutputPlacement::Segments(segments) => {
            segments
                .iter()
                .fold(IndexIntervals::default(), |rows, segment| {
                    let image = match segment {
                        ScalarOutputSegment::Range(range) => {
                            IndexIntervals::range(range.start, range.end)
                        }
                        ScalarOutputSegment::Explicit(indices) => {
                            IndexIntervals::of(indices.iter().copied())
                        }
                    };
                    rows.union(&image)
                })
        }
    }
}

pub(crate) fn validate_root_inventory(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.continuous;
    let shared = system
        .implicit_row_targets
        .shares_projection_owner(&system.algebraic_projection_plan.blocks);
    let mut rows = IndexIntervals::default();
    let mut targets = IndexIntervals::default();
    for stage in system.algebraic_projection_plan.blocks.stages() {
        let (stage_rows, stage_targets) = checked_stage_inventory(problem, stage, shared)?;
        rows = rows.union(&stage_rows);
        targets = targets.union(&stage_targets);
    }
    if !shared {
        for row in 0..system.implicit_row_targets.len() {
            if system.implicit_row_targets.target_at(row).is_some() && !rows.contains(row) {
                return Err(stage_error(
                    "continuous implicit target has no correlated projection row",
                ));
            }
        }
    }
    let produced = match compact_source_rows(&system.implicit_rhs)? {
        Some(rows) => rows,
        None if system
            .algebraic_projection_plan
            .blocks
            .stages()
            .iter()
            .all(|stage| matches!(stage, ProjectionStage::Explicit(_))) =>
        {
            IndexIntervals::of(
                system
                    .implicit_rhs
                    .produced_output_indices("continuous.implicit_rhs")?,
            )
        }
        None => {
            return Err(stage_error(
                "admitted compact projection source has an unsupported output image",
            ));
        }
    };
    let state_count = problem.solve_layout.state_scalar_count();
    let required_end = state_count
        .checked_add(problem.solve_layout.algebraic_scalar_count())
        .ok_or_else(stage_overflow)?;
    if produced != rows
        || !IndexIntervals::range(state_count, required_end).is_subset(&targets)
        || !targets.is_subset(&IndexIntervals::range(
            state_count,
            problem.solve_layout.solver_scalar_count(),
        ))
    {
        return Err(stage_error(&format!(
            "continuous implicit produced outputs {produced:?} and correlated projection inventory {rows:?}/{targets:?} do not cover required algebraic Y {state_count}..{required_end}"
        )));
    }
    Ok(())
}

fn checked_stage_inventory(
    problem: &SolveProblem,
    stage: &ProjectionStage,
    shared: bool,
) -> Result<(IndexIntervals, IndexIntervals), SolveProblemShapeContractError> {
    let system = &problem.continuous;
    Ok(match stage {
        ProjectionStage::Explicit(block) => {
            for (&row, &target) in block.rows.iter().zip(&block.y_indices) {
                if system.implicit_row_targets.target_at(row) != Some(crate::scalar_slot_y(target))
                {
                    return Err(stage_error(
                        "continuous implicit projection row/target pair disagrees with its target owner",
                    ));
                }
            }
            (
                IndexIntervals::of(block.rows.iter().copied()),
                IndexIntervals::of(block.y_indices.iter().copied()),
            )
        }
        ProjectionStage::SingletonDeclarationRun {
            first_row, storage, ..
        } => {
            if !shared {
                return Err(stage_error(
                    "ranged implicit targets do not share their actual projection owner",
                ));
            }
            stage
                .checked_native_run(&system.implicit_rhs, &problem.solve_layout, &problem.layout)
                .map_err(|error| stage_error(&error.to_string()))?;
            let ScalarSlot::Y { index, .. } = storage.base else {
                return Err(stage_error("projection run is not Y storage"));
            };
            let row_end = first_row
                .checked_add(storage.scalar_count)
                .ok_or_else(stage_overflow)?;
            let target_end = index
                .checked_add(storage.scalar_count)
                .ok_or_else(stage_overflow)?;
            (
                IndexIntervals::range(*first_row, row_end),
                IndexIntervals::range(index, target_end),
            )
        }
    })
}
