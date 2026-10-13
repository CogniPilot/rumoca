//! Replay a correlated projection claim through the existing ranged isolator.

use super::*;
use crate::{ProjectionStage, SolveLayout};

pub(crate) fn projection_run_source(
    source: &ComputeBlock,
    rows: Range<usize>,
) -> Result<Option<crate::RefreshScalarProgramSource>, crate::SolveProblemShapeContractError> {
    source.checked_source_view("singleton projection source")?;
    let outputs = super::super::source_outputs::SourceOutputs::new(source).map_err(|error| {
        crate::SolveProblemShapeContractError::ContinuousRefreshOwner {
            detail: error.to_string(),
        }
    })?;
    for (node, owner) in source.nodes.iter().enumerate() {
        let ComputeNode::ScalarPrograms(programs) = owner else {
            continue;
        };
        for program in 0..programs.row_count() {
            let Some(selector) = crate::RefreshScalarProgramSource::checked(node, program) else {
                return Err(
                    crate::SolveProblemShapeContractError::ContinuousRefreshOwner {
                        detail: "projection source identity overflows".into(),
                    },
                );
            };
            let count = programs
                .program(program)
                .ok_or_else(
                    || crate::SolveProblemShapeContractError::ContinuousRefreshOwner {
                        detail: "projection source program does not resolve".into(),
                    },
                )?
                .len();
            if (0..count).any(|position| {
                outputs.store_contiguous_range(selector, position) == Some(rows.clone())
            }) {
                return Ok(Some(selector));
            }
        }
    }
    Ok(None)
}

pub(crate) fn derive_projection_run(
    stage: &ProjectionStage,
    source: &ComputeBlock,
    declarations: &SolveLayout,
    layout: &VarLayout,
) -> Checked<NativeRefreshAssignmentStage> {
    let ProjectionStage::SingletonDeclarationRun {
        source: selector,
        first_row,
        declaration,
        storage,
    } = stage
    else {
        return refused("native ranged projection requires a singleton declaration run");
    };
    source
        .checked_source_view("singleton projection source")
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native source"))?;
    let ComputeNode::ScalarPrograms(programs) =
        source
            .nodes
            .get(selector.node() as usize)
            .ok_or(NativeRefreshAssignmentRefusal(
                "projection source node is absent",
            ))?
    else {
        return refused("projection run source is not a scalar program owner");
    };
    let program = selector.program() as usize;
    let operations = programs
        .program(program)
        .ok_or(NativeRefreshAssignmentRefusal(
            "projection source program is absent",
        ))?;
    let span =
        programs
            .program_spans()
            .get(program)
            .copied()
            .ok_or(NativeRefreshAssignmentRefusal(
                "projection source program has no provenance",
            ))?;
    let end = first_row
        .checked_add(storage.scalar_count)
        .ok_or(NativeRefreshAssignmentRefusal(
            "projection source row range overflows",
        ))?;
    let outputs = super::super::source_outputs::SourceOutputs::new(source)
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native output inventory"))?;
    let mut stores = (0..operations.len()).filter(|position| {
        outputs.store_contiguous_range(*selector, *position) == Some(*first_row..end)
    });
    let position = stores.next().ok_or(NativeRefreshAssignmentRefusal(
        "projection source does not own the complete claimed row run",
    ))?;
    if stores.next().is_some() {
        return refused("projection source row run has multiple original store owners");
    }
    let family =
        range::derive_selected_store(
            *selector,
            *first_row..end,
            operations,
            position,
            span,
            layout,
            || {
                if declarations.variable_storage_runs.get(*declaration) != Some(storage) {
                    return refused("projection storage belongs to another declaration or layout");
                }
                let typed = declarations.variable_declarations.get(*declaration).ok_or(
                    NativeRefreshAssignmentRefusal("projection declaration is absent"),
                )?;
                if typed.role() != storage.role || typed.value_kind() != storage.value_kind {
                    return refused("projection storage disagrees with its typed declaration");
                }
                let crate::ScalarSlot::Y { index, byte_offset } = storage.base else {
                    return refused("projection declaration is not solver Y storage");
                };
                if index.checked_mul(8) != Some(byte_offset)
                    || index < declarations.state_scalar_count()
                {
                    return refused("projection storage has an invalid algebraic Y base");
                }
                let end = index.checked_add(storage.scalar_count).ok_or(
                    NativeRefreshAssignmentRefusal("projection target range overflows"),
                )?;
                Ok(index..end)
            },
        )?;
    Ok(family.stage)
}
