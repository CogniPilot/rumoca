use super::*;

pub(super) fn validate_partitioned_owner(
    problem: &SolveProblem,
    row: usize,
    run: &EventIterationRun,
    storage: SolveVariableStorageRun,
    current_base: usize,
    range: (usize, usize),
    claims: &mut EventIterationClaims,
) -> Result<(), SolveProblemShapeContractError> {
    let (first_partition, partition_count) = range;
    if partition_count == 0 {
        return Err(event_iteration_contract(
            row,
            "event-iteration partition owner is empty",
        ));
    }
    let end = first_partition
        .checked_add(partition_count)
        .ok_or_else(|| {
            event_iteration_contract(row, "event-iteration partition range overflowed")
        })?;
    let partitions = problem
        .discrete
        .event_iteration_plan
        .partitions
        .get(first_partition..end)
        .ok_or_else(|| {
            event_iteration_contract(row, "event-iteration partition range is out of bounds")
        })?;
    let mut scalar_end = 0usize;
    for (ordinal, partition) in partitions.iter().enumerate() {
        if partition.scalar_start != scalar_end || partition.scalar_count == 0 {
            return Err(event_iteration_contract(
                row,
                "event-iteration partitions are not a nonempty ordered cover",
            ));
        }
        scalar_end = scalar_end
            .checked_add(partition.scalar_count)
            .ok_or_else(|| {
                event_iteration_contract(row, "event-iteration partition scalar range overflowed")
            })?;
        if scalar_end > storage.scalar_count {
            return Err(event_iteration_contract(
                row,
                "event-iteration partition exceeds its variable owner",
            ));
        }
        if matches!(partition.owner, EventIterationOwner::Partitioned { .. }) {
            return Err(event_iteration_contract(
                row,
                "event-iteration partitions cannot contain partitioned owners",
            ));
        }
        if !claims.partitions.insert(first_partition + ordinal) {
            return Err(event_iteration_contract(
                row,
                "event-iteration partition is claimed more than once",
            ));
        }
        let base = current_base
            .checked_add(partition.scalar_start)
            .ok_or_else(|| {
                event_iteration_contract(row, "event-iteration partition target range overflowed")
            })?;
        let leaf_storage = sliced_storage(storage, base, *partition, row)?;
        let leaf_run = EventIterationRun {
            owner: partition.owner,
            ..*run
        };
        validate_event_iteration_owner(problem, row, &leaf_run, leaf_storage, base, claims)?;
    }
    if scalar_end != storage.scalar_count {
        return Err(event_iteration_contract(
            row,
            "event-iteration partitions do not cover their variable owner",
        ));
    }
    problem
        .discrete
        .event_iteration_run_clock(run)
        .map_err(|detail| event_iteration_contract(row, detail))?;
    Ok(())
}

fn sliced_storage(
    storage: SolveVariableStorageRun,
    index: usize,
    partition: EventIterationPartition,
    row: usize,
) -> Result<SolveVariableStorageRun, SolveProblemShapeContractError> {
    let ScalarSlot::P { byte_offset, .. } = storage.base else {
        return Err(event_iteration_contract(
            row,
            "typed discrete coordinate is not P-backed",
        ));
    };
    let byte_offset = partition
        .scalar_start
        .checked_mul(layout::F64_BYTES)
        .and_then(|offset| byte_offset.checked_add(offset))
        .ok_or_else(|| {
            event_iteration_contract(row, "event-iteration partition byte range overflowed")
        })?;
    Ok(SolveVariableStorageRun {
        base: ScalarSlot::P { index, byte_offset },
        scalar_count: partition.scalar_count,
        ..storage
    })
}
