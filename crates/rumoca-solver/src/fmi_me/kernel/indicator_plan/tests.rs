//! Constructor tests for the resolved FMI event-indicator table.
//!
//! The plan is the component's whole reading of SPEC_0044 ME-EVENT-001, so the
//! ablations here are the ones a step-time projection used to absorb silently:
//! a source outside the runtime vectors, and an inventory that abandoned the
//! checked order.

use super::*;

use rumoca_ir_solve::fmi::FmiEventIndicatorSource;

const NO_DOMAINS: &[solve::RootZeroDomain] = &[];
const NO_TARGETS: &[Option<ScalarSlot>] = &[];

fn inputs<'model>(
    model_root_count: usize,
    delay_count: usize,
    deadline_count: usize,
    root_zero_domains: &'model [solve::RootZeroDomain],
    root_relation_memory_targets: &'model [Option<ScalarSlot>],
) -> IndicatorPlanInputs<'model> {
    IndicatorPlanInputs {
        root_value_count: model_root_count + delay_count,
        model_root_count,
        deadline_count,
        root_zero_domains,
        root_relation_memory_targets,
    }
}

#[test]
fn an_empty_inventory_reads_neither_runtime_vector() {
    let plan = FmiIndicatorPlan::derive(&[], inputs(4, 2, 3, NO_DOMAINS, NO_TARGETS))
        .expect("an empty inventory resolves");

    assert_eq!(plan.len(), 0);
    assert_eq!(plan.root_value_len(), 0);
    assert_eq!(plan.deadline_len(), 0);
    assert!(!plan.reads_root_values());
    assert!(!plan.reads_deadlines());
}

#[test]
fn a_root_only_inventory_never_reads_the_deadline_rows() {
    let plan = FmiIndicatorPlan::derive(
        &[FmiEventIndicatorSource::RootCondition { index: 1 }],
        inputs(3, 0, 2, NO_DOMAINS, NO_TARGETS),
    )
    .expect("a root-only inventory resolves");

    assert!(plan.reads_root_values());
    assert_eq!(plan.root_value_len(), 3);
    assert!(
        !plan.reads_deadlines(),
        "a model with deadline rows but no deadline indicator must not settle for one"
    );
    assert_eq!(plan.deadline_len(), 0);
}

#[test]
fn a_deadline_only_inventory_never_reads_the_root_vector() {
    let plan = FmiIndicatorPlan::derive(
        &[FmiEventIndicatorSource::DynamicTimeEvent { index: 0 }],
        inputs(3, 0, 2, NO_DOMAINS, NO_TARGETS),
    )
    .expect("a deadline-only inventory resolves");

    assert!(!plan.reads_root_values());
    assert_eq!(plan.root_value_len(), 0);
    assert!(plan.reads_deadlines());
    assert_eq!(plan.deadline_len(), 2);
}

#[test]
fn delay_sources_read_the_root_vector_after_the_model_root_prefix() {
    let plan = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::RootCondition { index: 0 },
            FmiEventIndicatorSource::DynamicTimeEvent { index: 1 },
            FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
            FmiEventIndicatorSource::DelayDiscontinuity { index: 1 },
        ],
        inputs(2, 2, 2, NO_DOMAINS, NO_TARGETS),
    )
    .expect("the checked inventory order resolves");

    let readings = plan
        .entries()
        .iter()
        .map(IndicatorEntry::reading)
        .collect::<Vec<_>>();
    assert_eq!(
        readings,
        vec![
            IndicatorReading::RootValue { index: 0 },
            IndicatorReading::DeadlineDistance { index: 1 },
            IndicatorReading::RootValue { index: 2 },
            IndicatorReading::RootValue { index: 3 },
        ]
    );
    assert_eq!(plan.crossing_root_index(0), Some(0));
    assert_eq!(
        plan.crossing_root_index(1),
        None,
        "a deadline event belongs to the time schedule, not to relation memory"
    );
    assert_eq!(plan.crossing_root_index(2), Some(2));
    assert_eq!(plan.crossing_root_index(3), Some(3));
}

#[test]
fn the_checked_zero_domain_decides_each_root_position_once() {
    let domains = [
        solve::RootZeroDomain::Positive,
        solve::RootZeroDomain::NonPositive,
        solve::RootZeroDomain::Previous,
    ];
    let plan = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::RootCondition { index: 0 },
            FmiEventIndicatorSource::RootCondition { index: 1 },
            FmiEventIndicatorSource::RootCondition { index: 2 },
            FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
        ],
        inputs(3, 1, 0, &domains, NO_TARGETS),
    )
    .expect("typed zero domains resolve");

    let sides = plan
        .entries()
        .iter()
        .map(IndicatorEntry::zero_side)
        .collect::<Vec<_>>();
    assert_eq!(
        sides,
        vec![
            IndicatorZeroSide::Positive,
            IndicatorZeroSide::NonPositive,
            IndicatorZeroSide::Frozen,
            IndicatorZeroSide::Frozen,
        ]
    );
}

#[test]
fn relation_memory_targets_stay_positional() {
    let targets = [
        None,
        Some(ScalarSlot::P {
            index: 7,
            byte_offset: 0,
        }),
    ];
    let plan = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::RootCondition { index: 1 },
            FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
        ],
        inputs(2, 1, 0, NO_DOMAINS, &targets),
    )
    .expect("relation-memory targets resolve");

    assert_eq!(
        plan.relation_memory_targets(),
        [
            Some(ScalarSlot::P {
                index: 7,
                byte_offset: 0
            }),
            None
        ]
    );
    assert_eq!(
        plan.relation_memory_target(0),
        Some(ScalarSlot::P {
            index: 7,
            byte_offset: 0
        })
    );
    assert_eq!(plan.relation_memory_target(1), None);
}

#[test]
fn a_root_source_outside_the_model_root_prefix_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[FmiEventIndicatorSource::RootCondition { index: 2 }],
        inputs(2, 4, 0, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("a root source may not reach into the delay suffix");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 0,
            rejection: IndicatorPlanRejection::RootIndexOutOfRange,
        }
    );
}

#[test]
fn a_deadline_source_outside_the_deadline_block_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[FmiEventIndicatorSource::DynamicTimeEvent { index: 1 }],
        inputs(0, 0, 1, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("a deadline source may not leave its block");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 0,
            rejection: IndicatorPlanRejection::DeadlineIndexOutOfRange,
        }
    );
}

#[test]
fn a_delay_source_outside_the_root_vector_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[FmiEventIndicatorSource::DelayDiscontinuity { index: 1 }],
        inputs(2, 1, 0, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("a delay source may not leave the runtime root vector");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 0,
            rejection: IndicatorPlanRejection::DelayIndexOutOfRange,
        }
    );
}

#[test]
fn a_reordered_inventory_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
            FmiEventIndicatorSource::RootCondition { index: 0 },
        ],
        inputs(2, 1, 0, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("kinds must arrive in the checked inventory order");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 1,
            rejection: IndicatorPlanRejection::SourceKindOutOfOrder,
        }
    );
}

#[test]
fn a_duplicated_inventory_source_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::RootCondition { index: 1 },
            FmiEventIndicatorSource::RootCondition { index: 1 },
        ],
        inputs(2, 0, 0, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("one root may occupy only one FMI position");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 1,
            rejection: IndicatorPlanRejection::SourceIndexNotAscending,
        }
    );
}

#[test]
fn a_descending_inventory_source_is_refused() {
    let error = FmiIndicatorPlan::derive(
        &[
            FmiEventIndicatorSource::RootCondition { index: 1 },
            FmiEventIndicatorSource::RootCondition { index: 0 },
        ],
        inputs(2, 0, 0, NO_DOMAINS, NO_TARGETS),
    )
    .expect_err("root sources arrive strictly ascending");

    assert_eq!(
        error,
        IndicatorPlanError {
            position: 1,
            rejection: IndicatorPlanRejection::SourceIndexNotAscending,
        }
    );
}
