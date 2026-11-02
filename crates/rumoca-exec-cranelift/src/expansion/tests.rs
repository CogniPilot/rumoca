use super::*;
use rumoca_ir_solve::LinearOp;
use std::panic::{AssertUnwindSafe, catch_unwind};

fn begin(name: &'static str, count: u64, instructions: usize) -> Option<Opened> {
    open(
        ExpansionSource::Row(LinearOp::StoreOutputRange {
            start: 0,
            count: count as usize,
            stride: 1,
        }),
        name,
        None,
        count,
        (1, instructions),
    )
}

#[test]
fn nested_instructions_and_capacity_copies_keep_distinct_units() {
    let ((), report) = record_expansion_report(|| {
        let outer = begin("Map", 64, 10);
        moved(512, "memmove", 1);
        let loop_scope = loop_scope(64, 1);
        let inner = begin("UpdateElement", 128, 14);
        moved(128, "memmove", 1);
        finish(inner, 20, true);
        drop(loop_scope);
        finish(outer, 30, true);
    });
    assert!(report.complete);
    assert_eq!(report.entries[0].instructions, 14);
    assert_eq!(report.entries[1].instructions, 6);
    assert_eq!(report.entries[1].parent, Some(0));
    assert_eq!(report.entries[0].copies[0].capacity_cells(), Some(512));
    assert_eq!(report.entries[1].copies[0].capacity_cells(), Some(8192));
    assert_eq!(report.entries[1].copies[0].loop_bounds, [64]);
}

#[test]
fn nested_collections_are_isolated_and_restore_the_parent() {
    let (inner, outer) = record_expansion_report(|| {
        let token = begin("Outer", 64, 0);
        let ((), inner) = record_expansion_report(|| {
            let token = begin("Inner", 64, 0);
            finish(token, 8, true);
        });
        moved(32, "memmove", 1);
        finish(token, 6, true);
        inner
    });
    assert_eq!(outer.entries.len(), 1);
    assert_eq!(inner.entries.len(), 1);
    assert_eq!(outer.entries[0].kind, "Outer");
    assert_eq!(outer.entries[0].copies[0].cells, 32);
    assert_eq!(inner.entries[0].kind, "Inner");
}

#[test]
fn panic_restores_the_parent_and_does_not_leak_into_the_next_report() {
    let ((), outer) = record_expansion_report(|| {
        let token = begin("Outer", 1, 0);
        assert!(
            catch_unwind(AssertUnwindSafe(|| record_expansion_report(|| {
                let _token = begin("Panic", 1, 0);
                let _loop_scope = loop_scope(64, 1);
                panic!("control");
            })))
            .is_err()
        );
        finish(token, 4, true);
    });
    assert!(outer.complete);
    assert_eq!(outer.entries.len(), 1);
    let ((), after) = record_expansion_report(|| {
        let token = begin("After", 1, 0);
        finish(token, 1, true);
    });
    assert!(after.complete);
    assert!(after.entries[0].loop_bounds.is_empty());
}

#[test]
fn failure_and_unclosed_operations_cannot_be_a_complete_report() {
    let ((), failed) = record_expansion_report(|| {
        let token = begin("Failed", 1, 0);
        finish(token, 5, false);
    });
    assert!(!failed.complete);
    assert!(!failed.entries[0].succeeded);
    let ((), abandoned) = record_expansion_report(|| {
        let _ = begin("Open", 1, 0);
    });
    assert!(!abandoned.complete);
}

#[test]
fn independent_threads_and_unscoped_children_never_cross_collect() {
    let threads: Vec<_> = (0..4)
        .map(|i| {
            std::thread::spawn(move || {
                record_expansion_report(|| {
                    let token = begin("Thread", 100 + i, 0);
                    let absent = std::thread::spawn(|| begin("Unscoped", 1, 0).is_none());
                    assert!(absent.join().unwrap());
                    finish(token, i as usize + 1, true);
                })
                .1
            })
        })
        .collect();
    for (i, thread) in threads.into_iter().enumerate() {
        let report = thread.join().unwrap();
        assert!(report.complete);
        assert_eq!(report.entries.len(), 1);
        assert_eq!(report.entries[0].elements, 100 + i as u64);
    }
}

#[test]
fn capacity_overflow_is_unknown_and_static_work_is_explicit() {
    let ((), report) = record_expansion_report(|| {
        let token = begin("MatrixMultiply", 51, 0);
        forced(ForcedExpansion::StaticUnroll { limit: 64 });
        let _scope = loop_scope(u64::MAX, 1);
        moved(2, "memmove", 1);
        finish(token, 300, true);
    });
    assert!(report.entries[0].is_suspect());
    assert!(!report.entries[0].is_unforced_suspect());
    assert_eq!(report.entries[0].copies[0].capacity_cells(), None);
}

#[test]
fn no_active_scope_means_no_recording() {
    assert!(begin("Absent", 128, 0).is_none());
    moved(128, "memmove", 1);
    let ((), report) = record_expansion_report(|| {});
    assert!(report.complete);
    assert!(report.entries.is_empty());
}

#[test]
fn another_functions_loop_cannot_multiply_a_callers_copy() {
    let ((), report) = record_expansion_report(|| {
        let token = begin("Caller", 128, 0);
        let _other_loop = loop_scope(64, 2);
        moved(128, "memmove", 1);
        moved(128, "unattributed-callee-copy", 2);
        finish(token, 12, true);
    });
    assert!(!report.complete);
    assert_eq!(report.unattributed_copy_sites, 1);
    assert_eq!(report.entries[0].copies.len(), 1);
    assert_eq!(report.entries[0].copies[0].capacity_cells(), Some(128));
}

fn source_table() -> rumoca_ir_solve::SolvePureCallTable {
    use rumoca_ir_solve::*;
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let at = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("table_scope.mo"),
        1,
        2,
    );
    SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![],
            vec![SolvePureCallOutput::result(SolveValueType::scalar(
                SolveScalarType::real(profile),
            ))],
            at,
            |b, _, outputs| {
                let value = b.constant(SolveValue::real(profile, 1.0), at)?;
                b.store(outputs[0], value, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn table_scopes_restore_and_equal_declarations_do_not_replace_owner_identity() {
    let a = source_table();
    let b = source_table();
    assert!(!a.shares_table_owner(&b));
    let ((), report) = record_expansion_report(|| {
        let _a = call_table_scope(Some(&a));
        let first = begin("A", 1, 0);
        finish(first, 1, true);
        {
            let _b = call_table_scope(Some(&b));
            let second = begin("B", 1, 0);
            finish(second, 1, true);
        }
        let last = begin("A-again", 1, 0);
        finish(last, 1, true);
    });
    assert!(report.complete);
    assert_eq!(report.tables.len(), 2);
    assert!(report.tables[0].shares_table_owner(&a));
    assert!(report.tables[1].shares_table_owner(&b));
    assert_eq!(
        report
            .entries
            .iter()
            .map(|entry| entry.call_table)
            .collect::<Vec<_>>(),
        [Some(0), Some(1), Some(0)]
    );
}

#[test]
fn a_known_refused_probe_is_retained_without_becoming_a_runtime_success() {
    let ((), report) = record_expansion_report(|| {
        let token = begin("Unsupported", 128, 0);
        phase("admission-probe");
        finish(token, 12, false);
    });
    assert!(report.complete);
    assert_eq!(report.entries.len(), 1);
    assert!(!report.entries[0].succeeded);
    assert_eq!(report.entries[0].phase, "admission-probe");
}
