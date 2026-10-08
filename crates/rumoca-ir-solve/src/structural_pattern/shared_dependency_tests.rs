//! Differential old BTreeSet allocation/value semantics for exact dependency state.
mod calls;
mod regions;

use super::*;
use std::cell::Cell;

thread_local! { static LEGACY: Cell<bool> = const { Cell::new(false) }; }

pub(super) fn legacy_enabled() -> bool {
    LEGACY.with(Cell::get)
}

struct LegacyGuard(bool);
impl Drop for LegacyGuard {
    fn drop(&mut self) {
        LEGACY.with(|value| value.set(self.0));
    }
}

fn legacy<T>(test: impl FnOnce() -> T) -> T {
    let old = LEGACY.with(|value| value.replace(true));
    let _guard = LegacyGuard(old);
    test()
}

fn span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("shared_dependencies.mo"),
        0,
        1,
    )
}

fn sources() -> [DependencySource; 5] {
    [
        DependencySource::Effect,
        DependencySource::Seed,
        DependencySource::SolverP,
        DependencySource::SolverY,
        DependencySource::Time,
    ]
}

fn compare_program(program: &[LinearOp]) {
    for source in sources() {
        compare_source(program, source);
    }
}

fn compare_source(program: &[LinearOp], source: DependencySource) {
    let old = legacy(|| {
        program_output_dependencies_with_fold(program, Some(span()), None, None, None, source)
    });
    let new =
        program_output_dependencies_with_fold(program, Some(span()), None, None, None, source);
    assert_eq!(format!("{old:?}"), format!("{new:?}"));
}

fn set(bits: usize) -> BTreeSet<usize> {
    (0..6).filter(|bit| bits & (1 << bit) != 0).collect()
}

#[test]
fn all_small_exact_unions_equal_literal_btree_reference() {
    for lhs in 0..64 {
        compare_right_sets(lhs);
    }
}

fn compare_right_sets(lhs: usize) {
    for rhs in 0..64 {
        let left = DependencyState::from_set(set(lhs));
        let alias = left.clone();
        let right = DependencyState::from_set(set(rhs));
        let mut expected = set(lhs);
        expected.extend(set(rhs));
        assert_eq!(left.union(right).into_set(), expected);
        assert_eq!(alias.into_set(), set(lhs));
    }
}

#[test]
fn full14400_empty_identity_subset_and_copy_on_write_keep_exact_aliases() {
    let original = DependencyState::from_set((0..14400).collect());
    let shared = shared_set(&original);
    let alias = original.clone();
    let clone = shared_set(&alias);
    assert!(Arc::ptr_eq(shared, clone));
    let same = original.clone().union(alias.clone());
    let empty = DependencyState::empty().union(same);
    let covered = empty.union(DependencyState::singleton(14399));
    let covered_set = shared_set(&covered);
    assert!(Arc::ptr_eq(shared, covered_set));
    let changed = covered.union(DependencyState::singleton(14400));
    assert_eq!(changed.into_set(), (0..14401).collect());
    assert_eq!(alias.into_set(), (0..14400).collect());
}

#[test]
fn all_five_source_categories_and_effects_equal_legacy_walk() {
    compare_program(&[
        LinearOp::LoadY { dst: 0, index: 7 },
        LinearOp::LoadP { dst: 1, index: 11 },
        LinearOp::LoadSeed { dst: 2, index: 13 },
        LinearOp::LoadTime { dst: 3 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::ImpureRandom {
            dst: 7,
            id: 6,
            call_site: 9,
        },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 7 },
    ]);
}

#[test]
fn invalid_registers_and_context_reads_preserve_first_error_and_span() {
    compare_program(&[
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Binary {
            dst: 1,
            op: BinaryOp::Add,
            lhs: 99,
            rhs: 100,
        },
    ]);
    compare_program(&[LinearOp::LoadFoldCarried { dst: 0, index: 0 }]);
    compare_program(&[LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 }]);
}

fn shared_set(state: &DependencyState) -> &Arc<crate::IndexIntervals> {
    match state {
        DependencyState::Known(indices) => indices,
        _ => panic!("full14400 fixture must own a shared set"),
    }
}

#[test]
fn small_states_have_allocation_free_variants_and_canonical_equality() {
    assert!(matches!(DependencyState::empty(), DependencyState::Empty));
    assert!(matches!(
        DependencyState::singleton(usize::MAX),
        DependencyState::Singleton(usize::MAX)
    ));
    assert_eq!(
        DependencyState::empty(),
        DependencyState::from_set(BTreeSet::new())
    );
    assert_eq!(
        DependencyState::singleton(7),
        DependencyState::from_set(BTreeSet::from([7]))
    );
    for bits in 0..64 {
        let state = DependencyState::from_set(set(bits));
        assert_eq!(state, DependencyState::from_set(state.clone().into_set()));
    }
}

#[test]
fn every_small_state_has_identical_presence_ordered_iteration_and_public_set() {
    for bits in 0..64 {
        let expected = set(bits);
        let state = DependencyState::from_set(expected.clone());
        assert_eq!(state.is_empty(), expected.is_empty());
        assert_eq!(
            state.elements().collect::<Vec<_>>(),
            expected.iter().copied().collect::<Vec<_>>()
        );
        assert_eq!(state.with_set(Clone::clone), expected);
        assert_eq!(state.into_set(), expected);
    }
}

#[test]
fn compact_singleton_copies_are_isolated_from_expanding_unions() {
    let original = DependencyState::singleton(usize::MAX);
    let alias = original.clone();
    let expanded = original.union(DependencyState::singleton(0));
    assert_eq!(alias, DependencyState::singleton(usize::MAX));
    assert_eq!(expanded.elements().collect::<Vec<_>>(), vec![0, usize::MAX]);
    assert_eq!(expanded.clone().union(alias), expanded);
}
