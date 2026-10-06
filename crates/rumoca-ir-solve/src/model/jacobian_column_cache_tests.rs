//! Unrun review controls for an exact, demand-materialized derived view.
use super::*;

fn full(rows: usize, columns: usize) -> StructuralPattern {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("lazy_column_cache.mo"),
        1,
        2,
    );
    StructuralPattern::full(
        rows,
        columns,
        PatternProvenance::derived(PatternDerivation::ConservativeFull, span).unwrap(),
    )
    .unwrap()
}

#[test]
fn unused_full_structure_has_no_column_coordinate_cache() {
    let structure = JacobianStructure::derived(full(128, 192));
    assert!(structure.column_rows.get().is_none());
    assert_eq!(structure.pattern().rows(), 128);
    assert_eq!(structure.pattern().columns(), 192);
    assert_eq!(structure.coloring().groups().len(), 192);
}

#[test]
fn full_cache_keeps_exact_rectangular_rows_and_stable_borrow() {
    let structure = JacobianStructure::derived(full(3, 5));
    let rows = structure.column_rows();
    assert_eq!(rows, vec![vec![0, 1, 2]; 5]);
    let pointer = rows.as_ptr();
    assert_eq!(pointer, structure.column_rows().as_ptr());
    assert_eq!(structure.column_rows.get().unwrap().len(), 5);
}

#[test]
fn empty_full_columns_retain_the_source_dimension() {
    let structure = JacobianStructure::derived(full(0, 4));
    assert!(structure.column_rows.get().is_none());
    assert_eq!(structure.column_rows(), vec![Vec::<usize>::new(); 4]);
    let no_columns = JacobianStructure::derived(full(3, 0));
    assert!(no_columns.column_rows().is_empty());
}

#[test]
fn clone_preserves_cold_and_warm_owned_cache_lifetimes() {
    let source = JacobianStructure::derived(full(3, 5));
    let cold = source.clone();
    assert!(source.column_rows.get().is_none());
    assert!(cold.column_rows.get().is_none());
    let source_rows = source.column_rows();
    assert!(cold.column_rows.get().is_none());
    let warm = source.clone();
    assert!(warm.column_rows.get().is_some());
    assert_eq!(source_rows, warm.column_rows());
    assert_ne!(source_rows.as_ptr(), warm.column_rows().as_ptr());
    assert_ne!(source_rows[0].as_ptr(), warm.column_rows()[0].as_ptr());
    assert_eq!(source_rows, cold.column_rows());
    assert_ne!(source_rows.as_ptr(), cold.column_rows().as_ptr());
}

#[test]
fn simultaneous_consumers_observe_one_exact_immutable_cache() {
    let structure = std::sync::Arc::new(JacobianStructure::derived(full(3, 5)));
    let mut tasks = Vec::new();
    for _ in 0..8 {
        let owner = std::sync::Arc::clone(&structure);
        tasks.push(std::thread::spawn(move || {
            let rows = owner.column_rows();
            assert_eq!(rows, vec![vec![0, 1, 2]; 5]);
            rows.as_ptr() as usize
        }));
    }
    let addresses: Vec<_> = tasks.into_iter().map(|task| task.join().unwrap()).collect();
    assert!(addresses.iter().all(|address| *address == addresses[0]));
    assert!(structure.column_rows.get().is_some());
}
