use super::render_solve_tests::{const_store_row, fixture_span};
use super::*;

#[test]
fn checked_row_views_borrow_exact_programs_and_retain_them_after_source_drop() {
    let block = solve::ScalarProgramBlock::with_source_span(
        vec![const_store_row(-0.0), const_store_row(3.5)],
        fixture_span("rows.mo")
            .require_provenance("row view fixture")
            .unwrap(),
    )
    .unwrap();
    let expected = serde_json::to_value(block.programs()).unwrap();
    let rows = Value::from_object(SolveRowsValue::from_block(&block));
    let owned = Value::from_object(SolveRowsValue::new(block.programs().to_vec()));
    assert_eq!(serde_json::to_value(&rows).unwrap(), expected);
    assert_eq!(
        serde_json::to_value(&rows).unwrap(),
        serde_json::to_value(&owned).unwrap()
    );
    for (index, program) in block.programs().iter().enumerate() {
        let row = rows.get_item(&Value::from(index)).unwrap();
        let borrowed = row.downcast_object_ref::<SolveRowValue>().unwrap().ops();
        assert!(std::ptr::eq(borrowed, program.as_slice()));
    }
    assert!(rows.get_item(&Value::from(2)).unwrap().is_undefined());
    let retained_row = rows.get_item(&Value::from(0)).unwrap();
    drop(block);
    drop(rows);
    assert_eq!(serde_json::to_value(&retained_row).unwrap(), expected[0]);
}

#[test]
fn empty_checked_row_view_preserves_empty_sequence_shape() {
    let rows = Value::from_object(SolveRowsValue::from_block(
        &solve::ScalarProgramBlock::default(),
    ));
    assert_eq!(rows.len(), Some(0));
    assert_eq!(serde_json::to_value(&rows).unwrap(), serde_json::json!([]));
    assert!(rows.get_item(&Value::from(0)).unwrap().is_undefined());
}
