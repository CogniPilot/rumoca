use super::*;

fn key(field: Option<usize>, scalar: usize) -> FunctionParameterDependency {
    match field {
        Some(field) => FunctionParameterDependency::RecordField {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            field,
            scalar,
        },
        None => FunctionParameterDependency::Scalar {
            activation: crate::projection::Activation::Guaranteed,
            parameter: 0,
            scalar,
        },
    }
}

// Independent literal replay: no chunk identity or inventory cache shortcuts.
fn dense(pieces: &[Piece]) -> Vec<FunctionParameterDependency> {
    let mut values = Vec::new();
    for piece in pieces {
        let incoming = match piece {
            Piece::Scalar(value) => std::slice::from_ref(value),
            Piece::Completed(values) => values.as_ref(),
        };
        for value in incoming {
            if !values.contains(value) {
                values.push(value.clone());
            }
        }
    }
    values
}

#[test]
fn mixed_completed_fragments_match_literal_replay_and_keep_record_field_identity() {
    let a: Dependencies = vec![key(Some(1), 2), key(None, 3), key(Some(0), 2)].into();
    let b: Dependencies = vec![key(None, 3), key(None, 1), key(Some(1), 2)].into();
    let mut recording = Recording::default();
    recording.scalar(&key(None, 1));
    recording.completed(&a);
    recording.scalar(&key(Some(0), 2));
    recording.completed(&b);
    recording.completed(&a);
    let expected = dense(&[
        Piece::Scalar(key(None, 1)),
        Piece::Completed(Arc::clone(&a)),
        Piece::Scalar(key(Some(0), 2)),
        Piece::Completed(b),
        Piece::Completed(a),
    ]);
    let actual = Inventories::default().finish(recording);
    assert_eq!(actual.as_ref(), expected);
}

#[test]
fn equal_contents_in_distinct_live_fragments_keep_exact_order_without_owner_aliasing() {
    let a: Dependencies = vec![key(None, 2), key(None, 0)].into();
    let b: Dependencies = vec![key(None, 2), key(None, 0)].into();
    assert!(!Arc::ptr_eq(&a, &b));
    let mut recording = Recording::default();
    recording.completed(&a);
    recording.scalar(&key(Some(1), 0));
    recording.completed(&b);
    assert_eq!(
        recording.pieces.len(),
        3,
        "different live fragments are never identified as one owner"
    );
    let expected = dense(&recording.pieces);
    assert_eq!(Inventories::default().finish(recording).as_ref(), expected);
}

#[test]
fn full_14400_completed_sequences_materialize_once_and_empty_captures_stay_empty() {
    let raster: Dependencies = (0..14400)
        .rev()
        .map(|scalar| key(None, scalar))
        .collect::<Vec<_>>()
        .into();
    let mut inventories = Inventories::default();
    let mut initial = Recording::default();
    initial.completed(&raster);
    let expected = inventories.finish(initial);
    assert_eq!(expected.as_ref(), raster.as_ref());
    for _ in 0..32 {
        let mut repeated = Recording::default();
        repeated.completed(&raster);
        repeated.completed(&raster);
        assert!(Arc::ptr_eq(&inventories.finish(repeated), &expected));
    }
    assert_eq!(inventories.flattened_occurrences, 14400);
    let mut empty = Recording::default();
    empty.completed(&Arc::from([]));
    assert!(inventories.finish(empty).is_empty());
    assert_eq!(inventories.flattened_occurrences, 14400);
}
