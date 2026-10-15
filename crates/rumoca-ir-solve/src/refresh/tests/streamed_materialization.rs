use super::*;

fn product(start: u32, input_start: usize) -> Vec<LinearOp> {
    let load = |dst_start, input| LinearOp::TensorLoad {
        dst_start,
        input,
        input_start,
        count: 2,
        seed_start: None,
        lanes: 1,
    };
    vec![
        load(start, crate::TensorInputKind::Y),
        load(start + 2, crate::TensorInputKind::P),
        LinearOp::TensorBinary {
            dst_start: start + 4,
            op: crate::BinaryOp::Mul,
            lhs_start: start,
            rhs_start: start + 2,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: start + 4,
            count: 2,
            stride: 1,
        },
    ]
}

#[test]
fn streamed_tensor_isolators_retain_later_source_producers() {
    let prefix = product(0, 0);
    let first = derive_target_assignment_shape_for_output(&prefix, 0, 0).unwrap();
    assert!(matches!(first, TargetAssignmentShape::TensorAffine { .. }));
    let mut independent = prefix.clone();
    let expected_first = materialize_target_assignment(&first, &mut independent).unwrap();
    let later = product(1_000, 2);
    independent.extend(later.iter().cloned());
    let second = derive_target_assignment_shape_for_output(&independent, 2, 2).unwrap();
    assert!(matches!(second, TargetAssignmentShape::TensorAffine { .. }));
    let expected_second = materialize_target_assignment(&second, &mut independent).unwrap();

    let mut streamed = Vec::new();
    let mut builder = ExactAssignmentProgramBuilder::new(&mut streamed).unwrap();
    for operation in prefix {
        builder.push(operation).unwrap();
    }
    assert_eq!(builder.materialize_guarded(&first).unwrap(), expected_first);
    for operation in later {
        builder.push(operation).unwrap();
    }
    assert_eq!(
        builder.materialize_guarded(&second).unwrap(),
        expected_second
    );
    // Every operation, including original stores and guarded coefficient math,
    // agrees with independent materialization at each source boundary.
    assert_eq!(streamed, independent);
}
