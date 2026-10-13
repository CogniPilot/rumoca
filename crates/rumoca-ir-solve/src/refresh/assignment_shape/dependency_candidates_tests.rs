use super::*;

// Independent small-program oracle: enumerate every prefix Y load, including
// loads unrelated to the selected output. Production prunes using its checked
// output-reachable complete operation ownership instead.
fn exhaustive_shapes(program: &[LinearOp]) -> Vec<(usize, TargetAssignmentShape)> {
    let mut shapes = Vec::new();
    for (offset, (output, position)) in store_output_registers(program).enumerate() {
        let prefix = &program[..position];
        let Some(producers) = UniqueProgram::new(prefix) else {
            continue;
        };
        let dependencies = ScalarProgramYDependency::new(prefix);
        for target in y_load_indices(prefix) {
            if let Some(shape) =
                canonical_assignment_shape(producers.view(), output, target, &dependencies)
            {
                shapes.push((offset, shape));
            }
        }
    }
    let mut queries = crate::CanonicalAssignmentQueries::new(program);
    for (offset, (_, position)) in store_output_registers(program).enumerate() {
        assert_eq!(
            queries.has_any(offset),
            shapes.iter().any(|(output, _)| *output == offset)
        );
        for target in y_load_indices(&program[..position])
            .into_iter()
            .chain([9999])
        {
            let expected = shapes
                .iter()
                .find(|(output, shape)| *output == offset && shape.target_y_index() == target)
                .map(|(_, shape)| shape.clone());
            assert_eq!(
                queries.derive(offset, target),
                expected,
                "output{offset}/target{target}"
            );
        }
    }
    shapes
}

fn cross_program() -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: crate::TensorInputKind::P,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 3,
            input: crate::TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 6,
            input: crate::TensorInputKind::Y,
            input_start: 3,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 9,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 12,
            op: BinaryOp::Sub,
            lhs_start: 6,
            rhs_start: 9,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 3,
            stride: 1,
        },
    ]
}

#[test]
fn output_dependency_candidates_match_exhaustive_tensor_isolation() {
    let program = cross_program();
    let expected = exhaustive_shapes(&program);
    assert!(!expected.is_empty());
    assert_eq!(derive_target_assignment_shapes(&program), expected);
    for (output, shape) in &expected {
        assert_eq!(
            derive_target_assignment_shape_for_output(&program, *output, shape.target_y_index()),
            Some(shape.clone())
        );
    }
}

#[test]
fn repeated_stores_keep_exact_prefix_and_signed_zero_arithmetic() {
    for scale in [0.0, -0.0, 1.0, -1.0] {
        let mut program = cross_program();
        program.push(LinearOp::Const {
            dst: 15,
            value: scale,
        });
        program.push(LinearOp::Binary {
            dst: 16,
            op: BinaryOp::Mul,
            lhs: 12,
            rhs: 15,
        });
        program.push(LinearOp::StoreOutput { src: 16 });
        program.push(LinearOp::Const { dst: 3, value: 8.0 });
        program.push(LinearOp::StoreOutput { src: 12 });
        let actual = derive_target_assignment_shapes(&program);
        assert_eq!(actual, exhaustive_shapes(&program));
        assert!(actual.iter().all(|(output, _)| *output < 4));
    }
}

#[test]
fn unrelated_tensor_loads_are_not_output_candidates() {
    let mut program = cross_program();
    program.insert(
        0,
        LinearOp::TensorLoad {
            dst_start: 100,
            input: crate::TensorInputKind::Y,
            input_start: 100,
            seed_start: None,
            count: 512,
            lanes: 1,
        },
    );
    let prefix = &program[..program.len() - 1];
    let producers = UniqueProgram::new(prefix).unwrap();
    assert_eq!(y_load_indices(prefix).len(), 518);
    assert_eq!(
        dependency_candidates::derive(producers.view(), 12)
            .unwrap()
            .targets
            .len(),
        6
    );
    assert_eq!(
        derive_target_assignment_shapes(&program),
        exhaustive_shapes(&program)
    );
}

#[test]
fn unsupported_projection_operations_are_opaque_to_candidates() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 100 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Sqrt,
            arg: 0,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let producers = UniqueProgram::new(&program[..5]).unwrap();
    let candidates = dependency_candidates::derive(producers.view(), 4).unwrap();
    assert_eq!(candidates.targets, BTreeSet::from([1]));
    assert!(candidates.linear);
    assert_eq!(
        derive_target_assignment_shapes(&program),
        exhaustive_shapes(&program)
    );
}

#[test]
fn aggregate_matrix_candidates_keep_whole_operation_certificates() {
    for size in 1..=3 {
        let count = size * size;
        let register_count = count as u32;
        let program = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::Y,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: register_count,
                input: crate::TensorInputKind::P,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::MatrixMultiply {
                dst_start: register_count * 2,
                lhs_start: 0,
                rhs_start: register_count,
                rows: size,
                inner: size,
                columns: size,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: register_count * 2,
                count,
                stride: 1,
            },
        ];
        let actual = derive_target_assignment_shapes(&program);
        assert!(!actual.is_empty());
        assert_eq!(actual, exhaustive_shapes(&program));
    }
}

/// The lanes of one wide residual each isolate their own target: the walk is
/// shared by the lanes and the candidates follow each lane's dependencies, so
/// the inventory still matches the exhaustive oracle.
#[test]
fn wide_lane_residuals_keep_the_exhaustive_inventory() {
    let count = 300;
    let lanes = |dst_start, input_start| LinearOp::TensorLoad {
        dst_start,
        input: crate::TensorInputKind::Y,
        input_start,
        seed_start: None,
        count,
        lanes: 1,
    };
    let registers = u32::try_from(count).unwrap();
    let program = vec![
        lanes(0, 0),
        lanes(registers, count),
        LinearOp::TensorBinary {
            dst_start: registers * 2,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: registers,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: registers * 2,
            count,
            stride: 1,
        },
    ];
    let actual = derive_target_assignment_shapes(&program);
    assert_eq!(actual.len(), count * 2);
    assert_eq!(actual, exhaustive_shapes(&program));
}
