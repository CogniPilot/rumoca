use super::*;

fn load(start: Reg, count: usize, lanes: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: start,
        input: crate::TensorInputKind::Y,
        input_start: 200,
        count,
        seed_start: Some(400),
        lanes,
    }
}

fn binary(destination: Reg, count: usize, lanes: usize) -> LinearOp {
    LinearOp::TensorBinary {
        dst_start: destination,
        op: BinaryOp::Mul,
        lhs_start: 0,
        rhs_start: (count * lanes) as Reg,
        count,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes,
    }
}

#[test]
fn four_million_tensor_cells_and_register_gap_retain_three_families() {
    let count = 4_194_304;
    let destination = 8_388_610;
    let program = [
        load(0, count, 1),
        LinearOp::Const {
            dst: count as Reg,
            value: 2.0,
        },
        binary(destination, count, 1),
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(registers.family_count(), 3);
    for offset in [0, count / 2, count - 1] {
        assert_eq!(
            *registers.state(destination + offset as Reg).unwrap(),
            DependencyState::singleton(200 + offset)
        );
    }
    assert!(registers.state(count as Reg + 1).is_none());
    assert_eq!(
        registers.range(destination, count).unwrap(),
        DependencyState::from_range(200, 200 + count)
    );
}

#[test]
fn one_scalar_at_four_million_gap_has_no_absent_register_inventory() {
    let registers = program_register_y_dependencies(&[LinearOp::LoadY {
        dst: 4_194_304,
        index: 17,
    }])
    .unwrap();
    assert_eq!(registers.family_count(), 1);
    assert!(registers.state(4_194_303).is_none());
    assert_eq!(
        *registers.state(4_194_304).unwrap(),
        DependencyState::singleton(17)
    );
}

#[test]
fn overlapping_raw_binary_keeps_sequential_reads() {
    let program = [
        load(0, 3, 1),
        LinearOp::Const { dst: 4, value: 2.0 },
        LinearOp::TensorBinary {
            dst_start: 1,
            lhs_start: 0,
            rhs_start: 4,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
            op: BinaryOp::Add,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    for register in 1..=3 {
        assert_eq!(
            *registers.state(register).unwrap(),
            DependencyState::singleton(200)
        );
    }
}

#[test]
fn captured_operand_versions_survive_later_partial_overwrite() {
    let program = [
        load(0, 4, 1),
        LinearOp::Const { dst: 4, value: 2.0 },
        binary(5, 4, 1),
        LinearOp::LoadY { dst: 1, index: 99 },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(*registers.state(1).unwrap(), DependencyState::singleton(99));
    assert_eq!(
        *registers.state(6).unwrap(),
        DependencyState::singleton(201)
    );
    assert_eq!(
        registers.range(5, 4).unwrap(),
        DependencyState::from_range(200, 204)
    );
}

#[test]
fn dual_tensor_lanes_keep_exact_seed_and_y_rules() {
    let program = [
        load(0, 3, 2),
        LinearOp::Const { dst: 6, value: 2.0 },
        LinearOp::Const { dst: 7, value: 0.0 },
        binary(8, 3, 2),
        LinearOp::StoreOutputRange {
            start: 8,
            count: 6,
            stride: 1,
        },
    ];
    let y = program_register_y_dependencies(&program).unwrap();
    for offset in 0..3 {
        for lane in 0..2 {
            assert_eq!(
                *y.state(8 + (2 * offset + lane) as Reg).unwrap(),
                DependencyState::singleton(200 + offset)
            );
        }
    }
    assert_eq!(
        y.range(9, 4).unwrap(),
        DependencyState::from_range(200, 203)
    );
    let seeds = program_output_dependencies(&program, None).unwrap();
    for offset in 0..3 {
        assert_eq!(seeds[2 * offset], DependencyState::Empty);
        assert_eq!(
            seeds[2 * offset + 1],
            DependencyState::singleton(400 + offset)
        );
    }
}

#[test]
fn strided_inputs_ignore_unread_holes_but_invalid_reads_keep_first_error() {
    let mut program = vec![
        LinearOp::LoadY { dst: 0, index: 10 },
        LinearOp::LoadY { dst: 2, index: 20 },
        LinearOp::Const { dst: 4, value: 2.0 },
        LinearOp::TensorBinary {
            dst_start: 5,
            op: BinaryOp::Add,
            lhs_start: 0,
            rhs_start: 4,
            count: 2,
            lhs_stride: 2,
            rhs_stride: 0,
            lanes: 1,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(registers.family_count(), 4);
    assert_eq!(*registers.state(6).unwrap(), DependencyState::singleton(20));
    assert_eq!(
        registers.range(5, 2).unwrap().into_set(),
        BTreeSet::from([10, 20])
    );
    program.pop();
    program.push(LinearOp::TensorBinary {
        dst_start: 5,
        op: BinaryOp::Add,
        lhs_start: 0,
        rhs_start: 99,
        count: 2,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes: 1,
    });
    assert!(matches!(
        program_register_y_dependencies(&program),
        Err(StructuralPatternError::UninitializedRegister { register: 99, .. })
    ));
}

#[test]
fn unused_invalid_destination_still_refuses_complete_analysis() {
    let program = [
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::TensorFill {
            dst_start: 100,
            value_start: 20,
            count: 4_194_304,
            lanes: 1,
        },
    ];
    assert!(matches!(
        program_register_y_dependencies(&program),
        Err(StructuralPatternError::UninitializedRegister { register: 20, .. })
    ));
}

#[test]
fn primal_only_load_optional_seed_regression() {
    let program = [
        load(0, 3, 1),
        LinearOp::Const { dst: 3, value: 2.0 },
        LinearOp::DotProduct {
            dst: 4,
            lhs_start: 0,
            rhs_start: 3,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    assert_eq!(
        program_output_dependencies(&program, None).unwrap(),
        [DependencyState::Empty]
    );
}

#[test]
fn overlapping_dual_fill_regression() {
    let program = [
        load(1, 2, 1),
        LinearOp::TensorFill {
            dst_start: 0,
            value_start: 1,
            count: 2,
            lanes: 2,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(
        *registers.state(0).unwrap(),
        DependencyState::singleton(200)
    );
    for register in 1..4 {
        assert_eq!(
            *registers.state(register).unwrap(),
            DependencyState::singleton(201)
        );
    }
}

#[test]
fn deep_shared_tensor_chain_projects_and_drops_without_recursive_expansion() {
    let mut program = vec![load(0, 1, 1)];
    for destination in 1..20_001 {
        program.push(LinearOp::TensorBinary {
            dst_start: destination,
            op: BinaryOp::Add,
            lhs_start: destination - 1,
            rhs_start: destination - 1,
            count: 1,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        });
    }
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(registers.family_count(), program.len());
    assert_eq!(
        *registers.state(20_000).unwrap(),
        DependencyState::singleton(200)
    );
    assert_eq!(
        registers.range(20_000, 1).unwrap(),
        DependencyState::singleton(200)
    );
    drop(registers);
}

#[test]
fn four_million_transpose_and_product_cells_keep_source_families() {
    let count = 4_194_304;
    let program = [
        load(0, count, 1),
        LinearOp::TensorTranspose {
            dst_start: count as Reg,
            src_start: 0,
            rows: 2,
            columns: count / 2,
            element_width: 1,
            lanes: 1,
        },
        LinearOp::Const {
            dst: (2 * count) as Reg,
            value: 2.0,
        },
        LinearOp::MatrixMultiply {
            dst_start: (2 * count + 1) as Reg,
            lhs_start: count as Reg,
            rhs_start: (2 * count) as Reg,
            rows: count,
            inner: 1,
            columns: 1,
            lanes: 1,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(registers.family_count(), 4);
    for (offset, expected) in [(0, 200), (count / 2, 201), (count - 1, 200 + count - 1)] {
        assert_eq!(
            *registers.state(count as Reg + offset as Reg).unwrap(),
            DependencyState::singleton(expected)
        );
        assert_eq!(
            *registers.state((2 * count + 1 + offset) as Reg).unwrap(),
            DependencyState::singleton(expected)
        );
    }
    assert_eq!(
        registers.range((2 * count + 1) as Reg, count),
        Some(DependencyState::from_range(200, 200 + count))
    );
}

#[test]
fn dual_matrix_product_keeps_primal_tangent_and_exact_row_column_dependencies() {
    let program = [
        load(0, 6, 2),
        LinearOp::TensorLoad {
            dst_start: 12,
            input: crate::TensorInputKind::Y,
            input_start: 300,
            count: 6,
            seed_start: Some(500),
            lanes: 2,
        },
        LinearOp::MatrixMultiply {
            dst_start: 24,
            lhs_start: 0,
            rhs_start: 12,
            rows: 2,
            inner: 3,
            columns: 2,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 24,
            count: 8,
            stride: 1,
        },
    ];
    let y = program_register_y_dependencies(&program).unwrap();
    assert_eq!(y.family_count(), 3);
    for row in 0..2 {
        for column in 0..2 {
            let expected: BTreeSet<_> = (0..3)
                .flat_map(|inner| [200 + row * 3 + inner, 300 + inner * 2 + column])
                .collect();
            for lane in 0..2 {
                assert_eq!(
                    y.state((24 + (row * 2 + column) * 2 + lane) as Reg)
                        .unwrap()
                        .clone()
                        .into_owned()
                        .into_set(),
                    expected
                );
            }
        }
    }
    let seeds = program_output_dependencies(&program, None).unwrap();
    assert_eq!(seeds[0], DependencyState::Empty);
    assert_eq!(
        seeds[1].clone().into_set(),
        BTreeSet::from([400, 401, 402, 500, 502, 504])
    );
}

#[test]
fn transpose_and_matrix_unused_holes_preserve_first_read_refusal() {
    for (operation, expected) in [
        (
            LinearOp::TensorTranspose {
                dst_start: 20,
                src_start: 0,
                rows: 2,
                columns: 2,
                element_width: 1,
                lanes: 1,
            },
            2,
        ),
        (
            LinearOp::MatrixMultiply {
                dst_start: 20,
                lhs_start: 0,
                rhs_start: 10,
                rows: 2,
                inner: 2,
                columns: 2,
                lanes: 1,
            },
            1,
        ),
    ] {
        let program = [load(0, 1, 1), load(10, 4, 1), operation];
        assert!(
            matches!(program_register_y_dependencies(&program), Err(StructuralPatternError::UninitializedRegister { register, .. }) if register == expected)
        );
    }
}

#[test]
fn concatenate_and_fixed_dynamic_updates_keep_compact_source_empty_families() {
    for count in [4096, 4_194_304] {
        let program = [
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorConcatenate {
                dst_start: count as Reg,
                sources: vec![crate::TensorConcatenateSource {
                    start: 0,
                    dimensions: vec![count as u32].into(),
                }]
                .into(),
                dimensions: vec![count as u32].into(),
                axis: 0,
                lanes: 1,
            },
            LinearOp::Const {
                dst: (2 * count) as Reg,
                value: 0.0,
            },
            LinearOp::TensorUpdate {
                dst_start: (2 * count + 1) as Reg,
                base_start: count as Reg,
                value_start: (2 * count) as Reg,
                dimensions: vec![count as u32].into(),
                subscripts: vec![crate::TensorUpdateSubscript::Index(
                    crate::TensorIndex::Constant(0),
                )]
                .into(),
                lanes: 1,
            },
            LinearOp::TensorUpdate {
                dst_start: (3 * count + 1) as Reg,
                base_start: (2 * count + 1) as Reg,
                value_start: (2 * count) as Reg,
                dimensions: vec![count as u32].into(),
                subscripts: vec![crate::TensorUpdateSubscript::Index(
                    crate::TensorIndex::Runtime((2 * count) as Reg),
                )]
                .into(),
                lanes: 1,
            },
        ];
        let registers = program_register_y_dependencies(&program).unwrap();
        assert_eq!(registers.family_count(), 5, "extent {count}");
        assert_eq!(
            registers.range((3 * count + 1) as Reg, count),
            Some(DependencyState::Empty)
        );
        for offset in [0, count / 2, count - 1] {
            assert_eq!(
                *registers.state((3 * count + 1 + offset) as Reg).unwrap(),
                DependencyState::Empty
            );
        }
    }
}

#[test]
fn concatenation_axis_and_dual_lane_projection_matches_literal_coordinates() {
    let program = [
        load(0, 4, 2),
        LinearOp::TensorLoad {
            dst_start: 8,
            input: crate::TensorInputKind::Y,
            input_start: 300,
            count: 2,
            seed_start: Some(500),
            lanes: 2,
        },
        LinearOp::TensorConcatenate {
            dst_start: 12,
            sources: vec![
                crate::TensorConcatenateSource {
                    start: 0,
                    dimensions: vec![2, 2].into(),
                },
                crate::TensorConcatenateSource {
                    start: 8,
                    dimensions: vec![2, 1].into(),
                },
            ]
            .into(),
            dimensions: vec![2, 3].into(),
            axis: 1,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 12,
            stride: 1,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    assert_eq!(registers.family_count(), 3);
    for (element, expected) in [200, 201, 300, 202, 203, 301].into_iter().enumerate() {
        for lane in 0..2 {
            let dependency = if lane == 0 {
                DependencyState::singleton(expected)
            } else {
                DependencyState::Empty
            };
            assert_eq!(
                *registers.state((12 + 2 * element + lane) as Reg).unwrap(),
                dependency
            );
        }
    }
    assert_eq!(
        registers.range(12, 12).unwrap().into_set(),
        BTreeSet::from([200, 201, 202, 203, 300, 301])
    );
    let seeds = program_output_dependencies(&program, None).unwrap();
    for (element, expected) in [400, 401, 500, 402, 403, 501].into_iter().enumerate() {
        assert_eq!(seeds[2 * element], DependencyState::Empty);
        assert_eq!(seeds[2 * element + 1], DependencyState::singleton(expected));
    }
}

#[test]
fn fixed_patch_unused_base_and_patch_holes_keep_original_read_policy() {
    let whole = [
        load(10, 4, 1),
        LinearOp::TensorUpdate {
            dst_start: 20,
            base_start: 0,
            value_start: 10,
            dimensions: vec![4].into(),
            subscripts: vec![crate::TensorUpdateSubscript::Whole].into(),
            lanes: 1,
        },
    ];
    let registers = program_register_y_dependencies(&whole).unwrap();
    assert_eq!(registers.family_count(), 2);
    assert_eq!(
        registers.range(20, 4),
        Some(DependencyState::from_range(200, 204))
    );
    let outside = [
        load(0, 4, 1),
        LinearOp::TensorUpdate {
            dst_start: 20,
            base_start: 0,
            value_start: 10,
            dimensions: vec![4].into(),
            subscripts: vec![crate::TensorUpdateSubscript::Index(
                crate::TensorIndex::Constant(99),
            )]
            .into(),
            lanes: 1,
        },
    ];
    let registers = program_register_y_dependencies(&outside).unwrap();
    assert_eq!(registers.family_count(), 2);
    assert_eq!(
        registers.range(20, 4),
        Some(DependencyState::from_range(200, 204))
    );
    let partial = [
        load(1, 3, 1),
        load(10, 1, 1),
        LinearOp::TensorUpdate {
            dst_start: 20,
            base_start: 0,
            value_start: 10,
            dimensions: vec![4].into(),
            subscripts: vec![crate::TensorUpdateSubscript::Index(
                crate::TensorIndex::Constant(0),
            )]
            .into(),
            lanes: 1,
        },
    ];
    let registers = program_register_y_dependencies(&partial).unwrap();
    assert_eq!(
        *registers.state(20).unwrap(),
        DependencyState::singleton(200)
    );
    assert_eq!(
        *registers.state(23).unwrap(),
        DependencyState::singleton(202)
    );
}

#[test]
fn overlapping_concatenation_and_fixed_update_keep_sequential_source_versions() {
    let concatenate = [
        load(0, 3, 1),
        LinearOp::TensorConcatenate {
            dst_start: 1,
            sources: vec![crate::TensorConcatenateSource {
                start: 0,
                dimensions: vec![3].into(),
            }]
            .into(),
            dimensions: vec![3].into(),
            axis: 0,
            lanes: 1,
        },
    ];
    let update = [
        load(0, 3, 1),
        LinearOp::TensorUpdate {
            dst_start: 1,
            base_start: 0,
            value_start: 0,
            dimensions: vec![3].into(),
            subscripts: vec![crate::TensorUpdateSubscript::Whole].into(),
            lanes: 1,
        },
    ];
    for program in [concatenate, update] {
        let registers = program_register_y_dependencies(&program).unwrap();
        for register in 1..=3 {
            assert_eq!(
                *registers.state(register).unwrap(),
                DependencyState::singleton(200)
            );
        }
    }
}

#[test]
fn raw_non_primal_dual_loads_keep_holes_and_zero_lane_sequential_rule() {
    let zero = program_register_y_dependencies(&[load(0, 3, 0)]).unwrap();
    assert_eq!(*zero.state(0).unwrap(), DependencyState::singleton(202));
    for input in [crate::TensorInputKind::Y, crate::TensorInputKind::P] {
        let raw = LinearOp::TensorLoad {
            dst_start: 0,
            input,
            input_start: 200,
            count: 3,
            seed_start: Some(400),
            lanes: 3,
        };
        let registers = program_register_y_dependencies(std::slice::from_ref(&raw)).unwrap();
        for element in 0..3 {
            let expected = if input == crate::TensorInputKind::Y {
                DependencyState::singleton(200 + element)
            } else {
                DependencyState::Empty
            };
            assert_eq!(*registers.state((3 * element) as Reg).unwrap(), expected);
            assert!(registers.state((3 * element + 1) as Reg).is_none());
            assert!(registers.state((3 * element + 2) as Reg).is_none());
        }
        assert!(matches!(
            program_register_y_dependencies(&[raw, LinearOp::StoreOutput { src: 1 }]),
            Err(StructuralPatternError::UninitializedRegister { register: 1, .. })
        ));
    }
}

#[test]
fn raw_fill_lanes_and_binary_primal_positions_preserve_exact_reads() {
    let program = [
        LinearOp::LoadY { dst: 0, index: 200 },
        LinearOp::LoadY { dst: 1, index: 201 },
        LinearOp::LoadY { dst: 2, index: 202 },
        LinearOp::TensorFill {
            dst_start: 10,
            value_start: 0,
            count: 3,
            lanes: 3,
        },
        LinearOp::TensorBinary {
            dst_start: 20,
            lhs_start: 0,
            rhs_start: 0,
            op: BinaryOp::Add,
            count: 2,
            lhs_stride: 0,
            rhs_stride: 0,
            lanes: 3,
        },
    ];
    let registers = program_register_y_dependencies(&program).unwrap();
    for offset in 0..9 {
        assert_eq!(
            *registers.state(10 + offset).unwrap(),
            DependencyState::singleton(200 + offset as usize % 3)
        );
    }
    for register in [20, 23] {
        assert_eq!(
            *registers.state(register).unwrap(),
            DependencyState::singleton(200)
        );
    }
    for register in [21, 22, 24, 25] {
        assert!(registers.state(register).is_none());
    }
    assert!(matches!(
        program_register_y_dependencies(&[LinearOp::TensorBinary {
            dst_start: 5,
            lhs_start: 0,
            rhs_start: 1,
            op: BinaryOp::Add,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 0
        }]),
        Err(StructuralPatternError::UninitializedRegister { register: 0, .. })
    ));
}
