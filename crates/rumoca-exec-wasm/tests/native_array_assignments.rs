//! Execute construction-issued compact array assignments with independent values.

mod suite_array_cross_alias;

use rumoca_exec_wasm::compile_native_assignment_schedule_wasm_bytes;
use rumoca_ir_solve as solve;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

fn strided_column(rows: usize, columns: usize, column: usize) -> solve::ComputeNode {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "row".into(),
            lower: 1,
            upper: rows as i64,
            step: 1,
        }],
    };
    solve::ComputeNode::Map {
        output_map: solve::TensorOutputMap::dense_contiguous(column * rows, &domain).unwrap(),
        domain,
        base_ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: column,
            },
            solve::LinearOp::LoadP {
                dst: 1,
                index: column * rows,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Unary {
                dst: 3,
                op: solve::UnaryOp::Sin,
                arg: 0,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
        load_strides: vec![
            solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: columns as isize,
                }],
            },
            solve::AffineStencilLoadStride {
                op_position: 1,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            },
        ],
        const_strides: vec![],
        metadata: solve::TensorNodeMetadata::default(),
        span: solve::source_span_from_offsets(1, 5, 10),
    }
}

#[test]
fn strided_columns_write_every_owned_cell_without_overwriting_other_columns() {
    for (rows, columns) in [(6, 16), (90, 160)] {
        let nodes = (0..columns)
            .map(|column| strided_column(rows, columns, column))
            .collect();
        let targets = (0..columns)
            .flat_map(|column| {
                (0..rows).map(move |row| Some(solve::scalar_slot_y(row * columns + column)))
            })
            .collect();
        let mut compiled = Program::from_block(
            solve::ComputeBlock { nodes },
            targets,
            rows * columns,
            rows * columns,
        );
        let bits = [
            0.,
            -0.,
            f64::from_bits(1),
            -f64::from_bits(1),
            1e100,
            -1e100,
            2.75,
        ];
        for frame in 0..3 {
            let p = (0..rows * columns)
                .map(|i| bits[(i + frame) % bits.len()])
                .collect::<Vec<_>>();
            let y = (0..rows * columns)
                .map(|i| (i + frame + 2) as f64 * 0.125)
                .collect::<Vec<_>>();
            let expected = (0..rows * columns)
                .map(|i| p[(i % columns) * rows + i / columns])
                .collect::<Vec<_>>();
            let prefix = (0..columns)
                .flat_map(|column| {
                    let y = &y;
                    (0..rows).map(move |row| y[row * columns + column])
                })
                .collect::<Vec<_>>();
            compiled.execute(&y, &p, &expected, &prefix);
        }
    }
}

fn rectangular_slice(
    rows: usize,
    columns: usize,
    start: usize,
    width: usize,
    output: usize,
) -> solve::ComputeNode {
    let mut node = strided_column(rows, columns, start);
    let solve::ComputeNode::Map {
        domain,
        output_map,
        base_ops,
        load_strides,
        ..
    } = &mut node
    else {
        unreachable!()
    };
    domain.binders.push(rumoca_core::StructuredIndexBinder {
        id: 1,
        display_name: "column".into(),
        lower: 1,
        upper: width as i64,
        step: 1,
    });
    *output_map = solve::TensorOutputMap::dense_contiguous(output, domain).unwrap();
    base_ops[1] = solve::LinearOp::LoadP {
        dst: 1,
        index: start,
    };
    for load in load_strides {
        load.terms = vec![
            solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: columns as isize,
            },
            solve::AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 1,
            },
        ];
    }
    node
}

fn slice_prefix(y: &[f64], rows: usize, columns: usize, width: usize) -> Vec<f64> {
    [0..width, width..columns]
        .into_iter()
        .flat_map(|range| {
            (0..rows)
                .flat_map(move |row| range.clone().map(move |column| y[row * columns + column]))
        })
        .collect()
}

#[test]
fn rectangular_slices_keep_the_gap_cells_owned_by_the_complementary_slice() {
    for (rows, columns, width) in [(15, 15, 6), (90, 160, 64)] {
        let block = solve::ComputeBlock {
            nodes: vec![
                rectangular_slice(rows, columns, 0, width, 0),
                rectangular_slice(rows, columns, width, columns - width, rows * width),
            ],
        };
        let targets = [0..width, width..columns]
            .into_iter()
            .flat_map(|range| {
                (0..rows).flat_map(move |row| {
                    range
                        .clone()
                        .map(move |column| Some(solve::scalar_slot_y(row * columns + column)))
                })
            })
            .collect();
        let mut compiled = Program::from_block(block, targets, rows * columns, rows * columns);
        for frame in 0..3 {
            let p = (0..rows * columns)
                .map(|i| [0., -0., f64::from_bits(1), -1e100, 2.75][(i + frame) % 5])
                .collect::<Vec<_>>();
            let y = (0..rows * columns)
                .map(|i| (i + frame + 1) as f64 * 0.125)
                .collect::<Vec<_>>();
            let prefix = slice_prefix(&y, rows, columns, width);
            compiled.execute(&y, &p, &p, &prefix);
        }
    }
}

fn array(
    count: usize,
    target: usize,
    kind: solve::TensorInputKind,
    input: usize,
) -> Vec<solve::LinearOp> {
    let n = count as u32;
    vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: target,
            count,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorLoad {
            dst_start: n,
            input: kind,
            input_start: input,
            count,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorBinary {
            dst_start: 2 * n,
            op: solve::BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: n,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        // A complete unused source prefix must still execute at the original Y.
        solve::LinearOp::Unary {
            dst: 3 * n,
            op: solve::UnaryOp::Sin,
            arg: 0,
        },
        solve::LinearOp::StoreOutputRange {
            start: 2 * n,
            count,
            stride: 1,
        },
    ]
}

struct Program {
    store: Store<Vec<u64>>,
    memory: Memory,
    entry: TypedFunc<(i32, i32, f64, i32, i32), ()>,
    p_start: usize,
    bytes: usize,
}

impl Program {
    fn new(programs: Vec<Vec<solve::LinearOp>>, y_count: usize, p_count: usize) -> Self {
        let spans = vec![solve::source_span_from_offsets(1, 5, 10); programs.len()];
        let block = solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::ScalarPrograms(
                solve::ScalarProgramBlock::with_program_spans(programs, spans).unwrap(),
            )],
        };
        let targets = (0..y_count)
            .map(|index| Some(solve::scalar_slot_y(index)))
            .collect::<Vec<_>>();
        Self::from_block(block, targets, y_count, p_count)
    }

    fn from_block(
        block: solve::ComputeBlock,
        targets: Vec<Option<solve::ScalarSlot>>,
        y_count: usize,
        p_count: usize,
    ) -> Self {
        let layout = solve::VarLayout::from_parts(Default::default(), y_count, p_count);
        let mut owner = solve::ContinuousRefreshOwners::default();
        owner
            .issue_native_assignment_schedule(&block, &targets, &layout)
            .unwrap();
        let bytes = compile_native_assignment_schedule_wasm_bytes(
            owner.native_assignment_schedule().unwrap(),
            &layout,
        )
        .unwrap();
        if y_count == 28_800
            && let Ok(path) = std::env::var("NATIVE_ARRAY_WASM_OUTPUT")
        {
            std::fs::write(path, &bytes).unwrap();
        }
        let engine = Engine::default();
        let mut store = Store::new(&engine, Vec::<u64>::new());
        let pages = ((y_count + p_count) * 8 + 64).div_ceil(65536) as u32;
        let memory = Memory::new(&mut store, MemoryType::new(pages.max(1), None)).unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker
            .func_wrap(
                "env",
                "sin",
                |mut caller: wasmi::Caller<'_, Vec<u64>>, value: f64| {
                    caller.data_mut().push(value.to_bits());
                    value.sin()
                },
            )
            .unwrap();
        let module = Module::new(&engine, &bytes[..]).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let entry = instance.get_typed_func(&store, "eval_assignments").unwrap();
        Self {
            store,
            memory,
            entry,
            p_start: y_count * 8,
            bytes: bytes.len(),
        }
    }

    fn write(&mut self, offset: usize, values: &[f64]) {
        let bytes = values
            .iter()
            .flat_map(|value| value.to_le_bytes())
            .collect::<Vec<_>>();
        self.memory.write(&mut self.store, offset, &bytes).unwrap();
    }

    fn execute(&mut self, y: &[f64], p: &[f64], expected: &[f64], sin_arguments: &[f64]) {
        self.write(0, y);
        self.write(self.p_start, p);
        self.store.data_mut().clear();
        let tail = self.p_start + p.len() * 8;
        self.memory
            .write(&mut self.store, tail, &[0xab; 64])
            .unwrap();
        self.entry
            .call(&mut self.store, (0, self.p_start as i32, 0., -1, -1))
            .unwrap();
        let actual = &self.memory.data(&self.store)[..y.len() * 8];
        for (index, (bytes, value)) in actual.chunks_exact(8).zip(expected).enumerate() {
            assert_eq!(
                u64::from_le_bytes(bytes.try_into().unwrap()),
                value.to_bits(),
                "slot {index}"
            );
        }
        assert_eq!(
            self.store.data(),
            &sin_arguments
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>()
        );
        let parameter_bytes = p
            .iter()
            .flat_map(|value| value.to_le_bytes())
            .collect::<Vec<_>>();
        assert_eq!(
            &self.memory.data(&self.store)[self.p_start..tail],
            parameter_bytes
        );
        assert_eq!(&self.memory.data(&self.store)[tail..tail + 64], &[0xab; 64]);
    }
}

#[test]
fn native_permuted_affine_stage_and_array_stage_share_bounded_private_registers() {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    let rows = 6usize;
    let columns = 16usize;
    let count = rows * columns;
    let domain = StructuredIndexDomain {
        binders: vec![
            StructuredIndexBinder {
                id: 0,
                display_name: "column".into(),
                lower: 1,
                upper: columns as i64,
                step: 1,
            },
            StructuredIndexBinder {
                id: 1,
                display_name: "row".into(),
                lower: 1,
                upper: rows as i64,
                step: 1,
            },
        ],
    };
    let dense = solve::TensorOutputMap::dense_contiguous(0, &domain).unwrap();
    let map = solve::ComputeNode::Map {
        domain,
        output_map: dense.clone(),
        base_ops: vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
        load_strides: vec![
            solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    },
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 1,
                        stride: columns as isize,
                    },
                ],
            },
            solve::AffineStencilLoadStride {
                op_position: 1,
                terms: dense.strides,
            },
        ],
        const_strides: vec![],
        metadata: solve::TensorNodeMetadata::default(),
        span: solve::source_span_from_offsets(1, 5, 10),
    };
    let block = solve::ComputeBlock {
        nodes: vec![
            map,
            solve::ComputeNode::ScalarPrograms(
                solve::ScalarProgramBlock::with_program_spans(
                    vec![array(9, count, solve::TensorInputKind::P, 0)],
                    vec![solve::source_span_from_offsets(1, 5, 10)],
                )
                .unwrap(),
            ),
        ],
    };
    let targets = (0..count)
        .map(|i| Some(solve::scalar_slot_y(i % rows * columns + i / rows)))
        .chain((count..count + 9).map(|i| Some(solve::scalar_slot_y(i))))
        .collect();
    let mut program = Program::from_block(block, targets, count + 9, count);
    for frame in 0..3 {
        let p = (0..count)
            .map(|i| {
                if (i + frame) % 3 == 0 {
                    -0.0
                } else {
                    (i + frame) as f64
                }
            })
            .collect::<Vec<_>>();
        let expected = (0..count)
            .map(|i| p[i % columns * rows + i / columns])
            .chain(p[..9].iter().copied())
            .collect::<Vec<_>>();
        program.execute(&vec![7.; count + 9], &p, &expected, &[7.]);
    }
}

#[test]
fn native_array_dependency_order_copies_every_value_bit_and_keeps_the_source_prefix() {
    for count in [3, 225, 14_400] {
        let mut program = Program::new(
            vec![
                array(count, 0, solve::TensorInputKind::Y, count),
                array(count, count, solve::TensorInputKind::P, 0),
            ],
            2 * count,
            count,
        );
        let values = [
            0.0,
            -0.0,
            f64::from_bits(1),
            -f64::from_bits(1),
            1e100,
            -1e100,
            2.75,
        ];
        for frame in 0..3 {
            let p = (0..count)
                .map(|i| values[(i + frame) % values.len()])
                .collect::<Vec<_>>();
            let y = (0..2 * count)
                .map(|i| -19.75 + i as f64 * 0.001)
                .collect::<Vec<_>>();
            let expected = p.iter().chain(&p).copied().collect::<Vec<_>>();
            program.execute(&y, &p, &expected, &[y[count], y[0]]);
        }
        eprintln!("native array {count}: {} bytes", program.bytes);
        assert!(
            program.bytes < 1024,
            "tensor extents must not expand emitted operations"
        );
    }
}

fn rhs_program(
    count: usize,
    rhs: Vec<solve::LinearOp>,
    value: u32,
    residual: u32,
) -> Vec<solve::LinearOp> {
    let mut ops = vec![solve::LinearOp::TensorLoad {
        dst_start: 0,
        input: solve::TensorInputKind::Y,
        input_start: 0,
        count,
        seed_start: None,
        lanes: 1,
    }];
    ops.extend(rhs);
    ops.push(solve::LinearOp::TensorBinary {
        dst_start: residual,
        op: solve::BinaryOp::Sub,
        lhs_start: 0,
        rhs_start: value,
        count,
        lhs_stride: 1,
        rhs_stride: 1,
        lanes: 1,
    });
    ops.push(solve::LinearOp::StoreOutputRange {
        start: residual,
        count,
        stride: 1,
    });
    ops
}

fn load_p(dst: u32, input: usize, count: usize) -> solve::LinearOp {
    solve::LinearOp::TensorLoad {
        dst_start: dst,
        input: solve::TensorInputKind::P,
        input_start: input,
        count,
        seed_start: None,
        lanes: 1,
    }
}

#[test]
fn native_transpose_trailing_dimensions_and_middle_axis_concatenation_keep_value_bits() {
    let transpose = rhs_program(
        12,
        vec![
            load_p(12, 0, 12),
            solve::LinearOp::TensorTranspose {
                dst_start: 24,
                src_start: 12,
                rows: 3,
                columns: 2,
                element_width: 2,
                lanes: 1,
            },
        ],
        24,
        36,
    );
    let concatenation = rhs_program(
        12,
        vec![
            load_p(12, 0, 4),
            load_p(16, 4, 8),
            solve::LinearOp::TensorConcatenate {
                dst_start: 24,
                sources: vec![
                    solve::TensorConcatenateSource {
                        start: 12,
                        dimensions: vec![2, 1, 2].into(),
                    },
                    solve::TensorConcatenateSource {
                        start: 16,
                        dimensions: vec![2, 2, 2].into(),
                    },
                ]
                .into(),
                dimensions: vec![2, 3, 2].into(),
                axis: 1,
                lanes: 1,
            },
        ],
        24,
        36,
    );
    let mut transposed = Program::new(vec![transpose], 12, 12);
    let mut concatenated = Program::new(vec![concatenation], 12, 12);
    let indices_t = [0, 1, 6, 7, 2, 3, 8, 9, 4, 5, 10, 11];
    let indices_c = [0, 1, 4, 5, 6, 7, 2, 3, 8, 9, 10, 11];
    for frame in 0..3 {
        let p = (0..12)
            .map(|i| {
                if (i + frame) % 3 == 0 {
                    -0.0
                } else {
                    (i + frame) as f64
                }
            })
            .collect::<Vec<_>>();
        transposed.execute(&[7.; 12], &p, &indices_t.map(|i| p[i]), &[]);
        concatenated.execute(&[7.; 12], &p, &indices_c.map(|i| p[i]), &[]);
    }
}

#[test]
fn native_cross_identity_and_rectangular_matrix_have_independent_ordered_oracles() {
    let cross = rhs_program(
        3,
        vec![
            load_p(3, 0, 6),
            solve::LinearOp::TensorCross {
                dst_start: 9,
                lhs_start: 3,
                rhs_start: 6,
                lanes: 1,
            },
        ],
        9,
        12,
    );
    let mut crossed = Program::new(vec![cross], 3, 6);
    crossed.execute(&[19.; 3], &[1., 2., 3., 4., 5., 6.], &[-3., 6., -3.], &[]);
    let identity = rhs_program(
        225,
        vec![solve::LinearOp::TensorIdentity {
            dst_start: 225,
            size: 15,
            lanes: 1,
        }],
        225,
        450,
    );
    let expected = (0..225)
        .map(|i| if i / 15 == i % 15 { 1. } else { 0. })
        .collect::<Vec<_>>();
    let mut identified = Program::new(vec![identity], 225, 0);
    identified.execute(&[19.; 225], &[], &expected, &[]);
    let rectangular = rhs_program(
        8,
        vec![
            load_p(8, 0, 6),
            load_p(14, 6, 12),
            solve::LinearOp::MatrixMultiply {
                dst_start: 26,
                lhs_start: 8,
                rhs_start: 14,
                rows: 2,
                inner: 3,
                columns: 4,
                lanes: 1,
            },
        ],
        26,
        34,
    );
    let p = (0..18).map(|i| (i as f64 - 7.) * 0.125).collect::<Vec<_>>();
    let expected = (0..8)
        .map(|i| {
            (0..3)
                .map(|k| p[i / 4 * 3 + k] * p[6 + k * 4 + i % 4])
                .sum()
        })
        .collect::<Vec<f64>>();
    let mut multiplied = Program::new(vec![rectangular], 8, 18);
    multiplied.execute(&[19.; 8], &p, &expected, &[]);
}

#[test]
fn native_private_arena_guards_public_buffers_before_any_output_write() {
    let mut program = Program::new(vec![array(225, 0, solve::TensorInputKind::P, 0)], 225, 225);
    let values = (0..450).map(|i| i as f64 * 0.5).collect::<Vec<_>>();
    program.write(0, &values);
    for (y, p) in [
        (1, program.p_start as i32),
        (0, 0),
        (65_000, program.p_start as i32),
    ] {
        let before = program.memory.data(&program.store).to_vec();
        assert!(
            program
                .entry
                .call(&mut program.store, (y, p, 0., -1, -1))
                .is_err()
        );
        assert_eq!(program.memory.data(&program.store), before);
        assert!(program.store.data().is_empty());
    }
}

#[test]
fn native_private_arena_refuses_excess_storage_without_allocating_the_register_extent() {
    let operations = rhs_program(
        1,
        vec![
            load_p(1, 0, 1),
            solve::LinearOp::TensorFill {
                dst_start: 10_000_000,
                value_start: 1,
                count: 1,
                lanes: 1,
            },
        ],
        10_000_000,
        10_000_001,
    );
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![operations],
                vec![solve::source_span_from_offsets(1, 5, 10)],
            )
            .unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 1, 1);
    let mut owner = solve::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&source, &[Some(solve::scalar_slot_y(0))], &layout)
        .unwrap();
    let error = compile_native_assignment_schedule_wasm_bytes(
        owner.native_assignment_schedule().unwrap(),
        &layout,
    )
    .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("private register arena exceeds 64 MiB")
    );
}

#[test]
fn native_matrix_and_broadcast_values_execute_after_the_unchanged_residual_prefix() {
    let mut matrix = array(9, 0, solve::TensorInputKind::P, 0);
    matrix[3] = solve::LinearOp::Unary {
        dst: 54,
        op: solve::UnaryOp::Sin,
        arg: 0,
    };
    matrix.insert(
        2,
        solve::LinearOp::TensorLoad {
            dst_start: 27,
            input: solve::TensorInputKind::P,
            input_start: 9,
            count: 9,
            seed_start: None,
            lanes: 1,
        },
    );
    matrix.insert(
        3,
        solve::LinearOp::MatrixMultiply {
            dst_start: 36,
            lhs_start: 9,
            rhs_start: 27,
            rows: 3,
            inner: 3,
            columns: 3,
            lanes: 1,
        },
    );
    if let solve::LinearOp::TensorBinary { rhs_start, .. } = &mut matrix[4] {
        *rhs_start = 36;
    }
    let mut compiled = Program::new(vec![matrix], 9, 18);
    let p = (0..18).map(|i| (i as f64 - 7.) * 0.125).collect::<Vec<_>>();
    let expected = (0..9)
        .map(|i| {
            (0..3)
                .map(|k| p[(i / 3) * 3 + k] * p[9 + k * 3 + i % 3])
                .sum()
        })
        .collect::<Vec<f64>>();
    compiled.execute(&[2.; 9], &p, &expected, &[2.]);
    for reverse in [false, true] {
        let mut ops = array(9, 0, solve::TensorInputKind::P, 0);
        ops[1] = solve::LinearOp::LoadP { dst: 9, index: 0 };
        if let solve::LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            lhs_stride,
            rhs_stride,
            ..
        } = &mut ops[2]
        {
            *rhs_stride = 0;
            if reverse {
                std::mem::swap(lhs_start, rhs_start);
                std::mem::swap(lhs_stride, rhs_stride);
            }
        }
        let mut compiled = Program::new(vec![ops], 9, 1);
        compiled.execute(&[2.; 9], &[-0.], &[-0.; 9], &[2.]);
    }
}
