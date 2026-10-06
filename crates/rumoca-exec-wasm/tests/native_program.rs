//! Differential direct-write fusion versus the existing issued-stage ABI.

use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_exec_wasm::{
    compile_expression_compute_block_wasm_bytes, compile_native_assignment_schedule_wasm_bytes,
};
use rumoca_ir_solve as solve;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

fn family(
    count: usize,
    output: usize,
    target: usize,
    input: solve::LinearOp,
    factor: solve::LinearOp,
) -> solve::ComputeNode {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    };
    let stride = vec![solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    solve::ComputeNode::Map {
        output_map: solve::TensorOutputMap::dense_contiguous(output, &domain).unwrap(),
        domain,
        base_ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: target,
            },
            input,
            factor,
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
        load_strides: vec![
            solve::AffineStencilLoadStride {
                op_position: 0,
                terms: stride.clone(),
            },
            solve::AffineStencilLoadStride {
                op_position: 1,
                terms: stride,
            },
        ],
        const_strides: vec![],
        metadata: solve::TensorNodeMetadata::default(),
        span: solve::source_span_from_offsets(1, 0, 1),
    }
}

fn fixture(count: usize) -> (solve::NativeRefreshAssignmentSchedule, solve::VarLayout) {
    fixture_with_stencil(count, false)
}

fn neighbor_stencil(count: usize) -> solve::ComputeNode {
    let solve::ComputeNode::Map {
        domain,
        output_map,
        metadata,
        span,
        ..
    } = family(
        count - 2,
        count + 1,
        count + 1,
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::LoadY { dst: 2, index: 2 },
    )
    else {
        unreachable!()
    };
    let stride = vec![solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    solve::ComputeNode::AffineStencil {
        domain,
        output_map,
        metadata,
        span,
        base_ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: count + 1,
            },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::LoadY { dst: 2, index: 2 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Add,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Const { dst: 4, value: 0.5 },
            solve::LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Mul,
                lhs: 3,
                rhs: 4,
            },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 5,
            },
            solve::LinearOp::StoreOutput { src: 6 },
        ],
        load_strides: (0..3)
            .map(|op_position| solve::AffineStencilLoadStride {
                op_position,
                terms: stride.clone(),
            })
            .collect(),
        const_strides: vec![],
    }
}

fn fixture_with_stencil(
    count: usize,
    stencil: bool,
) -> (solve::NativeRefreshAssignmentSchedule, solve::VarLayout) {
    let span = solve::source_span_from_offsets(1, 0, 1);
    let scalar = solve::ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                solve::LinearOp::LoadY {
                    dst: 0,
                    index: count * 2 - 1,
                },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadY {
                    dst: 0,
                    index: count,
                },
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                solve::LinearOp::Const { dst: 2, value: 0.5 },
                solve::LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Add,
                    lhs: 1,
                    rhs: 2,
                },
                solve::LinearOp::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                solve::LinearOp::StoreOutput { src: 4 },
            ],
        ],
        vec![span; 2],
        vec![count * 2 - 1, count],
    )
    .unwrap();
    let block = solve::ComputeBlock {
        nodes: vec![
            if stencil {
                neighbor_stencil(count)
            } else {
                family(
                    count - 2,
                    count + 1,
                    count + 1,
                    solve::LinearOp::LoadY { dst: 1, index: 1 },
                    solve::LinearOp::LoadY {
                        dst: 2,
                        index: count,
                    },
                )
            },
            family(
                count,
                0,
                0,
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                solve::LinearOp::Const { dst: 2, value: 2.0 },
            ),
            solve::ComputeNode::ScalarPrograms(scalar),
        ],
    };
    let targets = (0..count * 2)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let layout = solve::VarLayout::from_parts(Default::default(), count * 2, count);
    let mut owner = solve::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&block, &targets, &layout)
        .unwrap();
    (owner.native_assignment_schedule().unwrap().clone(), layout)
}

fn instantiate(
    store: &mut Store<()>,
    linker: &Linker<()>,
    engine: &Engine,
    bytes: &[u8],
    export: &str,
) -> wasmi::TypedFunc<(i32, i32, f64, i32, i32), ()> {
    let module = Module::new(engine, bytes).unwrap();
    linker
        .instantiate(&mut *store, &module)
        .unwrap()
        .start(&mut *store)
        .unwrap()
        .get_typed_func(&*store, export)
        .unwrap()
}

fn write(memory: Memory, store: &mut Store<()>, offset: usize, values: &[f64]) {
    let bytes = values
        .iter()
        .flat_map(|value| value.to_le_bytes())
        .collect::<Vec<_>>();
    memory.write(store, offset, &bytes).unwrap();
}

fn read(memory: Memory, store: &Store<()>, offset: usize, count: usize) -> Vec<f64> {
    let mut bytes = vec![0; count * 8];
    memory.read(store, offset, &mut bytes).unwrap();
    bytes
        .chunks_exact(8)
        .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
        .collect()
}

fn expected_values(p: &[f64], stencil: bool) -> Vec<f64> {
    let gray = p.iter().map(|value| value * 2.0).collect::<Vec<_>>();
    let gain = p[0] + 0.5;
    let mut result = gray.clone();
    result.push(gain);
    for index in 1..p.len() - 1 {
        result.push(if stencil {
            (gray[index - 1] + gray[index + 1]) * 0.5
        } else {
            gray[index] * gain
        });
    }
    result.push(0.0);
    result
}

#[test]
fn fused_mixed_images_match_issued_modules_bitwise_with_one_call_and_no_scratch_writes() {
    for (count, stencil) in [16, 160 * 90, 320 * 180]
        .into_iter()
        .flat_map(|count| [false, true].map(|stencil| (count, stencil)))
    {
        let (schedule, layout) = fixture_with_stencil(count, stencil);
        let fused = compile_native_assignment_schedule_wasm_bytes(&schedule, &layout).unwrap();
        assert!(
            fused.len() < 4096,
            "native domains were expanded into emitted instructions"
        );
        let engine = Engine::default();
        let mut store = Store::new(&engine, ());
        let p_start = layout.y_scalars() * 8;
        let out_start = p_start + layout.p_scalars() * 8;
        let memory = Memory::new(
            &mut store,
            MemoryType::new((count * 32).div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let program = instantiate(&mut store, &linker, &engine, &fused, "eval_assignments");
        let stages = schedule
            .stages()
            .iter()
            .map(|stage| {
                let bytes =
                    compile_expression_compute_block_wasm_bytes(stage.value_kernel(), &layout)
                        .unwrap();
                (
                    instantiate(&mut store, &linker, &engine, &bytes, "eval_residual"),
                    stage.target_range().unwrap(),
                )
            })
            .collect::<Vec<_>>();
        for frame in 0..8 {
            let p = (0..count)
                .map(|index| ((index * 7 + frame * 13) % 255) as f64 / 255.0 - 0.5)
                .collect::<Vec<_>>();
            write(memory, &mut store, p_start, &p);
            write(memory, &mut store, 0, &vec![f64::NAN; layout.y_scalars()]);
            for (kernel, target) in &stages {
                kernel
                    .call(
                        &mut store,
                        (0, p_start as i32, frame as f64, 0, out_start as i32),
                    )
                    .unwrap();
                let output = read(memory, &store, out_start, target.len());
                write(memory, &mut store, target.start * 8, &output);
            }
            let reference = read(memory, &store, 0, layout.y_scalars());
            write(memory, &mut store, 0, &vec![f64::NAN; layout.y_scalars()]);
            memory
                .write(&mut store, out_start, &vec![0xab; count * 8])
                .unwrap();
            program
                .call(&mut store, (0, p_start as i32, frame as f64, -1, -1))
                .unwrap();
            let actual = read(memory, &store, 0, layout.y_scalars());
            let independent = expected_values(&p, stencil);
            for (index, ((actual, reference), independent)) in
                actual.iter().zip(&reference).zip(&independent).enumerate()
            {
                assert!(actual.is_finite());
                assert_eq!(
                    actual.to_bits(),
                    reference.to_bits(),
                    "frame{frame}/slot{index}"
                );
                assert_eq!(
                    actual.to_bits(),
                    independent.to_bits(),
                    "independent frame{frame}/slot{index}"
                );
            }
            assert_eq!(read(memory, &store, p_start, p.len()), p);
            let mut scratch = vec![0; count * 8];
            memory.read(&store, out_start, &mut scratch).unwrap();
            assert!(scratch.iter().all(|&byte| byte == 0xab));
        }
    }
}

#[test]
fn fused_buffers_reject_alias_alignment_and_bounds_before_any_write() {
    let (schedule, layout) = fixture(16);
    let bytes = compile_native_assignment_schedule_wasm_bytes(&schedule, &layout).unwrap();
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let program = instantiate(&mut store, &linker, &engine, &bytes, "eval_assignments");
    for (y, p) in [
        (0, 0),
        (0, 8),
        (1, 256),
        (0, 257),
        (65528, 256),
        (0, 65528),
        (-8, 256),
    ] {
        let sentinel = vec![0x53; 65536];
        memory.write(&mut store, 0, &sentinel).unwrap();
        assert!(program.call(&mut store, (y, p, 0.0, 0, 0)).is_err());
        let mut actual = vec![0; 65536];
        memory.read(&store, 0, &mut actual).unwrap();
        assert_eq!(actual, sentinel);
    }
    let wrong_y = solve::VarLayout::from_parts(Default::default(), 33, 16);
    assert!(compile_native_assignment_schedule_wasm_bytes(&schedule, &wrong_y).is_err());
    let wrong_p = solve::VarLayout::from_parts(Default::default(), 32, 15);
    assert!(compile_native_assignment_schedule_wasm_bytes(&schedule, &wrong_p).is_err());
}

#[test]
fn fused_native_execution_keeps_the_entire_issued_source_prefix() {
    let operations = vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::Unary {
            dst: 2,
            op: solve::UnaryOp::Sin,
            arg: 0,
        },
        solve::LinearOp::Const { dst: 3, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ];
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![operations],
                vec![solve::source_span_from_offsets(1, 0, 1)],
            )
            .unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 1, 1);
    let mut owner = solve::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&block, &[Some(solve::scalar_slot_y(0))], &layout)
        .unwrap();
    let bytes = compile_native_assignment_schedule_wasm_bytes(
        owner.native_assignment_schedule().unwrap(),
        &layout,
    )
    .unwrap();
    let engine = Engine::default();
    let mut store = Store::new(&engine, Vec::<u64>::new());
    let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
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
    let kernel = instance
        .get_typed_func::<(i32, i32, f64, i32, i32), ()>(&store, "eval_assignments")
        .unwrap();
    memory
        .write(&mut store, 0, &f64::NAN.to_le_bytes())
        .unwrap();
    for (input, expected) in [(4.0_f64, 6.0_f64), (-3.0, -1.0)] {
        memory.write(&mut store, 8, &input.to_le_bytes()).unwrap();
        kernel.call(&mut store, (0, 8, 0.0, 0, 0)).unwrap();
        let mut output = [0; 8];
        memory.read(&store, 0, &mut output).unwrap();
        assert_eq!(f64::from_le_bytes(output).to_bits(), expected.to_bits());
    }
    assert_eq!(store.data(), &[f64::NAN.to_bits(), 6.0_f64.to_bits()]);
}
