//! Whole-program tuple ordering, checked table linkage and atomic publication.
mod affine;
mod conditionals;
mod maps;
mod math;
mod packed_tuples;
use super::*;
use rumoca_exec_wasm::compile_native_assignment_schedule_with_calls_wasm;

fn fixture(
    count: usize,
) -> (
    solve::NativeRefreshAssignmentSchedule,
    solve::VarLayout,
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
) {
    let (table, site) = mixed(count as u32);
    let layout = solve::VarLayout::from_parts(Default::default(), count + 4, count + 3);
    let mut programs = vec![vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadP {
            dst: 1,
            index: count + 1,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]];
    let mut target = 1;
    let mut output_offset = 0;
    for width in [count, 1, 1, 1] {
        programs.push(call_program(width, target, output_offset, count, &site));
        target += width;
        output_offset += width;
    }
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(programs, vec![span(900); 5]).unwrap(),
        )],
    };
    let targets = (0..count + 4)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let mut owners = solve::ContinuousRefreshOwners::default();
    owners
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    (
        owners.native_assignment_schedule().unwrap().clone(),
        layout,
        table,
        site,
    )
}

fn call_program(
    width: usize,
    target: usize,
    output_offset: usize,
    count: usize,
    site: &solve::SolvePureCallSite,
) -> Vec<solve::LinearOp> {
    let start = width as u32;
    let call = start + count as u32 + 3;
    let mut ops = vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: target,
            count: width,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorLoad {
            dst_start: start,
            input: solve::TensorInputKind::P,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::LoadP {
            dst: start + count as u32,
            index: count,
        },
        solve::LinearOp::LoadP {
            dst: start + count as u32 + 1,
            index: count + 1,
        },
        solve::LinearOp::LoadP {
            dst: start + count as u32 + 2,
            index: count + 2,
        },
        solve::LinearOp::PureCall {
            dst_start: call,
            input_starts: vec![
                start,
                start + count as u32,
                start + count as u32 + 1,
                start + count as u32 + 2,
            ]
            .into_boxed_slice(),
            site: site.clone(),
        },
    ];
    let residual = call + count as u32 + 3;
    let value = call + output_offset as u32;
    if width == 1 {
        ops.push(solve::LinearOp::Binary {
            dst: residual,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: value,
        });
        ops.push(solve::LinearOp::StoreOutput { src: residual });
    } else {
        ops.push(solve::LinearOp::TensorBinary {
            dst_start: residual,
            op: solve::BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: value,
            count: width,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        });
        ops.push(solve::LinearOp::StoreOutputRange {
            start: residual,
            count: width,
            stride: 1,
        });
    }
    ops
}

struct ProgramRunner {
    store: Store<usize>,
    memory: Memory,
    call: TypedFunc<(i32, i32, f64, i32, i32), i32>,
    p: usize,
    scratch: usize,
    y_count: usize,
    total: usize,
}

impl ProgramRunner {
    fn new(
        compiled: &rumoca_exec_wasm::CompiledNativeCallProgramWasm,
        layout: &solve::VarLayout,
    ) -> Self {
        let engine = Engine::default();
        let module = Module::new(&engine, compiled.module_bytes()).unwrap();
        let mut store = Store::new(&engine, 0usize);
        let p = layout.y_scalars() * 8;
        let scratch = p + layout.p_scalars() * 8;
        let total = scratch + compiled.scratch_bytes() as usize;
        let memory = Memory::new(
            &mut store,
            MemoryType::new(total.div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        suite_typed_calls::math::bind(&mut linker);
        linker
            .func_wrap(
                "env",
                "pow",
                |mut caller: wasmi::Caller<'_, usize>, b: f64, e: f64| {
                    *caller.data_mut() += 1;
                    b.powf(e)
                },
            )
            .unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let call = instance.get_typed_func(&store, "eval_assignments").unwrap();
        Self {
            store,
            memory,
            call,
            p,
            scratch,
            y_count: layout.y_scalars(),
            total,
        }
    }

    fn run(&mut self, parameters: &[f64]) -> (i32, Vec<f64>) {
        self.memory
            .write(&mut self.store, 0, &vec![0xa5; self.p])
            .unwrap();
        let bytes = parameters
            .iter()
            .flat_map(|v| v.to_le_bytes())
            .collect::<Vec<_>>();
        self.memory.write(&mut self.store, self.p, &bytes).unwrap();
        // Deliberately dirty scratch: each invocation must reset its own memo flags.
        self.memory
            .write(
                &mut self.store,
                self.scratch,
                &vec![0x7f; self.total - self.scratch],
            )
            .unwrap();
        let status = self
            .call
            .call(
                &mut self.store,
                (0, self.p as i32, 0., self.scratch as i32, 0),
            )
            .unwrap();
        let mut output = vec![0; self.y_count * 8];
        self.memory.read(&self.store, 0, &mut output).unwrap();
        let mut unchanged = vec![0; bytes.len()];
        self.memory
            .read(&self.store, self.p, &mut unchanged)
            .unwrap();
        assert_eq!(unchanged, bytes);
        if status != 0 {
            assert_eq!(
                output,
                vec![0xa5; self.p],
                "late failure published an earlier stage"
            );
        }
        (
            status,
            output
                .chunks_exact(8)
                .map(|c| f64::from_le_bytes(c.try_into().unwrap()))
                .collect(),
        )
    }
}

#[test]
fn identical_complete_coordinates_call_once_but_changed_y_arguments_are_evaluated_fresh() {
    let (table, site) = suite_typed_calls::square::square_table(Some(2.));
    let mut programs = Vec::new();
    for target in 0..4 {
        programs.push(vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: target,
            },
            if target == 3 {
                solve::LinearOp::LoadY { dst: 1, index: 2 }
            } else {
                solve::LinearOp::LoadP { dst: 1, index: 0 }
            },
            solve::LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![1].into_boxed_slice(),
                site: site.clone(),
            },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]);
    }
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(programs, vec![span(900); 4]).unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 4, 1);
    let targets = (0..4)
        .map(|i| Some(solve::scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let mut owners = solve::ContinuousRefreshOwners::default();
    owners
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    let compiled = compile_native_assignment_schedule_with_calls_wasm(
        owners.native_assignment_schedule().unwrap(),
        &layout,
        &table,
    )
    .unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    for value in [3., -0., 7.] {
        *runner.store.data_mut() = 0;
        let (status, actual) = runner.run(&[value]);
        assert_eq!(status, 0);
        assert_eq!(
            actual,
            [
                value.powf(2.),
                value.powf(2.),
                value.powf(2.),
                value.powf(2.).powf(2.)
            ]
        );
        assert_eq!(
            *runner.store.data(),
            2,
            "three identical P call tuples must share one complete invocation; Y-dependent call must remain fresh"
        );
    }
}

#[test]
fn complete_call_tuple_runs_once_per_immutable_coordinate_and_preserves_full_domain() {
    let mut sizes = Vec::new();
    for count in [4, 14400] {
        let (schedule, layout, table, site) = fixture(count);
        let compiled =
            compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
        sizes.push(compiled.module_bytes().len());
        let mut runner = ProgramRunner::new(&compiled, &layout);
        for (index, value, flag) in [
            (1., 12.7, 1.),
            (count as f64, -8.3, 0.),
            (2., -0., f64::NAN),
        ] {
            let mut parameters = (0..count)
                .map(|n| (n as f64 - 7.) / 13.)
                .collect::<Vec<_>>();
            parameters.extend([index, value, flag]);
            let inputs = vec![
                parameters[..count].iter().map(|&v| real(v)).collect(),
                vec![solve::SolveValueKind::Integer(index as i64)],
                vec![real(value)],
                vec![solve::SolveValueKind::Boolean(flag != 0.)],
            ];
            let typed = site
                .inputs()
                .iter()
                .zip(inputs)
                .map(|(ty, values)| TypedValue::construct(ty.clone(), values).unwrap())
                .collect::<Vec<_>>();
            let expected = eval_pure_call(&table, site.owner(), &typed).unwrap();
            let (status, actual) = runner.run(&parameters);
            assert_eq!(status, 0);
            assert_eq!(actual[0].to_bits(), value.to_bits());
            let flattened = expected
                .iter()
                .flat_map(|v| v.elements())
                .map(|v| match *v {
                    solve::SolveValueKind::Real64(bits) => f64::from_bits(bits),
                    solve::SolveValueKind::Integer(i) => i as f64,
                    solve::SolveValueKind::Boolean(b) => f64::from(u8::from(b)),
                    _ => panic!("Binary64 fixture"),
                })
                .collect::<Vec<_>>();
            assert_eq!(
                actual[1..].iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
                flattened.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
            );
        }
    }
    assert!(
        sizes[1].abs_diff(sizes[0]) < 1024,
        "full-domain calls must remain compact: {sizes:?}"
    );
}

#[test]
fn value_independence_cannot_hide_a_call_fault_dependent_on_its_own_unknown_target() {
    let p = profile();
    let ty = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut builder = solve::SolvePureCallTable::builder(p);
    let owner = builder
        .add_owner(
            identity(900),
            vec![ty.clone()],
            vec![solve::SolvePureCallOutput::result(ty)],
            span(950),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(951))?;
                // The result is constant, but conversion must still run and can fail.
                b.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    x,
                    span(952),
                )?;
                let one = b.constant(solve::SolveValue::real(p, 1.), span(953))?;
                b.store(outputs[0], one, span(954))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::PureCall {
                        dst_start: 1,
                        input_starts: vec![0].into_boxed_slice(),
                        site,
                    },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ]],
                vec![span(955)],
            )
            .unwrap(),
        )],
    };
    let mut owners = solve::ContinuousRefreshOwners::default();
    let error = owners
        .issue_native_assignment_schedule(
            &source,
            &[Some(solve::scalar_slot_y(0))],
            &solve::VarLayout::from_parts(Default::default(), 1, 0),
        )
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "native call inputs depend on its own assignment target"
    );
    assert!(owners.native_assignment_schedule().is_none());
}

#[test]
fn input_conversion_late_checked_fault_and_buffer_alias_never_publish_and_recover() {
    let (schedule, layout, table, _) = fixture(4);
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    for (index, value) in [
        (1.5, 3.),
        (f64::NAN, 3.),
        (5., 3.),
        (1., f64::NAN),
        (1., f64::INFINITY),
    ] {
        let (status, _) = runner.run(&[1., 2., 3., 4., index, value, 1.]);
        assert_ne!(status, 0);
        if index == 1.5 || index.is_nan() {
            assert_eq!(status, 2);
        } else {
            assert!(compiled.faults().iter().any(|f| f.status == status as u32));
        }
        assert_eq!(runner.run(&[1., 2., 3., 4., 1., 3., 1.]).0, 0);
    }
    for scratch in [
        0,
        runner.p as i32,
        runner.scratch as i32 + 1,
        runner.memory.data(&runner.store).len() as i32,
    ] {
        let before = runner.memory.data(&runner.store)[..runner.p].to_vec();
        assert_eq!(
            runner
                .call
                .call(&mut runner.store, (0, runner.p as i32, 0., scratch, 0))
                .unwrap(),
            1
        );
        assert_eq!(runner.memory.data(&runner.store)[..runner.p], before);
    }
    let (_, _, other, _) = fixture(5);
    assert!(
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &other).is_err()
    );
}
