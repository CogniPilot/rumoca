use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_eval_solve::{
    RowEvalContext, eval_scalar_program_block_with_context, to_scalar_program_block,
};
use rumoca_ir_solve as solve;
use wasmi::{Engine, Linker, Memory, MemoryType as RuntimeMemory, Store, TypedFunc};

fn span() -> rumoca_core::Span {
    solve::source_span_from_offsets(77, 20, 40)
}

fn calls() -> (SolvePureCallTable, solve::SolvePureCallSite) {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut table = SolvePureCallTable::builder(profile);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(77).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(),
            |body, inputs, outputs| {
                let x = body.load(inputs[0], span())?;
                let i = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    x,
                    span(),
                )?;
                let value =
                    body.convert(solve::SolveConversionOperator::IntegerToReal, i, span())?;
                let two = body.constant(solve::SolveValue::real(profile, 2.0), span())?;
                let value = body.binary(solve::SolveBinaryOperator::Power, value, two, span())?;
                body.store(outputs[0], value, span())
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn scalar(ops: Vec<LinearOp>) -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_program_spans(vec![ops], vec![span()]).unwrap(),
    )
}

fn mapped(site: solve::SolvePureCallSite, count: i64) -> ComputeNode {
    ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: 0,
                display_name: "point".into(),
                lower: 1,
                upper: count,
                step: 1,
            }],
        },
        output_map: solve::TensorOutputMap {
            start: 0,
            strides: vec![solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 2,
            }],
        },
        base_ops: vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into_boxed_slice(),
                site,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        load_strides: vec![solve::AffineStencilLoadStride {
            op_position: 0,
            terms: vec![solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: Default::default(),
        span: span(),
    }
}

struct Runner {
    store: Store<usize>,
    memory: Memory,
    entry: TypedFunc<(i32, i32, f64, i32, i32), i32>,
    p: usize,
    scratch: usize,
}

impl Runner {
    fn new(artifact: &CompiledNativeCallProgramWasm, layout: &VarLayout) -> Self {
        let engine = Engine::default();
        let module = wasmi::Module::new(&engine, artifact.module_bytes()).unwrap();
        let mut store = Store::new(&engine, 0usize);
        let p = layout.y_scalars() * 8;
        let scratch = p + layout.p_scalars() * 8;
        let bytes = scratch + artifact.scratch_bytes() as usize;
        let memory = Memory::new(
            &mut store,
            RuntimeMemory::new(bytes.div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker
            .func_wrap(
                "env",
                "pow",
                |mut caller: wasmi::Caller<'_, usize>, x: f64, y: f64| {
                    *caller.data_mut() += 1;
                    x.powf(y)
                },
            )
            .unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let entry = instance.get_typed_func(&store, "eval_values").unwrap();
        Self {
            store,
            memory,
            entry,
            p,
            scratch,
        }
    }

    fn run(&mut self, y: &[f64], p: &[f64], outputs: usize) -> (i32, Vec<u64>) {
        for (base, values) in [(0, y), (self.p, p)] {
            for (offset, value) in values.iter().enumerate() {
                self.memory
                    .write(
                        &mut self.store,
                        base + offset * 8,
                        &value.to_bits().to_le_bytes(),
                    )
                    .unwrap();
            }
        }
        let status = self
            .entry
            .call(
                &mut self.store,
                (0, self.p as i32, 0.0, self.scratch as i32, 0),
            )
            .unwrap();
        let bits = self.memory.data(&self.store)[self.scratch..self.scratch + outputs * 8]
            .chunks_exact(8)
            .map(|v| u64::from_le_bytes(v.try_into().unwrap()))
            .collect();
        (status, bits)
    }
}

fn oracle(block: &ComputeBlock, table: &SolvePureCallTable, y: &[f64], p: &[f64]) -> Vec<u64> {
    let scalar = to_scalar_program_block(block).unwrap();
    let mut out = vec![0.0; scalar.output_count()];
    eval_scalar_program_block_with_context(
        &scalar,
        y,
        p,
        0.0,
        RowEvalContext {
            pure_calls: Some(table),
            ..Default::default()
        },
        &mut out,
    )
    .unwrap();
    out.into_iter().map(f64::to_bits).collect()
}

#[test]
fn compact_call_points_keep_strided_inputs_sparse_outputs_and_ieee_bits() {
    let (table, site) = calls();
    let block = ComputeBlock {
        nodes: vec![
            mapped(site, 3),
            scalar(vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::StoreOutput { src: 1 },
            ]),
        ],
    };
    let layout = VarLayout::from_parts(Default::default(), 2, 3);
    let artifact = emit_values(&block, &layout, &table).unwrap();
    let mut runner = Runner::new(&artifact, &layout);
    for (iteration, last) in [
        0.0,
        -0.0,
        f64::from_bits(1),
        f64::INFINITY,
        f64::from_bits(0x7ff8_1234_5678_9abc),
    ]
    .into_iter()
    .enumerate()
    {
        let y = [-0.0, last];
        let p = [-3.75, 2.25, 7.9];
        let expected = oracle(&block, &table, &y, &p);
        assert_eq!(runner.run(&y, &p, expected.len()), (0, expected));
        assert_eq!(*runner.store.data(), 3 * (iteration + 1));
        let inputs = runner.memory.data(&runner.store)[..runner.scratch]
            .chunks_exact(8)
            .map(|v| u64::from_le_bytes(v.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(
            inputs,
            y.into_iter().chain(p).map(f64::to_bits).collect::<Vec<_>>()
        );
    }
}

#[test]
fn later_typed_fault_wins_over_an_earlier_nonfinite_value_and_instance_recovers() {
    let (table, site) = calls();
    let block = ComputeBlock {
        nodes: vec![scalar(vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![1].into_boxed_slice(),
                site,
            },
            LinearOp::StoreOutput { src: 2 },
        ])],
    };
    let layout = VarLayout::from_parts(Default::default(), 1, 1);
    let artifact = emit_values(&block, &layout, &table).unwrap();
    let fault = artifact
        .faults()
        .iter()
        .find(|f| f.kind == crate::TypedCallFaultKind::IntegerConversion)
        .unwrap();
    let mut runner = Runner::new(&artifact, &layout);
    let bad = runner.run(&[f64::NAN], &[1e100], 2);
    assert_eq!(bad.0 as u32, fault.status);
    assert!(f64::from_bits(bad.1[0]).is_nan());
    assert_eq!(fault.provenance, span());
    let expected = oracle(&block, &table, &[-0.0], &[-3.75]);
    assert_eq!(runner.run(&[-0.0], &[-3.75], 2), (0, expected));
}

#[test]
fn empty_domain_executes_no_call_and_bad_buffer_refuses_before_input_mutation() {
    let (table, site) = calls();
    let block = ComputeBlock {
        nodes: vec![mapped(site, 0)],
    };
    let layout = VarLayout::from_parts(Default::default(), 1, 1);
    let artifact = emit_values(&block, &layout, &table).unwrap();
    let mut runner = Runner::new(&artifact, &layout);
    assert_eq!(runner.run(&[-0.0], &[1e100], 0), (0, Vec::new()));
    assert_eq!(*runner.store.data(), 0);
    let before = runner.memory.data(&runner.store).to_vec();
    let status = runner
        .entry
        .call(&mut runner.store, (0, runner.p as i32, 0.0, 0, 0))
        .unwrap();
    assert_eq!(status, 1);
    assert_eq!(runner.memory.data(&runner.store), before);
}
