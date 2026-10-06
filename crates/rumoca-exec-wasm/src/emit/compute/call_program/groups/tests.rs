//! Actual production Wasmi execution at forced target-local byte boundaries.
mod gathers;
mod transfer_cells;
use super::*;
use rumoca_core::{SourceId, Span};
use rumoca_eval_solve::{TypedValue, eval_pure_call};
use std::num::NonZeroU64;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

const COUNT: usize = 96;
const BUDGET: usize = 4096;
fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("outlined_calls.mo"),
        offset,
        offset + 1,
    )
}

fn fixture() -> (
    solve::NativeRefreshAssignmentSchedule,
    VarLayout,
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
) {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut table = solve::SolvePureCallTable::builder(profile);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(42).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(40),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(41))?;
                // This unused checked operation must still fail in a late group.
                builder.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    x,
                    span(42),
                )?;
                let exponent = builder.constant(solve::SolveValue::real(profile, 2.), span(43))?;
                let value =
                    builder.binary(solve::SolveBinaryOperator::Power, x, exponent, span(44))?;
                builder.store(outputs[0], value, span(45))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let rows = (0..COUNT)
        .map(|target| {
            vec![
                LinearOp::LoadY {
                    dst: 0,
                    index: target,
                },
                if target + 1 == COUNT {
                    LinearOp::LoadY { dst: 1, index: 0 }
                } else {
                    LinearOp::LoadP { dst: 1, index: 0 }
                },
                LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: vec![1].into_boxed_slice(),
                    site: site.clone(),
                },
                LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 2,
                },
                LinearOp::StoreOutput { src: 3 },
            ]
        })
        .collect();
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(rows, vec![span(50); COUNT]).unwrap(),
        )],
    };
    let layout = VarLayout::from_parts(Default::default(), COUNT, 1);
    let targets = (0..COUNT)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let owners =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    (owners, layout, table.finish(), site)
}

struct Runner {
    store: Store<usize>,
    memory: Memory,
    call: TypedFunc<(i32, i32, f64, i32, i32), i32>,
    p: usize,
    scratch: usize,
}
impl Runner {
    fn new(compiled: &CompiledNativeCallProgramWasm, layout: &VarLayout) -> Self {
        let engine = Engine::default();
        let module = Module::new(&engine, compiled.module_bytes()).unwrap();
        let mut store = Store::new(&engine, 0usize);
        let p = layout.y_scalars() * 8;
        let scratch = p + layout.p_scalars() * 8;
        let bytes = scratch + compiled.scratch_bytes() as usize;
        let memory = Memory::new(
            &mut store,
            MemoryType::new(bytes.div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker
            .func_wrap(
                "env",
                "pow",
                |mut caller: wasmi::Caller<'_, usize>, base: f64, exponent: f64| {
                    *caller.data_mut() += 1;
                    base.powf(exponent)
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
        }
    }
    fn run(&mut self, input: f64) -> (i32, Vec<u64>, usize) {
        self.memory
            .write(&mut self.store, 0, &vec![0xa5; self.p])
            .unwrap();
        self.memory
            .write(&mut self.store, self.p, &input.to_le_bytes())
            .unwrap();
        let total = self.memory.data(&self.store).len();
        self.memory
            .write(
                &mut self.store,
                self.scratch,
                &vec![0x7f; total - self.scratch],
            )
            .unwrap();
        *self.store.data_mut() = 0;
        let status = self
            .call
            .call(
                &mut self.store,
                (0, self.p as i32, 0., self.scratch as i32, 0),
            )
            .unwrap();
        assert_eq!(
            &self.memory.data(&self.store)[self.p..self.p + 8],
            &input.to_le_bytes()
        );
        let result = self.memory.data(&self.store)[..self.p]
            .chunks_exact(8)
            .map(|bytes| u64::from_le_bytes(bytes.try_into().unwrap()))
            .collect();
        if status != 0 {
            assert_eq!(&self.memory.data(&self.store)[..self.p], vec![0xa5; self.p]);
        }
        (status, result, *self.store.data())
    }
}

fn body_sizes(bytes: &[u8]) -> Vec<usize> {
    wasmparser::Parser::new(0)
        .parse_all(bytes)
        .filter_map(|payload| match payload.unwrap() {
            wasmparser::Payload::CodeSectionEntry(body) => Some(body.range().len()),
            _ => None,
        })
        .collect()
}
fn oracle(table: &solve::SolvePureCallTable, site: &solve::SolvePureCallSite, input: f64) -> u64 {
    let value = TypedValue::construct(
        site.inputs()[0].clone(),
        vec![solve::SolveValueKind::Real64(input.to_bits())],
    )
    .unwrap();
    let output = eval_pure_call(table, site.owner(), &[value]).unwrap();
    match output[0].elements()[0] {
        solve::SolveValueKind::Real64(bits) => bits,
        _ => panic!("Binary64 fixture"),
    }
}

#[test]
fn stage_groups_match_unbounded_emission_and_canonical_oracle_in_actual_wasmi() {
    let (schedule, layout, table, site) = fixture();
    let baseline = emit_with_budget(&schedule, &layout, &table, usize::MAX).unwrap();
    let bounded = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert!(body_sizes(bounded.module_bytes()).len() > body_sizes(baseline.module_bytes()).len());
    assert_eq!(baseline.scratch_bytes(), bounded.scratch_bytes());
    assert_eq!(baseline.math_imports(), bounded.math_imports());
    let mut before = Runner::new(&baseline, &layout);
    let mut after = Runner::new(&bounded, &layout);
    for input in [-0., 0., f64::from_bits(1), 1.25, -2.] {
        let actual = after.run(input);
        assert_eq!(actual, before.run(input));
        assert_eq!(actual.0, 0);
        assert_eq!(
            actual.2, 2,
            "P memo must survive group boundaries; Y call stays fresh"
        );
        let first = oracle(&table, &site, input);
        let second = oracle(&table, &site, f64::from_bits(first));
        assert_eq!(actual.1[..COUNT - 1], vec![first; COUNT - 1]);
        assert_eq!(actual.1[COUNT - 1], second);
    }
}

#[test]
fn stage_groups_late_fault_keeps_y_atomic_p_readonly_and_original_fault_identity() {
    let (schedule, layout, table, _) = fixture();
    let baseline = emit_with_budget(&schedule, &layout, &table, usize::MAX).unwrap();
    let bounded = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert_eq!(baseline.faults(), bounded.faults());
    let mut before = Runner::new(&baseline, &layout);
    let mut after = Runner::new(&bounded, &layout);
    let failed = after.run(1e10);
    assert_eq!(failed, before.run(1e10));
    assert_ne!(failed.0, 0);
    assert_eq!(
        failed.2, 1,
        "fault belongs to final Y-dependent group after earlier work"
    );
    let fault = bounded
        .faults()
        .iter()
        .find(|fault| fault.status == failed.0 as u32)
        .unwrap();
    assert_eq!(fault.provenance, span(42));
    assert_eq!(after.run(2.), before.run(2.));
}

#[test]
fn stage_groups_buffer_preflight_runs_before_any_group_or_memo_effect() {
    let (schedule, layout, table, _) = fixture();
    let bounded = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    let mut runner = Runner::new(&bounded, &layout);
    runner.run(2.);
    for scratch in [
        0,
        runner.p as i32,
        runner.scratch as i32 + 1,
        runner.memory.data(&runner.store).len() as i32,
    ] {
        let before = runner.memory.data(&runner.store).to_vec();
        *runner.store.data_mut() = 0;
        let result = runner
            .call
            .call(&mut runner.store, (0, runner.p as i32, 0., scratch, 0))
            .unwrap();
        assert_eq!(result, 1);
        assert_eq!(runner.memory.data(&runner.store), before);
        assert_eq!(*runner.store.data(), 0);
    }
}

#[test]
fn stage_groups_bound_actual_encoded_functions_and_keep_one_exported_entrypoint() {
    let (schedule, layout, table, _) = fixture();
    let bounded = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert!(
        body_sizes(bounded.module_bytes())
            .iter()
            .all(|size| *size <= BUDGET)
    );
    let mut exported = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bounded.module_bytes()) {
        let wasmparser::Payload::ExportSection(section) = payload.unwrap() else {
            continue;
        };
        for export in section {
            let export = export.unwrap();
            if export.kind == wasmparser::ExternalKind::Func {
                exported.push(export.name.to_owned());
            }
        }
    }
    assert_eq!(exported, ["eval_assignments"]);
    let error = emit_with_budget(&schedule, &layout, &table, 32).unwrap_err();
    assert!(error.contains("single native assignment stage"));
    let production = emit_native_call_assignment_module(&schedule, &layout, &table).unwrap();
    assert!(
        body_sizes(production.module_bytes())
            .iter()
            .all(|size| *size <= FUNCTION_BYTES)
    );
}
