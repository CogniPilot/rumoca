use super::*;
use crate::{CompiledFmiDerivativeOperationWasm, compile_fmi_derivative_operation_wasm};
use rumoca_ir_solve::{
    self as solve,
    fmi::{FmiComponent, FmiDerivativeStatus as Status, FmiDerivativeWorkspace},
};
use wasmi::{Engine, Linker, Memory, MemoryType as WasmiMemory, Store, TypedFunc};

fn component() -> FmiComponent {
    let mut session = rumoca_compile::Session::default();
    session.add_document("causal-entry.mo", "model CausalEntry parameter Real gain=2; Real x[2](each start=1,each fixed=true); Real a; Real b; equation a=3*gain; b=a*x[1]; der(x[1])=b; der(x[2])=b+x[2]; end CausalEntry;").unwrap();
    let compiled = session.compile_model("CausalEntry").unwrap();
    let lowered = rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &Default::default(),
        Default::default(),
        solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap();
    rumoca_phase_solve::fmi::finish_fmi_component(lowered).unwrap()
}

type Entry = TypedFunc<(i32, i32, f64, i32, i32, i32, i32, i32, i32), i32>;
#[derive(Default)]
struct State {
    calls: [usize; 3],
    fault: Option<usize>,
    trap: Option<usize>,
    nonfinite: bool,
}
struct Runner {
    store: Store<State>,
    memory: Memory,
    entry: Entry,
    at: [usize; 8],
    work: FmiDerivativeWorkspace,
}
impl Runner {
    fn new(
        op: &FmiDerivativeCausalOperation<'_>,
        code: &CompiledFmiDerivativeOperationWasm,
        stub: bool,
    ) -> Self {
        let engine = Engine::default();
        let mut store = Store::new(&engine, State::default());
        let w = code.workspace();
        let regions = w.regions();
        let mut at = [0usize; 8];
        let mut end = 64usize;
        for (arg, bytes) in regions {
            at[arg as usize] = end;
            end = (end + bytes as usize + 15) & !7;
        }
        let memory = Memory::new(
            &mut store,
            WasmiMemory::new(end.div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let artifacts = [
            ("static_refresh", code.static_program(), "eval_assignments"),
            (
                "dynamic_refresh",
                code.dynamic_program(),
                "eval_assignments",
            ),
            ("values", Some(code.values()), "eval_values"),
        ];
        for (index, (name, artifact, export)) in artifacts.into_iter().enumerate() {
            let Some(artifact) = artifact else { continue };
            if stub {
                let targets = op.static_targets().iter().collect::<Vec<_>>();
                let outputs = op.source().vector_extent();
                install_stub(&mut linker, name, index, targets, outputs);
            } else {
                let module = wasmi::Module::new(&engine, artifact.module_bytes()).unwrap();
                let instance = linker
                    .instantiate(&mut store, &module)
                    .unwrap()
                    .start(&mut store)
                    .unwrap();
                let function = instance.get_func(&store, export).unwrap();
                linker.define("operation", name, function).unwrap();
            }
        }
        let module = wasmi::Module::new(&engine, code.module_bytes()).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let entry = instance.get_typed_func(&store, "eval_derivatives").unwrap();
        Self {
            store,
            memory,
            entry,
            at,
            work: w,
        }
    }
    fn put(&mut self, arg: usize, values: &[f64]) {
        for (i, value) in values.iter().enumerate() {
            self.memory
                .write(
                    &mut self.store,
                    self.at[arg] + i * 8,
                    &value.to_bits().to_le_bytes(),
                )
                .unwrap();
        }
    }
    fn bits(&self, arg: usize, count: usize) -> Vec<u64> {
        self.memory.data(&self.store)[self.at[arg]..self.at[arg] + count * 8]
            .chunks_exact(8)
            .map(|n| u64::from_le_bytes(n.try_into().unwrap()))
            .collect()
    }
    fn record(&self, offset: usize) -> u32 {
        u32::from_le_bytes(
            self.memory.data(&self.store)[self.at[4] + offset..self.at[4] + offset + 4]
                .try_into()
                .unwrap(),
        )
    }
    fn run(&mut self, valid: bool) -> i32 {
        let a = self.at;
        self.entry
            .call(
                &mut self.store,
                (
                    a[0] as i32,
                    a[1] as i32,
                    0.25,
                    a[3] as i32,
                    a[4] as i32,
                    a[5] as i32,
                    a[6] as i32,
                    a[7] as i32,
                    i32::from(valid),
                ),
            )
            .unwrap()
    }
}

fn oracle(op: &FmiDerivativeCausalOperation<'_>, mut y: Vec<f64>, p: &[f64]) -> Vec<u64> {
    let source = op.source();
    let problem = source.problem();
    for schedule in op
        .static_schedule()
        .into_iter()
        .chain(op.dynamic_schedule())
    {
        for &id in schedule.program_ids() {
            let owner = problem
                .continuous
                .refresh_owners
                .exact_assignment_program(id)
                .unwrap();
            let block = owner
                .final_scalar_program(&problem.continuous.implicit_rhs)
                .unwrap();
            let mut out = vec![0.; block.output_count()];
            rumoca_eval_solve::eval_scalar_program_block_with_context(
                &block,
                &y,
                p,
                0.25,
                rumoca_eval_solve::RowEvalContext {
                    pure_calls: Some(source.calls()),
                    ..Default::default()
                },
                &mut out,
            )
            .unwrap();
            for (&target, value) in owner.target_indices().iter().zip(out) {
                y[target] = value;
            }
        }
    }
    let values = rumoca_eval_solve::to_scalar_program_block(source.values()).unwrap();
    let mut out = vec![0.; values.output_count()];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &values,
        &y,
        p,
        0.25,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(source.calls()),
            ..Default::default()
        },
        &mut out,
    )
    .unwrap();
    out.into_iter().map(f64::to_bits).collect()
}

#[test]
fn complete_entry_links_real_guest_helpers_and_matches_original_operations() {
    let component = component();
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    assert!(op.static_schedule().is_some());
    assert!(op.dynamic_schedule().is_some());
    let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
    let plan = op
        .control_plan(
            [code.values().scratch_bytes()],
            solve::CheckedNativeWasmCapacityPolicy::default(),
        )
        .unwrap();
    assert!(code.module_bytes().len() <= plan.encoded_byte_bound() as usize);
    let mut runner = Runner::new(&op, &code, false);
    let mut y = root.model().initial_y.to_vec();
    let mut p = root.model().parameters.to_vec();
    y[0] = 1.5;
    y[1] = -0.;
    for trial in 0..3 {
        if trial == 2 {
            p[op.parameter_keys()[0]] += 1.;
        }
        runner.put(0, &y);
        runner.put(1, &p);
        assert_eq!(runner.run(trial != 0), Status::Success as i32);
        assert_eq!(
            runner.bits(3, op.source().vector_extent()),
            oracle(&op, y.clone(), &p)
        );
        y[0] += 0.5;
    }
}

#[test]
fn refresh_faults_restore_y_but_later_value_faults_keep_refreshed_y() {
    let component = component();
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
    for fault in 0..3 {
        let mut r = Runner::new(&op, &code, true);
        let y = vec![1.; op.source().problem().layout.y_scalars()];
        r.put(0, &y);
        r.put(1, &root.model().parameters);
        r.put(3, &[91., 92.]);
        r.store.data_mut().fault = Some(fault);
        let status = r.run(false);
        assert_eq!(
            status,
            [
                Status::StaticRefresh,
                Status::DynamicRefresh,
                Status::Values
            ][fault] as i32
        );
        assert_eq!(r.record(4), 17 + fault as u32);
        assert_eq!(r.record(12), 1);
        assert_eq!(r.bits(3, 2), [91.0f64.to_bits(), 92.0f64.to_bits()]);
        if fault < 2 {
            assert_eq!(
                r.bits(0, y.len()),
                y.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
            );
        } else {
            assert_eq!(r.bits(0, 1), [13.0f64.to_bits()]);
        }
    }
}

#[test]
fn cache_hit_key_bits_invalidation_and_nonfinite_diagnostic_order_are_preserved() {
    let component = component();
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
    let mut r = Runner::new(&op, &code, true);
    let y = vec![1.; op.source().problem().layout.y_scalars()];
    let mut p = root.model().parameters.to_vec();
    let key = op.parameter_keys()[0];
    p[key] = 0.;
    r.put(0, &y);
    r.put(1, &p);
    assert_eq!(r.run(false), 0);
    assert_eq!(r.store.data().calls, [1, 1, 1]);
    r.put(0, &y);
    assert_eq!(r.run(true), 0);
    assert_eq!(r.store.data().calls, [1, 2, 2]);
    p[key] = -0.;
    r.put(1, &p);
    assert_eq!(r.run(true), 0);
    assert_eq!(r.store.data().calls, [2, 3, 3]);
    r.store.data_mut().nonfinite = true;
    assert_eq!(r.run(true), Status::NonFinite as i32);
    assert_eq!(r.record(8), 0);
    assert_eq!(r.bits(3, 2), [0x7ff8000000000042, 3.0f64.to_bits()]);
    r.store.data_mut().fault = Some(2);
    r.put(3, &[91., 92.]);
    assert_eq!(r.run(true), Status::Values as i32);
    assert_eq!(r.bits(3, 2), [91.0f64.to_bits(), 92.0f64.to_bits()]);
    let before = r.memory.data(&r.store).to_vec();
    let a = r.at;
    let bad = r
        .entry
        .call(
            &mut r.store,
            (
                a[0] as i32,
                a[1] as i32,
                0.,
                a[0] as i32,
                a[4] as i32,
                a[5] as i32,
                a[6] as i32,
                a[7] as i32,
                1,
            ),
        )
        .unwrap();
    assert_eq!(bad, Status::InvalidBuffers as i32);
    assert_eq!(r.memory.data(&r.store), before);
    assert!(r.work.bytes() > r.work.y_bytes());
}

#[test]
fn a_projection_is_never_dropped_to_issue_the_causal_operation() {
    let mut session = rumoca_compile::Session::default();
    session.add_document("projection-entry.mo", "model ProjectionEntry Real x(start=0.1,fixed=true); Real y(start=0.1); equation sin(y)=x; der(x)=y; end ProjectionEntry;").unwrap();
    let compiled = session.compile_model("ProjectionEntry").unwrap();
    let component =
        rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &Default::default())
            .unwrap();
    let source = component.runtime_view().try_derivative_operation().unwrap();
    let Err(refusal) = source.try_causal_operation() else {
        panic!("the operation needs its projection");
    };
    assert!(matches!(
        refusal,
        rumoca_ir_solve::fmi::FmiDerivativeCausalRefusal::ProjectionRequired
    ));
    // The reason a host's adoption receipt reports for the unissued operation.
    assert_eq!(
        refusal.to_string(),
        "derivative operation requires its projection executor"
    );
}

#[test]
fn an_interrupted_helper_retains_its_issued_failure_effect_for_the_host_boundary() {
    let component = component();
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
    for trap in 0..3 {
        let mut r = Runner::new(&op, &code, true);
        let y = vec![1.0; op.source().problem().layout.y_scalars()];
        r.put(0, &y);
        r.put(1, &root.model().parameters);
        r.store.data_mut().trap = Some(trap);
        let a = r.at;
        let result = r.entry.call(
            &mut r.store,
            (
                a[0] as i32,
                a[1] as i32,
                0.0,
                a[3] as i32,
                a[4] as i32,
                a[5] as i32,
                a[6] as i32,
                a[7] as i32,
                0,
            ),
        );
        assert!(result.is_err());
        let phase = Status::from_code(r.record(0)).unwrap();
        assert_eq!(
            phase,
            [
                Status::StaticRefresh,
                Status::DynamicRefresh,
                Status::Values
            ][trap]
        );
        assert_eq!(phase.restores_coordinate(), trap < 2);
        assert_eq!(phase.has_evaluated_values(), trap == 2);
    }
}

fn install_stub(
    linker: &mut Linker<State>,
    name: &str,
    index: usize,
    targets: Vec<usize>,
    outputs: usize,
) {
    linker
        .func_wrap(
            "operation",
            name,
            move |caller: wasmi::Caller<'_, State>,
                  y: i32,
                  _p: i32,
                  _t: f64,
                  scratch: i32,
                  _unused: i32| {
                invoke_stub(caller, index, &targets, outputs, y, scratch)
            },
        )
        .unwrap();
}
fn invoke_stub(
    mut caller: wasmi::Caller<'_, State>,
    index: usize,
    targets: &[usize],
    outputs: usize,
    y: i32,
    scratch: i32,
) -> Result<i32, wasmi::Error> {
    caller.data_mut().calls[index] += 1;
    let fault = caller.data().fault == Some(index);
    let nonfinite = caller.data().nonfinite;
    let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
    match index {
        0 => {
            for &target in targets {
                mem.write(&mut caller, y as usize + target * 8, &7.0f64.to_le_bytes())
                    .unwrap();
            }
        }
        1 => mem
            .write(&mut caller, y as usize, &13.0f64.to_le_bytes())
            .unwrap(),
        _ => {
            for output in 0..outputs {
                let bits = if nonfinite && output == 0 {
                    0x7ff8000000000042
                } else {
                    (2.0 + output as f64).to_bits()
                };
                mem.write(
                    &mut caller,
                    scratch as usize + output * 8,
                    &bits.to_le_bytes(),
                )
                .unwrap();
            }
        }
    }
    if caller.data().trap == Some(index) {
        return Err(wasmi::Error::new("injected engine trap"));
    }
    Ok(if fault { 17 + index as i32 } else { 0 })
}
