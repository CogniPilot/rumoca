//! Real directly emitted WASM hooks inside the existing Rust ME lifecycle.

use std::{cell::Cell, cell::RefCell, rc::Rc};

use rumoca_ir_solve as solve;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

use super::{fixture_instance_config, harmonic_oscillator, refresh_owned};
use crate::fmi_me::{MeExecutionBackend, MeModelSource, MeTime, SolveMeKernel};
use crate::{CompiledSolveExpression, PrecompiledSolveBackendBuilder, SolveExecutionBackend};

type Entry = TypedFunc<(i32, i32, f64, i32, i32), ()>;

struct WasmExpression {
    store: RefCell<Store<()>>,
    memory: Memory,
    entry: Entry,
    calls: Cell<usize>,
    fail: Cell<bool>,
}

impl WasmExpression {
    fn new(source: &solve::ScalarProgramBlock, layout: &solve::VarLayout) -> Self {
        let block = solve::ComputeBlock::from_scalar_program_block(source.clone());
        let bytes = rumoca_exec_wasm::compile_expression_compute_block_wasm_bytes(&block, layout)
            .expect("direct encoder accepts the checked harmonic expression");
        let engine = Engine::default();
        let module = Module::new(&engine, &bytes[..]).unwrap();
        let mut store = Store::new(&engine, ());
        let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let entry = instance.get_typed_func(&store, "eval_residual").unwrap();
        Self {
            store: RefCell::new(store),
            memory,
            entry,
            calls: Cell::new(0),
            fail: Cell::new(false),
        }
    }
}

impl CompiledSolveExpression for WasmExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        assert_eq!(y.len(), 2);
        assert_eq!(out.len(), 2);
        assert!(p.is_empty() && tables.is_empty());
        self.calls.set(self.calls.get() + 1);
        let mut store = self.store.borrow_mut();
        let input: Vec<_> = y.iter().flat_map(|v| v.to_le_bytes()).collect();
        self.memory
            .write(&mut *store, 0, &input)
            .map_err(|e| e.to_string())?;
        self.entry
            .call(
                &mut *store,
                (0, 16, t, 0, if self.fail.get() { -1 } else { 24 }),
            )
            .map_err(|e| format!("precompiled model hook failed: {e}"))?;
        let mut bytes = [0; 16];
        self.memory
            .read(&*store, 24, &mut bytes)
            .map_err(|e| e.to_string())?;
        for (value, bytes) in out.iter_mut().zip(bytes.chunks_exact(8)) {
            *value = f64::from_le_bytes(bytes.try_into().unwrap());
        }
        Ok(())
    }
}

fn derivative_source(model: &solve::SolveModel) -> &solve::ScalarProgramBlock {
    let [solve::ComputeNode::ScalarPrograms(source)] =
        model.problem.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("the retained fixture has one canonical scalar source");
    };
    source
}

fn model() -> solve::SolveModel {
    let mut model = harmonic_oscillator();
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 2, 0);
    refresh_owned(model)
}

#[test]
fn precompiled_pure_calls_bind_exact_table_without_execution_or_replacement() {
    use rumoca_eval_solve::{EvalSolveError, PureCallExecution, PureCallInvocation};
    struct Call(Cell<usize>);
    impl PureCallExecution for Call {
        fn call(
            &self,
            _: PureCallInvocation<'_>,
            input: &[f64],
            output: &mut [f64],
        ) -> Result<(), EvalSolveError> {
            self.0.set(self.0.get() + 1);
            output[0] = input[0];
            Ok(())
        }
    }
    let mut model = model();
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("RetainedCall.mo"),
        1,
        2,
    );
    model.pure_calls = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span,
            |body, inputs, outputs| {
                let value = body.load(inputs[0], span)?;
                body.store(outputs[0], value, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let call = Rc::new(Call(Cell::new(0)));
    let mut builder = PrecompiledSolveBackendBuilder::new(&model).unwrap();
    assert!(
        builder
            .pure_call_execution(&solve::SolvePureCallTable::default(), call.clone())
            .is_err()
    );
    builder
        .pure_call_execution(&model.pure_calls.clone(), call.clone())
        .unwrap();
    assert!(
        builder
            .pure_call_execution(&model.pure_calls, call.clone())
            .is_err()
    );
    let backend = builder.finish();
    assert_eq!(call.0.get(), 0);
    backend.validate_model_context(&model.clone()).unwrap();
    let site = model.pure_calls.owners()[0].call_site();
    let mut output = [0.0];
    backend
        .pure_call_execution()
        .unwrap()
        .call(PureCallInvocation::Primal(&site), &[3.0], &mut output)
        .unwrap();
    assert_eq!(call.0.get(), 1);
    assert_eq!(output, [3.0]);
}

#[test]
fn precompiled_hooks_keep_owner_identity_and_refuse_replacement() {
    let model = model();
    let source = derivative_source(&model);
    let hook = Rc::new(WasmExpression::new(source, &model.problem.layout));
    let mut builder = PrecompiledSolveBackendBuilder::new(&model).unwrap();
    builder.expression(source, hook.clone()).unwrap();
    assert!(builder.expression(&source.clone(), hook.clone()).is_err());
    let backend = builder.finish();
    assert!(backend.validate_model_context(&model.clone()).is_ok());
    assert!(backend.compile_expression(&source.clone()).is_ok());
    let replay: solve::ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(source).unwrap()).unwrap();
    assert!(backend.compile_expression(&replay).is_err());
    assert!(
        backend
            .compile_expression(derivative_source(&harmonic_oscillator()))
            .is_err()
    );
    assert!(backend.compile_jacobian_expression(source).is_err());
    assert_eq!(
        hook.calls.get(),
        0,
        "binding must not evaluate or compile the model again"
    );
}

#[test]
fn foreign_model_context_refuses_before_preparation_or_hook_execution() {
    let model = model();
    let hook = Rc::new(WasmExpression::new(
        derivative_source(&model),
        &model.problem.layout,
    ));
    let mut builder = PrecompiledSolveBackendBuilder::new(&model).unwrap();
    builder
        .expression(derivative_source(&model), hook.clone())
        .unwrap();
    let backend = Rc::new(builder.finish());
    let mut foreign_layout = model.clone();
    foreign_layout.problem.layout = solve::VarLayout::from_parts(
        [(String::from("tag"), solve::ScalarSlot::Constant(-0.0))].into(),
        2,
        0,
    );
    let mut foreign_calls = model.clone();
    foreign_calls.pure_calls = solve::SolvePureCallTable::default();
    let mut foreign_tables = model.clone();
    foreign_tables.external_tables = solve::ExternalTables::new(vec![Default::default()]);
    for foreign in [foreign_layout, foreign_calls, foreign_tables] {
        assert!(derivative_source(&model).shares_program_owner(derivative_source(&foreign)));
        let handle = MeExecutionBackend::new(backend.clone());
        let error = match SolveMeKernel::instantiate_with_execution_backend(
            MeModelSource::fixture(&foreign),
            &fixture_instance_config(),
            Some(handle),
        ) {
            Ok(_) => panic!("foreign execution context must not instantiate"),
            Err(error) => error,
        };
        assert!(
            error
                .to_string()
                .contains("compiled backend model context refused"),
            "{error}"
        );
    }
    assert_eq!(hook.calls.get(), 0);
}

#[test]
fn direct_wasm_hook_uses_rust_me_lifecycle_and_propagates_execution_failure() {
    let model = model();
    let source = derivative_source(&model);
    let hook = Rc::new(WasmExpression::new(source, &model.problem.layout));
    let mut builder = PrecompiledSolveBackendBuilder::new(&model).unwrap();
    builder.expression(source, hook.clone()).unwrap();
    let backend = MeExecutionBackend::new(Rc::new(builder.finish()));
    let mut kernel = SolveMeKernel::instantiate_with_execution_backend(
        MeModelSource::fixture(&model),
        &fixture_instance_config(),
        Some(backend.clone()),
    )
    .unwrap();
    kernel.enter_initialization_mode().unwrap();
    kernel.exit_initialization_mode().unwrap();
    kernel.update_discrete_states().unwrap();
    kernel.enter_continuous_time_mode().unwrap();
    let saved = kernel.fmu_state();
    kernel.set_time(MeTime::at(0.25)).unwrap();
    kernel.set_continuous_states(&[2., -3.]).unwrap();
    let mut values = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut values)
        .unwrap();
    assert_eq!(values, [-3., -8.]);
    let calls = hook.calls.get();
    kernel
        .get_continuous_state_derivatives(&mut values)
        .unwrap();
    assert_eq!(
        hook.calls.get(),
        calls,
        "existing ME derivative cache is shared"
    );
    hook.fail.set(true);
    kernel.set_time(MeTime::at(0.5)).unwrap();
    let error = kernel
        .get_continuous_state_derivatives(&mut values)
        .unwrap_err();
    assert!(error.to_string().contains("precompiled model hook failed"));
    assert_eq!(
        values,
        [-3., -8.],
        "a WASM trap must not publish output bytes"
    );
    assert_eq!(
        hook.calls.get(),
        calls + 1,
        "admitted hook failure must not run the evaluator"
    );
    hook.fail.set(false);
    kernel.reset_to_fmu_state(&saved).unwrap();
    kernel
        .get_continuous_state_derivatives(&mut values)
        .unwrap();
    assert_eq!(values, [0., -4.]);
    kernel.set_time(MeTime::at(0.25)).unwrap();
    kernel.set_continuous_states(&[2., -3.]).unwrap();
    kernel
        .get_continuous_state_derivatives(&mut values)
        .unwrap();
    assert_eq!(
        values,
        [-3., -8.],
        "snapshot restore preserves the same issued hooks on replay"
    );
    kernel.terminate().unwrap();
    assert!(kernel.enter_initialization_mode().is_err());
    assert!(
        backend
            .declined()
            .iter()
            .all(|entry| entry.program != "expression" || !entry.reason.contains("hook failed"))
    );
}
