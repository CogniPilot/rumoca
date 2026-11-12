//! The kinematic entry against the complete entry and the original values.
use crate::compile_fmi_derivative_operation_wasm;
use rumoca_ir_solve::fmi::{FmiComponent, FmiDerivativeStatus as Status};
use wasmi::{Engine, Linker, Memory, MemoryType as WasmiMemory, Store, TypedFunc};

/// `der(q) = w`, `der(w) = -a`, `der(s) = a` with `a = k*q`: `w` is the
/// velocity of `q`, and `q` and `s` read nothing that `w` moves.
const MIXED: &str = "model Mixed parameter Real k=2; Real q(start=1,fixed=true); \
    Real w(start=0.5,fixed=true); Real s(start=0,fixed=true); Real a; equation a=k*q; \
    der(q)=w; der(w)=-a; der(s)=a; end Mixed;";
/// No velocity coordinate and no relation: every state is kinematic.
const FULL: &str = "model Full Real x[2](each start=1,each fixed=true); equation \
    der(x[1])=-x[1]; der(x[2])=x[1]-x[2]; end Full;";
/// A relation in every row: no state is kinematic.
const EMPTY: &str = "model Empty Real x(start=1,fixed=true); Real v(start=0,fixed=true); \
    equation der(x)=if x>0 then v else -v; der(v)=if x>0 then -x else x; end Empty;";

fn component(name: &str, source: &str) -> FmiComponent {
    let mut session = rumoca_compile::Session::default();
    session.add_document(&format!("{name}.mo"), source).unwrap();
    let compiled = session.compile_model(name).unwrap();
    rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &Default::default()).unwrap()
}

type Complete = TypedFunc<(i32, i32, f64, i32, i32, i32, i32, i32, i32), i32>;
type Kinematic = TypedFunc<(i32, i32, f64, i32, i32), i32>;

struct Runner {
    store: Store<()>,
    memory: Memory,
    complete: Complete,
    kinematic: Kinematic,
    at: [usize; 8],
}

impl Runner {
    fn new(code: &crate::CompiledFmiDerivativeOperationWasm) -> Self {
        let engine = Engine::default();
        let mut store = Store::new(&engine, ());
        let mut at = [0usize; 8];
        let mut end = 64usize;
        for (arg, bytes) in code.workspace().regions() {
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
        for (name, artifact, export) in [
            ("static_refresh", code.static_program(), "eval_assignments"),
            (
                "dynamic_refresh",
                code.dynamic_program(),
                "eval_assignments",
            ),
            ("values", Some(code.values()), "eval_values"),
            ("kinematic_values", code.kinematic(), "eval_values"),
        ] {
            let Some(artifact) = artifact else { continue };
            let module = wasmi::Module::new(&engine, artifact.module_bytes()).unwrap();
            let instance = linker
                .instantiate(&mut store, &module)
                .unwrap()
                .start(&mut store)
                .unwrap();
            let function = instance.get_func(&store, export).unwrap();
            linker.define("operation", name, function).unwrap();
        }
        let module = wasmi::Module::new(&engine, code.module_bytes()).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let complete = instance.get_typed_func(&store, "eval_derivatives").unwrap();
        let kinematic = instance.get_typed_func(&store, "eval_kinematic").unwrap();
        Self {
            store,
            memory,
            complete,
            kinematic,
            at,
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
    fn run_complete(&mut self, time: f64) -> i32 {
        let a = self.at;
        let args = (
            a[0] as i32,
            a[1] as i32,
            time,
            a[3] as i32,
            a[4] as i32,
            a[5] as i32,
            a[6] as i32,
            a[7] as i32,
            0,
        );
        self.complete.call(&mut self.store, args).unwrap()
    }
    fn run_kinematic(&mut self, time: f64) -> i32 {
        let a = self.at;
        let args = (a[0] as i32, a[1] as i32, time, a[3] as i32, a[4] as i32);
        self.kinematic.call(&mut self.store, args).unwrap()
    }
}

fn original_values(
    op: &rumoca_ir_solve::fmi::FmiDerivativeCausalOperation<'_>,
    y: &[f64],
    p: &[f64],
    time: f64,
) -> Vec<u64> {
    let source = op.source();
    let values = rumoca_eval_solve::to_scalar_program_block(source.values()).unwrap();
    let mut out = vec![0.; values.output_count()];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &values,
        y,
        p,
        time,
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
fn kinematic_entry_is_issued_only_for_a_proper_partition() {
    for (name, source) in [("Full", FULL), ("Empty", EMPTY)] {
        let component = component(name, source);
        let root = component.runtime_view().retain_assertion_root();
        let op = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        assert!(op.kinematic().is_none(), "{name}");
        let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
        assert!(code.kinematic().is_none());
        let module = wasmparser::Parser::new(0)
            .parse_all(code.module_bytes())
            .filter_map(Result::ok)
            .filter_map(|payload| match payload {
                wasmparser::Payload::ExportSection(exports) => {
                    Some(exports.into_iter().filter_map(Result::ok).count())
                }
                _ => None,
            })
            .sum::<usize>();
        assert_eq!(
            module, 2,
            "{name} exports memory and the complete entry only"
        );
    }
}

#[test]
fn kinematic_rows_over_a_settled_coordinate_match_the_complete_values_bit_for_bit() {
    let component = component("Mixed", MIXED);
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    let kinematic = op.kinematic().expect("a proper partition issues the entry");
    let rows = kinematic.rows().iter().collect::<Vec<_>>();
    let states = op.source().vector_extent();
    assert!(!rows.is_empty() && rows.len() < states);
    let code = compile_fmi_derivative_operation_wasm(&op).unwrap();
    let plan = op
        .control_plan(
            code.static_program()
                .into_iter()
                .chain(code.dynamic_program())
                .chain([code.values()])
                .chain(code.kinematic())
                .map(|program| program.scratch_bytes()),
            rumoca_ir_solve::CheckedNativeWasmCapacityPolicy::default(),
        )
        .unwrap();
    assert!(code.module_bytes().len() <= plan.encoded_byte_bound() as usize);
    let mut runner = Runner::new(&code);
    let y_cells = op.source().problem().layout.y_scalars();
    let p = root.model().parameters.to_vec();
    let mut y = root.model().initial_y.to_vec();
    y[0] = 1.25;
    y[1] = -0.0;
    runner.put(0, &y);
    runner.put(1, &p);
    assert_eq!(runner.run_complete(0.5), Status::Success as i32);
    let complete = runner.bits(3, states);
    let settled = runner
        .bits(0, y_cells)
        .into_iter()
        .map(f64::from_bits)
        .collect::<Vec<_>>();
    // Every state other than the kinematic ones moves; the kinematic rows
    // still read only what the settled refresh left.
    let mut moved = settled.clone();
    for state in (0..states).filter(|state| !rows.contains(state)) {
        moved[state] = 3.0 + state as f64;
    }
    for (coordinate, expected) in [
        (&settled, complete.clone()),
        (&moved, original_values(&op, &moved, &p, 0.5)),
    ] {
        runner.put(0, coordinate);
        runner.put(3, &vec![91.0; states]);
        assert_eq!(runner.run_kinematic(0.5), Status::Success as i32);
        assert_eq!(runner.record(0), Status::Success as u32);
        let out = runner.bits(3, states);
        for state in 0..states {
            if rows.contains(&state) {
                assert_eq!(out[state], expected[state], "kinematic row {state}");
            } else {
                assert_eq!(out[state], 91.0f64.to_bits(), "row {state} is not written");
            }
        }
        assert_eq!(
            runner.bits(0, y_cells),
            coordinate.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            "the kinematic entry never writes y"
        );
    }
    // A non-finite kinematic row is named as the complete entry names it.
    let velocity = (0..states).find(|state| !rows.contains(state)).unwrap();
    let mut poisoned = settled;
    poisoned[velocity] = f64::NAN;
    let original = original_values(&op, &poisoned, &p, 0.5);
    let first = rows
        .iter()
        .copied()
        .find(|&row| !f64::from_bits(original[row]).is_finite())
        .expect("a kinematic row reads the velocity");
    runner.put(0, &poisoned);
    assert_eq!(runner.run_kinematic(0.5), Status::NonFinite as i32);
    assert_eq!(runner.record(0), Status::NonFinite as u32);
    assert_eq!(runner.record(8), first as u32);
}

#[test]
fn an_unissued_kinematic_entry_carries_its_typed_reason() {
    use rumoca_ir_solve::fmi::FmiKinematicRefusal;
    for (name, source, refusal, reason) in [
        (
            "Full",
            FULL,
            FmiKinematicRefusal::EveryStateKinematic,
            "every state is kinematic",
        ),
        (
            "Empty",
            EMPTY,
            FmiKinematicRefusal::NoKinematicStates,
            "the kinematic partition is empty",
        ),
    ] {
        let component = component(name, source);
        let root = component.runtime_view().retain_assertion_root();
        let op = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        let issued = op.kinematic_issuance();
        assert_eq!(issued.err(), Some(refusal), "{name}");
        // The reason an adoption receipt reports for the unbound entry.
        assert_eq!(
            issued.map(|_| ()).map_err(|refusal| refusal.to_string()),
            Err(reason.to_owned()),
            "{name}"
        );
    }
    let component = component("Mixed", MIXED);
    let root = component.runtime_view().retain_assertion_root();
    let op = root
        .try_derivative_operation()
        .unwrap()
        .try_causal_operation()
        .unwrap();
    assert!(op.kinematic_issuance().is_ok());
}
