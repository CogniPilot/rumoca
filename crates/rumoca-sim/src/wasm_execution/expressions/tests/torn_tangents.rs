//! A torn block's tangent issued as one assignment-row schedule and compiled
//! by the portable backend over the issued slots. A host without an admitted
//! engine refuses the compiled call with its typed failure before writing a
//! slot.
use super::*;
use rumoca_eval_solve::to_scalar_program_block;

/// One nonlinear loop over `a`, `b` and `c`, torn like the first loop of the
/// tangent-Jacobian tests.
const LOOP: &str = "model PolynomialLoop
  Real x(start=1, fixed=true);
  Real a(start=1);
  Real b(start=1);
  Real c(start=1);
equation
  der(x) = -0.5*a;
  a = x + 0.2*c*c;
  b = a*a + 0.1*a;
  c = 1 + 0.1*b - 0.05*b*b + 0.1*c*c;
end PolynomialLoop;";

const NO_ENGINE: &str = "compiled WASM schedules have no admitted engine on this target";

fn lowered() -> rumoca_ir_solve::SolveModel {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document("input.mo", LOOP);
    let compiled = compiler.compile_model("PolynomialLoop").unwrap();
    crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap()
    .model()
    .clone()
}

fn backend(model: &rumoca_ir_solve::SolveModel) -> WasmExecutionBackend {
    WasmExecutionBackend::new(
        model.problem.layout.clone(),
        Rc::new(model.pure_calls.clone()),
    )
}

/// The backend compiles each torn block's issued schedule over the issued
/// slots as its Y, beside the model's parameters: a call over the model's own
/// Y is refused for its extent, and a call over the issued slots reaches the
/// engine, which this target does not admit, without writing any slot.
#[test]
fn the_backend_compiles_an_issued_tangent_schedule_over_its_own_slots() {
    let model = &lowered();
    let jvp = to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v).unwrap();
    let backend = backend(model);
    let y = model.initial_y.to_vec();
    let mut torn = 0;
    for block in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
    {
        let Some(tearing) = &block.tearing else {
            continue;
        };
        let plan = rumoca_ir_solve::TornTangentPlan::derive_directional(tearing, &jvp).unwrap();
        let schedule = plan.issue_schedule(&jvp, y.len()).unwrap();
        let total = schedule.slots().total;
        assert!(total > y.len(), "the slots extend the solver Y");
        let compiled = backend
            .compile_issued_assignment_rows(schedule.rows())
            .expect("the issued rows compile over their own slots");
        let mut model_y = y.clone();
        let refused = compiled
            .call(&mut model_y, &model.parameters, 0.0, &[])
            .unwrap_err();
        assert!(
            refused
                .to_string()
                .contains("WASM input lengths differ from the issued layout"),
            "{refused}"
        );
        let mut slots = vec![0.0; total];
        slots[..y.len()].copy_from_slice(&y);
        let before: Vec<u64> = slots.iter().map(|value| value.to_bits()).collect();
        let refused = compiled
            .call(&mut slots, &model.parameters, 0.0, &[])
            .unwrap_err();
        assert!(refused.to_string().contains(NO_ENGINE), "{refused}");
        assert_eq!(
            slots
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            before
        );
        torn += 1;
    }
    assert!(torn > 0, "the loop is solved as a torn block");
}
