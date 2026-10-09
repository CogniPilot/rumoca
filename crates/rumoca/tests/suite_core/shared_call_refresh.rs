//! SPEC_0043 §6a: a closed-input pure call that many refresh rows read runs
//! once per refresh, and the shared result equals each row's own call bit for
//! bit.

use rumoca::Compiler;
use rumoca_eval_solve::PreparedComputeBlock;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};
use rumoca_solver::SolveRuntime;

fn model(n: usize) -> rumoca_ir_solve::SolveModel {
    let source = format!(
        "
function Spread
  input Real a[:];
  input Real s;
  output Real b[size(a, 1)];
  output Real c[size(a, 1)];
  output Real total;
algorithm
  total := 0;
  for i in 1:size(a, 1) loop
    b[i] := sin(a[i]) * s;
    c[i] := a[i] / s;
    total := total + a[i];
  end for;
end Spread;

model ManyRowsOneCall
  input Real a[{n}] = {{0.1 * i for i in 1:{n}}};
  input Real s = 3.0;
  output Real b[{n}];
  output Real c[{n}];
  output Real total;
equation
  (b, c, total) = Spread(a, s);
end ManyRowsOneCall;
"
    );
    let compiled = Compiler::new()
        .model("ManyRowsOneCall")
        .compile_str(&source, "ManyRowsOneCall.mo")
        .unwrap_or_else(|error| panic!("compile ManyRowsOneCall: {error:#}"));
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("lower ManyRowsOneCall: {error:#}"))
}

/// Calls the issued algebraic refresh schedule executes per refresh.
fn scheduled_calls(model: &rumoca_ir_solve::SolveModel) -> usize {
    let continuous = &model.problem.continuous;
    let owners = &continuous.refresh_owners;
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
        .expect("the causal refresh issues an exact assignment schedule");
    let shared = schedule
        .shared_segments(&continuous.implicit_rhs, owners)
        .expect("the schedule's segments prove");
    shared
        .segments()
        .segments()
        .iter()
        .flat_map(|segment| segment.ops())
        .filter(|op| matches!(op, rumoca_ir_solve::LinearOp::PureCall { .. }))
        .count()
}

#[test]
fn every_row_of_a_tuple_call_reads_one_call_at_any_width() {
    // 3000 elements put each row program past the shared-value register cap.
    for n in [4, 3000] {
        let model = model(n);
        let rows = model
            .problem
            .continuous
            .refresh_owners
            .algebraic()
            .rows
            .len();
        assert_eq!(rows, 2 * n + 1);
        assert_eq!(scheduled_calls(&model), 1, "n = {n}");
    }
}

#[test]
fn the_shared_call_equals_every_row_call_bit_for_bit() {
    let model = model(3000);
    let runtime = SolveRuntime::new(&model).expect("the runtime builds");
    let mut y = model.initial_y.to_vec();
    runtime
        .refresh_algebraic_and_output_slots_certified(0.0, &mut y, &model.parameters, 1e-10, 20)
        .expect("the refresh runs");
    // Each residual program runs its own call; `y - f` is zero exactly when
    // the shared value has the bits of that row's call.
    let residual = PreparedComputeBlock::new(&model.problem.continuous.implicit_rhs).unwrap();
    let mut out = vec![f64::NAN; residual.len()];
    residual
        .eval_with_context(
            &y,
            &model.parameters,
            0.0,
            runtime.row_eval_context(),
            &mut out,
        )
        .unwrap();
    assert!(out.iter().all(|value| *value == 0.0), "{out:?}");
    assert!(y.iter().any(|value| *value != 0.0));
}
