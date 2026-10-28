//! A full derivative JVP must not repeat a whole shared program per scalar row.
mod backend;
use super::*;
use rumoca_solver::{AlgebraicLinearization, AlgebraicSettle, SolveRuntime};

fn source(n: usize) -> String {
    format!(
        r#"
model Heat
  parameter Integer N={n};
  Real x[N](each start=0.5, each fixed=true);
  Real u[N];
  Real F[N+1];
equation
  for i in 1:N loop u[i]=x[i]/(1+x[i]); end for;
  F[1]=1-u[1];
  for k in 2:N loop F[k]=u[k-1]-u[k]; end for;
  F[N+1]=0;
  for i in 1:N loop der(x[i])=(F[i]-F[i+1])*N; end for;
end Heat;
"#
    )
}

fn shared_work(model: &solve::SolveModel) -> (usize, usize) {
    let continuous = &model.problem.continuous;
    let owners = &continuous.refresh_owners;
    let plan = owners.derivative();
    assert!(plan.causal_solution_certified);
    let mut segments = 0;
    let mut outputs = 0;
    for schedule in owners.exact_assignment_schedules() {
        if schedule.sequence_id() != plan.static_causal_sequence
            && schedule.sequence_id() != plan.dynamic_causal_sequence
        {
            continue;
        }
        let shared = schedule
            .shared_segments(&continuous.implicit_rhs, owners)
            .unwrap();
        for segment in shared.segments().segments() {
            segments += 1;
            outputs += solve::ScalarProgramBlock::program_output_count(segment.ops());
        }
    }
    (segments, outputs)
}

fn native_calls(n: usize) -> (usize, usize, usize) {
    let opts = sim_opts(SimExecutionPolicy::NativeRequired);
    let fixture = lower(&source(n), "Heat", &opts);
    let (segments, outputs) = shared_work(&fixture.model);
    let (backend, counters) = backend::counted(&fixture.model);
    let runtime = SolveRuntime::new_with_execution_backend(&fixture.model, Some(backend)).unwrap();
    let compiled = counters.compiles.get();
    let mut calls = None;
    for scale in [1.0, -2.0, 0.5] {
        let before = counters.succeeded.get();
        let mut out = vec![f64::NAN; n];
        runtime
            .eval_state_jacobian_v_ad_into(
                AlgebraicLinearization {
                    t: 0.0,
                    params: &fixture.parameters,
                    settle: AlgebraicSettle {
                        tol: 1e-10,
                        max_iters: 20,
                    },
                },
                &vec![0.5; n],
                &vec![scale; n],
                &mut out,
            )
            .unwrap();
        assert_eq!(
            counters.projection_calls.get(),
            0,
            "a certified causal seed sweep must not solve a simultaneous projection"
        );
        let observed = counters.succeeded.get() - before;
        assert!(observed > 0, "the production compiled JVP must execute");
        if let Some(previous) = calls {
            assert_eq!(observed, previous);
        }
        calls = Some(observed);
        assert_eq!(
            counters.compiles.get(),
            compiled,
            "no compilation in the hot path"
        );
        assert_eq!(
            counters.failed.get(),
            0,
            "no failed native call hidden by a fallback"
        );
        for (index, value) in out.iter().enumerate() {
            let name = fixture.problem.solver_name(index).unwrap();
            let expected = if name.ends_with("x[1]") {
                -(n as f64) * (4.0 / 9.0) * scale
            } else {
                0.0
            };
            assert!(
                (value - expected).abs() < 1e-10,
                "{name}: {value} vs {expected}"
            );
        }
    }
    (calls.unwrap(), segments, outputs)
}

#[test]
fn shared_heat_seed_native_calls_do_not_grow_per_scalar_output() {
    let small = native_calls(4);
    for n in [16, 48] {
        let large = native_calls(n);
        assert_eq!(
            (large.0, large.1),
            (small.0, small.1),
            "native calls and issued shared segments must not grow with output width"
        );
        assert!(
            large.2 <= small.2 * (n / 4),
            "target storage grows at most linearly"
        );
    }
}
