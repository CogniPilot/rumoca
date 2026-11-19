//! MLS 10.5: indexed reads in Solve lowering and differentiation.

use rumoca::Compiler;
use rumoca_solver::fmi_me::{MeError, MeSessionError};

fn source(equation: &str, next: usize) -> String {
    format!(
        r#"model IndexedDerivative
  Real x[2](each start=0,each fixed=true);
  Real values[2,2]=[time,3*time;2*time,4*time];
  Integer k(start=1,fixed=true);
equation
  {equation};
  when time>0.5 then k={next}; end when;
end IndexedDerivative;"#
    )
}

fn simulate(source: &str) -> Result<rumoca_sim::SimResult, rumoca_sim::SimError> {
    let dae = Compiler::new()
        .model("IndexedDerivative")
        .compile_str(source, "indexed_derivative.mo")
        .unwrap()
        .dae;
    rumoca_sim::simulate_dae(
        &dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            dt: Some(0.01),
            rtol: 1e-9,
            atol: 1e-10,
            ..Default::default()
        },
    )
}

#[test]
fn a_runtime_index_is_not_a_compile_time_derivative_projection() {
    for equation in [
        "der(x)={values[k,1],values[k,2]}",
        "{values[k,1],values[k,2]}=der(x)",
    ] {
        let result = simulate(&source(equation, 2)).unwrap();
        assert_eq!(result.times.last().copied(), Some(1.0));
        for (name, initial_rate) in [("x[1]", 1.0), ("x[2]", 3.0)] {
            let channel = result.names.iter().position(|n| n == name).unwrap();
            for (&actual, &time) in result.data[channel].iter().zip(&result.times) {
                let expected =
                    initial_rate * time * time / 2.0 + (time * time - 0.25).max(0.0) / 2.0;
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{equation}: {name} at {time}: {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn an_invalid_runtime_derivative_index_cannot_complete() {
    let error = simulate(&source("der(x)={values[k,1],values[k,2]}", 3)).unwrap_err();
    let rumoca_sim::SimError::ModelExchangeSession(MeSessionError::Component(component)) =
        error.kind()
    else {
        panic!("expected a component evaluation failure, got {error:?}");
    };
    let MeError::Evaluation { message } = component.kind() else {
        panic!("expected an evaluation failure, got {component:?}");
    };
    // The ME facade renders the original source span into its evaluation diagnostic.
    let (bounds, provenance) = message.split_once(" @ ").expect("source span is retained");
    let bounds = bounds
        .rsplit_once(": ")
        .map_or(bounds, |(_, detail)| detail);
    assert_eq!(bounds, "index 3 is outside 1..=2");
    let source = rumoca_core::SourceId::from_source_name("indexed_derivative.mo");
    assert!(
        provenance.starts_with(&format!("Span {{ source: {source:?},")),
        "{message}"
    );
}

#[test]
fn runtime_tensor_gather_derivative_reuses_register_lanes() {
    use rumoca_eval_solve::{ad::lower_scalar_program_block_ad, to_scalar_program_block};
    use rumoca_ir_solve::LinearOp;

    let dae = Compiler::new()
        .model("DynamicTensorGather")
        .compile_str(
            include_str!("../fixtures/solve/DynamicTensorGather.mo"),
            "DynamicTensorGather.mo",
        )
        .unwrap()
        .dae;
    let solve = rumoca_sim::lower_dae_for_simulation(&dae, &Default::default()).unwrap();
    let implicit_rhs = to_scalar_program_block(&solve.problem.continuous.implicit_rhs).unwrap();
    let gather = implicit_rhs
        .programs()
        .iter()
        .find(|row| {
            row.iter()
                .any(|op| matches!(op, LinearOp::LoadIndexedRegister { .. }))
        })
        .expect("the dynamic state selection must emit a checked gather");
    let derivative = lower_scalar_program_block_ad(std::slice::from_ref(gather)).unwrap();
    let strides = derivative[0]
        .iter()
        .filter_map(|op| match op {
            LinearOp::LoadIndexedRegister { stride, .. } => Some(*stride),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(strides, [2, 2]);
}

#[test]
fn loop_conditional_selectors_lower_to_fixed_cells() {
    let dae = Compiler::new()
        .model("SelectorGather2")
        .compile_str(
            include_str!("../fixtures/solve/SelectorGather2.mo"),
            "SelectorGather2.mo",
        )
        .unwrap()
        .dae;
    let solve = rumoca_sim::lower_dae_for_simulation(&dae, &Default::default()).unwrap();
    let implicit_rhs =
        rumoca_eval_solve::to_scalar_program_block(&solve.problem.continuous.implicit_rhs).unwrap();
    assert!(!implicit_rhs.programs().is_empty());
    let dynamic_gathers = implicit_rhs
        .programs()
        .iter()
        .flatten()
        .filter(|op| {
            matches!(
                op,
                rumoca_ir_solve::LinearOp::LoadIndexedRegister { indices, .. }
                    if indices.iter().any(|index| matches!(index, rumoca_ir_solve::TensorIndex::Runtime(_)))
            )
        })
        .count();
    assert_eq!(dynamic_gathers, 0, "each loop point selects one fixed cell");
}
