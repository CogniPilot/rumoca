//! SPEC_0007 / SOLVE-C56: refresh follows the issued assignment projection.
use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};
use rumoca_solver::{AlgebraicLinearization, AlgebraicSettle, SolveRuntime};

fn lower(equations: &str) -> rumoca_ir_solve::SolveModel {
    let source = format!(
        "model SelectedTensor Real x(start=2,fixed=true), a[3], y(start=1); equation {equations} end SelectedTensor;"
    );
    let compiled = Compiler::new()
        .model("SelectedTensor")
        .compile_str(&source, "SelectedTensor.mo")
        .unwrap();
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap()
}

fn projects(plan: &rumoca_ir_solve::RefreshPlan, index: usize) -> bool {
    plan.simultaneous_plan
        .blocks
        .iter()
        .any(|block| block.y_indices.contains(&index))
}

#[test]
fn selected_tensor_assignment_does_not_refresh_unrelated_nonlinear_sibling() {
    for (member, values) in [(2, "{y,-x,0}"), (1, "{-x,y,0}")] {
        let model = lower(&format!("der(x)=a[{member}]; a={values}; y*y*y=x;"));
        let names = &model.problem.solve_layout.solver_maps.names;
        let y = names.iter().position(|name| name == "y").unwrap();
        let owners = &model.problem.continuous.refresh_owners;
        assert!(projects(owners.algebraic(), y));
        assert!(
            !projects(owners.derivative(), y),
            "the exact a[{member}] assignment must not require its sibling's solve: {values}"
        );
        let runtime = SolveRuntime::new(&model).unwrap();
        let mut guess = vec![-10.; names.len()];
        let mut output = [0.];
        runtime
            .eval_state_derivatives_with_guess_into(
                0.,
                &[2.],
                &model.parameters,
                &mut guess,
                1e-10,
                20,
                &mut output,
            )
            .unwrap();
        assert_eq!(output, [-2.]);
        assert_eq!(guess[y], -10., "the unused observation is deferred");
        runtime
            .eval_state_jacobian_v_ad_with_guess_into(
                AlgebraicLinearization {
                    t: 0.,
                    params: &model.parameters,
                    settle: AlgebraicSettle {
                        tol: 1e-10,
                        max_iters: 20,
                    },
                },
                &[2.],
                &[1.],
                &mut guess,
                &mut output,
            )
            .unwrap();
        assert_eq!(output, [-1.]);
        guess[y] = 1.;
        runtime
            .refresh_algebraic_and_output_slots(0., &mut guess, &model.parameters, 1e-10, 20)
            .unwrap();
        assert!((guess[y] - 2_f64.cbrt()).abs() < 1e-10);
    }
}

#[test]
fn assignment_coefficients_and_coupled_blocks_retain_their_dependencies() {
    for equations in [
        "der(x)=a[1]; a={y,-x,0}; y*y*y=x;",
        "der(x)=a[2]; a={1,y*x,0}; y*y*y=x;",
        "der(x)=a[2]; a={1,y*a[2]+x,0}; y*y*y=x;",
        "der(x)=a[2]; a={y,a[1]+x,0}; y*y*y=a[2];",
    ] {
        let model = lower(equations);
        let y = model
            .problem
            .solve_layout
            .solver_maps
            .names
            .iter()
            .position(|name| name == "y")
            .unwrap();
        assert!(projects(
            model.problem.continuous.refresh_owners.derivative(),
            y
        ));
    }
}
