use super::{SimOptions, SimSolverMode, compile, simulate_dae};

#[test]
fn rk45_batch_runs_no_state_discrete_controller() {
    for input in [0.0, 2.0] {
        let source = format!(
            "model Controller
               input Real u={input};
               discrete Real y(start=0, fixed=true);
             equation
               when sample(0.01, 0.01) then y=pre(y)+u; end when;
             end Controller;"
        );
        let model = compile(&source, "Controller");
        let result = simulate_dae(
            &model,
            &SimOptions {
                t_end: 0.05,
                dt: Some(0.01),
                solver_mode: SimSolverMode::RkLike,
                ..Default::default()
            },
        )
        .expect("no-state batch simulation should succeed");

        // The current trace contract retains event-left and settled observations.
        assert_eq!(result.times.len(), 11);
        assert_eq!(result.times[0], 0.0);
        assert!(result.times.windows(2).all(|pair| pair[0] <= pair[1]));
        assert_eq!(result.n_states, 0);
        let y = result
            .names
            .iter()
            .position(|name| name == "y")
            .expect("controller output");
        assert_eq!(result.data[y][0], 0.0);
        for sample in 1..=5 {
            let settled = 2 * sample;
            assert!((result.times[settled] - sample as f64 * 0.01).abs() < 1.0e-12);
            assert_eq!(result.data[y][settled - 1], (sample - 1) as f64 * input);
            assert_eq!(result.data[y][settled], sample as f64 * input);
        }
    }
}
