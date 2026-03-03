//! Integration tests for DAE simulation using diffsol.

use rumoca_eval_runtime::dual::Dual;
use rumoca_eval_runtime::eval::{VarEnv, build_env, eval_expr, get_pre_value, lift_env};
use rumoca_session::{Session, SessionConfig};
use rumoca_sim_diffsol::problem::{default_params, reorder_equations_for_solver};
use rumoca_sim_diffsol::{SimOptions, eliminate, simulate};

/// Helper to compile a Modelica model to a DAE.
fn compile_model(source: &str, model_name: &str) -> rumoca_ir_dae::Dae {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("Failed to parse/resolve/typecheck");
    let result = session
        .compile_model(model_name)
        .expect("Failed to compile model");
    result.dae
}

#[test]
fn test_simple_ode_exponential_decay() {
    // der(x) = -x, x(0) = 1  →  x(t) = e^(-t)
    let source = r#"
model ExpDecay
    Real x(start = 1.0);
equation
    der(x) = -x;
end ExpDecay;
"#;
    let dae = compile_model(source, "ExpDecay");
    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    assert!(!result.times.is_empty(), "Should have time points");
    assert_eq!(result.n_states, 1);
    assert_eq!(result.names[0], "x");

    // Print trajectory
    println!("\n=== ExpDecay: der(x) = -x, x(0) = 1 ===");
    println!(
        "{:>10} {:>12} {:>12} {:>12}",
        "t", "x_sim", "x_exact", "error"
    );
    for (i, t) in result.times.iter().enumerate() {
        let x_sim = result.data[0][i];
        let x_exact = (-t).exp();
        let err = (x_sim - x_exact).abs();
        println!("{:10.6} {:12.8} {:12.8} {:12.2e}", t, x_sim, x_exact, err);
    }

    // x(1) = e^(-1) ≈ 0.3679
    let x_final = *result.data[0].last().unwrap();
    let expected = (-1.0_f64).exp();
    assert!(
        (x_final - expected).abs() < 1e-4,
        "x(1) = {} but expected {} (e^-1)",
        x_final,
        expected
    );
}

#[test]
fn test_ode_with_parameter() {
    // der(x) = -k*x, x(0) = 1, k = 2  →  x(t) = e^(-2t)
    let source = r#"
model ParamDecay
    parameter Real k = 2.0;
    Real x(start = 1.0);
equation
    der(x) = -k * x;
end ParamDecay;
"#;
    let dae = compile_model(source, "ParamDecay");
    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    println!("\n=== ParamDecay: der(x) = -2x, x(0) = 1 ===");
    println!(
        "{:>10} {:>12} {:>12} {:>12}",
        "t", "x_sim", "x_exact", "error"
    );
    for (i, t) in result.times.iter().enumerate() {
        let x_sim = result.data[0][i];
        let x_exact = (-2.0 * t).exp();
        let err = (x_sim - x_exact).abs();
        println!("{:10.6} {:12.8} {:12.8} {:12.2e}", t, x_sim, x_exact, err);
    }

    let x_final = *result.data[0].last().unwrap();
    let expected = (-2.0_f64).exp();
    assert!(
        (x_final - expected).abs() < 1e-4,
        "x(1) = {} but expected {} (e^-2)",
        x_final,
        expected
    );
}

#[test]
fn test_simulation_result_includes_discrete_boolean_channel() {
    let source = r#"
model DiscreteBooleanVisible
    Real x(start = 1.0);
    discrete Boolean b1;
equation
    der(x) = -x;
    b1 = time < 0.5;
end DiscreteBooleanVisible;
"#;
    let dae = compile_model(source, "DiscreteBooleanVisible");
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let b1_idx = result
        .names
        .iter()
        .position(|n| n == "b1")
        .unwrap_or_else(|| panic!("b1 channel should exist; names={:?}", result.names));
    let b1_series = &result.data[b1_idx];
    assert_eq!(b1_series.len(), result.times.len());
    assert!(
        b1_series.iter().any(|v| *v > 0.5),
        "expected b1 to be true for part of the trajectory; series={b1_series:?}"
    );
    assert!(
        b1_series.iter().any(|v| *v < 0.5),
        "expected b1 to be false for part of the trajectory; series={b1_series:?}"
    );
}

#[test]
fn test_initial_algorithm_calculated_parameter_is_applied_in_simulation() {
    let source = r#"
model InitParamBug
    parameter Boolean cp(start = false, fixed = false);
    Real x;
initial algorithm
    cp := true;
equation
    x = if cp then 1 else 2;
end InitParamBug;
"#;

    let dae = compile_model(source, "InitParamBug");
    let params = default_params(&dae);
    assert_eq!(params.len(), 1, "expected one parameter (cp)");
    assert!(
        (params[0] - 1.0).abs() <= 1e-12,
        "cp should be initialized to true (1.0), got {}",
        params[0]
    );

    let opts = SimOptions {
        t_end: 0.1,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let x_idx = result
        .names
        .iter()
        .position(|n| n == "x")
        .expect("x output should exist");

    for (sample_idx, x) in result.data[x_idx].iter().enumerate() {
        assert!(
            (*x - 1.0).abs() <= 1e-12,
            "x should remain 1.0 when cp=true; sample {} = {}",
            sample_idx,
            x
        );
    }
}

#[test]
fn test_clocked_sample_hold_no_state_regression() {
    let source = r#"
model ClockSampleHoldRegression
    output Real s;
    output Boolean c;
equation
    c = time > 0.5;
    s = sample(time, Clock(c));
end ClockSampleHoldRegression;
"#;

    let dae = compile_model(source, "ClockSampleHoldRegression");
    assert!(
        dae.discrete_reals
            .contains_key(&rumoca_ir_dae::VarName::new("s")),
        "sampled signal should be classified as discrete real"
    );
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let _ = simulate(&dae, &opts).expect("Simulation failed");

    let s_final =
        get_pre_value("s").expect("discrete sampled signal should be retained in pre-store");
    assert!(
        s_final > 0.45 && s_final < 0.7,
        "expected sampled value to latch near first crossing; got s_final={s_final}"
    );
}

#[test]
fn test_clocked_alias_propagation_regression() {
    let source = r#"
model ClockedAliasPropagationRegression
    output Real diff;
    output Real sampled_view;
    output Boolean c;
    Real u;
    discrete Real sampled;
    discrete Real offset;
    discrete Real clk;
equation
    u = time;
    c = time > 0.5;
    clk = Clock(c);
    sampled = sample(u, clk);
    sampled = offset;
    sampled_view = offset;
    diff = u - offset;
end ClockedAliasPropagationRegression;
"#;

    let dae = compile_model(source, "ClockedAliasPropagationRegression");
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    let diff_idx = result
        .names
        .iter()
        .position(|n| n == "diff" || n.ends_with(".diff"))
        .unwrap_or_else(|| panic!("diff output should exist; names={:?}", result.names));
    let sampled_idx = result
        .names
        .iter()
        .position(|n| n == "sampled_view" || n.ends_with(".sampled_view"))
        .unwrap_or_else(|| panic!("sampled_view output should exist; names={:?}", result.names));

    let diff_final = *result.data[diff_idx]
        .last()
        .expect("diff should have samples");
    let sampled_final = *result.data[sampled_idx]
        .last()
        .expect("sampled_view should have samples");

    assert!(
        sampled_final > 0.45 && sampled_final < 0.7,
        "expected sampled alias to latch near first crossing; sampled_final={sampled_final}"
    );
    assert!(
        diff_final > 0.25 && diff_final < 0.55,
        "expected diff to reset after sampling event; diff_final={diff_final}"
    );
}

#[test]
fn test_discrete_input_binding_alias_drives_continuous_equation_regression() {
    let source = r#"
model DiscreteInputBindingAliasRegression
    Real x(start = 0.0);
    output Boolean local(start = false);
    input Boolean localAlias(start = false) = local;
equation
    local = time >= 0.5;
    der(x) = if localAlias then 1.0 else 0.0;
end DiscreteInputBindingAliasRegression;
"#;

    let dae = compile_model(source, "DiscreteInputBindingAliasRegression");
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("simulation failed");
    let x_idx = result
        .names
        .iter()
        .position(|n| n == "x" || n.ends_with(".x"))
        .unwrap_or_else(|| panic!("x state should exist; names={:?}", result.names));
    let x_series = &result.data[x_idx];
    let x_initial = *x_series.first().expect("x should have at least one sample");
    let x_final = *x_series.last().expect("x should have at least one sample");

    assert!(
        x_initial.abs() < 1.0e-9,
        "x should start at 0; x_initial={x_initial}"
    );
    assert!(
        x_final > 0.2,
        "x should increase after localAlias switches true at t=0.5; x_final={x_final}"
    );
}

#[test]
fn test_subsample_clock_counter_resolution_ticks_at_expected_period() {
    let source = r#"
model SubSampleCounterResolutionRegression
    parameter Integer factor = 20;
    parameter Integer resolutionFactor = 1000;
    Real c;
    output Real y;
equation
    c = Clock(factor, resolutionFactor);
    y = c;
end SubSampleCounterResolutionRegression;
"#;

    let dae = compile_model(source, "SubSampleCounterResolutionRegression");
    let params = default_params(&dae);
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            println!("param {} = {}", name.as_str(), params[pidx]);
            pidx += 1;
        } else {
            for i in 0..sz {
                println!("param {}[{}] = {}", name.as_str(), i + 1, params[pidx + i]);
            }
            pidx += sz;
        }
    }

    let clock_expr = rumoca_ir_dae::Expression::FunctionCall {
        name: rumoca_ir_dae::VarName::new("Clock"),
        args: vec![
            rumoca_ir_dae::Expression::VarRef {
                name: rumoca_ir_dae::VarName::new("factor"),
                subscripts: vec![],
            },
            rumoca_ir_dae::Expression::VarRef {
                name: rumoca_ir_dae::VarName::new("resolutionFactor"),
                subscripts: vec![],
            },
        ],
        is_constructor: false,
    };
    for t in [0.0, 0.01, 0.02, 0.03, 0.04] {
        let env = build_env(&dae, &vec![0.0; dae.f_x.len()], &params, t);
        let v = eval_expr::<f64>(&clock_expr, &env);
        println!("clock_eval t={t} -> {v}");
    }

    let opts = SimOptions {
        t_end: 0.1,
        dt: Some(0.0004),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];

    let pulse_times: Vec<f64> = result
        .times
        .iter()
        .zip(y_series.iter())
        .filter_map(|(t, v)| (*v > 0.5).then_some(*t))
        .collect();

    assert!(
        pulse_times.len() >= 5,
        "expected periodic pulses from clock; pulse_times={pulse_times:?}"
    );
    assert!(
        (pulse_times[1] - 0.02).abs() < 1.0e-9,
        "expected second pulse at t=0.02 for factor/resolution=20/1000; pulse_times={pulse_times:?}"
    );
}

#[test]
fn test_clocked_unit_delay_feedback_counter_progresses_each_tick() {
    let source = r#"
model ClockedUnitDelayFeedbackCounterRegression
    parameter Integer factor = 20;
    parameter Integer resolutionFactor = 1000;
    discrete Real c;
    Real clockIn;
    discrete Real sampled(start = 0);
    discrete Real delayed(start = 0);
    discrete Real add_u1;
    output Real out;
equation
    c = Clock(factor, resolutionFactor);
    clockIn = c;
    when clockIn then
        sampled = out;
    end when;
    sampled = delayed;
    delayed = pre(sampled);
    delayed = add_u1;
    out = add_u1 + 1;
end ClockedUnitDelayFeedbackCounterRegression;
"#;

    let dae = compile_model(source, "ClockedUnitDelayFeedbackCounterRegression");
    let opts = SimOptions {
        t_end: 0.09,
        dt: Some(0.001),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let out_idx = result
        .names
        .iter()
        .position(|name| name == "out")
        .expect("out output should exist");
    let out_final = *result.data[out_idx]
        .last()
        .expect("out should contain samples");

    assert!(
        out_final > 4.5,
        "clocked feedback should increment every 0.02s tick and reach ~5 by 0.09s; out_final={out_final}"
    );
}

#[test]
fn test_clocked_when_reads_algebraic_from_current_discrete_event_context_regression() {
    let source = r#"
model ClockedWhenAlgebraicCurrentDiscreteContextRegression
    parameter Integer factor = 20;
    parameter Integer resolutionFactor = 1000;
    discrete Real c;
    discrete Real y(start = 0);
    discrete Real ud_u(start = 0);
    discrete Real ud_y(start = 0);
    Real u;
    output Real out;
equation
    c = Clock(factor, resolutionFactor);
    ud_y = pre(ud_u);
    ud_u = y;
    u = ud_y + 1;
    when c then
        y = u;
    end when;
    out = y;
end ClockedWhenAlgebraicCurrentDiscreteContextRegression;
"#;

    let dae = compile_model(
        source,
        "ClockedWhenAlgebraicCurrentDiscreteContextRegression",
    );
    let opts = SimOptions {
        t_end: 0.09,
        dt: Some(0.001),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let out_idx = result
        .names
        .iter()
        .position(|name| name == "out")
        .expect("out output should exist");
    let out_final = *result.data[out_idx]
        .last()
        .expect("out should contain samples");

    assert!(
        out_final > 4.5,
        "clocked when should see algebraic value recomputed from current discrete context each tick; out_final={out_final}"
    );
}

#[test]
fn test_clocked_sample_uses_left_limit_at_step_tick_regression() {
    let source = r#"
model ClockedSampleLeftLimitRegression
    parameter Integer factor = 20;
    parameter Integer resolutionFactor = 1000;
    Real c;
    Real step;
    discrete Real sampled;
    output Real y;
equation
    step = if time < 0.04 then 0 else 1;
    c = Clock(factor, resolutionFactor);
    sampled = sample(step, c);
    y = sampled;
end ClockedSampleLeftLimitRegression;
"#;

    let dae = compile_model(source, "ClockedSampleLeftLimitRegression");
    let opts = SimOptions {
        t_end: 0.08,
        dt: Some(0.0004),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];

    // The step changes at t=0.04 exactly. The tick at t=0.04 must sample the
    // left-limit (0), so the first latched 1 must appear at t=0.06.
    for (t, y) in result.times.iter().zip(y_series.iter()) {
        if *t < 0.06 - 1.0e-12 {
            assert!(
                y.abs() < 1.0e-12,
                "sampled output must remain 0 before t=0.06; got y={y} at t={t}"
            );
        }
    }
    let has_one_after = result
        .times
        .iter()
        .zip(y_series.iter())
        .any(|(t, y)| *t >= 0.06 - 1.0e-12 && *y > 0.5);
    assert!(
        has_one_after,
        "expected sampled output to latch to 1 at/after t=0.06"
    );
}

#[test]
fn test_implicit_sample_hold_clock_rate_regression() {
    let source = r#"
model ImplicitSampleHoldClockRateRegression
    Real step;
    Real c;
    discrete Real sampledClocked;
    Real gain;
    discrete Real sampledImplicit;
    output Real y;
equation
    step = if time < 0.04 then 0 else 1;
    c = Clock(0.02);
    sampledClocked = sample(step, c);
    gain = 1.2 * (sampledClocked - sampledImplicit);
    sampledImplicit = sample(gain);
    y = sampledImplicit;
end ImplicitSampleHoldClockRateRegression;
"#;

    let dae = compile_model(source, "ImplicitSampleHoldClockRateRegression");
    for required in ["sampledClocked", "sampledImplicit"] {
        assert!(
            dae.discrete_reals
                .contains_key(&rumoca_ir_dae::VarName::new(required)),
            "expected '{required}' to be classified as discrete real"
        );
    }

    let opts = SimOptions {
        t_end: 0.2,
        dt: Some(0.0004),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];

    assert!(
        y_series.iter().all(|value| value.is_finite()),
        "implicit sample/hold loop produced non-finite output"
    );
    let max_abs = y_series
        .iter()
        .fold(0.0_f64, |acc, value| acc.max(value.abs()));
    assert!(
        max_abs < 5.0,
        "implicit sample/hold loop diverged; max |y|={max_abs}"
    );

    let transitions = y_series
        .windows(2)
        .filter(|pair| (pair[1] - pair[0]).abs() > 1.0e-9)
        .count();
    assert!(
        transitions < 40,
        "implicit sample changed too frequently (transitions={transitions})"
    );
}

#[test]
fn test_no_state_clock_events_are_scheduled_independent_of_output_grid() {
    let source = r#"
model NoStateClockScheduleRegression
    Real c;
    discrete Real sampled;
    output Real y;
equation
    c = Clock(0.02);
    sampled = sample(time, c);
    y = sampled;
end NoStateClockScheduleRegression;
"#;

    let dae = compile_model(source, "NoStateClockScheduleRegression");
    let opts = SimOptions {
        t_end: 0.09,
        dt: None,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];
    let y_final = *y_series.last().expect("y should have samples");

    assert!(
        (y_final - 0.08).abs() < 1.0e-6,
        "scheduled clock ticks should update sampled value up to t=0.08; y_final={y_final}"
    );
}

#[test]
fn test_mixed_continuous_discrete_event_cascade_regression() {
    let source = r#"
model MixedEventCascadeRegression
    Real x(start = 0);
    discrete Real d(start = 0);
    output Real y;
equation
    der(x) = 1 + d;
    when sample(0, 0.1) then
        d = if pre(d) >= 2 then 0 else pre(d) + 1;
    end when;
    when x >= 1 then
        reinit(x, 0);
    end when;
    y = x;
end MixedEventCascadeRegression;
"#;

    let dae = compile_model(source, "MixedEventCascadeRegression");
    assert!(
        dae.discrete_reals
            .contains_key(&rumoca_ir_dae::VarName::new("d")),
        "sample-updated variable should be classified as discrete real"
    );
    assert!(
        !dae.relation.is_empty(),
        "expected canonical relation conditions for event handling"
    );

    let opts = SimOptions {
        t_end: 1.2,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];

    assert!(
        y_series.iter().all(|value| value.is_finite()),
        "mixed event cascade produced non-finite output"
    );
    let y_max = y_series.iter().fold(
        f64::NEG_INFINITY,
        |acc, value| {
            if *value > acc { *value } else { acc }
        },
    );
    assert!(
        y_max <= 1.05,
        "reinit threshold should bound state near 1.0; observed y_max={y_max}"
    );
}

#[test]
fn test_implicit_sample_hold_feedback_uses_left_limit_regression() {
    let source = r#"
model ImplicitSampleHoldFeedbackLeftLimitRegression
    Real step;
    Real c;
    discrete Real sampledStep;
    Real gain;
    discrete Real sampledImplicit;
    output Real y;
equation
    step = if time < 0.04 then 0 else 1;
    c = Clock(0.02);
    sampledStep = sample(step, c);
    gain = 1.2 * y;
    sampledImplicit = sample(gain);
    y = sampledStep - sampledImplicit;
end ImplicitSampleHoldFeedbackLeftLimitRegression;
"#;

    let dae = compile_model(source, "ImplicitSampleHoldFeedbackLeftLimitRegression");
    for required in ["sampledStep", "sampledImplicit"] {
        assert!(
            dae.discrete_reals
                .contains_key(&rumoca_ir_dae::VarName::new(required)),
            "expected '{required}' to be classified as discrete real"
        );
    }

    let opts = SimOptions {
        t_end: 0.12,
        dt: Some(0.0004),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y output should exist");
    let y_series = &result.data[y_idx];

    let nearest_index = |t_target: f64| -> usize {
        let mut best_idx = 0usize;
        let mut best_err = f64::INFINITY;
        for (idx, t) in result.times.iter().enumerate() {
            let err = (*t - t_target).abs();
            if err < best_err {
                best_err = err;
                best_idx = idx;
            }
        }
        best_idx
    };

    let y_060 = y_series[nearest_index(0.06)];
    let y_080 = y_series[nearest_index(0.08)];
    let y_100 = y_series[nearest_index(0.10)];

    // Expected recurrence with left-limit sampling:
    // y(0.06)=1.0, y(0.08)=-0.2, y(0.10)=1.24.
    assert!(
        (y_060 - 1.0).abs() < 1.0e-6,
        "expected y(0.06)~1.0 from one-tick sample delay; got {y_060}"
    );
    assert!(
        (y_080 + 0.2).abs() < 1.0e-6,
        "expected y(0.08)~-0.2 from delayed hold sample; got {y_080}"
    );
    assert!(
        (y_100 - 1.24).abs() < 1.0e-6,
        "expected y(0.10)~1.24 from delayed recurrence; got {y_100}"
    );
}

#[test]
fn test_two_state_system() {
    // der(x) = -x, der(y) = x - y
    // x(0) = 1, y(0) = 0
    // x(t) = e^(-t)
    // y(t) = t * e^(-t)
    let source = r#"
model TwoState
    Real x(start = 1.0);
    Real y(start = 0.0);
equation
    der(x) = -x;
    der(y) = x - y;
end TwoState;
"#;
    let dae = compile_model(source, "TwoState");
    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    assert_eq!(result.n_states, 2);

    // Find indices by name
    let x_idx = result.names.iter().position(|n| n == "x").unwrap();
    let y_idx = result.names.iter().position(|n| n == "y").unwrap();

    println!("\n=== TwoState: der(x)=-x, der(y)=x-y, x(0)=1, y(0)=0 ===");
    println!(
        "{:>10} {:>12} {:>12} {:>12} {:>12}",
        "t", "x_sim", "x_exact", "y_sim", "y_exact"
    );
    for (i, t) in result.times.iter().enumerate() {
        let x_sim = result.data[x_idx][i];
        let y_sim = result.data[y_idx][i];
        let x_exact = (-t).exp();
        let y_exact = t * (-t).exp();
        println!(
            "{:10.6} {:12.8} {:12.8} {:12.8} {:12.8}",
            t, x_sim, x_exact, y_sim, y_exact
        );
    }

    let x_final = *result.data[x_idx].last().unwrap();
    let y_final = *result.data[y_idx].last().unwrap();

    let x_expected = (-1.0_f64).exp();
    let y_expected = 1.0 * (-1.0_f64).exp();

    assert!(
        (x_final - x_expected).abs() < 1e-4,
        "x(1) = {} but expected {}",
        x_final,
        x_expected
    );
    assert!(
        (y_final - y_expected).abs() < 1e-4,
        "y(1) = {} but expected {}",
        y_final,
        y_expected
    );
}

#[test]
fn test_harmonic_oscillator() {
    // der(x) = v, der(v) = -x
    // x(0) = 1, v(0) = 0
    // x(t) = cos(t), v(t) = -sin(t)
    let source = r#"
model Oscillator
    Real x(start = 1.0);
    Real v(start = 0.0);
equation
    der(x) = v;
    der(v) = -x;
end Oscillator;
"#;
    let dae = compile_model(source, "Oscillator");
    let opts = SimOptions {
        t_end: std::f64::consts::TAU,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    let x_idx = result.names.iter().position(|n| n == "x").unwrap();
    let v_idx = result.names.iter().position(|n| n == "v").unwrap();

    println!("\n=== Harmonic Oscillator: der(x)=v, der(v)=-x ===");
    println!(
        "{:>10} {:>12} {:>12} {:>12} {:>12}",
        "t", "x_sim", "x_exact", "v_sim", "v_exact"
    );
    // Print a subset of timesteps
    let step = (result.times.len() / 20).max(1);
    for (i, t) in result.times.iter().enumerate() {
        if i % step == 0 || i == result.times.len() - 1 {
            let x_sim = result.data[x_idx][i];
            let v_sim = result.data[v_idx][i];
            let x_exact = t.cos();
            let v_exact = -t.sin();
            println!(
                "{:10.6} {:12.8} {:12.8} {:12.8} {:12.8}",
                t, x_sim, x_exact, v_sim, v_exact
            );
        }
    }

    // At t=2*pi, should return to x≈1, v≈0
    let x_final = *result.data[x_idx].last().unwrap();
    let v_final = *result.data[v_idx].last().unwrap();
    let t_final = *result.times.last().unwrap();
    let x_expected = t_final.cos();
    let v_expected = -t_final.sin();

    assert!(
        (x_final - x_expected).abs() < 1e-3,
        "x(2pi) = {} but expected {}",
        x_final,
        x_expected
    );
    assert!(
        (v_final - v_expected).abs() < 1e-3,
        "v(2pi) = {} but expected {}",
        v_final,
        v_expected
    );
}

#[test]
fn test_nonlinear_ode() {
    // Logistic equation: der(x) = x*(1-x), x(0) = 0.1
    // x(t) = 1 / (1 + 9*exp(-t))
    let source = r#"
model Logistic
    Real x(start = 0.1);
equation
    der(x) = x * (1 - x);
end Logistic;
"#;
    let dae = compile_model(source, "Logistic");
    let opts = SimOptions {
        t_end: 5.0,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    println!("\n=== Logistic: der(x) = x*(1-x), x(0) = 0.1 ===");
    println!(
        "{:>10} {:>12} {:>12} {:>12}",
        "t", "x_sim", "x_exact", "error"
    );
    let step = (result.times.len() / 15).max(1);
    for (i, t) in result.times.iter().enumerate() {
        if i % step == 0 || i == result.times.len() - 1 {
            let x_sim = result.data[0][i];
            let x_exact = 1.0 / (1.0 + 9.0 * (-t).exp());
            let err = (x_sim - x_exact).abs();
            println!("{:10.6} {:12.8} {:12.8} {:12.2e}", t, x_sim, x_exact, err);
        }
    }

    let x_final = *result.data[0].last().unwrap();
    let expected = 1.0 / (1.0 + 9.0 * (-5.0_f64).exp());
    assert!(
        (x_final - expected).abs() < 1e-4,
        "x(5) = {} but expected {}",
        x_final,
        expected
    );
}

#[test]
fn test_write_csv_for_plot() {
    // Harmonic oscillator - write CSV for plotting
    let source = r#"
model Oscillator
    Real x(start = 1.0);
    Real v(start = 0.0);
equation
    der(x) = v;
    der(v) = -x;
end Oscillator;
"#;
    let dae = compile_model(source, "Oscillator");
    let opts = SimOptions {
        t_end: 2.0 * std::f64::consts::TAU,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    let x_idx = result.names.iter().position(|n| n == "x").unwrap();
    let v_idx = result.names.iter().position(|n| n == "v").unwrap();

    // Write CSV
    let csv_path = std::env::temp_dir().join("rumoca_oscillator.csv");
    let mut csv = String::from("t,x_sim,v_sim,x_exact,v_exact\n");
    for (i, t) in result.times.iter().enumerate() {
        let x = result.data[x_idx][i];
        let v = result.data[v_idx][i];
        csv.push_str(&format!("{},{},{},{},{}\n", t, x, v, t.cos(), -t.sin()));
    }
    std::fs::write(&csv_path, &csv).expect("Failed to write CSV");
    println!("\nCSV written to: {}", csv_path.display());
    println!("Time points: {}", result.times.len());
    println!(
        "t: [{:.4} .. {:.4}]",
        result.times.first().unwrap(),
        result.times.last().unwrap()
    );
}

#[test]
fn test_bouncing_ball_events() {
    // Bouncing ball: h drops under gravity, bounces off floor with restitution
    let source = r#"
model BouncingBall
    Real h(start=1) "height above ground";
    Real v(start=0) "velocity (positive upward)";
    parameter Real g = 9.81 "gravitational acceleration";
    parameter Real e = 0.8 "coefficient of restitution";
equation
    der(h) = v;
    der(v) = -g;
    when h <= 0 then
        reinit(v, -e * pre(v));
    end when;
end BouncingBall;
"#;
    let dae = compile_model(source, "BouncingBall");

    // Verify DAE structure
    assert_eq!(dae.states.len(), 2, "Should have 2 states (h, v)");
    assert!(
        dae.f_z
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "v")),
        "reinit target should lower to event partition equation for state v"
    );

    let opts = SimOptions {
        t_end: 3.0,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    let h_idx = result.names.iter().position(|n| n == "h").unwrap();
    let v_idx = result.names.iter().position(|n| n == "v").unwrap();

    // Print trajectory summary
    println!("\n=== BouncingBall: event handling test ===");
    println!("Time points: {}", result.times.len());

    // Verify uniform output spacing (dt = 0.01)
    for i in 1..result.times.len().min(10) {
        let dt_actual = result.times[i] - result.times[i - 1];
        assert!(
            (dt_actual - 0.01).abs() < 1e-10,
            "Non-uniform dt at step {}: dt={}",
            i,
            dt_actual
        );
    }

    // The ball should bounce: height should stay >= some small negative threshold
    // (allowing for numerical tolerance at impact)
    let h_min = result.data[h_idx]
        .iter()
        .copied()
        .fold(f64::INFINITY, f64::min);
    println!("h_min = {h_min:.6}");
    assert!(h_min > -0.1, "Ball fell through floor: h_min = {h_min}");

    // The velocity should reverse sign (have both positive and negative values)
    let has_positive_v = result.data[v_idx].iter().any(|&v| v > 0.1);
    let has_negative_v = result.data[v_idx].iter().any(|&v| v < -0.1);
    assert!(
        has_positive_v && has_negative_v,
        "Velocity should reverse at impacts"
    );

    // After 3 seconds, the ball should have bounced multiple times.
    // Each bounce has a smaller peak. Verify the final height is reasonable.
    let h_final = *result.data[h_idx].last().unwrap();
    println!("h(3.0) = {h_final:.6}");
    assert!(
        h_final < 1.0,
        "Ball should have lost energy: h_final = {h_final}"
    );
}

#[test]
fn test_bouncing_ball_reinit_without_explicit_pre_uses_left_limit_state() {
    // MLS semantics: reinit(x, expr) evaluates expr at the event instant
    // using left-limit state values. Users should not need to write pre(v)
    // explicitly for the reinit target variable.
    let source = r#"
model Ball
    Real x(start=1);
    Real v;
equation
    der(x) = v;
    der(v) = -9.8;
    when (x < 0) then
        reinit(v, -0.8*v);
    end when;
end Ball;
"#;
    let dae = compile_model(source, "Ball");
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.002),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");
    let v_idx = result.names.iter().position(|n| n == "v").unwrap();
    let x_idx = result.names.iter().position(|n| n == "x").unwrap();

    let velocity = &result.data[v_idx];
    let height = &result.data[x_idx];
    let mut bounce_idx = None;
    for i in 1..velocity.len() {
        if velocity[i - 1] < -0.1 && velocity[i] > 0.1 {
            bounce_idx = Some(i);
            break;
        }
    }
    let idx = bounce_idx.expect("expected velocity sign flip at first bounce");
    let post_bounce_v = velocity[idx];
    let impact_x = height[idx];

    assert!(
        post_bounce_v > 3.0,
        "post-bounce velocity should be positive and near restitution impulse, got {post_bounce_v}"
    );
    assert!(
        impact_x >= -0.05,
        "height should remain near the floor at first bounce, got {impact_x}"
    );
}

#[test]
fn test_dae_with_algebraic_variable() {
    // Regression test: algebraic equations appearing before ODE equations
    // in f_x used to cause "LU solve failed" due to mass-matrix mismatch.
    // der(x) = u*cos(theta), der(y) = u*sin(theta), der(theta) = 1, u = 1
    let source = r#"
model Airplane
    Real x;
    Real y;
    Real theta;
    Real u = 1;
equation
    der(x) = u * cos(theta);
    der(y) = u * sin(theta);
    der(theta) = 1;
end Airplane;
"#;
    let dae = compile_model(source, "Airplane");

    assert_eq!(dae.states.len(), 3, "Should have 3 states (x, y, theta)");
    assert_eq!(dae.algebraics.len(), 1, "Should have 1 algebraic (u)");

    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("Simulation failed");

    let x_idx = result.names.iter().position(|n| n == "x").unwrap();
    let y_idx = result.names.iter().position(|n| n == "y").unwrap();
    let theta_idx = result.names.iter().position(|n| n == "theta").unwrap();

    // theta(t) = t, so x(t) = sin(t), y(t) = 1 - cos(t) (from integration)
    let t_final = *result.times.last().unwrap();
    let x_final = *result.data[x_idx].last().unwrap();
    let y_final = *result.data[y_idx].last().unwrap();
    let theta_final = *result.data[theta_idx].last().unwrap();

    println!("\n=== Airplane: DAE with algebraic variable ===");
    println!("theta(1) = {theta_final:.6} (expected {:.6})", t_final);
    println!("x(1) = {x_final:.6} (expected {:.6})", t_final.sin());
    println!("y(1) = {y_final:.6} (expected {:.6})", 1.0 - t_final.cos());

    assert!(
        (theta_final - t_final).abs() < 1e-4,
        "theta(1) = {theta_final} but expected {t_final}"
    );
    assert!(
        (x_final - t_final.sin()).abs() < 1e-4,
        "x(1) = {x_final} but expected {}",
        t_final.sin()
    );
    assert!(
        (y_final - (1.0 - t_final.cos())).abs() < 1e-4,
        "y(1) = {y_final} but expected {}",
        1.0 - t_final.cos()
    );
}

/// Faithful transcription of the flattened MSL `Modelica.Electrical.Analog.Examples.Resistor`
/// with 28 unknowns and 28 equations. Dotted names replaced with underscores;
/// `Modelica.Math.sin`, `Modelica.Constants.pi`, and `from_degC` are inlined.
const MSL_RESISTOR_SOURCE: &str = r#"
model MslResistor
    parameter Real SineVoltage1_V = 220;
    parameter Real SineVoltage1_f = 1;
    parameter Real SineVoltage1_offset = 0;
    parameter Real SineVoltage1_phase = 0;
    parameter Real SineVoltage1_signalSource_amplitude = 220;
    parameter Real SineVoltage1_signalSource_f = 1;
    parameter Real SineVoltage1_signalSource_offset = 0;
    parameter Real SineVoltage1_signalSource_phase = 0;
    parameter Real SineVoltage1_signalSource_startTime = 0;
    parameter Real SineVoltage1_startTime = 0;
    parameter Real fixedTemperature_T = 20;
    parameter Real resistor_R = 100;
    parameter Real resistor_T = 293.15;
    parameter Real resistor_T_ref = 293.15;
    parameter Real resistor_alpha = 0.001;
    parameter Real resistor_useHeatPort = 1;
    parameter Real thermalConductor_G = 50;

    Real G_p_i;
    Real G_p_v;
    Real SineVoltage1_i;
    Real SineVoltage1_n_i;
    Real SineVoltage1_n_v;
    Real SineVoltage1_p_i;
    Real SineVoltage1_p_v;
    Real SineVoltage1_v;
    Real fixedTemperature_port_Q_flow;
    Real fixedTemperature_port_T;
    Real resistor_LossPower;
    Real resistor_R_actual;
    Real resistor_T_heatPort;
    Real resistor_heatPort_Q_flow;
    Real resistor_heatPort_T;
    Real resistor_i(start = 0);
    Real resistor_n_i;
    Real resistor_n_v;
    Real resistor_p_i;
    Real resistor_p_v;
    Real resistor_v;
    Real thermalConductor_Q_flow;
    Real thermalConductor_dT;
    Real thermalConductor_port_a_Q_flow;
    Real thermalConductor_port_a_T;
    Real thermalConductor_port_b_Q_flow;
    Real thermalConductor_port_b_T;
    output Real SineVoltage1_signalSource_y;
equation
    resistor_heatPort_T = resistor_T_heatPort;
    resistor_heatPort_Q_flow = -resistor_LossPower;
    0 = resistor_p_i + resistor_n_i;
    resistor_i = resistor_p_i;
    resistor_v = resistor_p_v - resistor_n_v;
    resistor_R_actual = resistor_R * (1 + resistor_alpha * (resistor_T_heatPort - resistor_T_ref));
    resistor_v = resistor_R_actual * resistor_i;
    resistor_LossPower = resistor_v * resistor_i;
    G_p_v = 0;
    SineVoltage1_signalSource_y = SineVoltage1_signalSource_offset +
        (if time < SineVoltage1_signalSource_startTime then 0 else
            SineVoltage1_signalSource_amplitude * sin(
                2 * 3.14159265358979 * SineVoltage1_signalSource_f *
                (time - SineVoltage1_signalSource_startTime) +
                SineVoltage1_signalSource_phase));
    SineVoltage1_v = SineVoltage1_signalSource_y;
    0 = SineVoltage1_p_i + SineVoltage1_n_i;
    SineVoltage1_i = SineVoltage1_p_i;
    SineVoltage1_v = SineVoltage1_p_v - SineVoltage1_n_v;
    thermalConductor_dT = thermalConductor_port_a_T - thermalConductor_port_b_T;
    thermalConductor_port_a_Q_flow = thermalConductor_Q_flow;
    thermalConductor_port_b_Q_flow = -thermalConductor_Q_flow;
    thermalConductor_Q_flow = thermalConductor_G * thermalConductor_dT;
    fixedTemperature_port_T = fixedTemperature_T + 273.15;
    0 = SineVoltage1_n_i + G_p_i + resistor_n_i;
    0 = resistor_heatPort_Q_flow + thermalConductor_port_a_Q_flow;
    0 = SineVoltage1_p_i + resistor_p_i;
    0 = thermalConductor_port_b_Q_flow + fixedTemperature_port_Q_flow;
    SineVoltage1_n_v = G_p_v;
    G_p_v = resistor_n_v;
    resistor_heatPort_T = thermalConductor_port_a_T;
    SineVoltage1_p_v = resistor_p_v;
    thermalConductor_port_b_T = fixedTemperature_port_T;
end MslResistor;
"#;

/// Inject a dummy state into a pure-algebraic DAE (no states).
fn inject_dummy_state(dae: &mut rumoca_ir_dae::Dae) {
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;
    use rumoca_ir_dae::{BuiltinFunction, Literal, VarName};

    let name = VarName::new("_rumoca_dummy_state");
    let mut var = dae::Variable::new(name.clone());
    var.start = Some(dae::Expression::Literal(Literal::Real(0.0)));
    dae.states.insert(name.clone(), var);
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![dae::Expression::VarRef {
                name,
                subscripts: vec![],
            }],
        },
        span: Span::DUMMY,
        origin: "dummy_state_injection".to_string(),
        scalar_count: 1,
    });
}

/// Prepare a DAE for diagnostic analysis: eliminate, inject dummy, reorder.
fn prepare_for_diagnostic(dae: &rumoca_ir_dae::Dae) -> (rumoca_ir_dae::Dae, usize, usize, usize) {
    let mut prep = dae.clone();
    let elim = eliminate::eliminate_trivial(&mut prep);

    println!("\n--- After elimination ---");
    println!("Eliminated: {} variables", elim.n_eliminated);
    for sub in &elim.substitutions {
        println!("  {} = {:?}", sub.var_name, sub.expr);
    }

    let n_x_after: usize = prep.states.values().map(|v| v.size()).sum();
    if n_x_after == 0 {
        inject_dummy_state(&mut prep);
    }

    reorder_equations_for_solver(&mut prep).expect("reorder failed");

    let n_x: usize = prep.states.values().map(|v| v.size()).sum();
    let n_z: usize = prep.algebraics.values().map(|v| v.size()).sum::<usize>()
        + prep.outputs.values().map(|v| v.size()).sum::<usize>();
    let n_total = n_x + n_z;
    (prep, n_x, n_z, n_total)
}

/// Collect solver-ordered variable names from a DAE.
fn collect_var_names(dae: &rumoca_ir_dae::Dae) -> Vec<String> {
    dae.states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .flat_map(|(name, var)| {
            let sz = var.size();
            if sz <= 1 {
                vec![name.to_string()]
            } else {
                (0..sz).map(|k| format!("{}[{}]", name, k + 1)).collect()
            }
        })
        .collect()
}

/// Analyze a Jacobian for NaN/Inf/zero-row issues.
fn analyze_jacobian(
    jac: &[Vec<f64>],
    dae: &rumoca_ir_dae::Dae,
    var_names: &[String],
    n_total: usize,
) {
    let n_rows = n_total.min(dae.f_x.len());
    let mut nan_count = 0;
    let mut inf_count = 0;
    let mut zero_rows = Vec::new();
    for (row, jac_row) in jac.iter().enumerate().take(n_rows) {
        let mut all_zero = true;
        for &v in jac_row {
            if v.is_nan() {
                nan_count += 1;
            }
            if v.is_infinite() {
                inf_count += 1;
            }
            if v != 0.0 {
                all_zero = false;
            }
        }
        if all_zero {
            zero_rows.push(row);
        }
    }

    println!("\n--- Jacobian analysis ---");
    println!("Size: {}x{}", dae.f_x.len(), n_total);
    println!("NaN entries: {nan_count}");
    println!("Inf entries: {inf_count}");
    println!("All-zero rows: {:?}", zero_rows);

    if nan_count > 0 || inf_count > 0 {
        println!("\nProblematic entries:");
        print_problematic_entries(jac, dae, var_names, n_rows);
    }

    for &row in &zero_rows {
        println!("  Zero row {row}: 0 = {:?}", dae.f_x[row].rhs);
    }
}

/// Full diagnostic deep-dive for MSL Resistor example (pure algebraic DAE).
#[test]
fn test_diagnostic_full_msl_resistor() {
    let dae = compile_model(MSL_RESISTOR_SOURCE, "MslResistor");

    println!("\n=== Full MSL Resistor Diagnostic ===");
    println!(
        "States: {}, Algebraics: {}, Outputs: {}, Parameters: {}",
        dae.states.len(),
        dae.algebraics.len(),
        dae.outputs.len(),
        dae.parameters.len()
    );
    println!("Equations: {}, Balance: {}", dae.f_x.len(), dae.balance());

    // Prepare: eliminate → dummy state → reorder
    let (prep_dae, n_x, n_z, n_total) = prepare_for_diagnostic(&dae);
    println!("\n--- Prepared system ---");
    println!(
        "n_x={n_x}, n_z={n_z}, n_total={n_total}, n_eq={}",
        prep_dae.f_x.len()
    );

    let var_names = collect_var_names(&prep_dae);
    for (i, name) in var_names.iter().enumerate() {
        let kind = if i < n_x { "state" } else { "algebraic" };
        println!("  y[{i}] = {name} ({kind})");
    }

    // Equations after preparation
    println!("\n--- Equations after prepare ---");
    for (i, eq) in prep_dae.f_x.iter().enumerate() {
        let kind = if i < n_x { "ODE" } else { "ALG" };
        println!("  eq[{i}] ({kind}): 0 = {:?}", eq.rhs);
    }

    // Residual at y=0
    let p = default_params(&prep_dae);
    let y = vec![0.0; n_total];
    let env0 = build_env(&prep_dae, &y, &p, 0.0);
    println!("\nResidual at y=0, t=0:");
    for (i, eq) in prep_dae.f_x.iter().enumerate() {
        let val = eval_expr::<f64>(&eq.rhs, &env0);
        let signed = if i < n_x { -val } else { val };
        println!("  f[{i}] = {signed:.6e}");
    }

    // Jacobian analysis
    let jac = compute_diagnostic_jacobian(&prep_dae, &y, &p, n_x, n_total);
    analyze_jacobian(&jac, &prep_dae, &var_names, n_total);
    check_algebraic_singularity(&jac, &prep_dae, n_x, n_z);
    print_dense_jacobian(&jac, &var_names, &prep_dae, n_total);

    // Simulation attempt
    println!("\n--- Simulation attempt ---");
    let opts = SimOptions {
        t_end: 0.01,
        ..SimOptions::default()
    };
    match simulate(&dae, &opts) {
        Ok(r) => {
            println!("Simulation SUCCEEDED! {} time points", r.times.len());
        }
        Err(e) => {
            println!("Simulation FAILED: {e}");
        }
    }
}

/// Print NaN/Inf entries in the Jacobian.
fn print_problematic_entries(
    jac: &[Vec<f64>],
    dae: &rumoca_ir_dae::Dae,
    var_names: &[String],
    n_rows: usize,
) {
    for (row, jac_row) in jac.iter().enumerate().take(n_rows) {
        for (col, &v) in jac_row.iter().enumerate() {
            if !v.is_nan() && !v.is_infinite() {
                continue;
            }
            let col_name = var_names.get(col).map(|s| s.as_str()).unwrap_or("?");
            println!("  J[{row}][{col}] = {v}  d(eq[{row}])/d({col_name})");
            println!("    eq[{row}]: 0 = {:?}", dae.f_x[row].rhs);
        }
    }
}

/// Compute full Jacobian via forward-mode AD for diagnostic purposes.
fn compute_diagnostic_jacobian(
    dae: &rumoca_ir_dae::Dae,
    y: &[f64],
    p: &[f64],
    n_x: usize,
    n_total: usize,
) -> Vec<Vec<f64>> {
    let mut jac_columns = Vec::with_capacity(n_total);
    for col in 0..n_total {
        let env_f64 = build_env(dae, y, p, 0.0);
        let mut env_dual: VarEnv<Dual> = lift_env(&env_f64);
        seed_dual_column(dae, &mut env_dual, col);
        rumoca_eval_runtime::statement::eval_algorithms(dae, &mut env_dual);
        let mut col_values = vec![0.0; n_total];
        for (row, eq) in dae.f_x.iter().enumerate() {
            let val = eval_expr::<Dual>(&eq.rhs, &env_dual);
            col_values[row] = if row < n_x { -val.du } else { val.du };
        }
        jac_columns.push(col_values);
    }
    let mut jac = vec![vec![0.0; n_total]; n_total];
    for (col, col_values) in jac_columns.into_iter().enumerate() {
        for (row, value) in col_values.into_iter().enumerate() {
            jac[row][col] = value;
        }
    }
    jac
}

/// Set the dual part of a named variable to 1.0 if it exists in the env.
fn set_dual_if_present(env: &mut VarEnv<Dual>, key: &str) {
    if let Some(entry) = env.vars.get_mut(key) {
        entry.du = 1.0;
    }
}

/// Build a map from solver index → variable key for dual seeding.
fn build_solver_key_map(dae: &rumoca_ir_dae::Dae) -> Vec<String> {
    let mut keys = Vec::new();
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        let sz = var.size();
        if sz <= 1 {
            keys.push(name.to_string());
        } else {
            for k in 0..sz {
                keys.push(format!("{}[{}]", name.as_str(), k + 1));
            }
        }
    }
    keys
}

/// Seed the dual part of the `col`-th solver variable to 1.0.
fn seed_dual_column(dae: &rumoca_ir_dae::Dae, env_dual: &mut VarEnv<Dual>, col: usize) {
    let keys = build_solver_key_map(dae);
    if let Some(key) = keys.get(col) {
        set_dual_if_present(env_dual, key);
    }
}

/// Check if two Jacobian rows are linearly dependent (scalar multiples).
fn rows_are_dependent(ri: &[f64], rj: &[f64]) -> Option<f64> {
    let k = ri.iter().position(|&v| v.abs() > 1e-15)?;
    if rj[k].abs() <= 1e-15 {
        return None;
    }
    let ratio = ri[k] / rj[k];
    let dependent = ri
        .iter()
        .zip(rj.iter())
        .all(|(&a, &b)| (a - ratio * b).abs() < 1e-12 * (1.0 + a.abs()));
    dependent.then_some(ratio)
}

/// Check the algebraic sub-block of the Jacobian for singularity.
fn check_algebraic_singularity(jac: &[Vec<f64>], dae: &rumoca_ir_dae::Dae, n_x: usize, n_z: usize) {
    if n_z == 0 || n_z > 20 {
        return;
    }
    println!("\n--- Algebraic Jacobian singularity check ---");

    let alg_jac: Vec<Vec<f64>> = (0..n_z)
        .map(|r| (0..n_z).map(|c| jac[r + n_x][c + n_x]).collect())
        .collect();

    for i in 0..n_z {
        for j in (i + 1)..n_z {
            if let Some(ratio) = rows_are_dependent(&alg_jac[i], &alg_jac[j]) {
                let (eq_i, eq_j) = (i + n_x, j + n_x);
                println!("  LINEARLY DEPENDENT: eq[{eq_i}] ≈ {ratio:.4} * eq[{eq_j}]");
                println!("    eq[{eq_i}]: {:?}", dae.f_x[eq_i].rhs);
                println!("    eq[{eq_j}]: {:?}", dae.f_x[eq_j].rhs);
            }
        }
    }

    for (r, row) in alg_jac.iter().enumerate() {
        if row.iter().all(|&v| v.abs() < 1e-15) {
            println!("  ZERO ROW in algebraic block: eq[{}]", r + n_x);
        }
    }
}

/// Print the Jacobian as a dense matrix (for systems with n_total <= 20).
fn print_dense_jacobian(
    jac: &[Vec<f64>],
    var_names: &[String],
    dae: &rumoca_ir_dae::Dae,
    n_total: usize,
) {
    if n_total > 20 {
        return;
    }
    println!("\nFull Jacobian matrix (rows=equations, cols=variables):");
    print!("{:>8}", "");
    for name in var_names {
        let short = if name.len() > 7 {
            &name[name.len() - 7..]
        } else {
            name
        };
        print!("{:>8}", short);
    }
    println!();
    for (row, jac_row) in jac.iter().enumerate().take(n_total.min(dae.f_x.len())) {
        print!("eq[{row:>2}] ");
        for &v in jac_row {
            match () {
                _ if v.is_nan() => print!("{:>8}", "NaN"),
                _ if v.is_infinite() => print!("{:>8}", "Inf"),
                _ if v == 0.0 => print!("{:>8}", "."),
                _ => print!("{v:>8.3}"),
            }
        }
        println!();
    }
}

/// Diagnostic test for mass-damper system (mimics MSL Translational.Examples.Damper)
#[test]
fn test_diagnostic_damper() {
    // Simple mass-damper: mass connected to fixed wall via damper
    // Exact solution: v(t) = v0 * exp(-d/m * t)
    let source = r#"
model MassDamper
    parameter Real m = 1 "mass";
    parameter Real d = 25 "damping";
    Real s(start = 3.0, fixed = true);
    Real v(start = 10.0, fixed = true);
equation
    der(s) = v;
    m * der(v) = -d * v;
end MassDamper;
"#;
    let dae = compile_model(source, "MassDamper");
    println!("\n=== MassDamper DAE ===");
    println!("States: {:?}", dae.states.keys().collect::<Vec<_>>());
    println!(
        "Algebraics: {:?}",
        dae.algebraics.keys().collect::<Vec<_>>()
    );
    println!("Equations ({}):", dae.f_x.len());
    for (i, eq) in dae.f_x.iter().enumerate() {
        println!("  [{}] 0 = {:?}", i, eq.rhs);
    }

    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts).expect("MassDamper simulation failed");
    println!("\nSimulation OK! names={:?}", result.names);
    for (i, name) in result.names.iter().enumerate() {
        let last = result.data[i].last().copied().unwrap_or(f64::NAN);
        println!("  {} final={:.6}", name, last);
    }

    // Check accuracy: v(1) = 10 * exp(-25) ≈ 0
    let v_idx = result.names.iter().position(|n| n == "v").unwrap();
    let v_final = *result.data[v_idx].last().unwrap();
    assert!(v_final.abs() < 1e-6, "v(1) should be ~0, got {}", v_final);
}

/// Diagnostic test with connector-style equations (like the MSL Damper example)
const MASS_DAMPER_CONNECTORS_SOURCE: &str = r#"
model MassDamperConnectors
    parameter Real mass1_m = 1;
    parameter Real mass1_L = 1;
    parameter Real damper1_d = 25;
    parameter Real fixed1_s0 = 4.5;
    
    Real mass1_s(start = 3.0, fixed = true);
    Real mass1_v(start = 10.0, fixed = true);
    Real mass1_a;
    Real mass1_flange_a_s;
    Real mass1_flange_a_f;
    Real mass1_flange_b_s;
    Real mass1_flange_b_f;
    
    Real damper1_s_rel;
    Real damper1_v_rel;
    Real damper1_f;
    Real damper1_flange_a_s;
    Real damper1_flange_a_f;
    Real damper1_flange_b_s;
    Real damper1_flange_b_f;
    Real damper1_lossPower;
    
    Real fixed1_flange_s;
    Real fixed1_flange_f;
equation
    // Mass equations
    mass1_flange_a_s = mass1_s - mass1_L / 2;
    mass1_flange_b_s = mass1_s + mass1_L / 2;
    mass1_v = der(mass1_s);
    mass1_a = der(mass1_v);
    mass1_m * mass1_a = mass1_flange_a_f + mass1_flange_b_f;
    
    // Damper equations
    damper1_s_rel = damper1_flange_b_s - damper1_flange_a_s;
    damper1_v_rel = der(damper1_s_rel);
    damper1_flange_b_f = damper1_f;
    damper1_flange_a_f = -damper1_f;
    damper1_f = damper1_d * damper1_v_rel;
    damper1_lossPower = damper1_f * damper1_v_rel;
    
    // Fixed
    fixed1_flange_s = fixed1_s0;
    
    // Connections
    0 = mass1_flange_b_f + damper1_flange_a_f;
    0 = damper1_flange_b_f + fixed1_flange_f;
    mass1_flange_b_s = damper1_flange_a_s;
    damper1_flange_b_s = fixed1_flange_s;
    0 = mass1_flange_a_f;
end MassDamperConnectors;
"#;

/// Connector-style regression test (Damper-style equations with connections)
#[test]
fn test_diagnostic_damper_with_connectors() {
    let dae = compile_model(MASS_DAMPER_CONNECTORS_SOURCE, "MassDamperConnectors");
    println!("\n=== MassDamperConnectors DAE ===");
    println!("States: {:?}", dae.states.keys().collect::<Vec<_>>());
    println!(
        "Algebraics: {:?}",
        dae.algebraics.keys().collect::<Vec<_>>()
    );
    println!("Equations ({}):", dae.f_x.len());
    for (i, eq) in dae.f_x.iter().enumerate() {
        println!("  [{}] 0 = {:?}", i, eq.rhs);
    }

    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    match simulate(&dae, &opts) {
        Ok(result) => {
            println!("\nSimulation OK! names={:?}", result.names);
            println!("n_states={}", result.n_states);
            for (i, name) in result.names.iter().enumerate() {
                let first = result.data[i].first().copied().unwrap_or(f64::NAN);
                let last = result.data[i].last().copied().unwrap_or(f64::NAN);
                println!("  {} : first={:.6}, last={:.6}", name, first, last);
            }
        }
        Err(e) => {
            panic!("MassDamperConnectors simulation FAILED: {:?}", e);
        }
    }
}

/// More detailed diagnostic - print the DAE after prepare_dae transformations
#[test]
fn test_diagnostic_damper_detailed() {
    let dae = compile_model(MASS_DAMPER_CONNECTORS_SOURCE, "MassDamperConnectors");

    // Phase 1: eliminate trivials
    let mut dae_copy = dae.clone();
    let elim = eliminate::eliminate_trivial(&mut dae_copy);
    println!("\n=== After eliminate_trivial ===");
    println!("States: {:?}", dae_copy.states.keys().collect::<Vec<_>>());
    println!(
        "Algebraics: {:?}",
        dae_copy.algebraics.keys().collect::<Vec<_>>()
    );
    println!("Equations ({}):", dae_copy.f_x.len());
    for (i, eq) in dae_copy.f_x.iter().enumerate() {
        println!("  [{}] 0 = {:?}", i, eq.rhs);
    }
    println!(
        "Substitutions: {:?}",
        elim.substitutions
            .iter()
            .map(|s| s.var_name.as_str())
            .collect::<Vec<_>>()
    );

    // Check what der() expressions remain
    println!("\n=== der() expressions in equations ===");
    for (i, eq) in dae_copy.f_x.iter().enumerate() {
        let ders = find_der_calls(&eq.rhs);
        if !ders.is_empty() {
            println!("  eq[{}]: der calls = {:?}", i, ders);
        }
    }

    // After reorder
    reorder_equations_for_solver(&mut dae_copy).unwrap();
    println!("\n=== After reorder ===");
    println!("States: {:?}", dae_copy.states.keys().collect::<Vec<_>>());
    println!("Equations ({}):", dae_copy.f_x.len());
    for (i, eq) in dae_copy.f_x.iter().enumerate() {
        println!("  [{}] 0 = {:?}", i, eq.rhs);
    }

    // Evaluate residual at initial point
    let n_x = dae_copy.states.values().map(|v| v.size()).sum::<usize>();
    let n_total = dae_copy.f_x.len();
    let params = default_params(&dae_copy);
    let mut y = vec![0.0; n_total];
    // Initialize manually: set state start values
    for (i, (_name, var)) in dae_copy.states.iter().enumerate() {
        if i < y.len() {
            y[i] = var
                .start
                .as_ref()
                .map(|e| eval_expr::<f64>(e, &VarEnv::new()))
                .unwrap_or(0.0);
        }
    }
    println!(
        "\n=== Initial state vector (n_x={}, n_total={}) ===",
        n_x, n_total
    );
    let all_names: Vec<_> = dae_copy
        .states
        .keys()
        .chain(dae_copy.algebraics.keys())
        .chain(dae_copy.outputs.keys())
        .collect();
    for (i, name) in all_names.iter().enumerate() {
        if i < y.len() {
            println!("  y[{}] {} = {}", i, name.as_str(), y[i]);
        }
    }

    // Evaluate residual
    let env = build_env(&dae_copy, &y, &params, 0.0);
    println!("\n=== Residual at t=0 ===");
    for (i, eq) in dae_copy.f_x.iter().enumerate() {
        let val: f64 = eval_expr(&eq.rhs, &env);
        println!("  eq[{}] = {:.6}", i, val);
    }
}

fn find_der_calls(expr: &rumoca_ir_dae::Expression) -> Vec<String> {
    let mut result = Vec::new();
    match expr {
        rumoca_ir_dae::Expression::BuiltinCall {
            function: rumoca_ir_dae::BuiltinFunction::Der,
            args,
        } => {
            result.push(format!("der({:?})", args));
        }
        rumoca_ir_dae::Expression::Binary { lhs, rhs, .. } => {
            result.extend(find_der_calls(lhs));
            result.extend(find_der_calls(rhs));
        }
        rumoca_ir_dae::Expression::Unary { rhs, .. } => {
            result.extend(find_der_calls(rhs));
        }
        rumoca_ir_dae::Expression::BuiltinCall { args, .. }
        | rumoca_ir_dae::Expression::FunctionCall { args, .. } => {
            for a in args {
                result.extend(find_der_calls(a));
            }
        }
        _ => {}
    }
    result
}
