use super::*;

#[test]
fn test_eval_shift_sample_value_form_respects_startup_ticks_and_tick_boundaries() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let shift_expr = fn_call("shiftSample", vec![var("u"), lit(1.0), lit(1.0)]);

    env.set("u", 10.0);
    env.set("time", 0.0);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 0.0);

    env.set("time", 0.01);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 0.0);
    env.set("u", 99.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 0.0);

    env.set("time", 0.02);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    env.set("u", 20.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 20.0);

    // Same event instant must not count as a second tick.
    env.set("u", 30.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 20.0);

    env.set("time", 0.03);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 0.0);
    env.set("u", 40.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 20.0);

    env.set("time", 0.04);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    env.set("u", 40.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 40.0);
}

#[test]
fn test_eval_shift_sample_value_form_state_resets_with_clear_pre_values() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let shift_expr = fn_call("shiftSample", vec![var("u"), lit(1.0), lit(1.0)]);

    env.set("u", 7.0);
    env.set("time", 0.0);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 0.0);

    env.set("u", 11.0);
    env.set("time", 0.02);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &env), 11.0);

    clear_pre_values();

    let mut reset_env = VarEnv::<f64>::new();
    reset_env.set("u", 9.0);
    reset_env.set("time", 0.0);
    reset_env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    assert_eq!(eval_expr::<f64>(&shift_expr, &reset_env), 0.0);
}

#[test]
fn test_eval_back_sample_value_form_respects_startup_ticks_and_tick_boundaries() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let back_expr = fn_call("backSample", vec![var("u"), lit(1.0), lit(1.0)]);

    env.set("u", 10.0);
    env.set("time", 0.0);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 0.0);

    env.set("time", 0.01);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 0.0);
    env.set("u", 99.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 0.0);

    env.set("time", 0.02);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    env.set("u", 20.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 20.0);

    env.set("u", 30.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 20.0);

    env.set("time", 0.03);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 0.0);
    env.set("u", 40.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 20.0);

    env.set("time", 0.04);
    env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    env.set("u", 40.0);
    assert_eq!(eval_expr::<f64>(&back_expr, &env), 40.0);
}
