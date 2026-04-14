use super::*;

use rumoca_ir_dae as dae;

struct NoStateTestHarness {
    dae: dae::Dae,
    elim: EliminationResult,
    all_names: Vec<String>,
    clock_event_times: Vec<f64>,
    dynamic_time_event_names: Vec<String>,
    solver_name_to_idx: HashMap<String, usize>,
    direct_assignment_ctx: crate::runtime::assignment::RuntimeDirectAssignmentContext,
    alias_ctx: crate::runtime::alias::RuntimeAliasPropagationContext,
    n_x: usize,
    requires_projection: bool,
    projection_needs_event_refresh: bool,
    requires_live_pre_values: bool,
}

struct NoStateHarnessOptions {
    clock_event_times: Vec<f64>,
    dynamic_time_event_names: Vec<String>,
    solver_name_to_idx: HashMap<String, usize>,
    n_x: usize,
    requires_projection: bool,
    projection_needs_event_refresh: bool,
    requires_live_pre_values: bool,
}

impl NoStateTestHarness {
    fn new(
        dae: dae::Dae,
        elim: EliminationResult,
        all_names: Vec<String>,
        options: NoStateHarnessOptions,
    ) -> Self {
        let y_len = options
            .solver_name_to_idx
            .values()
            .copied()
            .max()
            .map_or(options.n_x, |idx| idx + 1)
            .max(options.n_x);
        let direct_assignment_ctx =
            crate::runtime::assignment::build_runtime_direct_assignment_context(
                &dae,
                y_len,
                options.n_x,
            );
        let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(
            &dae,
            y_len,
            options.n_x,
        );
        Self {
            dae,
            elim,
            all_names,
            clock_event_times: options.clock_event_times,
            dynamic_time_event_names: options.dynamic_time_event_names,
            solver_name_to_idx: options.solver_name_to_idx,
            direct_assignment_ctx,
            alias_ctx,
            n_x: options.n_x,
            requires_projection: options.requires_projection,
            projection_needs_event_refresh: options.projection_needs_event_refresh,
            requires_live_pre_values: options.requires_live_pre_values,
        }
    }

    fn ctx(&self) -> NoStateSampleContext<'_> {
        NoStateSampleContext {
            dae: &self.dae,
            elim: &self.elim,
            param_values: &[],
            all_names: &self.all_names,
            clock_event_times: &self.clock_event_times,
            direct_assignment_ctx: &self.direct_assignment_ctx,
            alias_ctx: &self.alias_ctx,
            needs_eliminated_env: false,
            dynamic_time_event_names: &self.dynamic_time_event_names,
            solver_name_to_idx: &self.solver_name_to_idx,
            n_x: self.n_x,
            t_start: 0.0,
            requires_projection: self.requires_projection,
            projection_needs_event_refresh: self.projection_needs_event_refresh,
            requires_live_pre_values: self.requires_live_pre_values,
        }
    }
}

#[test]
fn runtime_env_vars_stable_treats_matching_nan_slots_as_converged() {
    let mut lhs = eval::VarEnv::new();
    lhs.set("x", f64::NAN);
    lhs.set("y", 3.0);

    let mut rhs = eval::VarEnv::new();
    rhs.set("x", f64::NAN);
    rhs.set("y", 3.0);

    assert!(runtime_env_vars_stable(&lhs, &rhs));
}

fn comp_ref(name: &str) -> dae::ComponentReference {
    dae::ComponentReference {
        local: false,
        parts: name
            .split('.')
            .map(|ident| dae::ComponentRefPart {
                ident: ident.to_string(),
                subs: vec![],
            })
            .collect(),
        def_id: None,
    }
}

fn dense_sample_times(end: f64, step: f64) -> Vec<f64> {
    let mut times = Vec::new();
    let mut t = 0.0_f64;
    while t <= end + 1.0e-15 {
        times.push(t);
        t += step;
    }
    times
}

fn rounded_change_times(times: &[f64], series: &[f64]) -> Vec<f64> {
    let mut out = Vec::new();
    let mut prev = series[0];
    for (time, value) in times.iter().copied().zip(series.iter().copied()).skip(1) {
        if value == prev {
            continue;
        }
        out.push((time * 1.0e6).round() / 1.0e6);
        prev = value;
    }
    out
}

fn sample_no_state_channels(
    harness: &NoStateTestHarness,
    output_times: &[f64],
    evaluation_times: &[f64],
    y: Vec<f64>,
) -> Vec<Vec<f64>> {
    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &harness.ctx(),
        output_times,
        evaluation_times,
        y,
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("no-state test harness sampling should succeed");
    rumoca_eval_dae::runtime::clear_pre_values();
    data
}

fn logic_enum(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(
            format!("Modelica.Electrical.Digital.Interfaces.Logic.'{name}'").as_str(),
        ),
        subscripts: vec![],
    }
}

fn default_no_state_harness_options() -> NoStateHarnessOptions {
    NoStateHarnessOptions {
        clock_event_times: Vec::new(),
        dynamic_time_event_names: Vec::new(),
        solver_name_to_idx: HashMap::new(),
        n_x: 0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    }
}

fn var_ref(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn real_lit(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn int_lit(value: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(value))
}

fn pre_var(name: &str) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var_ref(name)],
    }
}

fn time_var() -> dae::Expression {
    var_ref("time")
}

fn time_window(start: f64, end: f64) -> dae::Expression {
    dae::Expression::Binary {
        op: OpBinary::And(Default::default()),
        lhs: Box::new(dae::Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(time_var()),
            rhs: Box::new(real_lit(start)),
        }),
        rhs: Box::new(dae::Expression::Binary {
            op: OpBinary::Lt(Default::default()),
            lhs: Box::new(time_var()),
            rhs: Box::new(real_lit(end)),
        }),
    }
}

fn sample_expr(signal: dae::Expression, clock: dae::Expression) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![signal, clock],
    }
}

fn clock_expr_real(period: f64) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("Clock"),
        args: vec![real_lit(period)],
        is_constructor: false,
    }
}

fn clock_expr_fraction(numerator: i64, denominator: i64) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("Clock"),
        args: vec![int_lit(numerator), int_lit(denominator)],
        is_constructor: false,
    }
}

fn explicit_eq(name: &str, rhs: dae::Expression, origin: &str) -> dae::Equation {
    dae::Equation::explicit(
        dae::VarName::new(name),
        rhs,
        rumoca_core::Span::DUMMY,
        origin,
    )
}

fn insert_output_and_discrete_vars(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.outputs.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
    }
}

fn build_no_state_harness(
    dae: dae::Dae,
    all_names: &[&str],
    options: NoStateHarnessOptions,
) -> NoStateTestHarness {
    NoStateTestHarness::new(
        dae,
        EliminationResult::default(),
        all_names.iter().map(|name| (*name).to_string()).collect(),
        options,
    )
}

fn sample_no_state_channels_with_sync<F>(
    harness: &NoStateTestHarness,
    output_times: &[f64],
    evaluation_times: &[f64],
    y: Vec<f64>,
    mut sync_solver_values: F,
) -> Vec<Vec<f64>>
where
    F: FnMut(&mut [f64], f64, bool) -> Result<(), ()>,
{
    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &harness.ctx(),
        output_times,
        evaluation_times,
        y,
        || Ok::<(), ()>(()),
        |y, t, requires_projection| sync_solver_values(y, t, requires_projection),
    )
    .expect("no-state test harness sampling should succeed");
    rumoca_eval_dae::runtime::clear_pre_values();
    data
}

fn sample_no_state_channels_with_schedule(
    harness: &NoStateTestHarness,
    output_times: &mut Vec<f64>,
    evaluation_times: &[f64],
    y: Vec<f64>,
) -> (Vec<f64>, Vec<Vec<f64>>) {
    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, final_output_times, data) = collect_algebraic_samples_with_schedule(
        &harness.ctx(),
        output_times,
        evaluation_times,
        y,
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("no-state test harness scheduled sampling should succeed");
    rumoca_eval_dae::runtime::clear_pre_values();
    (final_output_times, data)
}

fn periodic_clock_times() -> Vec<f64> {
    vec![0.0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18]
}

fn add_discrete_real_start(dae_model: &mut dae::Dae, name: &str, start: f64) {
    let mut var = dae::Variable::new(dae::VarName::new(name));
    var.start = Some(real_lit(start));
    dae_model
        .discrete_reals
        .insert(dae::VarName::new(name), var);
}

fn add_discrete_bool_start(dae_model: &mut dae::Dae, name: &str, start: bool) {
    let mut var = dae::Variable::new(dae::VarName::new(name));
    var.start = Some(dae::Expression::Literal(dae::Literal::Boolean(start)));
    dae_model
        .discrete_valued
        .insert(dae::VarName::new(name), var);
}

fn event_due_expr(name: &str) -> dae::Expression {
    dae::Expression::Binary {
        op: OpBinary::Ge(Default::default()),
        lhs: Box::new(time_var()),
        rhs: Box::new(pre_var(name)),
    }
}

fn initial_expr() -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Initial,
        args: vec![],
    }
}

fn initial_or_event_pre_expr(
    name: &str,
    initial_rhs: dae::Expression,
    event_due: dae::Expression,
    event_rhs: dae::Expression,
) -> dae::Expression {
    dae::Expression::If {
        branches: vec![(initial_expr(), initial_rhs), (event_due, event_rhs)],
        else_branch: Box::new(pre_var(name)),
    }
}

fn change_latch_expr(name: &str) -> dae::Expression {
    dae::Expression::If {
        branches: vec![(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Change,
                args: vec![var_ref(name)],
            },
            dae::Expression::Literal(dae::Literal::Boolean(true)),
        )],
        else_branch: Box::new(pre_var("on")),
    }
}

fn eliminated_substitution(var_name: &str, rhs_name: &str) -> EliminationResult {
    EliminationResult {
        substitutions: vec![rumoca_phase_solve::Substitution {
            var_name: dae::VarName::new(var_name),
            expr: var_ref(rhs_name),
            env_keys: vec![var_name.to_string()],
        }],
        n_eliminated: 1,
    }
}

fn add_pulse_start_tracking(dae_model: &mut dae::Dae, start: f64, period: f64) {
    let mut pulse_start = dae::Variable::new(dae::VarName::new("pulseStart"));
    pulse_start.start = Some(real_lit(10.0));
    dae_model
        .discrete_reals
        .insert(dae::VarName::new("pulseStart"), pulse_start);
    dae_model.f_z.push(explicit_eq(
        "pulseStart",
        dae::Expression::If {
            branches: vec![(sample_expr(real_lit(start), real_lit(period)), time_var())],
            else_branch: Box::new(pre_var("pulseStart")),
        },
        "pulseStart := sampled time marker",
    ));
}

fn build_boolean_pulse_harness() -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    insert_output_and_discrete_vars(&mut dae_model, &["y"]);
    add_pulse_start_tracking(&mut dae_model, 0.0, 1.0);
    dae_model.f_m.push(explicit_eq(
        "y",
        dae::Expression::Binary {
            op: OpBinary::And(Default::default()),
            lhs: Box::new(dae::Expression::Binary {
                op: OpBinary::Ge(Default::default()),
                lhs: Box::new(time_var()),
                rhs: Box::new(var_ref("pulseStart")),
            }),
            rhs: Box::new(dae::Expression::Binary {
                op: OpBinary::Lt(Default::default()),
                lhs: Box::new(time_var()),
                rhs: Box::new(dae::Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(var_ref("pulseStart")),
                    rhs: Box::new(real_lit(0.2)),
                }),
            }),
        },
        "y := time >= pulseStart and time < pulseStart + 0.2",
    ));

    let mut options = default_no_state_harness_options();
    options.clock_event_times = vec![0.0, 1.0];
    options.requires_live_pre_values = true;
    build_no_state_harness(dae_model, &["y"], options)
}

fn build_pulse_trigger_edge_harness() -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    insert_output_and_discrete_vars(&mut dae_model, &["trigger", "y"]);
    dae_model.discrete_reals.insert(
        dae::VarName::new("pulseStart"),
        dae::Variable::new(dae::VarName::new("pulseStart")),
    );
    dae_model.f_z.push(explicit_eq(
        "pulseStart",
        dae::Expression::If {
            branches: vec![(sample_expr(real_lit(1.0), real_lit(1.0)), time_var())],
            else_branch: Box::new(pre_var("pulseStart")),
        },
        "pulseStart := if sample(1, 1) then time else pre(pulseStart)",
    ));
    dae_model.f_m.push(explicit_eq(
        "trigger",
        dae::Expression::Binary {
            op: OpBinary::And(Default::default()),
            lhs: Box::new(dae::Expression::Binary {
                op: OpBinary::Ge(Default::default()),
                lhs: Box::new(time_var()),
                rhs: Box::new(var_ref("pulseStart")),
            }),
            rhs: Box::new(dae::Expression::Binary {
                op: OpBinary::Lt(Default::default()),
                lhs: Box::new(time_var()),
                rhs: Box::new(dae::Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(var_ref("pulseStart")),
                    rhs: Box::new(real_lit(0.5)),
                }),
            }),
        },
        "trigger := time >= pulseStart and time < pulseStart + 0.5",
    ));
    dae_model.f_m.push(explicit_eq(
        "y",
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Initial,
                    args: vec![],
                },
                int_lit(0),
            )],
            else_branch: Box::new(dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Edge,
                        args: vec![var_ref("trigger")],
                    },
                    dae::Expression::Binary {
                        op: OpBinary::Add(Default::default()),
                        lhs: Box::new(pre_var("y")),
                        rhs: Box::new(int_lit(1)),
                    },
                )],
                else_branch: Box::new(pre_var("y")),
            }),
        },
        "y := if initial() then 0 else if edge(trigger) then pre(y) + 1 else pre(y)",
    ));

    let mut options = default_no_state_harness_options();
    options.clock_event_times = vec![1.0, 2.0];
    options.requires_live_pre_values = true;
    build_no_state_harness(dae_model, &["y", "trigger"], options)
}

struct SampleShiftHoldOptions {
    flattened_boolean_alias: bool,
    solver_backed_clock: bool,
    include_alias_prefix_series: bool,
}

fn sample_shift_hold_all_names(options: &SampleShiftHoldOptions) -> Vec<&'static str> {
    if options.flattened_boolean_alias && options.include_alias_prefix_series {
        vec![
            "table.realToBoolean.y",
            "table.y",
            "sample1.u",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ]
    } else if options.flattened_boolean_alias {
        vec![
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ]
    } else {
        vec![
            "table.y",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ]
    }
}

fn add_simple_boolean_sample_source(dae_model: &mut dae::Dae) {
    insert_output_and_discrete_vars(
        dae_model,
        &[
            "table.y",
            "sample1.clock",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
            "periodicClock.y",
            "periodicClock.c",
        ],
    );
    dae_model.f_m.push(explicit_eq(
        "table.y",
        time_window(0.05, 0.15),
        "table.y = time >= 0.05 and time < 0.15",
    ));
}

fn add_flattened_boolean_sample_source(dae_model: &mut dae::Dae) {
    insert_output_and_discrete_vars(
        dae_model,
        &[
            "table.realToBoolean.u",
            "table.realToBoolean.y",
            "table.y",
            "sample1.u",
            "sample1.clock",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
            "periodicClock.y",
            "periodicClock.c",
        ],
    );
    dae_model.f_m.push(explicit_eq(
        "table.realToBoolean.u",
        dae::Expression::If {
            branches: vec![(time_window(0.05, 0.15), real_lit(1.0))],
            else_branch: Box::new(real_lit(0.0)),
        },
        "table.realToBoolean.u = if 0.05 <= time < 0.15 then 1 else 0",
    ));
    dae_model.f_m.push(explicit_eq(
        "table.realToBoolean.y",
        dae::Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(var_ref("table.realToBoolean.u")),
            rhs: Box::new(real_lit(0.5)),
        },
        "table.realToBoolean.y = table.realToBoolean.u >= 0.5",
    ));
    dae_model.f_m.push(explicit_eq(
        "table.y",
        var_ref("table.realToBoolean.y"),
        "table.y = table.realToBoolean.y",
    ));
    dae_model.f_m.push(explicit_eq(
        "sample1.u",
        var_ref("table.y"),
        "sample1.u = table.y",
    ));
}

fn add_sample_shift_hold_clock(dae_model: &mut dae::Dae, options: &SampleShiftHoldOptions) {
    if options.solver_backed_clock {
        dae_model.states.insert(
            dae::VarName::new("_rumoca_dummy_state"),
            dae::Variable::new(dae::VarName::new("_rumoca_dummy_state")),
        );
        dae_model.algebraics.insert(
            dae::VarName::new("sample1.clock"),
            dae::Variable::new(dae::VarName::new("sample1.clock")),
        );
        dae_model.f_x.push(explicit_eq(
            "_rumoca_dummy_state",
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("_rumoca_dummy_state")],
            },
            "dummy_state_injection",
        ));
        dae_model.f_x.push(dae::Equation {
            lhs: None,
            rhs: dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(var_ref("periodicClock.c")),
                rhs: Box::new(var_ref("sample1.clock")),
            },
            span: rumoca_core::Span::DUMMY,
            origin: "connection equation: periodicClock.y = sample1.clock".to_string(),
            scalar_count: 1,
        });
        return;
    }
    if options.flattened_boolean_alias {
        dae_model.f_m.push(explicit_eq(
            "periodicClock.y",
            var_ref("sample1.clock"),
            "periodicClock.y = sample1.clock",
        ));
        dae_model.f_m.push(explicit_eq(
            "periodicClock.y",
            var_ref("periodicClock.c"),
            "periodicClock.y = periodicClock.c",
        ));
    } else {
        dae_model.f_m.push(explicit_eq(
            "periodicClock.y",
            var_ref("periodicClock.c"),
            "periodicClock.y = periodicClock.c",
        ));
        dae_model.f_m.push(explicit_eq(
            "sample1.clock",
            var_ref("periodicClock.y"),
            "sample1.clock = periodicClock.y",
        ));
    }
}

fn sample_shift_hold_harness_options(options: &SampleShiftHoldOptions) -> NoStateHarnessOptions {
    let mut harness_options = default_no_state_harness_options();
    harness_options.clock_event_times = periodic_clock_times();
    harness_options.requires_live_pre_values = true;
    if options.solver_backed_clock {
        harness_options.solver_name_to_idx = HashMap::from([
            ("_rumoca_dummy_state".to_string(), 0usize),
            ("sample1.clock".to_string(), 1usize),
        ]);
        harness_options.n_x = 1;
    }
    harness_options
}

fn add_sample_shift_hold_chain(
    dae_model: &mut dae::Dae,
    sampled_input_name: &str,
    clock_name: &str,
) {
    dae_model.f_m.push(explicit_eq(
        "sample1.y",
        sample_expr(var_ref(sampled_input_name), var_ref(clock_name)),
        "sample1.y = sample(input, clock)",
    ));
    dae_model.f_m.push(explicit_eq(
        "shiftSample1.u",
        var_ref("sample1.y"),
        "shiftSample1.u = sample1.y",
    ));
    dae_model.f_m.push(explicit_eq(
        "shiftSample1.y",
        dae::Expression::FunctionCall {
            name: dae::VarName::new("shiftSample"),
            args: vec![var_ref("shiftSample1.u"), real_lit(2.0), real_lit(1.0)],
            is_constructor: false,
        },
        "shiftSample1.y = shiftSample(shiftSample1.u, 2, 1)",
    ));
    dae_model.f_m.push(explicit_eq(
        "hold1.u",
        var_ref("shiftSample1.y"),
        "hold1.u = shiftSample1.y",
    ));
    dae_model.f_m.push(explicit_eq(
        "hold1.y",
        dae::Expression::FunctionCall {
            name: dae::VarName::new("hold"),
            args: vec![var_ref("hold1.u")],
            is_constructor: false,
        },
        "hold1.y = hold(hold1.u)",
    ));
}

fn build_sample_shift_hold_harness(options: SampleShiftHoldOptions) -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    if options.flattened_boolean_alias {
        add_flattened_boolean_sample_source(&mut dae_model);
    } else {
        add_simple_boolean_sample_source(&mut dae_model);
    }
    let clock_expr = if options.flattened_boolean_alias || options.solver_backed_clock {
        clock_expr_fraction(20, 1000)
    } else {
        clock_expr_real(0.02)
    };
    dae_model.f_m.push(explicit_eq(
        "periodicClock.c",
        clock_expr,
        "periodicClock.c = Clock(...)",
    ));
    add_sample_shift_hold_clock(&mut dae_model, &options);
    let sample_input_name = if options.flattened_boolean_alias {
        "sample1.u"
    } else {
        "table.y"
    };
    add_sample_shift_hold_chain(&mut dae_model, sample_input_name, "sample1.clock");
    let all_names = sample_shift_hold_all_names(&options);
    let harness_options = sample_shift_hold_harness_options(&options);
    build_no_state_harness(dae_model, &all_names, harness_options)
}

fn build_projected_function_output_harness() -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    dae_model.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    add_discrete_real_start(&mut dae_model, "a", 0.0);
    add_discrete_real_start(&mut dae_model, "nextEvent", 0.0);
    add_discrete_real_start(&mut dae_model, "nextEventScaled", 0.0);
    let mut last_var = dae::Variable::new(dae::VarName::new("last"));
    last_var.start = Some(int_lit(1));
    dae_model
        .discrete_valued
        .insert(dae::VarName::new("last"), last_var);

    let cond = dae::Expression::Array {
        elements: vec![event_due_expr("nextEvent"), initial_expr()],
        is_matrix: false,
    };
    let guarded = |then_expr: dae::Expression, pre_name: &str| dae::Expression::If {
        branches: vec![(cond.clone(), then_expr)],
        else_branch: Box::new(pre_var(pre_name)),
    };
    let call_expr = |suffix: &str| dae::Expression::FunctionCall {
        name: dae::VarName::new(format!("Pkg.lookup.{suffix}")),
        args: vec![var_ref("last")],
        is_constructor: false,
    };

    dae_model.f_x.push(explicit_eq("y", var_ref("a"), "y = a"));
    dae_model.f_z.push(explicit_eq(
        "a",
        guarded(call_expr("a"), "a"),
        "a := lookup.a(last)",
    ));
    dae_model.f_z.push(explicit_eq(
        "nextEventScaled",
        guarded(call_expr("nextEventScaled"), "nextEventScaled"),
        "nextEventScaled := lookup.nextEventScaled(last)",
    ));
    dae_model.f_z.push(explicit_eq(
        "nextEvent",
        guarded(var_ref("nextEventScaled"), "nextEvent"),
        "nextEvent := nextEventScaled",
    ));
    dae_model.f_m.push(explicit_eq(
        "last",
        guarded(
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Pkg.lookup.next"),
                args: vec![pre_var("last")],
                is_constructor: false,
            },
            "last",
        ),
        "last := lookup.next(pre(last))",
    ));

    let mut lookup = dae::Function::new("Pkg.lookup", rumoca_core::Span::DUMMY);
    lookup.add_input(dae::FunctionParam::new("last", "Integer"));
    lookup.add_output(dae::FunctionParam::new("a", "Real"));
    lookup.add_output(dae::FunctionParam::new("nextEventScaled", "Real"));
    lookup.add_output(dae::FunctionParam::new("next", "Integer"));
    lookup.body = vec![
        dae::Statement::Assignment {
            comp: comp_ref("a"),
            value: var_ref("last"),
        },
        dae::Statement::Assignment {
            comp: comp_ref("nextEventScaled"),
            value: var_ref("last"),
        },
        dae::Statement::Assignment {
            comp: comp_ref("next"),
            value: dae::Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var_ref("last")),
                rhs: Box::new(int_lit(1)),
            },
        },
    ];
    dae_model
        .functions
        .insert(dae::VarName::new("Pkg.lookup"), lookup);

    let mut options = default_no_state_harness_options();
    options.solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    options.requires_live_pre_values = true;
    build_no_state_harness(dae_model, &["y"], options)
}

fn build_direct_time_threshold_harness() -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    let mut y_var = dae::Variable::new(dae::VarName::new("y"));
    y_var.start = Some(real_lit(0.0));
    dae_model.outputs.insert(dae::VarName::new("y"), y_var);
    dae_model.discrete_reals.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae_model.f_m.push(explicit_eq(
        "y",
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::Binary {
                        op: OpBinary::Ge(Default::default()),
                        lhs: Box::new(time_var()),
                        rhs: Box::new(real_lit(0.05)),
                    }],
                },
                time_var(),
            )],
            else_branch: Box::new(dae::Expression::If {
                branches: vec![(initial_expr(), var_ref("y"))],
                else_branch: Box::new(pre_var("y")),
            }),
        },
        "y := if edge(time >= 0.05) then time else if initial() then y else pre(y)",
    ));

    let mut options = default_no_state_harness_options();
    options.requires_live_pre_values = true;
    build_no_state_harness(dae_model, &["y"], options)
}

fn build_recurrent_direct_threshold_harness() -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_valued.insert(
        dae::VarName::new("count"),
        dae::Variable::new(dae::VarName::new("count")),
    );
    dae_model.f_m.push(explicit_eq(
            "count",
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Edge,
                        args: vec![dae::Expression::Binary {
                            op: OpBinary::Ge(Default::default()),
                            lhs: Box::new(time_var()),
                            rhs: Box::new(dae::Expression::Binary {
                                op: OpBinary::Mul(Default::default()),
                                lhs: Box::new(dae::Expression::Binary {
                                    op: OpBinary::Add(Default::default()),
                                    lhs: Box::new(pre_var("count")),
                                    rhs: Box::new(int_lit(1)),
                                }),
                                rhs: Box::new(real_lit(0.1)),
                            }),
                        }],
                    },
                    dae::Expression::Binary {
                        op: OpBinary::Add(Default::default()),
                        lhs: Box::new(pre_var("count")),
                        rhs: Box::new(int_lit(1)),
                    },
                )],
                else_branch: Box::new(dae::Expression::If {
                    branches: vec![(initial_expr(), var_ref("count"))],
                    else_branch: Box::new(pre_var("count")),
                }),
            },
            "count := if edge(time >= (pre(count)+1)*0.1) then pre(count)+1 else if initial() then count else pre(count)",
        ));

    let mut options = default_no_state_harness_options();
    options.requires_live_pre_values = true;
    build_no_state_harness(dae_model, &["count"], options)
}

fn build_clocked_previous_initial_harness() -> NoStateTestHarness {
    let mut dae = dae::Dae::default();
    insert_output_and_discrete_vars(
        &mut dae,
        &[
            "periodicClock.c",
            "periodicClock.y",
            "assignClock1.clock",
            "assignClock1.u",
            "assignClock1.y",
            "unitDelay1.u",
            "unitDelay1.y",
            "const.y",
            "sum.u[1]",
            "sum.u[2]",
            "sum.y",
        ],
    );
    dae.clock_schedules.push(dae::ClockSchedule {
        period_seconds: 0.02,
        phase_seconds: 0.0,
    });
    dae.f_z.push(explicit_eq(
        "periodicClock.c",
        clock_expr_real(0.02),
        "periodicClock.c = Clock(0.02)",
    ));
    dae.f_m.push(explicit_eq(
        "periodicClock.y",
        var_ref("periodicClock.c"),
        "periodicClock.y = periodicClock.c",
    ));
    dae.f_m.push(explicit_eq(
        "assignClock1.clock",
        var_ref("periodicClock.y"),
        "assignClock1.clock = periodicClock.y",
    ));
    dae.f_m.push(explicit_eq(
        "unitDelay1.y",
        dae::Expression::FunctionCall {
            name: dae::VarName::new("previous"),
            args: vec![var_ref("unitDelay1.u")],
            is_constructor: false,
        },
        "unitDelay1.y = previous(unitDelay1.u)",
    ));
    dae.f_m
        .push(explicit_eq("const.y", int_lit(1), "const.y = 1"));
    dae.f_m.push(explicit_eq(
        "sum.u[1]",
        var_ref("unitDelay1.y"),
        "sum.u[1] = unitDelay1.y",
    ));
    dae.f_m.push(explicit_eq(
        "sum.u[2]",
        var_ref("const.y"),
        "sum.u[2] = const.y",
    ));
    dae.f_m.push(explicit_eq(
        "sum.y",
        dae::Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(var_ref("sum.u[1]")),
            rhs: Box::new(var_ref("sum.u[2]")),
        },
        "sum.y = sum.u[1] + sum.u[2]",
    ));
    dae.f_m.push(explicit_eq(
        "assignClock1.u",
        var_ref("sum.y"),
        "assignClock1.u = sum.y",
    ));
    dae.f_m.push(explicit_eq(
            "assignClock1.y",
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Edge,
                        args: vec![var_ref("assignClock1.clock")],
                    },
                    var_ref("assignClock1.u"),
                )],
                else_branch: Box::new(dae::Expression::If {
                    branches: vec![(initial_expr(), var_ref("assignClock1.y"))],
                    else_branch: Box::new(pre_var("assignClock1.y")),
                }),
            },
            "assignClock1.y = if edge(assignClock1.clock) then assignClock1.u else if initial() then assignClock1.y else pre(assignClock1.y)",
        ));
    dae.f_m.push(explicit_eq(
        "unitDelay1.u",
        var_ref("assignClock1.y"),
        "unitDelay1.u = assignClock1.y",
    ));

    let mut options = default_no_state_harness_options();
    options.clock_event_times = vec![0.0, 0.02];
    options.requires_live_pre_values = true;
    build_no_state_harness(dae, &["assignClock1.y"], options)
}

fn build_dynamic_lhs_pre_feedback_harness() -> NoStateTestHarness {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
    ]);

    let mut n = dae::Variable::new(dae::VarName::new("n"));
    n.start = Some(int_lit(3));
    dae.parameters.insert(dae::VarName::new("n"), n);

    let mut auxiliary = dae::Variable::new(dae::VarName::new("auxiliary"));
    auxiliary.dims = vec![3];
    auxiliary.start = Some(var_ref("Modelica.Electrical.Digital.Interfaces.Logic.'U'"));
    dae.discrete_valued
        .insert(dae::VarName::new("auxiliary"), auxiliary);
    dae.discrete_valued.insert(
        dae::VarName::new("auxiliary_n"),
        dae::Variable::new(dae::VarName::new("auxiliary_n")),
    );
    insert_output_and_discrete_vars(&mut dae, &["y"]);

    let n_index = dae::Subscript::Expr(Box::new(var_ref("n")));
    dae.f_m.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("auxiliary"),
                subscripts: vec![n_index.clone()],
            }),
            rhs: Box::new(var_ref("Modelica.Electrical.Digital.Interfaces.Logic.'0'")),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "auxiliary[n] := Logic.'0'".to_string(),
        scalar_count: 1,
    });
    dae.f_m.push(explicit_eq(
        "auxiliary_n",
        dae::Expression::VarRef {
            name: dae::VarName::new("auxiliary"),
            subscripts: vec![n_index],
        },
        "auxiliary_n := auxiliary[n]",
    ));
    dae.f_m.push(explicit_eq(
        "y",
        pre_var("auxiliary_n"),
        "y := pre(auxiliary_n)",
    ));

    let mut options = default_no_state_harness_options();
    options.requires_live_pre_values = true;
    build_no_state_harness(dae, &["y"], options)
}

fn add_enum_direct_assignment_parameters(dae_model: &mut dae::Dae, nested: bool) {
    let mut y0 = dae::Variable::new(dae::VarName::new("a.y0"));
    y0.start = Some(logic_enum("0"));
    dae_model.parameters.insert(dae::VarName::new("a.y0"), y0);

    let (x_dims, x_elements, t_elements) = if nested {
        (
            vec![4],
            vec![
                logic_enum("1"),
                logic_enum("0"),
                logic_enum("1"),
                logic_enum("0"),
            ],
            vec![1.0, 2.0, 3.0, 4.0],
        )
    } else {
        (vec![1], vec![logic_enum("1")], vec![1.0])
    };

    let mut x = dae::Variable::new(dae::VarName::new("a.x"));
    x.dims = x_dims.clone();
    x.start = Some(dae::Expression::Array {
        elements: x_elements,
        is_matrix: false,
    });
    dae_model.parameters.insert(dae::VarName::new("a.x"), x);

    let mut t_param = dae::Variable::new(dae::VarName::new("a.t"));
    t_param.dims = x_dims;
    t_param.start = Some(dae::Expression::Array {
        elements: t_elements
            .into_iter()
            .map(|value| dae::Expression::Literal(dae::Literal::Real(value)))
            .collect(),
        is_matrix: false,
    });
    dae_model
        .parameters
        .insert(dae::VarName::new("a.t"), t_param);
}

fn enum_direct_assignment_expr(nested: bool) -> dae::Expression {
    let indexed = |name: &str, idx: i64| dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::Literal(
            dae::Literal::Integer(idx),
        )))],
    };
    let time_ge = |idx: i64| dae::Expression::Binary {
        op: OpBinary::Ge(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("time"),
            subscripts: vec![],
        }),
        rhs: Box::new(indexed("a.t", idx)),
    };
    if nested {
        dae::Expression::If {
            branches: vec![(time_ge(4), indexed("a.x", 4))],
            else_branch: Box::new(dae::Expression::If {
                branches: vec![(time_ge(3), indexed("a.x", 3))],
                else_branch: Box::new(dae::Expression::If {
                    branches: vec![(time_ge(2), indexed("a.x", 2))],
                    else_branch: Box::new(dae::Expression::If {
                        branches: vec![(time_ge(1), indexed("a.x", 1))],
                        else_branch: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("a.y0"),
                            subscripts: vec![],
                        }),
                    }),
                }),
            }),
        }
    } else {
        dae::Expression::If {
            branches: vec![(time_ge(1), indexed("a.x", 1))],
            else_branch: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("a.y0"),
                subscripts: vec![],
            }),
        }
    }
}

fn build_enum_direct_assignment_alias_harness(nested: bool) -> NoStateTestHarness {
    let mut dae_model = dae::Dae::default();
    dae_model.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        ),
    ]);

    add_enum_direct_assignment_parameters(&mut dae_model, nested);

    for name in ["a.y", "Adder.a", "Adder.AND.x[2]"] {
        dae_model.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("a.y"),
        enum_direct_assignment_expr(nested),
        rumoca_core::Span::DUMMY,
        "a.y direct assignment",
    ));
    for (name, rhs, origin) in [
        ("Adder.a", "a.y", "Adder.a = a.y"),
        ("Adder.AND.x[2]", "Adder.a", "Adder.AND.x[2] = Adder.a"),
    ] {
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new(name),
            dae::Expression::VarRef {
                name: dae::VarName::new(rhs),
                subscripts: vec![],
            },
            rumoca_core::Span::DUMMY,
            origin,
        ));
    }

    NoStateTestHarness::new(
        dae_model,
        EliminationResult::default(),
        vec![
            "a.y".to_string(),
            "Adder.a".to_string(),
            "Adder.AND.x[2]".to_string(),
        ],
        default_no_state_harness_options(),
    )
}

fn build_eliminated_change_event_model() -> (dae::Dae, EliminationResult) {
    let mut dae_model = dae::Dae::default();
    add_discrete_real_start(&mut dae_model, "tableY", 0.0);
    add_discrete_real_start(&mut dae_model, "nextEvent", 1.0);
    add_discrete_bool_start(&mut dae_model, "flag", false);
    add_discrete_bool_start(&mut dae_model, "on", false);

    let event_due = event_due_expr("nextEvent");
    dae_model.f_z.push(explicit_eq(
        "tableY",
        initial_or_event_pre_expr("tableY", real_lit(0.0), event_due.clone(), real_lit(1.0)),
        "tableY := if initial() then 0 else if time >= pre(nextEvent) then 1 else pre(tableY)",
    ));
    dae_model.f_z.push(explicit_eq(
            "nextEvent",
            initial_or_event_pre_expr("nextEvent", real_lit(1.0), event_due, real_lit(2.0)),
            "nextEvent := if initial() then 1 else if time >= pre(nextEvent) then 2 else pre(nextEvent)",
        ));
    dae_model.f_m.push(explicit_eq(
        "flag",
        dae::Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(var_ref("u")),
            rhs: Box::new(real_lit(0.5)),
        },
        "flag := u >= 0.5",
    ));
    dae_model.f_m.push(explicit_eq(
        "on",
        change_latch_expr("flag"),
        "on := if change(flag) then true else pre(on)",
    ));

    (dae_model, eliminated_substitution("u", "tableY"))
}

fn build_eliminated_event_convergence_model() -> (dae::Dae, EliminationResult) {
    let mut dae_model = dae::Dae::default();
    add_discrete_real_start(&mut dae_model, "nextEventScaled", 1.0);
    add_discrete_bool_start(&mut dae_model, "flag", false);
    add_discrete_bool_start(&mut dae_model, "on", false);

    let event_due = event_due_expr("nextEventScaled");
    dae_model.f_z.push(explicit_eq(
            "nextEventScaled",
            initial_or_event_pre_expr("nextEventScaled", real_lit(1.0), event_due, real_lit(2.0)),
            "nextEventScaled := if initial() then 1 else if time >= pre(nextEventScaled) then 2 else pre(nextEventScaled)",
        ));
    dae_model.f_m.push(explicit_eq(
        "flag",
        dae::Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(var_ref("u")),
            rhs: Box::new(real_lit(1.5)),
        },
        "flag := u >= 1.5",
    ));
    dae_model.f_m.push(explicit_eq(
        "on",
        change_latch_expr("flag"),
        "on := if change(flag) then true else pre(on)",
    ));

    (dae_model, eliminated_substitution("u", "nextEventScaled"))
}

fn build_pre_output_dynamic_event_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let mut next_event = dae::Variable::new(dae::VarName::new("nextEvent"));
    next_event.start = Some(dae::Expression::Literal(dae::Literal::Real(1.0)));
    dae_model
        .discrete_reals
        .insert(dae::VarName::new("nextEvent"), next_event);
    let mut u = dae::Variable::new(dae::VarName::new("u"));
    u.start = Some(dae::Expression::Literal(dae::Literal::Boolean(false)));
    dae_model.discrete_valued.insert(dae::VarName::new("u"), u);
    let mut y = dae::Variable::new(dae::VarName::new("y"));
    y.start = Some(dae::Expression::Literal(dae::Literal::Boolean(false)));
    dae_model.discrete_valued.insert(dae::VarName::new("y"), y);

    let event_due = dae::Expression::Binary {
        op: OpBinary::Ge(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("time"),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("nextEvent"),
                subscripts: vec![],
            }],
        }),
    };
    let initial_event = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Initial,
        args: vec![],
    };

    dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("nextEvent"),
            dae::Expression::If {
                branches: vec![
                    (
                        initial_event.clone(),
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                    ),
                    (
                        event_due.clone(),
                        dae::Expression::Literal(dae::Literal::Real(2.0)),
                    ),
                ],
                else_branch: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("nextEvent"),
                        subscripts: vec![],
                    }],
                }),
            },
            rumoca_core::Span::DUMMY,
            "nextEvent := if initial() then 1 else if time >= pre(nextEvent) then 2 else pre(nextEvent)",
        ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("u"),
        dae::Expression::If {
            branches: vec![
                (
                    initial_event,
                    dae::Expression::Literal(dae::Literal::Boolean(false)),
                ),
                (
                    event_due,
                    dae::Expression::Literal(dae::Literal::Boolean(true)),
                ),
            ],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![dae::Expression::VarRef {
                    name: dae::VarName::new("u"),
                    subscripts: vec![],
                }],
            }),
        },
        rumoca_core::Span::DUMMY,
        "u := if initial() then false else if time >= pre(nextEvent) then true else pre(u)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "y := pre(u)",
    ));

    dae_model
}

mod cases;
