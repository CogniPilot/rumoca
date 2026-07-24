//! End-to-end lowering test over a hand-built DAE with the exact shape the
//! real compiler produces for the discrete-PID reference model (verified
//! facts: fully discrete, one 1 kHz `ClockSchedule`, guarded `f_z`/`f_m`
//! rows `if (c[i] and not __pre__.c[i]) then <body> else (if Initial()
//! then <var> else __pre__.<var>)`, `pre()` eliminated into generated
//! `__pre__.` parameters, the discrete output living in `z` with
//! `causality = output`, the dependent parameter's defining expression
//! preserved symbolically in `start`, and the sample tick lowered to the
//! internal `__rumoca_sample` call in `f_c[0]`).
//!
//! Hand-building `Dae` values is the established repo precedent for
//! DAE-consumer tests; the `ScalarTypeMap` is likewise hand-built (typed
//! test fixture) until `rumoca-compile` wires real Flat-side provenance.

use std::collections::HashMap;

use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Reference, Span, Subscript, VarName,
};
use rumoca_galec_codegen::input::ScalarTypeMap;
use rumoca_galec_codegen::manifest_context::algorithm_code_manifest::{
    BlockCausality, StartValue, Variable as MVar,
};
use rumoca_galec_codegen::{
    AcManifestCtx, AlgorithmCodePackage, GalecInput, GalecOptions, ManifestIdentity, Sha1Hex,
    assemble_manifest_with_identity, c_template_context, lower_to_algorithm_code,
    render_algorithm_code,
};
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast::{ScalarType, Spanned, Statement};

// ---------------------------------------------------------------------
// Expression builders
// ---------------------------------------------------------------------

fn span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("efmi_discrete_pid_fixture.mo"),
        1,
        2,
    )
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: span(),
    }
}

fn boolean(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: span(),
    }
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

/// `c[i]`-style reference with a structured subscript.
fn indexed(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![Subscript::index(index, span())],
        span: span(),
    }
}

fn neg(expr: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span: span(),
    }
}

fn not(expr: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(expr),
        span: span(),
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(),
    }
}

fn if_expr(branches: Vec<(Expression, Expression)>, else_branch: Expression) -> Expression {
    Expression::If {
        branches,
        else_branch: Box::new(else_branch),
        span: span(),
    }
}

fn initial() -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Initial,
        args: Vec::new(),
        span: span(),
    }
}

fn sample_call() -> Expression {
    Expression::FunctionCall {
        name: Reference::generated(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
        args: vec![real(0.0), var("samplePeriod")],
        is_constructor: false,
        span: span(),
    }
}

/// The when-edge of condition `i`: `c[i] and not __pre__.c[i]`.
fn edge_guard(index: i64) -> Expression {
    binary(
        OpBinary::And,
        indexed("c", index),
        not(indexed("__pre__.c", index)),
    )
}

/// The canonical guarded update row: fires on the edge of condition
/// `guard_index`, holds `if Initial() then <target> else __pre__.<target>`
/// otherwise.
fn guarded_update(target: &str, guard_index: i64, body: Expression) -> dae::Equation {
    let hold = if_expr(
        vec![(initial(), var(target))],
        var(&format!("__pre__.{target}")),
    );
    dae::Equation {
        lhs: Some(Reference::new(target)),
        rhs: if_expr(vec![(edge_guard(guard_index), body)], hold),
        span: span(),
        origin: format!("when sample then {target}"),
        scalar_count: 1,
    }
}

// ---------------------------------------------------------------------
// Fixture DAE
// ---------------------------------------------------------------------

fn variable(name: &str) -> dae::Variable {
    let mut variable = dae::Variable::empty_with_span(span());
    variable.name = VarName::new(name);
    variable
}

fn add_input(model: &mut dae::Dae, name: &str) {
    let mut input = variable(name);
    input.causality = dae::VariableCausality::Input;
    input.start = Some(real(0.0));
    input.min = Some(neg(real(1e5)));
    input.max = Some(real(1e5));
    model.variables.inputs.insert(input.name.clone(), input);
}

fn add_tunable(model: &mut dae::Dae, name: &str, start: f64, min: f64, max: f64) {
    let mut parameter = variable(name);
    parameter.causality = dae::VariableCausality::Parameter;
    parameter.is_tunable = true;
    parameter.start = Some(real(start));
    parameter.min = Some(if min < 0.0 {
        neg(real(-min))
    } else {
        real(min)
    });
    parameter.max = Some(real(max));
    model
        .variables
        .parameters
        .insert(parameter.name.clone(), parameter);
}

fn add_z(model: &mut dae::Dae, name: &str) {
    let mut z = variable(name);
    z.start = Some(real(0.0));
    model.variables.discrete_reals.insert(z.name.clone(), z);
}

/// Generated `__pre__.<base>` slot mirroring the base variable's start.
fn add_pre_slot(model: &mut dae::Dae, base: &str, start: Expression, dims: Vec<i64>) {
    let mut slot = variable(&format!("__pre__.{base}"));
    slot.causality = dae::VariableCausality::CalculatedParameter;
    slot.origin = dae::VariableOrigin::Generated;
    slot.fixed = Some(true);
    slot.start = Some(start);
    slot.dims = dims;
    model.variables.parameters.insert(slot.name.clone(), slot);
}

/// The full compiler-output shape of the discrete PID model.
fn pid_dae() -> dae::Dae {
    let mut model = dae::Dae::default();
    add_pid_variables(&mut model);
    add_pid_conditions(&mut model);
    add_pid_updates(&mut model);
    model.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 1e-3,
        phase_seconds: 0.0,
        source_span: span(),
    });
    model
}

fn add_pid_variables(model: &mut dae::Dae) {
    add_input(model, "wLoadRef");
    add_input(model, "wMotor");

    // Discrete output: lives in `z`, keeps causality=output (brief fact 3).
    let mut v_motor = variable("vMotor");
    v_motor.causality = dae::VariableCausality::Output;
    v_motor.start = Some(real(0.0));
    v_motor.min = Some(neg(real(1e7)));
    v_motor.max = Some(real(1e7));
    model
        .variables
        .discrete_reals
        .insert(v_motor.name.clone(), v_motor);

    add_tunable(model, "limiterUMax", 400.0, 1.0, 1e5);
    add_tunable(model, "gearRatio", 105.0, 10.0, 500.0);
    add_tunable(model, "Ti", 0.1, 1e-7, 100.0);
    add_tunable(model, "Td", 0.1, 1e-7, 100.0);
    add_tunable(model, "kd", 0.1, 0.0, 1000.0);
    add_tunable(model, "k", 10.0, 0.0, 1000.0);
    add_tunable(model, "stepSize", 1e-3, 1e-10, 0.01);

    // Dependent parameter: defining expression preserved symbolically in
    // `start` (brief fact 4).
    let mut limiter_u_min = variable("limiterUMin");
    limiter_u_min.causality = dae::VariableCausality::CalculatedParameter;
    limiter_u_min.is_tunable = false;
    limiter_u_min.start = Some(neg(var("limiterUMax")));
    limiter_u_min.min = Some(neg(real(1e5)));
    limiter_u_min.max = Some(neg(real(1.0)));
    model
        .variables
        .parameters
        .insert(limiter_u_min.name.clone(), limiter_u_min);

    // Constant sample period (dedicated constants map, brief fact 5).
    let mut sample_period = variable("samplePeriod");
    sample_period.unit = Some("s".to_owned());
    sample_period.start = Some(real(1e-3));
    model
        .variables
        .constants
        .insert(sample_period.name.clone(), sample_period);

    for name in [
        "pidIx",
        "pidDx",
        "previousFeedback",
        "gainY",
        "feedbackY",
        "derivativePidIx",
        "derivativePidDx",
        "pidDy",
        "pidY",
    ] {
        add_z(model, name);
    }

    let mut first_tick = variable("firstTick");
    first_tick.start = Some(boolean(true));
    model
        .variables
        .discrete_valued
        .insert(first_tick.name.clone(), first_tick);

    // Generated condition vector + its pre slot (dims [4]).
    let mut condition = variable("c");
    condition.origin = dae::VariableOrigin::Generated;
    condition.dims = vec![4];
    model
        .variables
        .discrete_valued
        .insert(condition.name.clone(), condition);
    add_pre_slot(model, "c", boolean(false), vec![4]);

    // Pre slots exist for EVERY when target (the hold branches read them),
    // plus the source-level pre() reads. Only the slots the bodies read may
    // survive into the block.
    for base in [
        "pidIx",
        "pidDx",
        "previousFeedback",
        "gainY",
        "feedbackY",
        "derivativePidIx",
        "derivativePidDx",
        "pidDy",
        "pidY",
        "vMotor",
    ] {
        add_pre_slot(model, base, real(0.0), Vec::new());
    }
    add_pre_slot(model, "firstTick", boolean(true), Vec::new());
}

fn add_pid_conditions(model: &mut dae::Dae) {
    // f_c: sample tick, the firstTick branch condition, and the two limiter
    // relations (4 conditions, brief headline).
    let f_c: [(usize, Expression); 4] = [
        (1, sample_call()),
        (2, var("__pre__.firstTick")),
        (3, binary(OpBinary::Gt, var("pidY"), var("limiterUMax"))),
        (4, binary(OpBinary::Lt, var("pidY"), var("limiterUMin"))),
    ];
    for (index, rhs) in f_c {
        model.conditions.relations.push(rhs.clone());
        model.conditions.equations.push(dae::Equation {
            lhs: Some(Reference::new(format!("c[{index}]"))),
            rhs,
            span: span(),
            origin: format!("condition equation {index}"),
            scalar_count: 1,
        });
    }
}

fn add_pid_updates(model: &mut dae::Dae) {
    // f_z rows happen to be in the source algorithm's dependency order
    // here, but lowering must not rely on that: MLS B.1b rows are
    // simultaneous (see `do_step_is_ordered_by_dependencies_not_dae_order`).
    // All rows are guarded by the sample-tick when-edge (brief fact 1).
    for (target, body) in pid_integrator_rows().into_iter().chain(pid_output_rows()) {
        model
            .discrete
            .real_updates
            .push(guarded_update(target, 1, body));
    }

    // f_m: firstTick (Boolean) update.
    model.discrete.valued_updates.push(guarded_update(
        "firstTick",
        1,
        if_expr(
            vec![(var("__pre__.firstTick"), boolean(false))],
            var("__pre__.firstTick"),
        ),
    ));
}

/// Body kept on the first tick (`if pre(firstTick) then keep else compute`).
fn first_tick_guard(kept: &str, body: Expression) -> Expression {
    if_expr(
        vec![(var("__pre__.firstTick"), var(&format!("__pre__.{kept}")))],
        body,
    )
}

fn pid_integrator_rows() -> Vec<(&'static str, Expression)> {
    vec![
        (
            "derivativePidIx",
            first_tick_guard(
                "derivativePidIx",
                binary(OpBinary::Div, var("__pre__.previousFeedback"), var("Ti")),
            ),
        ),
        (
            "derivativePidDx",
            first_tick_guard(
                "derivativePidDx",
                binary(
                    OpBinary::Div,
                    binary(
                        OpBinary::Sub,
                        var("__pre__.previousFeedback"),
                        var("__pre__.pidDx"),
                    ),
                    var("Td"),
                ),
            ),
        ),
        (
            "pidIx",
            first_tick_guard(
                "pidIx",
                binary(
                    OpBinary::Add,
                    var("__pre__.pidIx"),
                    binary(OpBinary::Mul, var("stepSize"), var("derivativePidIx")),
                ),
            ),
        ),
        (
            "pidDx",
            first_tick_guard(
                "pidDx",
                binary(
                    OpBinary::Add,
                    var("__pre__.pidDx"),
                    binary(OpBinary::Mul, var("stepSize"), var("derivativePidDx")),
                ),
            ),
        ),
    ]
}

fn pid_output_rows() -> Vec<(&'static str, Expression)> {
    vec![
        (
            "gainY",
            binary(OpBinary::Mul, var("gearRatio"), var("wLoadRef")),
        ),
        (
            "feedbackY",
            binary(OpBinary::Sub, var("gainY"), var("wMotor")),
        ),
        (
            "pidDy",
            binary(
                OpBinary::Div,
                binary(
                    OpBinary::Mul,
                    var("kd"),
                    binary(OpBinary::Sub, var("feedbackY"), var("pidDx")),
                ),
                var("Td"),
            ),
        ),
        (
            "pidY",
            binary(
                OpBinary::Mul,
                var("k"),
                binary(
                    OpBinary::Add,
                    binary(OpBinary::Add, var("pidDy"), var("pidIx")),
                    var("feedbackY"),
                ),
            ),
        ),
        // The limiter references the canonical condition slots c[3]/c[4];
        // lowering must inline the relations, never emit `c` reads.
        (
            "vMotor",
            if_expr(
                vec![
                    (indexed("c", 3), var("limiterUMax")),
                    (indexed("c", 4), var("limiterUMin")),
                ],
                var("pidY"),
            ),
        ),
        ("previousFeedback", var("feedbackY")),
    ]
}

fn pid_types() -> ScalarTypeMap {
    let mut types = HashMap::new();
    for name in [
        "limiterUMax",
        "limiterUMin",
        "gearRatio",
        "Ti",
        "Td",
        "kd",
        "k",
        "stepSize",
        "samplePeriod",
    ] {
        types.insert(VarName::new(name), ScalarType::Real);
    }
    types.insert(VarName::new("firstTick"), ScalarType::Boolean);
    types
}

fn lower_pid() -> rumoca_galec_codegen::AlgorithmCodePackage {
    let model = pid_dae();
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    match lower_to_algorithm_code(&input, &GalecOptions::default()) {
        Ok(package) => package,
        Err(errors) => panic!("lowering failed: {errors:#?}"),
    }
}

// ---------------------------------------------------------------------
// (i) Rendered .alg
// ---------------------------------------------------------------------

#[test]
fn rendered_alg_has_the_block_interface_and_no_projection_artifacts() {
    let package = lower_pid();
    let alg = render_algorithm_code(&package).expect("renders");

    for section in ["method Startup", "method Recalibrate", "method DoStep"] {
        assert!(alg.contains(section), "missing `{section}`:\n{alg}");
    }
    // The guarded form dissolved: no condition machinery, no __pre__ names.
    assert!(!alg.contains("__pre__"), "pre artifacts leaked:\n{alg}");
    assert!(!alg.contains("self.c"), "condition vector leaked:\n{alg}");
    assert!(
        !alg.contains("Initial"),
        "hold-branch Initial() leaked:\n{alg}"
    );
    // Limiter conditions inlined back to their relations.
    assert!(
        alg.contains("if self.pidY > self.limiterUMax then self.limiterUMax"),
        "limiter relation not inlined:\n{alg}"
    );
    // Unreferenced hold-only pre slots are dropped entirely.
    assert!(
        !alg.contains("previous(gainY)"),
        "vestigial hold-branch pre slot survived:\n{alg}"
    );
}

#[test]
fn do_step_ends_with_previous_commits_for_exactly_the_read_slots() {
    let package = lower_pid();
    let alg = render_algorithm_code(&package).expect("renders");

    assert!(
        alg.contains("self.'previous(previousFeedback)' := self.previousFeedback;"),
        "missing previous-commit:\n{alg}"
    );
    // Commits come after every update row in DoStep.
    let do_step = alg
        .split("method DoStep")
        .nth(1)
        .expect("DoStep section exists");
    let is_commit =
        |line: &&str| line.trim_start().starts_with("self.'previous(") && line.contains(":=");
    let first_commit_line = do_step
        .lines()
        .position(|line| is_commit(&line))
        .expect("commit inside DoStep");
    let last_update_line = do_step
        .lines()
        .position(|line| line.contains("self.vMotor :="))
        .expect("vMotor update");
    assert!(
        first_commit_line > last_update_line,
        "commits must come at the END of DoStep:\n{alg}"
    );
    // Exactly the read slots commit: 6 referenced pre slots.
    let commit_count = do_step
        .lines()
        .filter(|line| line.trim_start().starts_with("self.'previous(") && line.contains(":="))
        .count();
    assert_eq!(
        commit_count, 6,
        "expected 6 previous-commits, DoStep was:\n{do_step}"
    );
}

#[test]
fn startup_initializes_everything_and_recalibrate_recomputes_dependents() {
    let package = lower_pid();
    let alg = render_algorithm_code(&package).expect("renders");

    let section = |name: &str| {
        alg.split(&format!("method {name}"))
            .nth(1)
            .unwrap_or_else(|| panic!("{name} section"))
            .split("end ")
            .next()
            .unwrap()
            .to_owned()
    };
    let startup = section("Startup");
    // Literal starts, including outputs (GAL-017; control inputs are
    // read-only in GALEC and excluded).
    for statement in [
        "self.vMotor := 0.0;",
        "self.limiterUMax := 400.0;",
        "self.samplePeriod := 0.001;",
        "self.firstTick := true;",
        "self.'previous(firstTick)' := true;",
    ] {
        assert!(
            startup.contains(statement),
            "missing `{statement}`:\n{startup}"
        );
    }
    // Dependent parameter recomputed symbolically, never folded.
    assert!(
        startup.contains("self.limiterUMin := -self.limiterUMax;"),
        "dependent parameter must be recomputed symbolically:\n{startup}"
    );
    let recalibrate = section("Recalibrate");
    assert!(
        recalibrate.contains("self.limiterUMin := -self.limiterUMax;"),
        "Recalibrate must recompute dependents:\n{recalibrate}"
    );
}

// ---------------------------------------------------------------------
// (ii) Manifest fragment
// ---------------------------------------------------------------------

#[test]
fn manifest_wires_the_clock_to_the_sample_period_constant() {
    let package = lower_pid();
    let sample_period = package
        .manifest
        .variables
        .iter()
        .find(|variable| variable.common().name.as_str() == "samplePeriod")
        .expect("samplePeriod listed");
    assert_eq!(
        sample_period.common().block_causality,
        BlockCausality::Constant
    );
    let MVar::Real(real) = sample_period else {
        panic!("samplePeriod must be Real");
    };
    assert_eq!(real.start, StartValue::Scalar(1e-3));
    assert_eq!(
        package.manifest.clock_variable_ref_id.as_str(),
        sample_period.common().id.as_str(),
        "Clock must reference the samplePeriod constant"
    );
}

#[test]
fn manifest_variables_mirror_startup_and_keep_causalities() {
    let package = lower_pid();
    let find = |name: &str| {
        package
            .manifest
            .variables
            .iter()
            .find(|variable| variable.common().name.as_str() == name)
            .unwrap_or_else(|| panic!("{name} listed"))
    };
    assert_eq!(
        find("vMotor").common().block_causality,
        BlockCausality::Output
    );
    assert_eq!(
        find("limiterUMax").common().block_causality,
        BlockCausality::TunableParameter
    );
    assert_eq!(
        find("limiterUMin").common().block_causality,
        BlockCausality::DependentParameter
    );
    let MVar::Real(limiter_u_min) = find("limiterUMin") else {
        panic!("limiterUMin must be Real");
    };
    assert_eq!(limiter_u_min.start, StartValue::Scalar(-400.0));
    let MVar::Boolean(previous_first_tick) = find("previous(firstTick)") else {
        panic!("previous(firstTick) must be Boolean");
    };
    assert_eq!(previous_first_tick.start, StartValue::Scalar(true));
    assert_eq!(
        find("previous(firstTick)").common().block_causality,
        BlockCausality::State
    );
    // Projection-internal machinery and vestigial pre slots are unlisted.
    for absent in ["c", "previous(c)", "previous(gainY)", "previous(vMotor)"] {
        assert!(
            package
                .manifest
                .variables
                .iter()
                .all(|variable| variable.common().name.as_str() != absent),
            "`{absent}` must not be listed"
        );
    }
}

// ---------------------------------------------------------------------
// (iii) Post-validation through the public API + facades
// ---------------------------------------------------------------------

#[test]
fn lowered_block_passes_the_galec_validator() {
    let package = lower_pid();
    rumoca_ir_galec::validate(&package.block).expect("emitted block must validate");
}

/// Assemble the typed AC manifest for `package` (with a freshly minted
/// identity and the real `.alg` SHA-1) and serialize the product-agnostic
/// [`AcManifestCtx`] the manifest template consumes — the serializers are gone
/// (SPEC_0034 D3 amended), so assertions target the serializable context.
fn ac_manifest_ctx(package: &AlgorithmCodePackage) -> serde_json::Value {
    let alg = render_algorithm_code(package).expect("alg renders");
    let identity = ManifestIdentity::generated().expect("identity mints");
    let manifest =
        assemble_manifest_with_identity(package, Sha1Hex::of_bytes(alg.as_bytes()), &identity)
            .expect("manifest assembles");
    serde_json::to_value(AcManifestCtx::from_manifest(&manifest)).expect("ctx serializes")
}

#[test]
fn manifest_context_carries_the_clock_wiring() {
    let package = lower_pid();
    let ctx = ac_manifest_ctx(&package);
    assert!(
        ctx["variables"]
            .as_array()
            .expect("variables array")
            .iter()
            .any(|variable| variable["name"] == "samplePeriod"),
        "{ctx}"
    );
    assert_eq!(
        ctx["clock"]["variable_ref_id"],
        package.manifest.clock_variable_ref_id.as_str(),
        "{ctx}"
    );
}

#[test]
fn c_template_context_serializes_the_c_export_shape() {
    let package = lower_pid();
    let context = c_template_context(&package, "EfmiDiscretePid").expect("context serializes");
    assert_eq!(context["model_name"], "EfmiDiscretePid");
    assert_eq!(context["block_name"], "EfmiDiscretePid");
    assert_eq!(context["struct_name"], "EfmiDiscretePidState");
    assert_eq!(context["function_prefix"], "EfmiDiscretePid");
    assert_eq!(context["include_guard"], "EFMIDISCRETEPID_GALEC_C_H");
    assert!(
        context["variables"]
            .as_array()
            .expect("variables array")
            .iter()
            .any(|variable| variable["name"] == "vMotor"
                && variable["scalar_type"] == "real"
                && variable["c_name"] == "vMotor")
    );
    let do_step = context["methods"]["do_step"]
        .as_array()
        .expect("do_step statements");
    assert!(!do_step.is_empty());
    for statement in do_step {
        assert!(
            matches!(statement["kind"].as_str(), Some("assign" | "copy")),
            "every item is structured C codegen IR: {statement}"
        );
        assert!(statement["target"]["name"].is_string(), "{statement}");
        assert!(statement.get("c_lines").is_none(), "{statement}");
    }
    // The end-of-DoStep pre-commit surfaces as a structured mangled target.
    assert!(
        do_step.iter().any(|statement| {
            statement["target"]["name"]
                .as_str()
                .is_some_and(|name| name.contains("previous_"))
        }),
        "expected a 'previous(x)' commit in structured C IR: {do_step:#?}"
    );
}

// ---------------------------------------------------------------------
// Negative: unrecognizable / non-clock guards stay loud
// ---------------------------------------------------------------------

#[test]
fn non_clock_when_guard_is_a_stable_unsupported_feature() {
    let mut model = pid_dae();
    // Re-guard one row with the limiter condition's edge instead of the
    // sample tick.
    let row = guarded_update("gainY", 3, var("wLoadRef"));
    model.discrete.real_updates[4] = row;
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default()).unwrap_err();
    assert!(
        errors.iter().any(|error| {
            error.code() == "ET017"
                && error
                    .to_string()
                    .contains("unsupported-feature:non-clock-when")
        }),
        "{errors:#?}"
    );
}

#[test]
fn unguarded_update_row_is_rejected_never_silently_lowered() {
    let mut model = pid_dae();
    model.discrete.real_updates[4] = dae::Equation {
        lhs: Some(Reference::new("gainY")),
        rhs: var("wLoadRef"),
        span: span(),
        origin: "bare".to_owned(),
        scalar_count: 1,
    };
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default()).unwrap_err();
    assert!(
        errors.iter().any(|error| {
            error
                .to_string()
                .contains("unsupported-feature:when-update-form")
        }),
        "{errors:#?}"
    );
}

// ---------------------------------------------------------------------
// DoStep dependency ordering (MLS B.1b rows are simultaneous)
// ---------------------------------------------------------------------

/// Byte offset of `needle` inside the DoStep section of `alg`.
fn do_step_position(alg: &str, needle: &str) -> usize {
    alg.split("method DoStep")
        .nth(1)
        .expect("DoStep section exists")
        .find(needle)
        .unwrap_or_else(|| panic!("missing `{needle}` in DoStep:\n{alg}"))
}

#[test]
fn do_step_is_ordered_by_dependencies_not_dae_order() {
    // Equations are declarative: reverse the f_z rows so raw partition
    // order is maximally anti-causal. Lowering must still emit a causal
    // DoStep (raw-order emission would compute stale values silently).
    let mut model = pid_dae();
    model.discrete.real_updates.reverse();
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let package = lower_to_algorithm_code(&input, &GalecOptions::default())
        .expect("non-causally-ordered rows must still lower");
    let alg = render_algorithm_code(&package).expect("renders");
    for (before, after) in [
        ("self.gainY :=", "self.feedbackY :="),
        ("self.feedbackY :=", "self.pidDy :="),
        ("self.derivativePidDx :=", "self.pidDx :="),
        ("self.pidDx :=", "self.pidDy :="),
        ("self.derivativePidIx :=", "self.pidIx :="),
        ("self.pidIx :=", "self.pidY :="),
        ("self.pidDy :=", "self.pidY :="),
        // vMotor's inlined limiter conditions read pidY at the current tick.
        ("self.pidY :=", "self.vMotor :="),
        ("self.feedbackY :=", "self.previousFeedback :="),
    ] {
        assert!(
            do_step_position(&alg, before) < do_step_position(&alg, after),
            "`{before}` must precede `{after}`:\n{alg}"
        );
    }
}

#[test]
fn causal_dae_order_is_kept_stable() {
    // The fixture's rows are already causal; the stable sort must keep
    // their order (no gratuitous reordering of independent rows).
    let package = lower_pid();
    let alg = render_algorithm_code(&package).expect("renders");
    let mut last = 0;
    for target in [
        "self.derivativePidIx :=",
        "self.derivativePidDx :=",
        "self.pidIx :=",
        "self.pidDx :=",
        "self.gainY :=",
        "self.feedbackY :=",
        "self.pidDy :=",
        "self.pidY :=",
        "self.vMotor :=",
        "self.previousFeedback :=",
    ] {
        let position = do_step_position(&alg, target);
        assert!(last < position, "`{target}` out of source order:\n{alg}");
        last = position;
    }
}

#[test]
fn discrete_algebraic_loop_is_rejected_never_misordered() {
    let mut model = pid_dae();
    // gainY and feedbackY read each other at the current tick: a discrete
    // algebraic loop no sequential DoStep order can honor.
    model.discrete.real_updates[4] = guarded_update("gainY", 1, var("feedbackY"));
    model.discrete.real_updates[5] = guarded_update(
        "feedbackY",
        1,
        binary(OpBinary::Sub, var("gainY"), var("wMotor")),
    );
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default()).unwrap_err();
    assert!(
        errors.iter().any(|error| {
            error.code() == "ET017"
                && error
                    .to_string()
                    .contains("unsupported-feature:discrete-algebraic-loop")
        }),
        "{errors:#?}"
    );
}

#[test]
fn self_referencing_update_is_a_discrete_algebraic_loop() {
    let mut model = pid_dae();
    model.discrete.real_updates[4] =
        guarded_update("gainY", 1, binary(OpBinary::Add, var("gainY"), real(1.0)));
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default()).unwrap_err();
    assert!(
        errors.iter().any(|error| error
            .to_string()
            .contains("unsupported-feature:discrete-algebraic-loop")),
        "{errors:#?}"
    );
}

#[test]
fn errors_are_collected_across_rows() {
    let mut model = pid_dae();
    model.discrete.real_updates[4] = guarded_update("gainY", 3, var("wLoadRef"));
    model.discrete.real_updates[5] = guarded_update("feedbackY", 4, var("wMotor"));
    let types = pid_types();
    let input = GalecInput::new(&model, "EfmiDiscretePid").with_scalar_types(&types);
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default()).unwrap_err();
    assert!(errors.len() >= 2, "{errors:#?}");
}

// ---------------------------------------------------------------------
// (vii) D11 codegen provenance
// ---------------------------------------------------------------------

/// SPEC_0034 D11 / GAL-014: codegen-generated statements carry the originating
/// Modelica span, not unconditional `Span::DUMMY`. Every equation and variable
/// in the fixture is stamped with `span()`, so every lowered *real* statement
/// (DoStep updates, Startup inits, Recalibrate recomputes) must carry it, while
/// genuinely-synthesized bookkeeping — the `'previous(x)' := x` commits — stays
/// `DUMMY` (the honest "or `Span::DUMMY` when none" half of the contract).
#[test]
fn generated_statements_carry_originating_modelica_spans() {
    let package = lower_pid();
    let block = &package.block;

    // Each method carries real provenance, and every non-dummy span it carries
    // is the fixture's source span — never an unrelated or fabricated one.
    let assert_provenance = |method: &str, statements: &[Spanned<Statement>]| {
        let real: Vec<Span> = statements
            .iter()
            .filter(|statement| !statement.span.is_dummy())
            .map(|statement| statement.span)
            .collect();
        assert!(
            !real.is_empty(),
            "{method} statements must carry the originating Modelica span (D11), \
             not all DUMMY"
        );
        assert!(
            real.iter().all(|carried| *carried == span()),
            "{method} real spans must be the fixture source span, got {real:?}"
        );
    };
    assert_provenance("DoStep", &block.do_step.statements);
    assert_provenance("Startup", &block.startup.statements);
    assert_provenance("Recalibrate", &block.recalibrate.statements);

    // The synthesized end-of-DoStep `'previous(x)' := x` commits have no
    // originating Modelica construct, so they honestly carry DUMMY.
    assert!(
        block
            .do_step
            .statements
            .iter()
            .any(|statement| statement.span.is_dummy()),
        "synthesized pre-commits must carry DUMMY (no originating construct)"
    );
}
