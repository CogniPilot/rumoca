//! SPEC_0034 Phase-3 non-negotiable test battery, exercised end-to-end
//! through the public projection API (`lower_to_algorithm_code`,
//! `render_algorithm_code`, `assemble_manifest_with_identity` + the
//! serializable `AcManifestCtx` view — the typed serializers are gone,
//! SPEC_0034 D3 amended):
//!
//! - GAL-025 scope rejections (continuous states, external functions,
//!   runtime events, dynamic/sample-expression clocks, multi-clock and
//!   multi-rate models) as stable diagnostics, never panics;
//! - GAL-005 builtin parity anchored to the §3.2.6 catalog (every emittable
//!   target exists there with the emitted arity; unlowerable builtins get
//!   distinct stable `unsupported-feature` ids);
//! - GAL-015 reserved-name handling through rendering (quoted identifiers,
//!   injectivity, `'previous(x)'` round-trip as exact printed text);
//! - S8/GAL-007 type-inference failure as a diagnostic, never a default;
//! - GAL-017 block interface: parameter-free method headers, Startup
//!   covering every non-input manifest variable with values mirroring the
//!   manifest `start`s, and Recalibrate emitted even when empty.
//!
//! Hand-building `Dae` values is the established repo precedent for
//! DAE-consumer tests (see `projection_front_half.rs`); the fixture mirrors
//! the verified real-compiler shape: guarded `f_z`/`f_m` rows, a generated
//! condition vector, and the sample tick as `__rumoca_sample` in `f_c[1]`.

use std::collections::{HashMap, HashSet};

use rumoca_core::{
    BuiltinFunction, ComponentReference, Expression, Function, FunctionParam, Literal, OpBinary,
    OpUnary, Reference, Span, Statement, StatementBlock, Subscript, VarName,
};
use rumoca_galec_codegen::input::ScalarTypeMap;
use rumoca_galec_codegen::lower::emittable_builtin_targets;
use rumoca_galec_codegen::mangle::manifest_name;
use rumoca_galec_codegen::manifest_context::Sha1Hex;
use rumoca_galec_codegen::manifest_context::algorithm_code_manifest::{
    BlockCausality, StartValue, Variable as MVar,
};
use rumoca_galec_codegen::manifest_context::manifest_common::FileChecksum;
use rumoca_galec_codegen::{
    AcManifestCtx, AlgorithmCodePackage, GalecInput, GalecOptions, GalecTargetError,
    ManifestIdentity, assemble_manifest_with_identity, c_template_context, lower_to_algorithm_code,
    render_algorithm_code,
};
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast::{self as gast, ScalarType};
use rumoca_ir_galec::builtins::{find_builtin, is_appendix_c_reserved};

// ---------------------------------------------------------------------
// Expression builders
// ---------------------------------------------------------------------

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

fn boolean(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: Span::DUMMY,
    }
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn indexed(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![Subscript::index(index, Span::DUMMY)],
        span: Span::DUMMY,
    }
}

fn not(expr: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(expr),
        span: Span::DUMMY,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn if_expr(branches: Vec<(Expression, Expression)>, else_branch: Expression) -> Expression {
    Expression::If {
        branches,
        else_branch: Box::new(else_branch),
        span: Span::DUMMY,
    }
}

fn builtin(function: BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: Span::DUMMY,
    }
}

fn sample_call() -> Expression {
    sample_call_with_period(var("samplePeriod"))
}

fn sample_call_with_period(period: Expression) -> Expression {
    Expression::FunctionCall {
        name: Reference::generated(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
        args: vec![real(0.0), period],
        is_constructor: false,
        span: Span::DUMMY,
    }
}

// ---------------------------------------------------------------------
// Fixture: minimal admissible discrete model with one guarded update
// ---------------------------------------------------------------------

fn variable(name: &str) -> dae::Variable {
    let mut variable = dae::Variable::empty_with_span(Span::DUMMY);
    variable.name = VarName::new(name);
    variable
}

fn add_pre_slot(model: &mut dae::Dae, base: &str, start: Expression, dims: Vec<i64>) {
    let mut slot = variable(&format!("__pre__.{base}"));
    slot.causality = dae::VariableCausality::CalculatedParameter;
    slot.origin = dae::VariableOrigin::Generated;
    slot.fixed = Some(true);
    slot.start = Some(start);
    slot.dims = dims;
    model.variables.parameters.insert(slot.name.clone(), slot);
}

fn add_state(model: &mut dae::Dae, name: &str) {
    let mut z = variable(name);
    z.start = Some(real(0.0));
    model.variables.discrete_reals.insert(z.name.clone(), z);
    add_pre_slot(model, name, real(0.0), Vec::new());
}

/// The canonical guarded row: fires on the sample-tick when-edge of
/// condition 1, holds `if Initial() then <target> else __pre__.<target>`.
fn guarded_update(target: &str, body: Expression) -> dae::Equation {
    guarded_update_on_condition(target, body, 1)
}

fn guarded_update_on_condition(
    target: &str,
    body: Expression,
    condition_index: i64,
) -> dae::Equation {
    let edge = binary(
        OpBinary::And,
        indexed("c", condition_index),
        not(indexed("__pre__.c", condition_index)),
    );
    let hold = if_expr(
        vec![(builtin(BuiltinFunction::Initial, Vec::new()), var(target))],
        var(&format!("__pre__.{target}")),
    );
    dae::Equation {
        lhs: Some(Reference::new(target)),
        rhs: if_expr(vec![(edge, body)], hold),
        span: Span::DUMMY,
        origin: format!("when sample then {target}"),
        scalar_count: 1,
    }
}

/// Minimal real-compiler-shaped model: inputs `u`/`x2`, Integer tunables
/// `i1`/`i2`, constant `samplePeriod`, discrete state `y` updated by `body`
/// on the sample tick, condition vector `c[1]` defined by the internal
/// sample call.
fn model_with_body(body: Expression) -> dae::Dae {
    let mut model = dae::Dae::default();
    for name in ["u", "x2"] {
        let mut input = variable(name);
        input.causality = dae::VariableCausality::Input;
        input.start = Some(real(0.0));
        model.variables.inputs.insert(input.name.clone(), input);
    }
    for (name, start) in [("i1", 3), ("i2", 4)] {
        let mut parameter = variable(name);
        parameter.causality = dae::VariableCausality::Parameter;
        parameter.is_tunable = true;
        parameter.start = Some(integer(start));
        model
            .variables
            .parameters
            .insert(parameter.name.clone(), parameter);
    }
    let mut sample_period = variable("samplePeriod");
    sample_period.unit = Some("s".to_owned());
    sample_period.start = Some(real(1e-3));
    model
        .variables
        .constants
        .insert(sample_period.name.clone(), sample_period);

    add_state(&mut model, "y");

    let mut condition = variable("c");
    condition.origin = dae::VariableOrigin::Generated;
    condition.dims = vec![1];
    model
        .variables
        .discrete_valued
        .insert(condition.name.clone(), condition);
    add_pre_slot(&mut model, "c", boolean(false), vec![1]);

    model.conditions.relations.push(sample_call());
    model.conditions.equations.push(dae::Equation {
        lhs: Some(Reference::new("c[1]")),
        rhs: sample_call(),
        span: Span::DUMMY,
        origin: "condition equation 1".to_owned(),
        scalar_count: 1,
    });

    model.discrete.real_updates.push(guarded_update("y", body));
    model.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 1e-3,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });
    model
}

fn base_types() -> ScalarTypeMap {
    let mut types = HashMap::new();
    types.insert(VarName::new("samplePeriod"), ScalarType::Real);
    types.insert(VarName::new("i1"), ScalarType::Integer);
    types.insert(VarName::new("i2"), ScalarType::Integer);
    types
}

fn lower(model: &dae::Dae, types: &ScalarTypeMap) -> AlgorithmCodePackage {
    let input = GalecInput::new(model, "Battery").with_scalar_types(types);
    match lower_to_algorithm_code(&input, &GalecOptions::default()) {
        Ok(package) => package,
        Err(errors) => panic!("lowering failed: {errors:#?}"),
    }
}

fn lower_err(model: &dae::Dae, types: &ScalarTypeMap) -> Vec<GalecTargetError> {
    let input = GalecInput::new(model, "Battery").with_scalar_types(types);
    lower_to_algorithm_code(&input, &GalecOptions::default())
        .map(|_| ())
        .expect_err("lowering must fail")
}

fn render_with_body(body: Expression) -> String {
    let package = lower(&model_with_body(body), &base_types());
    render_algorithm_code(&package).expect("renders")
}

fn assert_unsupported(errors: &[GalecTargetError], feature: &str) {
    let marker = format!("unsupported-feature:{feature}]");
    assert!(
        errors
            .iter()
            .any(|error| error.code() == "ET017" && error.to_string().contains(&marker)),
        "expected `{marker}` among: {errors:#?}"
    );
}

const GAL_025_WORDING: &str = "not yet supported by the Rumoca GALEC projection";

// ---------------------------------------------------------------------
// 1. Scope rejections through the public API (GAL-025, GAL-016)
// ---------------------------------------------------------------------

mod rejected_constructs {
    use super::*;

    /// Every v1 scope rejection surfaces through `lower_to_algorithm_code`
    /// as its stable code with the GAL-025 wording — never a panic, never
    /// blamed on eFMI.
    #[test]
    fn scope_rejections_carry_stable_codes_and_gal025_wording() {
        type Mutate = fn(&mut dae::Dae);
        let cases: &[(&str, &str, Mutate)] = &[
            ("continuous state", "ET001", |model| {
                model
                    .variables
                    .states
                    .insert(VarName::new("x"), variable("x"));
                model.continuous.equations.push(dae::Equation {
                    lhs: Some(Reference::new("x")),
                    rhs: real(0.0),
                    span: Span::DUMMY,
                    origin: "der".to_owned(),
                    scalar_count: 1,
                });
            }),
            ("external function", "ET002", |model| {
                let mut function = rumoca_core::Function::new("tableLookup", Span::DUMMY);
                function.external = Some(rumoca_core::ExternalFunction {
                    language: "C".to_owned(),
                    ..rumoca_core::ExternalFunction::default()
                });
                model
                    .symbols
                    .functions
                    .insert(VarName::new("tableLookup"), function);
            }),
            ("runtime event", "ET003", |model| {
                model.events.scheduled_time_events.push(0.5);
            }),
            ("dynamic clock", "ET004", |model| {
                model.clocks.triggered_conditions.push(boolean(true));
            }),
        ];
        for (label, code, mutate) in cases {
            let mut model = model_with_body(var("u"));
            mutate(&mut model);
            let errors = lower_err(&model, &base_types());
            let found = errors
                .iter()
                .find(|error| error.code() == *code)
                .unwrap_or_else(|| panic!("{label}: no {code} among {errors:#?}"));
            let message = found.to_string();
            assert!(
                message.contains(GAL_025_WORDING),
                "{label}: GAL-025 wording missing in `{message}`"
            );
            assert!(
                !message.contains("unsupported by eFMI"),
                "{label}: must not blame eFMI: `{message}`"
            );
        }
    }

    #[test]
    fn multi_clock_rejected_through_lowering() {
        let mut model = model_with_body(var("u"));
        model.clocks.schedules.push(dae::ClockSchedule {
            period_seconds: 2e-3,
            phase_seconds: 0.0,
            source_span: Span::DUMMY,
        });
        let errors = lower_err(&model, &base_types());
        assert!(
            errors.iter().any(|error| error.code() == "ET005"),
            "{errors:#?}"
        );
    }

    /// Duplicate sample-tick condition slots for the same sample call are one
    /// eFMI clock, not multi-rate. This occurs when an inlined component and
    /// its parent both use `sample(0, dt)`.
    #[test]
    fn duplicate_same_clock_sample_conditions_are_accepted() {
        let mut model = model_with_body(var("u"));
        if let Some(condition) = model.variables.discrete_valued.get_mut(&VarName::new("c")) {
            condition.dims = vec![2];
        }
        let mut component_period = variable("pid.samplePeriod");
        component_period.start = Some(real(1e-3));
        model
            .variables
            .constants
            .insert(component_period.name.clone(), component_period);
        model.conditions.equations.push(dae::Equation {
            lhs: Some(Reference::new("c[2]")),
            rhs: sample_call_with_period(var("pid.samplePeriod")),
            span: Span::DUMMY,
            origin: "condition equation 2".to_owned(),
            scalar_count: 1,
        });
        model.discrete.real_updates.clear();
        model
            .discrete
            .real_updates
            .push(guarded_update_on_condition("y", var("u"), 2));

        let mut types = base_types();
        types.insert(VarName::new("pid.samplePeriod"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("self.y := self.u;"), "{alg}");
    }

    /// Distinct sample-tick condition definitions are genuinely multi-rate
    /// and remain rejected with a stable feature id.
    #[test]
    fn distinct_sample_conditions_are_multi_rate() {
        let mut model = model_with_body(var("u"));
        if let Some(condition) = model.variables.discrete_valued.get_mut(&VarName::new("c")) {
            condition.dims = vec![2];
        }
        model.conditions.equations.push(dae::Equation {
            lhs: Some(Reference::new("c[2]")),
            rhs: sample_call_with_period(real(2e-3)),
            span: Span::DUMMY,
            origin: "condition equation 2".to_owned(),
            scalar_count: 1,
        });
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "multi-rate");
    }

    /// `sample()` used as a value expression (a sample-expression clock)
    /// is rejected, never folded into the block clock.
    #[test]
    fn sample_expression_clock_rejected() {
        let body = if_expr(vec![(sample_call(), var("u"))], var("x2"));
        let errors = lower_err(&model_with_body(body), &base_types());
        assert_unsupported(&errors, "sample-in-expression");
    }
}

// ---------------------------------------------------------------------
// 2. Builtin parity anchored to the §3.2.6 catalog (GAL-005, T5/T8)
// ---------------------------------------------------------------------

mod builtin_parity {
    use super::*;

    /// Every name the lowering can emit exists in the GALEC catalog with
    /// exactly the emitted call arity, one result, and no Appendix C
    /// collision (GAL-005: gate/codegen drift emits nonexistent functions).
    #[test]
    fn every_emittable_target_is_in_the_catalog_with_matching_arity() {
        let targets = emittable_builtin_targets();
        assert!(!targets.is_empty());
        for (name, arity) in targets {
            let catalog = find_builtin(name)
                .unwrap_or_else(|| panic!("`{name}` is not in the §3.2.6 catalog"));
            assert_eq!(
                catalog.inputs.len(),
                arity,
                "`{name}` emitted with arity {arity}, catalog says {}",
                catalog.inputs.len()
            );
            assert_eq!(catalog.outputs.len(), 1, "`{name}` must return one value");
            assert!(
                !is_appendix_c_reserved(name),
                "`{name}` is Appendix-C reserved and must never be emitted"
            );
        }
    }

    /// Accepted Modelica builtins lower and render to their catalog names
    /// with preserved argument order and T5 explicit widening.
    #[test]
    fn accepted_builtins_render_to_catalog_names() {
        use BuiltinFunction as B;
        let unary: &[(B, &str)] = &[
            (B::Abs, "absolute(self.u)"),
            (B::Sign, "sign(self.u)"),
            (B::Sqrt, "sqrt(self.u)"),
            (B::Exp, "exp(self.u)"),
            (B::Log, "ln(self.u)"),
            (B::Log10, "lg(self.u)"),
            (B::Floor, "roundDown(self.u)"),
            (B::Ceil, "roundUp(self.u)"),
            (B::Sin, "sin(self.u)"),
            (B::Cos, "cos(self.u)"),
            (B::Tan, "tan(self.u)"),
            (B::Asin, "asin(self.u)"),
            (B::Acos, "acos(self.u)"),
            (B::Atan, "atan(self.u)"),
            (B::Sinh, "sinh(self.u)"),
            (B::Cosh, "cosh(self.u)"),
            (B::Tanh, "tanh(self.u)"),
        ];
        for (function, expected) in unary {
            let alg = render_with_body(builtin(*function, vec![var("u")]));
            assert!(alg.contains(expected), "missing `{expected}` in:\n{alg}");
        }
        let real_binary: &[(Expression, &str)] = &[
            (
                builtin(B::Atan2, vec![var("u"), var("x2")]),
                // Argument order preserved: the catalog is also `(y, x)`.
                "atan2(self.u, self.x2)",
            ),
            (
                builtin(B::Min, vec![var("u"), var("x2")]),
                "min(self.u, self.x2)",
            ),
            (
                builtin(B::Max, vec![var("u"), var("x2")]),
                "max(self.u, self.x2)",
            ),
            (
                builtin(B::Min, vec![var("i1"), var("i2")]),
                // Integer min: `imin`, then explicit T5 widening to the
                // Real assignment target.
                "real(imin(self.i1, self.i2))",
            ),
            (
                builtin(B::Max, vec![var("i1"), var("i2")]),
                "real(imax(self.i1, self.i2))",
            ),
            (
                builtin(B::Div, vec![var("i1"), var("i2")]),
                "real(divisionTowardsZero(self.i1, self.i2))",
            ),
            (
                // Integer→Real promotion inserts an explicit real() cast
                // exactly where Modelica promotes implicitly (T5).
                binary(OpBinary::Add, var("u"), var("i1")),
                "self.u + real(self.i1)",
            ),
        ];
        for (body, expected) in real_binary {
            let alg = render_with_body(body.clone());
            assert!(alg.contains(expected), "missing `{expected}` in:\n{alg}");
        }
        // noEvent is a semantic pass-through: no call survives.
        let alg = render_with_body(builtin(B::NoEvent, vec![var("u")]));
        assert!(alg.contains("self.y := self.u;"), "{alg}");
        assert!(!alg.contains("noEvent"), "{alg}");
    }

    /// Unlowerable builtins fail with distinct stable feature ids — nothing
    /// outside the catalog is ever emitted.
    #[test]
    fn unlowerable_builtins_get_distinct_stable_feature_ids() {
        use BuiltinFunction as B;
        let cases: &[(Expression, &str)] = &[
            (builtin(B::Mod, vec![var("i1"), var("i2")]), "builtin:mod"),
            (builtin(B::Rem, vec![var("i1"), var("i2")]), "builtin:rem"),
            (builtin(B::Sum, vec![var("u")]), "builtin:Sum"),
            (builtin(B::Integer, vec![var("u")]), "builtin:Integer"),
            // 1-argument min is the array reduction GALEC lacks (T8).
            (builtin(B::Min, vec![var("u")]), "array-reduction:Min"),
        ];
        let mut seen = HashSet::new();
        for (body, feature) in cases {
            assert!(seen.insert(*feature), "feature ids must be distinct");
            let errors = lower_err(&model_with_body(body.clone()), &base_types());
            assert_unsupported(&errors, feature);
        }
    }

    /// Real→Integer narrowing is rejected (D8: GALEC `integer()` truncates
    /// and signals, Modelica floors) — never a silent cast.
    #[test]
    fn real_to_integer_narrowing_is_rejected() {
        let mut model = model_with_body(var("u"));
        let mut counter = variable("counter");
        counter.start = Some(integer(0));
        model
            .variables
            .discrete_valued
            .insert(counter.name.clone(), counter);
        add_pre_slot(&mut model, "counter", integer(0), Vec::new());
        model
            .discrete
            .valued_updates
            .push(guarded_update("counter", var("u")));
        let mut types = base_types();
        types.insert(VarName::new("counter"), ScalarType::Integer);
        let errors = lower_err(&model, &types);
        assert_unsupported(&errors, "real-to-integer-conversion");
    }

    /// D8 slice-1 stance: Real relationals DO lower (with empty escape
    /// accounting), so guarded discrete models with comparisons project.
    #[test]
    fn real_relationals_lower_in_slice_1() {
        let body = if_expr(
            vec![(binary(OpBinary::Gt, var("u"), var("x2")), var("u"))],
            var("x2"),
        );
        let alg = render_with_body(body);
        assert!(
            alg.contains("if self.u > self.x2 then self.u else self.x2"),
            "{alg}"
        );
    }

    /// A canonical `noEvent` call has exactly one argument; extra arguments
    /// are a compiler bug and must be reported (ET018), never silently
    /// truncated to the first argument.
    #[test]
    fn no_event_extra_arguments_are_a_diagnostic_never_truncated() {
        let body = builtin(BuiltinFunction::NoEvent, vec![var("u"), var("x2")]);
        let errors = lower_err(&model_with_body(body), &base_types());
        assert!(
            errors.iter().any(|error| {
                error.code() == "ET018"
                    && error.to_string().contains("NoEvent")
                    && error.to_string().contains("expected 1")
            }),
            "{errors:#?}"
        );
    }
}

// ---------------------------------------------------------------------
// 3. GALEC array/vector projection regressions (GAL-026, T11)
// ---------------------------------------------------------------------

mod array_vector_regressions {
    use super::*;

    fn array(elements: Vec<Expression>) -> Expression {
        Expression::Array {
            elements,
            is_matrix: false,
            span: Span::DUMMY,
        }
    }

    fn index(base: Expression, subscripts: Vec<Subscript>) -> Expression {
        Expression::Index {
            base: Box::new(base),
            subscripts,
            span: Span::DUMMY,
        }
    }

    fn subscript_expr(expr: Expression) -> Subscript {
        Subscript::expr(Box::new(expr), Span::DUMMY)
    }

    fn range(start: i64, end: i64) -> Expression {
        Expression::Range {
            start: Box::new(integer(start)),
            step: None,
            end: Box::new(integer(end)),
            span: Span::DUMMY,
        }
    }

    fn range_subscript(start: i64, end: i64) -> Subscript {
        subscript_expr(range(start, end))
    }

    fn add_real_vector(model: &mut dae::Dae, name: &str, len: i64) {
        let mut vector = variable(name);
        vector.dims = vec![len];
        vector.start = Some(real(0.0));
        model
            .variables
            .discrete_reals
            .insert(vector.name.clone(), vector);
    }

    fn add_real_matrix(model: &mut dae::Dae, name: &str, rows: i64, cols: i64) {
        let mut matrix = variable(name);
        matrix.dims = vec![rows, cols];
        matrix.start = Some(real(0.0));
        model
            .variables
            .discrete_reals
            .insert(matrix.name.clone(), matrix);
    }

    fn vector_model(body: Expression) -> dae::Dae {
        let mut model = model_with_body(body);
        add_real_vector(&mut model, "a", 3);
        add_real_vector(&mut model, "b", 3);
        model
    }

    fn vector_target_model(body: Expression) -> dae::Dae {
        let mut model = vector_model(body);
        model
            .variables
            .discrete_reals
            .get_mut(&VarName::new("y"))
            .expect("y exists")
            .dims = vec![3];
        model
            .variables
            .parameters
            .get_mut(&VarName::new("__pre__.y"))
            .expect("pre y exists")
            .dims = vec![3];
        model
    }

    fn add_waypoints(model: &mut dae::Dae) {
        let mut waypoints = variable("waypoints");
        waypoints.causality = dae::VariableCausality::Parameter;
        waypoints.dims = vec![3, 2];
        model
            .variables
            .parameters
            .insert(waypoints.name.clone(), waypoints);
    }

    fn make_y_vector(model: &mut dae::Dae, len: i64) {
        model
            .variables
            .discrete_reals
            .get_mut(&VarName::new("y"))
            .expect("y exists")
            .dims = vec![len];
        model
            .variables
            .parameters
            .get_mut(&VarName::new("__pre__.y"))
            .expect("pre y exists")
            .dims = vec![len];
    }

    #[test]
    fn dynamic_array_subscript_lowers_to_static_index_selection() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                subscript_expr(var("__pre__.idx")),
                Subscript::index(1, Span::DUMMY),
            ],
            span: Span::DUMMY,
        });
        add_waypoints(&mut model);
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        idx.min = Some(integer(1));
        idx.max = Some(integer(3));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.waypoints[1, 1]"),
            "{alg}"
        );
        assert!(alg.contains("else self.waypoints[3, 1]"), "{alg}");
        assert!(
            !alg.contains("waypoints[self.'previous(idx)'"),
            "runtime subscript must not be emitted into GALEC:\n{alg}"
        );
    }

    #[test]
    fn dynamic_array_subscript_without_proven_bounds_is_rejected() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                subscript_expr(var("__pre__.idx")),
                Subscript::index(1, Span::DUMMY),
            ],
            span: Span::DUMMY,
        });
        add_waypoints(&mut model);
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let errors = lower_err(&model, &types);
        assert_unsupported(&errors, "dynamic-array-subscript-range");
    }

    #[test]
    fn dynamic_array_subscript_with_out_of_range_bounds_is_rejected() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                subscript_expr(var("__pre__.idx")),
                Subscript::index(1, Span::DUMMY),
            ],
            span: Span::DUMMY,
        });
        add_waypoints(&mut model);
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        idx.min = Some(integer(0));
        idx.max = Some(integer(3));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let errors = lower_err(&model, &types);
        assert_unsupported(&errors, "dynamic-array-subscript-range");
    }

    #[test]
    fn array_slice_subscript_lowers_to_static_index_constructor() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                Subscript::index(2, Span::DUMMY),
                Subscript::colon(Span::DUMMY),
            ],
            span: Span::DUMMY,
        });
        add_waypoints(&mut model);
        make_y_vector(&mut model, 2);
        let mut types = base_types();
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("self.waypoints[2, 1]"), "{alg}");
        assert!(alg.contains("self.waypoints[2, 2]"), "{alg}");
        assert!(
            !alg.contains("waypoints[2, :"),
            "GALEC must not receive slice syntax:\n{alg}"
        );
    }

    #[test]
    fn dynamic_row_array_slice_uses_static_selection_per_element() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                subscript_expr(var("__pre__.idx")),
                Subscript::colon(Span::DUMMY),
            ],
            span: Span::DUMMY,
        });
        add_waypoints(&mut model);
        make_y_vector(&mut model, 2);
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        idx.min = Some(integer(1));
        idx.max = Some(integer(3));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.waypoints[1, 1]"),
            "{alg}"
        );
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.waypoints[1, 2]"),
            "{alg}"
        );
        assert!(alg.contains("else self.waypoints[3, 2]"), "{alg}");
        assert!(
            !alg.contains("waypoints[self.'previous(idx)'"),
            "runtime subscript must not be emitted into GALEC:\n{alg}"
        );
        assert!(
            !alg.contains("waypoints[self.'previous(idx)', :"),
            "GALEC must not receive slice syntax:\n{alg}"
        );
    }

    #[test]
    fn structural_range_slice_lowers_to_static_index_constructor() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("a"),
            subscripts: vec![range_subscript(1, 2)],
            span: Span::DUMMY,
        });
        add_real_vector(&mut model, "a", 3);
        make_y_vector(&mut model, 2);
        let mut types = base_types();
        types.insert(VarName::new("a"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("self.a[1]"), "{alg}");
        assert!(alg.contains("self.a[2]"), "{alg}");
        assert!(
            !alg.contains("1:2"),
            "GALEC must not receive range syntax:\n{alg}"
        );
    }

    #[test]
    fn scalarized_index_into_dynamic_row_slice_projects_original_dimension() {
        let row_slice = Expression::VarRef {
            name: Reference::new("waypoints"),
            subscripts: vec![
                subscript_expr(var("__pre__.idx")),
                Subscript::colon(Span::DUMMY),
            ],
            span: Span::DUMMY,
        };
        let mut model = model_with_body(index(row_slice, vec![Subscript::index(2, Span::DUMMY)]));
        add_waypoints(&mut model);
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        idx.min = Some(integer(1));
        idx.max = Some(integer(3));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);
        types.insert(VarName::new("waypoints"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.waypoints[1, 2]"),
            "{alg}"
        );
        assert!(alg.contains("else self.waypoints[3, 2]"), "{alg}");
        assert!(
            !alg.contains("waypoints[self.'previous(idx)', :, 2"),
            "slice result index must project into the original array rank:\n{alg}"
        );
    }

    #[test]
    fn scalarized_index_into_range_slice_projects_original_dimension() {
        let range_slice = Expression::VarRef {
            name: Reference::new("a"),
            subscripts: vec![range_subscript(1, 2)],
            span: Span::DUMMY,
        };
        let mut model = model_with_body(index(range_slice, vec![Subscript::index(2, Span::DUMMY)]));
        add_real_vector(&mut model, "a", 3);
        let mut types = base_types();
        types.insert(VarName::new("a"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("self.a[2]"), "{alg}");
        assert!(
            !alg.contains("1:2"),
            "GALEC must not receive range syntax:\n{alg}"
        );
    }

    #[test]
    fn vector_constructor_inside_if_can_be_indexed_after_dae_scalarization() {
        let vector_if = if_expr(
            vec![(
                binary(OpBinary::Eq, var("i1"), var("i2")),
                array(vec![var("u"), var("x2"), real(3.0)]),
            )],
            array(vec![real(1.0), real(2.0), real(3.0)]),
        );
        let alg = render_algorithm_code(&lower(
            &model_with_body(index(vector_if, vec![Subscript::index(2, Span::DUMMY)])),
            &base_types(),
        ))
        .expect("renders");
        assert!(
            alg.contains("if self.i1 == self.i2 then self.x2 else 2.0"),
            "{alg}"
        );
    }

    #[test]
    fn vector_subtraction_lowers_for_whole_array_and_scalarized_rows() {
        let whole_package = lower(
            &vector_target_model(binary(OpBinary::Sub, var("a"), var("b"))),
            &base_types(),
        );
        let whole = render_algorithm_code(&whole_package).expect("whole vector render");
        assert!(whole.contains("self.y := self.a - self.b;"), "{whole}");

        let c_lines = production_c_lines(&whole_package);
        assert!(
            c_lines.contains("self->y[0]") && c_lines.contains("(self->a[0] - self->b[0])"),
            "{c_lines}"
        );
        assert!(
            !c_lines.contains("self->y ="),
            "C arrays must be assigned element-wise:\n{c_lines}"
        );

        let scalarized = render_algorithm_code(&lower(
            &vector_model(index(
                binary(OpBinary::Sub, var("a"), var("b")),
                vec![Subscript::index(2, Span::DUMMY)],
            )),
            &base_types(),
        ))
        .expect("scalarized vector render");
        assert!(
            scalarized.contains("self.y := self.a[2] - self.b[2];"),
            "{scalarized}"
        );
    }

    #[test]
    fn vector_subtraction_lowers_for_dynamic_scalarized_loop_rows() {
        let mut model = vector_model(index(
            binary(OpBinary::Sub, var("a"), var("b")),
            vec![subscript_expr(var("__pre__.idx"))],
        ));
        let mut idx = variable("idx");
        idx.start = Some(integer(1));
        idx.min = Some(integer(1));
        idx.max = Some(integer(3));
        model
            .variables
            .discrete_valued
            .insert(idx.name.clone(), idx);
        add_pre_slot(&mut model, "idx", integer(1), Vec::new());
        let mut types = base_types();
        types.insert(VarName::new("idx"), ScalarType::Integer);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.a[1]"),
            "{alg}"
        );
        assert!(
            alg.contains("if self.'previous(idx)' == 1 then self.b[1]"),
            "{alg}"
        );
        assert!(
            !alg.contains("(self.a - self.b)["),
            "indexed vector arithmetic must be distributed before GALEC validation:\n{alg}"
        );
    }

    #[test]
    fn constant_integer_subscript_folds_to_literal_static_index() {
        let mut model = model_with_body(Expression::VarRef {
            name: Reference::new("pid_error"),
            subscripts: vec![subscript_expr(var("pitchPid"))],
            span: Span::DUMMY,
        });
        let mut pid_error = variable("pid_error");
        pid_error.dims = vec![3];
        pid_error.start = Some(real(0.0));
        model
            .variables
            .discrete_reals
            .insert(pid_error.name.clone(), pid_error);
        let mut pitch_pid = variable("pitchPid");
        pitch_pid.start = Some(integer(1));
        model
            .variables
            .constants
            .insert(pitch_pid.name.clone(), pitch_pid);
        let mut types = base_types();
        types.insert(VarName::new("pid_error"), ScalarType::Real);
        types.insert(VarName::new("pitchPid"), ScalarType::Integer);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("self.pid_error[1]"), "{alg}");
        assert!(
            !alg.contains("pid_error[self.pitchPid"),
            "constant index must be folded before GALEC validation:\n{alg}"
        );
    }

    #[test]
    fn vector_dot_product_lowers_to_scalar_sum_not_array_multiply() {
        let alg = render_algorithm_code(&lower(
            &vector_model(binary(OpBinary::Mul, var("a"), var("b"))),
            &base_types(),
        ))
        .expect("renders");
        for term in [
            "self.a[1] * self.b[1]",
            "self.a[2] * self.b[2]",
            "self.a[3] * self.b[3]",
        ] {
            assert!(alg.contains(term), "missing `{term}` in:\n{alg}");
        }
        assert!(!alg.contains("self.a * self.b"), "{alg}");
    }

    #[test]
    fn non_vector_array_multiplication_is_rejected_not_elementwise() {
        let mut model = model_with_body(binary(OpBinary::Mul, var("a"), var("b")));
        add_real_matrix(&mut model, "a", 2, 2);
        add_real_matrix(&mut model, "b", 2, 2);
        model
            .variables
            .discrete_reals
            .get_mut(&VarName::new("y"))
            .expect("y exists")
            .dims = vec![2, 2];
        model
            .variables
            .parameters
            .get_mut(&VarName::new("__pre__.y"))
            .expect("pre y exists")
            .dims = vec![2, 2];
        let mut types = base_types();
        types.insert(VarName::new("a"), ScalarType::Real);
        types.insert(VarName::new("b"), ScalarType::Real);

        let errors = lower_err(&model, &types);
        assert_unsupported(&errors, "array-multiplication");
    }

    #[test]
    fn vector_norm_lowers_dot_product_before_sqrt() {
        let norm = builtin(
            BuiltinFunction::Sqrt,
            vec![binary(OpBinary::Mul, var("a"), var("a"))],
        );
        let alg =
            render_algorithm_code(&lower(&vector_model(norm), &base_types())).expect("renders");
        assert!(alg.contains("sqrt("), "{alg}");
        assert!(alg.contains("self.a[1] * self.a[1]"), "{alg}");
        assert!(alg.contains("self.a[3] * self.a[3]"), "{alg}");
        assert!(!alg.contains("sqrt(self.a * self.a)"), "{alg}");
    }

    #[test]
    fn pure_single_output_user_function_call_is_inlined() {
        let mut model = model_with_body(Expression::FunctionCall {
            name: Reference::new("norm3"),
            args: vec![var("velocity")],
            is_constructor: false,
            span: Span::DUMMY,
        });
        add_real_vector(&mut model, "velocity", 3);
        let mut norm3 = Function::new("norm3", Span::DUMMY);
        norm3
            .inputs
            .push(FunctionParam::new("v", "Real", Span::DUMMY).with_dims(vec![3]));
        norm3
            .outputs
            .push(FunctionParam::new("n", "Real", Span::DUMMY));
        let square_sum = binary(
            OpBinary::Add,
            binary(OpBinary::Mul, indexed("v", 1), indexed("v", 1)),
            binary(
                OpBinary::Add,
                binary(OpBinary::Mul, indexed("v", 2), indexed("v", 2)),
                binary(OpBinary::Mul, indexed("v", 3), indexed("v", 3)),
            ),
        );
        norm3.body.push(Statement::Assignment {
            comp: ComponentReference::from_flat_segments("n", Span::DUMMY, None),
            value: builtin(BuiltinFunction::Sqrt, vec![square_sum]),
            span: Span::DUMMY,
        });
        model.symbols.functions.insert(norm3.name.clone(), norm3);
        let mut types = base_types();
        types.insert(VarName::new("velocity"), ScalarType::Real);

        let alg = render_algorithm_code(&lower(&model, &types)).expect("renders");
        assert!(alg.contains("sqrt("), "{alg}");
        assert!(alg.contains("self.velocity[1] * self.velocity[1]"), "{alg}");
        assert!(alg.contains("self.velocity[3] * self.velocity[3]"), "{alg}");
        assert!(!alg.contains("norm3("), "{alg}");
    }

    #[test]
    fn indexed_vector_user_function_call_inlines_before_subscript_lowering() {
        let call = Expression::FunctionCall {
            name: Reference::new("lowPass"),
            args: vec![var("a"), var("b"), real(0.5)],
            is_constructor: false,
            span: Span::DUMMY,
        };
        let mut model = vector_model(index(call, vec![Subscript::index(2, Span::DUMMY)]));
        let mut low_pass = Function::new("lowPass", Span::DUMMY);
        low_pass
            .inputs
            .push(FunctionParam::new("sample", "Real", Span::DUMMY).with_dims(vec![3]));
        low_pass
            .inputs
            .push(FunctionParam::new("previous", "Real", Span::DUMMY).with_dims(vec![3]));
        low_pass
            .inputs
            .push(FunctionParam::new("sampleWeight", "Real", Span::DUMMY));
        low_pass
            .outputs
            .push(FunctionParam::new("result", "Real", Span::DUMMY).with_dims(vec![3]));
        low_pass.body.push(Statement::Assignment {
            comp: ComponentReference::from_flat_segments("result", Span::DUMMY, None),
            value: binary(
                OpBinary::Add,
                binary(OpBinary::Mul, var("sampleWeight"), var("sample")),
                binary(
                    OpBinary::Mul,
                    binary(OpBinary::Sub, real(1.0), var("sampleWeight")),
                    var("previous"),
                ),
            ),
            span: Span::DUMMY,
        });
        model
            .symbols
            .functions
            .insert(low_pass.name.clone(), low_pass);

        let alg = render_algorithm_code(&lower(&model, &base_types())).expect("renders");
        assert!(alg.contains("self.a[2]"), "{alg}");
        assert!(alg.contains("self.b[2]"), "{alg}");
        assert!(
            !alg.contains("lowPass("),
            "inlineable vector helper must not survive into GALEC:\n{alg}"
        );
    }

    #[test]
    fn nested_same_user_function_call_is_not_recursive() {
        let nested = Expression::FunctionCall {
            name: Reference::new("clip"),
            args: vec![
                Expression::FunctionCall {
                    name: Reference::new("clip"),
                    args: vec![var("__pre__.y"), real(-1.0), real(1.0)],
                    is_constructor: false,
                    span: Span::DUMMY,
                },
                real(-2.0),
                real(2.0),
            ],
            is_constructor: false,
            span: Span::DUMMY,
        };
        let mut model = model_with_body(nested);
        add_clip_function(&mut model);

        let alg = render_algorithm_code(&lower(&model, &base_types())).expect("renders");
        assert!(alg.contains("max(self.'previous(y)', -1.0)"), "{alg}");
        assert!(!alg.contains("clip("), "{alg}");
    }

    #[test]
    fn multi_output_user_function_projection_call_inlines_named_output() {
        let call = Expression::FunctionCall {
            name: Reference::new("split.hi"),
            args: vec![var("__pre__.y")],
            is_constructor: false,
            span: Span::DUMMY,
        };
        let mut model = model_with_body(call);
        add_split_function(&mut model);

        let alg = render_algorithm_code(&lower(&model, &base_types())).expect("renders");
        assert!(alg.contains("if self.'previous(y)' > 0.0 then"), "{alg}");
        assert!(!alg.contains("split.hi("), "{alg}");
        assert!(!alg.contains("split("), "{alg}");
    }

    fn add_clip_function(model: &mut dae::Dae) {
        let mut clip = Function::new("clip", Span::DUMMY);
        clip.inputs
            .push(FunctionParam::new("value", "Real", Span::DUMMY));
        clip.inputs
            .push(FunctionParam::new("lower", "Real", Span::DUMMY));
        clip.inputs
            .push(FunctionParam::new("upper", "Real", Span::DUMMY));
        clip.outputs
            .push(FunctionParam::new("result", "Real", Span::DUMMY));
        clip.body.push(Statement::Assignment {
            comp: ComponentReference::from_flat_segments("result", Span::DUMMY, None),
            value: builtin(
                BuiltinFunction::Min,
                vec![
                    builtin(BuiltinFunction::Max, vec![var("value"), var("lower")]),
                    var("upper"),
                ],
            ),
            span: Span::DUMMY,
        });
        model.symbols.functions.insert(clip.name.clone(), clip);
    }

    fn add_split_function(model: &mut dae::Dae) {
        let mut split = Function::new("split", Span::DUMMY);
        split
            .inputs
            .push(FunctionParam::new("u", "Real", Span::DUMMY));
        split
            .outputs
            .push(FunctionParam::new("lo", "Real", Span::DUMMY));
        split
            .outputs
            .push(FunctionParam::new("hi", "Real", Span::DUMMY));
        split.body.push(Statement::Assignment {
            comp: ComponentReference::from_flat_segments("lo", Span::DUMMY, None),
            value: binary(OpBinary::Sub, var("u"), real(1.0)),
            span: Span::DUMMY,
        });
        split.body.push(Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: binary(OpBinary::Gt, var("u"), real(0.0)),
                stmts: vec![Statement::Assignment {
                    comp: ComponentReference::from_flat_segments("hi", Span::DUMMY, None),
                    value: var("u"),
                    span: Span::DUMMY,
                }],
            }],
            else_block: Some(vec![Statement::Assignment {
                comp: ComponentReference::from_flat_segments("hi", Span::DUMMY, None),
                value: real(0.0),
                span: Span::DUMMY,
            }]),
            span: Span::DUMMY,
        });
        model.symbols.functions.insert(split.name.clone(), split);
    }

    fn production_c_lines(package: &AlgorithmCodePackage) -> String {
        let context = c_template_context(package, "Battery").expect("C context");
        ["startup", "recalibrate", "do_step"]
            .into_iter()
            .flat_map(|method| {
                context["methods"][method]
                    .as_array()
                    .expect("method statements are an array")
            })
            .flat_map(|statement| {
                statement["c_lines"]
                    .as_array()
                    .expect("statement c_lines are an array")
            })
            .map(|line| line.as_str().expect("C line is a string").to_owned())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

// ---------------------------------------------------------------------
// 4. Reserved names through rendering (GAL-015, T13, T2)
// ---------------------------------------------------------------------

mod reserved_names {
    use super::*;

    /// Variables named like GALEC keywords, builtins, Appendix C names, or
    /// `__`-prefixed identifiers render as quoted identifiers carrying the
    /// original name, stay injective, and pass the GALEC validator (which
    /// runs inside lowering post-validation, GAL-004).
    #[test]
    fn reserved_named_variables_render_quoted_and_stay_injective() {
        const CORPUS: &[&str] = &[
            "block",         // keyword
            "signal",        // keyword
            "absolute",      // builtin
            "ln",            // builtin
            "remainderDown", // Appendix C reserved
            "__shadow",      // reserved `__` prefix space
        ];
        let mut model = model_with_body(var("u"));
        for name in CORPUS {
            add_state(&mut model, name);
        }
        let package = lower(&model, &base_types());
        let alg = render_algorithm_code(&package).expect("renders");
        for name in CORPUS {
            let declaration = format!("Real '{name}';");
            assert!(
                alg.contains(&declaration),
                "missing `{declaration}`:\n{alg}"
            );
            let startup = format!("self.'{name}' := 0.0;");
            assert!(alg.contains(&startup), "missing `{startup}`:\n{alg}");
            // No bare (unquoted) reserved identifier is ever declared.
            assert!(
                !alg.contains(&format!("Real {name};")),
                "`{name}` leaked unquoted:\n{alg}"
            );
        }
        // Injectivity across the whole emitted corpus: manifest names and
        // ids are pairwise distinct.
        let names: Vec<&str> = package
            .manifest
            .variables
            .iter()
            .map(|variable| variable.common().name.as_str())
            .collect();
        let unique: HashSet<&str> = names.iter().copied().collect();
        assert_eq!(
            unique.len(),
            names.len(),
            "manifest name collision: {names:?}"
        );
    }

    /// The `'previous(x)'` quoted identifier round-trips as exact printed
    /// text: read in the update body, committed at the end of DoStep, and
    /// listed in the manifest as `state`.
    #[test]
    fn previous_state_round_trips_exact_printed_text() {
        let body = binary(OpBinary::Add, var("__pre__.y"), var("u"));
        let package = lower(&model_with_body(body), &base_types());
        let alg = render_algorithm_code(&package).expect("renders");
        assert!(
            alg.contains("self.y := self.'previous(y)' + self.u;"),
            "{alg}"
        );
        assert!(alg.contains("self.'previous(y)' := self.y;"), "{alg}");
        let previous = package
            .manifest
            .variables
            .iter()
            .find(|variable| variable.common().name.as_str() == "previous(y)")
            .expect("previous(y) listed");
        assert_eq!(previous.common().block_causality, BlockCausality::State);
    }
}

// ---------------------------------------------------------------------
// 4. Type-inference failure => diagnostic, never a default (S8)
// ---------------------------------------------------------------------

#[test]
fn missing_type_provenance_is_a_diagnostic_through_lowering() {
    // Without the ScalarTypeMap, parameter/constant types are honestly
    // unresolvable: ET011 per variable, never a guessed default type.
    let model = model_with_body(var("u"));
    let input = GalecInput::new(&model, "Battery");
    let errors = lower_to_algorithm_code(&input, &GalecOptions::default())
        .map(|_| ())
        .expect_err("must not default types");
    assert!(!errors.is_empty());
    assert!(
        errors.iter().all(|error| error.code() == "ET011"),
        "{errors:#?}"
    );
    for name in ["samplePeriod", "i1", "i2"] {
        assert!(
            errors.iter().any(|error| error.to_string().contains(name)),
            "no ET011 for `{name}`: {errors:#?}"
        );
    }
}

/// A pre reference arriving as a rendered element name (`__pre__.h[2]`)
/// resolves through its base slot with the index as a literal subscript —
/// the same fallback `lower_state_ref` and the condition machinery have
/// (GAL-026 array-native intent), never a misleading ET019.
#[test]
fn rendered_pre_element_reference_resolves_through_the_base_slot() {
    let mut model = model_with_body(var("__pre__.h[2]"));
    let mut h = variable("h");
    h.dims = vec![2];
    h.start = Some(real(0.0));
    model.variables.discrete_reals.insert(h.name.clone(), h);
    add_pre_slot(&mut model, "h", real(0.0), vec![2]);
    let package = lower(&model, &base_types());
    let alg = render_algorithm_code(&package).expect("renders");
    assert!(
        alg.contains("self.y := self.'previous(h)'[2];"),
        "rendered pre element must lower to an indexed previous-state read:\n{alg}"
    );
    assert!(
        alg.contains("self.'previous(h)' := self.h;"),
        "the read slot must be committed at the end of DoStep:\n{alg}"
    );
}

/// GAL-004 post-validation holds on every rendering path: the package
/// fields are public, so a package mutated after lowering must fail the
/// re-validation inside the rendering facades, never print invalid GALEC.
#[test]
fn mutated_package_cannot_render_unvalidated_galec() {
    let mut package = lower(&model_with_body(var("u")), &base_types());
    package
        .block
        .do_step
        .statements
        .push(gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::state(gast::Name::ident("undeclared")),
            value: gast::Expression::Real(1.0),
        }));
    let error = render_algorithm_code(&package).expect_err("facade must re-validate the block");
    assert_eq!(error.code(), "ET018");
}

/// The typed AC manifest is assembled from one identity + the SHA-1 of the
/// rendered `.alg` bytes: the minted UUID and the `.alg` checksum live in the
/// typed model (the templates own the XML — SPEC_0034 D3 amended), so
/// downstream consumers (the Production Code `ManifestReference`, the
/// `__content.xml` entry) read packaging facts from typed data — never
/// re-parsing XML or re-serializing the model (GAL-021).
#[test]
fn ac_manifest_carries_the_minted_uuid_and_alg_checksum() {
    let package = lower(&model_with_body(var("u")), &base_types());
    let alg = render_algorithm_code(&package).expect("renders");
    let alg_sha1 = Sha1Hex::of_bytes(alg.as_bytes());
    let identity = ManifestIdentity::generated().expect("identity mints");
    let manifest =
        assemble_manifest_with_identity(&package, alg_sha1.clone(), &identity).expect("assembles");
    assert_eq!(
        manifest.parts().attributes.id,
        identity.id,
        "manifest carries the minted identity UUID"
    );
    let checksum = manifest
        .parts()
        .files
        .iter()
        .find_map(|file| match &file.checksum {
            FileChecksum::Sha1(sha1) => Some(sha1),
            FileChecksum::NotNeeded => None,
        })
        .expect("the .alg File entry carries a checksum");
    assert_eq!(
        checksum.as_str(),
        alg_sha1.as_str(),
        "the .alg File/@checksum is the SHA-1 of the exact rendered .alg bytes"
    );
}

// ---------------------------------------------------------------------
// 5. Block interface (GAL-017, GAL-020)
// ---------------------------------------------------------------------

mod block_interface {
    use super::*;

    fn interface_package() -> AlgorithmCodePackage {
        // Body reads the pre slot so a `'previous(y)'` state is kept.
        let body = binary(OpBinary::Add, var("__pre__.y"), var("u"));
        lower(&model_with_body(body), &base_types())
    }

    /// GALEC block methods are parameter-free (trap T1): the rendered
    /// headers are exactly `method <Name>` with no parameter list.
    #[test]
    fn method_headers_are_parameter_free() {
        let alg = render_algorithm_code(&interface_package()).expect("renders");
        for name in ["Startup", "Recalibrate", "DoStep"] {
            let header = alg
                .lines()
                .find(|line| line.trim_start().starts_with(&format!("method {name}")))
                .unwrap_or_else(|| panic!("no `method {name}` header:\n{alg}"));
            assert_eq!(
                header.trim(),
                format!("method {name}"),
                "header must carry no parameters"
            );
        }
    }

    /// Startup target names of a package, in manifest spelling.
    fn startup_targets(package: &AlgorithmCodePackage) -> HashMap<String, gast::Expression> {
        package
            .block
            .startup
            .statements
            .iter()
            .map(|statement| match &statement.node {
                gast::Statement::Assignment { target, value } => {
                    let gast::Reference::State(parts) = target else {
                        panic!("Startup writes block state only, got {target:?}");
                    };
                    (manifest_name(&parts[0].name).to_owned(), value.clone())
                }
                other => panic!("Startup contains only assignments, got {other:?}"),
            })
            .collect()
    }

    /// GAL-017: every manifest variable is assigned in Startup — except
    /// control inputs, which are read-only inside a GALEC block (EG033).
    #[test]
    fn every_non_input_manifest_variable_is_assigned_in_startup() {
        let package = interface_package();
        let targets = startup_targets(&package);
        for manifest_variable in &package.manifest.variables {
            let common = manifest_variable.common();
            if common.block_causality == BlockCausality::Input {
                continue;
            }
            assert!(
                targets.contains_key(common.name.as_str()),
                "`{}` ({:?}) not assigned in Startup; targets: {:?}",
                common.name.as_str(),
                common.block_causality,
                targets.keys().collect::<Vec<_>>()
            );
        }
    }

    /// GAL-020: manifest `start` values mirror Startup's initial literal
    /// assignments numerically (this fixture has no dependent parameters,
    /// so every non-input assignment is a literal).
    #[test]
    fn manifest_starts_mirror_startup_literals_numerically() {
        let package = interface_package();
        let targets = startup_targets(&package);
        let mut compared = 0usize;
        for manifest_variable in &package.manifest.variables {
            let common = manifest_variable.common();
            if common.block_causality == BlockCausality::Input {
                continue;
            }
            let value = &targets[common.name.as_str()];
            match (manifest_variable, value) {
                (MVar::Real(real), gast::Expression::Real(assigned)) => {
                    assert_eq!(real.start, StartValue::Scalar(*assigned), "{}", common.name);
                }
                (MVar::Integer(integer), gast::Expression::Integer(assigned)) => {
                    let scalar = StartValue::Scalar(
                        i32::try_from(*assigned).expect("fixture literals fit i32"),
                    );
                    assert_eq!(integer.start, scalar, "{}", common.name);
                }
                (MVar::Boolean(boolean), gast::Expression::Bool(assigned)) => {
                    assert_eq!(
                        boolean.start,
                        StartValue::Scalar(*assigned),
                        "{}",
                        common.name
                    );
                }
                (_, other) => panic!(
                    "`{}`: Startup value is not a matching literal: {other:?}",
                    common.name
                ),
            }
            compared += 1;
        }
        assert!(
            compared >= 5,
            "expected >= 5 mirrored starts, got {compared}"
        );
    }

    /// GAL-017: Recalibrate is emitted in the `.alg` and listed in the
    /// manifest even when the model has no dependent parameters.
    #[test]
    fn recalibrate_emitted_even_without_dependent_parameters() {
        let package = interface_package();
        assert!(
            package.block.recalibrate.statements.is_empty(),
            "fixture must have no dependent parameters"
        );
        let alg = render_algorithm_code(&package).expect("renders");
        let recalibrate = alg
            .split("method Recalibrate")
            .nth(1)
            .expect("Recalibrate section")
            .split("end Recalibrate;")
            .next()
            .expect("Recalibrate end");
        assert_eq!(
            recalibrate
                .lines()
                .map(str::trim)
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>(),
            vec!["algorithm"],
            "empty Recalibrate must still be emitted:\n{alg}"
        );
        let ctx = ac_manifest_ctx(&package);
        assert!(
            ctx.to_string().contains("Recalibrate"),
            "empty Recalibrate must still appear in the manifest context:\n{ctx}"
        );
    }
}

/// Assemble the typed AC manifest for `package` (fresh identity + real `.alg`
/// SHA-1) and serialize the product-agnostic [`AcManifestCtx`] the manifest
/// template consumes — the typed serializers are gone (SPEC_0034 D3 amended),
/// so assertions target the serializable context.
fn ac_manifest_ctx(package: &AlgorithmCodePackage) -> serde_json::Value {
    let alg = render_algorithm_code(package).expect("alg renders");
    let identity = ManifestIdentity::generated().expect("identity mints");
    let manifest =
        assemble_manifest_with_identity(package, Sha1Hex::of_bytes(alg.as_bytes()), &identity)
            .expect("manifest assembles");
    serde_json::to_value(AcManifestCtx::from_manifest(&manifest)).expect("ctx serializes")
}
