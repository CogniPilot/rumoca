//! SPEC_0034 Phase-3 non-negotiable test battery, exercised end-to-end
//! through the public projection API (`lower_to_algorithm_code`,
//! `render_algorithm_code`, `render_manifest_document`,
//! `render_manifest_xml`):
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
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Reference, Span, Subscript, VarName,
};
use rumoca_efmi::Sha1Hex;
use rumoca_efmi::algorithm_code_manifest::{BlockCausality, StartValue, Variable as MVar};
use rumoca_galec::ast::{self as gast, ScalarType};
use rumoca_galec::builtins::{find_builtin, is_appendix_c_reserved};
use rumoca_ir_dae as dae;
use rumoca_target_galec::input::ScalarTypeMap;
use rumoca_target_galec::lower::emittable_builtin_targets;
use rumoca_target_galec::mangle::manifest_name;
use rumoca_target_galec::{
    AlgorithmCodePackage, GalecInput, GalecOptions, GalecTargetError, lower_to_algorithm_code,
    render_algorithm_code, render_manifest_document, render_manifest_xml,
};

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
    Expression::FunctionCall {
        name: Reference::generated(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
        args: vec![real(0.0), var("samplePeriod")],
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
    let edge = binary(OpBinary::And, indexed("c", 1), not(indexed("__pre__.c", 1)));
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

    /// Two sample-tick conditions (one schedule) = multi-rate: rejected
    /// with a stable feature id, not silently wired to one clock.
    #[test]
    fn multi_rate_sample_conditions_rejected() {
        let mut model = model_with_body(var("u"));
        if let Some(condition) = model.variables.discrete_valued.get_mut(&VarName::new("c")) {
            condition.dims = vec![2];
        }
        model.conditions.equations.push(dae::Equation {
            lhs: Some(Reference::new("c[2]")),
            rhs: sample_call(),
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
// 3. Reserved names through rendering (GAL-015, T13, T2)
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
        .push(gast::Statement::Assignment {
            target: gast::Reference::state(gast::Name::ident("undeclared")),
            value: gast::Expression::Real(1.0),
        });
    let error = render_algorithm_code(&package).expect_err("facade must re-validate the block");
    assert_eq!(error.code(), "ET018");
    let error =
        render_manifest_xml(&package).expect_err("manifest facade must re-validate the block");
    assert_eq!(error.code(), "ET018");
}

/// [`render_manifest_document`] returns a coherent pair from one
/// assemble-serialize pass: the XML embeds the typed manifest's freshly
/// minted UUID and the SHA-1 of the rendered `.alg` bytes, so downstream
/// consumers (the Production Code `ManifestReference`) can checksum the
/// returned string while reading packaging facts from typed data — never
/// re-parsing the XML or re-serializing the model (GAL-021).
#[test]
fn manifest_document_pair_comes_from_one_serialization_pass() {
    let package = lower(&model_with_body(var("u")), &base_types());
    let (manifest, xml) = render_manifest_document(&package).expect("manifest renders");
    let uuid = manifest.parts().attributes.id.to_string();
    assert!(
        xml.contains(&uuid),
        "XML must embed the typed manifest's UUID `{uuid}`:\n{xml}"
    );
    let alg = render_algorithm_code(&package).expect("renders");
    let alg_sha1 = Sha1Hex::of_bytes(alg.as_bytes());
    assert!(
        xml.contains(alg_sha1.as_str()),
        "XML must embed the `.alg` checksum `{alg_sha1}` (checksum over the \
         exact rendered `.alg` bytes):\n{xml}"
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
            .map(|statement| match statement {
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
        let xml = render_manifest_xml(&package).expect("manifest renders");
        assert!(xml.contains("Recalibrate"), "{xml}");
    }
}
