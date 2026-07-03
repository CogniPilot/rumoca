//! Front-half projection tests over hand-built minimal DAE values.
//!
//! Hand-building `Dae` values is the established repo precedent for
//! DAE-consumer unit tests (e.g. `rumoca-eval-dae` scalar-eval tests build
//! `Dae::default()` and populate partitions directly); no front-end crates
//! are pulled in, which also keeps this crate's dev-dependencies free of
//! anything that becomes circular once `rumoca-compile` depends on it.

use std::collections::HashMap;

use rumoca_core::{Expression, Literal, OpBinary, OpUnary, Reference, Span, VarName};
use rumoca_ir_galec::ast::{Name, ScalarType};
use rumoca_ir_dae as dae;
use rumoca_galec_codegen::input::ScalarTypeMap;
use rumoca_galec_codegen::{
    GalecInput, GalecTargetError, VariableClass, build_manifest_variables, check_admissibility,
    classify_variables,
};

// ---------------------------------------------------------------------
// Shared builders
// ---------------------------------------------------------------------

fn variable(name: &str) -> dae::Variable {
    let mut variable = dae::Variable::empty_with_span(Span::DUMMY);
    variable.name = VarName::new(name);
    variable
}

fn base_dae() -> dae::Dae {
    let mut model = dae::Dae::default();
    model.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 1e-3,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });
    model
}

fn equation(lhs: &str) -> dae::Equation {
    dae::Equation {
        lhs: Some(Reference::new(lhs)),
        rhs: real(0.0),
        span: Span::DUMMY,
        origin: "test".to_owned(),
        scalar_count: 1,
    }
}

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

fn neg(expr: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
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

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn codes(errors: &[GalecTargetError]) -> Vec<&'static str> {
    errors.iter().map(GalecTargetError::code).collect()
}

// ---------------------------------------------------------------------
// Admissibility (GAL-004, GAL-025)
// ---------------------------------------------------------------------

mod admissibility {
    use super::*;

    #[test]
    fn discrete_model_with_one_fixed_clock_is_admissible() {
        let model = base_dae();
        let clock = check_admissibility(&GalecInput::new(&model, "M")).expect("admissible");
        assert_eq!(clock.period_seconds, 1e-3);
        assert_eq!(clock.phase_seconds, 0.0);
    }

    #[test]
    fn continuous_state_rejected_with_projection_scope_wording() {
        let mut model = base_dae();
        model
            .variables
            .states
            .insert(VarName::new("x"), variable("x"));
        model.continuous.equations.push(equation("x"));
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET001"]);
        let message = errors[0].to_string();
        assert!(
            message.contains("not yet supported by the Rumoca GALEC projection"),
            "GAL-025 wording missing: {message}"
        );
        assert!(
            !message.contains("eFMI"),
            "must not blame eFMI for a Rumoca scope limit: {message}"
        );
    }

    #[test]
    fn external_function_rejected_with_projection_scope_wording() {
        let mut model = base_dae();
        let mut function = rumoca_core::Function::new("tableLookup", Span::DUMMY);
        function.external = Some(rumoca_core::ExternalFunction {
            language: "C".to_owned(),
            ..rumoca_core::ExternalFunction::default()
        });
        model
            .symbols
            .functions
            .insert(VarName::new("tableLookup"), function);
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET002"]);
        assert!(
            errors[0]
                .to_string()
                .contains("not yet supported by the Rumoca GALEC projection")
        );
    }

    #[test]
    fn runtime_events_rejected_with_projection_scope_wording() {
        let mut model = base_dae();
        model.events.scheduled_time_events.push(0.5);
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET003"]);
        assert!(
            errors[0]
                .to_string()
                .contains("not yet supported by the Rumoca GALEC projection")
        );
    }

    #[test]
    fn dynamic_clock_rejected() {
        let mut model = base_dae();
        model.clocks.triggered_conditions.push(boolean(true));
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET004"]);
    }

    #[test]
    fn clock_count_must_be_exactly_one() {
        let mut two = base_dae();
        two.clocks.schedules.push(dae::ClockSchedule {
            period_seconds: 2e-3,
            phase_seconds: 0.0,
            source_span: Span::DUMMY,
        });
        let errors = check_admissibility(&GalecInput::new(&two, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET005"]);

        let mut none = base_dae();
        none.clocks.schedules.clear();
        let errors = check_admissibility(&GalecInput::new(&none, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET005"]);
    }

    #[test]
    fn non_positive_or_non_finite_period_rejected() {
        for bad in [0.0, -1e-3, f64::NAN, f64::INFINITY] {
            let mut model = base_dae();
            model.clocks.schedules[0].period_seconds = bad;
            let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
            assert_eq!(codes(&errors), vec!["ET006"], "period {bad}");
        }
    }

    #[test]
    fn partial_and_non_model_class_types_rejected() {
        let mut partial = base_dae();
        partial.metadata.is_partial = true;
        let errors = check_admissibility(&GalecInput::new(&partial, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET007"]);

        let mut package = base_dae();
        package.metadata.class_type = rumoca_core::ClassType::Package;
        let errors = check_admissibility(&GalecInput::new(&package, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET008"]);
    }

    #[test]
    fn structurally_parametric_dimension_rejected() {
        let mut model = base_dae();
        let mut array = variable("table");
        array.dims = vec![2, 0];
        model
            .variables
            .discrete_reals
            .insert(VarName::new("table"), array);
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET009"]);
    }

    #[test]
    fn clocked_relations_stay_admissible() {
        // Limiter comparisons appear in relations/f_c even in fully discrete
        // models; they evaluate at ticks only and must not be rejected.
        let mut model = base_dae();
        model
            .conditions
            .relations
            .push(binary(OpBinary::Gt, var_ref("pidY"), var_ref("uMax")));
        model.conditions.equations.push(equation("c[1]"));
        assert!(check_admissibility(&GalecInput::new(&model, "M")).is_ok());
    }

    #[test]
    fn all_failures_are_collected() {
        let mut model = base_dae();
        model.metadata.is_partial = true;
        model.clocks.schedules.clear();
        model.events.scheduled_time_events.push(1.0);
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        let codes = codes(&errors);
        assert!(codes.contains(&"ET007"), "{codes:?}");
        assert!(codes.contains(&"ET003"), "{codes:?}");
        assert!(codes.contains(&"ET005"), "{codes:?}");
    }

    /// Startup is built from `start` values only; a model with initial
    /// equations must be rejected up front (ET021, GAL-025 wording), never
    /// projected with its initialization partition silently ignored.
    #[test]
    fn initial_equations_rejected_never_silently_ignored() {
        let mut model = base_dae();
        model.initialization.equations.push(equation("n"));
        let errors = check_admissibility(&GalecInput::new(&model, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET021"]);
        assert!(
            errors[0]
                .to_string()
                .contains("not yet supported by the Rumoca GALEC projection"),
            "{errors:#?}"
        );

        // Structured families alone (a broken scalar-view invariant) still
        // reject rather than slip through.
        let mut structured_only = base_dae();
        structured_only
            .initialization
            .structured_equations
            .push(dae::StructuredEquationFamily {
                domain: rumoca_core::StructuredIndexDomain {
                    binders: Vec::new(),
                },
                first_equation_index: 0,
                equation_counts: vec![1],
                span: Span::DUMMY,
                origin: "test".to_owned(),
                regular: None,
                template: None,
                interiors_materialized: true,
            });
        let errors = check_admissibility(&GalecInput::new(&structured_only, "M")).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET021"]);
    }
}

// ---------------------------------------------------------------------
// Classification (GAL-020)
// ---------------------------------------------------------------------

mod classification {
    use super::*;

    /// A miniature of the real compiler output shape for the PID fixture:
    /// discrete output in `z` keeping causality, tunable + dependent +
    /// structural parameters in `p`, generated `__pre__.` slots, a constant,
    /// a Boolean discrete state in `m`, and the generated condition vector.
    fn pid_like_dae() -> dae::Dae {
        let mut model = base_dae();

        let mut input_var = variable("wLoadRef");
        input_var.causality = dae::VariableCausality::Input;
        model
            .variables
            .inputs
            .insert(input_var.name.clone(), input_var);

        let mut output_in_z = variable("vMotor");
        output_in_z.causality = dae::VariableCausality::Output;
        output_in_z.min = Some(neg(real(1e7)));
        output_in_z.max = Some(real(1e7));
        model
            .variables
            .discrete_reals
            .insert(output_in_z.name.clone(), output_in_z);

        let mut tunable = variable("limiterUMax");
        tunable.causality = dae::VariableCausality::Parameter;
        tunable.is_tunable = true;
        tunable.start = Some(real(400.0));
        model
            .variables
            .parameters
            .insert(tunable.name.clone(), tunable);

        let mut dependent = variable("limiterUMin");
        dependent.causality = dae::VariableCausality::CalculatedParameter;
        dependent.start = Some(neg(var_ref("limiterUMax")));
        model
            .variables
            .parameters
            .insert(dependent.name.clone(), dependent);

        let mut structural = variable("n");
        structural.causality = dae::VariableCausality::Parameter;
        structural.is_tunable = false;
        structural.start = Some(integer(3));
        model
            .variables
            .parameters
            .insert(structural.name.clone(), structural);

        let mut constant = variable("samplePeriod");
        constant.unit = Some("s".to_owned());
        constant.start = Some(real(1e-3));
        model
            .variables
            .constants
            .insert(constant.name.clone(), constant);

        let mut discrete_state = variable("pidIx");
        discrete_state.start = Some(real(0.0));
        model
            .variables
            .discrete_reals
            .insert(discrete_state.name.clone(), discrete_state);

        let mut first_tick = variable("firstTick");
        first_tick.start = Some(boolean(true));
        model
            .variables
            .discrete_valued
            .insert(first_tick.name.clone(), first_tick);

        let mut pre_pid_ix = variable("__pre__.pidIx");
        pre_pid_ix.causality = dae::VariableCausality::CalculatedParameter;
        pre_pid_ix.origin = dae::VariableOrigin::Generated;
        pre_pid_ix.fixed = Some(true);
        model
            .variables
            .parameters
            .insert(pre_pid_ix.name.clone(), pre_pid_ix);

        // Generated when-edge machinery: condition vector + its pre slot.
        let mut condition = variable("c");
        condition.dims = vec![4];
        model
            .variables
            .discrete_valued
            .insert(condition.name.clone(), condition);
        let mut pre_condition = variable("__pre__.c");
        pre_condition.dims = vec![4];
        pre_condition.causality = dae::VariableCausality::CalculatedParameter;
        pre_condition.origin = dae::VariableOrigin::Generated;
        model
            .variables
            .parameters
            .insert(pre_condition.name.clone(), pre_condition);
        for index in 1..=4 {
            model
                .conditions
                .equations
                .push(equation(&format!("c[{index}]")));
        }

        model
    }

    fn pid_types() -> ScalarTypeMap {
        let mut map = HashMap::new();
        map.insert(VarName::new("limiterUMax"), ScalarType::Real);
        map.insert(VarName::new("limiterUMin"), ScalarType::Real);
        map.insert(VarName::new("n"), ScalarType::Integer);
        map.insert(VarName::new("samplePeriod"), ScalarType::Real);
        map.insert(VarName::new("firstTick"), ScalarType::Boolean);
        map
    }

    #[test]
    fn classification_follows_causality_not_partition() {
        let model = pid_like_dae();
        let types = pid_types();
        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");

        let by_name = |name: &str| classification.find(name).expect(name);
        assert_eq!(by_name("wLoadRef").class, VariableClass::Input);
        // Output-in-z: partition is z, causality wins.
        assert_eq!(by_name("vMotor").class, VariableClass::Output);
        assert_eq!(
            by_name("limiterUMax").class,
            VariableClass::TunableParameter
        );
        assert_eq!(
            by_name("limiterUMin").class,
            VariableClass::DependentParameter
        );
        assert_eq!(by_name("n").class, VariableClass::Constant);
        assert_eq!(by_name("samplePeriod").class, VariableClass::Constant);
        assert_eq!(by_name("pidIx").class, VariableClass::State);
        assert_eq!(by_name("firstTick").class, VariableClass::State);
    }

    #[test]
    fn scalar_types_resolve_structurally_and_from_provenance() {
        let model = pid_like_dae();
        let types = pid_types();
        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");

        let by_name = |name: &str| classification.find(name).expect(name);
        assert_eq!(by_name("wLoadRef").scalar_type, ScalarType::Real);
        assert_eq!(by_name("vMotor").scalar_type, ScalarType::Real);
        assert_eq!(by_name("firstTick").scalar_type, ScalarType::Boolean);
        assert_eq!(by_name("n").scalar_type, ScalarType::Integer);
        // Pre slots inherit the base variable's type.
        assert_eq!(by_name("__pre__.pidIx").scalar_type, ScalarType::Real);
        // Condition machinery is Boolean by MLS B.1d structure.
        assert_eq!(by_name("c").scalar_type, ScalarType::Boolean);
        assert_eq!(by_name("__pre__.c").scalar_type, ScalarType::Boolean);
    }

    #[test]
    fn pre_slots_become_previous_state() {
        let model = pid_like_dae();
        let types = pid_types();
        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");

        let pre = classification.find("__pre__.pidIx").expect("pre slot");
        assert_eq!(pre.class, VariableClass::State);
        assert_eq!(pre.galec_name, Name::quoted("previous(pidIx)"));
        assert_eq!(pre.pre_base.as_deref(), Some("pidIx"));
        assert!(!pre.projection_internal);
    }

    #[test]
    fn condition_vector_and_its_pre_slot_are_projection_internal() {
        let model = pid_like_dae();
        let types = pid_types();
        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");

        assert!(classification.find("c").expect("c").projection_internal);
        assert!(
            classification
                .find("__pre__.c")
                .expect("pre c")
                .projection_internal
        );
        assert!(!classification.find("pidIx").expect("z").projection_internal);
    }

    #[test]
    fn type_inference_failure_is_a_diagnostic_not_a_default() {
        // Without type provenance, parameter/constant/m types are honestly
        // unresolvable — never guessed from start literals (S8).
        let model = pid_like_dae();
        let input = GalecInput::new(&model, "M");
        let errors = classify_variables(&input).unwrap_err();
        assert!(!errors.is_empty());
        assert!(
            codes(&errors).iter().all(|code| *code == "ET011"),
            "{errors:?}"
        );
        let unresolved: Vec<String> = errors
            .iter()
            .map(|error| error.to_string())
            .filter(|message| message.contains("limiterUMax"))
            .collect();
        assert_eq!(unresolved.len(), 1);
    }

    #[test]
    fn unclassifiable_evidence_fails_early() {
        let mut model = base_dae();
        // A local-causality variable in the algebraic partition matches no
        // classification row.
        model
            .variables
            .algebraics
            .insert(VarName::new("y"), variable("y"));
        let input = GalecInput::new(&model, "M");
        let errors = classify_variables(&input).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET010"]);
    }
}

// ---------------------------------------------------------------------
// Mangling (GAL-015, traps T2/T13)
// ---------------------------------------------------------------------

mod mangling {
    use super::*;
    use rumoca_ir_galec::builtins::is_reserved_name;
    use rumoca_galec_codegen::mangle::{galec_variable_name, manifest_name, pre_state_name};

    /// Name corpus mixing legal identifiers, keywords, builtins, Appendix C
    /// reserved names, predefined signals, `__` prefixes, and hierarchical
    /// scalarized names.
    const CORPUS: &[&str] = &[
        "x",
        "x_1",
        "firstTick",
        "vMotor",
        "if",
        "then",
        "for",
        "self",
        "size",
        "while",
        "absolute",
        "min",
        "max",
        "sin",
        "sin1D",
        "integer",
        "remainderDown",
        "divisionEuclidean",
        "abs",
        "OVERFLOW",
        "NAN",
        "__x",
        "__pre__x",
        "a.b",
        "a.b[2]",
        "a.b[2].c",
        "gear.ratio",
        "previous",
        "previousFeedback",
    ];

    #[test]
    fn mangling_is_injective_over_the_corpus() {
        let mut seen = std::collections::HashSet::new();
        for name in CORPUS {
            let mangled = galec_variable_name(name).expect(name);
            assert!(
                seen.insert(mangled.clone()),
                "collision for {name}: {mangled:?}"
            );
        }
        // The pre-state space stays disjoint from the plain mapping space.
        for name in CORPUS {
            let pre = pre_state_name(name).expect(name);
            assert!(
                seen.insert(pre.clone()),
                "pre collision for {name}: {pre:?}"
            );
        }
    }

    #[test]
    fn plain_identifiers_are_never_reserved() {
        for name in CORPUS {
            if let Name::Ident(ident) = galec_variable_name(name).expect(name) {
                assert!(
                    !is_reserved_name(ident.as_str()),
                    "mangling emitted reserved plain identifier `{}`",
                    ident.as_str()
                );
            }
        }
    }

    #[test]
    fn reserved_and_illegal_names_take_the_quoted_route() {
        for reserved in [
            "if",
            "absolute",
            "min",
            "sin1D",
            "remainderDown",
            "OVERFLOW",
            "__x",
        ] {
            let mangled = galec_variable_name(reserved).expect(reserved);
            assert_eq!(mangled, Name::quoted(reserved), "{reserved}");
        }
        assert_eq!(
            galec_variable_name("a.b[2]").expect("hierarchical"),
            Name::quoted("a.b[2]")
        );
        assert_eq!(galec_variable_name("x").expect("legal"), Name::ident("x"));
    }

    #[test]
    fn quoted_route_round_trips_original_names() {
        for name in ["a.b[2].c", "if", "__x", "gear.ratio"] {
            let mangled = galec_variable_name(name).expect(name);
            assert_eq!(manifest_name(&mangled), name);
        }
        let pre = pre_state_name("feedback.y").expect("pre");
        assert_eq!(manifest_name(&pre), "previous(feedback.y)");
        assert_eq!(pre, Name::quoted("previous(feedback.y)"));
    }

    #[test]
    fn unrepresentable_names_are_rejected() {
        for bad in ["Logic.'1'", "has space", "", "previous(x)"] {
            let error = galec_variable_name(bad).unwrap_err();
            assert_eq!(error.code(), "ET012", "{bad}");
        }
        let error = pre_state_name("a(b)").unwrap_err();
        assert_eq!(error.code(), "ET012");
    }
}

// ---------------------------------------------------------------------
// Constant evaluation + manifest variable population (GAL-020)
// ---------------------------------------------------------------------

mod manifest_variables {
    use super::*;
    use rumoca_efmi::algorithm_code_manifest::{BlockCausality, StartValue, Variable as MVar};
    use rumoca_galec_codegen::manifest_vars::const_eval::{ConstEnv, ConstValue, EvalFailure};

    #[test]
    fn const_evaluator_folds_the_supported_subset() {
        let env = ConstEnv::default();
        let eval = |expr: &Expression| env.evaluate_scalar(expr);

        assert_eq!(eval(&neg(real(1e5))), Ok(ConstValue::Real(-1e5)));
        assert_eq!(
            eval(&binary(OpBinary::Add, integer(2), integer(3))),
            Ok(ConstValue::Integer(5))
        );
        assert_eq!(
            eval(&binary(OpBinary::Add, integer(2), real(3.0))),
            Ok(ConstValue::Real(5.0))
        );
        assert_eq!(
            eval(&binary(OpBinary::Div, integer(1), integer(2))),
            Ok(ConstValue::Real(0.5))
        );
        assert_eq!(
            eval(&Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(boolean(true)),
                span: Span::DUMMY,
            }),
            Ok(ConstValue::Boolean(false))
        );
    }

    #[test]
    fn const_evaluator_rejects_the_unsupported() {
        let env = ConstEnv::default();
        let string_literal = Expression::Literal {
            value: Literal::String("s".to_owned()),
            span: Span::DUMMY,
        };
        assert!(matches!(
            env.evaluate_scalar(&string_literal),
            Err(EvalFailure::NotEvaluable { .. })
        ));
        assert!(matches!(
            env.evaluate_scalar(&var_ref("unknown")),
            Err(EvalFailure::NotEvaluable { .. })
        ));
        let call = Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![],
            is_constructor: false,
            span: Span::DUMMY,
        };
        assert!(matches!(
            env.evaluate_scalar(&call),
            Err(EvalFailure::NotEvaluable { .. })
        ));
    }

    /// Classification over a small parameter set for evaluator/manifest
    /// tests; `extra` lets each test add variables.
    fn classified_model(extra: impl FnOnce(&mut dae::Dae)) -> (dae::Dae, ScalarTypeMap) {
        let mut model = base_dae();
        let mut tunable = variable("limiterUMax");
        tunable.causality = dae::VariableCausality::Parameter;
        tunable.is_tunable = true;
        tunable.start = Some(real(400.0));
        model
            .variables
            .parameters
            .insert(tunable.name.clone(), tunable);
        let mut dependent = variable("limiterUMin");
        dependent.causality = dae::VariableCausality::CalculatedParameter;
        dependent.start = Some(neg(var_ref("limiterUMax")));
        model
            .variables
            .parameters
            .insert(dependent.name.clone(), dependent);
        extra(&mut model);
        let mut types = HashMap::new();
        types.insert(VarName::new("limiterUMax"), ScalarType::Real);
        types.insert(VarName::new("limiterUMin"), ScalarType::Real);
        (model, types)
    }

    #[test]
    fn parameter_defaults_resolve_through_references() {
        let (model, types) = classified_model(|_| {});
        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let manifest = build_manifest_variables(&classification).expect("builds");

        assert_eq!(manifest.variables.len(), 2);
        let MVar::Real(min) = &manifest.variables[1] else {
            panic!("expected Real variable");
        };
        assert_eq!(min.common.name.as_str(), "limiterUMin");
        assert_eq!(
            min.common.block_causality,
            BlockCausality::DependentParameter
        );
        assert_eq!(min.start, StartValue::Scalar(-400.0));
        // Ids are positional and recorded per DAE name.
        assert_eq!(
            manifest
                .ids_by_dae_name
                .get("limiterUMax")
                .map(String::as_str),
            Some("V1")
        );
        assert_eq!(
            manifest
                .ids_by_dae_name
                .get("limiterUMin")
                .map(String::as_str),
            Some("V2")
        );
    }

    #[test]
    fn start_dependency_cycles_are_diagnosed() {
        let (mut model, mut types) = classified_model(|_| {});
        let mut a = variable("a");
        a.causality = dae::VariableCausality::CalculatedParameter;
        a.start = Some(var_ref("b"));
        let mut b = variable("b");
        b.causality = dae::VariableCausality::CalculatedParameter;
        b.start = Some(var_ref("a"));
        model.variables.parameters.insert(a.name.clone(), a);
        model.variables.parameters.insert(b.name.clone(), b);
        types.insert(VarName::new("a"), ScalarType::Real);
        types.insert(VarName::new("b"), ScalarType::Real);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let errors = build_manifest_variables(&classification).unwrap_err();
        assert!(codes(&errors).contains(&"ET015"), "{errors:?}");
    }

    #[test]
    fn attribute_values_coerce_to_the_declared_type_only() {
        // Boolean variable with a Real start: value/type mismatch (the type
        // comes from the DAE side, never from the literal).
        let (mut model, mut types) = classified_model(|_| {});
        let mut flag = variable("flag");
        flag.start = Some(real(1.0));
        model
            .variables
            .discrete_valued
            .insert(flag.name.clone(), flag);
        types.insert(VarName::new("flag"), ScalarType::Boolean);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let errors = build_manifest_variables(&classification).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET014"]);
    }

    #[test]
    fn boolean_state_and_missing_start_defaults() {
        let (mut model, mut types) = classified_model(|model| {
            // Missing start takes the MLS default for the declared type.
            model
                .variables
                .discrete_reals
                .insert(VarName::new("pidIx"), variable("pidIx"));
        });
        let mut flag = variable("firstTick");
        flag.start = Some(boolean(true));
        model
            .variables
            .discrete_valued
            .insert(flag.name.clone(), flag);
        types.insert(VarName::new("firstTick"), ScalarType::Boolean);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let manifest = build_manifest_variables(&classification).expect("builds");

        let find = |name: &str| {
            manifest
                .variables
                .iter()
                .find(|variable| variable.common().name.as_str() == name)
                .expect(name)
        };
        let MVar::Boolean(first_tick) = find("firstTick") else {
            panic!("expected Boolean variable");
        };
        assert_eq!(first_tick.start, StartValue::Scalar(true));
        let MVar::Real(pid_ix) = find("pidIx") else {
            panic!("expected Real variable");
        };
        assert_eq!(pid_ix.start, StartValue::Scalar(0.0));
    }

    #[test]
    fn array_starts_flatten_row_major_and_scalars_broadcast() {
        let (mut model, types) = classified_model(|model| {
            let mut matrix = variable("gains");
            matrix.dims = vec![2, 2];
            matrix.start = Some(Expression::Array {
                elements: vec![
                    Expression::Array {
                        elements: vec![real(1.0), real(2.0)],
                        is_matrix: false,
                        span: Span::DUMMY,
                    },
                    Expression::Array {
                        elements: vec![real(3.0), real(4.0)],
                        is_matrix: false,
                        span: Span::DUMMY,
                    },
                ],
                is_matrix: true,
                span: Span::DUMMY,
            });
            model
                .variables
                .discrete_reals
                .insert(matrix.name.clone(), matrix);
        });
        let mut broadcast = variable("offsets");
        broadcast.dims = vec![3];
        broadcast.start = Some(real(0.5));
        model
            .variables
            .discrete_reals
            .insert(broadcast.name.clone(), broadcast);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let manifest = build_manifest_variables(&classification).expect("builds");
        let find = |name: &str| {
            manifest
                .variables
                .iter()
                .find(|variable| variable.common().name.as_str() == name)
                .expect(name)
        };
        let MVar::Real(gains) = find("gains") else {
            panic!("expected Real variable");
        };
        assert_eq!(gains.common.dimensions, vec![2, 2]);
        assert_eq!(gains.start, StartValue::Array(vec![1.0, 2.0, 3.0, 4.0]));
        let MVar::Real(offsets) = find("offsets") else {
            panic!("expected Real variable");
        };
        assert_eq!(offsets.start, StartValue::Scalar(0.5));
    }

    #[test]
    fn min_max_expression_trees_evaluate() {
        let (mut model, types) = classified_model(|_| {});
        let mut bounded = variable("vMotor");
        bounded.causality = dae::VariableCausality::Output;
        bounded.min = Some(neg(real(1e7)));
        bounded.max = Some(real(1e7));
        model
            .variables
            .discrete_reals
            .insert(bounded.name.clone(), bounded);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let manifest = build_manifest_variables(&classification).expect("builds");
        let MVar::Real(v_motor) = manifest
            .variables
            .iter()
            .find(|variable| variable.common().name.as_str() == "vMotor")
            .expect("vMotor")
        else {
            panic!("expected Real variable");
        };
        assert_eq!(v_motor.min, Some(-1e7));
        assert_eq!(v_motor.max, Some(1e7));
        assert_eq!(v_motor.common.block_causality, BlockCausality::Output);
    }

    #[test]
    fn unevaluable_start_is_a_diagnostic() {
        let (mut model, mut types) = classified_model(|_| {});
        let mut bad = variable("k");
        bad.causality = dae::VariableCausality::Parameter;
        bad.is_tunable = true;
        bad.start = Some(Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![],
            is_constructor: false,
            span: Span::DUMMY,
        });
        model.variables.parameters.insert(bad.name.clone(), bad);
        types.insert(VarName::new("k"), ScalarType::Real);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let errors = build_manifest_variables(&classification).unwrap_err();
        assert_eq!(codes(&errors), vec!["ET013"]);
    }

    #[test]
    fn projection_internal_variables_are_not_listed() {
        let (mut model, mut types) = classified_model(|_| {});
        let mut condition = variable("c");
        condition.dims = vec![1];
        model
            .variables
            .discrete_valued
            .insert(condition.name.clone(), condition);
        model.conditions.equations.push(equation("c[1]"));
        types.insert(VarName::new("c"), ScalarType::Boolean);

        let input = GalecInput::new(&model, "M").with_scalar_types(&types);
        let classification = classify_variables(&input).expect("classifies");
        let manifest = build_manifest_variables(&classification).expect("builds");
        assert!(
            manifest
                .variables
                .iter()
                .all(|variable| variable.common().name.as_str() != "c")
        );
        assert!(!manifest.ids_by_dae_name.contains_key("c"));
    }
}
