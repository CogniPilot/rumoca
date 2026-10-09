use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("typed_extremum.mo"), 0, 1)
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    )
}

fn binary_table(operator: solve::SolveBinaryOperator) -> solve::SolvePureCallTable {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(real)],
            span(),
            |builder, _, outputs| {
                let a = builder.constant(solve::SolveValue::real(profile(), 1.0), span())?;
                let b = builder.constant(solve::SolveValue::real(profile(), 2.0), span())?;
                let result = builder.binary(operator, a, b, span())?;
                builder.store(outputs[0], result, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn renderer(table: solve::SolvePureCallTable) -> crate::codegen::SolveTemplateRenderer {
    use crate::codegen::fmi_projection_tests::{model_with_one_state_run, state_input};
    let mut model = model_with_one_state_run(false);
    let site = table.owners()[0].call_site();
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::PureCall {
                        dst_start: 0,
                        input_starts: Box::new([]),
                        site,
                    },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 0.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            span().require_provenance("typed extrema test").unwrap(),
        )
        .unwrap(),
    );
    model.pure_calls = table;
    let component = solve::fmi::FmiComponent::construct(model, vec![state_input()]).unwrap();
    crate::codegen::SolveTemplateRenderer::new_owned_with_fmi(
        component.into_codegen_view().try_event_free().unwrap(),
    )
    .unwrap()
}

#[test]
fn typed_only_real_binary_extrema_declare_helpers_in_the_public_fmi_header() {
    use crate::codegen::codegen_test_support::builtin_template;
    for operator in [
        solve::SolveBinaryOperator::Min,
        solve::SolveBinaryOperator::Max,
    ] {
        let rendered = renderer(binary_table(operator))
            .render_with_name_and_artifact(
                builtin_template("fmi3", "model.h.jinja"),
                "TypedExtremum",
                &serde_json::json!({"identities": {"fmu": "typed-extremum", "instance": "typed-extremum"}}),
            )
            .unwrap();
        assert!(
            rendered.contains("static inline double rumoca_real_min("),
            "{rendered}"
        );
        assert!(
            rendered.contains("static inline double rumoca_real_max("),
            "{rendered}"
        );
    }
}

#[derive(Clone, Copy, Debug)]
enum Form {
    Binary(solve::SolveBinaryOperator),
    Broadcast(solve::SolveBinaryOperator),
    Reduce(solve::SolveReductionOperator),
}

fn operation_table(scalar: solve::SolveScalarType, form: Form) -> solve::SolvePureCallTable {
    let p = match scalar {
        solve::SolveScalarType::Real { format } => {
            solve::SolveArithmeticProfile::construct(format, profile().integer_domain())
        }
        _ => profile(),
    };
    let output = if matches!(form, Form::Broadcast(_)) {
        solve::SolveValueType::tensor(scalar, vec![2]).unwrap()
    } else {
        solve::SolveValueType::scalar(scalar)
    };
    solve::SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(output)],
            span(),
            |b, _, outputs| {
                let value = match scalar {
                    solve::SolveScalarType::Real { .. } => solve::SolveValue::real(p, 2.0),
                    solve::SolveScalarType::Integer(_) => solve::SolveValue::integer(p, 2).unwrap(),
                    solve::SolveScalarType::Boolean => solve::SolveValue::boolean(true),
                };
                let x = b.constant(value, span())?;
                let result = match form {
                    Form::Binary(op) => b.binary(op, x, x, span())?,
                    Form::Broadcast(op) => {
                        let tensor = b.construct_aggregate(&[x, x], vec![2], span())?;
                        b.broadcast_binary(op, tensor, x, false, span())?
                    }
                    Form::Reduce(op) => {
                        let tensor = b.construct_aggregate(&[x, x], vec![2], span())?;
                        b.reduce(op, tensor, span())?
                    }
                };
                b.store(outputs[0], result, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn detected(table: &solve::SolvePureCallTable) -> bool {
    super::super::used_by(
        &solve::SolveProblem::default(),
        &solve::SolveArtifacts::default(),
        Some(table),
    )
}

#[test]
fn typed_extrema_detection_uses_checked_destination_element_types() {
    use solve::{SolveBinaryOperator as B, SolveReductionOperator as R, SolveScalarType as S};
    let extrema = [
        Form::Binary(B::Min),
        Form::Binary(B::Max),
        Form::Broadcast(B::Min),
        Form::Broadcast(B::Max),
        Form::Reduce(R::Minimum),
        Form::Reduce(R::Maximum),
    ];
    for scalar in [
        S::real(profile()),
        S::Real {
            format: solve::SolveRealFormat::Binary32,
        },
        S::integer(profile()),
        S::Boolean,
    ] {
        for form in extrema {
            assert_eq!(
                detected(&operation_table(scalar, form)),
                matches!(scalar, S::Real { .. }),
                "{scalar:?} {form:?}"
            );
        }
    }
    for form in [
        Form::Binary(B::Add),
        Form::Broadcast(B::Multiply),
        Form::Reduce(R::Sum),
        Form::Reduce(R::Product),
    ] {
        assert!(
            !detected(&operation_table(S::real(profile()), form)),
            "{form:?}"
        );
    }
}

fn render_builtin(renderer: &crate::codegen::SolveTemplateRenderer, name: &str) -> String {
    renderer
        .render_with_name_and_artifact(
            crate::codegen::codegen_test_support::builtin_template("fmi3", name),
            "TypedExtremum",
            &serde_json::json!({"identities": {"fmu": "typed-extremum"}}),
        )
        .unwrap()
}

#[test]
fn public_fmi_header_and_typed_extremum_functions_compile_and_execute() {
    for (operator, expected) in [
        (solve::SolveBinaryOperator::Min, 1.0),
        (solve::SolveBinaryOperator::Max, 2.0),
    ] {
        let renderer = renderer(binary_table(operator));
        let dir = tempfile::tempdir().unwrap();
        let headers = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src/templates/fmi-ls-wasm/fmi3-headers");
        for header in [
            "fmi3Functions.h",
            "fmi3FunctionTypes.h",
            "fmi3PlatformTypes.h",
        ] {
            std::fs::copy(headers.join(header), dir.path().join(header)).unwrap();
        }
        std::fs::write(
            dir.path().join("model.h"),
            render_builtin(&renderer, "model.h.jinja"),
        )
        .unwrap();
        let mut source = render_builtin(&renderer, "rmc_functions.c.jinja");
        source.push_str(&format!(
            "\nint main(void) {{ double result=0.0; return rumoca_scalar_pure_0(NULL, &result, NULL) || result != {expected}; }}\n"
        ));
        let path = dir.path().join("functions.c");
        std::fs::write(&path, source).unwrap();
        let binary = dir.path().join("control");
        let compile = std::process::Command::new("cc")
            .args([
                "-std=c11",
                "-O2",
                "-ffp-contract=off",
                "-Wall",
                "-Wextra",
                "-Werror",
            ])
            .arg(&path)
            .args(["-lm", "-o"])
            .arg(&binary)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        assert!(
            std::process::Command::new(binary)
                .status()
                .unwrap()
                .success()
        );
    }
}

mod regions;

#[test]
fn public_fmi_header_omits_real_helpers_for_typed_boolean_and_integer_extrema() {
    for scalar in [
        solve::SolveScalarType::Boolean,
        solve::SolveScalarType::integer(profile()),
    ] {
        for form in [
            Form::Binary(solve::SolveBinaryOperator::Min),
            Form::Binary(solve::SolveBinaryOperator::Max),
            Form::Broadcast(solve::SolveBinaryOperator::Min),
            Form::Reduce(solve::SolveReductionOperator::Maximum),
        ] {
            let header = render_builtin(&renderer(operation_table(scalar, form)), "model.h.jinja");
            assert!(!header.contains("rumoca_real_min"), "{scalar:?} {form:?}");
            assert!(!header.contains("rumoca_real_max"), "{scalar:?} {form:?}");
        }
    }
}
