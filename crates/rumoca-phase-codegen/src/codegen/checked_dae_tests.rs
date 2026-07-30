use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;

fn empty_checked_dae() -> dae::Dae {
    dae::Dae::construct(SourceMap::new(), |_| Ok(())).expect("empty checked DAE is valid")
}

#[test]
fn dae_template_context_is_a_semantic_projection_not_wire_storage() {
    let dae = empty_checked_dae();
    let json = dae_template_json(&dae).expect("checked DAE projects");

    assert_eq!(
        json.pointer("/schema/name")
            .and_then(serde_json::Value::as_str),
        Some("rumoca.checked-dae-template")
    );
    assert_eq!(
        json.pointer("/schema/version")
            .and_then(serde_json::Value::as_u64),
        Some(u64::from(dae_backend::TEMPLATE_SCHEMA_VERSION))
    );
    assert!(json.get("variables").is_some());
    assert!(json.get("expressions").is_some());
    assert!(json.get("systems").is_some());
    assert!(json.get("source_map").is_none());
    assert!(json.get("storage").is_none());
}

#[test]
fn dae_render_context_accepts_only_a_finalized_checked_root() {
    let dae = empty_checked_dae();
    let rendered = render_template(&dae, "{{ ir_kind }}:{{ dae.schema.version }}")
        .expect("checked DAE render succeeds");

    assert_eq!(
        rendered,
        format!("dae:{}", dae_backend::TEMPLATE_SCHEMA_VERSION)
    );
}

#[test]
fn checked_modelica_distinguishes_omitted_and_explicit_unit_range_steps() {
    let source = "parameter Integer a[3] = 1:3; parameter Integer b[3] = 4:1:6;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("ranges.mo", source);
    let at = |snippet: &str, occurrence: usize| {
        let start = source
            .match_indices(snippet)
            .nth(occurrence)
            .map(|(start, _)| start)
            .unwrap();
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + snippet.len()))
            .unwrap()
    };
    let dae = dae::Dae::construct(source_map, |model| {
        let integers = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Integer, [3]),
                at("Integer a[3]", 0),
            )
        })?;
        let omitted = model.expressions(|expressions| {
            let start = expressions
                .at(at("1", 0))
                .literal(dae::DaeLiteral::Integer(1))?;
            let stop = expressions
                .at(at("3", 1))
                .literal(dae::DaeLiteral::Integer(3))?;
            expressions.at(at("1:3", 0)).range(start, None, stop)
        })?;
        let explicit = model.expressions(|expressions| {
            let start = expressions
                .at(at("4", 0))
                .literal(dae::DaeLiteral::Integer(4))?;
            let step = expressions
                .at(at("1", 1))
                .literal(dae::DaeLiteral::Integer(1))?;
            let stop = expressions
                .at(at("6", 0))
                .literal(dae::DaeLiteral::Integer(6))?;
            expressions
                .at(at("4:1:6", 0))
                .range(start, Some(step), stop)
        })?;
        model.variables(|variables| {
            variables.parameter(
                VarName::new("a"),
                integers,
                at("parameter Integer a[3]", 0),
                dae::VariableAttributes {
                    binding: Some(omitted),
                    ..dae::VariableAttributes::default()
                },
            )?;
            variables.parameter(
                VarName::new("b"),
                integers,
                at("parameter Integer b[3]", 0),
                dae::VariableAttributes {
                    binding: Some(explicit),
                    ..dae::VariableAttributes::default()
                },
            )?;
            Ok(())
        })
    })
    .unwrap();

    let projected = dae_template_json(&dae).unwrap();
    let ranges = projected["expressions"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|expression| {
            (expression["operation"]["kind"] == "range").then_some(&expression["operation"])
        })
        .collect::<Vec<_>>();
    assert_eq!(ranges.len(), 2);
    assert_eq!(ranges[0]["start"]["value"], 1);
    assert!(ranges[0]["explicit_step"].is_null());
    assert_eq!(ranges[0]["stop"]["value"], 3);
    assert_eq!(ranges[1]["explicit_step"]["value"], 1);
    assert!(ranges[0].get("step").is_none());
    for bound in [&ranges[0]["start"], &ranges[0]["stop"]] {
        assert!(bound["expression"].is_u64());
        assert!(bound["provenance"]["span"].is_object());
    }

    let template =
        crate::templates::builtin_template_source("dae-modelica", "dae_modelica.mo.jinja").unwrap();
    let rendered = render_template_with_name(&dae, template, "Ranges").unwrap();
    assert!(rendered.contains("parameter Integer a[3] = 1:3;"));
    assert!(rendered.contains("parameter Integer b[3] = 4:1:6;"));
}

#[test]
fn dae_modelica_target_walks_the_checked_expression_arena() {
    let source = "model M parameter Real p = 2; Real x; equation der(x) = p; end M;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("M.mo", source);
    let at = |snippet: &str| {
        let start = source.find(snippet).expect("fixture snippet exists");
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + snippet.len()))
            .expect("fixture provenance is exact")
    };
    let equation_parameter = {
        let start = source.rfind("= p;").expect("equation parameter exists") + 2;
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + 1))
            .expect("equation parameter provenance is exact")
    };
    let dae = dae::Dae::construct(source_map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("Real p"),
            )
        })?;
        let two = dae.expressions(|expressions| {
            expressions.at(at("2")).literal(dae::DaeLiteral::Real(2.0))
        })?;
        let parameter = dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                real,
                at("parameter Real p = 2"),
                dae::VariableAttributes {
                    binding: Some(two),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                at("Real x"),
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = dae.expressions(|expressions| {
            let derivative = expressions
                .at(at("der(x)"))
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let parameter = expressions
                .at(equation_parameter)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            expressions.at(at("der(x) = p")).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                parameter,
            )
        })?;
        dae.continuous(|continuous| continuous.value_equation(at("der(x) = p"), residual))
    })
    .expect("checked fixture constructs");
    let template =
        crate::templates::builtin_template_source("dae-modelica", "dae_modelica.mo.jinja")
            .expect("checked DAE Modelica template exists");

    let rendered =
        render_template_with_name(&dae, template, "M").expect("checked DAE target renders");

    assert!(rendered.contains("parameter Real p = 2.0;"));
    assert!(rendered.contains("Real x;"));
    assert!(rendered.contains("0.0 = (der(x) - p);"));
    assert!(!rendered.contains("dae.x"));
    assert_eq!(
        dae_template_json(&dae).unwrap()["schema"]["name"],
        "rumoca.checked-dae-template"
    );
}

#[test]
fn dae_modelica_target_fails_closed_on_unowned_array_update() {
    let source = "parameter Integer a[2] = {1, 2};";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("array_update.mo", source);
    let at = |snippet: &str| {
        let start = source.find(snippet).expect("fixture snippet exists");
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + snippet.len()))
            .expect("fixture provenance is exact")
    };
    let dae = dae::Dae::construct(source_map, |dae| {
        let integers = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Integer, [2]),
                at("Integer a[2]"),
            )
        })?;
        let update = dae.expressions(|expressions| {
            let one = expressions
                .at(at("1"))
                .literal(dae::DaeLiteral::Integer(1))?;
            let two = expressions
                .at(at("2"))
                .literal(dae::DaeLiteral::Integer(2))?;
            let base = expressions.at(at("{1, 2}")).array([one, two])?;
            expressions.at(at("{1, 2}")).array_update(
                base,
                two,
                [dae::Subscript::Index {
                    expression: one,
                    provenance: at("1"),
                }],
            )
        })?;
        dae.variables(|variables| {
            variables.parameter(
                VarName::new("a"),
                integers,
                at("parameter Integer a[2]"),
                dae::VariableAttributes {
                    binding: Some(update),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        Ok(())
    })
    .expect("checked DAE accepts the typed array update");
    let template =
        crate::templates::builtin_template_source("dae-modelica", "dae_modelica.mo.jinja").unwrap();

    let error = render_template_with_name(&dae, template, "M")
        .expect_err("the Modelica target must reject an unowned array update")
        .to_string();

    assert!(error.contains("unsupported-feature:dae-modelica-expression:array_update"));
}

#[test]
fn dae_modelica_target_renders_checked_function_values_and_statements() {
    let source = "function f input Real u; output Real y; protected Real z; algorithm z := u + 1.0; y := z; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("f.mo", source);
    let at = |snippet: &str, occurrence: usize| {
        let start = source
            .match_indices(snippet)
            .nth(occurrence)
            .map(|(start, _)| start)
            .unwrap();
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + snippet.len()))
            .unwrap()
    };
    let dae = dae::Dae::construct(source_map, |dae| {
        let real = dae.types(|types| {
            types.derived(
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("function f", 0),
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [real], [real], at("function f", 0));
        dae.function(signature, |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at("input Real u", 0))
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, at("output Real y", 0))
            })?;
            let local = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("z"), real, at("Real z", 0))
            })?;
            let parameter = dae.expressions(|expressions| {
                expressions.at(at("u", 1)).function_parameter(parameter)
            })?;
            let one = dae.expressions(|expressions| {
                expressions
                    .at(at("1.0", 0))
                    .literal(dae::DaeLiteral::Real(1.0))
            })?;
            let local_definition = dae.expressions(|expressions| {
                expressions
                    .at(at("u + 1.0", 0))
                    .binary(dae::BinaryOperator::Add, parameter, one)
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, at("function f", 0)))?;
            dae.functions(|functions| {
                functions.assign(&mut body, local, local_definition, at("z := u + 1.0", 0))
            })?;
            let local_use = dae.functions(|functions| functions.read(&body, local, at("z", 2)))?;
            dae.functions(|functions| {
                functions.assign(&mut body, output, local_use, at("y := z", 0))
            })?;
            dae.functions(|functions| functions.define(body, at("function f", 0)))
        })?;
        Ok(())
    })
    .unwrap();
    let template =
        crate::templates::builtin_template_source("dae-modelica", "dae_modelica.mo.jinja").unwrap();
    let rendered = render_template_with_name(&dae, template, "M").unwrap();

    assert!(rendered.contains("function f"));
    assert!(rendered.contains("input Real u;"));
    assert!(rendered.contains("output Real y;"));
    assert!(rendered.contains("Real z;"));
    assert!(rendered.contains("z := (u + 1.0);"));
    assert!(rendered.contains("y := z;"));
}

#[test]
fn dae_template_preserves_distinct_definitions_with_one_rhs() {
    let source =
        "function f output Real y; protected Real z; algorithm z := 1; z := 1; y := z; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("same_rhs.mo", source);
    let at = |snippet: &str, occurrence: usize| {
        let start = source
            .match_indices(snippet)
            .nth(occurrence)
            .map(|(start, _)| start)
            .expect("fixture snippet occurrence exists");
        dae::DaeProvenance::source(Span::from_offsets(source_id, start, start + snippet.len()))
            .expect("fixture provenance is exact")
    };
    let dae = dae::Dae::construct(source_map, |dae| {
        let real = dae.types(|types| {
            types.derived(
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("function f", 0),
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [], [real], at("function f", 0));
        dae.function(signature, |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, at("output Real y", 0))
            })?;
            let local = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("z"), real, at("Real z", 0))
            })?;
            let rhs = dae.expressions(|expressions| {
                expressions
                    .at(at("1", 0))
                    .literal(dae::DaeLiteral::Real(1.0))
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, at("function f", 0)))?;
            dae.functions(|functions| {
                functions.assign(&mut body, local, rhs, at("z := 1", 0))?;
                functions.assign(&mut body, local, rhs, at("z := 1", 1))
            })?;
            let local_use = dae.functions(|functions| functions.read(&body, local, at("z", 3)))?;
            dae.functions(|functions| {
                functions.assign(&mut body, output, local_use, at("y := z", 0))?;
                functions.define(body, at("function f", 0))
            })
        })
        .map(|_| ())
    })
    .expect("same-RHS checked function constructs");

    let projected = dae_template_json(&dae).expect("same-RHS DAE projects");
    let function = &projected["functions"][0];
    let definitions = function["definitions"]
        .as_array()
        .expect("canonical definition table is an array");
    assert_eq!(definitions.len(), 3);
    assert_eq!(definitions[0]["ordinal"], 0);
    assert_eq!(definitions[1]["ordinal"], 1);
    assert_eq!(definitions[0]["target"], definitions[1]["target"]);
    assert_eq!(definitions[0]["rhs"], definitions[1]["rhs"]);
    assert_ne!(definitions[0]["ordinal"], definitions[1]["ordinal"]);
    assert_ne!(definitions[0]["provenance"], definitions[1]["provenance"]);
    assert_eq!(function["statements"][0]["definition"], 0);
    assert_eq!(function["statements"][1]["definition"], 1);
    assert!(function["statements"][0].get("target").is_none());
    assert!(function["statements"][0].get("value").is_none());
    assert_eq!(function["results"][0], 2);

    let function_use = projected["expressions"]
        .as_array()
        .expect("expression projection is an array")
        .iter()
        .find(|expression| expression["operation"]["kind"] == "function_value")
        .expect("output RHS retains its function-value occurrence");
    assert_eq!(function_use["operation"]["definition"], 1);
}

const FOLD_SOURCE: &str =
    "function f output Real x; algorithm x := 0; for k in 1:2 loop x := x + k; end for; end f;";

#[derive(Clone, Copy)]
struct FoldSource {
    id: rumoca_core::SourceId,
}

impl FoldSource {
    fn attach() -> (Self, SourceMap) {
        let mut source_map = SourceMap::new();
        let id = source_map.add("fold.mo", FOLD_SOURCE);
        (Self { id }, source_map)
    }

    fn at(self, snippet: &str, occurrence: usize) -> dae::DaeProvenance {
        let start = FOLD_SOURCE
            .match_indices(snippet)
            .nth(occurrence)
            .map(|(start, _)| start)
            .expect("fixture snippet occurrence exists");
        dae::DaeProvenance::source(Span::from_offsets(self.id, start, start + snippet.len()))
            .expect("fixture provenance is exact")
    }

    fn generated_fold(self) -> dae::DaeProvenance {
        dae::DaeProvenance::generated(
            dae::DaeGeneration::FunctionLoopLowering,
            self.at("for k in 1:2 loop", 0).span(),
        )
        .expect("generated fold provenance has an exact owner")
    }
}

fn checked_fold_fixture() -> (dae::Dae, FoldSource) {
    let (source, source_map) = FoldSource::attach();
    let at = |snippet: &str, occurrence: usize| source.at(snippet, occurrence);
    let dae = dae::Dae::construct(source_map, |dae| {
        let real = dae.types(|types| {
            types.derived(
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("function f", 0),
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [], [real], at("function f", 0));
        dae.function(signature, |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("x"), 0, at("output Real x", 0))
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, at("function f", 0)))?;
            let zero = dae.expressions(|expressions| {
                expressions
                    .at(at("0", 0))
                    .literal(dae::DaeLiteral::Real(0.0))
            })?;
            dae.functions(|functions| functions.assign(&mut body, output, zero, at("x := 0", 0)))?;
            let domain = dae.domains(|domains| {
                domains.structured(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: 0,
                            display_name: "k".to_owned(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    at("for k in 1:2 loop", 0),
                )
            })?;
            let binder = dae.domains(|domains| domains.binder(domain, 0, at("k", 1)))?;
            let mut loop_body = dae.functions(|functions| {
                functions.begin_loop(body, domain, [output], at("for k in 1:2 loop", 0))
            })?;
            let current =
                dae.functions(|functions| functions.read(loop_body.body(), output, at("x", 3)))?;
            let index = dae.expressions(|expressions| expressions.at(at("k", 1)).binder(binder))?;
            let update = dae.expressions(|expressions| {
                expressions
                    .at(at("x + k", 0))
                    .binary(dae::BinaryOperator::Add, current, index)
            })?;
            dae.functions(|functions| {
                functions.assign_loop(&mut loop_body, output, update, at("x := x + k", 0))
            })?;
            let body = dae.functions(|functions| {
                functions.finish_loop(loop_body, at("for k in 1:2 loop", 0))
            })?;
            dae.functions(|functions| functions.define(body, at("function f", 0)))
        })
        .map(|_| ())
    })
    .expect("checked fold fixture constructs");
    (dae, source)
}

fn assert_fold_definition_links(function: &serde_json::Value, source: FoldSource) {
    let generated = source.generated_fold();
    let at = |snippet: &str, occurrence: usize| source.at(snippet, occurrence);

    assert_eq!(function["folds"][0]["ordinal"], 0);
    assert_eq!(function["folds"][0]["targets"], serde_json::json!([0]));
    assert_eq!(
        function["folds"][0]["parameter_definitions"],
        serde_json::json!([1])
    );
    assert_eq!(
        function["folds"][0]["initial_definitions"],
        serde_json::json!([0])
    );
    assert_eq!(
        function["folds"][0]["update_definitions"],
        serde_json::json!([2])
    );
    assert_eq!(
        function["folds"][0]["output_definitions"],
        serde_json::json!([3])
    );
    assert_eq!(function["statements"][1]["kind"], "for");
    assert_eq!(function["statements"][1]["fold"], 0);
    assert_eq!(function["statements"][1]["statements"][0]["definition"], 2);
    assert_eq!(function["results"], serde_json::json!([3]));

    let definitions = function["definitions"]
        .as_array()
        .expect("definition projection is an array");
    let expected = [
        (0, 0, at("x := 0", 0)),
        (0, 1, generated),
        (0, 4, at("x := x + k", 0)),
        (0, 5, generated),
    ];
    for (definition, (target, rhs, provenance)) in definitions.iter().zip(expected) {
        assert_eq!(definition["target"], target);
        assert_eq!(definition["rhs"], rhs);
        assert_eq!(
            definition["provenance"],
            serde_json::to_value(provenance).unwrap()
        );
    }
    assert_eq!(
        function["folds"][0]["provenance"],
        serde_json::to_value(at("for k in 1:2 loop", 0)).unwrap()
    );
}

fn assert_fold_expression_links(projected: &serde_json::Value, generated: dae::DaeProvenance) {
    let expressions = projected["expressions"]
        .as_array()
        .expect("expression projection is an array");
    assert_eq!(
        expressions[1]["operation"],
        serde_json::json!({
            "kind": "function_fold_parameter",
            "function": 0,
            "fold": 0,
            "carried": 0,
            "definition": 1,
        })
    );
    assert_eq!(
        expressions[5]["operation"],
        serde_json::json!({
            "kind": "function_fold_output",
            "function": 0,
            "fold": 0,
            "carried": 0,
            "definition": 3,
        })
    );
    assert_eq!(
        expressions[1]["provenance"],
        serde_json::to_value(generated).unwrap()
    );
    assert_eq!(
        expressions[5]["provenance"],
        serde_json::to_value(generated).unwrap()
    );
}

#[test]
fn dae_template_projects_function_folds_by_definition_identity() {
    let (dae, source) = checked_fold_fixture();
    let projected = dae_template_json(&dae).expect("checked fold projects");
    let function = &projected["functions"][0];

    assert_fold_definition_links(function, source);
    assert_fold_expression_links(&projected, source.generated_fold());
}

#[test]
fn symbolic_solve_targets_use_checked_declarations_and_solve_programs() {
    let dae = empty_checked_dae();
    let problem = solve::SolveProblem::default();
    let artifacts = solve::SolveArtifacts::default();

    for (target, template_name, marker) in [
        (
            "casadi-solve",
            "casadi_solve.py.jinja",
            "import casadi as ca",
        ),
        ("jax-solve", "jax_solve.py.jinja", "import jax"),
    ] {
        let template = crate::templates::builtin_template_source(target, template_name)
            .expect("checked Solve template exists");
        let rendered = SolveTemplateRenderer::new_with_dae(&problem, &artifacts, dae.clone())
            .expect("checked Solve context constructs")
            .render(template)
            .expect("checked Solve target renders");

        assert!(rendered.contains(marker));
        assert!(rendered.contains("PARAM_NAMES = []"));
        assert!(!template.contains("dae.p"));
    }
}
