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
        let (_function, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], at("function f", 0))
        })?;
        let parameter = dae.functions(|functions| {
            functions.parameter(&reservation, VarName::new("u"), 0, at("input Real u", 0))
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, at("output Real y", 0))
        })?;
        let local = dae.functions(|functions| {
            functions.local(&reservation, VarName::new("z"), real, at("Real z", 0))
        })?;
        let parameter = dae
            .expressions(|expressions| expressions.at(at("u", 1)).function_parameter(parameter))?;
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
        dae.functions(|functions| functions.assign(&mut body, output, local_use, at("y := z", 0)))?;
        dae.functions(|functions| functions.define(body, at("function f", 0)))?;
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
