//! Retained compilation artifacts, without JavaScript serialization round trips.

use super::*;
use crate::compiled_model_api::compile_with_options;

const SOURCE: &str =
    "model Decay Real x(start=1,fixed=true); Real a; equation der(x)=-x; a=x*x; end Decay;";

#[test]
fn compiled_model_exposes_only_requested_representations() {
    let _guard = session_test_guard();
    for (options, has_dae, has_solve) in [
        ("{}", true, false),
        (r#"{"representations":["dae"]}"#, true, false),
        (r#"{"representations":["solve"]}"#, false, true),
        (r#"{"representations":["dae","solve"]}"#, true, true),
    ] {
        let model = compile_with_options(SOURCE, "Decay", options).unwrap();
        assert_eq!(model.representation_json("dae").is_ok(), has_dae);
        assert_eq!(model.representation_json("solve").is_ok(), has_solve);
        assert!(model.solve_error().is_none());
        let analysis: Value = serde_json::from_str(&model.analysis_json().unwrap()).unwrap();
        assert_eq!(analysis["balance"]["is_balanced"], true);
    }
}

#[test]
fn compiled_model_renders_both_views_without_lowering_again() {
    let _guard = session_test_guard();
    let model =
        compile_with_options(SOURCE, "Decay", r#"{"representations":["dae","solve"]}"#).unwrap();
    let solve_before = model.representation_json("solve").unwrap();
    for ir in ["dae", "solve", "solve"] {
        let manifest = format!(
            r#"version = 1
ir = "{ir}"
[capabilities]
continuous_states = true
residual_equations = true
initialization = true
scalar_fallback = true
[[files]]
path = "model.js"
template = "model.jinja"
"#
        );
        let template = if ir == "dae" {
            "{{ dae.variables|length }}"
        } else {
            "{{ solve_model.initial_y|tojson }}"
        };
        let files = model
            .render_files(
                &manifest,
                &BTreeMap::from([("model.jinja".to_owned(), template.to_owned())]),
            )
            .unwrap();
        assert_eq!(
            files[0].content,
            if ir == "dae" { "2" } else { "[1.0,0.0]" }
        );
        assert_eq!(model.representation_json("solve").unwrap(), solve_before);
    }
}

#[test]
fn compiled_model_admits_function_parameter_initialization_like_source_rendering() {
    let _guard = session_test_guard();
    let source = r#"
function Square
  input Real value;
  output Real result;
algorithm
  result := value * value;
end Square;
model FunctionParameterInitialization
  parameter Real base = 2;
  parameter Real gain = Square(base);
  Real x(start=1, fixed=true);
equation
  der(x) = -gain*x;
end FunctionParameterInitialization;
"#;
    let name = "FunctionParameterInitialization";
    let mut session = Session::default();
    session.update_document("fixture.mo", source);
    let compiled = compile_requested_model(&mut session, name).unwrap();
    let source_files =
        crate::target_render_api::render_target_files("c-ode", "", "{}", &compiled.dae, None, name)
            .expect("source renderer admits checked function initialization");
    let model =
        compile_with_options(source, name, r#"{"representations":["dae","solve"]}"#).unwrap();
    let rendered = model
        .render_target("c-ode", "", "{}")
        .expect("retained renderer admits the same checked function initialization");
    let rendered: Value = serde_json::from_str(&rendered).unwrap();
    assert_eq!(
        rendered["files"].as_array().unwrap().len(),
        source_files.len()
    );
}

#[test]
fn compiled_model_rejects_unknown_and_empty_selections() {
    let _guard = session_test_guard();
    for options in [
        r#"{"representations":["unknown"]}"#,
        r#"{"representations":[]}"#,
    ] {
        assert!(compile_with_options(SOURCE, "Decay", options).is_err());
    }
    let model = compile_with_options(SOURCE, "Decay", "{}").unwrap();
    assert!(model.representation_json("solve").is_err());
    assert!(model.representation_json("unknown").is_err());
}

#[test]
fn compiled_model_retains_dae_after_solve_refusal() {
    let _guard = session_test_guard();
    let source =
        "model Nonlinear Real x(start=1,fixed=true); equation der(x)*der(x)=x; end Nonlinear;";
    let model = compile_with_options(
        source,
        "Nonlinear",
        r#"{"representations":["dae","solve"]}"#,
    )
    .unwrap();
    assert!(model.representation_json("dae").is_ok());
    assert!(model.solve_error().is_some());
    assert!(model.representation_json("solve").is_err());
}

#[test]
fn compiled_model_exposes_guarded_event_targets_and_lifecycle_inventory() {
    let _guard = session_test_guard();
    let source = "model Bounce Real h(start=1,fixed=true); Real v(start=0,fixed=true); equation der(h)=v; der(v)=-9.81; when h<=0 then reinit(v,-0.9*pre(v)); end when; end Bounce;";
    let model =
        compile_with_options(source, "Bounce", r#"{"representations":["dae","solve"]}"#).unwrap();
    let manifest = r#"version = 1
ir = "solve"
[capabilities]
continuous_states = true
residual_equations = true
initialization = true
scalar_fallback = true
events = true
[[files]]
path = "inventory.json"
template = "inventory.jinja"
"#;
    let template = "{{ solve.discrete.guarded_assignment_targets|tojson }}\n{{ solve.events.delays.value_parameter_indices|length }}\n{{ solve.artifacts.initialization.residual_jacobian_v.scalar_plan.stored_output_count }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("inventory.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    let lines: Vec<_> = files[0].content.lines().collect();
    let targets: Value = serde_json::from_str(lines[0]).unwrap();
    assert_eq!(targets[0][0]["base"]["Y"]["index"], 1);
    assert_eq!(targets[0][0]["count"], 1);
    assert_eq!(&lines[1..], &["0", "0"]);
}

#[test]
fn compiled_model_exposes_directional_function_owners_and_full_jacobian_plans() {
    let _guard = session_test_guard();
    let source = "function Square input Real x; output Real y; algorithm y:=x*x; end Square; model Squared Real x(start=1,fixed=true); equation der(x)=Square(x); end Squared;";
    let model =
        compile_with_options(source, "Squared", r#"{"representations":["dae","solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\n[[files]]\npath = \"derivatives.json\"\ntemplate = \"derivatives.jinja\"\n";
    let template = "{{ solve_model.pure_call_directional_owners|length }}|{{ solve.artifacts.continuous.full_jacobian_plan.stored_output_count }}|{% for program in solve.artifacts.continuous.full_jacobian_plan.programs %}{% for op in program.ops %}{{ op.kind }};{% endfor %}{% endfor %}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("derivatives.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    assert!(files[0].content.starts_with("1|1|"), "{}", files[0].content);
    assert!(files[0].content.contains("PureCallDirectional;"));
    assert!(files[0].content.contains("LoadSeed;"));
}

#[test]
fn compiled_model_exposes_checked_visible_programs_for_private_discrete_values() {
    let _guard = session_test_guard();
    let source = "model Observed Real x(start=0,fixed=true); discrete Integer count(start=0,fixed=true); equation der(x)=1; when time>=0.35 then count=pre(count)+1; end when; end Observed;";
    let model =
        compile_with_options(source, "Observed", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\n[[files]]\npath = \"observations.json\"\ntemplate = \"observations.jinja\"\n";
    let template = "{{ solve_model.visible_names|tojson }}|{{ solve_model.visible_value_plan.stored_output_count }}|{% for program in solve_model.visible_value_plan.programs %}{% for op in program.ops %}{{ op.kind }};{% endfor %}{% endfor %}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("observations.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    assert!(
        files[0].content.starts_with("[\"x\",\"count\"]|2|"),
        "{}",
        files[0].content
    );
    assert!(files[0].content.contains("LoadY;"));
    assert!(files[0].content.contains("LoadP;"));
}

#[test]
fn compiled_model_exposes_clock_owners_and_exact_render_schedule() {
    let _guard = session_test_guard();
    let source = "model Clocked Real x(start=0,fixed=true); discrete Integer count(start=0,fixed=true); equation der(x)=1; when sample(0,0.1) then count=pre(count)+1; end when; end Clocked;";
    let model =
        compile_with_options(source, "Clocked", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\nclocks = true\n[[files]]\npath = \"clocks.json\"\ntemplate = \"clocks.jinja\"\n";
    let template = "{{ solve.clocks.render_schedules|tojson }}\n{{ solve.discrete.guarded_assignment_clock_owners|tojson }}\n{{ solve.discrete.guarded_assignment_plan.programs[0].output_indices|tojson }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("clocks.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    let lines: Vec<_> = files[0].content.lines().collect();
    let schedule: Value = serde_json::from_str(lines[0]).unwrap();
    assert_eq!(schedule[0]["period_num"], "1");
    assert_eq!(schedule[0]["period_den"], "10");
    assert_eq!(schedule[0]["phase_num"], "0");
    assert_eq!(schedule[0]["anchor"], "absolute");
    assert_eq!(lines[1], "[0]");
    assert_eq!(lines[2], "[0]");
}

#[test]
fn compiled_model_exposes_exact_clock_history_schedules() {
    let _guard = session_test_guard();
    let source = "model Previous Clock c=Clock(1,10); Integer count(start=0,fixed=true); equation when c then count=previous(count)+1; end when; end Previous;";
    let model =
        compile_with_options(source, "Previous", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\nclocks = true\n[[files]]\npath = \"history.json\"\ntemplate = \"history.jinja\"\n";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([(
                "history.jinja".to_owned(),
                "{{ solve.clocks.pre_binding_schedules|tojson }}".to_owned(),
            )]),
        )
        .unwrap();
    let bindings: Value = serde_json::from_str(&files[0].content).unwrap();
    let clock = bindings
        .as_array()
        .unwrap()
        .iter()
        .find(|binding| !binding.is_null())
        .unwrap();
    assert_eq!(clock["period_num"], "1");
    assert_eq!(clock["period_den"], "10");
    assert_eq!(clock["anchor"], "absolute");
}

#[test]
fn compiled_model_exposes_checked_clock_intermediates() {
    let _guard = session_test_guard();
    let source = "model Alias Real x(start=0,fixed=true); discrete Integer fast(start=0,fixed=true); Integer alias; Integer slow(start=0,fixed=true); equation der(x)=1; alias=fast+10; when sample(0,.1) then fast=pre(fast)+1; end when; when sample(0,.2) then slow=alias; end when; end Alias;";
    let model = compile_with_options(source, "Alias", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\nclocks = true\n[[files]]\npath = \"intermediates.json\"\ntemplate = \"intermediates.jinja\"\n";
    let template = "{{ solve.discrete.clock_partition_intermediate_plan.stored_output_count }}\n{{ solve.discrete.clock_partition_intermediate_targets|tojson }}\n{{ solve.discrete.clock_partition_intermediate_clocks|tojson }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("intermediates.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    let lines: Vec<_> = files[0].content.lines().collect();
    let count = lines[0].parse::<usize>().unwrap();
    assert!(count > 0);
    let targets: Value = serde_json::from_str(lines[1]).unwrap();
    let clocks: Value = serde_json::from_str(lines[2]).unwrap();
    assert_eq!(targets.as_array().unwrap().len(), count);
    assert_eq!(clocks.as_array().unwrap().len(), count);
    assert!(
        clocks
            .as_array()
            .unwrap()
            .iter()
            .all(|owners| !owners.as_array().unwrap().is_empty())
    );
}

#[test]
fn compiled_model_exposes_checked_structured_discrete_assignments() {
    let _guard = session_test_guard();
    let source = "model Structured Real x(start=0,fixed=true); Clock c=Clock(1,10); Integer a(start=0,fixed=true); Integer b[3]; equation der(x)=1; when c then a=previous(a)+1; end when; for i in 1:3 loop b[i]=a+i; end for; end Structured;";
    let model =
        compile_with_options(source, "Structured", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\nclocks = true\n[[files]]\npath = \"structured.json\"\ntemplate = \"structured.jinja\"\n";
    let template = "{{ solve.discrete.structured_rhs.scalar_plan.stored_output_count }}\n{{ solve.discrete.structured_assignments|tojson }}\n{{ solve.discrete.structured_clock_owners|tojson }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("structured.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    let lines: Vec<_> = files[0].content.lines().collect();
    assert_eq!(lines[0], "3");
    let targets: Value = serde_json::from_str(lines[1]).unwrap();
    assert_eq!(targets.as_array().unwrap().len(), 1);
    assert_eq!(targets[0].as_array().unwrap().len(), 3);
    assert_eq!(lines[2], "[0]");
}

#[test]
fn compiled_model_exposes_checked_relation_neighborhoods() {
    let _guard = session_test_guard();
    let source = "model Relations Real x(start=0,fixed=true); Real a; Boolean p,q; equation der(x)=1; a=x*x; p=a>=0.15; q=a>=0.2; end Relations;";
    let model =
        compile_with_options(source, "Relations", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\n[[files]]\npath = \"relations.json\"\ntemplate = \"relations.jinja\"\n";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([(
                "relations.jinja".to_owned(),
                "{{ solve.events.root_neighborhoods|tojson }}".to_owned(),
            )]),
        )
        .unwrap();
    let neighborhoods: Value = serde_json::from_str(&files[0].content).unwrap();
    assert_eq!(neighborhoods, serde_json::json!([[0, 1], [0, 1]]));
}

#[test]
fn compiled_model_exposes_checked_delay_programs() {
    let _guard = session_test_guard();
    let source =
        "model Delayed Real x(start=1,fixed=true); equation der(x)=-delay(x,0.1,0.2); end Delayed;";
    let model =
        compile_with_options(source, "Delayed", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\nevents = true\n[[files]]\npath = \"delay.json\"\ntemplate = \"delay.jinja\"\n";
    let template = "{{ solve.events.delays.source_plan.stored_output_count }}|{{ solve.events.delays.delay_time_plan.stored_output_count }}|{{ solve.events.delays.delay_max_plan.stored_output_count }}|{{ solve.events.delays.source_is_discrete|tojson }}|{{ solve.events.delays.value_parameter_indices|length }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("delay.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    assert_eq!(files[0].content, "1|1|1|[false]|1");
}

#[test]
fn compiled_model_exposes_checked_manifold_derivatives() {
    let _guard = session_test_guard();
    let source = r#"model Quaternion Real Q[4](start={1,0,0,0}); parameter Real w1=0.1,w2=0.2,w3=0.3; equation der(Q[1])=0.5*(-Q[2]*w1-Q[3]*w2-Q[4]*w3); der(Q[2])=0.5*(Q[1]*w1+Q[3]*w3-Q[4]*w2); der(Q[3])=0.5*(Q[1]*w2-Q[2]*w3+Q[4]*w1); Q*Q=1; end Quaternion;"#;
    let model =
        compile_with_options(source, "Quaternion", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\n[[files]]\npath = \"manifold.json\"\ntemplate = \"manifold.jinja\"\n";
    let template = "{{ solve.continuous.manifold_residual.scalar_plan.stored_output_count }}|{{ solve.artifacts.continuous.manifold_jacobian_v.scalar_plan.stored_output_count }}|{{ solve.continuous.manifold_projection_plan.blocks|length }}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("manifold.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    assert_eq!(files[0].content, "1|1|1");
}

#[test]
fn compiled_model_exposes_native_torn_algebraic_programs() {
    let _guard = session_test_guard();
    let source = "model Torn Real x(start=1,fixed=true); Real a(start=1), b(start=1), c; equation der(x)=-x; a*a+b=x+1; b=a; c=2*a; end Torn;";
    let model = compile_with_options(source, "Torn", r#"{"representations":["solve"]}"#).unwrap();
    let manifest = "version = 1\nir = \"solve\"\n[capabilities]\ncontinuous_states = true\nresidual_equations = true\ninitialization = true\nscalar_fallback = true\n[[files]]\npath = \"tearing.txt\"\ntemplate = \"tearing.jinja\"\n";
    let template = "{% for block in solve.continuous.algebraic_tearing %}{% if block is not none %}{{ block.tear_y_indices|length }}|{{ block.assignment_targets|length }}|{{ block.assignment_plan.stored_output_count }}|{{ block.residual_plan.stored_output_count }}{% endif %}{% endfor %}";
    let files = model
        .render_files(
            manifest,
            &BTreeMap::from([("tearing.jinja".to_owned(), template.to_owned())]),
        )
        .unwrap();
    assert!(files[0].content.contains("1|1|1|1"), "{}", files[0].content);
}

#[test]
fn compiled_model_supports_function_loop_returns() {
    let _guard = session_test_guard();
    let source = r#"
    function first
      input Boolean b[:];
      output Integer index;
    algorithm
      index := 0;
      for i in 1:size(b,1) loop
        if b[i] then index := i; return; end if;
      end for;
      index := -1;
    end first;
    model LoopReturn
      input Real u[4]={0,0,0,0}; Integer index;
    equation index = first({u[1]>0,u[2]>0,u[3]>0,u[4]>0}); end LoopReturn;
    "#;
    let model = compile_with_options(
        source,
        "LoopReturn",
        r#"{"representations":["dae","solve"]}"#,
    )
    .unwrap();
    assert!(model.solve_error().is_none(), "{:?}", model.solve_error());
    assert!(model.representation_json("solve").is_ok());
    for (values, expected) in [
        ([0, 0, 0, 0], -1.0),
        ([0, 1, 0, 1], 2.0),
        ([1, 1, 1, 1], 1.0),
        ([0, 0, 0, 1], 4.0),
    ] {
        let initial_inputs = values
            .into_iter()
            .enumerate()
            .map(|(index, value)| (format!("u[{}]", index + 1), value))
            .collect::<Vec<_>>();
        let options =
            serde_json::json!({"initial_inputs":initial_inputs,"execution_policy":"interpreter"})
                .to_string();
        let mut session = crate::WasmSimulationSession::with_interactive_configuration(
            source,
            "LoopReturn",
            &options,
        )
        .unwrap();
        assert_eq!(session.get("index").unwrap(), Some(expected));
    }
}

#[test]
fn compiled_model_checks_undefined_output_on_loop_return() {
    let _guard = session_test_guard();
    let source = r#"
    function undefinedOnReturn
      input Boolean b[:]; output Integer index;
    algorithm
      for i in 1:size(b,1) loop
        if b[i] then return; end if;
      end for;
      index := -1;
    end undefinedOnReturn;
    model UndefinedLoopReturn
      input Real u[4]={0,0,0,0}; Integer index;
    equation index = undefinedOnReturn({u[1]>0,u[2]>0,u[3]>0,u[4]>0}); end UndefinedLoopReturn;
    "#;
    compile_with_options(
        source,
        "UndefinedLoopReturn",
        r#"{"representations":["dae"]}"#,
    )
    .unwrap();
    for policy in ["auto", "interpreter"] {
        for (first, returns) in [(0, false), (1, true)] {
            let options = serde_json::json!({"execution_policy":policy,
                "initial_inputs":[["u[1]",first],["u[2]",0],["u[3]",0],["u[4]",0]]})
            .to_string();
            match crate::WasmSimulationSession::with_interactive_configuration(
                source,
                "UndefinedLoopReturn",
                &options,
            ) {
                Err(error) => {
                    assert!(returns, "{error}");
                    assert!(
                        error
                            .to_string()
                            .contains("the executed path never assigned it"),
                        "{error}"
                    );
                }
                Ok(mut session) => {
                    assert!(!returns, "an undefined output must not be fabricated");
                    assert_eq!(session.get("index").unwrap(), Some(-1.0));
                }
            }
        }
    }
}
