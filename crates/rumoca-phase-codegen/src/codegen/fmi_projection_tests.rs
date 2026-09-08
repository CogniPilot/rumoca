//! What a built-in FMI target's templates are allowed to render.
//!
//! Their templates walk the value-reference inventory once and read, per entry,
//! the storage run that sizes their scalar loop and the per-scalar `start` they
//! emit, and they describe no semantic event instant. The checked component
//! proves both facts in Rust, as one typed narrowing, before a renderer exists;
//! these cases pin that the renderer's input is only that narrowed view, that
//! the view is its *only* input, and that a component which cannot be narrowed
//! is refused with a typed cause instead.
//!
//! A delay-bearing component is exactly such a component. Reaching that refusal
//! already means a target's `runtime_events = false` capability gate was
//! bypassed, so the outcome is never a partial description of behavior the
//! generated C does not implement.

use super::codegen_test_support::builtin_template;
use super::*;
use rumoca_core::{SourceMap, Span, TypeId, VarName};
use std::collections::HashMap;

#[derive(Clone, Copy)]
enum FixtureKind {
    EventFree,
    Delay,
    Initialization,
}

fn fixture_dae(kind: FixtureKind) -> dae::Dae {
    let text =
        "input Real u(start=1) = 1; Real x(start=1); der(x) = -x + u; initial equation x = 1;";
    let mut source_map = SourceMap::new();
    let source = source_map.add("fmi_projection_fixture.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len()))
        .expect("fixture provenance is source-backed");
    dae::Dae::construct(source_map, |model| populate_fixture_dae(model, kind, at))
        .expect("fixture DAE is valid by construction")
}

fn populate_fixture_dae<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    kind: FixtureKind,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let real = model.types(|types| {
        types.intern(
            TypeId::new(0),
            dae::ValueType::scalar(dae::ScalarType::Real),
            at,
        )
    })?;
    let (one, two) = model.expressions(|expressions| {
        Ok((
            expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
            expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?,
        ))
    })?;
    let (state, input) = model.variables(|variables| {
        let state = variables.state(
            VarName::new("x"),
            rumoca_core::InstanceId::new(1),
            real,
            at,
            dae::VariableAttributes {
                start: Some(one),
                fixed: Some(rumoca_core::Fixity::Free),
                unit: Some("m".to_string()),
                description: Some("position".to_string()),
                ..dae::VariableAttributes::default()
            },
        )?;
        variables.parameter(
            VarName::new("cp"),
            rumoca_core::InstanceId::new(2),
            real,
            at,
            dae::VariableAttributes {
                binding: Some(two),
                unit: Some("kg".to_string()),
                description: Some("derived mass".to_string()),
                causality: dae::VariableCausality::CalculatedParameter,
                ..dae::VariableAttributes::default()
            },
        )?;
        let input = variables.input(
            VarName::new("u"),
            rumoca_core::InstanceId::new(3),
            real,
            dae::InputVariability::Continuous,
            at,
            dae::VariableAttributes {
                binding: Some(one),
                start: Some(one),
                unit: Some("N".to_string()),
                description: Some("external force".to_string()),
                causality: dae::VariableCausality::Input,
                ..dae::VariableAttributes::default()
            },
        )?;
        Ok((state, input))
    })?;
    let right = fixture_right_hand_side(model, kind, state, input, at)?;
    let residual = model.expressions(|expressions| {
        let derivative = expressions
            .at(at)
            .coordinate(dae::CoordinateInput::Derivative(state))?;
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Subtract, derivative, right)
    })?;
    model.continuous(|continuous| continuous.value_equation(at, residual))?;
    if matches!(kind, FixtureKind::Initialization) {
        let initial = model.expressions(|expressions| {
            let state = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, state, one)
        })?;
        model.initialization(|initialization| initialization.value_equation(at, initial))?;
    }
    Ok(())
}

fn fixture_right_hand_side<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    kind: FixtureKind,
    state: dae::StateId<'dae>,
    input: dae::InputId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let right = match kind {
        FixtureKind::Delay => {
            let (source, delay_time) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::State(state))?,
                    expressions.at(at).literal(dae::DaeLiteral::Real(0.5))?,
                ))
            })?;
            let timing =
                model.temporal(|temporal| temporal.positive_parameter(delay_time, 0.5, at))?;
            model.expressions(|expressions| {
                expressions
                    .at(at)
                    .delay(source, timing, at)
                    .map(|delay| delay.expression())
            })?
        }
        FixtureKind::EventFree | FixtureKind::Initialization => {
            model.expressions(|expressions| {
                let state = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::State(state))?;
                expressions.at(at).unary(dae::UnaryOperator::Negate, state)
            })?
        }
    };
    model.expressions(|expressions| {
        let input = expressions
            .at(at)
            .coordinate(dae::CoordinateInput::Input(input))?;
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, right, input)
    })
}

fn component(kind: FixtureKind) -> solve::fmi::FmiComponent {
    let dae = fixture_dae(kind);
    let lowered = rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .expect("the production DAE-to-SolveModel path constructs one complete root");
    rumoca_phase_solve::fmi::finish_fmi_component(lowered)
        .expect("the production completed-Solve path constructs FMI")
}

fn event_free_view(component: solve::fmi::FmiComponent) -> solve::fmi::FmiEventFreeCodegenView {
    component
        .into_codegen_view()
        .try_event_free()
        .expect("an event-free component narrows to the event-free view")
}

fn event_free_renderer() -> PreparedFmiComponentRendering {
    PreparedFmiComponentRendering::prepare(event_free_view(component(FixtureKind::EventFree)))
        .expect("an event-free component renders")
}

fn render_with_fmi_identity(
    renderer: &PreparedFmiComponentRendering,
    template: &str,
) -> Result<String, CodegenError> {
    rumoca_core::with_target_invocation_brand(|brand| {
        let identities = std::collections::BTreeMap::from([(
            "fmu".to_owned(),
            "fmi-projection-fixture".to_owned(),
        )]);
        let checksums = std::collections::BTreeMap::new();
        let artifact = super::codegen_test_support::artifact_bindings(
            brand,
            "1970-01-01T00:00:00Z",
            "test-tool",
            "FmiProjectionFixture",
            &identities,
            &checksums,
        );
        renderer.render_with_bindings(template, &artifact)
    })
}

/// The renderer's input domain excludes a delay-bearing component: the only
/// way to reach `PreparedFmiComponentRendering::prepare` is through the narrowing, and the
/// narrowing refuses with its own typed cause.
///
/// The cause is the kernel's event class, not the inventory entry it happens to
/// publish: what the templates cannot render is the delay behaviour itself.
#[test]
fn a_delay_bearing_component_reaches_no_renderer() {
    let component = component(FixtureKind::Delay);
    assert!(component.max_step_duration().is_some());

    let rejected = component
        .into_codegen_view()
        .try_event_free()
        .map(|_| ())
        .expect_err("a runtime-event kernel has no storage-backed rendering");

    assert_eq!(
        rejected,
        solve::fmi::FmiEventFreeError::EventBearingKernel {
            class: solve::SolveEventClass::Runtime,
        }
    );
}

/// Event freedom alone is not render readiness: the current built-in C
/// templates also omit initialization owners. The renderer constructor owns
/// that complete capability check and rejects before a template context exists.
#[test]
fn an_initialization_bearing_component_reaches_no_renderer() {
    let view = event_free_view(component(FixtureKind::Initialization));
    let error = PreparedFmiComponentRendering::prepare(view)
        .expect_err("initialization cannot be rendered as an event-free ODE");
    assert!(
        error.to_string().contains("initialization owners"),
        "the rejected capability stays explicit: {error}"
    );
}

/// The positive control for the case above: an event-free component narrows,
/// and both built-in descriptions render from that one view with no predicate
/// of their own.
#[test]
fn the_builtin_fmi_descriptions_render_the_event_free_projection() {
    let component = component(FixtureKind::EventFree);
    assert!(component.max_step_duration().is_none());
    let renderer = PreparedFmiComponentRendering::prepare(event_free_view(component))
        .expect("an event-free component renders");

    for target in ["fmi2", "fmi3"] {
        let rendered = render_with_fmi_identity(
            &renderer,
            builtin_template(target, "modelDescription.xml.jinja"),
        )
        .unwrap_or_else(|error| panic!("{target} description renders: {error}"));
        assert!(
            rendered.contains("start=\"1"),
            "{target} description must emit the checked start: {rendered}"
        );
        assert!(
            !rendered.contains(solve::fmi::MAX_STEP_DURATION_NAME),
            "{target} description must not name a local this component does not publish: {rendered}"
        );
    }
}

/// FMI 2 sizes its scalar loop from each entry's storage run, so the run has to
/// survive into the render context of the narrowed view rather than being
/// rediscovered from the entry's name.
#[test]
fn the_fmi2_scalar_walk_reads_the_projected_storage_run() {
    let rendered = render_with_fmi_identity(
        &event_free_renderer(),
        builtin_template("fmi2", "modelDescription.xml.jinja"),
    )
    .expect("FMI 2 description renders");

    assert!(rendered.contains("name=\"x\""), "{rendered}");
    assert!(rendered.contains("name=\"der(x)\""), "{rendered}");
}

/// The correlated component is the FMI path's only semantic input.
///
/// The compile-time half of this is on `PreparedFmiComponentRendering::prepare`, which no
/// longer has a `Dae` parameter to mispair. This is the runtime half: the
/// render context an FMI target sees carries no DAE entry at all, so no
/// template can come to depend on one arriving beside the component.
#[test]
fn the_fmi_render_context_exposes_no_dae() {
    let renderer = event_free_renderer();

    assert_eq!(
        renderer
            .render("{{ dae is undefined }}|{{ fmi.variables | length }}")
            .expect("the probe template renders"),
        "true|3"
    );
}

fn render_builtin(target: &str, file: &str) -> String {
    let renderer =
        PreparedFmiComponentRendering::prepare(event_free_view(component(FixtureKind::EventFree)))
            .expect("the checked fixture reaches the FMI renderer");
    render_with_fmi_identity(&renderer, builtin_template(target, file))
        .unwrap_or_else(|error| panic!("{target}/{file} renders: {error}"))
}

fn element_with_name<'xml>(xml: &'xml str, element: &str, name: &str) -> &'xml str {
    let start_pattern = format!("<{element} name=\"{name}\"");
    let start = xml
        .find(&start_pattern)
        .unwrap_or_else(|| panic!("missing `{start_pattern}` in {xml}"));
    let remaining = &xml[start..];
    let end = if element == "ScalarVariable" {
        remaining
            .find("</ScalarVariable>")
            .map(|offset| offset + "</ScalarVariable>".len())
            .expect("FMI 2 scalar variable is closed")
    } else {
        remaining
            .find("</Float64>")
            .map(|offset| offset + "</Float64>".len())
            .expect("FMI 3 Float64 variable is closed")
    };
    &remaining[..end]
}

struct Fmi2ExpectedProjection {
    state_value_reference: u32,
    state_start: String,
    derivative_value_reference: u32,
    state_model_index: u32,
    derivative_indices: Vec<u32>,
    initial_unknowns: Vec<u32>,
}

struct Fmi3ExpectedProjection {
    state_value_reference: u32,
    state_start: String,
    derivative_value_reference: u32,
    state_value_reference_link: u32,
    derivative_value_references: Vec<u32>,
    initial_unknowns: Vec<u32>,
}

fn expected_fmi2_projection(view: &solve::fmi::FmiEventFreeCodegenView) -> Fmi2ExpectedProjection {
    let state = view
        .fmi2()
        .variables()
        .iter()
        .find(|variable| variable.name() == "x")
        .expect("fixture state is projected");
    let calculated = view
        .fmi2()
        .variables()
        .iter()
        .find(|variable| variable.name() == "cp")
        .expect("fixture calculated parameter is projected");
    let input = view
        .fmi2()
        .variables()
        .iter()
        .find(|variable| variable.name() == "u")
        .expect("fixture input is projected");
    assert_eq!(state.initial(), Some(solve::fmi::FmiInitial::Approx));
    assert_eq!(
        calculated.initial(),
        Some(solve::fmi::FmiInitial::Calculated)
    );
    assert_eq!(calculated.start(), None);
    assert_eq!(input.initial(), None);
    assert_eq!(input.start(), Some(1.0));
    let derivative = state
        .derivative()
        .expect("fixture state owns its derivative link");
    Fmi2ExpectedProjection {
        state_value_reference: state.value_reference(),
        state_start: serde_json::to_string(
            &state
                .start()
                .expect("fixture state has one checked FMI 2 start"),
        )
        .expect("finite checked start is JSON encodable"),
        derivative_value_reference: derivative.derivative_value_reference(),
        state_model_index: derivative.state_model_index(),
        derivative_indices: view.fmi2().derivative_model_indices().to_vec(),
        initial_unknowns: view.fmi2().initial_unknown_model_indices().to_vec(),
    }
}

fn expected_fmi3_projection(view: &solve::fmi::FmiEventFreeCodegenView) -> Fmi3ExpectedProjection {
    let state = view
        .fmi3()
        .variables()
        .iter()
        .find(|variable| variable.name() == "x")
        .expect("fixture state is projected");
    let calculated = view
        .fmi3()
        .variables()
        .iter()
        .find(|variable| variable.name() == "cp")
        .expect("fixture calculated parameter is projected");
    let input = view
        .fmi3()
        .variables()
        .iter()
        .find(|variable| variable.name() == "u")
        .expect("fixture input is projected");
    assert_eq!(state.initial(), Some(solve::fmi::FmiInitial::Approx));
    assert_eq!(
        calculated.initial(),
        Some(solve::fmi::FmiInitial::Calculated)
    );
    assert_eq!(calculated.start(), None);
    assert_eq!(input.initial(), None);
    assert_eq!(input.start(), Some([1.0].as_slice()));
    let derivative = state
        .derivative()
        .expect("fixture tensor state owns its derivative link");
    Fmi3ExpectedProjection {
        state_value_reference: state.value_reference(),
        state_start: serde_json::to_string(
            &state
                .start()
                .expect("fixture state has checked FMI 3 starts")[0],
        )
        .expect("finite checked start is JSON encodable"),
        derivative_value_reference: derivative.derivative_value_reference(),
        state_value_reference_link: derivative.state_value_reference(),
        derivative_value_references: view.fmi3().derivative_value_references().to_vec(),
        initial_unknowns: view.fmi3().initial_unknown_value_references().to_vec(),
    }
}

fn render_fmi_descriptions(view: solve::fmi::FmiEventFreeCodegenView) -> (String, String) {
    let renderer = PreparedFmiComponentRendering::prepare(view)
        .expect("the checked fixture reaches the renderer");
    let fmi2_xml = render_with_fmi_identity(
        &renderer,
        builtin_template("fmi2", "modelDescription.xml.jinja"),
    )
    .expect("FMI 2 XML renders");
    let fmi3_xml = render_with_fmi_identity(
        &renderer,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
    )
    .expect("FMI 3 XML renders");
    (fmi2_xml, fmi3_xml)
}

fn assert_fmi2_xml(xml: &str, expected: &Fmi2ExpectedProjection) {
    let state = element_with_name(xml, "ScalarVariable", "x");
    assert!(state.contains(&format!(
        "valueReference=\"{}\"",
        expected.state_value_reference
    )));
    assert!(state.contains("initial=\"approx\""));
    assert!(state.contains(&format!("start=\"{}\"", expected.state_start)));
    assert!(state.contains("unit=\"m\""));
    let calculated = element_with_name(xml, "ScalarVariable", "cp");
    assert!(calculated.contains("causality=\"calculatedParameter\""));
    assert!(calculated.contains("initial=\"calculated\""));
    assert!(!calculated.contains("start="));
    assert!(calculated.contains("unit=\"kg\""));
    let input = element_with_name(xml, "ScalarVariable", "u");
    assert!(input.contains("causality=\"input\""));
    assert!(input.contains("start=\"1.0\""));
    assert!(!input.contains("initial="));
    let derivative = element_with_name(xml, "ScalarVariable", "der(x)");
    assert!(derivative.contains(&format!(
        "valueReference=\"{}\"",
        expected.derivative_value_reference
    )));
    assert!(derivative.contains(&format!("derivative=\"{}\"", expected.state_model_index)));
    for index in &expected.derivative_indices {
        assert!(xml.contains(&format!("<Unknown index=\"{index}\"/>")));
    }
    for index in &expected.initial_unknowns {
        assert!(xml.contains(&format!("<Unknown index=\"{index}\"/>")));
    }
}

fn assert_fmi3_xml(xml: &str, expected: &Fmi3ExpectedProjection) {
    let state = element_with_name(xml, "Float64", "x");
    assert!(state.contains(&format!(
        "valueReference=\"{}\"",
        expected.state_value_reference
    )));
    assert!(state.contains("initial=\"approx\""));
    assert!(state.contains(&format!("start=\"{}\"", expected.state_start)));
    assert!(state.contains("unit=\"m\""));
    let calculated = element_with_name(xml, "Float64", "cp");
    assert!(calculated.contains("causality=\"calculatedParameter\""));
    assert!(calculated.contains("initial=\"calculated\""));
    assert!(!calculated.contains("start="));
    assert!(calculated.contains("unit=\"kg\""));
    let input = element_with_name(xml, "Float64", "u");
    assert!(input.contains("causality=\"input\""));
    assert!(input.contains("start=\"1.0\""));
    assert!(!input.contains("initial="));
    let derivative = element_with_name(xml, "Float64", "der(x)");
    assert!(derivative.contains(&format!(
        "valueReference=\"{}\"",
        expected.derivative_value_reference
    )));
    assert!(derivative.contains(&format!(
        "derivative=\"{}\"",
        expected.state_value_reference_link
    )));
    for value_reference in &expected.derivative_value_references {
        assert!(xml.contains(&format!(
            "<ContinuousStateDerivative valueReference=\"{value_reference}\"/>"
        )));
    }
    for value_reference in &expected.initial_unknowns {
        assert!(xml.contains(&format!(
            "<InitialUnknown valueReference=\"{value_reference}\"/>"
        )));
    }
}

fn assert_unit_definitions(fmi2_xml: &str, fmi3_xml: &str) {
    for unit in ["N", "kg", "m", "s"] {
        assert_eq!(
            fmi2_xml.matches(&format!("<Unit name=\"{unit}\"")).count(),
            1,
            "FMI 2 must define referenced unit {unit} exactly once"
        );
        assert_eq!(
            fmi3_xml.matches(&format!("<Unit name=\"{unit}\"")).count(),
            1,
            "FMI 3 must define referenced unit {unit} exactly once"
        );
    }
}

#[test]
fn rendered_xml_uses_the_checked_version_projection_verbatim() {
    let view = event_free_view(component(FixtureKind::EventFree));
    let expected_fmi2 = expected_fmi2_projection(&view);
    let expected_fmi3 = expected_fmi3_projection(&view);
    let (fmi2_xml, fmi3_xml) = render_fmi_descriptions(view);
    assert_fmi2_xml(&fmi2_xml, &expected_fmi2);
    assert_fmi3_xml(&fmi3_xml, &expected_fmi3);
    assert_unit_definitions(&fmi2_xml, &fmi3_xml);
}

#[test]
fn generated_c_uses_the_same_checked_value_reference_plan_as_xml() {
    for (target, projection_key) in [("fmi2", "fmi2"), ("fmi3", "fmi3")] {
        let xml = render_builtin(target, "modelDescription.xml.jinja");
        let c = render_builtin(target, "model.c.jinja");
        let view = event_free_view(component(FixtureKind::EventFree));
        let (variables, derivatives) = if target == "fmi2" {
            (
                view.fmi2()
                    .variables()
                    .iter()
                    .map(|variable| (variable.name().to_string(), variable.value_reference()))
                    .collect::<Vec<_>>(),
                view.fmi2()
                    .derivatives()
                    .iter()
                    .map(|derivative| {
                        (
                            derivative.name().to_string(),
                            derivative.link().derivative_value_reference(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        } else {
            (
                view.fmi3()
                    .variables()
                    .iter()
                    .map(|variable| (variable.name().to_string(), variable.value_reference()))
                    .collect::<Vec<_>>(),
                view.fmi3()
                    .derivatives()
                    .iter()
                    .map(|derivative| {
                        (
                            derivative.name().to_string(),
                            derivative.link().derivative_value_reference(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        };
        for (name, value_reference) in variables.into_iter().chain(derivatives) {
            assert!(
                xml.contains(&format!(
                    "name=\"{name}\" valueReference=\"{value_reference}\""
                )),
                "{target} XML omits checked {name} VR {value_reference}"
            );
            assert!(
                c.contains(&format!("case {value_reference}:")),
                "{target} C omits checked {name} VR {value_reference}"
            );
        }
        let template = builtin_template(target, "model.c.jinja");
        assert!(template.contains(&format!("fmi.{projection_key}.variables")));
        assert!(
            !template.contains("namespace("),
            "{target} C may not own counters"
        );
    }
}

#[test]
fn fmi_xml_templates_cannot_recreate_projection_policy_or_identity() {
    for (target, projection_key) in [("fmi2", "fmi2"), ("fmi3", "fmi3")] {
        let template = builtin_template(target, "modelDescription.xml.jinja");
        for forbidden in [
            "namespace(",
            "fmi.variables",
            "derivative_value_reference_base_fmi3",
            "variable.role",
            "variable.causality ==",
            "initial=\"exact\"",
            "initial=\"calculated\"",
            "value_reference_fmi3",
            "<Unit name=\"s\">",
        ] {
            assert!(
                !template.contains(forbidden),
                "{target} XML template recreates checked policy through `{forbidden}`"
            );
        }
        assert!(template.contains(&format!("fmi.{projection_key}.variables")));
        assert!(template.contains(&format!("fmi.{projection_key}.derivatives")));
        assert!(template.contains(&format!("fmi.{projection_key}.unit_definitions")));
    }
}
