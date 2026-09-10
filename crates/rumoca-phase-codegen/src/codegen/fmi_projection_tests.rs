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
use rumoca_core::{SourceId, Span};

fn fixture_span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("fmi_projection_fixture.mo"),
        0,
        1,
    )
}

fn one_row_block() -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.5 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span()
            .require_provenance("FMI projection fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture delay program is computable")
}

/// One two-scalar state run, so a version template has an ordinary entry to
/// walk, plus the delay partition the caller asks for.
fn model_with_one_state_run(delay_bearing: bool) -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.layout = solve::VarLayout::from_parts(IndexMap::new(), 2, 1);
    model.problem.solve_layout.variable_storage_runs = vec![solve::SolveVariableStorageRun {
        base: solve::ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
        scalar_count: 2,
        role: solve::SolveVariableStorageRole::State,
        value_kind: solve::SolveVariableValueKind::Real,
    }];
    model.problem.solve_layout.variable_declarations = vec![solve::SolveVariableDeclaration::new(
        solve::SolveVariableStorageRole::State,
        solve::SolveVariableValueKind::Real,
    )];
    model.problem.solve_layout.state_scalar_count = 2;
    if delay_bearing {
        model.problem.events.delays = solve::SolveDelayPartition {
            source_rhs: one_row_block(),
            delay_time_rhs: one_row_block(),
            delay_max_rhs: one_row_block(),
            value_parameter_indices: vec![0],
            source_is_discrete: vec![false],
        };
    }
    model
}

fn state_input() -> solve::fmi::FmiVariableInput {
    solve::fmi::FmiVariableInput {
        name: "x".to_string(),
        scalar_names: vec!["x[1]".to_string(), "x[2]".to_string()],
        role: solve::SolveVariableStorageRole::State,
        value_kind: solve::SolveVariableValueKind::Real,
        dimensions: vec![2],
        start: vec![1.0, 2.0],
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality: solve::fmi::FmiCausality::Local,
        variability: solve::fmi::FmiVariability::Continuous,
        tunable: false,
        declaration: fixture_span(),
    }
}

fn component(delay_bearing: bool) -> solve::fmi::FmiComponent {
    solve::fmi::FmiComponent::construct(
        model_with_one_state_run(delay_bearing),
        vec![state_input()],
    )
    .expect("one state run is a complete inventory")
}

fn component_with_initialization() -> solve::fmi::FmiComponent {
    let mut model = model_with_one_state_run(false);
    model.problem.initialization.update_rhs = one_row_block();
    model.problem.initialization.update_targets = vec![solve::ScalarSlot::Y {
        index: 0,
        byte_offset: 0,
    }];
    solve::fmi::FmiComponent::construct(model, vec![state_input()])
        .expect("the initialization-bearing fixture is a checked component")
}

fn event_free_view(component: solve::fmi::FmiComponent) -> solve::fmi::FmiEventFreeCodegenView {
    component
        .into_codegen_view()
        .try_event_free()
        .expect("an event-free component narrows to the event-free view")
}

fn event_free_renderer() -> SolveTemplateRenderer {
    SolveTemplateRenderer::new_owned_with_fmi(event_free_view(component(false)))
        .expect("an event-free component renders")
}

fn artifact_identities() -> serde_json::Value {
    serde_json::json!({ "identities": { "fmu": "fmi-projection-fixture" } })
}

/// The renderer's input domain excludes a delay-bearing component: the only
/// way to reach `new_owned_with_fmi` is through the narrowing, and the
/// narrowing refuses with its own typed cause.
///
/// The cause is the kernel's event class, not the inventory entry it happens to
/// publish: what the templates cannot render is the delay behaviour itself.
#[test]
fn a_delay_bearing_component_reaches_no_renderer() {
    let component = component(true);
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

/// ME-PARAM-001: parameter initialization support does not admit state writes.
#[test]
fn a_state_initialization_bearing_component_reaches_no_renderer() {
    let view = event_free_view(component_with_initialization());
    let error = SolveTemplateRenderer::new_owned_with_fmi(view)
        .expect_err("state initialization cannot be rendered by the parameter-only C profile");
    assert!(
        error
            .to_string()
            .contains("C initialization can only assign parameter storage"),
        "the rejected capability stays explicit: {error}"
    );
}

/// The positive control for the case above: an event-free component narrows,
/// and both built-in descriptions render from that one view with no predicate
/// of their own.
#[test]
fn the_builtin_fmi_descriptions_render_the_event_free_projection() {
    let component = component(false);
    assert!(component.max_step_duration().is_none());
    let renderer = SolveTemplateRenderer::new_owned_with_fmi(event_free_view(component))
        .expect("an event-free component renders");

    for target in ["fmi2", "fmi3"] {
        let rendered = renderer
            .render_with_name_and_artifact(
                builtin_template(target, "modelDescription.xml.jinja"),
                "FmiProjectionFixture",
                &artifact_identities(),
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
    let rendered = event_free_renderer()
        .render_with_name_and_artifact(
            builtin_template("fmi2", "modelDescription.xml.jinja"),
            "FmiProjectionFixture",
            &artifact_identities(),
        )
        .expect("FMI 2 description renders");

    assert!(rendered.contains("name=\"x[1]\""), "{rendered}");
    assert!(rendered.contains("name=\"x[2]\""), "{rendered}");
    assert!(rendered.contains("name=\"der(x[1])\""), "{rendered}");
}

/// The correlated component is the FMI path's only semantic input.
///
/// The compile-time half of this is on `new_owned_with_fmi` itself, which no
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
        "true|1"
    );
}
