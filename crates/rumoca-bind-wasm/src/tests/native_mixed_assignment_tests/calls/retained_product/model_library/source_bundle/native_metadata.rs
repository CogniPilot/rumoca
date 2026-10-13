use super::*;
use rumoca_ir_solve::fmi::FmiNativeDeploymentView;

#[test]
fn native_publication_metadata_retains_the_actual_getter_source_without_c_admission() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let code = preparation(&component);
        let view = FmiNativeDeploymentView::from_preparation(code.inputs()).unwrap();
        assert!(view.root().shares_root_owner(code.inputs().root()));
        assert!(std::ptr::eq(view.root().metadata(), component.metadata()));
        assert!(std::ptr::eq(view.variables(), component.variables()));
        assert!(!view.capabilities().fmu_state);
        assert_eq!(view.capabilities().interfaces, rumoca_ir_solve::fmi::FmiInterfaceProfile::CoSimulation);
        let renderer = rumoca_compile::codegen::NativeFmiTemplateRenderer::new(view).unwrap();
        let xml = renderer.render_with_name_and_artifact(
            include_str!("../../../../../../../../rumoca-phase-codegen/src/templates/fmi3/modelDescription.xml.jinja"),
            "ReturnPredicates", &serde_json::json!({"identities":{"fmu":"native-control"}}),
        ).unwrap();
        assert!(xml.contains("<CoSimulation "));
        assert!(!xml.contains("<ModelExchange "));
        assert!(!xml.contains("canGetAndSetFMUState"));
        assert!(xml.contains("<Int64 name=\"k\""));
        for category in ["error", "warning", "message"] {
            assert!(xml.contains(&format!("<Category name=\"{category}\"")));
        }
        assert!(!xml.contains("<Category name=\"projection\""));
        Ok(String::new())
    }).unwrap();
}

#[test]
fn native_publication_canonical_int64_start_does_not_round_through_float64() {
    let _lock = session_test_guard();
    let source = returns::SOURCE.replace(
        "input Integer k = 1;",
        "input Integer k = 9223372036854775807;",
    );
    with_source_component(&source, "ReturnPredicates", |component| {
        let code = preparation(&component);
        let view = FmiNativeDeploymentView::from_preparation(code.inputs()).unwrap();
        let renderer = rumoca_compile::codegen::NativeFmiTemplateRenderer::new(view).unwrap();
        let projected = renderer.render_with_name_and_artifact(
            "{% for v in fmi.variables %}{% if v.name == 'k' %}{{ v.fmi3_type }}:{% for x in v.start_fmi3 %}{{ x | int }}{% endfor %}{% endif %}{% endfor %}",
            "ReturnPredicates", &(),
        ).unwrap();
        assert_eq!(projected, "Int64:9223372036854775807");
        Ok(String::new())
    }).unwrap();
}
