use super::*;
use std::collections::{BTreeMap, BTreeSet};

fn render_inline(source: &str) -> Result<String, minijinja::Error> {
    let mut env = target_template_environment();
    env.add_template_owned("probe".to_string(), source.to_string())?;
    env.get_template("probe")?.render(minijinja::context! {})
}

fn render_inline_with_input(source: &str, input: String) -> Result<String, minijinja::Error> {
    let mut env = target_template_environment();
    env.add_template_owned("probe".to_string(), source.to_string())?;
    env.get_template("probe")?
        .render(minijinja::context! { input => input })
}

#[test]
fn modelica_string_escape_filter_is_exact_canonical_codec_transport() {
    let input = "quote \" slash \\ bell\x07 backspace\x08 formfeed\x0c newline\nreturn\r tab\t vertical\x0b unicode λ".to_string();
    let rendered = render_inline_with_input("{{ input | modelica_string_escape }}", input.clone())
        .expect("the registered lexical codec accepts string input");

    assert_eq!(rendered, rumoca_core::escape_modelica_string(&input));
}

#[test]
fn modelica_string_escape_filter_rejects_non_string_input() {
    let error = render_inline("{{ 7 | modelica_string_escape }}")
        .expect_err("lexical codec input typing must fail closed");

    assert!(
        error.to_string().contains("invalid operation")
            || error.to_string().contains("cannot convert")
            || error.to_string().contains("expected string"),
        "unexpected diagnostic: {error}"
    );
}

#[test]
fn production_template_global_surface_is_exact_and_has_no_state_introspection() {
    let globals = target_template_environment()
        .globals()
        .map(|(name, _value)| name.to_owned())
        .collect::<BTreeSet<_>>();
    let expected = [
        "dict",
        "fail",
        "fail_at",
        "namespace",
        "range",
        "render_linsolve_mlir",
        "render_matmul_mlir",
        "render_solve_native_family_mlir",
        "render_solve_native_family_output_index_wgsl",
        "render_solve_native_family_output_map_start",
        "render_solve_native_family_wgsl",
        "render_solve_row_output_wgsl",
        "wgsl_kernel_schedule_json",
        "wgsl_kernel_workgroup_total",
        "wgsl_native_family_inventory_json",
    ]
    .into_iter()
    .map(str::to_owned)
    .collect::<BTreeSet<_>>();

    assert_eq!(globals, expected);
    let error = render_inline("{{ debug() }}")
        .expect_err("the production environment must not expose MiniJinja State debugging");
    assert!(
        error.to_string().contains("unknown function") && error.to_string().contains("debug"),
        "unexpected debug-global diagnostic: {error:#}"
    );
}

#[test]
fn artifact_identity_scalars_are_flattened_and_semantic_collisions_reject() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let identities = BTreeMap::from([("alpha".to_owned(), "identity-secret".to_owned())]);
        let checksums = BTreeMap::new();
        let facts = super::codegen_test_support::artifact_bindings(
            brand,
            "1970-01-01T00:00:00Z",
            "test-tool",
            "model",
            &identities,
            &checksums,
        );
        let context = facts
            .render_context(minijinja::context! { semantic => "checked" })
            .expect("disjoint semantic and identity names construct one context");
        let artifact = context
            .get_item(&Value::from("artifact"))
            .expect("artifact projection lookup succeeds");
        assert!(
            artifact
                .get_item(&Value::from("identities"))
                .expect("missing artifact field lookup succeeds")
                .is_undefined(),
            "the serialized artifact object must contain no identity map"
        );
        assert_eq!(
            context
                .get_item(&Value::from("__rumoca_artifact_identity_v1_alpha"))
                .expect("flattened identity lookup succeeds")
                .as_str(),
            Some("identity-secret")
        );

        let collision = facts
            .render_context(minijinja::context! {
                __rumoca_artifact_identity_v1_alpha => "forged"
            })
            .expect_err("semantic context must not supply the reserved identity namespace");
        assert!(
            collision.to_string().contains(
                "collides with reserved top-level name '__rumoca_artifact_identity_v1_alpha'"
            ),
            "unexpected collision diagnostic: {collision:#}"
        );
    });
}

#[test]
fn debug_cannot_dump_a_context_that_contains_an_identity_scalar() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let identities = BTreeMap::from([("alpha".to_owned(), "identity-secret".to_owned())]);
        let checksums = BTreeMap::new();
        let facts = super::codegen_test_support::artifact_bindings(
            brand,
            "1970-01-01T00:00:00Z",
            "test-tool",
            "model",
            &identities,
            &checksums,
        );
        let context = facts
            .render_context(minijinja::context! {})
            .expect("checked identity scalar context");
        let mut environment = target_template_environment();
        environment
            .add_template_owned("probe".to_owned(), "{{ debug() }}".to_owned())
            .expect("debug mutation parses under the production grammar");
        let error = environment
            .get_template("probe")
            .expect("debug mutation compiles")
            .render(context)
            .expect_err("debug must remain absent at execution");
        assert!(
            !format!("{error:#?}").contains("identity-secret"),
            "a failed debug lookup must not dump the render context: {error:#?}"
        );
    });
}
