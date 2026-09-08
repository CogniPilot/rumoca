use minijinja::Value;
use rumoca_core::with_target_invocation_brand;

use super::{TemplateBindingError, TemplateBindingValue, TemplateBindings};

macro_rules! assert_not_implemented {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }
            impl<T> AmbiguousIfImplemented<()> for T {}
            struct Implements;
            impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

assert_not_implemented!(TemplateBindings<'static>, ::serde::Serialize);

#[test]
fn compiler_proves_bindings_have_no_wholesale_serialization_authority() {}

#[test]
fn bind_rejects_a_non_identifier_name() {
    with_target_invocation_brand(|brand| {
        let mut bindings = TemplateBindings::construct(brand);
        let error = bindings
            .bind("not a name", TemplateBindingValue::Text("value".to_owned()))
            .expect_err("a non-identifier binding name is refused");
        assert_eq!(
            error,
            TemplateBindingError::NonIdentifierName("not a name".to_owned())
        );
    });
}

#[test]
fn bind_rejects_a_duplicate_name() {
    with_target_invocation_brand(|brand| {
        let mut bindings = TemplateBindings::construct(brand);
        bindings
            .bind("model_name", TemplateBindingValue::Text("first".to_owned()))
            .expect("the first binding is accepted");
        let error = bindings
            .bind(
                "model_name",
                TemplateBindingValue::Text("second".to_owned()),
            )
            .expect_err("a duplicate binding name is refused");
        assert_eq!(
            error,
            TemplateBindingError::DuplicateName("model_name".to_owned())
        );
    });
}

#[test]
fn bindings_cannot_shadow_a_semantic_root() {
    with_target_invocation_brand(|brand| {
        let mut bindings = TemplateBindings::construct(brand);
        bindings
            .bind("model_name", TemplateBindingValue::Text("bound".to_owned()))
            .expect("model_name binds");
        let collision = bindings
            .render_context(minijinja::context! { model_name => "forged" })
            .expect_err("a semantic root cannot reuse a bound name");
        assert!(
            collision
                .to_string()
                .contains("collides with reserved top-level name 'model_name'"),
            "unexpected collision diagnostic: {collision:#}"
        );
    });
}

#[test]
fn disjoint_roots_and_bindings_join_into_one_context() {
    with_target_invocation_brand(|brand| {
        let mut bindings = TemplateBindings::construct(brand);
        bindings
            .bind("model_name", TemplateBindingValue::Text("Model".to_owned()))
            .expect("model_name binds");
        let mut artifact = std::collections::BTreeMap::new();
        artifact.insert(
            "generation_tool".to_owned(),
            TemplateBindingValue::Text("tool".to_owned()),
        );
        bindings
            .bind("artifact", TemplateBindingValue::Object(artifact))
            .expect("artifact binds");

        let context = bindings
            .render_context(minijinja::context! { semantic => "checked" })
            .expect("disjoint semantic and bound names join");
        assert_eq!(
            context.get_item(&Value::from("semantic")).unwrap().as_str(),
            Some("checked")
        );
        assert_eq!(
            context
                .get_item(&Value::from("model_name"))
                .unwrap()
                .as_str(),
            Some("Model")
        );
        assert_eq!(
            context
                .get_item(&Value::from("artifact"))
                .unwrap()
                .get_item(&Value::from("generation_tool"))
                .unwrap()
                .as_str(),
            Some("tool")
        );
    });
}
