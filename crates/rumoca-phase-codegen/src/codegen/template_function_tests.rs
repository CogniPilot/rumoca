//! Documented behavior of the shared template filters and functions.

use super::*;

fn render(template: &str, context: Value) -> Result<String, minijinja::Error> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    env.get_template("inline")?.render(context)
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::generated(name),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(op: rumoca_core::OpBinary, lhs: &str, rhs: &str) -> Value {
    Value::from_serialize(rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(var(lhs)),
        rhs: Box::new(var(rhs)),
        span: rumoca_core::Span::DUMMY,
    })
}

#[test]
fn name_and_dimension_filters_follow_their_documented_examples() {
    let rendered = render(
        r#"{{ "Modelica.Math.sin" | last_segment }};{{ [4, 3] | product }};{{ [] | product }};{{ 7 | product }}"#,
        minijinja::context! {},
    )
    .expect("name and dimension filters render");
    assert_eq!(rendered, "sin;12;1;1");
    let overflow = render(
        "{{ [9223372036854775807, 2] | product }}",
        minijinja::context! {},
    )
    .expect_err("an overflowing extent product is refused");
    assert!(overflow.to_string().contains("overflows"), "{overflow}");
}

#[test]
fn source_ref_maps_one_based_flat_indices_to_row_major_subscripts() {
    let rendered = render(
        r#"{{ source_ref("x", [4], 3) }};{{ source_ref("leg.f", [4, 3], 4) }};{{ source_ref("leg.f", [4, 3], 12) }};{{ source_ref("s", [], 5) }}"#,
        minijinja::context! {},
    )
    .expect("source references render");
    assert_eq!(rendered, "x[3];leg.f[2,1];leg.f[4,3];s");
    for (template, reason) in [
        (r#"{{ source_ref("x", [4], 0) }}"#, "one-based"),
        (r#"{{ source_ref("x", [4], 5) }}"#, "exceeds dimensions"),
        (r#"{{ source_ref("x", [4], "two") }}"#, "is not numeric"),
    ] {
        let error = render(template, minijinja::context! {}).expect_err(template);
        assert!(error.to_string().contains(reason), "{template}: {error}");
    }
}

#[test]
fn event_indicators_render_relations_as_signed_residuals() {
    let context = minijinja::context! {
        less => binary(rumoca_core::OpBinary::Lt, "a", "b"),
        greater_equal => binary(rumoca_core::OpBinary::Ge, "c", "d"),
        sum => binary(rumoca_core::OpBinary::Add, "a", "b"),
    };
    let rendered = render(
        "{{ render_event_indicator(less, {}) }};{{ render_event_indicator(greater_equal, {}) }};{{ render_event_indicator(sum, {}) }}",
        context,
    )
    .expect("event indicators render");
    let [less, greater_equal, sum] = rendered.split(';').collect::<Vec<_>>()[..] else {
        panic!("three indicators render: {rendered}");
    };
    assert_eq!(less, "((a) - (b))");
    assert_eq!(greater_equal, "((c) - (d))");
    assert!(
        !sum.starts_with("(("),
        "a non-relational expression renders as itself: {sum}"
    );
}
