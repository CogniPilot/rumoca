#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
use super::*;
#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
use crate::target_render_api::render_target_files;

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn solve_render_replays_checked_assignments_and_retains_coupled_loops() {
    let _guard = session_test_guard();
    let manifest = r#"
version = 1
ir = "solve"
[capabilities]
continuous_states = true
residual_equations = true
initialization = true
events = true
clocks = true
scalar_fallback = true
[[files]]
path = "model.js"
template = "model.jinja"
"#;
    let templates = BTreeMap::from([("model.jinja".to_owned(),
        "{{ ir_kind }}:{{ dae.variables|length }}:{% if solve.continuous.exact_algebraic_assignments %}{{ solve.continuous.exact_algebraic_assignments.plan.stored_output_count }}{% else %}implicit{% endif %}".to_owned())]);
    let templates_json = serde_json::to_string(&templates).unwrap();
    for (equations, expected) in [
        ("a=x*x; b=sqrt(a);", "solve:3:2"),
        ("a=cos(b)+x; b=sin(a);", "solve:3:implicit"),
    ] {
        let source = format!(
            "model Fixture Real x(start=1,fixed=true); Real a; Real b; equation der(x)=-x; {equations} end Fixture;"
        );
        let mut session = Session::default();
        session.update_document("fixture.mo", &source);
        let compiled = compile_requested_model(&mut session, "Fixture").unwrap();
        let rendered = render_target_files(
            "",
            manifest,
            &templates_json,
            &compiled.dae,
            None,
            "Fixture",
        )
        .unwrap();
        assert_eq!(rendered[0].content, expected);
    }
}
