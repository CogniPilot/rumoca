//! Regression tests for interface-flow balance accounting.

use rumoca_compile::{Session, SessionConfig};

#[test]
fn test_interface_flow_does_not_double_count_closed_boundary() {
    let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

connector Plug
    parameter Integer m = 3;
    Pin pin[m];
end Plug;

model Delta
    parameter Integer m = 3;
    Plug plug_p(m = m);
    Plug plug_n(m = m);
equation
    plug_n.pin[1].i + plug_p.pin[2].i = 0;
    plug_n.pin[2].i + plug_p.pin[3].i = 0;
    plug_n.pin[3].i + plug_p.pin[1].i = 0;
    plug_n.pin[1].v = plug_p.pin[2].v;
    plug_n.pin[2].v = plug_p.pin[3].v;
    plug_n.pin[3].v = plug_p.pin[1].v;
end Delta;

model BoundaryWithDelta
    parameter Integer m = 3;
    Plug plug1(m = m);
    Plug plug2(m = m);
    Delta d1(m = m);
    Delta d2(m = m);
equation
    connect(plug1, d1.plug_p);
    connect(plug2, d2.plug_p);
    connect(d1.plug_n, d2.plug_n);
end BoundaryWithDelta;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("model should parse/typecheck");

    let result = session
        .compile_model("BoundaryWithDelta")
        .expect("model should compile");

    let dae = &result.dae;
    let detail = rumoca_eval_dae::analysis::balance_detail(dae);
    let unknown_scalars = detail.state_unknowns + detail.alg_unknowns + detail.output_unknowns;
    let boundary_flow_zero_scalars: usize = dae
        .f_x
        .iter()
        .filter(|eq| eq.origin.starts_with("unconnected flow:"))
        .map(|eq| eq.scalar_count)
        .sum();

    assert_eq!(
        detail.f_x_scalar, unknown_scalars,
        "raw continuous equations should already close unknowns in this reproducer"
    );
    assert!(
        boundary_flow_zero_scalars > 0,
        "reproducer requires explicit boundary flow=0 equations (MLS §9.2)"
    );
    assert!(
        detail.interface_flow_count > 0,
        "raw interface-flow count should detect top-level connector flows (MLS §4.7)"
    );
    let base_without_iflow =
        (detail.f_x_scalar + detail.algorithm_outputs + detail.when_eq_scalar) as i64;
    let iflow_needed = (unknown_scalars as i64 - base_without_iflow).max(0);
    let effective_iflow = (detail.interface_flow_count as i64).min(iflow_needed);
    assert_eq!(
        effective_iflow, 0,
        "effective interface-flow contribution must not double-count flows already closed by explicit flow=0 equations"
    );
    assert_eq!(
        rumoca_eval_dae::analysis::balance(dae),
        0,
        "interface-flow terms must not overconstrain already-closed systems"
    );
}
