use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn structured_for_equation_remains_compact_and_simulates_all_rows() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "family.mo",
            "model Family Real x[3]; equation for i in 1:3 loop x[i]=i; end for; end Family;",
        )
        .expect("fixture parses");

    let compiled = session
        .compile_model("Family")
        .expect("structured family should compile without fabricated scalar DAE rows");
    compiled.dae.inspect(|view| {
        assert_eq!(view.continuous_equation_count(), 0);
        assert_eq!(view.continuous_family_count(), 1);
    });

    let simulation =
        simulate_dae(&compiled.dae, &SimOptions::default()).expect("family should simulate");
    for (name, expected) in [("x[1]", 1.0), ("x[2]", 2.0), ("x[3]", 3.0)] {
        let index = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("trace must retain structured coordinate `{name}`"));
        let value = simulation.data[index]
            .last()
            .copied()
            .expect("simulation trace is non-empty");
        assert!(
            (value - expected).abs() <= 1.0e-12,
            "{name} = {value}, expected {expected}"
        );
    }
}

/// A one-element array body (`ports[i].Xi_outflow = medium.Xi` with `nXi = 1`)
/// is an array equation at each point (MLS §10.6.1), not a scalar one: the
/// loop projects its residual as it does for a longer one.
#[test]
fn a_one_element_array_body_is_an_array_equation_at_each_point() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "one.mo",
            "model OneElement Real x[2, 1]; Real y[1]; equation y = {time}; \
             for i in 1:2 loop x[i, :] = y; end for; end OneElement;",
        )
        .expect("fixture parses");
    let compiled = session
        .compile_model("OneElement")
        .expect("a one-element array body compiles");
    let simulation = simulate_dae(&compiled.dae, &SimOptions::default())
        .expect("a one-element array body simulates");
    for name in ["x[1,1]", "x[2,1]"] {
        let index = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("trace must retain `{name}`"));
        let value = simulation.data[index].last().copied().unwrap();
        assert!((value - 1.0).abs() <= 1.0e-12, "{name} = {value}");
    }
}
