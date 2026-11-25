//! The `rust-fixed-ode` typed API (`typed` module): records named after the
//! model's variables, their projections onto the slot arrays, and the `Model`
//! handle over `Simulator`, checked against the slot API.

use super::rust_fixed_ode_events::{compiled, run_body};
use super::*;

const ALL_ROLES: &str = r#"
model AllRoles
  constant Real c = 2;
  parameter Real k = 3 "Inflow gain";
  parameter Real g[2] = {1, 2} "Weights";
  parameter Real x0 = 0.5 "Initial level";
  input Real u "Inflow";
  Real x(start = x0, fixed = true) "Level";
  Real a "Doubled level";
  output Real y "Measured level";
  discrete Integer n(start = 0, fixed = true) "Crossings";
equation
  der(x) = k * u - g[1] * x;
  a = c * x;
  y = a + g[2];
  when x > 1 then
    n = pre(n) + 1;
  end when;
end AllRoles;
"#;

/// Every record field reads the slot the slot API names; `Model` matches a
/// `Simulator` driven through the slot API step for step.
#[test]
fn rust_fixed_ode_typed_records_match_the_slot_api() {
    let result = compiled("AllRoles", ALL_ROLES);
    let (_directory, files) = rendered_model_target(&result, "AllRoles", "rust-fixed-ode");
    let source = &files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module")
        .content;
    for doc in [
        "/// Inflow gain",
        "/// Weights",
        "/// Level",
        "/// Crossings",
    ] {
        assert!(source.contains(doc), "missing doc comment {doc:?}");
    }
    let stdout = run_body(
        &result,
        "AllRoles",
        r#"
    let at = |slots: &[(&str, usize)], name: &str| slot(slots, name).unwrap();
    let parameters = typed::Parameters { k: 2.0, ..typed::Parameters::default() };
    assert_eq!(typed::Parameters::default().k, 3.0);
    assert_eq!(typed::Parameters::default().g, [1.0, 2.0]);

    let mut p = DEFAULT_P;
    parameters.write(&mut p);
    assert_eq!(p[at(P_SLOTS, "k")], 2.0);
    assert_eq!(p[at(P_SLOTS, "g")..at(P_SLOTS, "g") + 2], [1.0, 2.0]);
    init_params(&mut p).unwrap();
    let mut y = [0.0; Y_LEN];
    init_state(&p, &mut y).unwrap();
    let _ = &mut sim;
    let mut reference = Simulator::new(Method::Rk4, EventLocation::Bracketed, EventSettings::default(), 0.0, y, p);
    reference.initialize().unwrap();

    let mut model = typed::Model::new(&parameters).unwrap();
    assert_eq!(model.states().x, 0.5);
    let inputs = typed::Inputs { u: 1.0 };
    for _ in 0..40 {
        model.step(&inputs, 0.05, 2).unwrap();
        inputs.write(reference.p_mut());
        reference.do_step(0.025).unwrap();
        reference.do_step(0.025).unwrap();
        let mut refreshed = *reference.y();
        refresh_algebraics(reference.time(), &mut refreshed, reference.p()).unwrap();
        assert_eq!(model.time(), reference.time());
        assert_eq!(model.states(), typed::States::read(reference.y()));
        assert_eq!(model.states().x, reference.y()[at(Y_SLOTS, "x")]);
        assert_eq!(model.outputs().y, refreshed[at(Y_SLOTS, "y")]);
        assert_eq!(model.discretes().n, reference.p()[at(P_SLOTS, "n")]);
        assert_eq!(model.simulator().y(), reference.y());
    }
    assert!(model.discretes().n >= 1.0, "the level crosses 1");
    println!("ok");
"#,
    );
    assert_eq!(stdout.trim(), "ok");
}

/// An event-free model gets the same records and handle.
#[test]
fn rust_fixed_ode_typed_records_step_an_event_free_model() {
    let stdout = run_body(
        &checked_decay(),
        "Decay",
        r#"
    let _ = &mut sim;
    let mut model = typed::Model::with(
        Method::Euler,
        EventLocation::StepEnd,
        EventSettings::default(),
        &typed::Parameters::default(),
    )
    .unwrap();
    // `init_state` applies only parameter-computed starts: `x` starts at 0.
    assert_eq!(model.states().x, 0.0);
    model.step(&typed::Inputs::default(), 0.1, 2).unwrap();
    assert!((model.time() - 0.1).abs() < 1e-15);
    assert_eq!(model.simulator().y()[0], model.states().x);
    let mut y = [0.0; Y_LEN];
    typed::States { x: 0.25 }.write(&mut y);
    assert_eq!(typed::States::read(&y).x, 0.25);
    println!("ok");
"#,
    );
    assert_eq!(stdout.trim(), "ok");
}

/// Two Modelica names that flatten to one Rust field are refused before any
/// source is written.
#[test]
fn rust_fixed_ode_typed_records_refuse_colliding_field_names() {
    let result = compiled(
        "Collide",
        r#"
model Collide
  model A
    parameter Real b_c = 1;
  end A;
  model AB
    parameter Real c = 2;
  end AB;
  A a;
  AB a_b;
  Real x(start = 0, fixed = true);
equation
  der(x) = a.b_c + a_b.c;
end Collide;
"#,
    );
    let error = rumoca::render_target_files(&result, "Collide", "rust-fixed-ode", None)
        .expect_err("colliding field names are refused");
    assert!(
        format!("{error:#}").contains("unsupported-feature:typed.field_collision"),
        "{error:#}"
    );
}
