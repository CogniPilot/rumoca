//! The scalar FMI route reads a compiled stateful component by value
//! reference: state derivatives at the derivative base plus the state's
//! declaration ordinal (array states expanding over their extent), mixed and
//! repeated batches in request order, and whole-batch preflight that leaves
//! the caller's output untouched on refusal.

use rumoca_solver::fmi_me::{MeInstanceConfig, MeModelSource, MeRetainedComponent};

fn value_reference(component: &rumoca_ir_solve::fmi::FmiComponent, name: &str) -> u32 {
    component
        .variables()
        .iter()
        .find(|variable| variable.name() == name)
        .expect("published variable")
        .value_reference_fmi3()
}

#[test]
fn scalar_derivative_reads_follow_declaration_ordinals_and_preflight_whole_batches() {
    let dae = super::compile(
        "model TwoStates Real x[2](start = {1, 2}, each fixed = true); \
         Real y(start = 3, fixed = true); \
         equation der(x[1]) = -x[1]; der(x[2]) = -2 * x[2]; der(y) = -3 * y; end TwoStates;",
        "TwoStates",
    );
    let component = crate::lower_fmi_export_component(&dae).unwrap();
    let states: Vec<&str> = component
        .state_variable_indices()
        .iter()
        .map(|&index| component.variables()[index].name())
        .collect();
    assert_eq!(
        states,
        ["x", "y"],
        "declaration ordinals the template numbers"
    );
    let base = component.derivative_value_reference_base_fmi3();
    let (der_x, der_y, x) = (base, base + 1, value_reference(&component, "x"));

    let config = MeInstanceConfig::new("scalar", 1.0e-6, 0.0, 1.0).unwrap();
    let mut retained =
        MeRetainedComponent::instantiate(MeModelSource::new(&component), &config, None).unwrap();
    let mut before = [7.0; 2];
    assert!(
        retained.get_scalar_float64(&[der_x], &mut before).is_err(),
        "derivatives are not host-written cells"
    );
    assert_eq!(before, [7.0; 2]);
    retained.enter_initialization_mode(&config).unwrap();
    let options = retained.initialization_options().unwrap();
    let mut session = retained
        .complete_scalar_initialization_with_integrator_retaining(options, |plan, setup| {
            assert_eq!(*plan, rumoca_ir_solve::fmi::CoSimulationStepPlan::STANDARD);
            Ok(rumoca_solver_rk45::model_exchange_integrator(setup))
        })
        .map_err(|(_, error)| error)
        .unwrap();

    let mut mixed = [0.0; 5];
    session
        .get_scalar_float64(&[der_y, x, der_x], &mut mixed)
        .unwrap();
    assert_eq!(mixed, [-9.0, 1.0, 2.0, -1.0, -4.0]);

    let mut repeated = [0.0; 4];
    session
        .get_scalar_float64(&[der_x, der_x], &mut repeated)
        .unwrap();
    assert_eq!(repeated, [-1.0, -4.0, -1.0, -4.0]);

    let mut unknown = [7.0; 3];
    assert!(
        session
            .get_scalar_float64(&[der_x, u32::MAX], &mut unknown)
            .is_err()
    );
    assert_eq!(
        unknown, [7.0; 3],
        "a refused batch leaves the output untouched"
    );
    let mut short = [7.0; 1];
    assert!(session.get_scalar_float64(&[der_x], &mut short).is_err());
    assert_eq!(short, [7.0]);
    assert!(
        session.set_scalar_float64(&[der_x], &[0.0, 0.0]).is_err(),
        "derivatives are never writable"
    );
}

/// Integer and Boolean variables answer through the same request owner as
/// Float64 ones: each by its own type only, before initialization from the
/// host-written cells and after it from one observation, with writes refused
/// outside the exact Int64 cell range or the lifecycle's causality window and
/// nothing written by a refused batch.
#[test]
fn integer_and_boolean_values_read_and_write_through_the_scalar_route() {
    let dae = super::compile(
        "model Selector parameter Integer n = 3; input Boolean enable = true; \
         output Boolean active; Real x(start = 1, fixed = true); \
         equation der(x) = if enable then -n * x else 0; active = x > 0.5; end Selector;",
        "Selector",
    );
    let component =
        crate::lower_fmi_scalar_export_component(&dae, &rumoca_solver::SimOptions::default())
            .unwrap();
    let vr = |name| value_reference(&component, name);
    let (n, enable, active) = (vr("n"), vr("enable"), vr("active"));
    let config = MeInstanceConfig::new("typed", 1.0e-6, 0.0, 1.0).unwrap();
    let mut retained =
        MeRetainedComponent::instantiate(MeModelSource::new(&component), &config, None).unwrap();

    let mut integer = [0_i64];
    retained.get_scalar_integer(&[n], &mut integer).unwrap();
    assert_eq!(integer, [3]);
    let mut boolean = [false];
    retained
        .get_scalar_boolean(&[enable], &mut boolean)
        .unwrap();
    assert_eq!(boolean, [true]);
    assert!(
        retained.get_scalar_boolean(&[n], &mut boolean).is_err(),
        "an Integer is not read as a Boolean"
    );
    let mut real = [7.0];
    assert!(
        retained.get_scalar_float64(&[n], &mut real).is_err(),
        "an Integer is not read as a Float64"
    );
    assert_eq!(real, [7.0]);
    assert!(retained.set_scalar_integer(&[n], &[(1 << 53) + 1]).is_err());
    retained.get_scalar_integer(&[n], &mut integer).unwrap();
    assert_eq!(integer, [3], "a refused write changes nothing");

    retained.enter_initialization_mode(&config).unwrap();
    let options = retained.initialization_options().unwrap();
    let mut session = retained
        .complete_scalar_initialization_with_integrator_retaining(options, |_, setup| {
            Ok(rumoca_solver_rk45::model_exchange_integrator(setup))
        })
        .map_err(|(_, error)| error)
        .unwrap();

    let mut typed = [false; 2];
    session
        .get_scalar_boolean(&[enable, active], &mut typed)
        .unwrap();
    assert_eq!(typed, [true, true]);
    session.get_scalar_integer(&[n], &mut integer).unwrap();
    assert_eq!(integer, [3]);
    assert!(
        session.set_scalar_integer(&[n], &[4]).is_err(),
        "parameters are not writable after initialization"
    );
    assert!(
        session.set_scalar_boolean(&[active], &[false]).is_err(),
        "outputs are never writable"
    );
    session.set_scalar_boolean(&[enable], &[false]).unwrap();
    let mut written = [true];
    session.get_scalar_boolean(&[enable], &mut written).unwrap();
    assert_eq!(written, [false]);
}

/// An enumeration answers the Int64 accessors with its literal's ordinal and
/// accepts only its literals' ordinals; a String parameter answers the String
/// accessors with its literal start until a host installs another text. Both
/// are writable only in the parameter window, Reset returns both to their
/// starts, and the installed ordinal is the one the dynamics read.
#[test]
fn enumeration_and_string_values_read_and_write_through_the_scalar_route() {
    let dae = super::compile(
        "model Labelled type Mode = enumeration(off, slow, fast); \
         parameter Mode mode = Mode.slow; parameter String label = \"a\"; \
         Real x(start = 1, fixed = true); \
         equation der(x) = if mode == Mode.fast then -2 * x else -x; end Labelled;",
        "Labelled",
    );
    let component =
        crate::lower_fmi_scalar_export_component(&dae, &rumoca_solver::SimOptions::default())
            .unwrap();
    let (mode, label) = (
        value_reference(&component, "mode"),
        value_reference(&component, "label"),
    );
    let der_x = component.derivative_value_reference_base_fmi3();
    let config = MeInstanceConfig::new("labelled", 1.0e-6, 0.0, 1.0).unwrap();
    let mut retained =
        MeRetainedComponent::instantiate(MeModelSource::new(&component), &config, None).unwrap();

    let mut ordinal = [0_i64];
    retained.get_scalar_integer(&[mode], &mut ordinal).unwrap();
    assert_eq!(ordinal, [2], "Mode.slow is the second literal");
    let mut boolean = [false];
    assert!(
        retained.get_scalar_boolean(&[mode], &mut boolean).is_err(),
        "an enumeration is not read as a Boolean"
    );
    let mut real = [7.0];
    assert!(
        retained.get_scalar_float64(&[mode], &mut real).is_err(),
        "an enumeration is not read as a Float64"
    );
    assert_eq!(real, [7.0]);
    for outside in [0, 4] {
        assert!(
            retained.set_scalar_integer(&[mode], &[outside]).is_err(),
            "{outside} is not an ordinal of Mode"
        );
    }
    retained.get_scalar_integer(&[mode], &mut ordinal).unwrap();
    assert_eq!(ordinal, [2], "a refused write changes nothing");

    let mut text = [String::from("unread")];
    retained.get_scalar_string(&[label], &mut text).unwrap();
    assert_eq!(text, ["a"]);
    assert!(
        retained.get_scalar_string(&[mode], &mut text).is_err(),
        "an enumeration is not read as a String"
    );
    assert!(
        retained.get_scalar_integer(&[label], &mut ordinal).is_err(),
        "a String is not read as an Int64"
    );
    assert!(
        retained
            .set_scalar_string(&[label], &["b".into(), "c".into()])
            .is_err(),
        "a String batch must cover its scalars exactly"
    );
    retained.get_scalar_string(&[label], &mut text).unwrap();
    assert_eq!(text, ["a"], "a refused write installs nothing");

    retained.set_scalar_integer(&[mode], &[3]).unwrap();
    retained.set_scalar_string(&[label], &["b".into()]).unwrap();
    retained.reset().unwrap();
    retained.get_scalar_integer(&[mode], &mut ordinal).unwrap();
    retained.get_scalar_string(&[label], &mut text).unwrap();
    assert_eq!(
        (ordinal, &text),
        ([2], &["a".to_owned()]),
        "Reset restores the starts"
    );

    retained.set_scalar_integer(&[mode], &[3]).unwrap();
    retained.set_scalar_string(&[label], &["b".into()]).unwrap();
    retained.enter_initialization_mode(&config).unwrap();
    let options = retained.initialization_options().unwrap();
    let mut session = retained
        .complete_scalar_initialization_with_integrator_retaining(options, |_, setup| {
            Ok(rumoca_solver_rk45::model_exchange_integrator(setup))
        })
        .map_err(|(_, error)| error)
        .unwrap();

    session.get_scalar_integer(&[mode], &mut ordinal).unwrap();
    assert_eq!(ordinal, [3]);
    let mut derivative = [0.0];
    session
        .get_scalar_float64(&[der_x], &mut derivative)
        .unwrap();
    assert_eq!(derivative, [-2.0], "the dynamics read Mode.fast");
    session.get_scalar_string(&[label], &mut text).unwrap();
    assert_eq!(text, ["b"]);
    assert!(
        session.set_scalar_integer(&[mode], &[1]).is_err(),
        "parameters are not writable after initialization"
    );
    assert!(
        session.set_scalar_string(&[label], &["c".into()]).is_err(),
        "String parameters are not writable after initialization"
    );
    session.get_scalar_string(&[label], &mut text).unwrap();
    assert_eq!(text, ["b"]);
}
