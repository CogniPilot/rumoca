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
        .complete_scalar_initialization_with_integrator_retaining(
            options,
            rumoca_solver_rk45::model_exchange_integrator,
        )
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
