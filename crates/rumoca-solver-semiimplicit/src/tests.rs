use super::*;
use rumoca_solver::fmi_me::ode_driver::{ContinuousOde, OdeDriveError, OdeRun, integrate_ode};
use std::rc::Rc;

fn setup() -> MeNumericalSetup {
    MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0], 1, None).unwrap()
}

#[test]
fn a_step_outside_the_fixed_grid_bound_is_a_typed_construction_refusal() {
    for step in [0.0, -0.001, f64::NAN, f64::INFINITY, 0.0051] {
        let Err(error) = model_exchange_integrator_with_step(setup(), step) else {
            panic!("step {step} must be refused");
        };
        assert!(
            matches!(
                error,
                MeIntegrationError::Numerical {
                    method: METHOD,
                    category: MeNumericalFailure::Construction,
                    ref detail,
                } if detail.contains("step must be finite and in (0, 0.005]")
            ),
            "step {step}: {error}"
        );
    }
    assert!(model_exchange_integrator_with_step(setup(), STEP).is_ok());
    assert!(model_exchange_integrator_observed(setup(), 0.0).is_err());
}

/// `y' = -y`: an ordinary system with no Cartesian position/velocity roles.
struct Decay;

impl ContinuousOde for Decay {
    fn width(&self) -> usize {
        1
    }

    fn derivatives_into(&self, _time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        out[0] = -state[0];
        Ok(())
    }
}

fn run(extension_order_limit: Option<u32>) -> OdeRun {
    OdeRun {
        relative_tolerance: 1.0e-6,
        absolute_tolerance: 1.0e-9,
        nominals: vec![1.0],
        initial_step: None,
        t_start: 0.0,
        t_end: 0.01,
        initial_state: vec![1.0],
        breakpoints: Vec::new(),
        extension_order_limit,
    }
}

#[test]
fn the_driver_admits_the_linear_extension_and_then_meets_the_partition_refusal() {
    let mut steps = 0;
    let refused = integrate_ode(
        Rc::new(Decay),
        &run(Some(0)),
        &model_exchange_integrator,
        |_| {
            steps += 1;
            Ok(())
        },
    );
    assert!(matches!(
        refused,
        Err(OdeDriveError::ExtensionOrder {
            declared: Some(1),
            limit: 0
        })
    ));
    let error = integrate_ode(
        Rc::new(Decay),
        &run(Some(1)),
        &model_exchange_integrator,
        |_| {
            steps += 1;
            Ok(())
        },
    )
    .expect_err("an ordinary system has no checked Cartesian partition");
    assert!(
        matches!(
            error,
            OdeDriveError::Integration(MeIntegrationError::Numerical {
                method: "semi-implicit Euler",
                category: MeNumericalFailure::Construction,
                ref detail,
            }) if detail.contains("no checked complete Cartesian position/velocity partition")
        ),
        "{error}"
    );
    assert_eq!(steps, 0, "neither run accepts a step");
}
