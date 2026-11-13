//! A component's Co-Simulation step plan comes from the host's options
//! through the backend that owns them, travels with the component's wire,
//! and reaches the plugin factory of the instance's session, whose absolute
//! tolerance is the plan's.

use rumoca_eval_solve::fmi_wire::deserialize_fmi_component;
use rumoca_ir_solve::fmi::{CoSimulationMethod, CoSimulationStepPlan, CoSimulationSubstep};
use rumoca_solver::fmi_me::{MeInstanceConfig, MeModelSource, MeRetainedComponent};
use rumoca_solver::{SimOptions, SimSolverMode};

fn requested(json: &str) -> SimOptions {
    let options = crate::interactive_options::InteractiveOptions::parse(json).unwrap();
    let base = SimOptions {
        solver_mode: SimSolverMode::parse_request(Some(&options.solver)).0,
        ..SimOptions::default()
    };
    options.configure(base, Vec::new())
}

const PAGE: &str = r#"{"solver":"fixed-step","fixed_step":{"step":0.001,"order":2},
    "atol":1e-8,"rtol":1e-6,"execution_policy":"auto"}"#;

#[test]
fn page_options_map_onto_one_fixed_plan_and_other_requests_are_refused() {
    let plan = crate::co_simulation_plan(&requested(PAGE)).unwrap();
    assert_eq!(
        plan.method(),
        CoSimulationMethod::ExplicitRungeKutta { order: 2 }
    );
    assert_eq!(plan.substep(), CoSimulationSubstep::Fixed { step: 0.001 });
    assert_eq!(plan.default_tolerance(), 1.0e-6);
    assert_eq!(plan.absolute_tolerance(), Some(1.0e-8));

    let semi = r#"{"solver":"fixed-step","atol":1e-8,"rtol":1e-6,
        "fixed_step":{"step":0.001,"order":1,"scheme":"semi-implicit"}}"#;
    let semi = crate::co_simulation_plan(&requested(semi)).unwrap();
    assert_eq!(semi.method(), CoSimulationMethod::SemiImplicitEuler);

    let semi_heun = r#"{"solver":"fixed-step","atol":1e-8,"rtol":1e-6,
        "fixed_step":{"step":0.001,"order":2,"scheme":"semi-implicit"}}"#;
    assert!(matches!(
        crate::co_simulation_plan(&requested(semi_heun)),
        Err(crate::StepPlanRefusal::Backend(_))
    ));
    for refused in [
        r#"{"solver":"bdf","atol":1e-6,"rtol":1e-6}"#,
        r#"{"solver":"auto","atol":1e-6,"rtol":1e-6}"#,
        r#"{"solver":"rk45","atol":1e-8,"rtol":1e-6}"#,
    ] {
        assert!(
            matches!(
                crate::co_simulation_plan(&requested(refused)),
                Err(crate::StepPlanRefusal::Unsupported(_))
            ),
            "{refused}"
        );
    }
    let standard = r#"{"solver":"rk45","atol":1e-6,"rtol":1e-6}"#;
    assert_eq!(
        crate::co_simulation_plan(&requested(standard)).unwrap(),
        CoSimulationStepPlan::STANDARD
    );
}

#[test]
fn a_fixed_plan_travels_with_the_wire_and_reaches_the_session_plugin() {
    let dae = super::compile(
        "model Decay Real x(start = 1, fixed = true); equation der(x) = -x; end Decay;",
        "Decay",
    );
    let opts = requested(PAGE);
    let plan = crate::co_simulation_plan(&opts).unwrap();
    let standard = crate::lower_fmi_scalar_export_component(&dae, &opts).unwrap();
    let standard_wire = serde_json::to_value(
        crate::retained_fmi_component_wire(&standard.runtime_view().retain_assertion_root())
            .unwrap(),
    )
    .unwrap();
    assert!(
        standard_wire.get("co_simulation").is_none(),
        "a standard component's wire is unchanged"
    );
    let mut explicit = standard_wire.clone();
    explicit["co_simulation"] = serde_json::json!(CoSimulationStepPlan::STANDARD);
    assert!(
        deserialize_fmi_component(explicit).is_err(),
        "the standard plan is written by omission"
    );

    let component = standard.with_co_simulation(plan);
    let wire = serde_json::to_value(
        crate::retained_fmi_component_wire(&component.runtime_view().retain_assertion_root())
            .unwrap(),
    )
    .unwrap();
    assert_eq!(wire["co_simulation"], serde_json::json!(plan));
    let replayed = deserialize_fmi_component(wire).unwrap();
    assert_eq!(*replayed.co_simulation(), plan);

    let config = MeInstanceConfig::new("fixed-plan", plan.default_tolerance(), 0.0, 1.0).unwrap();
    let mut retained =
        MeRetainedComponent::instantiate(MeModelSource::new(&replayed), &config, None).unwrap();
    retained.enter_initialization_mode(&config).unwrap();
    let options = retained.initialization_options().unwrap();
    assert_eq!(options.relative_tolerance(), 1.0e-6);
    assert_eq!(options.absolute_tolerance(), 1.0e-8);
    let mut received = None;
    retained
        .complete_scalar_initialization_with_integrator_retaining(options, |plan, setup| {
            received = Some(*plan);
            let config = rumoca_solver_rk45::FixedStepConfig {
                step: 0.001,
                order: 2,
                scheme: rumoca_solver_rk45::FixedStepScheme::Explicit,
            };
            Ok(rumoca_solver_rk45::fixed_step_integrator(setup, config).unwrap())
        })
        .map_err(|(_, error)| error)
        .unwrap();
    assert_eq!(received, Some(plan));
}
