//! Real source-to-component ownership checks for FMI total tangent preparation.
//! These do not substitute for generated-C/API execution or XML availability.

use rumoca::Compiler;
use rumoca_eval_solve::fmi_tangents::{FmiTangentMode, FmiTangentPlan, FmiTangentPlanError};
use rumoca_ir_solve::fmi::{FmiCCodegenView, FmiStorageColumn};

const INITIAL: &str = include_str!("../fixtures/fmi-partial-derivatives/InitialP.mo");
const POST: &str = "model PostP\nparameter Real gain=2;\nparameter Real k=3*gain;\ninput Real u=0;\nReal x(start=gain,fixed=true);\noutput Real y;\nequation\nder(x)=-k*x+u;\ny=k*x+u;\nend PostP;";
const TIME: &str = include_str!("../fixtures/fmi-partial-derivatives/TimeP.mo");
const OBSERVED: &str = "model ObservedInitial\nReal x(start=1,fixed=true);\nBoolean b;\nequation\nder(x)=-x;\nb=noEvent(x>0.5);\nend ObservedInitial;";

fn lower_component(name: &str, source: &str) -> rumoca_ir_solve::fmi::FmiCodegenView {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .expect("compile the exact source fixture");
    let lowered = rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &std::collections::HashMap::new(),
        rumoca_ir_dae::InputInitializationPolicy::FmiExportDefaults,
        rumoca_ir_solve::RefreshProfile::Scalar,
        |_| {},
    )
    .expect("lower the actual scalar FMI export component");
    rumoca_phase_solve::fmi::finish_fmi_component(lowered)
        .expect("retain the correlated source and executable owners")
        .into_codegen_view()
}

fn c_component(name: &str, source: &str) -> FmiCCodegenView {
    lower_component(name, source)
        .try_c()
        .expect("admit the actual C profile, without swallowing a refusal")
}

fn scalar_slot(component: &FmiCCodegenView, name: &str) -> usize {
    let variable = component
        .variables()
        .iter()
        .find(|variable| variable.name() == name)
        .expect("the fixture declaration survives in the checked inventory");
    assert_eq!(variable.scalar_count(), 1);
    let storage = variable
        .storage()
        .expect("the fixture has source-owned numeric storage");
    storage.base()
        + match storage.column() {
            FmiStorageColumn::Y => 0,
            FmiStorageColumn::P => component.problem().layout.y_scalars(),
        }
}

fn evaluate(
    program: &rumoca_ir_solve::ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    seed: &[f64],
) -> Vec<f64> {
    let mut result = vec![1234567.0; program.output_count()];
    rumoca_eval_solve::eval_scalar_program_block(program, y, p, 0.0, Some(seed), &mut result)
        .expect("execute the actual issued tangent program");
    result
}

#[test]
fn initial_calculated_parameter_keeps_its_solve_and_given_state_ownership() {
    let component = c_component("InitialP", INITIAL);
    let plan = FmiTangentPlan::derive_initialization(&component)
        .expect("derive the complete source-owned initialization tangent relation");
    assert!(std::ptr::eq(plan.component(), &component));
    assert_eq!(plan.mode(), FmiTangentMode::Initialization);
    let x = scalar_slot(&component, "x");
    let offset = scalar_slot(&component, "offset");
    let system = plan
        .implicit()
        .expect("the initial equation has a solved parameter");
    assert!(
        !system.targets().contains(&x),
        "the exact given state stays independent"
    );
    assert!(
        system.targets().contains(&offset),
        "calculated offset cannot be frozen"
    );
    let y_len = component.problem().layout.y_scalars();
    let (initial_y, initial_p) = component.instantiation_point();
    let (mut y, mut p) = (initial_y.to_vec(), initial_p.to_vec());
    let mut seed = vec![0.0; plan.seed_len()];
    // An analytic primal/tangent point, not an initialization solver or FMU
    // execution substitute: each position is read from the real metadata.
    for (name, value, tangent) in [
        ("gain", 2.0, 5.0),
        ("u", 0.0, 7.0),
        ("x", 1.0, 11.0),
        ("offset", 6.0, 22.0),
        ("y", 6.0, 95.0),
    ] {
        let slot = scalar_slot(&component, name);
        if slot < y_len {
            y[slot] = value;
        } else {
            p[slot - y_len] = value;
        }
        seed[slot] = tangent;
    }
    let residuals = evaluate(system.program(), &y, &p, &seed);
    for (row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
        let value = residuals[*row]
            - if subtract == 0 {
                0.0
            } else {
                seed[subtract - 1]
            };
        assert!(value.is_finite() && value.abs() < 1e-10);
    }
    if let Some(checks) = plan.consistency() {
        let values = evaluate(checks.program(), &y, &p, &seed);
        for &row in checks.rows() {
            assert!(values[row].is_finite() && values[row].abs() < 1e-10);
        }
    }
    let rhs = evaluate(plan.rhs(), &y, &p, &seed);
    assert_eq!(rhs.len(), 1);
    assert!((rhs[0] - 2.0).abs() < 1e-10);
    seed[offset] += 1.0;
    let wrong = evaluate(system.program(), &y, &p, &seed);
    assert!(
        system
            .rows()
            .iter()
            .zip(system.subtract_targets())
            .any(|(&row, &subtract)| {
                let value = wrong[row]
                    - if subtract == 0 {
                        0.0
                    } else {
                        seed[subtract - 1]
                    };
                value.abs() > 1e-10
            }),
        "a wrong calculated-parameter tangent cannot satisfy the issued relation"
    );
}

#[test]
fn scalar_event_initialization_is_not_relabelled_as_a_complete_initial_relation() {
    let component = c_component("ObservedInitial", OBSERVED);
    assert!(component.requires_event_iteration());
    let error = match FmiTangentPlan::derive_initialization(&component) {
        Ok(_) => panic!("event-initialization ownership must not be fabricated"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        FmiTangentPlanError::InitializationInventory(_)
    ));
    assert!(
        error
            .to_string()
            .contains("scalar-event initialization transactions")
    );
}

#[test]
fn optional_initialization_tangent_admission_preserves_both_primal_c_exports() {
    for (name, source) in [
        ("InitialP", INITIAL),
        ("ObservedInitial", OBSERVED),
        ("TimeP", TIME),
        ("PostP", POST),
    ] {
        let compiled = Compiler::new()
            .model(name)
            .compile_str(source, &format!("{name}.mo"))
            .expect("compile the actual source fixture");
        for target in ["fmi2", "fmi3"] {
            // Both an admitted initializer and a concrete derivative-owner
            // refusal must leave the actual primal C target renderable.
            let files = rumoca::render_target_files(&compiled, name, target, None)
                .expect("optional derivative preparation must not break primal export");
            let xml = &files
                .iter()
                .find(|file| file.path == "modelDescription.xml")
                .expect("metadata remains present")
                .content;
            for flag in [
                "providesDirectionalDerivative",
                "providesDirectionalDerivatives",
                "providesAdjointDerivatives",
            ] {
                let expected = name != "ObservedInitial"
                    && (target == "fmi3" || flag == "providesDirectionalDerivative")
                    && (target == "fmi2" || flag != "providesDirectionalDerivative");
                assert_eq!(
                    xml.contains(&format!("{flag}=\"true\"")),
                    expected,
                    "capability {flag} must match complete {name}/{target} preparation"
                );
            }
            assert!(files.iter().any(|file| file.path.ends_with("model.c")));
        }
    }
}

#[test]
fn parameter_only_admission_does_not_waive_declaration_y_isolation() {
    let source = "model DeclaredState\nReal x(start=0,fixed=false);\ninitial equation\nx=1;\nequation\nder(x)=-x;\nend DeclaredState;";
    let error = lower_component("DeclaredState", source)
        .try_c()
        .expect_err("a declaration-Y row still needs its missing isolated initializer owner");
    assert!(error.to_string().contains("declaration seeds"));
}

#[test]
fn post_initial_relation_propagates_parameter_bindings_without_resetting_state_starts() {
    let source = POST;
    let component = c_component("PostP", source);
    let plan = FmiTangentPlan::derive_event_or_step(&component)
        .expect("issue post-initial parameter/algebraic ownership");
    assert_eq!(plan.mode(), FmiTangentMode::EventOrStep);
    let x = scalar_slot(&component, "x");
    let k = scalar_slot(&component, "k");
    assert!(
        component
            .problem()
            .initialization
            .update_targets()
            .iter()
            .any(
                |slot| matches!(slot, rumoca_ir_solve::ScalarSlot::Y { index, .. } if *index == x)
            ),
        "the source actually owns a parameter-dependent initial-state assignment"
    );
    let system = plan
        .implicit()
        .expect("dependent parameter and output relation");
    assert!(system.targets().contains(&k));
    assert!(
        !system.targets().contains(&x),
        "post-initialization must not reset the initial state"
    );
    let y_len = component.problem().layout.y_scalars();
    let (initial_y, initial_p) = component.instantiation_point();
    let (mut y, mut p) = (initial_y.to_vec(), initial_p.to_vec());
    let mut seed = vec![0.0; plan.seed_len()];
    // An analytic post-initial point, not a runtime trajectory or mode proof.
    for (name, value, tangent) in [
        ("gain", 2.0, 5.0),
        ("k", 6.0, 15.0),
        ("u", 0.0, 7.0),
        ("x", 1.0, 0.0),
        ("y", 6.0, 22.0),
    ] {
        let slot = scalar_slot(&component, name);
        if slot < y_len {
            y[slot] = value;
        } else {
            p[slot - y_len] = value;
        }
        seed[slot] = tangent;
    }
    let values = evaluate(system.program(), &y, &p, &seed);
    for (&row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
        let residual = values[row]
            - if subtract == 0 {
                0.0
            } else {
                seed[subtract - 1]
            };
        assert!(residual.is_finite() && residual.abs() < 1e-10);
    }
    assert_eq!(evaluate(plan.rhs(), &y, &p, &seed), [-8.0]);
    seed[k] = 0.0;
    let values = evaluate(system.program(), &y, &p, &seed);
    assert!(
        system
            .rows()
            .iter()
            .zip(system.subtract_targets())
            .any(|(&row, &subtract)| {
                let residual = values[row]
                    - if subtract == 0 {
                        0.0
                    } else {
                        seed[subtract - 1]
                    };
                residual.abs() > 1e-10
            }),
        "freezing a dependent parameter cannot satisfy the post-initial relation"
    );
}

#[test]
fn post_initial_relation_does_not_resolve_an_initial_equation_again() {
    let component = c_component("InitialP", INITIAL);
    let plan = FmiTangentPlan::derive_event_or_step(&component)
        .expect("initial-equation results are held after initialization");
    let offset = scalar_slot(&component, "offset");
    let system = plan
        .implicit()
        .expect("the public output still has an algebraic owner");
    assert!(
        !system.targets().contains(&offset),
        "a solved initial parameter is not a post-initial binding"
    );
    let y_len = component.problem().layout.y_scalars();
    let (initial_y, initial_p) = component.instantiation_point();
    let (mut y, mut p) = (initial_y.to_vec(), initial_p.to_vec());
    let mut seed = vec![0.0; plan.seed_len()];
    for (name, value, tangent) in [
        ("gain", 2.0, 5.0),
        ("u", 0.0, 7.0),
        ("x", 1.0, 0.0),
        ("offset", 6.0, 0.0),
        ("y", 6.0, 7.0),
    ] {
        let slot = scalar_slot(&component, name);
        if slot < y_len {
            y[slot] = value;
        } else {
            p[slot - y_len] = value;
        }
        seed[slot] = tangent;
    }
    let values = evaluate(system.program(), &y, &p, &seed);
    for (&row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
        let residual = values[row]
            - if subtract == 0 {
                0.0
            } else {
                seed[subtract - 1]
            };
        assert!(residual.is_finite() && residual.abs() < 1e-10);
    }
    assert_eq!(evaluate(plan.rhs(), &y, &p, &seed), [2.0]);
}

#[test]
fn actual_component_time_direction_remains_distinct_from_state_and_input_storage() {
    let component = c_component("TimeP", TIME);
    let plan = FmiTangentPlan::derive(&component)
        .expect("derive the same component's time-aware total programs");
    let time = plan.time_seed_index();
    let y_len = component.problem().layout.y_scalars();
    assert_eq!(time, y_len + component.problem().layout.p_scalars());
    assert_eq!(plan.seed_len(), time + 1);
    let x = scalar_slot(&component, "x");
    let u = scalar_slot(&component, "u");
    let output = scalar_slot(&component, "y");
    assert!(x < time && u < time && output < time);
    let (initial_y, initial_p) = component.instantiation_point();
    let (mut y, mut p) = (initial_y.to_vec(), initial_p.to_vec());
    y[x] = 1.0;
    if u < y_len {
        y[u] = 0.0;
    } else {
        p[u - y_len] = 0.0;
    }
    if output < y_len {
        y[output] = 8.0;
    } else {
        p[output - y_len] = 8.0;
    }
    let mut seed = vec![0.0; plan.seed_len()];
    seed[x] = 7.0;
    seed[u] = 11.0;
    seed[time] = 5.0;
    for segment in plan.assignments() {
        let mut values = vec![0.0; segment.program().output_count()];
        rumoca_eval_solve::eval_scalar_program_block(
            segment.program(),
            &y,
            &p,
            2.0,
            Some(&seed),
            &mut values,
        )
        .unwrap();
        for (&target, value) in segment.targets().iter().zip(values) {
            seed[target] = value;
        }
    }
    if let Some(system) = plan.implicit() {
        // This analytic solved column checks the issued relation; it does not
        // execute or stand in for a generated-C implicit solve.
        seed[output] = 29.0;
        let mut values = vec![0.0; system.program().output_count()];
        rumoca_eval_solve::eval_scalar_program_block(
            system.program(),
            &y,
            &p,
            2.0,
            Some(&seed),
            &mut values,
        )
        .unwrap();
        for (&row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
            let residual = values[row]
                - if subtract == 0 {
                    0.0
                } else {
                    seed[subtract - 1]
                };
            assert!(residual.is_finite() && residual.abs() < 1e-10);
        }
        assert!(
            !system.targets().contains(&time),
            "time cannot become a solved storage cell"
        );
    }
    assert_eq!(seed[output], 29.0);
    let mut rhs = vec![0.0; plan.rhs().output_count()];
    rumoca_eval_solve::eval_scalar_program_block(plan.rhs(), &y, &p, 2.0, Some(&seed), &mut rhs)
        .unwrap();
    assert_eq!(rhs, [30.0]);
}
