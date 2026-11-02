//! Synthetic checked-backend fixtures, not Modelica lowering or FMI API proof.

use super::{
    SolveTemplateRenderer, artifact_identities, builtin_template, fixture_span,
    model_with_one_state_run, state_input,
};
use rumoca_ir_solve as solve;
use solve::fmi::{FmiCCodegenView, FmiCausality, FmiVariability, FmiVariableInput};
use solve::{BinaryOp, LinearOp, ScalarProgramBlock, ScalarSlot};

fn p(index: usize) -> ScalarSlot {
    ScalarSlot::P {
        index,
        byte_offset: index * 8,
    }
}

fn y(index: usize) -> ScalarSlot {
    ScalarSlot::Y {
        index,
        byte_offset: index * 8,
    }
}

fn parameter_input(name: &str, value: f64, tunable: bool) -> FmiVariableInput {
    FmiVariableInput {
        name: name.into(),
        role: solve::SolveVariableStorageRole::Parameter,
        dimensions: Vec::new(),
        start: vec![value].into(),
        causality: FmiCausality::Parameter,
        variability: if tunable {
            FmiVariability::Tunable
        } else {
            FmiVariability::Fixed
        },
        tunable,
        ..state_input()
    }
}

const CASES: [(bool, bool); 4] = [(false, false), (false, true), (true, false), (true, true)];

fn binding_block(compact: bool, permuted: bool) -> ScalarProgramBlock {
    // Listed first but dependent on the later-listed program: h = 2*k.
    let later = vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    // One complete source program computes (k, x[1], x[2]) = (3*g, g, 2*g).
    let mut mixed = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::LoadP { dst: 3, index: 0 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 0,
        },
    ];
    if compact {
        mixed.push(LinearOp::StoreOutputRange {
            start: 2,
            count: 3,
            stride: 1,
        });
    } else {
        mixed.extend((2..5).map(|src| LinearOp::StoreOutput { src }));
    }
    // Both the contiguous compact fast path and a non-affine placement are
    // exercised. The later-listed program still owns the dependent k value.
    let outputs = if permuted {
        vec![3, 2, 0, 1]
    } else {
        vec![3, 0, 1, 2]
    };
    ScalarProgramBlock::with_output_indices(vec![later, mixed], vec![fixture_span(); 2], outputs)
        .expect("the synthetic source has checked operations, provenance and placements")
}

fn component(compact: bool, permuted: bool) -> FmiCCodegenView {
    component_from_binding(binding_block(compact, permuted), permuted)
}

fn component_from_binding(block: ScalarProgramBlock, permuted: bool) -> FmiCCodegenView {
    let mut model = model_with_one_state_run(false);
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 2, 3);
    model.initial_y = vec![2.0, 4.0].into();
    model.parameters = vec![2.0, 6.0, 12.0].into();
    for index in 0..3 {
        model
            .problem
            .solve_layout
            .variable_storage_runs
            .push(solve::SolveVariableStorageRun {
                base: p(index),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::Parameter,
                value_kind: solve::SolveVariableValueKind::Real,
            });
        model.problem.solve_layout.variable_declarations.push(
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::Parameter,
                solve::SolveVariableValueKind::Real,
            ),
        );
    }
    model.problem.solve_layout.parameter_count = 3;
    model.problem.solve_layout.compiled_parameter_len = 3;
    let rhs = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span()
            .require_provenance("synthetic binding derivative fixture")
            .unwrap(),
    )
    .unwrap();
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(rhs);
    let mut initialization = model.problem.initialization.clone().into_input();
    initialization.update_rhs = block;
    initialization.update_targets = if permuted {
        vec![y(0), y(1), p(1), p(2)]
    } else {
        vec![p(1), y(0), y(1), p(2)]
    };
    model.problem.initialization = solve::InitializationSolveSystem::construct(initialization)
        .expect("the mixed fixture has disjoint checked initialization targets");
    let mut state = state_input();
    state.start = vec![2.0, 4.0].into();
    solve::fmi::FmiComponent::construct(
        model,
        vec![
            state,
            parameter_input("g", 2.0, true),
            parameter_input("k", 6.0, false),
            parameter_input("h", 12.0, false),
        ],
    )
    .expect("the synthetic model and declaration inventory form one new checked component")
    .into_codegen_view()
    .try_c()
    .expect("the actual C admission proves the dependency order and storage roles")
}

#[test]
fn binding_projection_reaches_the_actual_lazy_renderer_in_admitted_order() {
    for (compact, permuted) in CASES {
        let component = component(compact, permuted);
        assert_eq!(component.parameter_binding_programs(), [1, 0]);
        assert!(
            component
                .parameter_binding_source()
                .shares_program_owner(component.problem().initialization.update_rhs())
        );
        let renderer = SolveTemplateRenderer::new_owned_with_fmi(component)
            .expect("the same component supplies the actual lazy FMI context");
        let actual = renderer
            .render(concat!(
                "{% for p in fmi.binding_execution %}",
                "{{ p.program_index }}:P{{ p.parameter_rows|join(',') }}",
                ":Y{{ p.state_start_rows|join(',') }};{% endfor %}",
            ))
            .expect("the actual lazy consumer receives constructor-issued rows");
        assert_eq!(
            actual,
            if permuted {
                "1:P2:Y0,1;0:P3:Y;"
            } else {
                "1:P0:Y1,2;0:P3:Y;"
            }
        );
    }
}

#[test]
fn mixed_scalar_and_compact_post_relations_keep_whole_programs_without_state_constraints() {
    use rumoca_eval_solve::fmi_tangents::{FmiTangentMode, FmiTangentPlan};
    for (compact, permuted) in CASES {
        let component = component(compact, permuted);
        let plan = FmiTangentPlan::derive_event_or_step(&component)
            .expect("the actual post-initial consumer accepts the same mixed owner");
        assert_eq!(plan.mode(), FmiTangentMode::EventOrStep);
        assert!(std::ptr::eq(plan.component(), &component));
        let system = plan
            .implicit()
            .expect("k and h remain dependent parameter values");
        assert_eq!(system.targets(), [3, 4]);
        assert_eq!(system.subtract_targets(), [4, 5]);
        assert_eq!(system.rows().len(), 2);
        assert_eq!(
            system.program().output_count(),
            4,
            "both state-start results remain in whole-program evaluation scratch"
        );
        let mut seed = vec![0.0, 0.0, 5.0, 15.0, 30.0, 0.0];
        let mut values = vec![f64::NAN; system.program().output_count()];
        let evaluate = |seed: &[f64], values: &mut [f64]| {
            rumoca_eval_solve::eval_scalar_program_block(
                system.program(),
                &[2.0, 4.0],
                &[4.0, 12.0, 24.0],
                0.0,
                Some(seed),
                values,
            )
            .expect("execute actual issued AD programs, without simulating or compiling a model");
        };
        evaluate(&seed, &mut values);
        assert_eq!(values, [15.0, 5.0, 10.0, 30.0]);
        for (&row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
            assert_eq!(values[row], seed[subtract - 1]);
        }
        // A frozen dependent parameter violates the actual issued relation.
        seed[3] = 0.0;
        evaluate(&seed, &mut values);
        assert!(
            system
                .rows()
                .iter()
                .zip(system.subtract_targets())
                .any(|(&row, &subtract)| values[row] != seed[subtract - 1])
        );
    }
}

#[test]
fn actual_built_in_c_templates_render_derivative_capabilities_from_complete_plans() {
    for (compact, permuted) in CASES {
        let renderer = SolveTemplateRenderer::new_owned_with_fmi(component(compact, permuted))
            .expect("the actual typed context is renderable");
        for target in ["fmi2", "fmi3"] {
            for name in ["model.c.jinja", "modelDescription.xml.jinja"] {
                let rendered = renderer
                    .render_with_name_and_artifact(
                        builtin_template(target, name),
                        "BindingExecutionFixture",
                        &artifact_identities(),
                    )
                    .expect("actual MiniJinja templates consume the same checked component");
                assert!(!rendered.trim().is_empty());
                check_derivative_capabilities(&rendered, target, name);
            }
        }
    }
}

#[test]
fn every_fmi_model_description_starts_with_its_xml_declaration() {
    let renderer = SolveTemplateRenderer::new_owned_with_fmi(component(false, false)).unwrap();
    let mut rendered_targets = Vec::new();
    for target in crate::templates::builtin_targets() {
        let Some(template) = target.template_source("modelDescription.xml.jinja") else {
            continue;
        };
        let xml = renderer
            .render_with_name_and_artifact(
                template,
                "BindingExecutionFixture",
                &artifact_identities(),
            )
            .expect("the built-in FMI description renders");
        assert!(
            xml.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
            "{} must put its XML declaration at byte zero: {:?}",
            target.name,
            &xml.as_bytes()[..xml.len().min(64)]
        );
        rendered_targets.push(target.name);
    }
    for target in ["fmi2", "fmi3", "fmi-ls-wasm"] {
        assert!(rendered_targets.contains(&target));
    }
}

#[test]
fn layered_wasm_metadata_does_not_inherit_c_derivative_capabilities() {
    let renderer = SolveTemplateRenderer::new_owned_with_fmi(component(false, false)).unwrap();
    let xml = renderer
        .render_with_name_and_artifact(
            builtin_template("fmi-ls-wasm", "modelDescription.xml.jinja"),
            "BindingExecutionFixture",
            &artifact_identities(),
        )
        .expect("the layered adapter metadata renders with its own capabilities");
    for flag in [
        "providesDirectionalDerivatives",
        "providesAdjointDerivatives",
    ] {
        assert!(!xml.contains(&format!("{flag}=\"true\"")));
    }
}

fn check_derivative_capabilities(rendered: &str, target: &str, name: &str) {
    if name != "modelDescription.xml.jinja" {
        return;
    }
    for flag in [
        "providesDirectionalDerivative",
        "providesDirectionalDerivatives",
        "providesAdjointDerivatives",
    ] {
        let expected = (target == "fmi2") == (flag == "providesDirectionalDerivative");
        assert_eq!(rendered.contains(&format!("{flag}=\"true\"")), expected);
    }
}

/// The dependent parameter still has a valid value, but another result in
/// that same source program performs a checked runtime read. After init its
/// value has no state-write authority; its source failure must still survive.
fn binding_with_failable_state_result(compact: bool, permuted: bool) -> ScalarProgramBlock {
    let original = binding_block(compact, permuted);
    let mut programs = original
        .programs()
        .iter()
        .map(|p| p.to_vec())
        .collect::<Vec<_>>();
    assert!(matches!(
        programs[1][3],
        LinearOp::LoadP { dst: 3, index: 0 }
    ));
    // At g=2 the source index g-1 is 1 and [2,4][1] is a legal start.
    // At g=4 that index is 3 and must refuse even though this Y row will
    // not be committed. Keep the original k and second state calculations.
    programs[1].splice(
        3..4,
        [
            LinearOp::Const { dst: 5, value: 2.0 },
            LinearOp::Const { dst: 6, value: 4.0 },
            LinearOp::Const { dst: 7, value: 1.0 },
            LinearOp::Binary {
                dst: 8,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 7,
            },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 5,
                stride: 1,
                dimensions: vec![2].into_boxed_slice(),
                indices: vec![solve::TensorIndex::Runtime(8)].into_boxed_slice(),
            },
        ],
    );
    ScalarProgramBlock::with_output_indices(
        programs,
        vec![fixture_span(); 2],
        original.output_indices().to_vec(),
    )
    .expect("the failable synthetic body retains the same exact output inventory")
}

#[test]
fn post_initial_relation_does_not_erase_failure_of_an_uncommitted_state_result() {
    use rumoca_eval_solve::{EvalSolveError, fmi_tangents::FmiTangentPlan};
    for (compact, permuted) in CASES {
        let component = component_from_binding(
            binding_with_failable_state_result(compact, permuted),
            permuted,
        );
        let plan = FmiTangentPlan::derive_event_or_step(&component)
            .expect("the supported checked read remains in the same post-initial owner");
        let system = plan.implicit().expect("the k/h relations remain present");
        assert_eq!(system.targets(), [3, 4]);
        assert_eq!(system.program().output_count(), 4);
        let seed = [0.0, 0.0, 5.0, 15.0, 30.0, 0.0];
        let mut out = vec![f64::NAN; system.program().output_count()];
        // A successful first point distinguishes this fault from malformed
        // fixture storage or an unrelated evaluator/seed inventory error.
        rumoca_eval_solve::eval_scalar_program_block(
            system.program(),
            &[2.0, 4.0],
            &[2.0, 6.0, 12.0],
            0.0,
            Some(&seed),
            &mut out,
        )
        .expect("the within-bounds point evaluates the complete relation");
        for (&row, &subtract) in system.rows().iter().zip(system.subtract_targets()) {
            assert_eq!(out[row], seed[subtract - 1]);
        }
        let error = rumoca_eval_solve::eval_scalar_program_block(
            system.program(),
            &[2.0, 4.0],
            &[4.0, 12.0, 24.0],
            0.0,
            Some(&seed),
            &mut out,
        )
        .expect_err("the unused state result must retain its checked read failure");
        assert!(matches!(
            error,
            EvalSolveError::TensorIndexOutOfBounds {
                axis: 1,
                index: 3,
                extent: 2,
                ..
            }
        ));
        // This evaluator writes private output scratch and is not a public
        // transaction boundary. No unchanged-output or FMI rollback is inferred.
    }
}
