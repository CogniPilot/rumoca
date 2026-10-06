//! Image-sized native families must not pay for unused scalar fallbacks.

use super::SolveTemplateRenderer;
use super::codegen_test_support::builtin_template;
use rumoca_ir_solve as solve;
use std::time::Instant;

fn image_family(count: usize) -> solve::SolveProblem {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "pixel".into(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("RgbImage.mo"),
        1,
        2,
    );
    let mut problem = solve::SolveProblem::default();
    problem
        .continuous
        .derivative_rhs
        .nodes
        .push(solve::ComputeNode::AffineStencil {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain,
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadY { dst: 1, index: 1 },
                solve::LinearOp::LoadY { dst: 2, index: 2 },
                solve::LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Add,
                    lhs: 3,
                    rhs: 2,
                },
                solve::LinearOp::Const { dst: 5, value: 3. },
                solve::LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Div,
                    lhs: 4,
                    rhs: 5,
                },
                solve::LinearOp::StoreOutput { src: 6 },
            ],
            load_strides: (0..3)
                .map(|op_position| solve::AffineStencilLoadStride {
                    op_position,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 3,
                    }],
                })
                .collect(),
            const_strides: vec![],
            metadata: Default::default(),
            span,
        });
    problem
}

#[test]
fn image_sized_native_wgsl_is_extent_independent() {
    for count in [1024, 160 * 90, 320 * 180] {
        let problem = image_family(count);
        let start = Instant::now();
        let renderer =
            SolveTemplateRenderer::new(&problem, &solve::SolveArtifacts::default(), "RgbImage")
                .unwrap();
        let prepared = start.elapsed();
        let context = super::solve_render_context_value(
            &problem,
            &solve::SolveArtifacts::default(),
            Some("RgbImage"),
        )
        .unwrap();
        let legacy = context
            .get_attr("solve_blocks")
            .unwrap()
            .get_attr("continuous")
            .unwrap()
            .get_attr("derivative_rhs")
            .unwrap()
            .get_attr("scalar_programs")
            .unwrap();
        let projection = legacy
            .downcast_object_ref::<super::lazy_scalar_projection::LazyScalarProjection>()
            .unwrap();
        assert!(!projection.initialized());
        let shader = renderer
            .render(builtin_template("wgsl-ode", "model_ode.wgsl.jinja"))
            .unwrap();
        let mut env = super::create_environment();
        env.add_template(
            "native",
            builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        )
        .unwrap();
        assert_eq!(
            env.get_template("native").unwrap().render(context).unwrap(),
            shader
        );
        assert!(
            !projection.initialized(),
            "native WGSL consumed a scalar fallback"
        );
        let module = naga::front::wgsl::parse_str(&shader).unwrap();
        naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::empty(),
        )
        .validate(&module)
        .unwrap();
        assert_eq!(module.entry_points.len(), 1);
        assert!(
            shader.len() < 4000,
            "native image shader grew with image extent"
        );
        eprintln!(
            "native_image pixels={count} context_us={} total_us={} shader_bytes={}",
            prepared.as_micros(),
            start.elapsed().as_micros(),
            shader.len()
        );
    }
}

#[test]
fn scalar_fallback_is_lazy_shared_and_matches_checked_projection() {
    let problem = image_family(12);
    let block =
        super::solve_template_compute_block_json(&problem.continuous.derivative_rhs).unwrap();
    let legacy = block.get_attr("scalar_programs").unwrap();
    let projection = legacy
        .downcast_object_ref::<super::lazy_scalar_projection::LazyScalarProjection>()
        .unwrap();
    assert!(!projection.initialized());
    assert_eq!(
        block.get_attr("stencil_family_count").unwrap().as_usize(),
        Some(1)
    );
    assert!(!projection.initialized());
    let expected =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs).unwrap();
    let programs = legacy.get_attr("programs").unwrap();
    assert!(projection.initialized());
    let actual = programs
        .downcast_object_ref::<super::solve_lazy::SolveProgramsObject>()
        .unwrap();
    assert_eq!(
        serde_json::to_value(actual.block.as_ref()).unwrap(),
        serde_json::to_value(&expected).unwrap()
    );
    let plan = block.get_attr("scalar_plan").unwrap();
    assert_eq!(plan.get_attr("output_count").unwrap().as_usize(), Some(12));
    let again = legacy.get_attr("programs").unwrap();
    let cached = again
        .downcast_object_ref::<super::solve_lazy::SolveProgramsObject>()
        .unwrap();
    assert!(std::sync::Arc::ptr_eq(&actual.block, &cached.block));
}

#[test]
fn invalid_scalar_fallback_propagates_error_on_demand() {
    let mut problem = image_family(2);
    let solve::ComputeNode::AffineStencil { load_strides, .. } =
        &mut problem.continuous.derivative_rhs.nodes[0]
    else {
        unreachable!()
    };
    load_strides[0].op_position = 5;
    let (legacy, plan) = super::lazy_scalar_projection::views(std::sync::Arc::new(
        problem.continuous.derivative_rhs,
    ));
    let mut env = super::create_environment();
    for (template, value) in [
        ("{{ block.programs|length }}", legacy),
        ("{{ block.output_count }}", plan),
    ] {
        env.add_template("bad", template).unwrap();
        let error = env
            .get_template("bad")
            .unwrap()
            .render(minijinja::context! { block => value })
            .unwrap_err();
        assert!(
            error.to_string().contains("load stride at op 5"),
            "unexpected scalarization error: {error}"
        );
    }
}
