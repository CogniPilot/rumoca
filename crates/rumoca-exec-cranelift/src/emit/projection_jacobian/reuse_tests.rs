use super::*;
use crate::emit::host_runtime::take_sin_calls;
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ContinuousStructuralArtifacts,
    PatternDerivation, PatternProvenance, ScalarProgramBlock, StructuralPattern, UnaryOp,
};

fn application(source: &ScalarProgramBlock) -> ProjectionJacobianApplication {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("reuse.mo"), 0, 1);
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span).unwrap();
    let pattern =
        StructuralPattern::from_row_dependencies(2, 2, &[vec![0, 1], vec![0, 1]], provenance)
            .unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0, 1],
            tearing: None,
        }],
    };
    let artifacts = ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern],
        vec![false],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, source, source, source);
    artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap()
        .clone()
}

#[test]
fn projection_reuses_primal_work_only_within_one_call() {
    use LinearOp as L;
    let row = vec![
        L::LoadY { dst: 0, index: 0 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadTime { dst: 2 },
        L::Binary {
            dst: 0,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 0,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 2,
        },
        L::Unary {
            dst: 3,
            op: UnaryOp::Sin,
            arg: 0,
        },
        L::LoadSeed { dst: 0, index: 0 },
        L::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 0,
        },
        L::LoadSeed { dst: 0, index: 1 },
        L::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 0,
        },
        L::StoreOutputRange {
            start: 4,
            count: 2,
            stride: 1,
        },
    ];
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("reuse.mo"), 0, 1);
    let source =
        ScalarProgramBlock::with_output_indices(vec![row], vec![span], vec![0, 1]).unwrap();
    let compiled = crate::compile_jacobian_scalar_program_block(&source).unwrap();
    let mut prepared = compiled.prepare_projection(&application(&source)).unwrap();
    // The analytic expected matrix below is the oracle for this work-count
    // regression; do not also count the test-only reference native row calls.
    prepared.validate = false;
    for (y, p, t) in [(0.2_f64, 0.4, 0.5), (1.2, -0.8, 0.1)] {
        take_sin_calls();
        let mut out = [99.0; 4];
        prepared.call(&[y, 7.0], &[p], t, &[], &mut out).unwrap();
        let value = ((y + p) + t).sin();
        assert_eq!(
            out.map(f64::to_bits),
            [value, 0.0, 0.0, value].map(f64::to_bits)
        );
        assert_eq!(
            take_sin_calls(),
            1,
            "seed-independent sine must execute once per call"
        );
    }
}

#[test]
fn projection_keeps_mixed_tensor_seed_lanes_live() {
    use LinearOp as L;
    let row = vec![
        L::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: Some(0),
            lanes: 2,
        },
        L::TensorTranspose {
            dst_start: 4,
            src_start: 0,
            rows: 1,
            columns: 2,
            element_width: 1,
            lanes: 2,
        },
        L::StoreOutputRange {
            start: 5,
            count: 2,
            stride: 2,
        },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("reuse_tensor.mo"),
        0,
        1,
    );
    let source =
        ScalarProgramBlock::with_output_indices(vec![row], vec![span], vec![0, 1]).unwrap();
    let compiled = crate::compile_jacobian_scalar_program_block(&source).unwrap();
    let prepared = compiled.prepare_projection(&application(&source)).unwrap();
    let mut out = [99.0; 4];
    prepared.call(&[2.0, 7.0], &[], 0.0, &[], &mut out).unwrap();
    assert_eq!(out, [1.0, 0.0, 0.0, 1.0]);
}

fn reciprocal_owner(span: rumoca_core::Span) -> rumoca_ir_solve::SolvePureCallTable {
    use rumoca_ir_solve as solve;
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let matrix = solve::SolveValueType::tensor(element, vec![1, 1]).unwrap();
    let vector = solve::SolveValueType::tensor(element, vec![1]).unwrap();
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![matrix, vector.clone()],
            vec![solve::SolvePureCallOutput::result(vector)],
            span,
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span)?;
                let rhs = builder.load(inputs[1], span)?;
                let solution = builder.linear_solve(matrix, rhs, span)?;
                builder.store(outputs[0], solution, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn projection_reuse_preserves_native_failure_and_fresh_parameters() {
    use LinearOp as L;
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("reuse_failure.mo"),
        0,
        1,
    );
    let table = reciprocal_owner(span);
    let row = vec![
        L::LoadY { dst: 0, index: 0 },
        L::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        L::LoadP { dst: 2, index: 0 },
        L::Const { dst: 3, value: 1.0 },
        L::PureCall {
            dst_start: 4,
            input_starts: vec![2, 3].into_boxed_slice(),
            site: table.owners()[0].call_site(),
        },
        L::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 4,
        },
        L::LoadSeed { dst: 6, index: 0 },
        L::LoadSeed { dst: 7, index: 1 },
        L::Binary {
            dst: 8,
            op: BinaryOp::Mul,
            lhs: 5,
            rhs: 6,
        },
        L::Binary {
            dst: 9,
            op: BinaryOp::Mul,
            lhs: 5,
            rhs: 7,
        },
        L::StoreOutputRange {
            start: 8,
            count: 2,
            stride: 1,
        },
    ];
    let source =
        ScalarProgramBlock::with_output_indices(vec![row], vec![span], vec![0, 1]).unwrap();
    let pure_calls = crate::compile_pure_call_table(&table).unwrap();
    let compiled =
        crate::compile_jacobian_scalar_program_block_with_pure_calls(&source, &pure_calls).unwrap();
    let prepared = compiled.prepare_projection(&application(&source)).unwrap();
    let mut out = [99.0; 4];
    take_sin_calls();
    let error = prepared
        .call(&[0.5, 2.0], &[0.0], 1.0, &[], &mut out)
        .unwrap_err();
    assert!(
        error.to_string().contains("linear solve is singular"),
        "{error}"
    );
    assert_eq!(out, [99.0; 4]);
    assert_eq!(
        take_sin_calls(),
        1,
        "the first sine precedes the singular solve"
    );
    for value in [2.0, 4.0] {
        prepared
            .call(&[0.5, 2.0], &[value], 1.0, &[], &mut out)
            .unwrap();
        let coefficient = 0.5_f64.sin() * (1.0 / value);
        assert_eq!(out, [coefficient, 0.0, 0.0, coefficient]);
        assert_eq!(take_sin_calls(), 1);
    }
}
