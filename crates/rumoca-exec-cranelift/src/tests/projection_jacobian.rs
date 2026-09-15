use super::*;
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ContinuousStructuralArtifacts,
    PatternDerivation, PatternProvenance, ProjectionJacobianApplication, StructuralPattern,
};

fn source() -> ScalarProgramBlock {
    use LinearOp as L;
    ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                L::LoadY { dst: 0, index: 0 },
                L::LoadP { dst: 1, index: 0 },
                L::LoadTime { dst: 2 },
                L::LoadSeed { dst: 3, index: 0 },
                L::LoadSeed { dst: 4, index: 1 },
                L::Binary {
                    dst: 5,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 3,
                },
                L::Binary {
                    dst: 6,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 4,
                },
                L::Binary {
                    dst: 7,
                    op: BinaryOp::Add,
                    lhs: 5,
                    rhs: 6,
                },
                L::Binary {
                    dst: 8,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 4,
                },
                L::StoreOutputRange {
                    start: 7,
                    count: 2,
                    stride: 1,
                },
            ],
            vec![
                L::Const {
                    dst: 0,
                    value: 42.0,
                },
                L::Const { dst: 1, value: 1.0 },
                L::LoadTime { dst: 2 },
                L::TableLookup {
                    dst: 3,
                    table_id: 0,
                    column: 1,
                    input: 2,
                },
                L::StoreOutput { src: 3 },
            ],
        ],
        vec![fixture_span(); 2],
        vec![7, 3, 11],
    )
    .unwrap()
}

fn application(source: &ScalarProgramBlock, rows: Vec<usize>) -> ProjectionJacobianApplication {
    let dependencies = if rows == [3, 7] {
        vec![vec![0], vec![0, 1]]
    } else {
        vec![vec![0, 1], vec![0, 1]]
    };
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, fixture_span())
            .unwrap();
    let pattern =
        StructuralPattern::from_row_dependencies(2, 2, &dependencies, provenance).unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows,
            y_indices: vec![1, 0],
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
fn prepared_projection_matches_selected_jvp_at_fresh_points_and_keeps_code_alive() {
    let source = source();
    let application = application(&source, vec![3, 7]);
    let compiled = compile_jacobian_scalar_program_block(&source).unwrap();
    let prepared = compiled.prepare_projection(&application).unwrap();
    for (y, p, t) in [([3.0, 4.0], 2.0, 0.5), ([-1.0, 9.0], 8.0, 2.0)] {
        let mut expected = [0.0; 4];
        for (column, seed) in [[0.0, 1.0], [1.0, 0.0]].iter().enumerate() {
            expected[2 * column + 1] = compiled
                .call_program_output((0, 0), &y, &[p], t, seed, &[])
                .unwrap();
            if column == 0 {
                expected[0] = compiled
                    .call_program_output((0, 1), &y, &[p], t, seed, &[])
                    .unwrap();
            }
        }
        let before = compiled.jit.program_call_count();
        let mut out = [99.0; 4];
        prepared.call(&y, &[p], t, &[], &mut out).unwrap();
        assert_eq!(out.map(f64::to_bits), expected.map(f64::to_bits));
        assert_eq!(compiled.jit.program_call_count() - before, 3);
    }
    drop(compiled);
    drop(source);
    let mut out = [99.0; 4];
    prepared
        .call(&[3.0, 4.0], &[2.0], 0.5, &[], &mut out)
        .unwrap();
    assert_eq!(out, [0.5, 2.0, 0.0, 3.0]);
}

#[test]
fn prepared_projection_rejects_distinct_owners_of_identical_programs() {
    let source = source();
    let issued = application(&source, vec![3, 7]);
    let replay = ScalarProgramBlock::with_output_indices(
        source.programs().to_vec(),
        source.program_spans().to_vec(),
        source.output_indices().to_vec(),
    )
    .unwrap();
    let foreign = compile_jacobian_scalar_program_block(&replay).unwrap();
    assert!(foreign.prepare_projection(&issued).is_err());
    let replayed = application(&replay, vec![3, 7]);
    assert!(foreign.prepare_projection(&replayed).is_ok());
}

#[test]
fn prepared_projection_preserves_output_on_failure_and_clears_seeds_before_reuse() {
    let source = source();
    let compiled = compile_jacobian_scalar_program_block(&source).unwrap();
    let prepared = compiled
        .prepare_projection(&application(&source, vec![7, 11]))
        .unwrap();
    let mut out = [99.0; 4];
    assert!(
        prepared
            .call(&[3.0, 4.0], &[2.0], 1.0, &[], &mut out)
            .is_err()
    );
    assert_eq!(out, [99.0; 4]);
    assert!(prepared.call(&[3.0], &[2.0], 1.0, &[], &mut out).is_err());
    assert!(prepared.call(&[3.0, 4.0], &[], 1.0, &[], &mut out).is_err());
    let tables = [ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    prepared
        .call(&[3.0, 4.0], &[2.0], 1.0, &tables, &mut out)
        .unwrap();
    assert_eq!(out, [2.0, 12.0, 3.0, 12.0]);
}

fn domain_primal() -> ScalarProgramBlock {
    use LinearOp as L;
    let original = source();
    let first = vec![
        L::LoadY { dst: 0, index: 0 },
        L::LoadY { dst: 1, index: 1 },
        L::LoadP { dst: 2, index: 0 },
        L::LoadTime { dst: 3 },
        L::Const { dst: 4, value: 0.5 },
        L::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        L::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 4,
            rhs: 5,
        },
        L::Binary {
            dst: 7,
            op: BinaryOp::Mul,
            lhs: 2,
            rhs: 1,
        },
        L::Binary {
            dst: 8,
            op: BinaryOp::Add,
            lhs: 6,
            rhs: 7,
        },
        L::Binary {
            dst: 9,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 1,
        },
        L::StoreOutputRange {
            start: 8,
            count: 2,
            stride: 1,
        },
    ];
    ScalarProgramBlock::with_output_indices(
        vec![first, original.programs()[1].clone()],
        vec![fixture_span(); 2],
        vec![7, 3, 11],
    )
    .unwrap()
}

#[test]
fn prepared_projection_compiles_a_source_bound_selected_seed_kernel() {
    let source = source();
    let original = application(&source, vec![3, 7]);
    let primal = domain_primal();
    let domain = rumoca_ir_solve::ProjectionJacobianSeedDomain::derive(&original, &primal).unwrap();
    assert_eq!(domain.primal().programs().len(), 1);
    let derivative = ScalarProgramBlock::with_output_indices(
        vec![source.programs()[0].clone()],
        vec![fixture_span()],
        vec![7, 3],
    )
    .unwrap();
    let specialized = domain.with_lowered_derivative(derivative).unwrap();
    assert!(specialized.canonical_source().shares_program_owner(&source));
    assert!(
        specialized
            .primal_source()
            .unwrap()
            .shares_program_owner(&primal)
    );
    assert!(!specialized.source().shares_program_owner(&source));
    let compiled = compile_jacobian_scalar_program_block(&source).unwrap();
    let general = compiled.prepare_projection(&original).unwrap();
    let prepared = compiled.prepare_projection(&specialized).unwrap();
    drop(compiled);
    for (y, p, t) in [([3.0, 4.0], 2.0, 0.5), ([-1.0, 9.0], 8.0, 2.0)] {
        let mut expected = [0.0; 4];
        let mut actual = [0.0; 4];
        general.call(&y, &[p], t, &[], &mut expected).unwrap();
        prepared.call(&y, &[p], t, &[], &mut actual).unwrap();
        assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
    }
}

#[test]
fn projection_domain_rejects_even_an_unused_seed_outside_its_unknowns() {
    let source = source();
    let original = application(&source, vec![3, 7]);
    let domain =
        rumoca_ir_solve::ProjectionJacobianSeedDomain::derive(&original, &domain_primal()).unwrap();
    let mut program = source.programs()[0].clone();
    program.push(LinearOp::LoadSeed { dst: 99, index: 9 });
    let derivative =
        ScalarProgramBlock::with_output_indices(vec![program], vec![fixture_span()], vec![7, 3])
            .unwrap();
    assert!(domain.with_lowered_derivative(derivative).is_none());
}
