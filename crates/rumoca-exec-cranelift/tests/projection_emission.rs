//! Production crate (not cfg(test)): a specialized application's standalone
//! row entry is not emitted in addition to the actual colored batch.

use rumoca_core::{SourceId, Span};
use rumoca_exec_cranelift::{compile_jacobian_scalar_program_block, record_expansion_report};
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ContinuousStructuralArtifacts,
    LinearOp as L, PatternDerivation, PatternProvenance, ProjectionJacobianSeedDomain,
    ScalarProgramBlock, StructuralPattern,
};

#[test]
fn specialized_projection_emits_only_its_executed_colored_batch() {
    let span = Span::from_offsets(SourceId::from_source_name("square.mo"), 0, 1);
    let block =
        |ops| ScalarProgramBlock::with_output_indices(vec![ops], vec![span], vec![0]).unwrap();
    let primal = block(vec![
        L::LoadY { dst: 0, index: 0 },
        L::Binary {
            dst: 1,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        L::StoreOutput { src: 1 },
    ]);
    let derivative = || {
        block(vec![
            L::LoadY { dst: 0, index: 0 },
            L::LoadSeed { dst: 1, index: 0 },
            L::Const { dst: 2, value: 2.0 },
            L::Binary {
                dst: 3,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 2,
            },
            L::Binary {
                dst: 4,
                op: BinaryOp::Mul,
                lhs: 3,
                rhs: 1,
            },
            L::StoreOutput { src: 4 },
        ])
    };
    let source = derivative();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span).unwrap();
    let pattern = StructuralPattern::from_row_dependencies(1, 1, &[vec![0]], provenance).unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
            tearing: None,
            alternate_charts: vec![],
        }]
        .into(),
    };
    let artifacts = ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern],
        vec![false],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, &primal, &source, &source);
    let general = artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    let specialized = ProjectionJacobianSeedDomain::derive(general, &primal)
        .unwrap()
        .with_lowered_derivative(derivative())
        .unwrap();
    assert!(!specialized.source().shares_program_owner(&source));
    let compile = |application| {
        let compiled = compile_jacobian_scalar_program_block(&source).unwrap();
        let (batch, report) = record_expansion_report(|| compiled.prepare_projection(application));
        assert!(report.complete);
        (batch.unwrap(), report.entries.len())
    };
    let (general, general_entries) = compile(general);
    let (specialized, specialized_entries) = compile(&specialized);
    assert_eq!(
        specialized_entries, general_entries,
        "no second standalone derivative row is emitted"
    );
    for y in [0.25, -2.0, 11.0] {
        let mut actual = [0.0];
        let mut expected = [0.0];
        specialized.call(&[y], &[], 0.0, &[], &mut actual).unwrap();
        general.call(&[y], &[], 0.0, &[], &mut expected).unwrap();
        assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
        assert_eq!(actual, [2.0 * y]);
    }
}
