use super::*;
use crate::emit::typed_program::take_import_declarations;
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ContinuousStructuralArtifacts,
    FunctionConditionalProgram, PatternDerivation, PatternProvenance, ScalarProgramBlock,
    StructuralPattern, UnaryOp,
};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("shared_projection.mo"),
        0,
        1,
    )
}

fn application(source: &ScalarProgramBlock) -> ProjectionJacobianApplication {
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let pattern =
        StructuralPattern::from_row_dependencies(2, 2, &[vec![0, 1], vec![0, 1]], provenance)
            .unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0, 1],
            tearing: None,
            alternate_charts: Vec::new(),
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

/// A 1x1 linear solve, so `call(m, 1)` is `1 / m`.
fn reciprocal_table() -> rumoca_ir_solve::SolvePureCallTable {
    use rumoca_ir_solve as solve;
    let span = span();
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

/// One source whose program selects `scale` or `-scale` through conditional
/// owner 1. Every source uses the same block-local owner id with its own
/// arms, so a module shared across sources must keep their helpers apart.
fn source(scale: f64, table: &rumoca_ir_solve::SolvePureCallTable) -> ScalarProgramBlock {
    use LinearOp as L;
    let conditional = Arc::new(
        FunctionConditionalProgram::checked_owned(
            FunctionConditionalOwnerId::checked(1).unwrap(),
            0,
            [1],
            [(
                vec![L::LoadP { dst: 0, index: 0 }, L::StoreOutput { src: 0 }],
                vec![
                    L::Const {
                        dst: 0,
                        value: scale,
                    },
                    L::StoreOutput { src: 0 },
                ],
            )],
            vec![
                L::Const {
                    dst: 0,
                    value: -scale,
                },
                L::StoreOutput { src: 0 },
            ],
        )
        .unwrap(),
    );
    let row = vec![
        L::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: conditional,
        },
        L::LoadY { dst: 1, index: 0 },
        L::Unary {
            dst: 2,
            op: UnaryOp::Sin,
            arg: 1,
        },
        L::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        },
        L::LoadP { dst: 4, index: 1 },
        L::Const { dst: 5, value: 1.0 },
        L::PureCall {
            dst_start: 6,
            input_starts: vec![4, 5].into_boxed_slice(),
            site: table.owners()[0].call_site(),
        },
        L::Binary {
            dst: 7,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 6,
        },
        L::LoadSeed { dst: 8, index: 0 },
        L::LoadSeed { dst: 9, index: 1 },
        L::Binary {
            dst: 10,
            op: BinaryOp::Mul,
            lhs: 7,
            rhs: 8,
        },
        L::Binary {
            dst: 11,
            op: BinaryOp::Mul,
            lhs: 7,
            rhs: 9,
        },
        L::StoreOutputRange {
            start: 10,
            count: 2,
            stride: 1,
        },
    ];
    ScalarProgramBlock::with_output_indices(vec![row], vec![span()], vec![0, 1]).unwrap()
}

fn evaluate(batch: &ProjectionBatch, y: f64, p: [f64; 2]) -> [u64; 4] {
    let mut seed = [0.0; 2];
    let mut out = [99.0; 4];
    batch
        .call(&[y, 3.0], &p, 0.25, &mut seed, &mut out)
        .unwrap();
    out.map(f64::to_bits)
}

const POINTS: [(f64, [f64; 2]); 3] = [(0.3, [1.0, 2.0]), (-1.1, [0.0, 4.0]), (2.5, [-2.0, 0.5])];

#[test]
fn projection_applications_share_one_module_with_identical_results() {
    let table = reciprocal_table();
    let pure_calls = crate::compile_pure_call_table(&table).unwrap();
    let sources = [source(2.0, &table), source(3.0, &table)];
    let applications = sources.each_ref().map(application);

    // Each application in a module of its own: the per-application layout.
    take_import_declarations();
    let standalone = applications.each_ref().map(|application| {
        ProjectionBatch::compile(
            application,
            Some(&pure_calls.jit),
            &SharedProjectionModule::default(),
        )
        .unwrap()
    });
    assert_eq!(take_import_declarations(), 2);

    // Every application, one of them twice, in one shared module.
    let shared = SharedProjectionModule::default();
    let batches = [0, 1, 0].map(|index| {
        ProjectionBatch::compile(&applications[index], Some(&pure_calls.jit), &shared).unwrap()
    });
    assert_eq!(
        take_import_declarations(),
        1,
        "a shared projection module declares its pure-call imports once"
    );
    let module = shared.0.borrow().clone().expect("shared module");
    assert!(
        batches
            .iter()
            .all(|batch| Rc::ptr_eq(&batch._module, &module))
    );
    {
        let state = module.state.borrow();
        assert_eq!(state.scopes.len(), 2, "one conditional scope per source");
        assert!(
            state.scopes.iter().all(|scope| scope.functions.len() == 1),
            "each owned conditional helper is defined once per source"
        );
    }

    for (y, p) in POINTS {
        for (index, batch) in [0, 1, 0].into_iter().zip(&batches) {
            assert_eq!(
                evaluate(batch, y, p),
                evaluate(&standalone[index], y, p),
                "application {index} at y={y}, p={p:?}"
            );
        }
        let scale = if p[0] != 0.0 { 2.0 } else { -2.0 };
        let expected = scale * y.sin() * (1.0 / p[1]);
        assert_eq!(
            evaluate(&batches[0], y, p),
            [expected, 0.0, 0.0, expected].map(f64::to_bits)
        );
    }
}

#[test]
fn prepared_projections_of_one_source_share_its_module() {
    let table = reciprocal_table();
    let pure_calls = crate::compile_pure_call_table(&table).unwrap();
    let source = source(2.0, &table);
    let compiled =
        crate::compile_jacobian_scalar_program_block_with_pure_calls(&source, &pure_calls).unwrap();
    take_import_declarations();
    let first = compiled.prepare_projection(&application(&source)).unwrap();
    let second = compiled.prepare_projection(&application(&source)).unwrap();
    assert_eq!(take_import_declarations(), 1);
    for (y, p) in POINTS {
        let mut left = [0.0; 4];
        let mut right = [0.0; 4];
        first.call(&[y, 3.0], &p, 0.25, &[], &mut left).unwrap();
        second.call(&[y, 3.0], &p, 0.25, &[], &mut right).unwrap();
        assert_eq!(left.map(f64::to_bits), right.map(f64::to_bits));
    }
}
