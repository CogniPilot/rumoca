//! Delegation contracts for the work-counting adapter itself.
use super::*;
use solve::{BinaryOp, LinearOp as L};

fn scalar_source() -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("counted-backend.mo"),
        0,
        1,
    );
    let rows = (0..2)
        .map(|index| {
            vec![
                L::LoadP { dst: 0, index },
                L::LoadSeed { dst: 1, index },
                L::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                L::StoreOutput { src: 2 },
            ]
        })
        .collect();
    solve::ScalarProgramBlock::with_program_spans(rows, vec![span; 2]).unwrap()
}

fn counted_backend() -> (Rc<dyn SolveExecutionBackend>, Rc<Calls>) {
    let opts = super::super::super::sim_opts(crate::SimExecutionPolicy::NativeRequired);
    let fixture = super::super::super::lower(&super::super::source(4), "Heat", &opts);
    counted(&fixture.model)
}

#[test]
fn selected_jacobian_delegates_fresh_inputs_and_requested_order() {
    let (backend, calls) = counted_backend();
    let compiled = backend
        .compile_jacobian_expression(&scalar_source())
        .unwrap();
    assert_eq!(calls.compiles.get(), 1);
    for (p, seed) in [([2.0, 3.0], [5.0, 7.0]), ([-4.0, 11.0], [2.0, -3.0])] {
        assert_eq!(
            compiled
                .call_program_output((0, 0), &[], &p, 0.0, &seed, &[])
                .unwrap(),
            Some(p[0] * seed[0])
        );
        let mut out = [f64::NAN; 2];
        let inputs = eval::JacobianEvalInputs {
            y: &[],
            p: &p,
            t: 0.0,
            seed: &seed,
        };
        assert!(
            compiled
                .call_program_outputs_at(&[(1, 0), (0, 0)], inputs, &[], &mut out)
                .unwrap()
        );
        assert_eq!(out, [p[1] * seed[1], p[0] * seed[0]]);
    }
    assert_eq!(calls.succeeded.get(), 4);
    assert_eq!(calls.failed.get(), 0);
    assert_eq!(calls.compiles.get(), 1);
}

#[test]
fn torn_assignments_delegate_ordered_fresh_state() {
    let (backend, calls) = counted_backend();
    let rows = vec![
        vec![L::LoadP { dst: 0, index: 0 }, L::StoreOutput { src: 0 }],
        vec![
            L::LoadY { dst: 0, index: 0 },
            L::LoadTime { dst: 1 },
            L::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            L::StoreOutput { src: 2 },
        ],
    ];
    let compiled = backend
        .compile_torn_assignment_rows(&rows, &[0, 1])
        .unwrap();
    for (p, t) in [(3.0, 2.0), (-7.0, 0.5)] {
        let mut y = [99.0, 88.0];
        compiled.call(&mut y, &[p], t, &[]).unwrap();
        assert_eq!(y, [p, p + t]);
    }
    assert_eq!(calls.compiles.get(), 1);
}

fn projection_application(
    source: &solve::ScalarProgramBlock,
) -> solve::ProjectionJacobianApplication {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("projection.mo"),
        0,
        1,
    );
    let pattern = solve::StructuralPattern::derive_from_scalar_jvp(source, 2, 2, span).unwrap();
    let plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0, 1],
            tearing: None,
            alternate_charts: Vec::new(),
        }]
        .into(),
    };
    let artifacts = solve::ContinuousStructuralArtifacts::derived(
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
fn projection_delegates_fresh_parameters_and_counts_each_application() {
    let source = scalar_source();
    let (backend, calls) = counted_backend();
    let compiled = backend.compile_jacobian_expression(&source).unwrap();
    let application = projection_application(&source);
    let projections = compiled.prepare_projections(&[&application]).unwrap();
    assert_eq!(projections.len(), 1);
    let projection = projections[0]
        .as_ref()
        .expect("native projection capability retained");
    for p in [[2.0, 3.0], [-4.0, 11.0]] {
        let mut out = [f64::NAN; 2];
        projection
            .call(&[0.0, 0.0], &p, 0.0, &[], &mut out)
            .unwrap();
        assert_eq!(out, [p[0], p[1]]);
    }
    let mut wrong_extent = [19.0; 4];
    assert!(
        projection
            .call(&[0.0, 0.0], &[2.0, 3.0], 0.0, &[], &mut wrong_extent)
            .is_err()
    );
    assert_eq!(wrong_extent, [19.0; 4]);
    assert_eq!(calls.projection_calls.get(), 3);
    assert_eq!(calls.succeeded.get(), 2);
    assert_eq!(calls.failed.get(), 1);
    assert_eq!(calls.compiles.get(), 1);
}

#[test]
fn event_observation_delegates_predicate_and_message() {
    let opts = super::super::super::sim_opts(crate::SimExecutionPolicy::NativeRequired);
    let fixture = super::super::super::lower(
        r#"
function warningValue
 input Real q;
 output Real y;
algorithm
 assert(q < 0, "value=" + String(q), level=AssertionLevel.warning);
 y := q;
end warningValue;
model Observed
 Real x(start=0, fixed=true);
equation
 der(x)=warningValue(time);
end Observed;
"#,
        "Observed",
        &opts,
    );
    let (backend, calls) = counted(&fixture.model);
    let source = solve::SolveExecutionSource::new(fixture.model.clone());
    let mut checked = 0;
    for index in 0..fixture.model.problem.events.action_conditions.row_count() {
        let program = source
            .event_program(solve::RetainedAssertionSource::ActionConditions, index)
            .unwrap();
        if !program.has_observations() {
            continue;
        }
        let count = solve::ScalarProgramBlock::program_output_count(program.program());
        let direct_backend = crate::native_execution::backend(&fixture.model.pure_calls).unwrap();
        let direct = direct_backend
            .compile_event_observation(
                source
                    .event_program(solve::RetainedAssertionSource::ActionConditions, index)
                    .unwrap(),
            )
            .unwrap();
        let compiled = backend.compile_event_observation(program).unwrap();
        let y = vec![0.0; fixture.model.problem.layout.y_scalars()];
        let p = vec![0.0; fixture.model.problem.layout.p_scalars()];
        let mut out = vec![f64::NAN; count];
        compiled.call(&y, &p, -1.0, &[], &mut out).unwrap();
        let passing = out.clone();
        compiled.call(&y, &p, 1.0, &[], &mut out).unwrap();
        assert_ne!(out, passing, "fresh time changes the observed predicate");
        let mut reference = vec![f64::NAN; count];
        direct.call(&y, &p, 1.0, &[], &mut reference).unwrap();
        assert_eq!(out, reference);
        for action in &fixture.model.problem.events.actions {
            assert_eq!(
                compiled.observed_message(action),
                direct.observed_message(action)
            );
        }
        checked += 1;
    }
    assert!(checked > 0);
    assert_eq!(calls.compiles.get(), checked);
}

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("mixed_event.mo"),
        17,
        29,
    )
}

fn transaction() -> (solve::EventTransactionProgram, solve::SolvePureCallTable) {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let types = vec![
        solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic)),
        solve::SolveValueType::scalar(solve::SolveScalarType::Integer(arithmetic.integer_domain())),
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
    ];
    let mut table = solve::SolvePureCallTable::builder(arithmetic);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            types.clone(),
            types
                .iter()
                .cloned()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(),
            |body, inputs, outputs| {
                let real = body.load(inputs[0], span())?;
                body.store(outputs[0], real, span())?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    real,
                    span(),
                )?;
                let input_integer = body.load(inputs[1], span())?;
                let sum = body.binary(
                    solve::SolveBinaryOperator::Add,
                    integer,
                    input_integer,
                    span(),
                )?;
                body.store(outputs[1], sum, span())?;
                let boolean = body.load(inputs[2], span())?;
                let negated = body.unary(solve::SolveUnaryOperator::Not, boolean, span())?;
                body.store(outputs[2], negated, span())
            },
        )
        .unwrap();
    let program = solve::EventTransactionProgram::checked(
        solve::EventTransactionConstruction {
            site: table.call_site(owner).unwrap(),
            inputs: types
                .iter()
                .cloned()
                .enumerate()
                .map(|(i, kind)| (solve::scalar_slot_p(i + 3), kind))
                .collect(),
            targets: types
                .into_iter()
                .enumerate()
                .map(|(i, kind)| (solve::scalar_slot_p(i), kind, None))
                .collect(),
            producer_owners: (0..3)
                .map(|start_row| solve::EventTransactionProducerOwner::ScalarRows { start_row })
                .collect(),
            assertions: Vec::new(),
            assertion_action_indices: Vec::new(),
            statement_count: 3,
            clock_owners: Vec::new(),
        },
        span().require_provenance("mixed event fixture").unwrap(),
    )
    .unwrap();
    (program, table.finish())
}

#[test]
fn event_transaction_delegates_typed_values_and_atomic_errors() {
    let (program, table) = transaction();
    let calls = Rc::new(Calls::default());
    let backend = Backend {
        inner: crate::native_execution::backend(&table).unwrap(),
        calls: calls.clone(),
    };
    let compiled = backend.compile_event_transaction(&program).unwrap();
    let mut out = [19.0; 3];
    compiled.call(&[2.75, 5.0, 1.0], &mut out).unwrap();
    assert_eq!(out, [2.75, 7.0, 0.0]);
    out.fill(19.0);
    assert!(compiled.call(&[2.75, 5.5, 1.0], &mut out).is_err());
    assert_eq!(out, [19.0; 3]);
    compiled.call(&[3.5, 2.0, 0.0], &mut out).unwrap();
    assert_eq!(out, [3.5, 5.0, 1.0]);
    assert_eq!(calls.compiles.get(), 1);
}

#[test]
fn bound_entry_preserves_optional_absence_without_counting_execution() {
    let source = scalar_source();
    let (backend, calls) = counted_backend();
    let compiled = backend.compile_jacobian_expression(&source).unwrap();
    let bound = compiled.bind_program(&source, 0).unwrap().unwrap();
    assert!(
        bound
            .call_program_output((0, 0), &[], &[2.0, 3.0], 0.0, &[5.0, 7.0], &[])
            .unwrap()
            .is_none()
    );
    let mut out = [19.0];
    let inputs = eval::JacobianEvalInputs {
        y: &[],
        p: &[2.0, 3.0],
        t: 0.0,
        seed: &[5.0, 7.0],
    };
    assert!(
        !bound
            .call_program_outputs_at(&[(0, 0)], inputs, &[], &mut out)
            .unwrap()
    );
    assert_eq!(out, [19.0]);
    let application = projection_application(&source);
    let prepared = bound.prepare_projections(&[&application]).unwrap();
    assert_eq!(prepared.len(), 1);
    assert!(prepared[0].is_none());
    assert_eq!(calls.succeeded.get(), 0);
    assert_eq!(calls.failed.get(), 0);
    assert_eq!(calls.projection_calls.get(), 0);
    bound
        .call(&[], &[2.0, 3.0], 0.0, &[5.0, 7.0], &[], &mut out)
        .unwrap();
    assert_eq!(out, [10.0]);
    assert_eq!(calls.succeeded.get(), 1);
    assert_eq!(calls.compiles.get(), 1);
}
