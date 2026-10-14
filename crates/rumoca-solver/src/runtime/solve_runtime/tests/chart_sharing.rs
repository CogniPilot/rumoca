//! An alternate reduced chart's runtime references the primary's prepared and
//! compiled programs by identity except for the replaced programs.

use super::*;
use std::cell::Cell;

/// One compiled block of the recording backend: its programs and the calls
/// that reached this instance.
struct Recorded {
    programs: Vec<Vec<solve::LinearOp>>,
    calls: Rc<Cell<usize>>,
}

#[derive(Default)]
struct RecordingBackend {
    expressions: RefCell<Vec<Recorded>>,
    jacobians: RefCell<Vec<Recorded>>,
    targets: RefCell<Vec<RecordedTarget>>,
}

struct RecordedTarget {
    row: usize,
    source: solve::ScalarProgramBlock,
    calls: Rc<Cell<usize>>,
}

struct TargetValues {
    inner: Rc<dyn CompiledSolveTargetValues>,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveTargetValues for TargetValues {
    fn call(
        &self,
        selected: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        self.inner.call(selected, y, p, t, context)
    }
}

impl RecordingBackend {
    fn record(list: &RefCell<Vec<Recorded>>, block: &solve::ScalarProgramBlock) -> Interpreted {
        let calls = Rc::new(Cell::new(0));
        list.borrow_mut().push(Recorded {
            programs: block.programs().to_vec(),
            calls: calls.clone(),
        });
        let block = PreparedScalarProgramBlock::new(block.clone()).expect("fixture block prepares");
        Interpreted { block, calls }
    }
}

/// A compiled block evaluated by the prepared interpreter, counting its calls.
struct Interpreted {
    block: PreparedScalarProgramBlock,
    calls: Rc<Cell<usize>>,
}

impl Interpreted {
    fn eval(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        seed: Option<&[f64]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        let context = RowEvalContext {
            seed,
            ..RowEvalContext::default()
        };
        self.block
            .eval_with_context(y, p, t, context, out)
            .expect("fixture block evaluates");
        Ok(())
    }
}

impl CompiledSolveExpression for Interpreted {
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        let mut values = vec![0.0; self.block.len()];
        self.eval((y, p, t), None, &mut values)?;
        out.clear();
        out.push(values[program]);
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.eval((y, p, t), None, out)
    }
}

impl CompiledSolveJacobianExpression for Interpreted {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.eval((y, p, t), Some(seed), out)
    }
}

/// A prepared block Jacobian whose value names its block.
struct BlockJacobian(f64);

impl CompiledSolveProjectionJacobian for BlockJacobian {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        out.fill(self.0);
        Ok(())
    }
}

impl SolveExecutionBackend for RecordingBackend {
    fn compile_target_values(
        &self,
        plan: &solve_eval::PreparedTargetValuePlan,
        context: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        let calls = Rc::new(Cell::new(0));
        self.targets.borrow_mut().push(RecordedTarget {
            row: plan.row(),
            source: plan.program().clone(),
            calls: calls.clone(),
        });
        let inner =
            super::super::test_kernels::ReferenceBackend.compile_target_values(plan, context)?;
        Ok(Rc::new(TargetValues { inner, calls }))
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(Self::record(&self.expressions, block)))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Ok(Rc::new(Self::record(&self.jacobians, block)))
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("the sharing fixture has no event transaction".into())
    }
}

/// `slope * y[index] - offset`, and its tangent `slope * seed[index]`.
fn scaled_rows(index: usize, slope: f64, offset: f64) -> [Vec<solve::LinearOp>; 2] {
    use solve::BinaryOp::{Mul, Sub};
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};
    let scaled = |load| {
        vec![
            load,
            Const {
                dst: 1,
                value: slope,
            },
            Binary {
                dst: 2,
                op: Mul,
                lhs: 0,
                rhs: 1,
            },
        ]
    };
    let mut row = scaled(LoadY { dst: 0, index });
    row.extend([
        Const {
            dst: 3,
            value: offset,
        },
        Binary {
            dst: 4,
            op: Sub,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ]);
    let mut tangent = scaled(LoadSeed { dst: 0, index });
    tangent.push(StoreOutput { src: 2 });
    [row, tangent]
}

/// Three decoupled algebraic rows `slopes[i] * y[i] - offsets[i]`, one block
/// each.
fn offset_model(slopes: [f64; 3], offsets: [f64; 3]) -> solve::SolveModel {
    let [rows, tangents]: [Vec<_>; 2] = std::array::from_fn(|kind| {
        (0..3)
            .map(|index| scaled_rows(index, slopes[index], offsets[index])[kind].clone())
            .collect()
    });
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 0),
                vec!["a".into(), "b".into(), "c".into()],
            ),
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: 3,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    rows,
                    "chart_sharing.mo",
                )),
                implicit_row_targets: (0..3)
                    .map(|index| Some(solve::scalar_slot_y(index)))
                    .collect::<Vec<_>>()
                    .into(),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 3].into(),
        ..Default::default()
    };
    set_test_implicit_jvp(&mut model, tangents, "chart_sharing_jvp.mo");
    set_causal_test_projection_plan(&mut model);
    derive_test_structural_artifacts(&mut model);
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem).unwrap();
    model
}

/// The calls counter of the one recorded block with `programs` programs.
fn calls_of(list: &RefCell<Vec<Recorded>>, programs: usize) -> Rc<Cell<usize>> {
    list.borrow()
        .iter()
        .find(|recorded| recorded.programs.len() == programs)
        .map(|recorded| recorded.calls.clone())
        .expect("the block was compiled")
}

/// Blocks that read no replaced row share the primary's prepared Jacobian by
/// identity; the replaced row's block has none.
fn assert_unreplaced_block_jacobians_are_shared(
    alternate: &SolveRuntime,
    primary: &SolveRuntime,
    y: &[f64],
) {
    let rows = |runtime: &SolveRuntime| {
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .continuous_structural
            .algebraic_projection()
            .iter()
            .map(|structure| structure.jacobian_application().map(|a| a.rows().to_vec()))
            .collect::<Vec<_>>()
    };
    assert_eq!(rows(alternate), rows(primary));
    for (index, rows) in rows(alternate).iter().enumerate() {
        let shared = alternate
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_algebraic_jacobians[index]
            .as_ref();
        match rows {
            Some(rows) if !rows.contains(&1) => {
                let shared = shared.unwrap();
                assert!(Rc::ptr_eq(
                    shared,
                    primary
                        .scalar_execution()
                        .unwrap()
                        .scalar
                        .compiled_algebraic_jacobians[index]
                        .as_ref()
                        .unwrap()
                ));
                let mut entry = [f64::NAN];
                shared.call(y, &[], 0.0, &[], &mut entry).unwrap();
                assert_eq!(entry, [index as f64]);
            }
            _ => assert!(shared.is_none()),
        }
    }
    assert!(rows(alternate).iter().flatten().count() >= 2);
}

#[test]
fn an_alternate_references_the_primary_programs_except_the_replaced_rows() {
    let backend = Rc::new(RecordingBackend::default());
    let mut primary = SolveRuntime::new_with_execution_backend(
        &offset_model([1.0; 3], [0.0; 3]),
        Some(backend.clone()),
    )
    .unwrap();
    // The primary's prepared block Jacobians, one per projection block.
    primary.scalar_fixture_mut().compiled_algebraic_jacobians = (0..primary
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()
        .len())
        .map(|index| Some(Rc::new(BlockJacobian(index as f64)) as Rc<_>))
        .collect();
    let primary_rhs_calls = calls_of(&backend.expressions, 3);
    let primary_jvp_calls = calls_of(&backend.jacobians, 3);
    let primary_target_calls = backend
        .targets
        .borrow()
        .iter()
        .find(|target| target.row == 0)
        .unwrap()
        .calls
        .clone();
    backend.expressions.borrow_mut().clear();
    backend.jacobians.borrow_mut().clear();
    backend.targets.borrow_mut().clear();

    // The alternate replaces row 1, `2*b - 5`, and its tangent.
    let [replaced_row, replaced_tangent] = scaled_rows(1, 2.0, 5.0);
    let alternate = primary
        .new_alternate(&offset_model([1.0, 2.0, 1.0], [0.0, 5.0, 0.0]))
        .unwrap();

    // Only the replaced programs are compiled again.
    for (list, replaced) in [
        (&backend.expressions, &replaced_row),
        (&backend.jacobians, &replaced_tangent),
    ] {
        let compiled = list.borrow();
        assert!(!compiled.is_empty());
        assert!(
            compiled
                .iter()
                .all(|recorded| recorded.programs == [replaced.clone()]),
            "an alternate compiles no program it shares with the primary"
        );
    }
    let replacement_calls = backend.expressions.borrow()[0].calls.clone();
    assert_alternate_target_queries_share_unchanged_families(
        &alternate,
        &backend,
        &primary_target_calls,
    );

    // Unreplaced programs execute the primary's compiled instance.
    let view = alternate.scalar_execution().unwrap();
    let native = &view.scalar.implicit_scalar_rhs;
    let (y, mut out) = ([1.0, 2.0, 3.0], Vec::new());
    let point = RowEvalPoint {
        y: &y,
        p: &[],
        t: 0.0,
    };
    native
        .eval_program_outputs(0, point, view.row_eval_context(), &mut out)
        .unwrap();
    assert_eq!((out.as_slice(), primary_rhs_calls.get()), (&[1.0][..], 1));
    native
        .eval_program_outputs(1, point, view.row_eval_context(), &mut out)
        .unwrap();
    assert_eq!((out.as_slice(), replacement_calls.get()), (&[-1.0][..], 1));
    let mut whole = [0.0; 3];
    native
        .eval(point, view.row_eval_context(), &mut whole)
        .unwrap();
    assert_eq!(whole, [1.0, -1.0, 3.0]);
    assert_eq!((primary_rhs_calls.get(), replacement_calls.get()), (2, 2));
    let jacobian = &view.scalar.implicit_projection_scalar_jacobian_v;
    let mut tangent = [0.0; 3];
    jacobian
        .eval(
            solve_eval::JacobianEvalInputs {
                y: &y,
                p: &[],
                t: 0.0,
                seed: &[1.0, 1.0, 1.0],
            },
            view.row_eval_context(),
            &mut tangent,
        )
        .unwrap();
    assert_eq!(tangent, [1.0, 2.0, 1.0]);
    assert_eq!(primary_jvp_calls.get(), 1);

    assert_unreplaced_block_jacobians_are_shared(&alternate, &primary, &y);

    // The fixture declines to compile a refresh assignment schedule, so the
    // alternate refreshes through the interpreter onto its own rows.
    let mut refreshed = [0.0; 3];
    alternate
        .full_solver_y_with_guess(0.0, &[], &[], &mut refreshed, 1e-12, 20)
        .unwrap();
    assert_eq!(refreshed, [0.0, 2.5, 0.0]);
}

fn assert_alternate_target_queries_share_unchanged_families(
    alternate: &SolveRuntime,
    backend: &RecordingBackend,
    primary_calls: &Cell<usize>,
) {
    let compiled = backend.targets.borrow();
    assert_eq!(
        compiled.len(),
        1,
        "only the replaced target family compiles"
    );
    let replacement = &compiled[0];
    assert_eq!(replacement.row, 1);
    let scalar = alternate.scalar_execution().unwrap();
    let plan = scalar
        .scalar
        .implicit_scalar_rhs
        .source()
        .portable_target_value_plan(1, 0, 1)
        .unwrap()
        .unwrap();
    assert_eq!(replacement.source.programs(), plan.program().programs());
    let targets = scalar.scalar.compiled_targets(false).unwrap();
    let before = primary_calls.get();
    assert_eq!(
        targets
            .call(
                (0, 0, 0),
                (&[1.0, 2.0, 3.0], &[], 0.0),
                RowEvalContext::default()
            )
            .unwrap(),
        Some(0.0)
    );
    assert_eq!(
        primary_calls.get(),
        before + 1,
        "unchanged target uses its primary instance"
    );
    assert_eq!(
        targets
            .call(
                (1, 0, 1),
                (&[1.0, 2.0, 3.0], &[], 0.0),
                RowEvalContext::default()
            )
            .unwrap(),
        Some(2.5)
    );
    assert_eq!(replacement.calls.get(), 1);
}
#[test]
fn a_signed_zero_change_replaces_its_exact_target_family() {
    let source = |value| {
        PreparedScalarProgramBlock::new(spanned_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "signed_zero_target.mo",
        ))
        .unwrap()
    };
    let backend = RecordingBackend::default();
    let base_source = source(0.0);
    let target_source = source(-0.0);
    let base = super::super::compiled_targets::TargetQueries::prepare(
        &base_source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
    )
    .unwrap();
    backend.targets.borrow_mut().clear();
    let target = super::super::compiled_targets::TargetQueries::prepare_with_primary(
        &target_source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
        Some(&base),
    )
    .unwrap();
    let expected = target_source
        .eval_target_assignment_output_unchecked_with_context(
            solve_eval::TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: 0,
                target_y_index: 0,
                y: &[1.0],
                p: &[],
                t: 0.0,
                context: RowEvalContext::default(),
            },
        )
        .unwrap()
        .unwrap();
    assert_eq!(expected.to_bits(), (-0.0f64).to_bits());
    let value = target
        .call((0, 0, 0), (&[1.0], &[], 0.0), RowEvalContext::default())
        .unwrap()
        .unwrap();
    assert_eq!(
        value.to_bits(),
        expected.to_bits(),
        "a signed-zero source replacement cannot reuse the old family"
    );
    assert_eq!(backend.targets.borrow().len(), 1);
}
