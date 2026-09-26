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
    jacobians: RefCell<Vec<Vec<Vec<solve::LinearOp>>>>,
}

struct Interpreted {
    block: PreparedScalarProgramBlock,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveExpression for Interpreted {
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        let mut values = vec![0.0; self.block.len()];
        self.call(y, p, t, tables, &mut values)?;
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
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        self.block
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(|error| error.to_string())
    }
}

struct UnevaluatedJacobian;

impl CompiledSolveJacobianExpression for UnevaluatedJacobian {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), String> {
        Err("the sharing fixture does not evaluate Jacobians".into())
    }
}

struct BlockJacobian;

impl CompiledSolveProjectionJacobian for BlockJacobian {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        out.fill(1.0);
        Ok(())
    }
}

impl SolveExecutionBackend for RecordingBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        let calls = Rc::new(Cell::new(0));
        self.expressions.borrow_mut().push(Recorded {
            programs: block.programs().to_vec(),
            calls: calls.clone(),
        });
        let block = PreparedScalarProgramBlock::new(block.clone()).map_err(|e| e.to_string())?;
        Ok(Rc::new(Interpreted { block, calls }))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        self.jacobians.borrow_mut().push(block.programs().to_vec());
        Ok(Rc::new(UnevaluatedJacobian))
    }

    fn compile_assignment_schedule(
        &self,
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("unused by the sharing fixture".into())
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("unused by the sharing fixture".into())
    }
}

/// Three decoupled algebraic rows `y[i] - offsets[i]`, one block each.
fn offset_model(offsets: [f64; 3]) -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["a".into(), "b".into(), "c".into()],
                    ..Default::default()
                },
                algebraic_scalar_count: 3,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    (0..3)
                        .map(|index| shifted_variable_residual_row(index, offsets[index]))
                        .collect(),
                    "chart_sharing.mo",
                )),
                implicit_row_targets: (0..3)
                    .map(|index| Some(solve::scalar_slot_y(index)))
                    .collect(),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 3],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        (0..3)
            .map(|index| {
                vec![
                    solve::LinearOp::LoadSeed { dst: 0, index },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        "chart_sharing_jvp.mo",
    );
    set_causal_test_projection_plan(&mut model);
    derive_test_structural_artifacts(&mut model);
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem).unwrap();
    model
}

#[test]
fn an_alternate_references_the_primary_programs_except_the_replaced_rows() {
    let backend = Rc::new(RecordingBackend::default());
    let mut primary =
        SolveRuntime::new_with_execution_backend(&offset_model([0.0; 3]), Some(backend.clone()))
            .unwrap();
    // The primary's prepared block Jacobians, one per projection block.
    primary.compiled_algebraic_jacobians = primary
        .continuous_structural
        .algebraic_projection()
        .iter()
        .map(|_| Some(Rc::new(BlockJacobian) as Rc<dyn CompiledSolveProjectionJacobian>))
        .collect();
    let primary_rhs_calls = backend
        .expressions
        .borrow()
        .iter()
        .find(|recorded| recorded.programs.len() == 3)
        .map(|recorded| recorded.calls.clone())
        .expect("the primary compiles its residual block");
    backend.expressions.borrow_mut().clear();
    backend.jacobians.borrow_mut().clear();

    let replaced_row = shifted_variable_residual_row(1, 5.0);
    let alternate = primary
        .new_alternate(&offset_model([0.0, 5.0, 0.0]))
        .unwrap();

    // Only the replaced program is compiled again; the Jacobian programs are
    // the primary's.
    let compiled = backend.expressions.borrow();
    assert!(!compiled.is_empty());
    assert!(
        compiled
            .iter()
            .all(|recorded| recorded.programs == [replaced_row.clone()]),
        "an alternate compiles no program it shares with the primary"
    );
    assert!(backend.jacobians.borrow().is_empty());
    let replacement_calls = compiled[0].calls.clone();
    drop(compiled);

    // Unreplaced programs execute the primary's compiled instance.
    let native = alternate.compiled_implicit_rhs.as_ref().unwrap();
    let (y, mut out) = ([1.0, 2.0, 3.0], Vec::new());
    assert!(
        native
            .call_program_outputs(0, &y, &[], 0.0, &[], &mut out)
            .unwrap()
    );
    assert_eq!((out.as_slice(), primary_rhs_calls.get()), (&[1.0][..], 1));
    assert!(
        native
            .call_program_outputs(1, &y, &[], 0.0, &[], &mut out)
            .unwrap()
    );
    assert_eq!((out.as_slice(), replacement_calls.get()), (&[-3.0][..], 1));
    let mut whole = [0.0; 3];
    native.call(&y, &[], 0.0, &[], &mut whole).unwrap();
    assert_eq!(whole, [1.0, -3.0, 3.0]);
    assert_eq!((primary_rhs_calls.get(), replacement_calls.get()), (2, 2));

    // The prepared block Jacobians of blocks that read no replaced row are
    // the primary's by identity; the replaced row's block has none.
    let rows = |runtime: &SolveRuntime| {
        runtime
            .continuous_structural
            .algebraic_projection()
            .iter()
            .map(|structure| structure.jacobian_application().map(|a| a.rows().to_vec()))
            .collect::<Vec<_>>()
    };
    assert_eq!(rows(&alternate), rows(&primary));
    for (index, rows) in rows(&alternate).iter().enumerate() {
        let shared = alternate.compiled_algebraic_jacobians[index].as_ref();
        match rows {
            Some(rows) if !rows.contains(&1) => assert!(Rc::ptr_eq(
                shared.unwrap(),
                primary.compiled_algebraic_jacobians[index]
                    .as_ref()
                    .unwrap()
            )),
            _ => assert!(shared.is_none()),
        }
    }
    assert!(rows(&alternate).iter().flatten().count() >= 2);
}
