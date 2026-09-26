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
    ) -> Result<(), String> {
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
    ) -> Result<bool, String> {
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
    ) -> Result<(), String> {
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
    ) -> Result<(), String> {
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
    ) -> Result<(), String> {
        out.fill(self.0);
        Ok(())
    }
}

impl SolveExecutionBackend for RecordingBackend {
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
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("the sharing fixture refreshes through the interpreter".into())
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
                    rows,
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

#[test]
fn an_alternate_references_the_primary_programs_except_the_replaced_rows() {
    let backend = Rc::new(RecordingBackend::default());
    let mut primary = SolveRuntime::new_with_execution_backend(
        &offset_model([1.0; 3], [0.0; 3]),
        Some(backend.clone()),
    )
    .unwrap();
    // The primary's prepared block Jacobians, one per projection block.
    primary.compiled_algebraic_jacobians =
        (0..primary.continuous_structural.algebraic_projection().len())
            .map(|index| Some(Rc::new(BlockJacobian(index as f64)) as Rc<_>))
            .collect();
    let primary_rhs_calls = calls_of(&backend.expressions, 3);
    let primary_jvp_calls = calls_of(&backend.jacobians, 3);
    backend.expressions.borrow_mut().clear();
    backend.jacobians.borrow_mut().clear();

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
    assert_eq!((out.as_slice(), replacement_calls.get()), (&[-1.0][..], 1));
    let mut whole = [0.0; 3];
    native.call(&y, &[], 0.0, &[], &mut whole).unwrap();
    assert_eq!(whole, [1.0, -1.0, 3.0]);
    assert_eq!((primary_rhs_calls.get(), replacement_calls.get()), (2, 2));

    // The whole Jacobian runs the primary's block, then overwrites the
    // replaced row with the replacement's tangent.
    let jacobian = alternate
        .compiled_implicit_projection_jacobian_v
        .as_ref()
        .unwrap();
    let mut tangent = [0.0; 3];
    jacobian
        .call(&y, &[], 0.0, &[1.0, 1.0, 1.0], &[], &mut tangent)
        .unwrap();
    assert_eq!(tangent, [1.0, 2.0, 1.0]);
    assert_eq!(primary_jvp_calls.get(), 1);

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
            Some(rows) if !rows.contains(&1) => {
                let shared = shared.unwrap();
                assert!(Rc::ptr_eq(
                    shared,
                    primary.compiled_algebraic_jacobians[index]
                        .as_ref()
                        .unwrap()
                ));
                let mut entry = [f64::NAN];
                shared.call(&y, &[], 0.0, &[], &mut entry).unwrap();
                assert_eq!(entry, [index as f64]);
            }
            _ => assert!(shared.is_none()),
        }
    }
    assert!(rows(&alternate).iter().flatten().count() >= 2);
}
