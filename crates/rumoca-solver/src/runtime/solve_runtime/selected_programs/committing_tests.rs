use super::*;
use std::cell::Cell;

/// A whole-block entry whose committing call refuses before any program runs,
/// as native input validation does, and counts how often it was asked. Its
/// ordinary entry is the reference kernel of the same block.
struct RefusingJacobian {
    committing_calls: Rc<Cell<usize>>,
    ordinary: Rc<dyn CompiledSolveJacobianExpression>,
}

impl CompiledSolveJacobianExpression for RefusingJacobian {
    fn call_programs_committing(
        &self,
        _programs: std::ops::Range<usize>,
        _inputs: (&[f64], &[f64], f64),
        _seed: &mut [f64],
        _external_tables: &[rumoca_core::ExternalTableData],
        _commit: &mut ProgramOutputCommit<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        self.committing_calls.set(self.committing_calls.get() + 1);
        Err(RuntimeSolveError::solve_ir("block inputs refused"))
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.ordinary.call(y, p, t, seed, tables, out)
    }
}

/// The three-program block whose whole entry is `RefusingJacobian`; its
/// per-program codes are the reference kernels. The whole entry is returned
/// too, so a test can reach its ordinary call.
fn refusing_block(
    committing_calls: &Rc<Cell<usize>>,
) -> (Rc<CompleteJacobian>, Rc<RefusingJacobian>) {
    let whole = Rc::new(RefusingJacobian {
        committing_calls: Rc::clone(committing_calls),
        ordinary: super::super::test_kernels::jacobian(&three_programs()).unwrap(),
    });
    let code = jacobian_from_whole(
        &super::super::test_kernels::ReferenceBackend,
        "committing",
        &three_programs(),
        Rc::clone(&whole) as Rc<dyn CompiledSolveJacobianExpression>,
    )
    .unwrap();
    (code, whole)
}

fn span(start: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("committing.mo"),
        start,
        start + 1,
    )
}

/// Three programs, each storing `y[0]`.
fn three_programs() -> solve::ScalarProgramBlock {
    let program = vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    solve::ScalarProgramBlock::with_output_indices(
        vec![program; 3],
        vec![span(10), span(20), span(30)],
        vec![0, 1, 2],
    )
    .unwrap()
}

/// An empty range at the end of the block runs nothing and declines before the
/// whole-block entry (and its block-wide input check) is asked, so no error
/// span is looked up past the programs.
#[test]
fn an_empty_end_range_declines_before_the_native_entry() {
    let committing_calls = Rc::new(Cell::new(0));
    let (code, _) = refusing_block(&committing_calls);
    let declined = code
        .call_programs_committing(3..3, (&[], &[], 0.0), &mut [], &[], &mut |_, _, _| {
            panic!("an empty range commits nothing")
        })
        .unwrap();
    assert!(!declined);
    assert_eq!(committing_calls.get(), 0);
}

/// A refusal before any program commits is a typed error carrying the first
/// program's span, as the program-by-program path reports it, and it leaves
/// the same entry's ordinary whole-block call able to evaluate the block.
#[test]
fn a_refusal_before_any_commit_reports_the_first_program() {
    let committing_calls = Rc::new(Cell::new(0));
    let (code, whole) = refusing_block(&committing_calls);
    let error = code
        .call_programs_committing(0..3, (&[], &[], 0.0), &mut [0.0; 3], &[], &mut |_, _, _| {
            panic!("a refused block commits nothing")
        })
        .unwrap_err();
    assert_eq!(committing_calls.get(), 1);
    assert!(
        matches!(&error, RuntimeSolveError::SolveIr { span: Some(span), .. } if *span == super::committing_tests::span(10)),
        "{error:?}"
    );
    let mut out = [0.0; 3];
    whole.call(&[4.0], &[], 0.0, &[], &[], &mut out).unwrap();
    assert_eq!(out, [4.0; 3]);
}
