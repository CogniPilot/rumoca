//! Factory/reuse witnesses using explicit interpreted test kernels, not codegen proof.
use super::*;
use std::cell::Cell;

#[derive(Default)]
struct CompactBackend {
    expression: Cell<usize>,
    compact_expression: Cell<usize>,
    compact_jacobian: Cell<usize>,
}

impl SolveExecutionBackend for CompactBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.expression.set(self.expression.get() + 1);
        test_kernels::expression(source)
    }
    fn compile_compute_expression(
        &self,
        source: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        self.compact_expression
            .set(self.compact_expression.get() + 1);
        test_kernels::expression(
            &to_scalar_program_block(source).map_err(|error| error.to_string())?,
        )
        .map(Some)
    }
    fn compile_compute_jacobian_expression(
        &self,
        source: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.compact_jacobian.set(self.compact_jacobian.get() + 1);
        test_kernels::jacobian(&to_scalar_program_block(source).map_err(|error| error.to_string())?)
            .map(Some)
    }
}

fn constants(values: &[f64]) -> solve::ScalarProgramBlock {
    spanned_block(
        values
            .iter()
            .map(|&value| {
                vec![
                    solve::LinearOp::Const { dst: 0, value },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        "compact-owner.mo",
    )
}

#[test]
fn evaluation_primal_compiles_compact_only_when_fresh_and_reuses_exact_programs() {
    let native = Rc::new(CompactBackend::default());
    let recorded = Rc::new(test_kernels::recording::RecordingBackend::new(
        native.clone(),
    ));
    let backend: Rc<dyn SolveExecutionBackend> = recorded.clone();
    let source = constants(&[7.0, 11.0]);
    let compute = solve::ComputeBlock::from_scalar_program_block(source.clone());
    let first = EvaluationPrimal::prepare(
        "factory derivative",
        source.clone(),
        Some(&compute),
        Some(&backend),
        None,
    )
    .unwrap();
    assert!(
        first.reference().is_none(),
        "a compiled owner cannot issue a reverse interpreter"
    );
    let scalar_before = native.expression.get();
    let same = EvaluationPrimal::prepare(
        "factory derivative",
        source.clone(),
        Some(&compute),
        Some(&backend),
        Some(&first),
    )
    .unwrap();
    assert_eq!(native.compact_expression.get(), 1);
    assert_eq!(native.expression.get(), scalar_before);
    let changed = constants(&[7.0, 13.0]);
    let changed_compute = solve::ComputeBlock::from_scalar_program_block(changed.clone());
    let changed = EvaluationPrimal::prepare(
        "factory derivative",
        changed,
        Some(&changed_compute),
        Some(&backend),
        Some(&same),
    )
    .unwrap();
    assert_eq!(
        native.compact_expression.get(),
        1,
        "a replacement must not recompile the whole compact source"
    );
    assert_eq!(
        native.expression.get(),
        scalar_before + 1,
        "only the replaced program needs scalar lowering"
    );
    let mut out = [0.0; 2];
    changed
        .eval(
            RowEvalPoint {
                y: &[],
                p: &[],
                t: 0.0,
            },
            RowEvalContext::default(),
            &mut out,
        )
        .unwrap();
    assert_eq!(out, [7.0, 13.0]);
    assert_eq!(recorded.requests(), (0, 0, 0));
}

#[test]
fn evaluation_directional_preserves_compact_lowering_and_explicit_reference() {
    let native = Rc::new(CompactBackend::default());
    let recorded = Rc::new(test_kernels::recording::RecordingBackend::new(
        native.clone(),
    ));
    let backend: Rc<dyn SolveExecutionBackend> = recorded.clone();
    let source = constants(&[17.0]);
    let compute = solve::ComputeBlock::from_scalar_program_block(source.clone());
    let compiled = EvaluationDirectional::prepare(
        "factory initial JVP",
        source.clone(),
        Some(&compute),
        Some(&backend),
        None,
    )
    .unwrap();
    let reference =
        EvaluationDirectional::prepare("reference initial JVP", source, Some(&compute), None, None)
            .unwrap();
    let inputs = solve_eval::JacobianEvalInputs {
        y: &[],
        p: &[],
        t: 0.0,
        seed: &[],
    };
    let (mut actual, mut expected) = ([0.0], [0.0]);
    compiled
        .eval(inputs, RowEvalContext::default(), &mut actual)
        .unwrap();
    reference
        .eval(inputs, RowEvalContext::default(), &mut expected)
        .unwrap();
    assert_eq!(actual, expected);
    assert_eq!(actual, [17.0]);
    assert_eq!(native.compact_jacobian.get(), 1);
    assert_eq!(
        recorded.requests(),
        (0, 0, 0),
        "compact admission must not request a scalar JVP"
    );
}
