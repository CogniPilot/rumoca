use super::*;
use crate::NumericEvaluationErrorKind;

fn evaluate(
    matrix: [[f64; 2]; 2],
    rhs: [f64; 2],
) -> Result<Vec<f64>, crate::NumericEvaluationError> {
    let mut sources = SourceMap::new();
    let source = sources.add("solve.mo", "solve(A,b)");
    let span = Span::from_offsets(source, 0, 10);
    let at = DaeProvenance::source(span).unwrap();
    let dae = Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let mut rows = Vec::new();
            for row in matrix {
                let entries = row.map(|value| expressions.at(at).literal(DaeLiteral::Real(value)));
                rows.push(
                    expressions
                        .at(at)
                        .array(entries.into_iter().collect::<Result<Vec<_>, _>>()?)?,
                );
            }
            let a = expressions.at(at).array(rows)?;
            let entries = rhs.map(|value| expressions.at(at).literal(DaeLiteral::Real(value)));
            let b = expressions
                .at(at)
                .array(entries.into_iter().collect::<Result<Vec<_>, _>>()?)?;
            expressions
                .at(at)
                .builtin(PureBuiltin::LinearSolve, [a, b])?;
            Ok(())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        let id = view.expression_id(view.expression_count() - 1).unwrap();
        let result = NumericEvaluator::new(view).expression(id);
        if let Err(error) = &result {
            assert_eq!(error.span(), span);
        }
        result
    })
}

#[test]
fn linear_solve_initial_values_are_computed() {
    let values = evaluate([[2.0, 1.0], [1.0, 3.0]], [5.0, 7.0]).unwrap();
    assert!((values[0] - 1.6).abs() < 1e-12);
    assert!((values[1] - 1.8).abs() < 1e-12);
}

#[test]
fn linear_solve_initial_values_use_row_pivoting() {
    assert_eq!(
        evaluate([[0.0, 2.0], [1.0, 3.0]], [4.0, 7.0]).unwrap(),
        [1.0, 2.0]
    );
}

#[test]
fn linear_solve_singular_initial_values_keep_source_error() {
    let error = evaluate([[1.0, 2.0], [2.0, 4.0]], [3.0, 6.0]).unwrap_err();
    assert_eq!(error.kind(), NumericEvaluationErrorKind::InvalidValue);
    assert!(error.to_string().contains("singular"));
}

#[test]
fn linear_solve_nonfinite_source_values_keep_source_error() {
    check_nonfinite_source_error(true);
    check_nonfinite_source_error(false);
}

fn check_nonfinite_source_error(nonfinite_matrix: bool) {
    let mut sources = SourceMap::new();
    let source = sources.add("nonfinite.mo", "solve(A,b)");
    let span = Span::from_offsets(source, 0, 10);
    let at = DaeProvenance::source(span).unwrap();
    let dae = Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
            let nonfinite = expressions
                .at(at)
                .binary(BinaryOperator::Divide, one, zero)?;
            let row = expressions
                .at(at)
                .array([if nonfinite_matrix { nonfinite } else { one }])?;
            let matrix = expressions.at(at).array([row])?;
            let rhs = expressions
                .at(at)
                .array([if nonfinite_matrix { one } else { nonfinite }])?;
            expressions
                .at(at)
                .builtin(PureBuiltin::LinearSolve, [matrix, rhs])?;
            Ok(())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        let id = view.expression_id(view.expression_count() - 1).unwrap();
        let error = NumericEvaluator::new(view).expression(id).unwrap_err();
        assert_eq!(error.kind(), NumericEvaluationErrorKind::InvalidValue);
        assert_eq!(error.span(), span);
    });
}
