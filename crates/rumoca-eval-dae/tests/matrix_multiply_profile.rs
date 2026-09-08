use rumoca_core::{RealMatrixMultiplySemantics, SourceMap, Span};
use rumoca_eval_dae::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;

fn signed_zero_product() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "[-0.0] * [+1.0]";
    let source = sources.add("signed-zero.mo", text);
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len()))
        .expect("fixture span is exact");
    dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let negative_zero = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(-0.0))?;
            let positive_one = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let lhs = expressions.at(provenance).array([negative_zero])?;
            let rhs = expressions.at(provenance).array([positive_one])?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, lhs, rhs)?;
            Ok(())
        })
    })
    .expect("checked signed-zero product constructs")
}

fn empty_product(scalar: dae::ScalarType) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "[] * []";
    let source = sources.add("empty-product.mo", text);
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len()))
        .expect("fixture span is exact");
    dae::Dae::construct(sources, |dae| {
        let empty =
            dae.types(|types| types.derived(dae::ValueType::array(scalar, [0]), provenance))?;
        dae.expressions(|expressions| {
            let lhs = expressions.at(provenance).empty_array(empty)?;
            let rhs = expressions.at(provenance).empty_array(empty)?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, lhs, rhs)?;
            Ok(())
        })
    })
    .expect("checked empty product constructs")
}

fn evaluate(
    model: &dae::Dae,
    semantics: RealMatrixMultiplySemantics,
) -> Result<Vec<f64>, NumericEvaluationError> {
    model.inspect(|view| {
        let expression = view
            .expression_id(view.expression_count() - 1)
            .expect("fixture product is the final expression");
        NumericEvaluator::with_real_matrix_multiply_semantics(view, semantics)
            .expression(expression)
    })
}

#[test]
fn explicit_real_matrix_profile_controls_the_seed_sign() {
    let model = signed_zero_product();
    let first = evaluate(
        &model,
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
    .expect("non-empty FirstProduct evaluates");
    let positive = evaluate(
        &model,
        RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    )
    .expect("PositiveZero evaluates");

    assert_eq!(first, [-0.0]);
    assert!(first[0].is_sign_negative());
    assert_eq!(positive, [0.0]);
    assert!(positive[0].is_sign_positive());
}

#[test]
fn first_product_rejects_an_empty_real_inner_domain_before_result_iteration() {
    let error = evaluate(
        &empty_product(dae::ScalarType::Real),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
    .expect_err("empty Real FirstProduct has no seed");
    assert_eq!(
        error.kind(),
        NumericEvaluationErrorKind::UnsupportedOperation
    );
}

#[test]
fn real_profile_does_not_reselect_integer_matrix_arithmetic() {
    let value = evaluate(
        &empty_product(dae::ScalarType::Integer),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
    .expect("empty Integer product retains its additive-zero meaning");
    assert_eq!(value, [0.0]);
    assert!(value[0].is_sign_positive());
}
