use super::*;

#[test]
fn extrema_broadcast_scalars_and_keep_shape_and_element_checks() {
    let source = TestSource::new("min({1,2},3.0); max(3.0,{1,2}); min({1,2},{1})");
    let at = source.source("min({1,2},3.0)", 0);
    let mut outputs = Vec::new();
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(at).literal(DaeLiteral::Integer(1))?;
            let two = expressions.at(at).literal(DaeLiteral::Integer(2))?;
            let three = expressions.at(at).literal(DaeLiteral::Real(3.0))?;
            let vector = expressions.at(at).array([one, two])?;
            for (builtin, args) in [
                (PureBuiltin::Min, [vector, three]),
                (PureBuiltin::Min, [three, vector]),
                (PureBuiltin::Max, [vector, three]),
                (PureBuiltin::Max, [three, vector]),
            ] {
                outputs.push(expressions.at(at).builtin(builtin, args)?.index());
            }
            let short = expressions.at(at).array([one])?;
            let mismatch = expressions
                .at(at)
                .builtin(PureBuiltin::Min, [vector, short])
                .expect_err("unequal non-scalar shapes cannot broadcast");
            assert!(matches!(
                mismatch,
                DaeConstructionError::ShapeMismatch { .. }
            ));
            let flag = expressions.at(at).literal(DaeLiteral::Boolean(true))?;
            let mismatch = expressions
                .at(at)
                .builtin(PureBuiltin::Max, [vector, flag])
                .expect_err("broadcasting cannot promote Boolean to a numeric type");
            assert!(matches!(
                mismatch,
                DaeConstructionError::TypeMismatch { .. }
            ));
            Ok(())
        })
    })
    .expect("scalar operands broadcast while Integer and Real promote to Real");
    dae.inspect(|view| {
        for index in outputs {
            let expression = view
                .expression(view.expression_id(usize::try_from(index).unwrap()).unwrap())
                .unwrap();
            assert_eq!(expression.value_type().dimensions(), &[2]);
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        }
    });
}
