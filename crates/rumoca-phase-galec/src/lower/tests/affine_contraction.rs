//! A passive matrix offset must not hide the row producer of a contraction.
use super::*;

fn combine<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    values: [dae::ExprId<'dae>; 4],
    offset: Option<(dae::BinaryOperator, bool)>,
    outer: bool,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let [a, x, n, y] = values;
    let mut value = e.at(at).binary(dae::BinaryOperator::Multiply, a, x)?;
    if let Some((op, product_first)) = offset {
        let (lhs, rhs) = if product_first {
            (value, n)
        } else {
            (n, value)
        };
        value = e.at(at).binary(op, lhs, rhs)?;
    }
    if outer {
        value = e.at(at).binary(dae::BinaryOperator::Multiply, value, y)?;
    }
    Ok(value)
}

fn lower(offset: Option<(dae::BinaryOperator, bool)>, outer: bool) -> Vec<gast::UserFunction> {
    let mut sources = SourceMap::new();
    let text = "function compose input Real A[2,3]; input Real X[3,4]; input Real N[2,4]; input Real Y[4,5]; output Real P[2,5]; algorithm P := (A*X+N)*Y; end compose;";
    let source = sources.add("affine-contraction.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let (a, x, n, y, p) = dae.types(|t| {
            Ok((
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 3]), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [3, 4]), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 4]), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [4, 5]), at)?,
                t.derived(
                    dae::ValueType::array(
                        dae::ScalarType::Real,
                        if outer { [2, 5] } else { [2, 4] },
                    ),
                    at,
                )?,
            ))
        })?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("compose"), [a, x, n, y], [p], at),
            |dae, reservation| {
                let arguments = dae.functions(|f| {
                    (0..4)
                        .map(|i| f.parameter(&reservation, VarName::new(format!("u{i}")), i, at))
                        .collect::<Result<Vec<_>, _>>()
                })?;
                let output = dae.functions(|f| f.output(&reservation, VarName::new("P"), 0, at))?;
                let value = dae.expressions(|e| {
                    let a = e.at(at).function_parameter(arguments[0])?;
                    let x = e.at(at).function_parameter(arguments[1])?;
                    let n = e.at(at).function_parameter(arguments[2])?;
                    let y = e.at(at).function_parameter(arguments[3])?;
                    combine(e, at, [a, x, n, y], offset, outer)
                })?;
                let mut body = dae.functions(|f| f.begin(reservation, at))?;
                dae.functions(|f| f.assign(&mut body, output, value, at))?;
                dae.functions(|f| f.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([0]),
            EmissionFacts::structured(),
        )
        .unwrap()
    })
}

#[test]
fn an_offset_on_either_side_preserves_the_composed_row_producer() {
    for op in [dae::BinaryOperator::Add, dae::BinaryOperator::Subtract] {
        for product_first in [true, false] {
            assert_eq!(lower(Some((op, product_first)), true).len(), 1);
        }
    }
}
#[test]
fn a_direct_composed_contraction_remains_admitted() {
    assert_eq!(lower(None, true).len(), 1);
}
#[test]
fn an_offset_without_the_outer_product_remains_admitted() {
    assert_eq!(
        lower(Some((dae::BinaryOperator::Add, true)), false).len(),
        1
    );
}

fn row_witnesses(statements: &[gast::Spanned<gast::Statement>]) -> usize {
    statements
        .iter()
        .map(|statement| match &statement.node {
            gast::Statement::For(loop_) => {
                usize::from(loop_.row_contraction().is_some()) + row_witnesses(&loop_.body)
            }
            _ => 0,
        })
        .sum()
}

#[test]
fn only_an_unadjusted_product_attests_a_plain_row_contraction() {
    assert!(row_witnesses(&lower(None, true)[0].statements) > 0);
    for op in [dae::BinaryOperator::Add, dae::BinaryOperator::Subtract] {
        for product_first in [true, false] {
            assert_eq!(
                row_witnesses(&lower(Some((op, product_first)), true)[0].statements),
                0
            );
        }
    }
}
