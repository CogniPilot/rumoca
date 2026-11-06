//! Opaque owners retain their one original invocation outside product regions.

use super::*;

fn square<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("square"), [real], [real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let value = model.expressions(|e| {
                    let u = e.at(at).function_parameter(input)?;
                    e.at(at).binary(dae::BinaryOperator::Multiply, u, u)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign(&mut body, output, value, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn parent<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: [dae::ValueTypeId<'dae>; 2],
    child: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let [real, matrix] = types;
    model
        .function(
            dae::FunctionSignature::new(VarName::new("parent"), [real, matrix], [matrix], at),
            |model, reservation| {
                let u = model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let x = model.functions(|f| f.parameter(&reservation, VarName::new("x"), 1, at))?;
                let y = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let product = model.expressions(|e| {
                    let u = e.at(at).function_parameter(u)?;
                    let x = e.at(at).function_parameter(x)?;
                    let value = e.at(at).call(child, 0, [u])?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    let row = e.at(at).array([zero, value])?;
                    let coefficients = e.at(at).array([row, row])?;
                    e.at(at)
                        .binary(dae::BinaryOperator::Multiply, coefficients, x)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign(&mut body, y, product, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn fixture(guarded: bool, columns: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("partial_call.mo", "function square parent");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 22)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [2, columns]),
                    at,
                )?,
            ))
        })?;
        let child = if guarded {
            super::shared_nested_calls::guarded_square(model, real, at)?
        } else {
            square(model, real, at)?
        };
        let parent = parent(model, [real, matrix], child, at)?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), matrix, at, Default::default())
        })?;
        model.expressions(|e| {
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            e.at(at).call(parent, 0, [one, x])?;
            Ok(())
        })
    })
    .unwrap()
}

fn inputs(u: f64) -> Vec<rumoca_eval_solve::TypedValue> {
    [vec![u], vec![f64::NAN, f64::NAN, 4.0, 5.0]]
        .into_iter()
        .zip([vec![], vec![2, 2]])
        .map(|(values, shape)| {
            let scalar = solve::SolveScalarType::real(arithmetic_profile());
            let ty = if shape.is_empty() {
                solve::SolveValueType::scalar(scalar)
            } else {
                solve::SolveValueType::tensor(scalar, shape).unwrap()
            };
            rumoca_eval_solve::TypedValue::construct(
                ty,
                values
                    .into_iter()
                    .map(|x| solve::SolveValueKind::Real64(x.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect()
}

#[test]
fn partial_products_capture_one_opaque_call_in_the_original_scope() {
    let table = lower_root_call(&fixture(false, 2));
    let owner = table.owners().last().unwrap();
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), solve::SolveOperation::Call { .. }))
            .count(),
        1,
        "one source call must stay outside map/fold, not become one invocation per retained lane"
    );
    let output =
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs(3.0)).unwrap();
    assert_eq!(
        output[0].elements(),
        [36.0f64, 45.0, 36.0, 45.0].map(|x| solve::SolveValueKind::Real64(x.to_bits()))
    );
}

#[test]
fn partial_products_keep_the_same_assertion_identity_and_recover() {
    let table = lower_root_call(&fixture(true, 2));
    let owner = table.owners().last().unwrap();
    let root =
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs(-1.0)).unwrap_err();
    let child = rumoca_eval_solve::eval_pure_call_checked(
        &table,
        table.owners()[0].id(),
        &inputs(-1.0)[..1],
    )
    .unwrap_err();
    assert!(matches!(
        child,
        rumoca_eval_solve::TypedProgramEvalError::AssertionFailed { .. }
    ));
    assert_eq!(root, child);
    let output =
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs(3.0)).unwrap();
    assert_eq!(
        output[0].elements(),
        [36.0f64, 45.0, 36.0, 45.0].map(|x| solve::SolveValueKind::Real64(x.to_bits()))
    );
}

#[test]
fn empty_product_keeps_opaque_validation_with_no_result_leaves() {
    let table = lower_root_call(&fixture(true, 0));
    let owner = table.owners().last().unwrap();
    assert_eq!(
        owner.inputs().len(),
        1,
        "zero-width matrix input owns no scalar leaf"
    );
    assert!(matches!(
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs(-1.0)[..1]),
        Err(rumoca_eval_solve::TypedProgramEvalError::AssertionFailed { .. })
    ));
    let output =
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs(3.0)[..1]).unwrap();
    assert_eq!(output.len(), owner.outputs().len());
    for (value, layout) in output.iter().zip(owner.outputs()) {
        assert_eq!(
            layout.kind(),
            solve::SolvePureCallOutputKind::AssertionPredicate
        );
        assert_eq!(value.elements(), [solve::SolveValueKind::Boolean(true)]);
    }
}
