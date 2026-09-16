//! A comprehension keeps its declared domain even when its tensor body is invariant.

use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call, eval_pure_call_directional};

#[test]
fn binder_independent_tensor_comprehension_retains_axes_and_ad() {
    check_comprehension(false);
}

#[test]
fn inner_comprehension_keeps_its_domain_when_body_reads_only_outer_binder() {
    check_comprehension(true);
}

fn check_comprehension(nested: bool) {
    let model = tensor_comprehension(nested);
    let calls = lower_root_call(&model);
    let owner = calls.owners().last().unwrap();
    let input = TypedValue::construct(
        owner.inputs()[0].clone(),
        (1..=4)
            .map(|i| solve::SolveValueKind::Real64(f64::from(i).to_bits()))
            .collect(),
    )
    .unwrap();
    let tangent = TypedValue::construct(
        owner.inputs()[0].clone(),
        (5..=8)
            .map(|i| solve::SolveValueKind::Real64(f64::from(i).to_bits()))
            .collect(),
    )
    .unwrap();
    let expected = TypedValue::construct(
        owner.outputs()[0].value_type().clone(),
        repeated(input.elements(), nested),
    )
    .unwrap();
    let expected_tangent = TypedValue::construct(
        owner.outputs()[0].value_type().clone(),
        repeated(tangent.elements(), nested),
    )
    .unwrap();
    assert_eq!(
        expected.value_type().dimensions(),
        if nested {
            vec![2, 3, 2, 2]
        } else {
            vec![3, 2, 2]
        }
    );
    assert_eq!(
        eval_pure_call(&calls, owner.id(), std::slice::from_ref(&input)).unwrap(),
        std::slice::from_ref(&expected)
    );
    assert_eq!(
        eval_pure_call_directional(&calls, owner.id(), &[input, tangent]).unwrap(),
        [expected, expected_tangent]
    );
    assert!(
        owner
            .body()
            .operations()
            .iter()
            .any(|op| matches!(op.operation(), solve::SolveOperation::Map { .. }))
    );
}

fn repeated(elements: &[solve::SolveValueKind], nested: bool) -> Vec<solve::SolveValueKind> {
    let mut result = elements.repeat(3);
    if nested {
        result.extend(elements.repeat(3).into_iter().map(|v| {
            let solve::SolveValueKind::Real64(bits) = v else {
                panic!("Real payload");
            };
            solve::SolveValueKind::Real64((2. * f64::from_bits(bits)).to_bits())
        }));
    }
    result
}

fn tensor_comprehension(nested: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("copies.mo", "function copies input Real u[2,2]; output Real y[3,2,2]; algorithm y:={u for i in 1:3}; end copies;");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (input_type, result_type) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
                types.derived(
                    dae::ValueType::array(
                        dae::ScalarType::Real,
                        if nested {
                            vec![2, 3, 2, 2]
                        } else {
                            vec![3, 2, 2]
                        },
                    ),
                    at,
                )?,
            ))
        })?;
        let domain = model.domains(|domains| {
            domains.structured(structured_range("i", if nested { 2 } else { 3 }), at)
        })?;
        let outer_binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let inner = if nested {
            Some(model.domains(|domains| domains.nested(domain, structured_range("j", 3), at))?)
        } else {
            None
        };
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("copies"), [input_type], [result_type], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let repeated = repetition(model, input, domain, inner, outer_binder, at)?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, repeated, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        let variable = model.variables(|v| {
            v.algebraic(
                VarName::new("x"),
                input_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|e| {
            let value = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            e.at(at).call(function, 0, [value])
        })?;
        Ok(())
    })
    .unwrap()
}

fn repetition<'d>(
    model: &mut dae::DaeConstruction<'d>,
    input: dae::FunctionParameterId<'d>,
    domain: dae::DomainId<'d>,
    inner: Option<dae::DomainId<'d>>,
    outer_binder: dae::DomainBinderId<'d>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'d>, dae::DaeConstructionError> {
    model.expressions(|e| {
        let input = e.at(at).function_parameter(input)?;
        let body = if let Some(inner) = inner {
            let binder = e.at(at).binder(outer_binder)?;
            let scaled = e
                .at(at)
                .binary(dae::BinaryOperator::Multiply, binder, input)?;
            e.at(at).comprehension(inner, scaled)?
        } else {
            input
        };
        e.at(at).comprehension(domain, body)
    })
}
