//! Equal checked types may retain distinct source declaration identities.

use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call, eval_pure_call_directional};

#[test]
fn distinct_declarations_of_equal_types_preserve_values_and_tangents() {
    for shape in [vec![], vec![3], vec![2, 2]] {
        let model = equal_types(shape);
        let calls = lower_root_call(&model);
        let owner = calls.owners().last().unwrap();
        let input_type = &owner.inputs()[0];
        let input = TypedValue::construct(
            input_type.clone(),
            (1..=input_type.scalar_count())
                .map(|i| solve::SolveValueKind::Real64(f64::from(i).to_bits()))
                .collect(),
        )
        .unwrap();
        let tangent = TypedValue::construct(
            input_type.clone(),
            vec![
                solve::SolveValueKind::Real64(2_f64.to_bits());
                input_type.scalar_count() as usize
            ],
        )
        .unwrap();
        assert_eq!(
            eval_pure_call(&calls, owner.id(), std::slice::from_ref(&input)).unwrap(),
            std::slice::from_ref(&input)
        );
        assert_eq!(
            eval_pure_call_directional(&calls, owner.id(), &[input.clone(), tangent.clone()])
                .unwrap(),
            [input, tangent]
        );
    }
}

fn equal_types(shape: Vec<u32>) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "equal_types.mo",
        "function f input Real u; output Real y; algorithm y:=u; end f;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let ty = if shape.is_empty() {
            dae::ValueType::scalar(dae::ScalarType::Real)
        } else {
            dae::ValueType::array(dae::ScalarType::Real, shape)
        };
        let (first, second) = model.types(|types| {
            Ok((
                types.intern(rumoca_core::TypeId::new(0), ty.clone(), at)?,
                types.intern(rumoca_core::TypeId::new(1), ty, at)?,
            ))
        })?;
        assert_ne!(first, second);
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("f"), [first], [second], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let input = model.expressions(|e| e.at(at).function_parameter(input))?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, input, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        let variable = model.variables(|v| {
            v.algebraic(
                VarName::new("x"),
                first,
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
