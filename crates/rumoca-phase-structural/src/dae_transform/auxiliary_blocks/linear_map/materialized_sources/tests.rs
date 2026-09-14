use rumoca_core::{SourceMap, Span, VarName};

use super::super::affine::AffineMap;
use super::*;

#[test]
fn shared_dependencies_preserve_each_candidate_states_independence() {
    for extent in [3, 4096] {
        let (model, indices) = source_model(extent);
        model.inspect(|view| {
            let facts = DifferentiationFacts::collect(view);
            let mut sources = MaterializedSources::new(view, &facts);
            let context = FunctionCallContext::default();
            let product = view.expression_id(indices[0] as usize).unwrap();
            let square = view.expression_id(indices[1] as usize).unwrap();
            for variable in [1, 0, 1, 0] {
                let mut affine = AffineMap::new(&mut sources, variable, extent);
                assert!(
                    affine
                        .expression(product, &context)
                        .unwrap()
                        .coefficient
                        .is_some()
                );
                let value = affine.expression(square, &context);
                assert_eq!(
                    value.map(|value| value.coefficient.is_some()),
                    (variable != 0).then_some(false),
                    "a target's square is nonlinear; another state's square is independent"
                );
            }
            assert_eq!(&*sources.state_anchors(product, &context).unwrap(), &[0, 1]);
            assert_eq!(&*sources.state_anchors(square, &context).unwrap(), &[0]);
        });
    }
}

#[test]
fn shared_function_body_keeps_call_dependencies_and_refusals_distinct() {
    let (model, indices) = source_model(3);
    model.inspect(|view| {
        let facts = DifferentiationFacts::collect(view);
        let mut sources = MaterializedSources::new(view, &facts);
        let root = FunctionCallContext::default();
        let mut shared_body = None;
        for call in [4, 2, 3, 4, 3, 2] {
            let call_id = view.expression_id(indices[call] as usize).unwrap();
            let (body, context) = root.call_result(view, call_id).unwrap();
            assert_eq!(*shared_body.get_or_insert(body), body);
            let expected = (call != 4).then(|| vec![(call - 2) as u32]);
            assert_eq!(
                sources.state_anchors(body, &context).as_deref(),
                expected.as_deref()
            );
            assert_eq!(
                sources.state_anchors(call_id, &root).as_deref(),
                expected.as_deref()
            );
        }
    });
}

fn source_model(extent: u32) -> (dae::Dae, Vec<u32>) {
    let text = "Real x[3], y[3], z[3]; equation x*y=0; x*x=0;";
    let mut sources = SourceMap::new();
    let id = sources.add("source_dependencies.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(id, 0, text.len())).unwrap();
    let mut indices = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)
        })?;
        let x = model.variables(|v| v.state(VarName::new("x"), vector, at, Default::default()))?;
        let y = model.variables(|v| v.state(VarName::new("y"), vector, at, Default::default()))?;
        let z =
            model.variables(|v| v.algebraic(VarName::new("z"), vector, at, Default::default()))?;
        let identity = identity_function(model, vector, at)?;
        model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::State(y))?;
            let z = e.at(at).coordinate(dae::CoordinateInput::Algebraic(z))?;
            let product = e.at(at).binary(dae::BinaryOperator::Multiply, x, y)?;
            let square = e.at(at).binary(dae::BinaryOperator::Multiply, x, x)?;
            indices.extend([product.index(), square.index()]);
            for argument in [x, y, z] {
                indices.push(e.at(at).call(identity, 0, [argument])?.index());
            }
            Ok(())
        })
    })
    .unwrap();
    (model, indices)
}

fn identity_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("identity"), [vector], [vector], at);
    model
        .function(signature, |model, reservation| {
            let input = model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let output = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|e| e.at(at).function_parameter(input))?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, output, value, at))?;
            model.functions(|f| f.define(body, at))
        })
        .map(|(function, ())| function)
}
