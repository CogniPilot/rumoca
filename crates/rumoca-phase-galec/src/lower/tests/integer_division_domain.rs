//! Scalar outputs must not rely on a later array-index check for division safety.
use super::*;

#[test]
fn unknown_scalar_divisor_is_a_coded_refusal() {
    let mut sources = SourceMap::new();
    let text = "function quotient input Integer a; input Integer b; output Integer q; algorithm q:=div(a,b); end quotient;";
    let source = sources.add("scalar-division.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let integer =
            dae.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        dae.function(
            dae::FunctionSignature::new(
                VarName::new("quotient"),
                [integer, integer],
                [integer],
                at,
            ),
            |dae, reservation| {
                let (a, b, q) = dae.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("a"), 0, at)?,
                        f.parameter(&reservation, VarName::new("b"), 1, at)?,
                        f.output(&reservation, VarName::new("q"), 0, at)?,
                    ))
                })?;
                let mut body = dae.functions(|f| f.begin(reservation, at))?;
                let arguments = dae.expressions(|e| {
                    let a = e.at(at).function_parameter(a)?;
                    let b = e.at(at).function_parameter(b)?;
                    Ok([a, b])
                })?;
                let value =
                    dae.function_runtime_quotient(&body, dae::PureBuiltin::Div, arguments, at)?;
                dae.functions(|f| f.assign(&mut body, q, value, at))?;
                dae.functions(|f| f.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let definitions=rumoca_phase_structural::CausalDefinitions::derive(view);
        let error=user_functions::lower_reachable(view,&definitions,HashSet::from([0]),EmissionFacts::structured()).unwrap_err();
        assert!(matches!(error,GalecTargetError::UnsupportedFeature {feature,..} if feature=="integer-division-domain"));
    });
}
