use super::*;
use crate::NumericInitialRun;

#[test]
fn checked_fill_and_declaration_start_broadcast_issue_repeat_before_dense_reads() {
    let text = "parameter Real p[20,10000]=fill(-0.0,20,10000); Real x[20,10000](each start=0.25);";
    let mut source_map = SourceMap::new();
    let source = source_map.add("NativeStarts.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        let ty =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [20, 10000]), at))?;
        let value = dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(-0.0)))?;
        let first = dae.expressions(|e| e.at(at).literal(DaeLiteral::Integer(20)))?;
        let second = dae.expressions(|e| e.at(at).literal(DaeLiteral::Integer(10000)))?;
        let fill =
            dae.expressions(|e| e.at(at).builtin(PureBuiltin::Fill, [value, first, second]))?;
        let start = dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(0.25)))?;
        dae.variables(|v| {
            v.parameter(
                VarName::new("p"),
                ty,
                at,
                rumoca_ir_dae::VariableAttributes {
                    binding: Some(fill),
                    ..Default::default()
                },
            )?;
            v.algebraic(
                VarName::new("x"),
                ty,
                at,
                rumoca_ir_dae::VariableAttributes {
                    start: Some(start),
                    ..Default::default()
                },
            )?;
            Ok(())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        for (index, bits) in [(0, (-0.0f64).to_bits()), (1, 0.25f64.to_bits())] {
            let id = view.variable_id(index).unwrap();
            let values = evaluator.initial_values(id).unwrap();
            assert_eq!(values.len(), 200_000);
            assert_eq!(values.runs().count(), 1);
            assert!(matches!(values.runs().next(), Some(NumericInitialRun::Repeat { value, count }) if count == 200_000 && value.to_bits() == bits));
            assert!(evaluator.initial_value(id).unwrap().iter().all(|value| value.to_bits() == bits));
        }
    });
}
