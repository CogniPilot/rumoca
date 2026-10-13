use super::*;
use rumoca_core::{SourceMap, VarName};

#[test]
fn exact_source_binding_precedence_preserves_typed_values() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "exact_source.mo",
        "parameter Integer p=9007199254740993; parameter Boolean enabled=true;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 35)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        let (binding, ignored_start, enabled) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(9_007_199_254_740_993))?,
                expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::MAX))?,
                expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?,
            ))
        })?;
        model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                integer,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    start: Some(ignored_start),
                    ..Default::default()
                },
            )?;
            variables.parameter(
                VarName::new("enabled"),
                boolean,
                at,
                dae::VariableAttributes {
                    binding: Some(enabled),
                    ..Default::default()
                },
            )?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let variables = lower_variables(view, &[]).unwrap();
        let integer = variables
            .iter()
            .find(|variable| variable.name == "p")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Integer(values) = &integer.start else {
            panic!("exact Integer owner")
        };
        assert_eq!(values.value(0), Some(&9_007_199_254_740_993));
        assert!(values.is_repeat());
        let boolean = variables
            .iter()
            .find(|variable| variable.name == "enabled")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Boolean(values) = &boolean.start else {
            panic!("exact Boolean owner")
        };
        assert_eq!(values.value(0), Some(&true));
    });
}
