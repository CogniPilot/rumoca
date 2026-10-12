//! Exact Integer quotient admission must not mint a continuous event surface.
use super::*;

fn discrete_quotient(builtin: PureBuiltin) -> Dae {
    let source = TestSource::new("input Integer n; input Integer d; div(n, d)");
    let at = source.source("div(n, d)", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let (n, d) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("n"),
                    integer,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("d"),
                    integer,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        dae.expressions(|expressions| {
            let n = expressions.at(at).coordinate(CoordinateInput::Input(n))?;
            let d = expressions.at(at).coordinate(CoordinateInput::Input(d))?;
            let value = expressions.at(at).builtin(builtin, [n, d])?;
            assert_eq!(
                expressions.value_type(value, at)?,
                ValueType::scalar(ScalarType::Integer)
            );
            assert_eq!(
                expressions.variability(value, at)?,
                ExpressionVariability::Discrete
            );
            Ok(())
        })
    })
    .expect("Integer operands are discrete-time, including the divisor")
}

#[test]
fn discrete_integer_quotients_reissue_exact_type_and_variability_without_event_roots() {
    for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
        let dae = discrete_quotient(builtin);
        dae.inspect(|view| {
            assert_eq!(view.root_count(), 0);
            assert_eq!(view.runtime_quotient_owner_count(), 0);
        });
        let json = serde_json::to_string(&dae).unwrap();
        let decoded: Dae = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
        let bytes = bincode::serialize(&dae).unwrap();
        let decoded: Dae = bincode::deserialize(&bytes).unwrap();
        assert_eq!(bincode::serialize(&decoded).unwrap(), bytes);
    }
}

#[test]
fn discrete_integer_quotients_do_not_domain_prove_a_wide_difference_through_real() {
    let source = TestSource::new("div(7, 9007199254740993 - 9007199254740992)");
    let at = source.source("div(7, 9007199254740993 - 9007199254740992)", 0);
    Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let n = expressions.at(at).literal(DaeLiteral::Integer(7))?;
            let large = expressions
                .at(at)
                .literal(DaeLiteral::Integer(9_007_199_254_740_993))?;
            let prior = expressions
                .at(at)
                .literal(DaeLiteral::Integer(9_007_199_254_740_992))?;
            let divisor = expressions
                .at(at)
                .binary(BinaryOperator::Subtract, large, prior)?;
            for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
                expressions.at(at).builtin(builtin, [n, divisor])?;
            }
            Ok(())
        })
    })
    .expect("the exact divisor is one, not rounded zero");
}

#[test]
fn discrete_integer_quotients_retain_zero_and_non_integer_refusals() {
    let source = TestSource::new("div(7, 0); div(time, 7); div(7, time)");
    let at = source.source("div(7, 0)", 0);
    Dae::construct(source.map, |dae| dae.expressions(|expressions| {
        let n = expressions.at(at).literal(DaeLiteral::Integer(7))?;
        let zero = expressions.at(at).literal(DaeLiteral::Integer(0))?;
        let time = expressions.at(at).coordinate(CoordinateInput::Time)?;
        for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
            assert!(matches!(expressions.at(at).builtin(builtin, [n, zero]), Err(DaeConstructionError::UndefinedBuiltinDomain { span, .. }) if span == at.span()));
            for arguments in [[time, n], [n, time]] {
                assert!(matches!(expressions.at(at).builtin(builtin, arguments), Err(DaeConstructionError::NonStaticDiscontinuity { span, .. }) if span == at.span()));
            }
        }
        Ok(())
    })).expect("rejected forms leave construction valid");
}
