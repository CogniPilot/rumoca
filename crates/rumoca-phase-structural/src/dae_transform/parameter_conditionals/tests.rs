//! Checked Boolean selections retain only established invariance/event facts.
use super::*;
use rumoca_core::{SourceMap, Span, VarName};

#[test]
fn selected_boolean_guards_preserve_parameter_and_event_constancy() {
    let (model, guards) = guard_model();
    model.inspect(|view| {
        let context = FunctionCallContext::default();
        let parameter = view.expression_id(guards[0] as usize).unwrap();
        let relation = view.expression_id(guards[1] as usize).unwrap();
        assert!(invariant_guard(view, &context, parameter));
        assert!(piecewise_constant_guard(view, &context, parameter));
        assert!(!invariant_guard(view, &context, relation));
        assert!(piecewise_constant_guard(view, &context, relation));
    });
}

#[test]
fn selected_guards_cannot_manufacture_continuous_event_ownership() {
    let (model, guards) = guard_model();
    model.inspect(|view| {
        let context = FunctionCallContext::default();
        for ordinal in &guards[2..] {
            let guard = view.expression_id(*ordinal as usize).unwrap();
            assert!(!invariant_guard(view, &context, guard));
            assert!(!piecewise_constant_guard(view, &context, guard));
        }
    });
}

fn guard_model() -> (dae::Dae, Vec<u32>) {
    let text = "parameter Boolean c; Real x; x>0; x>1; noEvent(x>0); smooth(0,x)>0;";
    let mut sources = SourceMap::new();
    let source = sources.add("SelectedGuards.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let mut guards = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let (boolean, real) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let (c, x) = model.variables(|variables| {
            Ok((
                variables.parameter(VarName::new("c"), boolean, at, Default::default())?,
                variables.state(VarName::new("x"), real, at, Default::default())?,
            ))
        })?;
        let expressions = model.expressions(|e| {
            let c = e.at(at).coordinate(dae::CoordinateInput::Parameter(c))?;
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let order = e.at(at).literal(dae::DaeLiteral::Integer(0))?;
            let rooted = e.at(at).binary(dae::BinaryOperator::Greater, x, zero)?;
            let unowned = e.at(at).binary(dae::BinaryOperator::Greater, x, one)?;
            let no_event = e.at(at).builtin(dae::PureBuiltin::NoEvent, [rooted])?;
            let smooth = e.at(at).builtin(dae::PureBuiltin::Smooth, [order, x])?;
            let smooth = e
                .at(at)
                .binary(dae::BinaryOperator::Greater, smooth, zero)?;
            Ok([c, rooted, unowned, no_event, smooth])
        })?;
        model.conditions(|conditions| {
            let relation = conditions.relation(expressions[1], at)?;
            let activation = conditions.reserve(at)?;
            conditions.define(activation, dae::ConditionInput::Relation(relation), at)?;
            conditions.root(relation, activation, at).map(|_| ())
        })?;
        for condition in expressions {
            let selection = model.expressions(|e| {
                let yes = e.at(at).literal(dae::DaeLiteral::Boolean(true))?;
                let no = e.at(at).literal(dae::DaeLiteral::Boolean(false))?;
                e.at(at).conditional([(condition, yes)], no)
            })?;
            guards.push(selection.index());
        }
        Ok(())
    })
    .unwrap();
    (model, guards)
}
