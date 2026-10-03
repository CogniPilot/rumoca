//! The MLS 3.7 §3.7.2 event surface of `floor`, `ceil`, and `integer`.
//!
//! "div, ceil, floor, integer can only change values at events and will
//! trigger events as needed." An occurrence over a varying argument `x` that
//! analysis proved to own an event (outside `noEvent`/`smooth`, at model
//! scope) owns a root on `sin(pi*x) >= 0`, whose sign changes exactly where
//! `x` crosses an integer: the indicator the dynamic quotients own. Function
//! bodies are event-free and own nothing. `Integer(e)` of an enumeration value is
//! the ordinal conversion of MLS §4.9.5, not a step of a Real argument, so it
//! owns nothing either.

use super::*;

/// Build the root of one `floor`/`ceil`/`integer` occurrence when analysis
/// planned it; `source` is the occurrence's Flat argument list and `lowered`
/// the checked arguments built from it.
pub(super) fn own_integer_step<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    source: &[Expression],
    lowered: &[dae::ExprId<'dae>],
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let ([source], [argument]) = (source, lowered) else {
        return Ok(());
    };
    let planned = matches!(
        symbols
            .functions
            .expression_events
            .plan(provenance.span(), &[source]),
        Some(ExpressionEventPlan::IntegerStep)
    );
    if !planned || symbols.function_body.is_some() {
        return Ok(());
    }
    let argument_type =
        construction.expressions(|expressions| expressions.value_type(*argument, provenance))?;
    if argument_type.scalar_type() == dae::ScalarType::Enumeration {
        return Ok(());
    }
    let at =
        dae::DaeProvenance::generated(dae::DaeGeneration::RuntimeDiscontinuity, provenance.span())?;
    let indicator = construction.expressions(|expressions| {
        let pi = expressions
            .at(at)
            .literal(dae::DaeLiteral::Real(std::f64::consts::PI))?;
        let phase = expressions
            .at(at)
            .binary(dae::BinaryOperator::Multiply, pi, *argument)?;
        let sine = expressions.at(at).builtin(dae::PureBuiltin::Sin, [phase])?;
        let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
        expressions
            .at(at)
            .binary(dae::BinaryOperator::GreaterEqual, sine, zero)
    })?;
    lower_state_relation_root(construction, indicator, at)
}
