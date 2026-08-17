use super::*;
use crate::ConditionInput;
use crate::expression::PureBuiltin;

impl<'dae> DaeConstruction<'dae> {
    /// Construct a scalar runtime quotient together with its checked state-event surface.
    ///
    /// MLS discontinuities occur whenever `lhs / rhs` crosses an integer. The
    /// continuous indicator `sin(pi * lhs / rhs)` has exactly those integer
    /// quotient boundaries as zeros. Requiring a finite, nonzero static
    /// divisor keeps this compact owner both defined and scalar; a varying
    /// divisor or shaped quotient needs a different checked owner.
    pub fn runtime_quotient(
        &mut self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let generated =
            DaeProvenance::generated(DaeGeneration::RuntimeDiscontinuity, provenance.span())?;
        let quotient = self.expressions(|expressions| {
            expressions
                .at(provenance)
                .checked_runtime_quotient(builtin, arguments)
        })?;
        let indicator = self.expressions(|expressions| {
            let ratio = expressions.at(generated).binary(
                BinaryOperator::Divide,
                arguments[0],
                arguments[1],
            )?;
            let pi = expressions
                .at(generated)
                .literal(DaeLiteral::Real(std::f64::consts::PI))?;
            let phase = expressions
                .at(generated)
                .binary(BinaryOperator::Multiply, pi, ratio)?;
            expressions.at(generated).builtin(PureBuiltin::Sin, [phase])
        })?;
        let relation_expression = self.expressions(|expressions| {
            let zero = expressions.at(generated).literal(DaeLiteral::Real(0.0))?;
            expressions
                .at(generated)
                .binary(BinaryOperator::GreaterEqual, indicator, zero)
        })?;
        let relation =
            self.conditions(|conditions| conditions.relation(relation_expression, generated))?;
        let activation = self.conditions(|conditions| {
            let activation = conditions.reserve(generated)?;
            conditions.define(activation, ConditionInput::Always, generated)?;
            Ok(activation)
        })?;
        self.conditions(|conditions| conditions.root(relation, activation, generated))?;
        Ok(quotient)
    }

    /// Construct a runtime quotient inside one exact function body.
    ///
    /// MLS §3.7.2 exempts function bodies from event generation: the
    /// quotient keeps the same time-invariant divisor admission, but no
    /// discontinuity root exists to own — a root would smuggle a
    /// function-scope expression into the model's condition system. The
    /// `FunctionBody` capability is the SPEC_0036 proof that a body is
    /// open, and the constructed expression is validated against that exact
    /// body — model-scope runtime coordinates are rejected, never silently
    /// left eventless.
    pub fn function_runtime_quotient(
        &mut self,
        body: &FunctionBody<'dae>,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let quotient = self.expressions(|expressions| {
            expressions
                .at(provenance)
                .checked_runtime_quotient(builtin, arguments)
        })?;
        expect_function_body_expression(self.storage, body, quotient, provenance)?;
        validate_function_value_reads(self.storage, body, quotient, provenance)?;
        Ok(quotient)
    }
}
