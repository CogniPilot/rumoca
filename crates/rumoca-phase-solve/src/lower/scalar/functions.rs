//! Function calls, record projections, and function loops.
//!
//! A Solve program has no call op: every call is lowered through the callee's
//! own checked result definition, with the caller's arguments bound on an
//! explicit stack so a parameter cannot escape the call that supplies it.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn function_fold_parameter(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let values = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| (*active == fold).then_some(values))
            .ok_or_else(|| {
                LowerError::contract("function loop parameter escaped its checked fold", span)
            })?;
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function loop parameter scalar is out of range", span)
            })
    }

    pub(super) fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .ok_or_else(|| LowerError::contract("record field is out of range", span))?,
                scalar,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                if self.function_arguments.len() >= 256 {
                    return Err(LowerError::non_computable(
                        "function lowering exceeded the checked recursion limit",
                        span,
                    ));
                }
                let result = self.function_result(function, output, span)?;
                self.function_arguments
                    .push((function, arguments.iter().collect()));
                let lowered = self.record_field(result, field, scalar, span);
                self.function_arguments.pop();
                lowered
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar, span)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                let fallback = operands
                    .get(operands.len() - 1)
                    .expect("checked conditional has a fallback");
                let mut selected = self.record_field(fallback, field, scalar, span)?;
                for ordinal in (0..operands.len() - 1).step_by(2).rev() {
                    let condition = self.expression(
                        operands
                            .get(ordinal)
                            .expect("checked conditional condition ordinal"),
                        0,
                    )?;
                    let value = self.record_field(
                        operands
                            .get(ordinal + 1)
                            .expect("checked conditional value ordinal"),
                        field,
                        scalar,
                        span,
                    )?;
                    selected = self.select(condition, value, selected, span)?;
                }
                Ok(selected)
            }
            _ => Err(LowerError::contract(
                "record field has no checked aggregate definition",
                span,
            )),
        }
    }

    pub(super) fn function_fold_output(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| {
                (0..scalar_count(self.view, initial))
                    .map(|element| self.expression(initial, element))
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        let structured = domain.structured();
        let point_count = structured.scalar_count().map_err(|error| {
            LowerError::contract(
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        for point in 0..point_count {
            let indices = structured
                .index_tuple_at(point)
                .expect("checked function fold domain remains valid")
                .expect("checked function fold point is in range");
            self.domain_points.push((fold_view.domain(), indices));
            self.function_fold_values.push((fold, values));
            let updates = fold_view
                .update_values()
                .rhs_iter()
                .map(|update| {
                    (0..scalar_count(self.view, update))
                        .map(|element| self.expression(update, element))
                        .collect::<Result<Vec<_>, _>>()
                })
                .collect::<Result<Vec<_>, _>>();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("function fold frame was just pushed");
            self.domain_points.pop();
            values = updates?;
            debug_assert_eq!(
                previous.len(),
                values.len(),
                "checked DAE fold preserves carried arity"
            );
        }
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function fold output scalar is out of range", span)
            })
    }

    pub(super) fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let Some((function, arguments)) = self.function_arguments.pop() else {
            return Err(LowerError::non_computable(
                "function parameter escaped its checked call owner",
                span,
            ));
        };
        let argument = (function == parameter.function())
            .then(|| arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::non_computable(
                    "function parameter escaped its checked call owner",
                    span,
                )
            })
            .and_then(|argument| self.expression(argument, scalar));
        self.function_arguments.push((function, arguments));
        lowered
    }

    pub(super) fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        self.function_arguments
            .push((function, arguments.iter().collect()));
        let lowered = self.expression(result, scalar);
        self.function_arguments.pop();
        lowered
    }

    /// Resolve the checked result definition one call lowers through.
    ///
    /// Solve executes only programs it owns. An MLS §12.9 external body is
    /// foreign code with no Solve op, so lowering fails with the call's exact
    /// provenance rather than emitting a substitute value.
    fn function_result(
        &self,
        function: dae::FunctionId<'dae>,
        output: u32,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        let definition = self
            .view
            .function(function)
            .ok_or_else(|| LowerError::contract("function identity does not resolve", span))?;
        if let Some(external) = definition.external() {
            return Err(LowerError::non_computable(
                format!(
                    "external {} function `{}` calls `{}`, which the Solve runtime cannot execute",
                    external.language().as_str(),
                    definition.name(),
                    external.symbol()
                ),
                span,
            ));
        }
        definition
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| LowerError::contract("function result ordinal is out of range", span))
    }
}
