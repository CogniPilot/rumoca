//! Model-level Integer quotients and relations consume one typed expression owner.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn exact_integer_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Option<solve::Reg>, LowerError> {
        let span = self.node(expression).provenance().span();
        let Some((start, _)) = self.emit_exact_integer_expression(expression)? else {
            return Ok(None);
        };
        let offset = u32::try_from(scalar)
            .map_err(|_| LowerError::contract("typed expression scalar offset overflows", span))?;
        Ok(Some(start.checked_add(offset).ok_or_else(|| {
            LowerError::contract("typed expression result register overflows", span)
        })?))
    }

    pub(super) fn emit_exact_integer_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<
        Option<(
            solve::Reg,
            crate::lower::typed_functions::RegisteredCall<'dae>,
        )>,
        LowerError,
    > {
        // A call body expanded in place reads function-scoped values; its
        // Integer operations stay on the nested-call path, like nested calls.
        if !self.function_arguments.is_empty() {
            return Ok(None);
        }
        let node = self.node(expression);
        let span = node.provenance().span();
        let Some(form) =
            crate::lower::typed_functions::ExactExpressionForm::try_admit(self.view, expression)
                .map_err(|error| {
                    LowerError::contract(error.to_string(), error.source_span().unwrap_or(span))
                })?
        else {
            return Ok(None);
        };
        let registered = self
            .layout
            .pure_calls
            .borrow_mut()
            .register_exact_expression(self.view, &form)
            .map_err(|error| {
                LowerError::contract(error.to_string(), error.source_span().unwrap_or(span))
            })?;
        let inputs = form
            .input_readers()
            .map(|reader| {
                self.pack_typed_scalar_argument(
                    reader,
                    self.node(reader).value_type().scalar_type(),
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let count = registered
            .site
            .output_scalar_count()
            .ok_or_else(|| LowerError::contract("typed expression result width overflows", span))?;
        let start = self.register_range(count, span)?;
        self.ops.push(solve::LinearOp::PureCall {
            dst_start: start,
            input_starts: inputs.into_boxed_slice(),
            site: registered.site.clone(),
        });
        if !self.call_action_compilation && !registered.callee.assertions.is_empty() {
            self.schedule_typed_pure_call_assertions(
                super::call_scoped_actions::TypedCallInvocation::Expression(expression),
                &registered,
                span,
            )?;
        }
        Ok(Some((start, registered)))
    }
}
