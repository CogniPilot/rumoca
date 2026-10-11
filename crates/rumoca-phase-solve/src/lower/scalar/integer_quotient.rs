//! Model-level Integer quotients consume the shared typed expression owner.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn exact_integer_quotient(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Option<solve::Reg>, LowerError> {
        let node = self.node(expression);
        if node.value_type().scalar_type() != dae::ScalarType::Integer
            || !matches!(
                node.operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem,
                    ..
                }
            )
        {
            return Ok(None);
        }
        let span = node.provenance().span();
        let form = crate::lower::typed_functions::ExactExpressionForm::admit(self.view, expression)
            .map_err(|error| LowerError::contract(error.to_string(), span))?;
        let site = self
            .layout
            .pure_calls
            .borrow_mut()
            .register_exact_expression(self.view, &form)
            .map_err(|error| LowerError::contract(error.to_string(), span))?;
        let inputs = form
            .input_readers()
            .map(|reader| self.pack_expression(reader))
            .collect::<Result<Vec<_>, _>>()?;
        let count = site
            .output_scalar_count()
            .ok_or_else(|| LowerError::contract("typed expression result width overflows", span))?;
        let start = self.register_range(count, span)?;
        self.ops.push(solve::LinearOp::PureCall {
            dst_start: start,
            input_starts: inputs.into_boxed_slice(),
            site,
        });
        let offset = u32::try_from(scalar)
            .map_err(|_| LowerError::contract("typed expression scalar offset overflows", span))?;
        Ok(Some(start.checked_add(offset).ok_or_else(|| {
            LowerError::contract("typed expression result register overflows", span)
        })?))
    }
}
