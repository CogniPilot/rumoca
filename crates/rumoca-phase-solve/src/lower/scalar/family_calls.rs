//! The residual program of an admitted algebraic family (SPEC_0043 §6c,
//! SOLVE-C70): `target - call(inputs)`, stored as one range in the target's
//! row-major order.

use super::*;
use crate::lower::typed_functions::AlgebraicFamilyForm;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn algebraic_family_program(
        mut self,
        family: dae::ContinuousFamilyId<'dae>,
        form: &AlgebraicFamilyForm<'dae>,
    ) -> Result<Option<Vec<solve::LinearOp>>, LowerError> {
        let span = form.provenance;
        let site = self
            .layout
            .pure_calls
            .borrow_mut()
            .register_algebraic_family(self.view, family, form)
            .map_err(|error| LowerError::contract(error.to_string(), span))?;
        // The residual's derivatives are taken through the owner's directional
        // relation; a body with an operation that has none (a product
        // reduction, for one) keeps its scalar rows, which differentiate
        // in place.
        if site.directional().is_none() {
            return Ok(None);
        }
        let input_starts = form
            .input_readers()
            .map(|reader| self.pack_expression(reader))
            .collect::<Result<Vec<_>, _>>()?;
        if input_starts.len() != site.inputs().len()
            || site.output_scalar_count() != Some(form.scalar_count)
        {
            return Err(LowerError::contract(
                "algebraic family call does not match its owner interface",
                span,
            ));
        }
        let call = self.registers(form.scalar_count, span)?;
        self.emit(solve::LinearOp::PureCall {
            dst_start: call,
            input_starts: input_starts.into_boxed_slice(),
            site,
        })?;
        let target = self.pack_expression(form.target_expression)?;
        let residual = self.registers(form.scalar_count, span)?;
        self.emit(solve::LinearOp::TensorBinary {
            dst_start: residual,
            op: solve::BinaryOp::Sub,
            lhs_start: target,
            rhs_start: call,
            count: form.scalar_count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        })?;
        self.emit(solve::LinearOp::StoreOutputRange {
            start: residual,
            count: form.scalar_count,
            stride: 1,
        })?;
        Ok(Some(solve::prune_dead_constants(self.finish_operations()?)))
    }

    /// `count` consecutive fresh registers, returning the first.
    fn registers(&mut self, count: usize, span: Span) -> Result<solve::Reg, LowerError> {
        let start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        Ok(start)
    }
}
