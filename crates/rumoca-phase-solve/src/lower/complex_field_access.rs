use super::*;

impl LowerBuilder<'_> {
    pub(in crate::lower) fn lower_complex_operator_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let (re, im) = match base {
            // MLS operator overloading for Complex numbers is flattened into
            // ordinary expression trees. Projected `re/im` access must recover
            // the selected component from the complex arithmetic result.
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                let (lhs_re, lhs_im) =
                    self.lower_complex_operand_parts(lhs, *span, scope, call_depth)?;
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                let op = match op {
                    rumoca_core::OpBinary::Add => BinaryOp::Add,
                    rumoca_core::OpBinary::Sub => BinaryOp::Sub,
                    rumoca_core::OpBinary::Mul => BinaryOp::Mul,
                    rumoca_core::OpBinary::Div => BinaryOp::Div,
                    _ => return Ok(None),
                };
                self.lower_complex_binary_parts(op, lhs_re, lhs_im, rhs_re, rhs_im, *span)?
            }
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                span,
            } => {
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                (
                    self.emit_unary_at(UnaryOp::Neg, rhs_re, *span)?,
                    self.emit_unary_at(UnaryOp::Neg, rhs_im, *span)?,
                )
            }
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => {
                let Some(op) = complex_operator_call_op(name.as_str()) else {
                    return Ok(None);
                };
                let lhs = args.first().ok_or_else(|| {
                    LowerError::contract_violation(
                        format!("{} requires lhs for complex operator call", name.as_str()),
                        *span,
                    )
                })?;
                let rhs = args.get(1).ok_or_else(|| {
                    LowerError::contract_violation(
                        format!("{} requires rhs for complex operator call", name.as_str()),
                        *span,
                    )
                })?;
                let (lhs_re, lhs_im) =
                    self.lower_complex_operand_parts(lhs, *span, scope, call_depth)?;
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                self.lower_complex_binary_parts(op, lhs_re, lhs_im, rhs_re, rhs_im, *span)?
            }
            _ => return Ok(None),
        };
        Ok(Some(if field == "re" { re } else { im }))
    }

    pub(in crate::lower) fn lower_complex_binary_parts(
        &mut self,
        op: BinaryOp,
        lhs_re: Reg,
        lhs_im: Reg,
        rhs_re: Reg,
        rhs_im: Reg,
        span: rumoca_core::Span,
    ) -> Result<(Reg, Reg), LowerError> {
        match op {
            BinaryOp::Add => Ok((
                self.emit_binary_at(BinaryOp::Add, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Add, lhs_im, rhs_im, span)?,
            )),
            BinaryOp::Sub => Ok((
                self.emit_binary_at(BinaryOp::Sub, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Sub, lhs_im, rhs_im, span)?,
            )),
            BinaryOp::Mul => {
                let ac = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let bd = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let ad = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let bc = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                Ok((
                    self.emit_binary_at(BinaryOp::Sub, ac, bd, span)?,
                    self.emit_binary_at(BinaryOp::Add, ad, bc, span)?,
                ))
            }
            BinaryOp::Div => {
                let rr2 = self.emit_binary_at(BinaryOp::Mul, rhs_re, rhs_re, span)?;
                let ri2 = self.emit_binary_at(BinaryOp::Mul, rhs_im, rhs_im, span)?;
                let denom = self.emit_binary_at(BinaryOp::Add, rr2, ri2, span)?;
                let lhs_rr = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let lhs_ri = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let li_rr = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                let li_ri = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let re_num = self.emit_binary_at(BinaryOp::Add, lhs_rr, li_ri, span)?;
                let im_num = self.emit_binary_at(BinaryOp::Sub, li_rr, lhs_ri, span)?;
                Ok((
                    self.emit_binary_at(BinaryOp::Div, re_num, denom, span)?,
                    self.emit_binary_at(BinaryOp::Div, im_num, denom, span)?,
                ))
            }
            _ => Err(LowerError::contract_violation(
                format!(
                    "complex operator call mapped to unsupported binary op {}",
                    op.kind_name()
                ),
                span,
            )),
        }
    }
}
