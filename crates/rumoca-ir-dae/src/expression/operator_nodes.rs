use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn unary(
        self,
        operator: UnaryOperator,
        operand: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.expr_type(operand, self.provenance)?.clone();
        let variability = self.storage.expr_variability(operand, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(operand, self.provenance)?;
        match operator {
            UnaryOperator::Not if ty.scalar_type() != ScalarType::Boolean => {
                return Err(type_mismatch(
                    ScalarType::Boolean,
                    ty.scalar_type(),
                    self.provenance,
                ));
            }
            UnaryOperator::Plus | UnaryOperator::Negate if !ty.scalar_type().is_numeric() => {
                return Err(DaeConstructionError::ExpectedNumeric {
                    found: ty.scalar_type(),
                    span: self.provenance.span(),
                });
            }
            _ => {}
        }
        let ty = self.storage.intern_type(ty, self.provenance)?;
        self.insert(
            ExprNode::Unary {
                operator,
                operand: operand.index(),
            },
            ty,
            variability,
            binder_domain,
        )
    }

    pub fn binary(
        self,
        operator: BinaryOperator,
        lhs: ExprId<'dae>,
        rhs: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let lhs_ty = self.storage.expr_type(lhs, self.provenance)?.clone();
        let rhs_ty = self.storage.expr_type(rhs, self.provenance)?.clone();
        let variability = self
            .storage
            .expr_variability(lhs, self.provenance)?
            .max(self.storage.expr_variability(rhs, self.provenance)?);
        let binder_domain = merged_binder_domain(self.storage, [lhs, rhs], self.provenance)?;
        let result = binary_result(operator, &lhs_ty, &rhs_ty, self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        self.insert(
            ExprNode::Binary {
                operator,
                lhs: lhs.index(),
                rhs: rhs.index(),
            },
            ty,
            variability,
            binder_domain,
        )
    }
    pub fn conditional(
        self,
        branches: impl IntoIterator<Item = (ExprId<'dae>, ExprId<'dae>)>,
        fallback: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let branches = branches.into_iter().collect::<Vec<_>>();
        if branches.is_empty() {
            return Err(invalid_arity(1, 0, self.provenance));
        }
        let mut result = self.storage.expr_type(fallback, self.provenance)?.clone();
        let mut variability = self.storage.expr_variability(fallback, self.provenance)?;
        let mut binder_domain = self.storage.expr_binder_domain(fallback, self.provenance)?;
        for (condition, value) in &branches {
            let condition_ty = self.storage.expr_type(*condition, self.provenance)?;
            if !condition_ty.is_scalar() || condition_ty.scalar_type() != ScalarType::Boolean {
                return Err(type_mismatch(
                    ScalarType::Boolean,
                    condition_ty.scalar_type(),
                    self.provenance,
                ));
            }
            result = common_value_type(
                &result,
                self.storage.expr_type(*value, self.provenance)?,
                self.provenance,
            )?;
            variability =
                variability.max(self.storage.expr_variability(*condition, self.provenance)?);
            variability = variability.max(self.storage.expr_variability(*value, self.provenance)?);
            binder_domain = merge_binder_domain(
                self.storage,
                binder_domain,
                self.storage
                    .expr_binder_domain(*condition, self.provenance)?,
                self.provenance,
            )?;
            binder_domain = merge_binder_domain(
                self.storage,
                binder_domain,
                self.storage.expr_binder_domain(*value, self.provenance)?,
                self.provenance,
            )?;
        }
        let operands = self.storage.expressions.push_operands(
            branches
                .into_iter()
                .flat_map(|(condition, value)| [condition.index(), value.index()])
                .chain(std::iter::once(fallback.index())),
            self.provenance,
        )?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        self.insert(
            ExprNode::Conditional { operands },
            ty,
            variability,
            binder_domain,
        )
    }
}
