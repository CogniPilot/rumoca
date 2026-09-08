use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn array(
        self,
        elements: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let elements = elements.into_iter().collect::<Vec<_>>();
        let Some(first) = elements.first().copied() else {
            return Err(DaeConstructionError::EmptyArray {
                span: self.provenance.span(),
            });
        };
        let first_provenance = self.storage.expr_provenance(first, self.provenance)?;
        let mut element_ty = self.storage.expr_type(first, first_provenance)?.clone();
        let mut variability = self.storage.expr_variability(first, first_provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, elements.iter().copied(), self.provenance)?;
        for element in &elements[1..] {
            let element_provenance = self.storage.expr_provenance(*element, self.provenance)?;
            element_ty = common_value_type(
                &element_ty,
                self.storage.expr_type(*element, element_provenance)?,
                element_provenance,
            )?;
            variability = variability.max(
                self.storage
                    .expr_variability(*element, element_provenance)?,
            );
        }
        let mut dimensions = Vec::with_capacity(element_ty.dimensions().len() + 1);
        dimensions.push(checked_u32(
            elements.len(),
            "array extent",
            self.provenance,
        )?);
        dimensions.extend_from_slice(element_ty.dimensions());
        let ty = self
            .storage
            .intern_type(element_ty.with_dimensions(dimensions), self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(elements.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(ExprNode::Array { operands }, ty, variability, binder_domain)
    }

    /// Construct a nonempty array in an exact assignment context.
    ///
    /// The target type is not trusted as the resulting expression type: an
    /// all-Integer literal assigned to a Real array remains an Integer array
    /// and the assignment owns the numeric widening. It is the authority for
    /// checking each member, in source order, so an invalid first member cannot
    /// make a later valid member look like the type error.
    pub(crate) fn array_for_expected_type(
        self,
        expected: ValueTypeId<'dae>,
        elements: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let elements = elements.into_iter().collect::<Vec<_>>();
        let Some(_) = elements.first() else {
            return Err(DaeConstructionError::EmptyArray {
                span: self.provenance.span(),
            });
        };
        let expected_array = self
            .storage
            .value_type_at(expected.index(), self.provenance)?
            .clone();
        let Some((expected_extent, element_dimensions)) = expected_array.dimensions().split_first()
        else {
            return Err(DaeConstructionError::ShapeMismatch {
                span: self.provenance.span(),
            });
        };
        if usize::try_from(*expected_extent).ok() != Some(elements.len()) {
            return Err(DaeConstructionError::ShapeMismatch {
                span: self.provenance.span(),
            });
        }

        let expected_element = expected_array.with_dimensions(element_dimensions.to_vec());
        let mut variability = ExpressionVariability::Constant;
        let mut any_real = false;
        for element in &elements {
            let member_at = self.storage.expr_provenance(*element, self.provenance)?;
            let found = self.storage.expr_type(*element, member_at)?;
            expect_array_member_compatible(&expected_element, found, member_at)?;
            any_real |= found.scalar_type() == ScalarType::Real;
            variability = variability.max(self.storage.expr_variability(*element, member_at)?);
        }

        let result_element = if expected_element.scalar_type() == ScalarType::Real && !any_real {
            ValueType::array(ScalarType::Integer, element_dimensions.to_vec())
        } else {
            expected_element
        };
        let mut dimensions = Vec::with_capacity(result_element.dimensions().len() + 1);
        dimensions.push(checked_u32(
            elements.len(),
            "array extent",
            self.provenance,
        )?);
        dimensions.extend_from_slice(result_element.dimensions());
        let ty = self
            .storage
            .intern_type(result_element.with_dimensions(dimensions), self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, elements.iter().copied(), self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(elements.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(ExprNode::Array { operands }, ty, variability, binder_domain)
    }

    /// Construct an empty array using its context-proven value type.
    ///
    /// An empty literal has no element expression from which to derive its
    /// scalar type or trailing dimensions, so its semantic owner must supply
    /// an array type whose outer extent is zero.
    pub fn empty_array(
        self,
        value_type: ValueTypeId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self
            .storage
            .value_type_at(value_type.index(), self.provenance)?;
        if ty.dimensions().first() != Some(&0) {
            return Err(DaeConstructionError::ShapeMismatch {
                span: self.provenance.span(),
            });
        }
        let operands = self
            .storage
            .expressions
            .push_operands(std::iter::empty(), self.provenance)?;
        self.insert(
            ExprNode::Array { operands },
            value_type,
            ExpressionVariability::Constant,
            None,
        )
    }
    pub fn range(
        self,
        start: ExprId<'dae>,
        explicit_step: Option<ExprId<'dae>>,
        stop: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let start_value = range_bound(self.storage, start, self.provenance)?;
        let explicit_step_value = explicit_step
            .map(|step| range_bound(self.storage, step, self.provenance))
            .transpose()?;
        let stop_value = range_bound(self.storage, stop, self.provenance)?;
        let step_value = match explicit_step_value {
            Some((0, provenance)) => {
                return Err(DaeConstructionError::ZeroRangeStep {
                    span: provenance.span(),
                });
            }
            Some((value, _)) => value,
            None => 1,
        };
        let extent = range_extent(start_value.0, step_value, stop_value.0, self.provenance)?;
        let ty = self.storage.intern_type(
            ValueType::array(ScalarType::Integer, [extent]),
            self.provenance,
        )?;
        self.insert(
            ExprNode::Range {
                start: start.index(),
                explicit_step: explicit_step.map(ExprId::index),
                stop: stop.index(),
            },
            ty,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn comprehension(
        self,
        domain: DomainId<'dae>,
        body: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let domain_extents = self.storage.domain_extents(domain, self.provenance)?;
        let body_ty = self.storage.expr_type(body, self.provenance)?.clone();
        let variability = self.storage.expr_variability(body, self.provenance)?;
        self.storage
            .expect_domain_expression(body, domain, self.provenance)?;
        let mut dimensions = Vec::with_capacity(body_ty.dimensions().len() + domain_extents.len());
        dimensions.extend_from_slice(domain_extents);
        dimensions.extend_from_slice(body_ty.dimensions());
        let ty = self
            .storage
            .intern_type(body_ty.with_dimensions(dimensions), self.provenance)?;
        let binder_domain = self.storage.domain_parent(domain, self.provenance)?;
        self.insert(
            ExprNode::Comprehension {
                domain: domain.index(),
                body: body.index(),
            },
            ty,
            variability,
            binder_domain,
        )
    }
}

fn expect_array_member_compatible(
    expected: &ValueType,
    found: &ValueType,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let integer_to_real = found.dimensions() == expected.dimensions()
        && expected.scalar_type() == ScalarType::Real
        && found.scalar_type() == ScalarType::Integer;
    if found == expected || integer_to_real {
        return Ok(());
    }
    if found.scalar_type() != expected.scalar_type() {
        return Err(DaeConstructionError::TypeMismatch {
            expected: expected.scalar_type(),
            found: found.scalar_type(),
            span: at.span(),
        });
    }
    Err(DaeConstructionError::ShapeMismatch { span: at.span() })
}

fn range_bound<'dae>(
    storage: &Storage,
    expression: ExprId<'dae>,
    owner: DaeProvenance,
) -> Result<(i64, DaeProvenance), DaeConstructionError> {
    let provenance = storage.expr_provenance(expression, owner)?;
    storage.expect_closed_expression(expression, provenance)?;
    match storage.expressions.nodes.get(expression.index() as usize) {
        Some(ExprNode::Literal(DaeLiteral::Integer(value))) => Ok((*value, provenance)),
        Some(_) => Err(DaeConstructionError::InvalidRangeBound {
            span: provenance.span(),
        }),
        None => Err(unknown("expression", expression.index(), owner)),
    }
}
