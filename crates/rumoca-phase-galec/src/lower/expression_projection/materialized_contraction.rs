//! Lowering one tensor contraction into a materialized accumulation loop.
//!
//! The contraction node is where the shape of the emitted loops is decided:
//! its extents, its operands, whether an intermediate tensor is worth
//! materializing ([`composed_left_contraction`]), and, once its operands are
//! projected, whether the loop it issues is a whole row of a matrix product
//! that a target may accumulate at once
//! ([`issue_real_matrix_multiply_occurrence`]). Every
//! one of those answers is written into the IR here, so no later stage has to
//! recover it from the statements.

use super::*;

/// The loop one materialized contraction runs over its contracted index.
struct ContractionLoop<'a> {
    iterator: &'a gast::Name,
    start: i64,
    extent: u32,
    span: Span,
}

/// The loop over one contracted index, carrying the exact matrix-product
/// occurrence it legalizes when the contraction that filled it issued one.
fn contraction_loop(
    body: Vec<gast::Spanned<gast::Statement>>,
    shape: &ContractionLoop<'_>,
    occurrence: Option<gast::RealMatrixMultiplyOccurrenceContract>,
) -> gast::Spanned<gast::Statement> {
    let loop_ = gast::ForLoop::new(
        Some(shape.iterator.clone()),
        gast::Expression::Integer(shape.start),
        None,
        gast::Expression::Integer(i64::from(shape.extent)),
        body,
    );
    let loop_ = match occurrence {
        Some(occurrence) => loop_.with_real_matrix_multiply_occurrence(occurrence),
        None => loop_,
    };
    gast::Spanned::new(gast::Statement::for_loop(loop_), shape.span)
}

/// The loop body that fills one composed contraction's intermediate tensor,
/// with the matrix-product occurrence it legalizes.
struct IntermediateFill {
    statements: Vec<gast::Spanned<gast::Statement>>,
    occurrence: Option<gast::RealMatrixMultiplyOccurrenceContract>,
}

/// One lowered materialized contraction: its value and the matrix-product
/// occurrence issued for whichever node owns its legalization loop.
struct MaterializedContraction {
    value: TypedExpression,
    occurrence: Option<gast::RealMatrixMultiplyOccurrenceContract>,
}

fn additive_identity(scalar_type: gast::ScalarType) -> gast::Expression {
    match scalar_type {
        gast::ScalarType::Real => gast::Expression::Real(0.0),
        gast::ScalarType::Integer => gast::Expression::Integer(0),
        gast::ScalarType::Boolean => {
            unreachable!("checked tensor contractions are numeric")
        }
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_dot_product(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        extent: u32,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let fold_projection = self.materialize_function_values
            && (self.contains_function_fold_projection(lhs)
                || self.contains_function_fold_projection(rhs));
        if self.materialize_function_values && !fold_projection {
            return self.lower_materialized_contraction(
                lhs,
                rhs,
                TensorContraction {
                    extent,
                    lhs_outer: Vec::new(),
                    rhs_outer: Vec::new(),
                    lhs_matrix: false,
                    rhs_matrix: false,
                },
                scalar_type,
                span,
            );
        }
        let mut terms = Vec::with_capacity(extent as usize);
        for index in 1..=extent {
            let index = [gast::Expression::Integer(i64::from(index))];
            let lhs = self.lower_at(lhs, &index)?;
            let rhs = self.lower_at(rhs, &index)?;
            terms.push(lower_binary(
                dae::BinaryOperator::Multiply,
                lhs,
                rhs,
                scalar_type,
                span,
            )?);
        }
        let expression = self.finish_contraction_terms(
            terms,
            scalar_type,
            "zero-dot-product",
            "zero-length dot product requires an explicit additive identity",
            span,
        )?;
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            span,
        )
    }

    pub(super) fn lower_tensor_contraction(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        contraction: TensorContraction,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let fold_projection = self.materialize_function_values
            && (self.contains_function_fold_projection(lhs)
                || self.contains_function_fold_projection(rhs));
        if self.materialize_function_values && !fold_projection {
            return self.lower_materialized_contraction(lhs, rhs, contraction, scalar_type, span);
        }
        let mut terms = Vec::with_capacity(contraction.extent as usize);
        for contracted in 1..=contraction.extent {
            let contracted = gast::Expression::Integer(i64::from(contracted));
            let (lhs_indices, rhs_indices) = contraction_indices(&contraction, contracted);
            let lhs = self.lower_at(lhs, &lhs_indices)?;
            let rhs = self.lower_at(rhs, &rhs_indices)?;
            terms.push(lower_binary(
                dae::BinaryOperator::Multiply,
                lhs,
                rhs,
                scalar_type,
                span,
            )?);
        }
        let expression = self.finish_contraction_terms(
            terms,
            scalar_type,
            "zero-contraction",
            "zero-length tensor contraction needs an additive identity",
            span,
        )?;
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            span,
        )
    }

    fn lower_materialized_contraction(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        contraction: TensorContraction,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        self.lower_materialized_contraction_into(
            ContractionAccumulator::Fresh,
            lhs,
            rhs,
            contraction,
            scalar_type,
            span,
        )
        .map(|lowered| lowered.value)
    }

    /// Lower one materialized contraction into `accumulator`.
    ///
    /// A composed contraction (`A*X*A'`) decides here, before either operand
    /// is projected, that its left operand is an intermediate tensor worth an
    /// explicit IR value: the intermediate is declared with the contracted
    /// extent as its shape, the left contraction accumulates straight into the
    /// element each contracted value owns, and the loop over the contracted
    /// index that fills it is emitted beside the accumulation instead of
    /// inside it. The decision reads the checked DAE
    /// ([`composed_left_contraction`]); no statement is inspected or split.
    fn lower_materialized_contraction_into(
        &mut self,
        accumulator: ContractionAccumulator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        contraction: TensorContraction,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<MaterializedContraction, GalecTargetError> {
        if scalar_type == gast::ScalarType::Real && contraction.extent == 0 {
            return self.lower_empty_materialized_contraction(accumulator, scalar_type, span);
        }
        let (first_product, accumulator, element) =
            self.initialize_contraction_accumulator(accumulator, scalar_type, span);
        let iterator = gast::Name::ident(format!(
            "rumoca_{}_contracted_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;

        let contracted = gast::Expression::Ref(gast::Reference::local(iterator.clone()));
        let (lhs_indices, rhs_indices) = contraction_indices(&contraction, contracted.clone());
        self.loop_index_bounds.push(LoopIndexBound {
            name: iterator.clone(),
            minimum: 1,
            maximum: i64::from(contraction.extent),
        });
        let body_start = self.pending_prefix_statements.len();
        let composed = self.composed_contraction(lhs, &contraction, &contracted);
        let (intermediate, lhs) = match composed {
            Some(composed) => {
                self.lower_intermediate_tensor(composed, &contracted, &iterator, contraction.extent)
            }
            None => (None, self.lower_at(lhs, &lhs_indices)),
        };
        let rhs = self.lower_at(rhs, &rhs_indices);
        self.loop_index_bounds.pop();
        let (lhs, rhs) = (lhs?, rhs?);
        let product = lower_binary(dae::BinaryOperator::Multiply, lhs, rhs, scalar_type, span)?;
        let prefixes = self.pending_prefix_statements.split_off(body_start);
        let mut before = Vec::new();
        let intermediate = intermediate.map(|fill| {
            let (hoisted, body) = user_functions::partition_tensor_prefixes(
                fill.statements,
                std::slice::from_ref(&iterator),
            );
            before.extend(hoisted);
            IntermediateFill {
                statements: body,
                occurrence: fill.occurrence,
            }
        });
        let (hoisted, body) =
            user_functions::partition_tensor_prefixes(prefixes, std::slice::from_ref(&iterator));
        before.extend(hoisted);
        self.pending_prefix_statements.extend(before);
        let first_iteration =
            first_product.then(|| instantiate_iteration(&body, &product, &iterator, 1));
        // Only an exact single-product loop is eligible for the retained row
        // plan. Any prefix statement would need its own evaluation-order plan;
        // keeping that legalization is safer than silently changing it.
        let occurrence = element.as_ref().and_then(|element| {
            (body.is_empty() && intermediate.is_none()).then_some(())?;
            let seed = match &first_iteration {
                Some((_, value)) => gast::RealMatrixMultiplySeed::FirstProduct {
                    value: value.clone(),
                },
                None => gast::RealMatrixMultiplySeed::PositiveZero,
            };
            issue_real_matrix_multiply_occurrence(
                self.arithmetic.source_real(),
                element,
                &iterator,
                contraction.extent,
                &product_lhs(&product)?,
                &product_rhs(&product)?,
                seed,
            )
        });
        let shape = ContractionLoop {
            iterator: &iterator,
            start: if first_product { 2 } else { 1 },
            extent: contraction.extent,
            span,
        };
        if let Some(fill) = intermediate {
            self.pending_prefix_statements.push(contraction_loop(
                fill.statements,
                &ContractionLoop { start: 1, ..shape },
                fill.occurrence,
            ));
        }
        if let Some((mut seed_body, seed_product)) = first_iteration {
            seed_body.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: accumulator.clone(),
                    value: seed_product,
                },
                span,
            ));
            self.pending_prefix_statements.extend(seed_body);
        }
        self.emit_contraction_loop(body, &accumulator, product, &shape);
        Ok(MaterializedContraction {
            value: TypedExpression {
                expression: gast::Expression::Ref(accumulator),
                scalar_type,
            },
            occurrence,
        })
    }

    fn lower_empty_materialized_contraction(
        &mut self,
        accumulator: ContractionAccumulator,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<MaterializedContraction, GalecTargetError> {
        let identity = self.empty_real_contraction(span)?;
        let expression = match accumulator {
            ContractionAccumulator::Fresh => identity,
            ContractionAccumulator::Element(element) => {
                self.pending_prefix_statements.push(gast::Spanned::new(
                    gast::Statement::Assignment {
                        target: element.reference.clone(),
                        value: identity,
                    },
                    span,
                ));
                gast::Expression::Ref(element.reference)
            }
        };
        Ok(MaterializedContraction {
            value: TypedExpression {
                expression,
                scalar_type,
            },
            occurrence: None,
        })
    }

    fn initialize_contraction_accumulator(
        &mut self,
        accumulator: ContractionAccumulator,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> (bool, gast::Reference, Option<Box<IntermediateElement>>) {
        let first_product = scalar_type == gast::ScalarType::Real
            && self.arithmetic.real_matrix_multiply()
                == rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct;
        let (accumulator, element) = match accumulator {
            ContractionAccumulator::Fresh => (
                gast::Reference::local(self.declare_contraction_accumulator(scalar_type, span)),
                None,
            ),
            ContractionAccumulator::Element(element) => (element.reference.clone(), Some(element)),
        };
        if !first_product {
            self.pending_prefix_statements.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: accumulator.clone(),
                    value: additive_identity(scalar_type),
                },
                span,
            ));
        }
        (first_product, accumulator, element)
    }

    /// The composed left contraction of this node, refused whenever its
    /// operands would not have taken the materialized path themselves.
    fn composed_contraction(
        &mut self,
        lhs: dae::ExprId<'dae>,
        contraction: &TensorContraction,
        contracted: &gast::Expression,
    ) -> Option<ComposedContraction<'dae>> {
        if self
            .assigned_primitive_expressions
            .read(lhs.index(), &self.conditional_activation_path)
            .is_some()
        {
            return None;
        }
        let composed = composed_left_contraction(self.view, lhs, contraction, contracted)?;
        let folded = self.contains_function_fold_projection(composed.lhs)
            || self.contains_function_fold_projection(composed.rhs);
        (self.materialize_function_values && !folded).then_some(composed)
    }

    /// Declare the intermediate tensor of a composed contraction and fill it,
    /// returning the statements that fill one contracted element and the
    /// element expression the composing product reads.
    fn lower_intermediate_tensor(
        &mut self,
        composed: ComposedContraction<'dae>,
        contracted: &gast::Expression,
        row: &gast::Name,
        extent: u32,
    ) -> (
        Option<IntermediateFill>,
        Result<TypedExpression, GalecTargetError>,
    ) {
        let ComposedContraction {
            lhs,
            rhs,
            contraction,
            scalar_type,
            span,
        } = composed;
        let name = self.declare_contraction_intermediate(scalar_type, extent, span);
        let element = IntermediateElement {
            reference: intermediate_element(&name, contracted),
            row: row.clone(),
            name,
            count: extent,
        };
        let start = self.pending_prefix_statements.len();
        let lowered = self.lower_materialized_contraction_into(
            ContractionAccumulator::Element(Box::new(element)),
            lhs,
            rhs,
            contraction,
            scalar_type,
            span,
        );
        let statements = self.pending_prefix_statements.split_off(start);
        let (occurrence, value) = match lowered {
            Ok(lowered) => (lowered.occurrence, Ok(lowered.value)),
            Err(error) => (None, Err(error)),
        };
        (
            Some(IntermediateFill {
                statements,
                occurrence,
            }),
            value,
        )
    }

    /// Declare the intermediate tensor of a composed contraction: one element
    /// per contracted value, live for exactly the loop that reads it.
    fn declare_contraction_intermediate(
        &mut self,
        scalar_type: gast::ScalarType,
        extent: u32,
        span: Span,
    ) -> gast::Name {
        let name = gast::Name::ident(format!(
            "rumoca_{}_contraction_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: name.clone(),
            dimensions: user_functions::dimensions(&[extent]),
            range: gast::RangeAttributes::default(),
            span,
        });
        name
    }

    fn declare_contraction_accumulator(
        &mut self,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> gast::Name {
        let accumulator = gast::Name::ident(format!(
            "rumoca_{}_contraction_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: accumulator.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        accumulator
    }

    fn emit_contraction_loop(
        &mut self,
        mut body: Vec<gast::Spanned<gast::Statement>>,
        accumulator: &gast::Reference,
        product: gast::Expression,
        shape: &ContractionLoop<'_>,
    ) {
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: accumulator.clone(),
                value: gast::Expression::binary(
                    gast::BinaryOp::Add,
                    gast::Expression::Ref(accumulator.clone()),
                    product,
                ),
            },
            shape.span,
        ));
        self.pending_prefix_statements
            .push(contraction_loop(body, shape, None));
    }

    fn finish_contraction_terms(
        &self,
        terms: Vec<gast::Expression>,
        scalar_type: gast::ScalarType,
        feature: &str,
        detail: &str,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if scalar_type != gast::ScalarType::Real {
            return sum_terms(terms, feature, detail, span);
        }
        if terms.is_empty() {
            return self.empty_real_contraction(span);
        }
        if self.arithmetic.real_matrix_multiply()
            == rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct
        {
            return sum_terms(terms, feature, detail, span);
        }
        Ok(terms
            .into_iter()
            .fold(gast::Expression::Real(0.0), |accumulator, product| {
                gast::Expression::binary(gast::BinaryOp::Add, accumulator, product)
            }))
    }

    fn empty_real_contraction(&self, span: Span) -> Result<gast::Expression, GalecTargetError> {
        match self.arithmetic.real_matrix_multiply() {
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct => {
                Err(unsupported(
                    "empty-first-product-matrix-multiply",
                    "a FirstProduct Real matrix product requires a non-empty inner domain"
                        .to_owned(),
                    span,
                ))
            }
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero => {
                Ok(gast::Expression::Real(0.0))
            }
        }
    }
}

fn product_lhs(product: &gast::Expression) -> Option<gast::Expression> {
    let gast::Expression::Binary {
        op: gast::BinaryOp::Mul,
        lhs,
        ..
    } = product
    else {
        return None;
    };
    Some(lhs.as_ref().clone())
}

fn product_rhs(product: &gast::Expression) -> Option<gast::Expression> {
    let gast::Expression::Binary {
        op: gast::BinaryOp::Mul,
        rhs,
        ..
    } = product
    else {
        return None;
    };
    Some(rhs.as_ref().clone())
}
