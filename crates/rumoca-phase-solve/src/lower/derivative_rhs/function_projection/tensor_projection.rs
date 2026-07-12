use super::*;

impl<'a> FunctionProjectionAnalysis<'a> {
    pub(super) fn project_function_call_value(
        &self,
        expr: &rumoca_core::Expression,
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let mut call = self.substitute(expr, scope)?;
        if call.span().is_none() {
            call = call.with_span(owner_span);
        }
        Ok(self
            .function_call_outputs_with_owner(&call, depth + 1, owner_span)?
            .and_then(|outputs| outputs.get(flat_index).map(|output| output.expr.clone())))
    }

    pub(super) fn project_indexed_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let span = inherited_projection_source_span(expr.span(), owner_span);
        let indices = required_flat_index_to_subscripts(dims, flat_index, span)?;
        let mut subscripts = projection_vec_with_capacity(
            indices.len(),
            "projected expression subscript count",
            span,
        )?;
        for idx in indices {
            subscripts.push(checked_generated_subscript_from_usize(
                idx,
                span,
                "projected expression index subscript",
            )?);
        }
        Ok(Some(rumoca_core::Expression::Index {
            base: Box::new(self.substitute(expr, scope)?),
            subscripts,
            span,
        }))
    }

    pub(super) fn project_binary_elementwise(
        &self,
        op: OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let lhs_dims = self.known_expr_dims(lhs, ctx.scope, ctx.depth, "binary lhs", ctx.span)?;
        let rhs_dims = self.known_expr_dims(rhs, ctx.scope, ctx.depth, "binary rhs", ctx.span)?;
        let lhs_expr = if lhs_dims.is_empty() {
            self.substitute(lhs, ctx.scope)?
        } else {
            self.project_value(
                lhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth,
                ctx.span,
            )?
            .ok_or_else(|| unsupported_at("binary lhs could not be projected", ctx.span))?
        };
        let rhs_expr = if rhs_dims.is_empty() {
            self.substitute(rhs, ctx.scope)?
        } else {
            self.project_value(
                rhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth,
                ctx.span,
            )?
            .ok_or_else(|| unsupported_at("binary rhs could not be projected", ctx.span))?
        };
        Ok(Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs_expr),
            rhs: Box::new(rhs_expr),
            span: ctx.span,
        }))
    }

    pub(super) fn project_tensor_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some(lhs_dims) = self.expr_dims_with_owner(lhs, ctx.scope, ctx.depth, ctx.span)? else {
            return Ok(None);
        };
        let Some(rhs_dims) = self.expr_dims_with_owner(rhs, ctx.scope, ctx.depth, ctx.span)? else {
            return Ok(None);
        };
        match (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims) {
            ([rows, cols], [n], [_]) if cols == n => self.project_matrix_vector_product(
                lhs,
                rhs,
                MatrixVectorProductDims {
                    lhs_dims: &lhs_dims,
                    rhs_dims: &rhs_dims,
                    rows: *rows,
                    cols: *cols,
                },
                ctx,
            ),
            ([n], [rows, cols], [_]) if n == rows => {
                self.project_vector_matrix_product(lhs, rhs, &rhs_dims, ctx, *rows, *cols)
            }
            ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
                if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols =>
            {
                self.project_matrix_matrix_product(lhs, rhs, &lhs_dims, &rhs_dims, ctx, *cols)
            }
            _ => Ok(None),
        }
    }

    fn project_matrix_vector_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        product_dims: MatrixVectorProductDims<'_>,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rows = valid_product_dim(product_dims.rows, ctx.span, "matrix-vector rows")?;
        let cols = valid_product_dim(product_dims.cols, ctx.span, "matrix-vector columns")?;
        if ctx.flat_index >= rows {
            return Ok(None);
        }
        let row = ctx.flat_index;
        let mut terms =
            projection_vec_with_capacity(cols, "matrix-vector product term count", ctx.span)?;
        for col in 0..cols {
            let lhs_idx = checked_projection_offset(
                row,
                cols,
                col,
                "matrix-vector lhs flat index",
                ctx.span,
            )?;
            let lhs_term = self
                .project_value(
                    lhs,
                    product_dims.lhs_dims,
                    lhs_idx,
                    ctx.scope,
                    ctx.depth,
                    ctx.span,
                )?
                .ok_or_else(|| {
                    unsupported_at("matrix-vector lhs could not be projected", ctx.span)
                })?;
            let rhs_term = self
                .project_value(
                    rhs,
                    product_dims.rhs_dims,
                    col,
                    ctx.scope,
                    ctx.depth,
                    ctx.span,
                )?
                .ok_or_else(|| {
                    unsupported_at("matrix-vector rhs could not be projected", ctx.span)
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }

    fn project_vector_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        rows: i64,
        cols: i64,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rows = valid_product_dim(rows, ctx.span, "vector-matrix rows")?;
        let cols = valid_product_dim(cols, ctx.span, "vector-matrix columns")?;
        if ctx.flat_index >= cols {
            return Ok(None);
        }
        let col = ctx.flat_index;
        let lhs_dims = [checked_usize_to_i64(rows, "vector-matrix rows", ctx.span)?];
        let mut terms =
            projection_vec_with_capacity(rows, "vector-matrix product term count", ctx.span)?;
        for row in 0..rows {
            let lhs_term = self
                .project_value(lhs, &lhs_dims, row, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("vector-matrix lhs could not be projected", ctx.span)
                })?;
            let rhs_idx = checked_projection_offset(
                row,
                cols,
                col,
                "vector-matrix rhs flat index",
                ctx.span,
            )?;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("vector-matrix rhs could not be projected", ctx.span)
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }

    pub(super) fn project_matrix_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        lhs_dims: &[i64],
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        cols: i64,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let inner = valid_product_dim(lhs_dims[1], ctx.span, "matrix-matrix inner dimension")?;
        let cols = valid_product_dim(cols, ctx.span, "matrix-matrix columns")?;
        if cols == 0 {
            return Ok(None);
        }
        let row = ctx.flat_index / cols;
        let col = ctx.flat_index % cols;
        let mut terms =
            projection_vec_with_capacity(inner, "matrix-matrix product term count", ctx.span)?;
        for inner_idx in 0..inner {
            let lhs_idx = checked_projection_offset(
                row,
                inner,
                inner_idx,
                "matrix-matrix lhs flat index",
                ctx.span,
            )?;
            let rhs_idx = checked_projection_offset(
                inner_idx,
                cols,
                col,
                "matrix-matrix rhs flat index",
                ctx.span,
            )?;
            let lhs_term = self
                .project_value(lhs, lhs_dims, lhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("matrix-matrix lhs could not be projected", ctx.span)
                })?;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("matrix-matrix rhs could not be projected", ctx.span)
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }
}
