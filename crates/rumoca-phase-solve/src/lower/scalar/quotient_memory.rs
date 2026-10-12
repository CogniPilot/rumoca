//! MLS §8.5 numeric memory over the quotient's exact issued domain.
//!
//! The Map binds each source index symbolically. Its only varying constants
//! are construction-owned affine strides, so no point programs are emitted.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn quotient_memory_map(
        mut self,
        memory: crate::layout::QuotientMemory<'dae>,
        output_start: usize,
    ) -> Result<solve::ComputeNode, LowerError> {
        let span = self.node(memory.quotient).provenance().span();
        let id = memory
            .domain
            .ok_or_else(|| LowerError::contract("quotient Map has no structured domain", span))?;
        let domain = self
            .view
            .domain(id)
            .ok_or_else(|| LowerError::contract("quotient domain is not issued by this DAE", span))?
            .structured()
            .clone();
        let const_strides = self.bind_symbolic_domain(id, None, span)?;
        let mut load_strides = Vec::new();
        let step = if let Some(guard) = memory.activation_guard {
            let position = self.ops.len();
            let held = self.load_slot(solve::scalar_slot_p(memory.parameter), span)?;
            load_strides.push(held_load_stride(&domain, position, span)?);
            self.guarded_quotient_step(memory, guard, held, span)?
        } else {
            let ratio = self.expression(memory.ratio, 0)?;
            self.solve_unary(memory.step(), ratio, span)?
        };
        self.ops.push(solve::LinearOp::StoreOutput { src: step });
        let output_map = solve::TensorOutputMap::dense_contiguous(output_start, &domain)
            .map_err(|error| LowerError::contract(format!("{error:?}"), span))?;
        Ok(solve::ComputeNode::Map {
            domain,
            output_map,
            base_ops: self.ops,
            load_strides,
            const_strides,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        })
    }

    /// A numeric root is a view of the same symbolic domain as its Map.
    /// Keep its substituted coordinates out of the literal-folding ledger:
    /// folding only the scalar view would change the certified ratio term.
    pub(in crate::lower) fn quotient_root_program(
        mut self,
        memory: crate::layout::QuotientMemory<'dae>,
        point: &[i64],
        expression: dae::ExprId<'dae>,
        guard: Option<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let id = memory
            .domain
            .ok_or_else(|| LowerError::contract("numeric root has no structured domain", span))?;
        self.bind_symbolic_domain(id, Some(point), span)?;
        self.root_expression_program(expression, guard, span)
    }

    pub(super) fn quotient_memory_read(
        &mut self,
        memory: crate::layout::QuotientMemory<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let Some(id) = memory.domain else {
            return self.load_slot(solve::scalar_slot_p(memory.parameter), span);
        };
        let domain = self
            .view
            .domain(id)
            .ok_or_else(|| LowerError::contract("quotient domain is not issued by this DAE", span))?
            .structured()
            .clone();
        if let Some((_, point)) = self
            .domain_points
            .iter()
            .rev()
            .find(|(found, _)| *found == id)
        {
            let ordinal = domain
                .ordinal_of(point)
                .map_err(|error| LowerError::contract(error.to_string(), span))?
                .ok_or_else(|| {
                    LowerError::contract("quotient memory point is outside its domain", span)
                })?;
            let index = memory
                .parameter
                .checked_add(ordinal)
                .ok_or_else(|| LowerError::contract("quotient memory address overflow", span))?;
            return self.load_slot(solve::scalar_slot_p(index), span);
        }
        let mut ordinal = self.constant(0.0, span)?;
        let strides = domain
            .ordinal_strides()
            .map_err(|error| LowerError::contract(format!("{error:?}"), span))?;
        for (dimension, (binder, stride)) in domain.binders.iter().zip(strides).enumerate() {
            let coordinate = self.domain_coordinate(id, dimension, span)?;
            let start = self.constant(binder.lower as f64, span)?;
            let delta = self.binary(dae::BinaryOperator::Subtract, coordinate, start, span)?;
            let step = self.constant(binder.step as f64, span)?;
            let offset = self.binary(dae::BinaryOperator::Divide, delta, step, span)?;
            let stride = self.constant(stride as f64, span)?;
            let term = self.binary(dae::BinaryOperator::Multiply, offset, stride, span)?;
            ordinal = self.binary(dae::BinaryOperator::Add, ordinal, term, span)?;
        }
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::LoadIndexedP {
            dst,
            base: memory.parameter,
            count: memory.count,
            index: ordinal,
        });
        Ok(dst)
    }
}

fn held_load_stride(
    domain: &rumoca_core::StructuredIndexDomain,
    op_position: usize,
    span: Span,
) -> Result<solve::AffineStencilLoadStride, LowerError> {
    let strides = domain
        .ordinal_strides()
        .map_err(|error| LowerError::contract(format!("{error:?}"), span))?;
    let terms = strides
        .into_iter()
        .enumerate()
        .map(|(dimension, stride)| {
            Ok(solve::AffineStencilIndexStrideTerm {
                dimension,
                stride: isize::try_from(stride)
                    .map_err(|_| LowerError::contract("quotient memory stride overflows", span))?,
            })
        })
        .collect::<Result<Vec<_>, LowerError>>()?;
    Ok(solve::AffineStencilLoadStride { op_position, terms })
}
