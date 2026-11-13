//! Compact bit-preserving first-two-axis transpose and checked tensor views.
use super::TypedCallCompileError;
use super::TypedCallFaultKind;
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use rumoca_ir_solve as solve;
use wasm_encoder::Instruction as I;

impl Emitter<'_> {
    pub(super) fn view_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        match operation.operation() {
            solve::SolveOperation::Transpose {
                destination,
                operand,
            } => self.transpose(*destination, *operand),
            solve::SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => self.project_view(
                index,
                operation.provenance(),
                *destination,
                *aggregate,
                axes,
            ),
            solve::SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => {
                // The original checked slice owns equal rank and in-bounds static axes.
                // Reuse only target address emission; retain the original typed owner.
                let axes = origin
                    .iter()
                    .zip(self.program.register_types()[destination.index()].dimensions())
                    .map(|(origin, extent)| solve::SolveTensorViewAxis::Span {
                        origin: *origin,
                        extent: *extent,
                    })
                    .collect::<Vec<_>>();
                self.project_view(
                    index,
                    operation.provenance(),
                    *destination,
                    *aggregate,
                    &axes,
                )
            }
            solve::SolveOperation::UpdateView { .. }
            | solve::SolveOperation::UpdateSlice { .. } => self.update_view(index, operation),
            _ => unreachable!("checked view dispatch"),
        }
    }

    fn transpose(
        &mut self,
        destination: solve::SolveRegisterId,
        operand: solve::SolveRegisterId,
    ) -> Result<(), TypedCallCompileError> {
        let shape = self.program.register_types()[operand.index()].dimensions();
        let rows = shape[0];
        let columns = shape[1];
        let width = shape[2..]
            .iter()
            .try_fold(1u32, |n, extent| n.checked_mul(*extent))
            .ok_or(TypedCallCompileError::SizeLimit)?;
        let (destination, source) = (self.reg(destination), self.reg(operand));
        // Destination axes: column, row, then the trailing cells.
        self.cells_shaped(&[columns, rows, width], |e| {
            e.cell_address(destination);
            let (column, row, trailing) = (e.axis(0), e.axis(1), e.axis(2));
            e.cell_at(
                source,
                0,
                &[(row, columns * width), (column, width), (trailing, 1)],
            );
            e.push(I::I64Load(CELL));
            e.push(I::I64Store(CELL));
        });
        Ok(())
    }

    fn project_view(
        &mut self,
        index: usize,
        provenance: rumoca_core::Span,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        axes: &[solve::SolveTensorViewAxis],
    ) -> Result<(), TypedCallCompileError> {
        let shape = self.program.register_types()[aggregate.index()].dimensions();
        let strides = row_major_strides(shape)?;
        let bounds = shape.to_vec();
        let status = axes
            .iter()
            .any(|axis| matches!(axis, solve::SolveTensorViewAxis::Index(_)))
            .then(|| {
                self.fault(
                    Some(index),
                    "project_view",
                    TypedCallFaultKind::IndexBounds,
                    provenance,
                )
            });
        let origin = self.view_origin(axes, &bounds, &strides, status);
        let destination = self.reg(destination);
        let source = self.reg(aggregate);
        let (shape, spans) = retained_spans(axes, &strides);
        self.cells_shaped(&shape, |e| {
            e.cell_address(destination);
            e.view_cell(source, origin, &spans);
            e.push(I::I64Load(CELL));
            e.push(I::I64Store(CELL));
        });
        Ok(())
    }

    fn update_view(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let (destination, aggregate, value, axes): (
            _,
            _,
            _,
            std::borrow::Cow<'_, [solve::SolveTensorViewAxis]>,
        ) = match operation.operation() {
            solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => (
                destination,
                aggregate,
                value,
                std::borrow::Cow::Borrowed(axes),
            ),
            solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => {
                let axes = origin
                    .iter()
                    .zip(self.program.register_types()[value.index()].dimensions())
                    .map(|(origin, extent)| solve::SolveTensorViewAxis::Span {
                        origin: *origin,
                        extent: *extent,
                    })
                    .collect();
                (destination, aggregate, value, std::borrow::Cow::Owned(axes))
            }
            _ => unreachable!("checked update view dispatch"),
        };
        let bounds = self.program.register_types()[aggregate.index()]
            .dimensions()
            .to_vec();
        let strides = row_major_strides(&bounds)?;
        let status = axes
            .iter()
            .any(|axis| matches!(axis, solve::SolveTensorViewAxis::Index(_)))
            .then(|| {
                self.fault(
                    Some(index),
                    "update_view",
                    TypedCallFaultKind::IndexBounds,
                    operation.provenance(),
                )
            });
        // Resolve all one-based indices in source-axis order before writing the result.
        let origin = self.view_origin(&axes, &bounds, &strides, status);
        let (destination, aggregate, value) = (
            self.reg(*destination),
            self.reg(*aggregate),
            self.reg(*value),
        );
        self.copy(destination, aggregate);
        let (shape, spans) = retained_spans(&axes, &strides);
        self.cells_shaped(&shape, |e| {
            e.view_cell(destination, origin, &spans);
            e.load_cell(value, false);
            e.push(I::I64Store(CELL));
        });
        Ok(())
    }

    /// Push the address of the cell of `range` at the view origin plus each
    /// retained axis index times its stride.
    fn view_cell(&mut self, range: CellRange, origin: Option<u32>, spans: &[u32]) {
        let terms = spans
            .iter()
            .enumerate()
            .map(|(axis, stride)| (self.axis(axis), *stride))
            .collect::<Vec<_>>();
        self.cell_at(range, origin.unwrap_or(0), &terms);
        if origin.is_none() {
            // The runtime origin resolved by `view_origin` (local 6).
            self.push(I::LocalGet(6));
            self.push(I::I32WrapI64);
            self.push(I::I32Const(8));
            self.push(I::I32Mul);
            self.push(I::I32Add);
        }
    }

    /// The view's first source cell: a constant when every axis is a span,
    /// otherwise resolved at run time into local 6.
    fn view_origin(
        &mut self,
        axes: &[solve::SolveTensorViewAxis],
        bounds: &[u32],
        strides: &[u32],
        status: Option<u32>,
    ) -> Option<u32> {
        if status.is_none() {
            return Some(
                axes.iter()
                    .zip(strides)
                    .map(|(axis, stride)| match axis {
                        solve::SolveTensorViewAxis::Span { origin, .. } => origin * stride,
                        solve::SolveTensorViewAxis::Index(_) => unreachable!("span-only view"),
                    })
                    .sum(),
            );
        }
        self.push(I::I64Const(0));
        self.push(I::LocalSet(6));
        // Resolve every checked runtime axis before publishing any view cell.
        for ((axis, extent), stride) in axes.iter().zip(bounds).zip(strides) {
            match *axis {
                solve::SolveTensorViewAxis::Index(register) => {
                    let status = status.expect("index axis has an issued bounds fault");
                    self.address(self.reg(register));
                    self.push(I::I64Load(CELL));
                    self.push(I::LocalSet(5));
                    self.push(I::LocalGet(5));
                    self.push(I::I64Const(1));
                    self.push(I::I64LtS);
                    self.fail_if(status);
                    self.push(I::LocalGet(5));
                    self.push(I::I64Const(i64::from(*extent)));
                    self.push(I::I64GtS);
                    self.fail_if(status);
                    self.push(I::LocalGet(6));
                    self.push(I::LocalGet(5));
                    self.push(I::I64Const(1));
                    self.push(I::I64Sub);
                }
                solve::SolveTensorViewAxis::Span { origin, .. } => {
                    self.push(I::LocalGet(6));
                    self.push(I::I64Const(i64::from(origin)));
                }
            }
            self.push(I::I64Const(i64::from(*stride)));
            self.push(I::I64Mul);
            self.push(I::I64Add);
            self.push(I::LocalSet(6));
        }
        None
    }
}

fn row_major_strides(shape: &[u32]) -> Result<Vec<u32>, TypedCallCompileError> {
    let mut result = vec![1; shape.len()];
    let mut stride = 1u32;
    for (index, extent) in shape.iter().enumerate().rev() {
        result[index] = stride;
        stride = stride
            .checked_mul(*extent)
            .ok_or(TypedCallCompileError::SizeLimit)?;
    }
    Ok(result)
}

/// The extent and source stride of each retained (span) axis of a view.
fn retained_spans(axes: &[solve::SolveTensorViewAxis], strides: &[u32]) -> (Vec<u32>, Vec<u32>) {
    axes.iter()
        .zip(strides)
        .filter_map(|(axis, stride)| match *axis {
            solve::SolveTensorViewAxis::Index(_) => None,
            solve::SolveTensorViewAxis::Span { extent, .. } => Some((extent, *stride)),
        })
        .unzip()
}
