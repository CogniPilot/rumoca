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
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            e.address(source);
            // Source ordinal = ((row * columns + column) * width) + trailing.
            e.push(I::LocalGet(3));
            e.push(I::I32Const(width as i32));
            e.push(I::I32DivU);
            e.push(I::I32Const(rows as i32));
            e.push(I::I32RemU);
            e.push(I::I32Const(columns as i32));
            e.push(I::I32Mul);
            e.push(I::LocalGet(3));
            e.push(I::I32Const(width as i32));
            e.push(I::I32DivU);
            e.push(I::I32Const(rows as i32));
            e.push(I::I32DivU);
            e.push(I::I32Add);
            e.push(I::I32Const(width as i32));
            e.push(I::I32Mul);
            e.push(I::LocalGet(3));
            e.push(I::I32Const(width as i32));
            e.push(I::I32RemU);
            e.push(I::I32Add);
            e.push(I::I32Const(8));
            e.push(I::I32Mul);
            e.push(I::I32Add);
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
        self.view_origin(axes, &bounds, &strides, status);
        let destination = self.reg(destination);
        let source = self.reg(aggregate);
        let spans = retained_spans(axes, &strides, destination.bytes / 8);
        self.cells(destination.bytes / 8, |e| {
            e.view_offset(&spans);
            e.copy_view_cell(destination, source);
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
        self.view_origin(&axes, &bounds, &strides, status);
        let (destination, aggregate, value) = (
            self.reg(*destination),
            self.reg(*aggregate),
            self.reg(*value),
        );
        self.copy(destination, aggregate);
        let spans = retained_spans(&axes, &strides, value.bytes / 8);
        self.cells(value.bytes / 8, |e| {
            e.view_offset(&spans);
            e.address(destination);
            e.push(I::LocalGet(4));
            e.push(I::I32WrapI64);
            e.push(I::I32Const(8));
            e.push(I::I32Mul);
            e.push(I::I32Add);
            e.load_cell(value, false);
            e.push(I::I64Store(CELL));
        });
        Ok(())
    }

    fn view_offset(&mut self, spans: &[(u32, u32, u32)]) {
        self.push(I::LocalGet(6));
        self.push(I::LocalSet(4));
        for &(stride, extent, output_stride) in spans {
            self.push(I::LocalGet(4));
            self.push(I::LocalGet(3));
            self.push(I::I32Const(output_stride as i32));
            self.push(I::I32DivU);
            self.push(I::I32Const(extent as i32));
            self.push(I::I32RemU);
            self.push(I::I64ExtendI32U);
            self.push(I::I64Const(i64::from(stride)));
            self.push(I::I64Mul);
            self.push(I::I64Add);
            self.push(I::LocalSet(4));
        }
    }

    fn view_origin(
        &mut self,
        axes: &[solve::SolveTensorViewAxis],
        bounds: &[u32],
        strides: &[u32],
        status: Option<u32>,
    ) {
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
    }

    fn copy_view_cell(&mut self, destination: CellRange, source: CellRange) {
        self.cell_address(destination);
        self.address(source);
        self.push(I::LocalGet(4));
        self.push(I::I32WrapI64);
        self.push(I::I32Const(8));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        self.push(I::I64Load(CELL));
        self.push(I::I64Store(CELL));
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

fn retained_spans(
    axes: &[solve::SolveTensorViewAxis],
    strides: &[u32],
    count: u32,
) -> Vec<(u32, u32, u32)> {
    let mut remaining = count;
    axes.iter()
        .zip(strides)
        .filter_map(|(axis, stride)| match *axis {
            solve::SolveTensorViewAxis::Index(_) => None,
            solve::SolveTensorViewAxis::Span { extent, .. } => {
                remaining /= extent;
                Some((*stride, extent, remaining))
            }
        })
        .collect()
}
