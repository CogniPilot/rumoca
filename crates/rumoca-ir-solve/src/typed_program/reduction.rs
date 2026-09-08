//! Construction-issued plans for compact tensor reductions.

use super::types::{SolveArithmeticProfile, SolveRealFormat, SolveScalarType, SolveValueType};
use rumoca_core::RealMatrixMultiplySemantics;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixOperandLayout {
    Vector,
    RowMajorMatrix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixResultLayout {
    Scalar,
    Vector,
    RowMajorMatrix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyArithmetic {
    Real {
        accumulator: SolveRealFormat,
        semantics: RealMatrixMultiplySemantics,
        order: SolveMatrixMultiplyOrder,
        primitive_rounding: SolveMatrixMultiplyRounding,
        contraction: SolveMatrixMultiplyContraction,
        intermediate_precision: SolveMatrixMultiplyIntermediatePrecision,
        final_rounding: SolveMatrixMultiplyFinalRounding,
        signed_zero: SolveMatrixMultiplySignedZero,
        nan: SolveMatrixMultiplyNan,
        infinity: SolveMatrixMultiplyInfinity,
        subnormal: SolveMatrixMultiplySubnormal,
        status: SolveMatrixMultiplyStatus,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyOrder {
    AscendingSharedAxis,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyRounding {
    RoundToNearestTiesToEven,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyContraction {
    SeparateMultiplyAdd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyIntermediatePrecision {
    AccumulatorFormatOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyFinalRounding {
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplySignedZero {
    IeeePrimitiveResult,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyNan {
    QuietPayloadAndSignQuotient,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyInfinity {
    IeeePrimitiveResult,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplySubnormal {
    GradualUnderflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveMatrixMultiplyStatus {
    NoObservableFloatingStatus,
}

/// One private-construction-issued plan stored on one compact
/// `MatrixMultiply` occurrence.
///
/// The plan owns the checked shape and affine operand layouts so executors do
/// not re-derive them. It is not a scalar graph: every plan remains attached
/// to one tensor-native operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SolveMatrixMultiplyPlan {
    rows: u32,
    inner: u32,
    columns: u32,
    output_count: u32,
    lhs_layout: SolveMatrixOperandLayout,
    rhs_layout: SolveMatrixOperandLayout,
    result_layout: SolveMatrixResultLayout,
    lhs_row_stride: u32,
    lhs_inner_stride: u32,
    rhs_inner_stride: u32,
    rhs_column_stride: u32,
    result_row_stride: u32,
    result_column_stride: u32,
    arithmetic: SolveMatrixMultiplyArithmetic,
}

pub(super) enum SolveMatrixMultiplyPlanIssuanceError {
    InvalidAlgebra,
    UnsupportedArithmetic,
    EmptyFirstProductDomain,
}

struct MatrixMultiplyShape {
    rows: u32,
    inner: u32,
    columns: u32,
    lhs_layout: SolveMatrixOperandLayout,
    rhs_layout: SolveMatrixOperandLayout,
    result_layout: SolveMatrixResultLayout,
}

impl SolveMatrixMultiplyPlan {
    pub(super) fn issued(
        profile: SolveArithmeticProfile,
        lhs: &SolveValueType,
        rhs: &SolveValueType,
    ) -> Result<(Self, SolveValueType), SolveMatrixMultiplyPlanIssuanceError> {
        if lhs.element_type() != rhs.element_type() {
            return Err(SolveMatrixMultiplyPlanIssuanceError::InvalidAlgebra);
        }
        let SolveScalarType::Real { format } = lhs.element_type() else {
            return Err(SolveMatrixMultiplyPlanIssuanceError::UnsupportedArithmetic);
        };
        if format != profile.real_format() {
            return Err(SolveMatrixMultiplyPlanIssuanceError::InvalidAlgebra);
        }
        let shape = matrix_multiply_shape(lhs.dimensions(), rhs.dimensions())?;
        if shape.inner == 0
            && profile.real_matrix_multiply_semantics()
                == RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct
        {
            return Err(SolveMatrixMultiplyPlanIssuanceError::EmptyFirstProductDomain);
        }
        let result_dimensions = match shape.result_layout {
            SolveMatrixResultLayout::Scalar => Vec::new(),
            SolveMatrixResultLayout::Vector if shape.rows == 1 => vec![shape.columns],
            SolveMatrixResultLayout::Vector => vec![shape.rows],
            SolveMatrixResultLayout::RowMajorMatrix => vec![shape.rows, shape.columns],
        };
        let result_type = if result_dimensions.is_empty() {
            SolveValueType::scalar(lhs.element_type())
        } else {
            SolveValueType::tensor(lhs.element_type(), result_dimensions)
                .map_err(|_| SolveMatrixMultiplyPlanIssuanceError::InvalidAlgebra)?
        };
        let output_count = result_type.scalar_count();
        let (lhs_row_stride, lhs_inner_stride) = match shape.lhs_layout {
            SolveMatrixOperandLayout::Vector => (0, 1),
            SolveMatrixOperandLayout::RowMajorMatrix => (shape.inner, 1),
        };
        let (rhs_inner_stride, rhs_column_stride) = match shape.rhs_layout {
            SolveMatrixOperandLayout::Vector => (1, 0),
            SolveMatrixOperandLayout::RowMajorMatrix => (shape.columns, 1),
        };
        let (result_row_stride, result_column_stride) = match shape.result_layout {
            SolveMatrixResultLayout::Scalar => (0, 0),
            SolveMatrixResultLayout::Vector if shape.rows == 1 => (0, 1),
            SolveMatrixResultLayout::Vector => (1, 0),
            SolveMatrixResultLayout::RowMajorMatrix => (shape.columns, 1),
        };
        Ok((
            Self {
                rows: shape.rows,
                inner: shape.inner,
                columns: shape.columns,
                output_count,
                lhs_layout: shape.lhs_layout,
                rhs_layout: shape.rhs_layout,
                result_layout: shape.result_layout,
                lhs_row_stride,
                lhs_inner_stride,
                rhs_inner_stride,
                rhs_column_stride,
                result_row_stride,
                result_column_stride,
                arithmetic: SolveMatrixMultiplyArithmetic::Real {
                    accumulator: format,
                    semantics: profile.real_matrix_multiply_semantics(),
                    order: SolveMatrixMultiplyOrder::AscendingSharedAxis,
                    primitive_rounding: SolveMatrixMultiplyRounding::RoundToNearestTiesToEven,
                    contraction: SolveMatrixMultiplyContraction::SeparateMultiplyAdd,
                    intermediate_precision:
                        SolveMatrixMultiplyIntermediatePrecision::AccumulatorFormatOnly,
                    final_rounding: SolveMatrixMultiplyFinalRounding::None,
                    signed_zero: SolveMatrixMultiplySignedZero::IeeePrimitiveResult,
                    nan: SolveMatrixMultiplyNan::QuietPayloadAndSignQuotient,
                    infinity: SolveMatrixMultiplyInfinity::IeeePrimitiveResult,
                    subnormal: SolveMatrixMultiplySubnormal::GradualUnderflow,
                    status: SolveMatrixMultiplyStatus::NoObservableFloatingStatus,
                },
            },
            result_type,
        ))
    }

    #[must_use]
    pub const fn rows(self) -> u32 {
        self.rows
    }

    #[must_use]
    pub const fn inner(self) -> u32 {
        self.inner
    }

    #[must_use]
    pub const fn columns(self) -> u32 {
        self.columns
    }

    #[must_use]
    pub const fn output_count(self) -> u32 {
        self.output_count
    }

    #[must_use]
    pub const fn lhs_layout(self) -> SolveMatrixOperandLayout {
        self.lhs_layout
    }

    #[must_use]
    pub const fn rhs_layout(self) -> SolveMatrixOperandLayout {
        self.rhs_layout
    }

    #[must_use]
    pub const fn result_layout(self) -> SolveMatrixResultLayout {
        self.result_layout
    }

    #[must_use]
    pub const fn lhs_row_stride(self) -> u32 {
        self.lhs_row_stride
    }

    #[must_use]
    pub const fn lhs_inner_stride(self) -> u32 {
        self.lhs_inner_stride
    }

    #[must_use]
    pub const fn rhs_inner_stride(self) -> u32 {
        self.rhs_inner_stride
    }

    #[must_use]
    pub const fn rhs_column_stride(self) -> u32 {
        self.rhs_column_stride
    }

    #[must_use]
    pub const fn result_row_stride(self) -> u32 {
        self.result_row_stride
    }

    #[must_use]
    pub const fn result_column_stride(self) -> u32 {
        self.result_column_stride
    }

    #[must_use]
    pub const fn arithmetic(self) -> SolveMatrixMultiplyArithmetic {
        self.arithmetic
    }
}

fn matrix_multiply_shape(
    lhs: &[u32],
    rhs: &[u32],
) -> Result<MatrixMultiplyShape, SolveMatrixMultiplyPlanIssuanceError> {
    let shape = match (lhs, rhs) {
        ([inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => MatrixMultiplyShape {
            rows: 1,
            inner: *inner_lhs,
            columns: 1,
            lhs_layout: SolveMatrixOperandLayout::Vector,
            rhs_layout: SolveMatrixOperandLayout::Vector,
            result_layout: SolveMatrixResultLayout::Scalar,
        },
        ([rows, inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => MatrixMultiplyShape {
            rows: *rows,
            inner: *inner_lhs,
            columns: 1,
            lhs_layout: SolveMatrixOperandLayout::RowMajorMatrix,
            rhs_layout: SolveMatrixOperandLayout::Vector,
            result_layout: SolveMatrixResultLayout::Vector,
        },
        ([inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => MatrixMultiplyShape {
            rows: 1,
            inner: *inner_lhs,
            columns: *columns,
            lhs_layout: SolveMatrixOperandLayout::Vector,
            rhs_layout: SolveMatrixOperandLayout::RowMajorMatrix,
            result_layout: SolveMatrixResultLayout::Vector,
        },
        ([rows, inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            MatrixMultiplyShape {
                rows: *rows,
                inner: *inner_lhs,
                columns: *columns,
                lhs_layout: SolveMatrixOperandLayout::RowMajorMatrix,
                rhs_layout: SolveMatrixOperandLayout::RowMajorMatrix,
                result_layout: SolveMatrixResultLayout::RowMajorMatrix,
            }
        }
        _ => return Err(SolveMatrixMultiplyPlanIssuanceError::InvalidAlgebra),
    };
    Ok(shape)
}
