//! Closed tensor print items: fills, identities, diagonals, concatenations,
//! aggregate construction, scaling, transposes, matrix products, cross
//! products and static element projections. Every extent, stride, offset
//! and coordinate a printer spells is issued here from the checked types.
//! The printed loops still evaluate row-major index expressions over those
//! issued numbers at run time; that order is the IR's defined element order,
//! and the printer chooses none of it. A shape or element type no item
//! covers stays a legacy item.

use serde::Serialize;

use super::super::{SolveOperation, SolveRegisterId, SolveValueType};
use super::SolveBodyItem;
use crate::{SolveRealFormat, SolveScalarType};

/// One operand of a concatenation or aggregate construction: `width`
/// elements of `operand`, written at `offset` of the destination (of each
/// outer block, for a concatenation, which prints the offset as its running
/// cursor).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveTensorPart {
    operand: usize,
    offset: u64,
    width: u64,
}

impl SolveTensorPart {
    #[must_use]
    pub const fn operand(&self) -> usize {
        self.operand
    }

    #[must_use]
    pub const fn offset(&self) -> u64 {
        self.offset
    }

    #[must_use]
    pub const fn width(&self) -> u64 {
        self.width
    }
}

/// One axis of a static element projection: the aggregate's `extent` on
/// that axis and the zero-based checked `coordinate`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveElementAxis {
    extent: u32,
    coordinate: u32,
}

impl SolveElementAxis {
    #[must_use]
    pub const fn extent(&self) -> u32 {
        self.extent
    }

    #[must_use]
    pub const fn coordinate(&self) -> u32 {
        self.coordinate
    }
}

/// One closed tensor kernel; registers are ids, counts are issued.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveTensorKernel {
    /// `extent` copies of the scalar `value`.
    Fill { value: usize, extent: u32 },
    /// The `size` by `size` Real identity.
    Identity { size: u32 },
    /// The `size` by `size` Real matrix with `operand` on its diagonal.
    Diagonal { operand: usize, size: u32 },
    /// For each of `outer` blocks, every part's `width` elements in order.
    Concatenate {
        outer: u64,
        parts: Box<[SolveTensorPart]>,
    },
    /// Each part's `width` elements at its `offset`.
    ConstructAggregate { parts: Box<[SolveTensorPart]> },
    /// `extent` Binary64 products of `aggregate` with the scalar `scalar`.
    Scale {
        aggregate: usize,
        scalar: usize,
        extent: u32,
    },
    /// `operand` with its first two axes, `rows` by `columns`, exchanged;
    /// `trailing` elements per position.
    Transpose {
        operand: usize,
        rows: u32,
        columns: u32,
        trailing: u64,
    },
    /// The Binary64 `rows` by `columns` product over `inner`.
    MatrixMultiply {
        lhs: usize,
        rhs: usize,
        rows: u32,
        inner: u32,
        columns: u32,
    },
    /// The Binary64 cross product of two three-vectors.
    Cross { lhs: usize, rhs: usize },
    /// The element of `aggregate` at the issued coordinates.
    ProjectElement {
        aggregate: usize,
        axes: Box<[SolveElementAxis]>,
    },
}

/// The closed tensor item of operation `index`, when every count it prints
/// is issued; `None` keeps the operation a legacy item.
pub(super) fn tensor_item(
    index: usize,
    operation: &SolveOperation,
    register_types: &[SolveValueType],
) -> Option<SolveBodyItem> {
    let value_type = |register: &SolveRegisterId| register_types.get(register.index());
    let (destination, kernel) = match operation {
        SolveOperation::Fill { destination, value } => (
            destination,
            SolveTensorKernel::Fill {
                value: value.index(),
                extent: value_type(destination)?.scalar_count(),
            },
        ),
        SolveOperation::Identity { destination } => {
            let &[size, columns] = value_type(destination)?.dimensions() else {
                return None;
            };
            (size == columns).then_some(())?;
            (destination, SolveTensorKernel::Identity { size })
        }
        SolveOperation::Diagonal {
            destination,
            operand,
        } => {
            let &[size] = value_type(operand)?.dimensions() else {
                return None;
            };
            (value_type(destination)?.dimensions() == [size, size]).then_some(())?;
            (
                destination,
                SolveTensorKernel::Diagonal {
                    operand: operand.index(),
                    size,
                },
            )
        }
        SolveOperation::Concatenate {
            destination,
            axis,
            operands,
        } => (
            destination,
            concatenate(value_type(destination)?, *axis, operands, register_types)?,
        ),
        SolveOperation::ConstructAggregate {
            destination,
            elements,
        } => (
            destination,
            construct_aggregate(value_type(destination)?, elements, register_types)?,
        ),
        SolveOperation::Scale {
            destination,
            aggregate,
            scalar,
        } => {
            let destination_type = binary64(value_type(destination)?)?;
            (
                destination,
                SolveTensorKernel::Scale {
                    aggregate: aggregate.index(),
                    scalar: scalar.index(),
                    extent: destination_type.scalar_count(),
                },
            )
        }
        SolveOperation::Transpose {
            destination,
            operand,
        } => {
            let [rows, columns, trailing @ ..] = value_type(operand)?.dimensions() else {
                return None;
            };
            (
                destination,
                SolveTensorKernel::Transpose {
                    operand: operand.index(),
                    rows: *rows,
                    columns: *columns,
                    trailing: product(trailing)?,
                },
            )
        }
        SolveOperation::MatrixMultiply {
            destination,
            lhs,
            rhs,
        } => {
            binary64(value_type(destination)?)?;
            let (rows, inner) = match *value_type(lhs)?.dimensions() {
                [inner] => (1, inner),
                [rows, inner] => (rows, inner),
                _ => return None,
            };
            let columns = match *value_type(rhs)?.dimensions() {
                [_] => 1,
                [_, columns] => columns,
                _ => return None,
            };
            (
                destination,
                SolveTensorKernel::MatrixMultiply {
                    lhs: lhs.index(),
                    rhs: rhs.index(),
                    rows,
                    inner,
                    columns,
                },
            )
        }
        SolveOperation::Cross {
            destination,
            lhs,
            rhs,
        } => {
            binary64(value_type(destination)?)?;
            (
                destination,
                SolveTensorKernel::Cross {
                    lhs: lhs.index(),
                    rhs: rhs.index(),
                },
            )
        }
        SolveOperation::ProjectElement {
            destination,
            aggregate,
            indices,
        } => {
            let shape = value_type(aggregate)?.dimensions();
            (shape.len() == indices.len()).then_some(())?;
            let axes = shape
                .iter()
                .zip(indices.iter())
                .map(|(&extent, &coordinate)| {
                    (coordinate < extent).then_some(SolveElementAxis { extent, coordinate })
                })
                .collect::<Option<Box<[_]>>>()?;
            (
                destination,
                SolveTensorKernel::ProjectElement {
                    aggregate: aggregate.index(),
                    axes,
                },
            )
        }
        _ => return None,
    };
    Some(SolveBodyItem::Tensor {
        index,
        destination: destination.index(),
        kernel,
    })
}

/// The type itself when its element is Binary64 Real.
fn binary64(value_type: &SolveValueType) -> Option<&SolveValueType> {
    matches!(
        value_type.element_type(),
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        }
    )
    .then_some(value_type)
}

/// The element count of `dimensions`, or `None` when it overflows.
fn product(dimensions: &[u32]) -> Option<u64> {
    dimensions
        .iter()
        .try_fold(1u64, |count, &extent| count.checked_mul(u64::from(extent)))
}

/// Concatenation along `axis`: per outer block, each operand contributes its
/// axis extent (one for an operand promoted past its rank) times the inner
/// block, and the parts exactly cover the destination.
fn concatenate(
    destination: &SolveValueType,
    axis: u32,
    operands: &[SolveRegisterId],
    register_types: &[SolveValueType],
) -> Option<SolveTensorKernel> {
    let axis = usize::try_from(axis).ok()?;
    let shape = destination.dimensions();
    let outer = product(shape.get(..axis)?)?;
    let inner = product(shape.get(axis + 1..)?)?;
    let mut offset = 0u64;
    let parts = operands
        .iter()
        .map(|operand| {
            let dimensions = register_types.get(operand.index())?.dimensions();
            let width = u64::from(dimensions.get(axis).copied().unwrap_or(1)).checked_mul(inner)?;
            let part = SolveTensorPart {
                operand: operand.index(),
                offset,
                width,
            };
            offset = offset.checked_add(width)?;
            Some(part)
        })
        .collect::<Option<Box<[_]>>>()?;
    (outer.checked_mul(offset)? == u64::from(destination.scalar_count()))
        .then_some(SolveTensorKernel::Concatenate { outer, parts })
}

/// Aggregate construction from equal, nonempty elements that exactly cover
/// the destination in order.
fn construct_aggregate(
    destination: &SolveValueType,
    elements: &[SolveRegisterId],
    register_types: &[SolveValueType],
) -> Option<SolveTensorKernel> {
    let mut offset = 0u64;
    let mut widths = elements.iter().map(|element| {
        register_types
            .get(element.index())
            .map(|value_type| u64::from(value_type.scalar_count()))
    });
    let width = widths.next().flatten()?;
    (width > 0 && widths.all(|other| other == Some(width))).then_some(())?;
    let parts = elements
        .iter()
        .map(|element| {
            let part = SolveTensorPart {
                operand: element.index(),
                offset,
                width,
            };
            offset = offset.checked_add(width)?;
            Some(part)
        })
        .collect::<Option<Box<[_]>>>()?;
    (offset == u64::from(destination.scalar_count()))
        .then_some(SolveTensorKernel::ConstructAggregate { parts })
}
