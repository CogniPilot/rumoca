//! Interleaved dual aggregates of a JVP program and their widened layout.

use super::operands::{Operand, visit_operands};
use crate::{LinearOp, Reg};

/// The interleaved primal/tangent width of a tensor operation, if it is one.
pub(super) fn aggregate_lanes(op: &LinearOp) -> Option<usize> {
    match op {
        LinearOp::MatrixMultiply { lanes, .. }
        | LinearOp::TensorBinary { lanes, .. }
        | LinearOp::TensorCross { lanes, .. }
        | LinearOp::TensorTranspose { lanes, .. }
        | LinearOp::TensorConcatenate { lanes, .. }
        | LinearOp::TensorUpdate { lanes, .. }
        | LinearOp::TensorFill { lanes, .. }
        | LinearOp::TensorIdentity { lanes, .. }
        | LinearOp::TensorLoad { lanes, .. } => Some(*lanes),
        _ => None,
    }
}

/// The interleaved dual operand ranges of `op`.
fn dual_operands(op: &LinearOp) -> Vec<(Reg, Operand)> {
    let mut probe = op.clone();
    let mut ranges = Vec::new();
    visit_operands(&mut probe, &mut |start, shape| ranges.push((*start, shape)));
    ranges.retain(|(_, shape)| matches!(shape, Operand::Strided { lanes: 2, .. }));
    ranges
}

/// Merge sorted `(start, len)` intervals into regions; `None` when a range
/// starts on a tangent lane of the region it overlaps.
fn merge_in_phase(intervals: Vec<(Reg, usize)>) -> Option<Vec<(Reg, usize)>> {
    let mut spans: Vec<(Reg, usize)> = Vec::new();
    for (start, len) in intervals.into_iter().filter(|(_, len)| *len > 0) {
        let end = start as usize + len;
        let Some((region_start, region_len)) =
            spans.last_mut().filter(|(region_start, region_len)| {
                (start as usize) < *region_start as usize + *region_len
            })
        else {
            spans.push((start, len));
            continue;
        };
        // Every range of a region starts on a primal lane.
        if (start - *region_start) % 2 == 1 {
            return None;
        }
        *region_len = (*region_len).max(end - *region_start as usize);
    }
    Some(spans)
}

/// Where one source register lives in the widened program.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Slot {
    /// A register outside every dual aggregate.
    Scalar,
    /// The primal lane of element `element` of region `region`.
    Primal { region: usize, element: usize },
    /// The tangent lane of element `element` of region `region`.
    Tangent { region: usize, element: usize },
}

/// Every register range a dual (`lanes: 2`) tensor operation reads or
/// writes, merged into regions. Each region is widened from two interleaved
/// lanes per element to one primal and `lanes` tangent lanes per element.
pub(super) struct Regions {
    /// `(start, register count)` of each region in the source program.
    spans: Vec<(Reg, usize)>,
    /// Widened start of each region.
    widened: Vec<Reg>,
    width: usize,
    lookup: Vec<Option<usize>>,
    end: Reg,
}

impl Regions {
    /// Collect the regions of `program` (`registers` registers) and place
    /// them from `base` with `lanes` tangent lanes. `None` when two dual
    /// ranges overlap out of lane phase or a register index overflows.
    pub(super) fn derive(
        program: &[LinearOp],
        registers: usize,
        base: usize,
        lanes: usize,
    ) -> Option<Self> {
        let mut intervals = Vec::new();
        for op in program {
            if aggregate_lanes(op) != Some(2) {
                continue;
            }
            if let Some(start) = op.dst_register() {
                intervals.push((start, op.dst_register_count()));
            }
            for (start, shape) in dual_operands(op) {
                let registers = shape.registers(start)?;
                let last = *registers.last()?;
                intervals.push((start, (last - start) as usize + 1));
            }
        }
        intervals.sort_unstable();
        let spans = merge_in_phase(intervals)?;
        let width = lanes.checked_add(1)?;
        let mut lookup = vec![None; registers];
        let mut widened = Vec::with_capacity(spans.len());
        let mut next = base;
        for (index, (start, len)) in spans.iter().enumerate() {
            if *len % 2 == 1 {
                return None;
            }
            for register in *start as usize..*start as usize + *len {
                *lookup.get_mut(register)? = Some(index);
            }
            widened.push(Reg::try_from(next).ok()?);
            next = next.checked_add((*len / 2).checked_mul(width)?)?;
        }
        Some(Self {
            spans,
            widened,
            width,
            lookup,
            end: Reg::try_from(next).ok()?,
        })
    }

    /// One past the last widened register.
    pub(super) const fn end(&self) -> Reg {
        self.end
    }

    /// Interleaved width of a widened aggregate.
    pub(super) const fn width(&self) -> usize {
        self.width
    }

    pub(super) fn slot(&self, register: Reg) -> Slot {
        let Some(Some(region)) = self.lookup.get(register as usize).copied() else {
            return Slot::Scalar;
        };
        let offset = (register - self.spans[region].0) as usize;
        let element = offset / 2;
        if offset.is_multiple_of(2) {
            Slot::Primal { region, element }
        } else {
            Slot::Tangent { region, element }
        }
    }

    /// The widened register of the primal lane of `(region, element)`.
    pub(super) fn primal(&self, region: usize, element: usize) -> Reg {
        self.widened[region] + (element * self.width) as Reg
    }

    /// The widened register of tangent lane `lane` of `(region, element)`.
    pub(super) fn tangent(&self, region: usize, element: usize, lane: usize) -> Reg {
        self.primal(region, element) + 1 + lane as Reg
    }
}
