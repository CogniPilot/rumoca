//! Construction of one tangent-lane program.

use super::operands::{Operand, destination, visit_operands};
use super::regions::{Regions, Slot, aggregate_lanes};
use super::{TangentLaneError, unsupported};
use crate::linear_op::ScalarProgramRegisterFlow;
use crate::{LinearOp, Reg, SolvePureCallOutputKind, TensorConcatenateSource};

/// Construction state.
///
/// Scalar registers keep their index for lane 0 and get a default copy
/// `register + lane * registers` for lane `lane`. A register may instead live
/// at explicit `homes` (one per lane, or one shared) when an operation wrote
/// it elsewhere; `at_default[lane][register]` records whether the default
/// copy currently holds the register's lane value.
pub(super) struct Replication {
    registers: usize,
    lanes: usize,
    regions: Regions,
    output_base: Reg,
    next_output: usize,
    lane_outputs: usize,
    next_scratch: usize,
    written: Vec<bool>,
    dependent: Vec<bool>,
    homes: Vec<Option<Box<[Reg]>>>,
    at_default: Vec<Vec<bool>>,
    ops: Vec<LinearOp>,
}

impl Replication {
    pub(super) fn new(
        registers: usize,
        lanes: usize,
        lane_outputs: usize,
        regions: Regions,
    ) -> Result<Self, TangentLaneError> {
        let output_base = regions.end();
        let next_scratch = lane_outputs
            .checked_mul(lanes)
            .and_then(|outputs| outputs.checked_add(output_base as usize))
            .ok_or(TangentLaneError::RegisterOverflow)?;
        Reg::try_from(next_scratch).map_err(|_| TangentLaneError::RegisterOverflow)?;
        Ok(Self {
            registers,
            lanes,
            regions,
            output_base,
            next_output: 0,
            lane_outputs,
            next_scratch,
            written: vec![false; registers],
            dependent: vec![false; registers],
            homes: vec![None; registers],
            at_default: vec![vec![false; registers]; lanes],
            ops: Vec::new(),
        })
    }

    /// Default lane copy of scalar `register`; lane 0 is the register.
    fn default_copy(&self, lane: usize, register: Reg) -> Reg {
        register + (lane * self.registers) as Reg
    }

    /// The widened register lane `lane` reads for source `register`.
    fn read(&self, lane: usize, register: Reg) -> Reg {
        match self.regions.slot(register) {
            Slot::Primal { region, element } => self.regions.primal(region, element),
            Slot::Tangent { region, element } => self.regions.tangent(region, element, lane),
            Slot::Scalar => match &self.homes[register as usize] {
                Some(homes) if homes.len() == 1 => homes[0],
                Some(homes) => homes[lane],
                None if self.dependent[register as usize] => self.default_copy(lane, register),
                None => register,
            },
        }
    }

    /// Whether lanes read distinct values for `register`.
    fn per_lane(&self, register: Reg) -> bool {
        match self.regions.slot(register) {
            Slot::Scalar => self.dependent[register as usize],
            Slot::Primal { .. } => false,
            Slot::Tangent { .. } => true,
        }
    }

    fn scratch(&mut self, count: usize) -> Result<Reg, TangentLaneError> {
        let start = self.next_scratch;
        self.next_scratch = start
            .checked_add(count)
            .filter(|end| Reg::try_from(*end).is_ok())
            .ok_or(TangentLaneError::RegisterOverflow)?;
        Ok(start as Reg)
    }

    /// The start lane `lane` reads a scalar range `registers` from: their
    /// lane values contiguous in source order, moved into default copies when
    /// they are not already so.
    fn range_start(&mut self, lane: usize, registers: &[Reg]) -> Reg {
        let first = registers[0];
        let mapped = registers
            .iter()
            .map(|register| self.read(lane, *register))
            .collect::<Vec<_>>();
        if contiguous_run(&mapped, registers) {
            return mapped[0];
        }
        for (&register, &source) in registers.iter().zip(&mapped) {
            let target = self.default_copy(lane, register);
            let index = register as usize;
            if target != source && !self.at_default[lane][index] {
                self.ops.push(LinearOp::Move {
                    dst: target,
                    src: source,
                });
                self.at_default[lane][index] = true;
            }
        }
        self.default_copy(lane, first)
    }

    pub(super) fn push(&mut self, op_index: usize, op: &LinearOp) -> Result<(), TangentLaneError> {
        match op {
            LinearOp::StoreOutput { src } => self.store(&[*src]),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                let sources = Operand::Strided {
                    count: *count,
                    stride: *stride,
                    lanes: 1,
                }
                .registers(*start)
                .ok_or(TangentLaneError::RegisterOverflow)?;
                self.store(&sources)
            }
            LinearOp::LoadSeed { dst, index } => {
                for lane in 0..self.lanes {
                    let index = index
                        .checked_mul(self.lanes)
                        .and_then(|index| index.checked_add(lane))
                        .ok_or(TangentLaneError::RegisterOverflow)?;
                    let dst = self.write_target(op_index, op, lane, *dst)?;
                    self.ops.push(LinearOp::LoadSeed { dst, index });
                }
                self.mark(op, true);
                Ok(())
            }
            // A runtime-indexed seed has no element-major lane form.
            LinearOp::LoadIndexedSeed { .. } => Err(unsupported(op_index, op)),
            _ if aggregate_lanes(op) == Some(2) => self.push_aggregate(op_index, op),
            LinearOp::TensorConcatenate { .. } | LinearOp::TensorTranspose { .. }
                if self.touches_region(op) =>
            {
                self.push_dual_packing(op_index, op)
            }
            _ => self.push_scalar(op_index, op),
        }
    }

    fn touches_region(&self, op: &LinearOp) -> bool {
        let mut probe = op.clone();
        let mut touches = op.dst_register().is_some_and(|start| {
            (start as usize..start as usize + op.dst_register_count())
                .any(|register| self.regions.slot(register as Reg) != Slot::Scalar)
        });
        visit_operands(&mut probe, &mut |start, shape| {
            touches |= shape.registers(*start).is_some_and(|registers| {
                registers
                    .iter()
                    .any(|register| self.regions.slot(*register) != Slot::Scalar)
            });
        });
        touches
    }

    /// Widen one dual aggregate: its primal lanes must not depend on the seed.
    fn push_aggregate(&mut self, op_index: usize, op: &LinearOp) -> Result<(), TangentLaneError> {
        let mut widened = op.clone();
        let mut failed = false;
        let this = &*self;
        let complete = visit_operands(&mut widened, &mut |start, shape| match this
            .aggregate_operand(*start, shape)
        {
            Some(mapped) => *start = mapped,
            None => failed = true,
        });
        match destination(&mut widened) {
            Some(dst) => match self.regions.slot(*dst) {
                Slot::Primal { region, element } => *dst = self.regions.primal(region, element),
                _ => failed = true,
            },
            None => failed = true,
        }
        set_aggregate_lanes(&mut widened, self.regions.width());
        if failed || !complete {
            return Err(unsupported(op_index, op));
        }
        self.ops.push(widened);
        self.mark(op, true);
        Ok(())
    }

    /// The widened start of one operand of a dual aggregate: a dual range
    /// maps to its region, a single-lane index operand stays as is when it is
    /// a seed-independent scalar at its own register.
    fn aggregate_operand(&self, start: Reg, shape: Operand) -> Option<Reg> {
        let registers = shape.registers(start)?;
        let scalar = |register: &Reg| self.regions.slot(*register) == Slot::Scalar;
        match (shape, self.regions.slot(start)) {
            (Operand::Strided { lanes: 2, .. }, Slot::Primal { region, element }) => {
                (!registers.iter().any(scalar)).then(|| self.regions.primal(region, element))
            }
            (Operand::Strided { lanes: 2, .. }, _) => None,
            (_, Slot::Scalar) => registers
                .iter()
                .all(|register| {
                    scalar(register)
                        && !self.per_lane(*register)
                        && self.homes[*register as usize].is_none()
                })
                .then_some(start),
            _ => None,
        }
    }

    /// Widen the two single-lane idioms that move between a dual region and
    /// its planes: interleaving a primal and a tangent plane into the region
    /// (`TensorConcatenate` of two `[c, 1]` columns along axis 1), and
    /// splitting a region into its planes (`TensorTranspose` of `[c, 2]`).
    fn push_dual_packing(
        &mut self,
        op_index: usize,
        op: &LinearOp,
    ) -> Result<(), TangentLaneError> {
        let width = self.regions.width();
        match op {
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis: 1,
                lanes: 1,
            } if dimensions.len() == 2 && dimensions[1] == 2 && sources.len() == 2 => {
                let count = dimensions[0] as usize;
                let (Slot::Primal { region, element }, true) = (
                    self.regions.slot(*dst_start),
                    self.region_run(*dst_start, 2 * count),
                ) else {
                    return Err(unsupported(op_index, op));
                };
                let [primal, tangent] = [&sources[0], &sources[1]]
                    .map(|source| (source.start..source.start + count as Reg).collect::<Vec<_>>());
                let column = |start| TensorConcatenateSource {
                    start,
                    dimensions: Box::new([dimensions[0], 1]),
                };
                if primal.iter().any(|register| self.per_lane(*register))
                    || primal
                        .iter()
                        .chain(&tangent)
                        .any(|register| self.regions.slot(*register) != Slot::Scalar)
                {
                    return Err(unsupported(op_index, op));
                }
                let mut columns = vec![column(self.range_start(0, &primal))];
                for lane in 0..self.lanes {
                    columns.push(column(self.range_start(lane, &tangent)));
                }
                self.ops.push(LinearOp::TensorConcatenate {
                    dst_start: self.regions.primal(region, element),
                    sources: columns.into_boxed_slice(),
                    dimensions: Box::new([dimensions[0], width as u32]),
                    axis: 1,
                    lanes: 1,
                });
                self.mark(op, true);
                Ok(())
            }
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows: 2,
                columns,
                element_width: 1,
                lanes: 1,
            } => {
                let count = *columns;
                let (Slot::Primal { region, element }, true) = (
                    self.regions.slot(*src_start),
                    self.region_run(*src_start, 2 * count),
                ) else {
                    return Err(unsupported(op_index, op));
                };
                if (*dst_start as usize..*dst_start as usize + 2 * count)
                    .any(|register| self.regions.slot(register as Reg) != Slot::Scalar)
                {
                    return Err(unsupported(op_index, op));
                }
                let planes = self.scratch(width * count)?;
                self.ops.push(LinearOp::TensorTranspose {
                    dst_start: planes,
                    src_start: self.regions.primal(region, element),
                    rows: width,
                    columns: count,
                    element_width: 1,
                    lanes: 1,
                });
                for offset in 0..count {
                    let primal = *dst_start as usize + offset;
                    self.place(primal, false, Box::new([planes + offset as Reg]));
                    let lanes = (0..self.lanes)
                        .map(|lane| planes + ((1 + lane) * count + offset) as Reg)
                        .collect();
                    self.place(primal + count, true, lanes);
                }
                Ok(())
            }
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes: 1,
            } => {
                self.push_concatenate_moves(op_index, op, *dst_start, (sources, dimensions, *axis))
            }
            _ => Err(unsupported(op_index, op)),
        }
    }

    /// Widen a single-lane concatenation that writes a dual region slot by
    /// slot: each primal slot moves once, each tangent slot once per lane.
    fn push_concatenate_moves(
        &mut self,
        op_index: usize,
        op: &LinearOp,
        dst_start: Reg,
        (sources, dimensions, axis): (&[TensorConcatenateSource], &[u32], usize),
    ) -> Result<(), TangentLaneError> {
        let product = |dimensions: &[u32]| {
            dimensions
                .iter()
                .map(|extent| *extent as usize)
                .product::<usize>()
        };
        let inner = product(dimensions.get(axis + 1..).unwrap_or_default());
        let result_axis = dimensions.get(axis).copied().unwrap_or_default() as usize;
        let mut moves = Vec::new();
        let mut axis_offset = 0usize;
        for source in sources {
            let source_axis = source.dimensions.get(axis).copied().unwrap_or_default() as usize;
            let block = (source_axis * inner).max(1);
            for element in 0..product(&source.dimensions) {
                let destination =
                    element / block * result_axis * inner + axis_offset * inner + element % block;
                moves.push((
                    dst_start + destination as Reg,
                    source.start + element as Reg,
                ));
            }
            axis_offset += source_axis;
        }
        for &(destination, source) in &moves {
            // A slot takes a scalar or the same lane of another dual region.
            let valid = match (self.regions.slot(destination), self.regions.slot(source)) {
                (Slot::Primal { .. }, Slot::Scalar) => !self.per_lane(source),
                (Slot::Primal { .. }, Slot::Primal { .. })
                | (Slot::Tangent { .. }, Slot::Scalar | Slot::Tangent { .. }) => true,
                _ => false,
            };
            if !valid {
                return Err(unsupported(op_index, op));
            }
        }
        for (destination, source) in moves {
            match self.regions.slot(destination) {
                Slot::Primal { region, element } => self.ops.push(LinearOp::Move {
                    dst: self.regions.primal(region, element),
                    src: self.read(0, source),
                }),
                Slot::Tangent { region, element } => {
                    self.move_tangent_lanes(region, element, source)
                }
                Slot::Scalar => {}
            }
        }
        self.mark(op, true);
        Ok(())
    }

    /// Move each lane of `source` into the tangent lanes of `(region, element)`.
    fn move_tangent_lanes(&mut self, region: usize, element: usize, source: Reg) {
        for lane in 0..self.lanes {
            self.ops.push(LinearOp::Move {
                dst: self.regions.tangent(region, element, lane),
                src: self.read(lane, source),
            });
        }
    }

    /// Whether `count` registers from `start` lie in one region.
    fn region_run(&self, start: Reg, count: usize) -> bool {
        let region = |register| match self.regions.slot(register) {
            Slot::Primal { region, .. } | Slot::Tangent { region, .. } => Some(region),
            Slot::Scalar => None,
        };
        let first = region(start);
        first.is_some() && (start..start + count as Reg).all(|register| region(register) == first)
    }

    /// Record that scalar `register` now lives at `homes`.
    fn place(&mut self, register: usize, dependent: bool, homes: Box<[Reg]>) {
        self.written[register] = true;
        self.dependent[register] = dependent;
        self.homes[register] = Some(homes);
        for lane in &mut self.at_default {
            lane[register] = false;
        }
    }

    fn push_scalar(&mut self, op_index: usize, op: &LinearOp) -> Result<(), TangentLaneError> {
        let mut operands = Vec::new();
        let mut probe = op.clone();
        let enumerable = visit_operands(&mut probe, &mut |start, shape| {
            operands.push((*start, shape))
        });
        if !enumerable {
            return self.push_verbatim(op_index, op);
        }
        let mut sources = Vec::with_capacity(operands.len());
        for &(start, shape) in &operands {
            let registers = shape
                .registers(start)
                .ok_or(TangentLaneError::RegisterOverflow)?;
            if shape != Operand::Single
                && registers
                    .iter()
                    .any(|register| self.regions.slot(*register) != Slot::Scalar)
            {
                return Err(unsupported(op_index, op));
            }
            sources.push(registers);
        }
        let writes_tangent = op
            .dst_register()
            .is_some_and(|dst| matches!(self.regions.slot(dst), Slot::Tangent { .. }));
        let replicate = writes_tangent
            || sources
                .iter()
                .flatten()
                .any(|register| self.per_lane(*register));
        for lane in 0..if replicate { self.lanes } else { 1 } {
            let starts = self.lane_operands(lane, &operands, &sources);
            let target = match op.dst_register() {
                Some(dst) if replicate => Some(self.write_target(op_index, op, lane, dst)?),
                Some(dst) => Some(self.write_once(op_index, op, dst)?),
                None => None,
            };
            self.ops.push(with_registers(op, &starts, target));
        }
        if replicate {
            self.mark_replicated_scalar(op, &sources);
        } else {
            self.mark(op, false);
        }
        Ok(())
    }

    /// Operand starts for lane `lane`.
    fn lane_operands(
        &mut self,
        lane: usize,
        operands: &[(Reg, Operand)],
        sources: &[Vec<Reg>],
    ) -> Vec<Reg> {
        let mut starts = Vec::with_capacity(operands.len());
        for (&(start, shape), registers) in operands.iter().zip(sources) {
            let mapped = if shape == Operand::Single {
                self.read(lane, start)
            } else {
                self.range_start(lane, registers)
            };
            starts.push(mapped);
        }
        starts
    }

    /// Nested programs and random streams run once, verbatim: they may touch
    /// no dual region, no relocated register, and nothing seed-dependent.
    fn push_verbatim(&mut self, op_index: usize, op: &LinearOp) -> Result<(), TangentLaneError> {
        let readable = (0..self.registers)
            .map(|register| {
                self.written[register]
                    && !self.dependent[register]
                    && self.homes[register].is_none()
                    && self.regions.slot(register as Reg) == Slot::Scalar
            })
            .collect::<Vec<_>>();
        let dst_scalar = op.dst_register().is_none_or(|dst| {
            (dst as usize..dst as usize + op.dst_register_count())
                .all(|register| self.regions.slot(register as Reg) == Slot::Scalar)
        });
        if !dst_scalar || !ScalarProgramRegisterFlow::op_reads_only(op, op_index, &readable) {
            return Err(unsupported(op_index, op));
        }
        self.ops.push(op.clone());
        self.mark(op, false);
        Ok(())
    }

    /// The widened destination lane `lane` of a replicated operation writes.
    fn write_target(
        &self,
        op_index: usize,
        op: &LinearOp,
        lane: usize,
        dst: Reg,
    ) -> Result<Reg, TangentLaneError> {
        let count = op.dst_register_count();
        match self.regions.slot(dst) {
            Slot::Scalar
                if (dst as usize..dst as usize + count)
                    .all(|register| self.regions.slot(register as Reg) == Slot::Scalar) =>
            {
                Ok(self.default_copy(lane, dst))
            }
            Slot::Tangent { region, element } if count == 1 => {
                Ok(self.regions.tangent(region, element, lane))
            }
            _ => Err(unsupported(op_index, op)),
        }
    }

    /// The widened destination of an operation that runs once.
    fn write_once(
        &self,
        op_index: usize,
        op: &LinearOp,
        dst: Reg,
    ) -> Result<Reg, TangentLaneError> {
        let count = op.dst_register_count();
        match self.regions.slot(dst) {
            Slot::Scalar
                if (dst as usize..dst as usize + count)
                    .all(|register| self.regions.slot(register as Reg) == Slot::Scalar) =>
            {
                Ok(dst)
            }
            Slot::Primal { region, element } if count == 1 => {
                Ok(self.regions.primal(region, element))
            }
            _ => Err(unsupported(op_index, op)),
        }
    }

    /// Record seed dependence of `op`'s destinations, written at their
    /// default copies (every lane when dependent, lane 0 otherwise).
    fn mark(&mut self, op: &LinearOp, dependent: bool) {
        let Some(start) = op.dst_register() else {
            return;
        };
        for register in start as usize..start as usize + op.dst_register_count() {
            self.mark_register(register, dependent, dependent);
        }
    }

    fn mark_written_by_every_lane(&mut self, start: usize, count: usize, dependent: bool) {
        for register in start..start + count {
            self.mark_register(register, dependent, true);
        }
    }

    fn mark_register(&mut self, register: usize, dependent: bool, every_lane: bool) {
        self.written[register] = true;
        self.dependent[register] = dependent;
        self.homes[register] = None;
        for (lane, copies) in self.at_default.iter_mut().enumerate() {
            copies[register] = lane == 0 || every_lane;
        }
    }

    /// Record a replicated scalar operation. A directional pure call's primal
    /// results depend only on its primal inputs, so they stay seed-independent
    /// when those do. Its interface interleaves each real input and each real
    /// result with its tangent.
    fn mark_replicated_scalar(&mut self, op: &LinearOp, sources: &[Vec<Reg>]) {
        let (LinearOp::PureCallDirectional { site, .. }, Some(start)) = (op, op.dst_register())
        else {
            self.mark(op, true);
            return;
        };
        let inputs = site.inputs();
        let mut primal_dependent = false;
        let mut input = 0;
        while input < inputs.len() {
            primal_dependent |= sources[input]
                .iter()
                .any(|register| self.per_lane(*register));
            input += if is_real(&inputs[input]) { 2 } else { 1 };
        }
        let outputs = site.outputs();
        let mut register = start as usize;
        let mut output = 0;
        while output < outputs.len() {
            let paired = outputs[output].kind() == SolvePureCallOutputKind::Result
                && is_real(outputs[output].value_type());
            let leaves = outputs[output..].iter().take(if paired { 2 } else { 1 });
            for (offset, leaf) in leaves.enumerate() {
                let count = leaf.value_type().scalar_count() as usize;
                self.mark_written_by_every_lane(register, count, offset == 1 || primal_dependent);
                register += count;
            }
            output += if paired { 2 } else { 1 };
        }
    }

    /// Copy each lane's value of `sources` into the lane-major output block.
    fn store(&mut self, sources: &[Reg]) -> Result<(), TangentLaneError> {
        for &source in sources {
            for lane in 0..self.lanes {
                let slot = lane
                    .checked_mul(self.lane_outputs)
                    .and_then(|slot| slot.checked_add(self.next_output))
                    .ok_or(TangentLaneError::RegisterOverflow)?;
                self.ops.push(LinearOp::Move {
                    dst: self.output_base + slot as Reg,
                    src: self.read(lane, source),
                });
            }
            self.next_output += 1;
        }
        Ok(())
    }

    pub(super) fn finish(mut self) -> Vec<LinearOp> {
        let count = self.lanes * self.lane_outputs;
        if count > 0 {
            self.ops.push(LinearOp::StoreOutputRange {
                start: self.output_base,
                count,
                stride: 1,
            });
        }
        self.ops
    }
}

fn is_real(value_type: &crate::SolveValueType) -> bool {
    matches!(
        value_type.element_type(),
        crate::SolveScalarType::Real { .. }
    )
}

/// `op` with its operand starts replaced in visit order and its destination
/// replaced by `target`.
fn with_registers(op: &LinearOp, starts: &[Reg], target: Option<Reg>) -> LinearOp {
    let mut copy = op.clone();
    let mut next = starts.iter().copied();
    visit_operands(&mut copy, &mut |start, _| {
        *start = next.next().unwrap_or(*start)
    });
    if let (Some(dst), Some(target)) = (destination(&mut copy), target) {
        *dst = target;
    }
    copy
}

/// Whether `mapped` holds `registers`' values at the same relative offsets.
fn contiguous_run(mapped: &[Reg], registers: &[Reg]) -> bool {
    mapped
        .iter()
        .zip(registers)
        .all(|(mapped_register, register)| {
            mapped_register.checked_sub(mapped[0]) == register.checked_sub(registers[0])
        })
}

/// Set the interleaved width of a dual tensor aggregate.
fn set_aggregate_lanes(op: &mut LinearOp, width: usize) {
    match op {
        LinearOp::MatrixMultiply { lanes, .. }
        | LinearOp::TensorBinary { lanes, .. }
        | LinearOp::TensorCross { lanes, .. }
        | LinearOp::TensorTranspose { lanes, .. }
        | LinearOp::TensorConcatenate { lanes, .. }
        | LinearOp::TensorUpdate { lanes, .. }
        | LinearOp::TensorFill { lanes, .. }
        | LinearOp::TensorIdentity { lanes, .. }
        | LinearOp::TensorLoad { lanes, .. } => *lanes = width,
        _ => {}
    }
}
