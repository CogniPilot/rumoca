//! The slices of several stored outputs of one program in one backward pass.
//!
//! [`slice_to_output`] sweeps a program back from one store; slicing every
//! row of an `n`-element equation that way walks each `n`-element tensor
//! operation once per row. Here each register holds the set of requested
//! outputs it is still live for, so every operation, and every element of a
//! tensor operation, is visited once for all of them. Each output's live
//! registers, kept operations, narrowing and decline are exactly those of its
//! own sweep: an output's liveness never touches another's.

use super::*;
use std::collections::{BTreeMap, BTreeSet};

/// For each of `outputs`, what [`slice_to_output`] returns for it. When
/// `bounded`, `None` for the whole request once the slices together would
/// compute more than the program does once: each operation weighs the
/// register elements it writes (one for a scalar or a store, so a tensor
/// operation a slice keeps whole weighs its whole extent), and an output
/// that does not slice weighs the whole program.
///
/// The bound is checked as the pass goes. The running total only drops
/// when an output declines, and that output then weighs the whole program,
/// so an early rejection answers as the finished total would: either the
/// total stays past the program, or the single requested output declines
/// and slices nothing either way.
#[must_use]
pub fn slice_to_outputs(
    program: &[LinearOp],
    outputs: &[usize],
    bounded: bool,
) -> Option<Vec<Option<Vec<LinearOp>>>> {
    let mut pass = Pass::new(program, outputs, bounded)?;
    for index in (0..program.len()).rev() {
        pass.step(index)?;
        pass.activate(index)?;
    }
    Some(pass.finish())
}

struct Pass<'a> {
    program: &'a [LinearOp],
    /// Per requested output, its store and stored register.
    targets: Vec<Option<(usize, Reg)>>,
    /// Requested outputs by the index of their store.
    stores: BTreeMap<usize, Vec<usize>>,
    /// Per register, the outputs it is live for.
    live: BTreeMap<Reg, BTreeSet<usize>>,
    /// Outputs whose sweep has passed their store and not declined.
    active: BTreeSet<usize>,
    declined: Vec<bool>,
    /// Per output, its kept operations, last first.
    kept: Vec<Vec<(usize, Kept)>>,
    /// Per output, the work of its slice so far.
    lengths: Vec<usize>,
    total: usize,
    /// The work of the whole program, which a declined output weighs.
    whole: usize,
    budget: usize,
}

impl<'a> Pass<'a> {
    fn new(program: &'a [LinearOp], outputs: &[usize], bounded: bool) -> Option<Self> {
        let whole = program_work(program);
        // An output stored before an operation that can fail declines.
        let last_fallible = program.iter().rposition(can_fail);
        let targets: Vec<_> = outputs
            .iter()
            .map(|&output| {
                output_register(program, output)
                    .filter(|&(store, _)| last_fallible.is_none_or(|fallible| fallible < store))
            })
            .collect();
        let mut stores = BTreeMap::<usize, Vec<usize>>::new();
        let mut total = 0usize;
        for (row, target) in targets.iter().enumerate() {
            match target {
                Some((store, _)) => stores.entry(*store).or_default().push(row),
                None => total = total.saturating_add(whole),
            }
        }
        let pass = Self {
            program,
            declined: targets.iter().map(Option::is_none).collect(),
            kept: vec![Vec::new(); targets.len()],
            lengths: vec![0; targets.len()],
            targets,
            stores,
            live: BTreeMap::new(),
            active: BTreeSet::new(),
            total,
            whole,
            budget: if bounded { whole } else { usize::MAX },
        };
        pass.within_budget().then_some(pass)
    }

    fn within_budget(&self) -> bool {
        self.total <= self.budget
    }

    /// Start the sweep of every output stored at `index`: it reads the
    /// operations before its store.
    fn activate(&mut self, index: usize) -> Option<()> {
        for row in self.stores.remove(&index).unwrap_or_default() {
            let (_, target) = self.targets[row]?;
            self.live.entry(target).or_default().insert(row);
            self.active.insert(row);
            self.add(row, 1);
        }
        self.within_budget().then_some(())
    }

    fn add(&mut self, row: usize, ops: usize) {
        self.lengths[row] = self.lengths[row].saturating_add(ops);
        self.total = self.total.saturating_add(ops);
    }

    /// `row` declines: it weighs the whole program from now on.
    fn decline(&mut self, row: usize) {
        if self.declined[row] {
            return;
        }
        self.declined[row] = true;
        self.active.remove(&row);
        self.total = self
            .total
            .saturating_sub(self.lengths[row])
            .saturating_add(self.whole);
    }

    fn decline_active(&mut self) {
        let rows: Vec<usize> = self.active.iter().copied().collect();
        for row in rows {
            self.decline(row);
        }
    }

    /// The live outputs of `register`, removed from it.
    fn take(&mut self, register: Reg) -> BTreeSet<usize> {
        let mut rows = self.live.remove(&register).unwrap_or_default();
        rows.retain(|&row| !self.declined[row]);
        rows
    }

    fn read(&mut self, register: Reg, rows: impl IntoIterator<Item = usize>) {
        self.live.entry(register).or_default().extend(rows);
    }

    /// Sweep operation `index` for every active output, as [`sweep`] does
    /// for one.
    fn step(&mut self, index: usize) -> Option<()> {
        let op = &self.program[index];
        match op {
            LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. } => {}
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadY { dst, .. }
            | LinearOp::LoadP { dst, .. } => self.scalar(index, *dst, &[]),
            LinearOp::Move { dst, src } => self.scalar(index, *dst, &[*src]),
            LinearOp::Unary { dst, arg, .. } => self.scalar(index, *dst, &[*arg]),
            LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
                self.scalar(index, *dst, &[*lhs, *rhs]);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.scalar(index, *dst, &[*cond, *if_true, *if_false]),
            _ => self.elementwise(index),
        }
        self.within_budget().then_some(())
    }

    fn scalar(&mut self, index: usize, dst: Reg, reads: &[Reg]) {
        let rows = self.take(dst);
        let work = work(&self.program[index]);
        for &row in &rows {
            self.kept[row].push((index, Kept::Whole));
            self.add(row, work);
        }
        for &register in reads {
            self.read(register, rows.iter().copied());
        }
    }

    /// A tensor operation the slicer narrows, or any other operation, which
    /// declines every active output.
    fn elementwise(&mut self, index: usize) {
        let op = &self.program[index];
        let Some((dst, count, overlap)) = element_shape(op) else {
            self.decline_active();
            return;
        };
        // A destination past the register space declines, as the sweep of
        // one output does on reaching that element.
        if count > 0 && register(dst, count - 1).is_none() {
            self.decline_active();
            return;
        }
        // Per live output, the destination elements it reads, ascending: only
        // the live registers of the destination are visited.
        let end = u64::from(dst) + count as u64;
        let registers: Vec<Reg> = self
            .live
            .range(dst..)
            .map(|(&register, _)| register)
            .take_while(|&register| u64::from(register) < end)
            .collect();
        let mut elements = BTreeMap::<usize, Vec<usize>>::new();
        for register in registers {
            let k = (register - dst) as usize;
            for row in self.take(register) {
                elements.entry(row).or_default().push(k);
            }
        }
        for (row, elements) in elements {
            if overlap {
                self.decline(row);
                continue;
            }
            let Some(sources) = elements
                .iter()
                .map(|&k| element_sources(op, k))
                .collect::<Option<Vec<_>>>()
            else {
                self.decline(row);
                continue;
            };
            for register in sources.into_iter().flatten().flatten() {
                self.read(register, [row]);
            }
            // Whole or narrowed, the slice computes these elements.
            let work = elements.len();
            self.kept[row].push((index, Kept::Elements(elements)));
            self.add(row, work);
        }
    }

    fn finish(self) -> Vec<Option<Vec<LinearOp>>> {
        let program = self.program;
        self.kept
            .into_iter()
            .zip(self.targets)
            .zip(self.declined)
            .map(|((kept, target), declined)| {
                let (_, target) = target?;
                (!declined).then_some(())?;
                assemble(program, kept, target)
            })
            .collect()
    }
}

/// One output's slice from its kept operations, last first, as
/// [`slice_to_output`] assembles it.
fn assemble(program: &[LinearOp], kept: Vec<(usize, Kept)>, target: Reg) -> Option<Vec<LinearOp>> {
    let mut sliced = Vec::new();
    for (index, kept) in kept.into_iter().rev() {
        match kept {
            Kept::Whole => sliced.push(program[index].clone()),
            Kept::Elements(elements) => narrow(&program[index], &elements, &mut sliced)?,
        }
    }
    sliced.push(LinearOp::StoreOutput { src: target });
    Some(sliced)
}

/// The destination, element count and source-destination overlap of a
/// tensor operation the slicer narrows, or `None` for an operation it does
/// not model.
fn element_shape(op: &LinearOp) -> Option<(Reg, usize, bool)> {
    match op {
        LinearOp::TensorLoad {
            dst_start,
            count,
            seed_start: None,
            lanes: 1,
            ..
        } => Some((*dst_start, *count, false)),
        LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => Some((
            *dst_start,
            *count,
            overlaps(*dst_start, *count, *lhs_start, *count, *lhs_stride)
                || overlaps(*dst_start, *count, *rhs_start, *count, *rhs_stride),
        )),
        LinearOp::TensorIntegerPower {
            dst_start,
            src_start,
            count,
            lanes: 1,
            ..
        } => Some((
            *dst_start,
            *count,
            overlaps(*dst_start, *count, *src_start, *count, 1),
        )),
        LinearOp::TensorFill { lanes: 1, .. } | LinearOp::TensorTranspose { lanes: 1, .. } => {
            let (dst, count, src, src_count) = copy_shape(op)?;
            Some((dst, count, overlaps(dst, count, src, src_count, 1)))
        }
        _ => None,
    }
}

/// The registers destination element `k` reads, or `None` when one is
/// outside the register space.
fn element_sources(op: &LinearOp, k: usize) -> Option<[Option<Reg>; 2]> {
    match op {
        LinearOp::TensorLoad { .. } => Some([None, None]),
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            lhs_stride,
            rhs_stride,
            ..
        } => Some([
            Some(register(*lhs_start, k * lhs_stride)?),
            Some(register(*rhs_start, k * rhs_stride)?),
        ]),
        LinearOp::TensorIntegerPower { src_start, .. } => {
            Some([Some(register(*src_start, k)?), None])
        }
        _ => Some([Some(copy_source(op, k)?), None]),
    }
}

#[cfg(test)]
mod tests;
