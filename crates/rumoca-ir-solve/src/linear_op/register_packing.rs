//! Register packing of a straight-line program by value lifetime.
//!
//! Lowering numbers registers so every value has its own: a program of many
//! short-lived values needs a register file as wide as all of them at once,
//! which a generated C function holds as one stack array. Packing lets values
//! whose lifetimes do not meet share registers, so the array is as wide as the
//! values live at one time.
//!
//! Registers an operation reads or writes as one range must keep their
//! relative offsets, so overlapping ranges are merged into one chunk that
//! moves as a whole. A chunk lives from the first to the last operation that
//! touches it, and two chunks whose lifetimes meet, even at one operation,
//! never share a register, so no operation reads a register it also writes
//! unless the original program did.
//!
//! Only single registers share: a chunk of several registers, a range the C
//! code walks in a loop, keeps registers no other chunk takes. A C compiler
//! that sees a range's slots reused by later values can no longer rule out
//! an overlap between the loop and those accesses, and reloads and stores
//! values it would keep in machine registers.

use super::*;
use crate::tangent_lanes::operands::{destination, visit_operands};

/// `program` with its registers packed, or `None` when packing would not
/// narrow it or the program holds an operation whose register fields this
/// pass does not enumerate (nested programs, folds, random streams). Every
/// operation keeps its kind, order, and fields other than registers, so each
/// value is computed from the same operands bit for bit.
#[must_use]
pub fn pack_registers(program: &[LinearOp]) -> Option<Vec<LinearOp>> {
    let touches = program
        .iter()
        .map(register_spans)
        .collect::<Option<Vec<_>>>()?;
    let chunks = Chunks::merge(touches.iter().flatten().copied());
    let width = chunks.ends.last().copied()?;
    let bases = chunks.allocate(&touches)?;
    let packed_width = chunks
        .starts
        .iter()
        .zip(&chunks.ends)
        .zip(&bases)
        .map(|((start, end), base)| base + (end - start))
        .max()?;
    if packed_width >= width {
        return None;
    }
    // Every packed register is below `packed_width < width`, and `width - 1`
    // is a register, so each relocation fits.
    let relocate = |register: &mut Reg| {
        let chunk = chunks.of(*register as usize);
        let packed = bases[chunk] + (*register as usize - chunks.starts[chunk]);
        if let Ok(packed) = Reg::try_from(packed) {
            *register = packed;
        }
    };
    let mut packed = program.to_vec();
    for op in &mut packed {
        visit_operands(op, &mut |register, _| relocate(register));
        if let Some(register) = destination(op) {
            relocate(register);
        }
    }
    Some(packed)
}

/// The register ranges `[start, end)` `op` reads and writes, or `None` when
/// they are not enumerable, overflow, or are empty: an empty range covers no
/// register, so its start belongs to no chunk.
fn register_spans(op: &LinearOp) -> Option<Vec<(usize, usize)>> {
    let mut op = op.clone();
    let mut spans = Vec::new();
    let mut valid = true;
    let enumerated = visit_operands(&mut op, &mut |register, operand| match operand
        .span()
        .and_then(|len| span(*register, len))
    {
        Some(Some(range)) => spans.push(range),
        Some(None) | None => valid = false,
    });
    if !enumerated || !valid {
        return None;
    }
    if let Some(&mut start) = destination(&mut op) {
        spans.push(span(start, op.dst_register_count())??);
    }
    Some(spans)
}

/// `[start, start + len)`, `None` inside when empty, or `None` on overflow.
fn span(start: Reg, len: usize) -> Option<Option<(usize, usize)>> {
    if len == 0 {
        return Some(None);
    }
    let start = start as usize;
    let end = start.checked_add(len)?;
    Reg::try_from(end - 1).ok()?;
    Some(Some((start, end)))
}

/// Disjoint register chunks `[starts[k], ends[k])`, ascending.
struct Chunks {
    starts: Vec<usize>,
    ends: Vec<usize>,
}

impl Chunks {
    /// Merge every overlapping range into one chunk.
    fn merge(ranges: impl Iterator<Item = (usize, usize)>) -> Self {
        let mut ranges = ranges.collect::<Vec<_>>();
        ranges.sort_unstable();
        let mut chunks = Self {
            starts: Vec::new(),
            ends: Vec::new(),
        };
        for (start, end) in ranges {
            match chunks.ends.last_mut() {
                Some(last) if start < *last => *last = (*last).max(end),
                _ => {
                    chunks.starts.push(start);
                    chunks.ends.push(end);
                }
            }
        }
        chunks
    }

    /// The chunk holding `register`, which some range of the program covers.
    fn of(&self, register: usize) -> usize {
        self.starts.partition_point(|&start| start <= register) - 1
    }

    /// The packed base register of every chunk. Single-register chunks, in
    /// order of first use, take the lowest registers no single-register chunk
    /// live at the same time holds; every wider chunk then takes registers of
    /// its own above them.
    fn allocate(&self, touches: &[Vec<(usize, usize)>]) -> Option<Vec<usize>> {
        let count = self.starts.len();
        let mut first = vec![usize::MAX; count];
        let mut last = vec![0usize; count];
        for (index, spans) in touches.iter().enumerate() {
            for &(start, _) in spans {
                let chunk = self.of(start);
                first[chunk] = first[chunk].min(index);
                last[chunk] = last[chunk].max(index);
            }
        }
        let mut order = (0..count).collect::<Vec<_>>();
        order.sort_by_key(|&chunk| (first[chunk], chunk));
        let size = |chunk: usize| self.ends[chunk] - self.starts[chunk];
        let mut bases = vec![0usize; count];
        // Live single registers as (base, end, last use), ascending by base.
        let mut live: Vec<(usize, usize, usize)> = Vec::new();
        let mut next = 0usize;
        for &chunk in order.iter().filter(|&&chunk| size(chunk) == 1) {
            live.retain(|&(_, _, until)| until >= first[chunk]);
            let base = first_fit(&live, 1)?;
            bases[chunk] = base;
            next = next.max(base + 1);
            let at = live.partition_point(|&(start, _, _)| start < base);
            live.insert(at, (base, base + 1, last[chunk]));
        }
        for &chunk in order.iter().filter(|&&chunk| size(chunk) > 1) {
            bases[chunk] = next;
            next = next.checked_add(size(chunk))?;
        }
        Some(bases)
    }
}

/// The lowest base where `size` registers meet none of `live`, ascending
/// by base, or `None` on overflow.
fn first_fit(live: &[(usize, usize, usize)], size: usize) -> Option<usize> {
    let mut base = 0usize;
    for &(start, end, _) in live {
        if base.checked_add(size)? <= start {
            break;
        }
        base = base.max(end);
    }
    Some(base)
}

#[cfg(test)]
mod tests;
