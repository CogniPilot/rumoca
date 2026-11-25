//! The computation of one stored output of a straight-line program.
//!
//! A residual program of an array equation stores every row of the equation;
//! a caller that reads one row needs only that row's computation. Liveness is
//! tracked per register element: an elementwise tensor operation (a load, a
//! binary operation, an integer power, a scalar fill, or a transpose) whose
//! destination is partly read is narrowed to the elements read, so the slice
//! of a row of an `n`-element equation does about `1/n` of the work.

use super::*;
use crate::FaultClass;
use std::collections::BTreeSet;

mod rows;
pub use rows::slice_to_outputs;

/// The program that computes stored output `output` of `program` and stores
/// it as its only output, or `None` when `output` is out of range or the
/// program holds an operation whose register reads and writes this slicer
/// does not model. Every kept operation reads and writes the same registers
/// as in `program` and keeps its order, so the stored value is the same bit
/// for bit. Only the operations before the store of `output` can produce the
/// value it stores; a later write to its register is another value.
///
/// A slice only drops operations that cannot fail: the full program fails
/// every output with any of its calls, lookups or solves, so an output stored
/// before one of them declines. Every operation the sweep models cannot fail
/// either, and any other declines before its store.
#[must_use]
pub fn slice_to_output(program: &[LinearOp], output: usize) -> Option<Vec<LinearOp>> {
    let (store, target) = output_register(program, output)?;
    if program[store + 1..].iter().any(can_fail) {
        return None;
    }
    let mut live = BTreeSet::from([target]);
    // The elements each kept operation must still write, in program order.
    let mut keep: Vec<Option<Kept>> = vec![None; program.len()];
    for (index, op) in program[..store].iter().enumerate().rev() {
        keep[index] = sweep(op, &mut live)?;
    }
    let mut sliced = Vec::new();
    for (op, kept) in program.iter().zip(keep) {
        match kept {
            None => {}
            Some(Kept::Whole) => sliced.push(op.clone()),
            Some(Kept::Elements(elements)) => narrow(op, &elements, &mut sliced)?,
        }
    }
    sliced.push(LinearOp::StoreOutput { src: target });
    Some(sliced)
}

/// What one operation still has to compute.
#[derive(Clone, Debug)]
enum Kept {
    Whole,
    /// The destination offsets (in elements) a tensor operation still writes.
    Elements(Vec<usize>),
}

/// The index of the operation that stores a program's `output`-th output,
/// and the register it stores.
fn output_register(program: &[LinearOp], output: usize) -> Option<(usize, Reg)> {
    let mut ordinal = 0usize;
    for (index, op) in program.iter().enumerate() {
        match op {
            LinearOp::StoreOutput { src } => {
                if ordinal == output {
                    return Some((index, *src));
                }
                ordinal += 1;
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                if output < ordinal + count {
                    let element = u32::try_from((output - ordinal) * stride).ok()?;
                    return Some((index, start.checked_add(element)?));
                }
                ordinal += count;
            }
            LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. } => {
                return None;
            }
            _ => {}
        }
    }
    None
}

/// The register elements `op` writes, one for a scalar or a store: the work
/// a residual sweep weighs a program and its slices by.
pub(super) fn work(op: &LinearOp) -> usize {
    op.dst_register_count().max(1)
}

pub(super) fn program_work(program: &[LinearOp]) -> usize {
    program.iter().map(work).fold(0usize, usize::saturating_add)
}

/// Whether `op` can fail the program, a store aside: the operations its
/// fault class lets no pass drop.
fn can_fail(op: &LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
    ) && op.fault_class() != FaultClass::CannotFault
}

fn register(base: Reg, offset: usize) -> Option<Reg> {
    base.checked_add(u32::try_from(offset).ok()?)
}

/// Update `live` across `op` (read backwards) and say what of `op` stays.
/// `None` aborts the slice.
fn sweep(op: &LinearOp, live: &mut BTreeSet<Reg>) -> Option<Option<Kept>> {
    let scalar = |live: &mut BTreeSet<Reg>, dst: Reg, reads: &[Reg]| {
        if live.remove(&dst) {
            live.extend(reads.iter().copied());
            Some(Kept::Whole)
        } else {
            None
        }
    };
    let kept = match op {
        LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. } => None,
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. } => scalar(live, *dst, &[]),
        LinearOp::Move { dst, src } => scalar(live, *dst, &[*src]),
        LinearOp::Unary { dst, arg, .. } => scalar(live, *dst, &[*arg]),
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            scalar(live, *dst, &[*lhs, *rhs])
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => scalar(live, *dst, &[*cond, *if_true, *if_false]),
        LinearOp::TensorLoad {
            dst_start,
            count,
            seed_start: None,
            lanes: 1,
            ..
        } => {
            let elements = live_elements(live, *dst_start, *count)?;
            (!elements.is_empty()).then_some(Kept::Elements(elements))
        }
        LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => {
            let elements = live_elements(live, *dst_start, *count)?;
            if elements.is_empty() {
                return Some(None);
            }
            // A source range that overlaps the destination would make an
            // element read another element this operation writes.
            if overlaps(*dst_start, *count, *lhs_start, *count, *lhs_stride)
                || overlaps(*dst_start, *count, *rhs_start, *count, *rhs_stride)
            {
                return None;
            }
            for &k in &elements {
                live.insert(register(*lhs_start, k * lhs_stride)?);
                live.insert(register(*rhs_start, k * rhs_stride)?);
            }
            Some(Kept::Elements(elements))
        }
        LinearOp::TensorIntegerPower {
            dst_start,
            src_start,
            count,
            lanes: 1,
            ..
        } => {
            // Element `k` reads source element `k` alone.
            let elements = live_elements(live, *dst_start, *count)?;
            if elements.is_empty() {
                return Some(None);
            }
            if overlaps(*dst_start, *count, *src_start, *count, 1) {
                return None;
            }
            for &k in &elements {
                live.insert(register(*src_start, k)?);
            }
            Some(Kept::Elements(elements))
        }
        LinearOp::TensorFill { lanes: 1, .. } | LinearOp::TensorTranspose { lanes: 1, .. } => {
            return sweep_copy(op, live);
        }
        _ => return None,
    };
    Some(kept)
}

/// The destination range of an elementwise copy (a scalar fill or a
/// transpose) and the one register element `k` copies.
fn copy_shape(op: &LinearOp) -> Option<(Reg, usize, Reg, usize)> {
    match op {
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            lanes: 1,
        } => Some((*dst_start, *count, *value_start, 1)),
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes: 1,
        } => {
            let count = rows.checked_mul(*columns)?.checked_mul(*element_width)?;
            Some((*dst_start, count, *src_start, count))
        }
        _ => None,
    }
}

/// The register destination element `k` of a copy reads.
fn copy_source(op: &LinearOp, k: usize) -> Option<Reg> {
    match op {
        LinearOp::TensorFill { value_start, .. } => Some(*value_start),
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            ..
        } => {
            // dst (i*columns + j)*width + e reads src (j*rows + i)*width + e.
            let (q, e) = (k / element_width, k % element_width);
            let (i, j) = (q / columns, q % columns);
            register(*src_start, (j * rows + i) * element_width + e)
        }
        _ => None,
    }
}

fn sweep_copy(op: &LinearOp, live: &mut BTreeSet<Reg>) -> Option<Option<Kept>> {
    let (dst, count, src, src_count) = copy_shape(op)?;
    let elements = live_elements(live, dst, count)?;
    if elements.is_empty() {
        return Some(None);
    }
    if overlaps(dst, count, src, src_count, 1) {
        return None;
    }
    for &k in &elements {
        live.insert(copy_source(op, k)?);
    }
    Some(Some(Kept::Elements(elements)))
}

/// Whether `[src, src + (count - 1) * stride]` meets `[dst, dst + dst_count)`.
fn overlaps(dst: Reg, dst_count: usize, src: Reg, count: usize, stride: usize) -> bool {
    if count == 0 || dst_count == 0 {
        return false;
    }
    let (dst, src) = (dst as usize, src as usize);
    let src_end = src + (count - 1) * stride;
    src < dst + dst_count && dst <= src_end
}

/// The destination elements of `[dst_start, dst_start + count)` that are
/// live, removed from `live`.
fn live_elements(live: &mut BTreeSet<Reg>, dst_start: Reg, count: usize) -> Option<Vec<usize>> {
    let mut elements = Vec::new();
    for k in 0..count {
        if live.remove(&register(dst_start, k)?) {
            elements.push(k);
        }
    }
    Some(elements)
}

/// Emit the part of a tensor operation that writes `elements`: the whole
/// operation when every element is read, else one scalar operation per
/// element read.
fn narrow(op: &LinearOp, elements: &[usize], out: &mut Vec<LinearOp>) -> Option<()> {
    match op {
        LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            ..
        } => {
            if elements.len() == *count {
                out.push(op.clone());
                return Some(());
            }
            for &k in elements {
                let dst = register(*dst_start, k)?;
                let index = input_start + k;
                out.push(match input {
                    TensorInputKind::Y => LinearOp::LoadY { dst, index },
                    TensorInputKind::P => LinearOp::LoadP { dst, index },
                });
            }
        }
        LinearOp::TensorBinary {
            dst_start,
            op: binary,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            if elements.len() == *count {
                out.push(op.clone());
                return Some(());
            }
            for &k in elements {
                out.push(LinearOp::Binary {
                    dst: register(*dst_start, k)?,
                    op: *binary,
                    lhs: register(*lhs_start, k * lhs_stride)?,
                    rhs: register(*rhs_start, k * rhs_stride)?,
                });
            }
        }
        LinearOp::TensorIntegerPower {
            dst_start,
            src_start,
            count,
            power,
            lanes,
        } => {
            if elements.len() == *count {
                out.push(op.clone());
                return Some(());
            }
            // One element of the power runs the same steps on one element.
            for &k in elements {
                out.push(LinearOp::TensorIntegerPower {
                    dst_start: register(*dst_start, k)?,
                    src_start: register(*src_start, k)?,
                    count: 1,
                    power: *power,
                    lanes: *lanes,
                });
            }
        }
        LinearOp::TensorFill { .. } | LinearOp::TensorTranspose { .. } => {
            let (dst_start, count, _, _) = copy_shape(op)?;
            if elements.len() == count {
                out.push(op.clone());
                return Some(());
            }
            for &k in elements {
                out.push(LinearOp::Move {
                    dst: register(dst_start, k)?,
                    src: copy_source(op, k)?,
                });
            }
        }
        _ => return None,
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::super::random_programs::{Stream, random_program, run};
    use super::*;

    /// r[0..3] = y[0..3]; r[3..6] = p[0..3]; r[6..9] = r[0..3] * r[3..6];
    /// r[9] = 1; r[10..13] = r[6..9] - r[9] (broadcast); store r[10..13].
    fn vector_program() -> Vec<LinearOp> {
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 3,
                input: TensorInputKind::P,
                input_start: 0,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: 6,
                op: BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: 3,
                count: 3,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::Const { dst: 9, value: 1.0 },
            LinearOp::TensorBinary {
                dst_start: 10,
                op: BinaryOp::Sub,
                lhs_start: 6,
                rhs_start: 9,
                count: 3,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 10,
                count: 3,
                stride: 1,
            },
        ]
    }

    #[test]
    fn a_row_of_an_array_equation_keeps_only_its_elements() {
        let sliced = slice_to_output(&vector_program(), 1).expect("the program is sliceable");
        assert_eq!(
            sliced,
            vec![
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::LoadP { dst: 4, index: 1 },
                LinearOp::Binary {
                    dst: 7,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 4
                },
                LinearOp::Const { dst: 9, value: 1.0 },
                LinearOp::Binary {
                    dst: 11,
                    op: BinaryOp::Sub,
                    lhs: 7,
                    rhs: 9
                },
                LinearOp::StoreOutput { src: 11 },
            ]
        );
    }

    #[test]
    fn a_scalar_output_keeps_its_dependency_chain() {
        let program = vec![
            LinearOp::LoadY { dst: 0, index: 2 },
            LinearOp::LoadY { dst: 1, index: 3 },
            LinearOp::Unary {
                dst: 2,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutput { src: 2 },
        ];
        assert_eq!(
            slice_to_output(&program, 1),
            Some(vec![
                LinearOp::LoadY { dst: 0, index: 2 },
                LinearOp::Unary {
                    dst: 2,
                    op: UnaryOp::Neg,
                    arg: 0
                },
                LinearOp::StoreOutput { src: 2 },
            ])
        );
    }

    #[test]
    fn a_register_written_twice_reads_its_last_write() {
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            slice_to_output(&program, 0),
            Some(vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ])
        );
    }

    #[test]
    fn a_store_reads_the_value_its_register_holds_at_the_store() {
        // r0 is stored, then written again and stored again: the first output
        // is the value before the rewrite.
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        for output in 0..2 {
            let slice = slice_to_output(&program, output).expect("the store slices");
            assert_eq!(
                run(&slice),
                vec![run(&program)[output]],
                "output {output}: {slice:?}"
            );
        }
    }

    #[test]
    fn a_row_of_an_elementwise_power_keeps_only_its_element() {
        // r[0..3] = y[0..3]; r[3..6] = r[0..3] .^ -3; store r[3..6].
        let program = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorIntegerPower {
                dst_start: 3,
                src_start: 0,
                count: 3,
                power: IntegerPower::new(-3).expect("a magnitude of at least 2"),
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 3,
                count: 3,
                stride: 1,
            },
        ];
        let sliced = slice_to_output(&program, 1).expect("the power slices");
        assert_eq!(
            sliced[1],
            LinearOp::TensorIntegerPower {
                dst_start: 4,
                src_start: 1,
                count: 1,
                power: IntegerPower::new(-3).expect("a magnitude of at least 2"),
                lanes: 1,
            }
        );
        assert_eq!(run(&sliced), vec![run(&program)[1]]);
        // The tangent lanes of a power are not modelled: the slice declines.
        let mut lanes = program;
        if let LinearOp::TensorIntegerPower { lanes: width, .. } = &mut lanes[1] {
            *width = 2;
        }
        assert!(slice_to_output(&lanes, 1).is_none());
    }

    /// r0 = y0; store r0; r1 = 0; r4 = table(r1, r1, r0); store r4. The
    /// lookup after the first store can fail, and the full program fails
    /// every row with it, so no row may slice it away.
    pub(super) fn a_store_before_a_fallible_operation() -> Vec<LinearOp> {
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::TableLookup {
                dst: 4,
                table_id: 1,
                column: 1,
                input: 0,
            },
            LinearOp::StoreOutput { src: 4 },
        ]
    }

    #[test]
    fn a_fallible_operation_after_the_store_declines() {
        let program = a_store_before_a_fallible_operation();
        assert!(slice_to_output(&program, 0).is_none());
        assert!(slice_to_output(&program, 1).is_none());
    }

    #[test]
    fn an_unmodelled_operation_or_a_missing_output_declines() {
        let mut program = vector_program();
        assert!(slice_to_output(&program, 3).is_none());
        program.insert(
            0,
            LinearOp::TensorIdentity {
                dst_start: 20,
                size: 2,
                lanes: 1,
            },
        );
        assert!(slice_to_output(&program, 0).is_none());
    }

    /// Check every slice of `program` against its full run; returns how
    /// many outputs sliced and how many declined.
    fn check_slices(program: &[LinearOp]) -> (usize, usize) {
        let full = run(program);
        let slices: Vec<_> = (0..full.len())
            .map(|output| slice_to_output(program, output))
            .collect();
        for (output, slice) in slices.iter().enumerate() {
            let Some(slice) = slice else { continue };
            assert_eq!(
                run(slice),
                vec![full[output]],
                "output {output} of {program:?}"
            );
        }
        assert!(slice_to_output(program, full.len()).is_none());
        let sliced = slices.iter().filter(|slice| slice.is_some()).count();
        (sliced, full.len() - sliced)
    }

    #[test]
    fn every_slice_of_a_random_program_stores_the_full_programs_output() {
        let mut stream = Stream(0x9e37_79b9_7f4a_7c15);
        let (mut sliced, mut declined) = (0usize, 0usize);
        for _ in 0..2000 {
            let (s, d) = check_slices(&random_program(&mut stream));
            sliced += s;
            declined += d;
        }
        // Only a live tensor operation whose sources overlap its
        // destination declines, so most slices are checked.
        assert!(
            sliced > 4 * declined,
            "sliced {sliced}, declined {declined}"
        );
    }
}
