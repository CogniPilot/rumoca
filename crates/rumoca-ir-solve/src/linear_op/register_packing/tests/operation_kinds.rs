//! Packing of the operation kinds the random programs do not draw: pure
//! calls and their directional form, matrix and dot products, indexed
//! register loads, tensor updates and concatenations, and the tangent lanes
//! of an elementwise integer power.
//!
//! Each program writes every register its operation reads, runs the
//! operation into a distant destination, stores that destination, and ends
//! with short-lived scalars, so packing narrows it. Both forms then run
//! through a register-flow fingerprint whose reads are listed here field by
//! field, apart from the operand visitor the pass relocates with: a register
//! field the pass misses or a range it splits makes the operation read
//! another value, and the stored fingerprints differ.

use super::*;
use crate::{
    IntegerPower, SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallSite, SolvePureCallTable, SolveRealFormat, SolveScalarType,
    SolveValueType, TensorConcatenateSource, TensorIndex, TensorUpdateSubscript,
};
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("PackedKinds.mo"), 0, 1)
}

/// A pure call of two vectors of three reals returning the first: a body the
/// table differentiates, so its site carries a directional form.
fn site() -> SolvePureCallSite {
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let vector = SolveValueType::tensor(SolveScalarType::real(profile), vec![3]).unwrap();
    let table = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![vector.clone(), vector.clone()],
            vec![SolvePureCallOutput::result(vector)],
            span(),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span())?;
                builder.store(outputs[0], value, span())
            },
        )?;
        Ok(())
    })
    .expect("a passthrough owner constructs");
    table.owners()[0].call_site()
}

fn range(start: Reg, count: usize, stride: usize) -> impl Iterator<Item = Reg> {
    (0..count).map(move |k| start + (k * stride) as Reg)
}

fn product(dimensions: &[u32]) -> usize {
    dimensions.iter().map(|&extent| extent as usize).product()
}

/// The registers `op` reads, in a fixed order.
fn reads(op: &LinearOp) -> Vec<Reg> {
    match op {
        LinearOp::Const { .. } => Vec::new(),
        LinearOp::StoreOutput { src } => vec![*src],
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => range(*start, *count, *stride).collect(),
        LinearOp::PureCall {
            input_starts, site, ..
        } => call_reads(input_starts, site.inputs()),
        LinearOp::PureCallDirectional {
            input_starts, site, ..
        } => call_reads(input_starts, site.inputs()),
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
            ..
        } => range(*lhs_start, rows * inner * lanes, 1)
            .chain(range(*rhs_start, inner * columns * lanes, 1))
            .collect(),
        LinearOp::DotProduct {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => range(*lhs_start, *count, *lhs_stride)
            .chain(range(*rhs_start, *count, *rhs_stride))
            .collect(),
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            dimensions,
            indices,
            ..
        } => range(*base, product(dimensions), *stride)
            .chain(indices.iter().filter_map(runtime))
            .collect(),
        LinearOp::TensorUpdate { .. } => update_reads(op),
        LinearOp::TensorIntegerPower {
            src_start,
            count,
            lanes,
            ..
        } => range(*src_start, count * lanes, 1).collect(),
        LinearOp::TensorConcatenate { sources, lanes, .. } => sources
            .iter()
            .flat_map(|source| range(source.start, product(&source.dimensions) * lanes, 1))
            .collect(),
        other => unreachable!("no program here holds {other:?}"),
    }
}

fn call_reads(starts: &[Reg], inputs: &[SolveValueType]) -> Vec<Reg> {
    starts
        .iter()
        .zip(inputs)
        .flat_map(|(&start, input)| range(start, input.scalar_count() as usize, 1))
        .collect()
}

fn runtime(index: &TensorIndex) -> Option<Reg> {
    match *index {
        TensorIndex::Runtime(register) => Some(register),
        TensorIndex::Constant(_) => None,
    }
}

fn update_reads(op: &LinearOp) -> Vec<Reg> {
    let LinearOp::TensorUpdate {
        base_start,
        value_start,
        dimensions,
        subscripts,
        lanes,
        ..
    } = op
    else {
        unreachable!("a tensor update")
    };
    let mut registers: Vec<Reg> = range(*base_start, product(dimensions) * lanes, 1).collect();
    let mut values = *lanes;
    for (&extent, subscript) in dimensions.iter().zip(subscripts) {
        match subscript {
            TensorUpdateSubscript::Whole => values *= extent as usize,
            TensorUpdateSubscript::Index(index) => registers.extend(runtime(index)),
            TensorUpdateSubscript::Slice { start, dimensions } => {
                values *= product(dimensions);
                registers.extend(range(*start, product(dimensions), 1));
            }
        }
    }
    registers.extend(range(*value_start, values, 1));
    registers
}

/// The first register `op` writes; `dst_register_count` gives how many.
fn destination(op: &LinearOp) -> Option<Reg> {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::DotProduct { dst, .. }
        | LinearOp::LoadIndexedRegister { dst, .. } => Some(dst),
        LinearOp::PureCall { dst_start, .. }
        | LinearOp::PureCallDirectional { dst_start, .. }
        | LinearOp::MatrixMultiply { dst_start, .. }
        | LinearOp::TensorUpdate { dst_start, .. }
        | LinearOp::TensorIntegerPower { dst_start, .. }
        | LinearOp::TensorConcatenate { dst_start, .. } => Some(dst_start),
        _ => None,
    }
}

fn mix(hash: u64, value: u64) -> u64 {
    (hash ^ value).wrapping_mul(0x0100_0000_01b3)
}

/// The stored fingerprints of `program`: every written register holds a
/// hash of the operation's position, the slot written, and every value it
/// read, in order.
fn fingerprint(program: &[LinearOp]) -> Vec<u64> {
    let mut registers: Vec<u64> = (0..4096).map(|k| 0x9e37_79b9_7f4a_7c15 ^ k).collect();
    let mut stored = Vec::new();
    for (position, op) in program.iter().enumerate() {
        let read = reads(op)
            .into_iter()
            .fold(position as u64, |hash, register| {
                mix(hash, registers[register as usize])
            });
        match (op, destination(op)) {
            (LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }, _) => {
                stored.extend(reads(op).into_iter().map(|r| registers[r as usize]));
            }
            (_, Some(start)) => {
                for slot in 0..op.dst_register_count() {
                    registers[start as usize + slot] = mix(read, slot as u64);
                }
            }
            (other, None) => unreachable!("no destination for {other:?}"),
        }
    }
    stored
}

/// Write every register `op` reads, run it into its destination, store the
/// destination, and end with two short-lived scalars.
fn program_around(op: LinearOp) -> Vec<LinearOp> {
    let mut program: Vec<LinearOp> = reads(&op)
        .into_iter()
        .enumerate()
        .map(|(k, dst)| LinearOp::Const {
            dst,
            value: k as f64,
        })
        .collect();
    let start = destination(&op).expect("the operation writes");
    let count = op.dst_register_count();
    program.push(op);
    program.push(LinearOp::StoreOutputRange {
        start,
        count,
        stride: 1,
    });
    for dst in [900, 950] {
        program.push(LinearOp::Const { dst, value: 7.0 });
        program.push(LinearOp::StoreOutput { src: dst });
    }
    program
}

fn assert_packs_the_same(op: LinearOp) {
    let program = program_around(op);
    let packed = pack_registers(&program).expect("the program narrows");
    let packed_op = &packed[packed.len() - 6];
    assert_ne!(
        packed_op,
        &program[program.len() - 6],
        "the operation is renumbered"
    );
    assert_eq!(
        fingerprint(&packed),
        fingerprint(&program),
        "{program:?}\n{packed:?}"
    );
}

#[test]
fn a_pure_call_and_its_directional_form_are_renumbered_whole() {
    let site = site();
    let directional = site
        .directional()
        .expect("the passthrough body has a directional form")
        .clone();
    assert_packs_the_same(LinearOp::PureCall {
        dst_start: 600,
        input_starts: vec![100, 200].into_boxed_slice(),
        site,
    });
    let starts = (0..directional.inputs().len())
        .map(|k| 100 + 50 * k as Reg)
        .collect::<Vec<_>>();
    assert_packs_the_same(LinearOp::PureCallDirectional {
        dst_start: 600,
        input_starts: starts.into_boxed_slice(),
        site: directional,
    });
}

#[test]
fn matrix_and_dot_products_are_renumbered_whole() {
    for lanes in [1, 2] {
        assert_packs_the_same(LinearOp::MatrixMultiply {
            dst_start: 600,
            lhs_start: 100,
            rhs_start: 300,
            rows: 2,
            inner: 3,
            columns: 2,
            lanes,
        });
    }
    assert_packs_the_same(LinearOp::DotProduct {
        dst: 600,
        lhs_start: 100,
        rhs_start: 300,
        count: 3,
        lhs_stride: 2,
        rhs_stride: 1,
    });
}

#[test]
fn an_indexed_register_load_is_renumbered_with_its_runtime_indices() {
    assert_packs_the_same(LinearOp::LoadIndexedRegister {
        dst: 600,
        base: 100,
        stride: 2,
        dimensions: vec![2, 3].into_boxed_slice(),
        indices: vec![TensorIndex::Runtime(400), TensorIndex::Constant(1)].into_boxed_slice(),
    });
}

#[test]
fn a_tensor_update_and_a_concatenation_are_renumbered_whole() {
    assert_packs_the_same(LinearOp::TensorUpdate {
        dst_start: 600,
        base_start: 100,
        value_start: 300,
        dimensions: vec![3, 4].into_boxed_slice(),
        subscripts: vec![
            TensorUpdateSubscript::Index(TensorIndex::Runtime(400)),
            TensorUpdateSubscript::Slice {
                start: 450,
                dimensions: vec![2].into_boxed_slice(),
            },
        ]
        .into_boxed_slice(),
        lanes: 1,
    });
    assert_packs_the_same(LinearOp::TensorUpdate {
        dst_start: 600,
        base_start: 100,
        value_start: 300,
        dimensions: vec![2, 3].into_boxed_slice(),
        subscripts: vec![
            TensorUpdateSubscript::Index(TensorIndex::Constant(1)),
            TensorUpdateSubscript::Whole,
        ]
        .into_boxed_slice(),
        lanes: 2,
    });
    assert_packs_the_same(LinearOp::TensorConcatenate {
        dst_start: 600,
        sources: vec![
            TensorConcatenateSource {
                start: 100,
                dimensions: vec![2, 2].into_boxed_slice(),
            },
            TensorConcatenateSource {
                start: 300,
                dimensions: vec![1, 2].into_boxed_slice(),
            },
        ]
        .into_boxed_slice(),
        dimensions: vec![3, 2].into_boxed_slice(),
        axis: 0,
        lanes: 1,
    });
}

#[test]
fn an_integer_power_is_renumbered_with_all_its_lanes() {
    for (exponent, lanes) in [(3, 1), (-2, 2), (5, 3)] {
        assert_packs_the_same(LinearOp::TensorIntegerPower {
            dst_start: 600,
            src_start: 100,
            count: 4,
            power: IntegerPower::new(exponent).expect("a magnitude of at least 2"),
            lanes,
        });
    }
}
