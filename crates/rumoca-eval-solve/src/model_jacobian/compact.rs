//! One map per admitted compact derivative node. The body computes one
//! point's node-local Dual, with one tangent per distinct state seed, and
//! issues each source operation's finite check under the mapped identity of
//! that point: program `base + point`, the operation's own position. Points
//! run in row-major order, so the first failing check is the scalarized
//! source's first. The node's placement stays compact until the selected
//! target materializes it.

use super::admission::{Compact, CompactNode};
use super::scalar::{Construction, Dual, Guard};
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    CompactJacobianPlacement, LinearOp, ProgramRegister, ProgramSlot, SolveArithmeticProfile,
    SolveBinaryOperator, SolveProgramConstructionError, SolveScalarType, SolveValue,
    SolveValueType, TypedProgramBuilder,
};
use std::collections::BTreeMap;

type Error = SolveProgramConstructionError;

pub(super) fn construct<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    outer: &Construction<'program>,
    compact: &Compact<'_>,
    program_base: u32,
) -> Result<(CompactJacobianPlacement, ProgramRegister<'program>), Error> {
    let node = &compact.node;
    let span = node.span;
    let profile = outer.profile();
    let extents = node
        .extents
        .iter()
        .map(|extent| {
            u32::try_from(*extent).map_err(|_| Error::InvalidTensorAlgebra { provenance: span })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let placement = CompactJacobianPlacement::new(
        extents.clone(),
        affine(node.output.base, &node.output.strides, span)?,
        node.seeds
            .iter()
            .map(|seed| affine(seed.base, &seed.strides, span))
            .collect::<Result<_, _>>()?,
        outer.states(),
    )
    .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
    let point = Point {
        node,
        extents: &extents,
        program_base,
        point_count: u32::try_from(compact.rows.len())
            .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?,
        states: outer.states(),
        inputs: outer.inputs().len(),
        profile,
    };
    let inputs = outer.inputs();
    let input_count = u32::try_from(inputs.len())
        .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
    let vector = builder.construct_aggregate(inputs, vec![input_count], span)?;
    let real = SolveScalarType::real(profile);
    // A node that reads no state still runs for its primal checks; each
    // point then stores one zero, and its placement names no cell.
    let width = node.seeds.len();
    let body_type = if width == 0 {
        SolveValueType::scalar(real)
    } else {
        SolveValueType::tensor(
            real,
            vec![
                u32::try_from(width)
                    .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?,
            ],
        )
        .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?
    };
    let value = builder.map(
        ordinal_domain(compact.domain, &extents, span)?,
        &[vector],
        body_type,
        span,
        |body, captures, binders, output| point.issue(body, captures[0], binders, output),
    )?;
    Ok((placement, value))
}

/// The facts one map body needs to issue a point of its node.
struct Point<'a, 'source> {
    node: &'a CompactNode<'source>,
    extents: &'a [u32],
    program_base: u32,
    point_count: u32,
    states: usize,
    inputs: usize,
    profile: SolveArithmeticProfile,
}

/// Registers shared by every operation of one point's body.
struct PointRegisters<'region> {
    vector: ProgramRegister<'region>,
    ordinals: Vec<ProgramRegister<'region>>,
    zero: ProgramRegister<'region>,
    one: ProgramRegister<'region>,
}

impl Point<'_, '_> {
    fn width(&self) -> usize {
        self.node.seeds.len()
    }

    /// Issue one point: its node-local Dual over every source operation, each
    /// guarded by the mapped identity `program_base + point`, then its partials.
    fn issue<'region>(
        &self,
        body: &mut TypedProgramBuilder<'region>,
        vector: ProgramSlot<'region>,
        binders: &[ProgramSlot<'region>],
        output: ProgramSlot<'region>,
    ) -> Result<(), Error> {
        let span = self.node.span;
        let vector = body.load(vector, span)?;
        let (ordinals, point) = self.ordinals(body, binders)?;
        let shared = PointRegisters {
            vector,
            ordinals,
            zero: body.constant(SolveValue::real(self.profile, 0.0), span)?,
            one: body.constant(SolveValue::real(self.profile, 1.0), span)?,
        };
        let local = Construction::seeded(self.width(), self.profile, shared.zero, shared.one);
        let mut registers: BTreeMap<u32, Dual<'_>> = BTreeMap::new();
        let mut stored = false;
        for (position, operation) in self.node.base_ops.iter().enumerate() {
            let guard = Guard::Mapped {
                program_base: self.program_base,
                point_count: self.point_count,
                operation: u32::try_from(position)
                    .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?,
                point,
            };
            if let LinearOp::StoreOutput { src } = *operation {
                let value = registers
                    .get(&src)
                    .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
                self.store(body, value, &shared, output)?;
                stored = true;
                continue;
            }
            let (destination, value) = match *operation {
                LinearOp::LoadY { dst, .. } => (dst, self.load_state(body, position, &shared)?),
                LinearOp::LoadP { dst, index } if index < self.inputs - self.states - 1 => {
                    let primal = projected(body, shared.vector, self.states + index, span)?;
                    (dst, local.absent(primal))
                }
                LinearOp::LoadTime { dst } => {
                    let primal = projected(body, shared.vector, self.inputs - 1, span)?;
                    (dst, local.absent(primal))
                }
                LinearOp::LoadP { .. } => {
                    return Err(Error::InvalidTensorAlgebra { provenance: span });
                }
                _ => {
                    let (destination, value) =
                        local.operation(body, operation, &registers, (span, guard))?;
                    registers.insert(destination, value);
                    continue;
                }
            };
            // Loads issue the same primal check the scalar path issues.
            guard.issue(body, value.primal, span)?;
            registers.insert(destination, value);
        }
        if stored {
            Ok(())
        } else {
            Err(Error::InvalidTensorAlgebra { provenance: span })
        }
    }

    /// 0-based ordinals and the row-major point, by Horner's rule, so every
    /// intermediate interval stays inside `0..point_count`.
    fn ordinals<'region>(
        &self,
        body: &mut TypedProgramBuilder<'region>,
        binders: &[ProgramSlot<'region>],
    ) -> Result<(Vec<ProgramRegister<'region>>, ProgramRegister<'region>), Error> {
        let span = self.node.span;
        let one = integer(body, self.profile, span, 1)?;
        let mut ordinals = Vec::with_capacity(binders.len());
        for binder in binders {
            let k = body.load(*binder, span)?;
            ordinals.push(body.binary(SolveBinaryOperator::Subtract, k, one, span)?);
        }
        let mut point = *ordinals
            .first()
            .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
        for (ordinal, extent) in ordinals.iter().zip(self.extents).skip(1) {
            let extent = integer(body, self.profile, span, i64::from(*extent))?;
            point = body.binary(SolveBinaryOperator::Multiply, point, extent, span)?;
            point = body.binary(SolveBinaryOperator::Add, point, *ordinal, span)?;
        }
        Ok((ordinals, point))
    }

    /// The state the `LoadY` at `position` reads: one-based element
    /// `base + 1 + Σ stride·k`, proven inside the state vector at admission,
    /// seeded on its node-local column.
    fn load_state<'region>(
        &self,
        body: &mut TypedProgramBuilder<'region>,
        position: usize,
        shared: &PointRegisters<'region>,
    ) -> Result<Dual<'region>, Error> {
        let span = self.node.span;
        let seed = self
            .node
            .load_seeds
            .iter()
            .find(|(at, _)| *at == position)
            .map(|(_, seed)| *seed)
            .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
        let key = &self.node.seeds[seed];
        let (base, strides) = affine(key.base, &key.strides, span)?;
        let mut element = integer(body, self.profile, span, base + 1)?;
        for (stride, ordinal) in strides.iter().zip(&shared.ordinals) {
            if *stride == 0 {
                continue;
            }
            let stride = integer(body, self.profile, span, *stride)?;
            let term = body.binary(SolveBinaryOperator::Multiply, stride, *ordinal, span)?;
            element = body.binary(SolveBinaryOperator::Add, element, term, span)?;
        }
        let primal = body.project_element_dynamic(shared.vector, &[element], span)?;
        let tangents = (0..self.width())
            .map(|j| (j == seed).then_some(shared.one))
            .collect();
        Ok(Dual { primal, tangents })
    }

    /// Store the point's partials, absent ones as zero; a node with no
    /// partials stores one zero.
    fn store<'region>(
        &self,
        body: &mut TypedProgramBuilder<'region>,
        value: &Dual<'region>,
        shared: &PointRegisters<'region>,
        output: ProgramSlot<'region>,
    ) -> Result<(), Error> {
        let span = self.node.span;
        if self.width() == 0 {
            return body.store(output, shared.zero, span);
        }
        let partials = value
            .tangents
            .iter()
            .map(|tangent| tangent.unwrap_or(shared.zero))
            .collect::<Vec<_>>();
        let width = u32::try_from(self.width())
            .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
        let partials = body.construct_aggregate(&partials, vec![width], span)?;
        body.store(output, partials, span)
    }
}

/// An admitted affine key narrowed to the placement's `i64` facts.
fn affine(base: i128, strides: &[i128], span: rumoca_core::Span) -> Result<(i64, Vec<i64>), Error> {
    let narrow = |value: i128| {
        i64::try_from(value).map_err(|_| Error::InvalidTensorAlgebra { provenance: span })
    };
    Ok((
        narrow(base)?,
        strides
            .iter()
            .map(|stride| narrow(*stride))
            .collect::<Result<_, _>>()?,
    ))
}

fn integer<'region>(
    body: &mut TypedProgramBuilder<'region>,
    profile: SolveArithmeticProfile,
    span: rumoca_core::Span,
    value: i64,
) -> Result<ProgramRegister<'region>, Error> {
    let value = SolveValue::integer(profile, value)
        .map_err(|_| Error::ProfileMismatch { provenance: span })?;
    body.constant(value, span)
}

/// The scalar input at `element` of the captured input vector.
fn projected<'region>(
    body: &mut TypedProgramBuilder<'region>,
    vector: ProgramRegister<'region>,
    element: usize,
    span: rumoca_core::Span,
) -> Result<ProgramRegister<'region>, Error> {
    let element =
        u32::try_from(element).map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
    body.project_element(vector, vec![element], span)
}

/// The admitted node domain as ordinals: each source binder keeps its id,
/// name and provenance and runs `1..=extent` with step one, so `k - 1` is
/// the 0-based ordinal every admitted index fact is stated over.
fn ordinal_domain(
    source: &StructuredIndexDomain,
    extents: &[u32],
    span: rumoca_core::Span,
) -> Result<StructuredIndexDomain, Error> {
    // The admitted extents are the source domain's own checked counts.
    let source_extents = source
        .extents()
        .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
    if source_extents.len() != extents.len()
        || source_extents
            .iter()
            .zip(extents)
            .any(|(source, admitted)| u32::try_from(*source).ok() != Some(*admitted))
    {
        return Err(Error::InvalidTensorAlgebra { provenance: span });
    }
    let domain = StructuredIndexDomain {
        binders: source
            .binders
            .iter()
            .zip(extents)
            .map(|(binder, extent)| StructuredIndexBinder {
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
                ..binder.clone()
            })
            .collect(),
    };
    domain
        .validate()
        .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
    Ok(domain)
}
