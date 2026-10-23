//! Elementwise maps lowered as one flat loop.
//!
//! Elementwise array expressions reach the backend as a `Map` whose body reads
//! each operand at the iteration's own coordinates and computes one scalar.
//! Run as a region, every element pays for a frame: its binders are divided
//! out of the ordinal, each projection recomputes and checks a row-major
//! offset, and the result is copied out. When every projection reads a
//! capture shaped like the domain at exactly the domain's binders, the
//! element at ordinal `n` reads cell `n` of each such capture, so the body is
//! evaluated inline per ordinal instead. Only operations that cannot fault are
//! admitted, so no failure moves.

use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve as solve;

use super::{CompileError, ProgramLowerer, ValueLocation};

/// What a body register holds during one flat iteration.
#[derive(Clone)]
enum Held {
    /// A capture, read at the iteration's ordinal by an identity projection.
    Capture(ValueLocation),
    /// A binder of the domain; only an identity projection may read it.
    Binder,
    Scalar(Value),
}

/// The symbolic form of [`Held`] the admission check tracks.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Identity,
    Binder(usize),
    Scalar,
}

fn real(scalar: solve::SolveScalarType) -> bool {
    matches!(scalar, solve::SolveScalarType::Real { .. })
}

/// The flat map's fixed facts: captures by slot, domain extents, and how many
/// input slots the body has.
struct FlatShape<'a> {
    captures: &'a [ValueLocation],
    extents: &'a [usize],
    input_count: usize,
}

impl FlatShape<'_> {
    /// The kind a load of input `slot` defines, if the flat form admits it.
    fn load(&self, slot: usize) -> Option<Kind> {
        let Some(capture) = self.captures.get(slot) else {
            return (slot < self.input_count).then_some(Kind::Binder(slot - self.captures.len()));
        };
        let dimensions = capture.value_type.dimensions();
        let identity = dimensions.len() == self.extents.len()
            && dimensions
                .iter()
                .zip(self.extents)
                .all(|(extent, axis)| *extent as usize == *axis);
        if identity {
            Some(Kind::Identity)
        } else {
            (capture.value_type.scalar_count() == 1).then_some(Kind::Scalar)
        }
    }

    /// The register and kind `operation` defines in the flat form, or `None`
    /// when the flat form does not admit it. `Ok(None)` from the outer layer
    /// marks the output store, which defines nothing.
    fn admit(
        &self,
        operation: &solve::SolveOperation,
        kinds: &[Option<Kind>],
        types: &[solve::SolveValueType],
    ) -> Option<Option<(solve::SolveRegisterId, Kind)>> {
        use solve::SolveOperation as Op;
        let kind =
            |register: &solve::SolveRegisterId| kinds.get(register.index()).copied().flatten();
        let scalar = |register: &solve::SolveRegisterId| kind(register) == Some(Kind::Scalar);
        let real_type =
            |register: &solve::SolveRegisterId| real(types[register.index()].element_type());
        let defined = match operation {
            Op::Load { destination, slot } => (*destination, self.load(slot.index())?),
            Op::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => {
                let identity = kind(aggregate) == Some(Kind::Identity)
                    && indices.len() == self.extents.len()
                    && indices
                        .iter()
                        .enumerate()
                        .all(|(axis, index)| kind(index) == Some(Kind::Binder(axis)));
                identity.then_some((*destination, Kind::Scalar))?
            }
            Op::Constant { destination, .. } => (*destination, Kind::Scalar),
            Op::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => (scalar(condition) && scalar(if_true) && scalar(if_false))
                .then_some((*destination, Kind::Scalar))?,
            Op::Compare {
                destination,
                lhs,
                rhs,
                ..
            } => (scalar(lhs) && scalar(rhs)).then_some((*destination, Kind::Scalar))?,
            Op::Unary {
                destination,
                operand,
                ..
            } => ((real_type(destination) || real_type(operand)) && scalar(operand))
                .then_some((*destination, Kind::Scalar))?,
            Op::Binary {
                destination,
                lhs,
                rhs,
                ..
            } => (real_type(destination) && scalar(lhs) && scalar(rhs))
                .then_some((*destination, Kind::Scalar))?,
            Op::Convert {
                destination,
                operator: solve::SolveConversionOperator::IntegerToReal,
                operand,
            } => scalar(operand).then_some((*destination, Kind::Scalar))?,
            Op::Store { slot, source } if slot.index() == self.input_count => {
                return scalar(source).then_some(None);
            }
            _ => return None,
        };
        Some(Some(defined))
    }

    /// Whether every operation of `program` is admitted, checked before any
    /// code is emitted: the emission handles exactly what this admits.
    fn admits(&self, program: &solve::TypedProgram) -> bool {
        let types = program.register_types();
        let mut kinds = vec![None::<Kind>; types.len()];
        for operation in program.operations() {
            match self.admit(operation.operation(), &kinds, types) {
                Some(Some((register, kind))) if register.index() < kinds.len() => {
                    kinds[register.index()] = Some(kind);
                }
                Some(None) => {}
                _ => return false,
            }
        }
        true
    }
}

/// One flat iteration's held values, the captures with their admitted kinds,
/// and the body's register types.
type FlatState<'a> = (
    &'a mut HashMap<usize, Held>,
    &'a [(ValueLocation, Option<Kind>)],
    &'a [solve::SolveValueType],
);

impl ProgramLowerer<'_, '_> {
    /// Lower `body` over `domain` as one flat loop, or return false having
    /// emitted nothing when the body is not an elementwise expression.
    pub(super) fn lower_flat_map(
        &mut self,
        domain: &StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        destination: solve::SolveRegisterId,
        body: &solve::SolveProgramRegion,
    ) -> Result<bool, CompileError> {
        let destination = self.register(destination)?.clone();
        let extents = domain
            .extents()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let shaped = destination
            .value_type
            .dimensions()
            .iter()
            .map(|extent| *extent as usize)
            .eq(extents.iter().copied());
        if body.outputs().len() != 1
            || body.outputs()[0].scalar_count() != 1
            || domain
                .binders
                .iter()
                .any(|binder| binder.lower != 1 || binder.step != 1)
            || !shaped
            || !super::placement::placeable(body)
            || body.inputs().len() != captures.len() + domain.binders.len()
        {
            return Ok(false);
        }
        let capture_locations = captures
            .iter()
            .map(|capture| self.register(*capture).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let shape = FlatShape {
            captures: &capture_locations,
            extents: &extents,
            input_count: body.inputs().len(),
        };
        if !shape.admits(body.body())
            || !self.flat_storage_is_elementwise(&destination, captures)?
        {
            return Ok(false);
        }
        let program = body.body();
        let captures = capture_locations
            .iter()
            .enumerate()
            .map(|(slot, location)| (location.clone(), shape.load(slot)))
            .collect::<Vec<_>>();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let mut held = HashMap::<usize, Held>::new();
            for operation in program.operations() {
                this.flat_element(
                    operation.operation(),
                    (&mut held, &captures, program.register_types()),
                    (&destination, index),
                )?;
            }
            Ok(())
        })?;
        Ok(true)
    }

    /// The region form reads every capture before writing; the flat loop
    /// reads and writes element by element, which only a capture on exactly
    /// the destination's cells (or none of them) tolerates.
    fn flat_storage_is_elementwise(
        &self,
        destination: &ValueLocation,
        captures: &[solve::SolveRegisterId],
    ) -> Result<bool, CompileError> {
        let target = self.physical(destination);
        for capture in captures {
            let cells = self.physical(self.register(*capture)?);
            if cells.overlaps(target) && cells != target {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Emit one admitted body operation for the element at `index`.
    fn flat_element(
        &mut self,
        operation: &solve::SolveOperation,
        (held, captures, types): FlatState<'_>,
        (destination, index): (&ValueLocation, Value),
    ) -> Result<(), CompileError> {
        use solve::SolveOperation as Op;
        let (register, value) = match operation {
            Op::Load { destination, slot } => {
                let value = match captures.get(slot.index()) {
                    Some((location, Some(Kind::Scalar))) => {
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        Held::Scalar(self.load_scalar(location, zero)?)
                    }
                    Some((location, Some(Kind::Identity))) => Held::Capture(location.clone()),
                    Some(_) => return Err(unadmitted()),
                    None => Held::Binder,
                };
                (*destination, value)
            }
            Op::ProjectElementDynamic {
                destination,
                aggregate,
                ..
            } => {
                let Some(Held::Capture(location)) = held.get(&aggregate.index()).cloned() else {
                    return Err(unadmitted());
                };
                let value = self.load_scalar(&location, index)?;
                (*destination, Held::Scalar(value))
            }
            Op::Constant { destination, value } => {
                (*destination, Held::Scalar(self.scalar_constant(value)))
            }
            Op::Store { source, .. } => {
                let value = held_scalar(held, source)?;
                return self.store_scalar(destination, index, value);
            }
            operation => {
                let (register, value) = self.flat_arithmetic(operation, held, types)?;
                (register, Held::Scalar(value))
            }
        };
        held.insert(register.index(), value);
        Ok(())
    }

    /// The scalar arithmetic of one admitted body operation.
    fn flat_arithmetic(
        &mut self,
        operation: &solve::SolveOperation,
        held: &HashMap<usize, Held>,
        types: &[solve::SolveValueType],
    ) -> Result<(solve::SolveRegisterId, Value), CompileError> {
        use solve::SolveOperation as Op;
        let scalar = |register: &solve::SolveRegisterId| held_scalar(held, register);
        let element = |register: &solve::SolveRegisterId| types[register.index()].element_type();
        Ok(match operation {
            Op::Unary {
                destination,
                operator,
                operand,
            } => {
                let value = scalar(operand)?;
                let result = self.unary_element(*operator, element(operand), value)?;
                (*destination, result)
            }
            Op::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let (lhs, rhs) = (scalar(lhs)?, scalar(rhs)?);
                let result = self.binary_element(*operator, element(destination), lhs, rhs)?;
                (*destination, result)
            }
            Op::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let compared = element(lhs);
                let (lhs, rhs) = (scalar(lhs)?, scalar(rhs)?);
                let condition = self.compare_element(*operator, compared, lhs, rhs);
                (
                    *destination,
                    self.builder.ins().uextend(types::I64, condition),
                )
            }
            Op::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                let condition = scalar(condition)?;
                let (when_true, when_false) = (scalar(if_true)?, scalar(if_false)?);
                let condition = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
                (
                    *destination,
                    self.builder.ins().select(condition, when_true, when_false),
                )
            }
            Op::Convert {
                destination,
                operator,
                operand,
            } => {
                let value = scalar(operand)?;
                let target = &types[destination.index()];
                let converted = self.convert_element(*operator, element(operand), target, value)?;
                (*destination, converted)
            }
            _ => return Err(unadmitted()),
        })
    }
}

fn unadmitted() -> CompileError {
    CompileError::Backend("flat map body changed shape during lowering".into())
}

fn held_scalar(
    held: &HashMap<usize, Held>,
    register: &solve::SolveRegisterId,
) -> Result<Value, CompileError> {
    match held.get(&register.index()) {
        Some(Held::Scalar(value)) => Ok(*value),
        _ => Err(unadmitted()),
    }
}
