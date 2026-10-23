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

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve as solve;

use super::{CompileError, ProgramLowerer, ValueLocation};

/// The symbolic form of a body register in the flat form.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    /// A capture shaped like the domain, read at the iteration's ordinal.
    Identity,
    /// A binder of the domain; only an identity projection may read it.
    Binder(usize),
    Scalar,
}

/// What a body register resolves to once its operation is admitted.
#[derive(Clone, Copy, PartialEq)]
enum Resolved {
    /// The capture in this slot, shaped like the domain.
    Capture(usize),
    Binder(usize),
    /// The value the step with this index forms.
    Value(usize),
}

/// One admitted body operation with its operands resolved. A scalar operand
/// is the index of the step that formed it, so emission never classifies an
/// operation again.
enum FlatStep<'a> {
    /// A one-cell capture, loaded once per element.
    LoadScalar(&'a ValueLocation),
    /// A domain-shaped capture read at the iteration's ordinal.
    Project(&'a ValueLocation),
    Constant(&'a solve::SolveValue),
    Unary {
        operator: solve::SolveUnaryOperator,
        element: solve::SolveScalarType,
        operand: usize,
    },
    Binary {
        operator: solve::SolveBinaryOperator,
        element: solve::SolveScalarType,
        lhs: usize,
        rhs: usize,
    },
    Compare {
        operator: solve::SolveCompareOperator,
        element: solve::SolveScalarType,
        lhs: usize,
        rhs: usize,
    },
    Select {
        condition: usize,
        if_true: usize,
        if_false: usize,
    },
    Convert {
        operator: solve::SolveConversionOperator,
        source: solve::SolveScalarType,
        target: &'a solve::SolveValueType,
        operand: usize,
    },
    /// The body's output element.
    Store(usize),
}

impl FlatStep<'_> {
    /// Whether the step forms a value later steps may read.
    fn forms_value(&self) -> bool {
        !matches!(self, Self::Store(_))
    }
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

/// The plan under construction: each register's resolution and the steps so
/// far, with the number of values they form.
struct Planning<'a> {
    resolved: Vec<Option<Resolved>>,
    steps: Vec<FlatStep<'a>>,
    values: usize,
}

impl<'a> Planning<'a> {
    fn value(&self, register: &solve::SolveRegisterId) -> Option<usize> {
        match self.resolved.get(register.index()).copied().flatten() {
            Some(Resolved::Value(value)) => Some(value),
            _ => None,
        }
    }

    /// Append `step`, defining `register` as its value when it forms one.
    fn push(&mut self, register: Option<solve::SolveRegisterId>, step: FlatStep<'a>) -> Option<()> {
        if let Some(register) = register {
            let resolved = self.resolved.get_mut(register.index())?;
            *resolved = Some(Resolved::Value(self.values));
        }
        if step.forms_value() {
            self.values += 1;
        }
        self.steps.push(step);
        Some(())
    }

    fn define(&mut self, register: solve::SolveRegisterId, resolved: Resolved) -> Option<()> {
        *self.resolved.get_mut(register.index())? = Some(resolved);
        Some(())
    }
}

impl<'a> FlatShape<'a> {
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

    /// The steps of `program` in the flat form, or `None` when the flat form
    /// does not admit one of its operations. This is the only place an
    /// operation is classified: emission follows the plan exactly.
    fn plan<'p: 'a>(&self, program: &'p solve::TypedProgram) -> Option<Vec<FlatStep<'a>>> {
        let types = program.register_types();
        let mut planning = Planning {
            resolved: vec![None; types.len()],
            steps: Vec::new(),
            values: 0,
        };
        for operation in program.operations() {
            self.admit(operation.operation(), types, &mut planning)?;
        }
        Some(planning.steps)
    }

    /// Plan one operation, or `None` when the flat form does not admit it.
    fn admit<'p: 'a>(
        &self,
        operation: &'p solve::SolveOperation,
        types: &'p [solve::SolveValueType],
        planning: &mut Planning<'a>,
    ) -> Option<()> {
        use solve::SolveOperation as Op;
        let captures: &'a [ValueLocation] = self.captures;
        let element = |register: &solve::SolveRegisterId| types[register.index()].element_type();
        match operation {
            Op::Load { destination, slot } => match self.load(slot.index())? {
                Kind::Identity => planning.define(*destination, Resolved::Capture(slot.index())),
                Kind::Binder(axis) => planning.define(*destination, Resolved::Binder(axis)),
                Kind::Scalar => planning.push(
                    Some(*destination),
                    FlatStep::LoadScalar(&captures[slot.index()]),
                ),
            },
            Op::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => {
                let step = self.project(aggregate, indices, planning)?;
                planning.push(Some(*destination), step)
            }
            Op::Constant { destination, value } => {
                planning.push(Some(*destination), FlatStep::Constant(value))
            }
            Op::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                let step = FlatStep::Select {
                    condition: planning.value(condition)?,
                    if_true: planning.value(if_true)?,
                    if_false: planning.value(if_false)?,
                };
                planning.push(Some(*destination), step)
            }
            Op::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let step = FlatStep::Compare {
                    operator: *operator,
                    element: element(lhs),
                    lhs: planning.value(lhs)?,
                    rhs: planning.value(rhs)?,
                };
                planning.push(Some(*destination), step)
            }
            Op::Unary {
                destination,
                operator,
                operand,
            } => {
                (real(element(destination)) || real(element(operand))).then_some(())?;
                let step = FlatStep::Unary {
                    operator: *operator,
                    element: element(operand),
                    operand: planning.value(operand)?,
                };
                planning.push(Some(*destination), step)
            }
            Op::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                real(element(destination)).then_some(())?;
                let step = FlatStep::Binary {
                    operator: *operator,
                    element: element(destination),
                    lhs: planning.value(lhs)?,
                    rhs: planning.value(rhs)?,
                };
                planning.push(Some(*destination), step)
            }
            Op::Convert {
                destination,
                operator: operator @ solve::SolveConversionOperator::IntegerToReal,
                operand,
            } => {
                let step = FlatStep::Convert {
                    operator: *operator,
                    source: element(operand),
                    target: &types[destination.index()],
                    operand: planning.value(operand)?,
                };
                planning.push(Some(*destination), step)
            }
            Op::Store { slot, source } if slot.index() == self.input_count => {
                let step = FlatStep::Store(planning.value(source)?);
                planning.push(None, step)
            }
            _ => None,
        }
    }

    /// A read of a domain-shaped capture at the domain's binders in order,
    /// the only projection the flat form admits.
    fn project(
        &self,
        aggregate: &solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
        planning: &Planning<'a>,
    ) -> Option<FlatStep<'a>> {
        let resolved = |register: &solve::SolveRegisterId| {
            planning.resolved.get(register.index()).copied().flatten()
        };
        let Some(Resolved::Capture(slot)) = resolved(aggregate) else {
            return None;
        };
        let identity = indices.len() == self.extents.len()
            && indices
                .iter()
                .enumerate()
                .all(|(axis, index)| resolved(index) == Some(Resolved::Binder(axis)));
        let captures: &'a [ValueLocation] = self.captures;
        identity.then_some(FlatStep::Project(&captures[slot]))
    }
}

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
        let Some(plan) = shape.plan(body.body()) else {
            return Ok(false);
        };
        if !self.flat_storage_is_elementwise(&destination, captures)? {
            return Ok(false);
        }
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let mut values = Vec::with_capacity(plan.len());
            for step in &plan {
                let value = this.flat_step(step, &values, (&destination, index))?;
                values.extend(value);
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

    /// Emit one planned step for the element at `index`, returning the value
    /// it forms.
    fn flat_step(
        &mut self,
        step: &FlatStep<'_>,
        values: &[Value],
        (destination, index): (&ValueLocation, Value),
    ) -> Result<Option<Value>, CompileError> {
        Ok(Some(match *step {
            FlatStep::LoadScalar(location) => {
                let zero = self.builder.ins().iconst(types::I64, 0);
                self.load_scalar(location, zero)?
            }
            FlatStep::Project(location) => self.load_scalar(location, index)?,
            FlatStep::Constant(value) => self.scalar_constant(value),
            FlatStep::Unary {
                operator,
                element,
                operand,
            } => self.unary_element(operator, element, values[operand])?,
            FlatStep::Binary {
                operator,
                element,
                lhs,
                rhs,
            } => self.binary_element(operator, element, values[lhs], values[rhs])?,
            FlatStep::Compare {
                operator,
                element,
                lhs,
                rhs,
            } => {
                let condition = self.compare_element(operator, element, values[lhs], values[rhs]);
                self.builder.ins().uextend(types::I64, condition)
            }
            FlatStep::Select {
                condition,
                if_true,
                if_false,
            } => {
                let condition = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, values[condition], 0);
                self.builder
                    .ins()
                    .select(condition, values[if_true], values[if_false])
            }
            FlatStep::Convert {
                operator,
                source,
                target,
                operand,
            } => self.convert_element(operator, source, target, values[operand])?,
            FlatStep::Store(source) => {
                self.store_scalar(destination, index, values[source])?;
                return Ok(None);
            }
        }))
    }
}
