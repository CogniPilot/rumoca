use super::admission::{Program, Unit};
use rumoca_core::Span;
use rumoca_ir_solve::{
    BinaryOp, LinearOp, ProgramRegister, ProgramSlot, SolveArithmeticProfile, SolveBinaryOperator,
    SolveFiniteCheck, SolveProgramConstructionError, SolveTargetSelection, SolveUnaryOperator,
    SolveValue, StoredStateJacobianOutputOrder, TypedProgramBuilder, UnaryOp,
};
use std::collections::BTreeMap;

type Error = SolveProgramConstructionError;

pub(super) struct Target<'profile> {
    pub arithmetic: SolveArithmeticProfile,
    pub selection: SolveTargetSelection<'profile>,
}

#[derive(Clone)]
pub(super) struct Dual<'program> {
    pub(super) primal: ProgramRegister<'program>,
    pub(super) tangents: Vec<Option<ProgramRegister<'program>>>,
}

/// The finite check one source operation issues: its scalar identity, or the
/// identity of a compact node's operation at the body's point ordinal.
#[derive(Clone, Copy)]
pub(super) enum Guard<'program> {
    Scalar(SolveFiniteCheck),
    Mapped {
        program_base: u32,
        point_count: u32,
        operation: u32,
        point: ProgramRegister<'program>,
    },
}

impl<'program> Guard<'program> {
    pub(super) fn issue(
        self,
        builder: &mut TypedProgramBuilder<'program>,
        value: ProgramRegister<'program>,
        span: Span,
    ) -> Result<(), Error> {
        match self {
            Self::Scalar(check) => builder.check_finite(value, check, span),
            Self::Mapped {
                program_base,
                point_count,
                operation,
                point,
            } => builder.check_finite_mapped(
                value,
                program_base,
                point_count,
                operation,
                point,
                span,
            ),
        }
    }
}

pub(super) struct Construction<'program> {
    inputs: Vec<ProgramRegister<'program>>,
    matrix: Vec<Option<ProgramRegister<'program>>>,
    states: usize,
    profile: SolveArithmeticProfile,
    zero: ProgramRegister<'program>,
    one: ProgramRegister<'program>,
}

pub(super) fn construct<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    output: ProgramSlot<'program>,
    units: &[Unit<'_>],
    order: &StoredStateJacobianOutputOrder<'_>,
    target: Target<'_>,
    span: Span,
) -> Result<(), Error> {
    let Target {
        arithmetic: profile,
        selection: target,
    } = target;
    let states = order.input_labels().len();
    let cells = order.cells();
    let mut construction = Construction {
        inputs: inputs
            .iter()
            .enumerate()
            .map(|(index, slot)| {
                let value = builder.load(*slot, span)?;
                let coordinate = u32::try_from(index)
                    .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
                builder.check_finite(value, SolveFiniteCheck::Input { coordinate }, span)?;
                Ok(value)
            })
            .collect::<Result<_, Error>>()?,
        matrix: vec![None; cells],
        states,
        profile,
        zero: builder.constant(SolveValue::real(profile, 0.0), span)?,
        one: builder.constant(SolveValue::real(profile, 1.0), span)?,
    };
    // Canonical program identities: one per scalar program, one per point of
    // a compact node, in source order, exactly as the scalarized source
    // numbers them. Evaluation follows the same order.
    let mut next_program = 0u32;
    let mut compact = Vec::new();
    for unit in units {
        match unit {
            Unit::Scalar(program) => {
                construction.program(builder, program, next_program)?;
                next_program = next_program
                    .checked_add(1)
                    .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
            }
            Unit::Compact(node) => {
                let count =
                    u32::try_from(node.rows.len()).map_err(|_| Error::InvalidTensorAlgebra {
                        provenance: node.node.span,
                    })?;
                if target.admits_nested_map_frames() {
                    compact.push(super::compact::construct(
                        builder,
                        &construction,
                        node,
                        next_program,
                    )?);
                } else {
                    // The selected target cannot execute a compact map: issue
                    // the node's scalarized programs, the scalar reference's
                    // own expansion, under the same program identities.
                    construction.scalarized(builder, node, next_program)?;
                }
                next_program =
                    next_program
                        .checked_add(count)
                        .ok_or(Error::InvalidTensorAlgebra {
                            provenance: node.node.span,
                        })?;
            }
        }
    }
    // Only the final aggregate's declared placements follow the
    // source-issued output axes.
    let compact = compact
        .iter()
        .map(|(placement, value)| (placement, *value))
        .collect::<Vec<_>>();
    let jacobian = target.materialize_model_jacobian(
        builder,
        &compact,
        &construction.matrix,
        construction.zero,
        order,
        span,
    )?;
    // Issued constant cells cannot fail; guard the rest in output order.
    builder.check_finite_cells(&jacobian.cells, SolveFiniteCheck::Derivative, span)?;
    builder.store(output, jacobian.value, span)
}

impl<'program> Construction<'program> {
    /// A body construction over `width` node-local seeds; it reads no inputs.
    pub(super) fn seeded(
        width: usize,
        profile: SolveArithmeticProfile,
        zero: ProgramRegister<'program>,
        one: ProgramRegister<'program>,
    ) -> Self {
        Self {
            inputs: Vec::new(),
            matrix: Vec::new(),
            states: width,
            profile,
            zero,
            one,
        }
    }

    pub(super) fn inputs(&self) -> &[ProgramRegister<'program>] {
        &self.inputs
    }

    pub(super) const fn states(&self) -> usize {
        self.states
    }

    pub(super) const fn profile(&self) -> SolveArithmeticProfile {
        self.profile
    }

    pub(super) fn absent(&self, primal: ProgramRegister<'program>) -> Dual<'program> {
        Dual {
            primal,
            tangents: vec![None; self.states],
        }
    }

    fn input(&self, index: usize, state: bool, span: Span) -> Result<Dual<'program>, Error> {
        let primal = *self
            .inputs
            .get(index)
            .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
        if !state {
            return Ok(self.absent(primal));
        }
        // This constructor owns the solver-Y seed domain. Only the matching
        // coordinate depends on it; runtime numeric zeros never erase presence.
        let tangents = (0..self.states)
            .map(|column| (index == column).then_some(self.one))
            .collect();
        Ok(Dual { primal, tangents })
    }

    /// Issue a compact node's scalarized programs under its point identities
    /// `program_base + point`, for a target that cannot execute its map.
    fn scalarized(
        &mut self,
        builder: &mut TypedProgramBuilder<'program>,
        compact: &super::admission::Compact<'_>,
        program_base: u32,
    ) -> Result<(), Error> {
        let span = compact.node.span;
        let programs = super::admission::scalarized_programs(compact)
            .map_err(|_| Error::InvalidTensorAlgebra { provenance: span })?;
        for (point, program) in programs.iter().enumerate() {
            let index = u32::try_from(point)
                .ok()
                .and_then(|point| program_base.checked_add(point))
                .ok_or(Error::InvalidTensorAlgebra { provenance: span })?;
            self.program(builder, program, index)?;
        }
        Ok(())
    }

    fn program(
        &mut self,
        builder: &mut TypedProgramBuilder<'program>,
        program: &Program<'_>,
        program_index: u32,
    ) -> Result<(), Error> {
        let mut registers = BTreeMap::new();
        let mut ordinal = 0;
        for (index, operation) in program.ops.iter().enumerate() {
            let operation_index =
                u32::try_from(index).map_err(|_| Error::InvalidTensorAlgebra {
                    provenance: program.span,
                })?;
            let check = Guard::Scalar(SolveFiniteCheck::Primal {
                program: program_index,
                operation: operation_index,
            });
            if let LinearOp::StoreOutput { src } = operation {
                let value: &Dual<'program> =
                    registers.get(src).ok_or(Error::InvalidTensorAlgebra {
                        provenance: program.span,
                    })?;
                let row = *program
                    .outputs
                    .get(ordinal)
                    .ok_or(Error::InvalidTensorAlgebra {
                        provenance: program.span,
                    })?;
                self.matrix[row * self.states..(row + 1) * self.states]
                    .copy_from_slice(&value.tangents);
                ordinal += 1;
            } else {
                let (destination, value) =
                    self.operation(builder, operation, &registers, (program.span, check))?;
                registers.insert(destination, value);
            }
        }
        if ordinal != program.outputs.len() {
            return Err(Error::InvalidTensorAlgebra {
                provenance: program.span,
            });
        }
        Ok(())
    }

    pub(super) fn operation(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        operation: &LinearOp,
        registers: &BTreeMap<u32, Dual<'program>>,
        (span, check): (Span, Guard<'program>),
    ) -> Result<(u32, Dual<'program>), Error> {
        let read = |index: &u32| {
            registers
                .get(index)
                .cloned()
                .ok_or(Error::InvalidTensorAlgebra { provenance: span })
        };
        let result = match *operation {
            LinearOp::Const { dst, value } => (
                dst,
                self.absent(builder.constant(SolveValue::real(self.profile, value), span)?),
            ),
            LinearOp::LoadY { dst, index } if index < self.states => {
                (dst, self.input(index, true, span)?)
            }
            LinearOp::LoadP { dst, index } if index < self.inputs.len() - self.states - 1 => {
                (dst, self.input(self.states + index, false, span)?)
            }
            LinearOp::LoadTime { dst } => (dst, self.input(self.inputs.len() - 1, false, span)?),
            LinearOp::Move { dst, src } => (dst, read(&src)?),
            LinearOp::Unary { dst, op, arg } => (
                dst,
                self.unary(builder, unary(op, span)?, read(&arg)?, (span, check))?,
            ),
            LinearOp::Binary { dst, op, lhs, rhs } => (
                dst,
                self.binary(
                    builder,
                    binary(op, span)?,
                    [read(&lhs)?, read(&rhs)?],
                    (span, check),
                )?,
            ),
            _ => return Err(Error::InvalidTensorAlgebra { provenance: span }),
        };
        if !matches!(operation, LinearOp::Unary { .. } | LinearOp::Binary { .. }) {
            check.issue(builder, result.1.primal, span)?;
        }
        Ok(result)
    }

    fn unary(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        operator: SolveUnaryOperator,
        value: Dual<'program>,
        (span, check): (Span, Guard<'program>),
    ) -> Result<Dual<'program>, Error> {
        let source = builder.scalar_unary_directional_source(operator, value.primal, span)?;
        check.issue(builder, source.primal(), span)?;
        let tangents = value
            .tangents
            .iter()
            .map(|direction| builder.scalar_unary_structural_tangent(source, *direction))
            .collect::<Result<_, _>>()?;
        Ok(Dual {
            primal: source.primal(),
            tangents,
        })
    }

    fn binary(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        operator: SolveBinaryOperator,
        values: [Dual<'program>; 2],
        (span, check): (Span, Guard<'program>),
    ) -> Result<Dual<'program>, Error> {
        let source = builder.scalar_binary_directional_source(
            operator,
            [values[0].primal, values[1].primal],
            span,
        )?;
        check.issue(builder, source.primal(), span)?;
        let tangents = values[0]
            .tangents
            .iter()
            .zip(&values[1].tangents)
            .map(|(left, right)| builder.scalar_binary_structural_tangent(source, [*left, *right]))
            .collect::<Result<_, _>>()?;
        Ok(Dual {
            primal: source.primal(),
            tangents,
        })
    }
}

fn unary(operator: UnaryOp, span: Span) -> Result<SolveUnaryOperator, Error> {
    Ok(match operator {
        UnaryOp::Neg => SolveUnaryOperator::Negate,
        UnaryOp::Abs => SolveUnaryOperator::Abs,
        UnaryOp::Sqrt => SolveUnaryOperator::Sqrt,
        UnaryOp::Sin => SolveUnaryOperator::Sin,
        UnaryOp::Cos => SolveUnaryOperator::Cos,
        UnaryOp::Tan => SolveUnaryOperator::Tan,
        UnaryOp::Asin => SolveUnaryOperator::Asin,
        UnaryOp::Acos => SolveUnaryOperator::Acos,
        UnaryOp::Atan => SolveUnaryOperator::Atan,
        UnaryOp::Sinh => SolveUnaryOperator::Sinh,
        UnaryOp::Cosh => SolveUnaryOperator::Cosh,
        UnaryOp::Tanh => SolveUnaryOperator::Tanh,
        UnaryOp::Exp => SolveUnaryOperator::Exp,
        UnaryOp::Log => SolveUnaryOperator::Log,
        UnaryOp::Log10 => SolveUnaryOperator::Log10,
        _ => return Err(Error::InvalidTensorAlgebra { provenance: span }),
    })
}

fn binary(operator: BinaryOp, span: Span) -> Result<SolveBinaryOperator, Error> {
    Ok(match operator {
        BinaryOp::Add => SolveBinaryOperator::Add,
        BinaryOp::Sub => SolveBinaryOperator::Subtract,
        BinaryOp::Mul => SolveBinaryOperator::Multiply,
        BinaryOp::Div => SolveBinaryOperator::Divide,
        BinaryOp::Pow => SolveBinaryOperator::Power,
        _ => return Err(Error::InvalidTensorAlgebra { provenance: span }),
    })
}
