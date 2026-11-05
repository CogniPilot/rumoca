use super::admission::Program;
use rumoca_core::Span;
use rumoca_ir_solve::{
    BinaryOp, LinearOp, ProgramRegister, ProgramSlot, SolveArithmeticProfile, SolveBinaryOperator,
    SolveFiniteCheck, SolveProgramConstructionError, SolveUnaryOperator, SolveValue,
    TypedProgramBuilder, UnaryOp,
};
use std::collections::BTreeMap;

type Error = SolveProgramConstructionError;

#[derive(Clone)]
struct Dual<'program> {
    primal: ProgramRegister<'program>,
    tangents: Vec<Option<ProgramRegister<'program>>>,
}

struct Construction<'program> {
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
    programs: &[Program<'_>],
    shape: (usize, usize),
    profile: SolveArithmeticProfile,
    span: Span,
) -> Result<(), Error> {
    let (states, cells) = shape;
    let mut construction = Construction {
        inputs: inputs
            .iter()
            .enumerate()
            .map(|(index, slot)| {
                let value = builder.load(*slot, span)?;
                let coordinate = u32::try_from(index).map_err(|_| invalid(span))?;
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
    for (index, program) in programs.iter().enumerate() {
        let program_index = u32::try_from(index).map_err(|_| invalid(span))?;
        construction.program(builder, program, program_index)?;
    }
    let values = construction
        .matrix
        .into_iter()
        .map(|value| value.unwrap_or(construction.zero))
        .collect::<Vec<_>>();
    let width = u32::try_from(states).map_err(|_| invalid(span))?;
    let value = builder.construct_aggregate(&values, vec![width, width], span)?;
    builder.check_finite(value, SolveFiniteCheck::Derivative, span)?;
    builder.store(output, value, span)
}

fn invalid(provenance: Span) -> Error {
    Error::InvalidTensorAlgebra { provenance }
}

impl<'program> Construction<'program> {
    fn absent(&self, primal: ProgramRegister<'program>) -> Dual<'program> {
        Dual {
            primal,
            tangents: vec![None; self.states],
        }
    }

    fn input(&self, index: usize, state: bool, span: Span) -> Result<Dual<'program>, Error> {
        let primal = *self.inputs.get(index).ok_or_else(|| invalid(span))?;
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

    fn program(
        &mut self,
        builder: &mut TypedProgramBuilder<'program>,
        program: &Program<'_>,
        program_index: u32,
    ) -> Result<(), Error> {
        let mut registers = BTreeMap::new();
        let mut ordinal = 0;
        for (index, operation) in program.ops.iter().enumerate() {
            let operation_index = u32::try_from(index).map_err(|_| invalid(program.span))?;
            let check = SolveFiniteCheck::Primal {
                program: program_index,
                operation: operation_index,
            };
            if let LinearOp::StoreOutput { src } = operation {
                let value: &Dual<'program> =
                    registers.get(src).ok_or_else(|| invalid(program.span))?;
                let row = *program
                    .outputs
                    .get(ordinal)
                    .ok_or_else(|| invalid(program.span))?;
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
            return Err(invalid(program.span));
        }
        Ok(())
    }

    fn operation(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        operation: &LinearOp,
        registers: &BTreeMap<u32, Dual<'program>>,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<(u32, Dual<'program>), Error> {
        let read = |index: &u32| registers.get(index).cloned().ok_or_else(|| invalid(span));
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
            _ => return Err(invalid(span)),
        };
        if !matches!(operation, LinearOp::Unary { .. } | LinearOp::Binary { .. }) {
            builder.check_finite(result.1.primal, check, span)?;
        }
        Ok(result)
    }

    fn unary(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        operator: SolveUnaryOperator,
        value: Dual<'program>,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<Dual<'program>, Error> {
        let source = builder.scalar_unary_directional_source(operator, value.primal, span)?;
        builder.check_finite(source.primal(), check, span)?;
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
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<Dual<'program>, Error> {
        let source = builder.scalar_binary_directional_source(
            operator,
            [values[0].primal, values[1].primal],
            span,
        )?;
        builder.check_finite(source.primal(), check, span)?;
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
        _ => return Err(invalid(span)),
    })
}

fn binary(operator: BinaryOp, span: Span) -> Result<SolveBinaryOperator, Error> {
    Ok(match operator {
        BinaryOp::Add => SolveBinaryOperator::Add,
        BinaryOp::Sub => SolveBinaryOperator::Subtract,
        BinaryOp::Mul => SolveBinaryOperator::Multiply,
        BinaryOp::Div => SolveBinaryOperator::Divide,
        BinaryOp::Pow => SolveBinaryOperator::Power,
        _ => return Err(invalid(span)),
    })
}
