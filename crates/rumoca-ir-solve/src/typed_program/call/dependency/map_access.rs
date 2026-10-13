//! Element-exact dependencies of a pointwise map (SPEC_0040 SOLVE-C70).
//!
//! A map body that reads its captured aggregates only at Integer coordinates
//! affine in the map binders (and in the binders of maps nested inside it,
//! which a reduction then folds) depends on exactly those elements. Each read
//! composes the captured aggregate's own dependency with that affine access,
//! so an image kernel's output pixel depends on its window, not on the whole
//! image. A body with any other use of a captured aggregate, or a coordinate
//! that is not affine, keeps the conservative whole-input rule.

use rumoca_core::AffineForm;

use super::*;
use crate::{
    SolveBinaryOperator, SolveConversionOperator, SolveProgramRegion, SolveReductionOperator,
};

/// The symbolic value of one region register over the coordinate space of
/// the enclosing maps: `rank` output axes followed by `free` axes of nested
/// maps a reduction folded.
#[derive(Clone)]
enum Symbol {
    /// No input dependency and no known Integer value.
    Independent,
    /// An Integer value affine in the space's axes.
    Integer(AffineForm),
    /// A captured aggregate, not yet read at a coordinate: the dependencies of
    /// its elements in the aggregate's own coordinates.
    Aggregate(Vec<SolveCallDependency>),
    /// A scalar whose dependencies are expressed over the space's axes.
    Scalar(Vec<SolveCallDependency>),
    /// A nested map's result: its elements' dependencies, whose free axes
    /// after the enclosing space's are the nested binders.
    Mapped(Vec<SolveCallDependency>),
}

/// Integer registers of an owner body whose values its construction fixes:
/// literals and their sums, differences, products, and negations. A map that
/// captures one reads it as a known coordinate offset.
pub(super) struct IntegerValues(Vec<Option<i64>>);

impl IntegerValues {
    pub(super) fn new(registers: usize) -> Self {
        Self(vec![None; registers])
    }

    pub(super) fn value(&self, register: SolveRegisterId) -> Option<i64> {
        self.0.get(register.index()).copied().flatten()
    }

    pub(super) fn track(&mut self, operation: &SolveOperation) {
        let (destination, value) = match operation {
            SolveOperation::Constant { destination, value } => match value.kind() {
                crate::SolveValueKind::Integer(value) => (destination, Some(value)),
                _ => return,
            },
            SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let value =
                    self.value(*lhs)
                        .zip(self.value(*rhs))
                        .and_then(|(l, r)| match operator {
                            SolveBinaryOperator::Add => l.checked_add(r),
                            SolveBinaryOperator::Subtract => l.checked_sub(r),
                            SolveBinaryOperator::Multiply => l.checked_mul(r),
                            _ => None,
                        });
                (destination, value)
            }
            SolveOperation::Unary {
                destination,
                operator: crate::SolveUnaryOperator::Negate,
                operand,
            } => (destination, self.value(*operand).and_then(i64::checked_neg)),
            _ => return,
        };
        if let Some(slot) = self.0.get_mut(destination.index()) {
            *slot = value;
        }
    }
}

/// The coordinate space a region is analyzed in.
#[derive(Clone)]
struct Space {
    rank: usize,
    free: Vec<u32>,
}

impl Space {
    fn width(&self) -> usize {
        self.rank + self.free.len()
    }
}

/// Dependencies of a scalar-bodied map's destination elements, or `None`
/// when the body falls outside the element-exact rule.
pub(super) fn derive(
    domain: &rumoca_core::StructuredIndexDomain,
    captures: &[(Vec<SolveCallDependency>, Option<i64>)],
    body: &SolveProgramRegion,
    provenance: Span,
) -> Option<Vec<SolveCallDependency>> {
    let space = Space {
        rank: domain.binders.len(),
        free: Vec::new(),
    };
    // A scalar capture is one value at every point of the space; an
    // aggregate capture is read at coordinates the body computes.
    let captures = captures
        .iter()
        .zip(body.inputs())
        .map(|((dependencies, integer), value_type)| {
            if let Some(value) = integer {
                return Some(Symbol::Integer(AffineForm::constant(*value, space.width())));
            }
            if !value_type.dimensions().is_empty() {
                return Some(Symbol::Aggregate(dependencies.clone()));
            }
            let access = coordinates::Coordinates::access(space.rank, &space.free, Vec::new());
            let mut scalar = Dependencies::default();
            for dependency in dependencies {
                scalar.insert(dependency.remap(&access, provenance).ok()?);
            }
            Some(Symbol::Scalar(scalar.finish()))
        })
        .collect::<Option<Vec<_>>>()?;
    match region(domain, &captures, body, &space, 0, provenance)? {
        Symbol::Scalar(dependencies) => Some(dependencies),
        Symbol::Independent | Symbol::Integer(_) => Some(Vec::new()),
        Symbol::Aggregate(_) | Symbol::Mapped(_) => None,
    }
}

/// The symbol of a map region's one scalar output. The region's binders are
/// the space's axes from `first_axis` (output axes for the outermost map,
/// free axes for a nested one).
fn region(
    domain: &rumoca_core::StructuredIndexDomain,
    captures: &[Symbol],
    body: &SolveProgramRegion,
    space: &Space,
    first_axis: usize,
    provenance: Span,
) -> Option<Symbol> {
    let program = body.body();
    if body.outputs().len() != 1 || !body.outputs()[0].dimensions().is_empty() {
        return None;
    }
    let mut slots = vec![Symbol::Independent; program.slots().len()];
    for (slot, capture) in slots.iter_mut().zip(captures) {
        *slot = capture.clone();
    }
    for (ordinal, binder) in domain.binders.iter().enumerate() {
        let mut form = AffineForm::unit_binder(first_axis + ordinal, space.width())
            .checked_scale(binder.step)?;
        form.constant = binder.lower;
        *slots.get_mut(captures.len() + ordinal)? = Symbol::Integer(form);
    }
    let mut registers = vec![Symbol::Independent; program.register_types().len()];
    for spanned in program.operations() {
        step(
            spanned.operation(),
            &mut slots,
            &mut registers,
            space,
            provenance,
        )?;
    }
    let output = captures.len() + domain.binders.len();
    slots.get(output).cloned()
}

/// The symbol of a register, or `None` for an id outside the program.
fn read(registers: &[Symbol], register: SolveRegisterId) -> Option<&Symbol> {
    registers.get(register.index())
}

fn step(
    operation: &SolveOperation,
    slots: &mut [Symbol],
    registers: &mut [Symbol],
    space: &Space,
    provenance: Span,
) -> Option<()> {
    let symbol = match operation {
        SolveOperation::Load { destination, slot } => {
            *registers.get_mut(destination.index())? = slots.get(slot.index())?.clone();
            return Some(());
        }
        SolveOperation::Store { slot, source } => {
            *slots.get_mut(slot.index())? = registers.get(source.index())?.clone();
            return Some(());
        }
        SolveOperation::Constant {
            destination: _,
            value,
        } => match value.kind() {
            crate::SolveValueKind::Integer(value) => {
                Symbol::Integer(AffineForm::constant(value, space.width()))
            }
            _ => Symbol::Independent,
        },
        SolveOperation::Binary {
            operator, lhs, rhs, ..
        } => binary(*operator, read(registers, *lhs)?, read(registers, *rhs)?)?,
        SolveOperation::Unary { operand, .. } => scalar(&[read(registers, *operand)?])?,
        SolveOperation::Convert {
            operator, operand, ..
        } => match (operator, read(registers, *operand)?) {
            (SolveConversionOperator::IntegerToReal, Symbol::Integer(_)) => Symbol::Independent,
            (_, symbol) => scalar(&[symbol])?,
        },
        SolveOperation::Compare { lhs, rhs, .. } => {
            scalar(&[read(registers, *lhs)?, read(registers, *rhs)?])?
        }
        SolveOperation::Select {
            condition,
            if_true,
            if_false,
            ..
        } => scalar(&[
            read(registers, *condition)?,
            read(registers, *if_true)?,
            read(registers, *if_false)?,
        ])?,
        SolveOperation::ProjectElementDynamic {
            aggregate, indices, ..
        } => project(registers, *aggregate, indices, space, provenance)?,
        SolveOperation::Map {
            domain,
            captures,
            body,
            ..
        } => nested_map(registers, domain, captures, body, space, provenance)?,
        SolveOperation::Reduce {
            operator, operand, ..
        } if *operator != SolveReductionOperator::All => match read(registers, *operand)? {
            // The nested binders stay free axes of each dependency: the
            // reduction reads every element they enumerate.
            Symbol::Mapped(dependencies) => Symbol::Scalar(dependencies.clone()),
            _ => return None,
        },
        _ => return None,
    };
    let mut outputs = Vec::new();
    operation.visit_output_registers(|output| outputs.push(output));
    let [output] = outputs[..] else {
        return None;
    };
    *registers.get_mut(output.index())? = symbol;
    Some(())
}

/// One element read of a captured aggregate at affine Integer coordinates.
fn project(
    registers: &[Symbol],
    aggregate: SolveRegisterId,
    indices: &[SolveRegisterId],
    space: &Space,
    provenance: Span,
) -> Option<Symbol> {
    let Symbol::Aggregate(dependencies) = read(registers, aggregate)? else {
        return None;
    };
    let subscripts = indices
        .iter()
        .map(|index| match read(registers, *index)? {
            Symbol::Integer(form) => {
                let mut zero_based = form.clone();
                zero_based.constant = zero_based.constant.checked_sub(1)?;
                Some(zero_based)
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let access = coordinates::Coordinates::access(space.rank, &space.free, subscripts);
    let mut result = Dependencies::default();
    for dependency in dependencies {
        result.insert(dependency.remap(&access, provenance).ok()?);
    }
    Some(Symbol::Scalar(result.finish()))
}

/// A map nested in a region: its binders become free axes after the
/// enclosing space's.
fn nested_map(
    registers: &[Symbol],
    domain: &rumoca_core::StructuredIndexDomain,
    captures: &[SolveRegisterId],
    body: &SolveProgramRegion,
    space: &Space,
    provenance: Span,
) -> Option<Symbol> {
    let nested = Space {
        rank: space.rank,
        free: space
            .free
            .iter()
            .copied()
            .map(Some)
            .chain(
                domain
                    .extents()
                    .ok()?
                    .into_iter()
                    .map(|extent| u32::try_from(extent).ok()),
            )
            .collect::<Option<Vec<_>>>()?,
    };
    let captured = captures
        .iter()
        .map(|capture| widen_symbol(read(registers, *capture)?, &nested))
        .collect::<Option<Vec<_>>>()?;
    match region(domain, &captured, body, &nested, space.width(), provenance)? {
        Symbol::Scalar(dependencies) => Some(Symbol::Mapped(dependencies)),
        Symbol::Independent | Symbol::Integer(_) => Some(Symbol::Mapped(Vec::new())),
        Symbol::Aggregate(_) | Symbol::Mapped(_) => None,
    }
}

fn binary(operator: SolveBinaryOperator, lhs: &Symbol, rhs: &Symbol) -> Option<Symbol> {
    if let (Symbol::Integer(lhs), Symbol::Integer(rhs)) = (lhs, rhs) {
        let affine = match operator {
            SolveBinaryOperator::Add => lhs.checked_add(rhs),
            SolveBinaryOperator::Subtract => lhs.checked_add(&rhs.checked_neg()?),
            SolveBinaryOperator::Multiply if rhs.is_binder_free() => {
                lhs.checked_scale(rhs.constant)
            }
            SolveBinaryOperator::Multiply if lhs.is_binder_free() => {
                rhs.checked_scale(lhs.constant)
            }
            _ => None,
        };
        // A non-affine Integer of binders reads no input; it only fails the
        // element-exact rule if it is used as a coordinate.
        return Some(affine.map_or(Symbol::Independent, Symbol::Integer));
    }
    scalar(&[lhs, rhs])
}

/// The union of pointwise operands' dependencies; an unread aggregate or a
/// nested map's result used pointwise is outside the rule.
fn scalar(operands: &[&Symbol]) -> Option<Symbol> {
    let mut result = Dependencies::default();
    for operand in operands {
        match operand {
            Symbol::Independent | Symbol::Integer(_) => {}
            Symbol::Scalar(dependencies) => {
                for dependency in dependencies {
                    result.insert(dependency.clone());
                }
            }
            Symbol::Aggregate(_) | Symbol::Mapped(_) => return None,
        }
    }
    Some(Symbol::Scalar(result.finish()))
}

/// A symbol of the enclosing space re-expressed in a nested map's space,
/// which appends the nested binders as free axes.
fn widen_symbol(symbol: &Symbol, nested: &Space) -> Option<Symbol> {
    Some(match symbol {
        Symbol::Independent => Symbol::Independent,
        Symbol::Integer(form) => {
            let mut form = form.clone();
            form.coeffs.resize(nested.width(), 0);
            Symbol::Integer(form)
        }
        // Each dependency describes its own free axes, so a scalar of the
        // enclosing space is the same relation in the nested one.
        Symbol::Aggregate(_) | Symbol::Scalar(_) => symbol.clone(),
        Symbol::Mapped(_) => return None,
    })
}
