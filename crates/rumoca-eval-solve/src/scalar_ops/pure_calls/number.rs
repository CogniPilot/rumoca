//! Numeric projection uses the same checked scalar semantics as reference execution.
use super::*;
use crate::typed_program::number::{
    compare_elements, convert_element, eval_binary_element, eval_unary_element,
};
use solve::{SolveRealFormat, SolveScalarType};

pub(super) fn constant(kind: SolveValueKind) -> Elem {
    Elem::Const(kind)
}

fn scalar_type(kind: SolveValueKind, profile: solve::SolveArithmeticProfile) -> SolveScalarType {
    match kind {
        SolveValueKind::Integer(_) => SolveScalarType::Integer(profile.integer_domain()),
        SolveValueKind::Boolean(_) => SolveScalarType::Boolean,
        SolveValueKind::Real32(_) => SolveScalarType::Real {
            format: SolveRealFormat::Binary32,
        },
        SolveValueKind::Real64(_) => SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        },
    }
}

fn typed_unary(op: UnaryOp) -> SolveUnaryOperator {
    match op {
        UnaryOp::Neg => SolveUnaryOperator::Negate,
        UnaryOp::Not => SolveUnaryOperator::Not,
        UnaryOp::Abs => SolveUnaryOperator::Abs,
        UnaryOp::Sign => SolveUnaryOperator::Sign,
        UnaryOp::Sqrt => SolveUnaryOperator::Sqrt,
        UnaryOp::Floor => SolveUnaryOperator::Floor,
        UnaryOp::Ceil => SolveUnaryOperator::Ceiling,
        UnaryOp::Trunc => SolveUnaryOperator::Truncate,
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
    }
}

fn typed_binary(op: BinaryOp) -> SolveBinaryOperator {
    match op {
        BinaryOp::Add => SolveBinaryOperator::Add,
        BinaryOp::Sub => SolveBinaryOperator::Subtract,
        BinaryOp::Mul => SolveBinaryOperator::Multiply,
        BinaryOp::Div => SolveBinaryOperator::Divide,
        BinaryOp::Pow => SolveBinaryOperator::Power,
        BinaryOp::And => SolveBinaryOperator::And,
        BinaryOp::Or => SolveBinaryOperator::Or,
        BinaryOp::Atan2 => SolveBinaryOperator::Atan2,
        BinaryOp::Min => SolveBinaryOperator::Min,
        BinaryOp::Max => SolveBinaryOperator::Max,
    }
}

fn typed_compare(op: CompareOp) -> SolveCompareOperator {
    match op {
        CompareOp::Eq => SolveCompareOperator::Equal,
        CompareOp::Ne => SolveCompareOperator::NotEqual,
        CompareOp::Lt => SolveCompareOperator::Less,
        CompareOp::Le => SolveCompareOperator::LessEqual,
        CompareOp::Gt => SolveCompareOperator::Greater,
        CompareOp::Ge => SolveCompareOperator::GreaterEqual,
    }
}

impl Inliner<'_> {
    pub(super) fn materialize(&mut self, elem: Elem) -> Option<Reg> {
        let value = match elem {
            Elem::Reg(reg) => return Some(reg),
            Elem::Const(SolveValueKind::Real64(bits)) => f64::from_bits(bits),
            Elem::Const(SolveValueKind::Boolean(value)) => f64::from(u8::from(value)),
            Elem::Const(SolveValueKind::Integer(value)) => {
                // The scalar ABI has no Integer lane. A wider Integer must first
                // pass through an explicit, canonical Integer-to-Real conversion.
                if !(-(1i64 << 53)..=(1i64 << 53)).contains(&value) {
                    return None;
                }
                #[allow(clippy::cast_precision_loss)]
                {
                    value as f64
                }
            }
            Elem::Const(SolveValueKind::Real32(_)) => return None,
        };
        let dst = self.fresh()?;
        self.push(LinearOp::Const { dst, value })?;
        Some(dst)
    }

    pub(super) fn unary(&mut self, op: UnaryOp, arg: Elem) -> Option<Elem> {
        if let Elem::Const(value) = arg {
            let fold = matches!(
                value,
                SolveValueKind::Integer(_) | SolveValueKind::Boolean(_)
            ) || matches!(
                op,
                UnaryOp::Neg
                    | UnaryOp::Not
                    | UnaryOp::Abs
                    | UnaryOp::Floor
                    | UnaryOp::Ceil
                    | UnaryOp::Trunc
            );
            if fold {
                return Some(Elem::Const(
                    eval_unary_element(
                        typed_unary(op),
                        value,
                        scalar_type(value, self.table.arithmetic()),
                        self.provenance?,
                    )
                    .ok()?,
                ));
            }
        }
        let arg = self.materialize(arg)?;
        let dst = self.fresh()?;
        self.push(LinearOp::Unary { dst, op, arg })?;
        Some(Elem::Reg(dst))
    }

    fn fold_binary(&self, op: SolveBinaryOperator, lhs: Elem, rhs: Elem) -> Option<Option<Elem>> {
        let (Elem::Const(lhs), Elem::Const(rhs)) = (lhs, rhs) else {
            return Some(None);
        };
        let fold = match (lhs, rhs) {
            (SolveValueKind::Real64(a), SolveValueKind::Real64(b)) => {
                let (a, b) = (f64::from_bits(a), f64::from_bits(b));
                matches!(op, SolveBinaryOperator::Min | SolveBinaryOperator::Max)
                    || (a.fract() == 0.0
                        && b.fract() == 0.0
                        && matches!(
                            op,
                            SolveBinaryOperator::Add
                                | SolveBinaryOperator::Subtract
                                | SolveBinaryOperator::Multiply
                                | SolveBinaryOperator::Divide
                        ))
            }
            _ => true,
        };
        if !fold {
            return Some(None);
        }
        Some(Some(Elem::Const(
            eval_binary_element(
                op,
                lhs,
                rhs,
                scalar_type(lhs, self.table.arithmetic()),
                self.provenance?,
            )
            .ok()?,
        )))
    }

    pub(super) fn binary(&mut self, op: BinaryOp, lhs: Elem, rhs: Elem) -> Option<Elem> {
        if let Some(value) = self.fold_binary(typed_binary(op), lhs, rhs)? {
            return Some(value);
        }
        let lhs = self.materialize(lhs)?;
        let rhs = self.materialize(rhs)?;
        let dst = self.fresh()?;
        self.push(LinearOp::Binary { dst, op, lhs, rhs })?;
        Some(Elem::Reg(dst))
    }

    pub(super) fn typed_binary(
        &mut self,
        op: SolveBinaryOperator,
        lhs: Elem,
        rhs: Elem,
    ) -> Option<Elem> {
        if let Some(value) = self.fold_binary(op, lhs, rhs)? {
            return Some(value);
        }
        let quotient = |inliner: &mut Self, round: UnaryOp| {
            let ratio = inliner.binary(BinaryOp::Div, lhs, rhs)?;
            inliner.unary(round, ratio)
        };
        let remainder = |inliner: &mut Self, round: UnaryOp| {
            let rounded = quotient(inliner, round)?;
            let multiple = inliner.binary(BinaryOp::Mul, rounded, rhs)?;
            inliner.binary(BinaryOp::Sub, lhs, multiple)
        };
        match op {
            SolveBinaryOperator::IntegerQuotient => quotient(self, UnaryOp::Trunc),
            SolveBinaryOperator::IntegerModulo => remainder(self, UnaryOp::Floor),
            SolveBinaryOperator::IntegerRemainder => remainder(self, UnaryOp::Trunc),
            op => self.binary(binary_op(op), lhs, rhs),
        }
    }

    pub(super) fn zip_binary(
        &mut self,
        op: SolveBinaryOperator,
        lhs: &Val,
        rhs: &Val,
    ) -> Option<Vec<Elem>> {
        if lhs.elems.len() != rhs.elems.len() {
            return None;
        }
        lhs.elems
            .iter()
            .zip(rhs.elems.iter())
            .map(|(lhs, rhs)| self.typed_binary(op, *lhs, *rhs))
            .collect()
    }

    pub(super) fn compare(&mut self, op: CompareOp, lhs: Elem, rhs: Elem) -> Option<Elem> {
        if let (Elem::Const(lhs), Elem::Const(rhs)) = (lhs, rhs) {
            return Some(Elem::Const(
                compare_elements(typed_compare(op), lhs, rhs, self.provenance?).ok()?,
            ));
        }
        let lhs = self.materialize(lhs)?;
        let rhs = self.materialize(rhs)?;
        let dst = self.fresh()?;
        self.push(LinearOp::Compare { dst, op, lhs, rhs })?;
        Some(Elem::Reg(dst))
    }

    pub(super) fn convert(
        &self,
        op: SolveConversionOperator,
        value: Elem,
        destination: SolveScalarType,
    ) -> Option<Elem> {
        // A scalar floating-point instruction cannot encode the Integer-domain
        // checks carried by a runtime conversion or checked Integer arithmetic.
        let Elem::Const(value) = value else {
            return None;
        };
        Some(Elem::Const(
            convert_element(op, value, destination, self.provenance?).ok()?,
        ))
    }

    pub(super) fn select(&mut self, cond: Elem, if_true: Elem, if_false: Elem) -> Option<Elem> {
        match cond {
            Elem::Const(SolveValueKind::Boolean(value)) => {
                return Some(if value { if_true } else { if_false });
            }
            Elem::Const(_) => return None,
            Elem::Reg(_) => {}
        }
        let cond = self.materialize(cond)?;
        let if_true = self.materialize(if_true)?;
        let if_false = self.materialize(if_false)?;
        let dst = self.fresh()?;
        self.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        })?;
        Some(Elem::Reg(dst))
    }
}

pub(super) fn unary_op(op: SolveUnaryOperator) -> UnaryOp {
    match op {
        SolveUnaryOperator::Negate => UnaryOp::Neg,
        SolveUnaryOperator::Not => UnaryOp::Not,
        SolveUnaryOperator::Abs => UnaryOp::Abs,
        SolveUnaryOperator::Sign => UnaryOp::Sign,
        SolveUnaryOperator::Sqrt => UnaryOp::Sqrt,
        SolveUnaryOperator::Floor => UnaryOp::Floor,
        SolveUnaryOperator::Ceiling => UnaryOp::Ceil,
        SolveUnaryOperator::Truncate => UnaryOp::Trunc,
        SolveUnaryOperator::Sin => UnaryOp::Sin,
        SolveUnaryOperator::Cos => UnaryOp::Cos,
        SolveUnaryOperator::Tan => UnaryOp::Tan,
        SolveUnaryOperator::Asin => UnaryOp::Asin,
        SolveUnaryOperator::Acos => UnaryOp::Acos,
        SolveUnaryOperator::Atan => UnaryOp::Atan,
        SolveUnaryOperator::Sinh => UnaryOp::Sinh,
        SolveUnaryOperator::Cosh => UnaryOp::Cosh,
        SolveUnaryOperator::Tanh => UnaryOp::Tanh,
        SolveUnaryOperator::Exp => UnaryOp::Exp,
        SolveUnaryOperator::Log => UnaryOp::Log,
        SolveUnaryOperator::Log10 => UnaryOp::Log10,
    }
}

/// Every typed binary operator except the Integer quotient family, which
/// [`Inliner::typed_binary`] expands.
pub(super) fn binary_op(op: SolveBinaryOperator) -> BinaryOp {
    match op {
        SolveBinaryOperator::Add => BinaryOp::Add,
        SolveBinaryOperator::Subtract => BinaryOp::Sub,
        SolveBinaryOperator::Multiply => BinaryOp::Mul,
        SolveBinaryOperator::Divide
        | SolveBinaryOperator::IntegerQuotient
        | SolveBinaryOperator::IntegerModulo
        | SolveBinaryOperator::IntegerRemainder => BinaryOp::Div,
        SolveBinaryOperator::Power => BinaryOp::Pow,
        SolveBinaryOperator::And => BinaryOp::And,
        SolveBinaryOperator::Or => BinaryOp::Or,
        SolveBinaryOperator::Atan2 => BinaryOp::Atan2,
        SolveBinaryOperator::Min => BinaryOp::Min,
        SolveBinaryOperator::Max => BinaryOp::Max,
    }
}

pub(super) fn compare_op(op: SolveCompareOperator) -> CompareOp {
    match op {
        SolveCompareOperator::Equal => CompareOp::Eq,
        SolveCompareOperator::NotEqual => CompareOp::Ne,
        SolveCompareOperator::Less => CompareOp::Lt,
        SolveCompareOperator::LessEqual => CompareOp::Le,
        SolveCompareOperator::Greater => CompareOp::Gt,
        SolveCompareOperator::GreaterEqual => CompareOp::Ge,
    }
}

/// One Integer scalar per loop binder, in domain order.
pub(super) fn binders(tuple: &[i64], profile: solve::SolveArithmeticProfile) -> Option<Vec<Val>> {
    tuple
        .iter()
        .map(|value| {
            let value = solve::SolveValue::integer(profile, *value).ok()?;
            Some(Val::scalar(
                value.value_type().element_type(),
                Elem::Const(value.kind()),
            ))
        })
        .collect()
}

pub(super) fn identity_element(scalar: solve::SolveScalarType, value: bool) -> Elem {
    Elem::Const(match scalar {
        solve::SolveScalarType::Integer(_) => SolveValueKind::Integer(i64::from(value)),
        solve::SolveScalarType::Boolean => SolveValueKind::Boolean(value),
        solve::SolveScalarType::Real { .. } => {
            SolveValueKind::Real64(f64::from(u8::from(value)).to_bits())
        }
    })
}
