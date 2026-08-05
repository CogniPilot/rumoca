use super::{BinaryOp, CompareOp, UnaryOp};

pub(crate) fn eval_unary(op: UnaryOp, value: f64) -> f64 {
    match op {
        UnaryOp::Neg => -value,
        UnaryOp::Not => (value == 0.0) as u8 as f64,
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => rumoca_core::modelica_sign(value),
        UnaryOp::Sqrt => value.sqrt(),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        UnaryOp::Sin => value.sin(),
        UnaryOp::Cos => value.cos(),
        UnaryOp::Tan => value.tan(),
        UnaryOp::Asin => value.asin(),
        UnaryOp::Acos => value.acos(),
        UnaryOp::Atan => value.atan(),
        UnaryOp::Sinh => value.sinh(),
        UnaryOp::Cosh => value.cosh(),
        UnaryOp::Tanh => value.tanh(),
        UnaryOp::Exp => value.exp(),
        UnaryOp::Log => value.ln(),
        UnaryOp::Log10 => value.log10(),
    }
}

pub(crate) fn eval_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => lhs / rhs,
        BinaryOp::Pow => lhs.powf(rhs),
        BinaryOp::And => ((lhs != 0.0) && (rhs != 0.0)) as u8 as f64,
        BinaryOp::Or => ((lhs != 0.0) || (rhs != 0.0)) as u8 as f64,
        BinaryOp::Atan2 => lhs.atan2(rhs),
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
    }
}

pub(crate) fn eval_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    op.compare_as_f64(lhs, rhs)
}
