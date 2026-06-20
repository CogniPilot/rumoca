use rumoca_core::{Literal, OpBinary};
use rumoca_ir_dae as dae;

pub(super) fn literal_to_f64(value: &Literal) -> Option<f64> {
    match value {
        Literal::Real(value) => Some(*value),
        Literal::Integer(value) => Some(*value as f64),
        Literal::Boolean(value) => Some(f64::from(*value)),
        _ => None,
    }
}

pub(super) fn compile_time_var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => {
                usize::try_from(*value).ok()
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

pub(super) fn compile_time_binary(op: &OpBinary, lhs: f64, rhs: f64) -> Option<f64> {
    match op {
        OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
        OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
        OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
        OpBinary::Div | OpBinary::DivElem => Some(lhs / rhs),
        OpBinary::Exp | OpBinary::ExpElem => Some(lhs.powf(rhs)),
        OpBinary::Lt => Some(f64::from(lhs < rhs)),
        OpBinary::Le => Some(f64::from(lhs <= rhs)),
        OpBinary::Gt => Some(f64::from(lhs > rhs)),
        OpBinary::Ge => Some(f64::from(lhs >= rhs)),
        OpBinary::Eq => Some(f64::from((lhs - rhs).abs() < f64::EPSILON)),
        OpBinary::Neq => Some(f64::from((lhs - rhs).abs() >= f64::EPSILON)),
        OpBinary::And => Some(f64::from(lhs != 0.0 && rhs != 0.0)),
        OpBinary::Or => Some(f64::from(lhs != 0.0 || rhs != 0.0)),
        OpBinary::Assign | OpBinary::Empty => None,
    }
}
