//! The one Binary64 math ABI shared by every emitted model module.
//!
//! Emitted modules import `env.<symbol>` for each [`MathImport`]. Guests that
//! carry a model (the fixed FMI-LS core and the browser kernel host) export
//! exactly these symbols, all implemented by the same `libm` routines, so the
//! result does not depend on the embedding engine's own math library.

use crate::emit::MathImport;

/// Evaluate one ABI function; unary imports ignore `rhs`.
pub(crate) fn evaluate(import: MathImport, lhs: f64, rhs: f64) -> f64 {
    match import {
        MathImport::Abs => libm::fabs(lhs),
        MathImport::Sin => libm::sin(lhs),
        MathImport::Cos => libm::cos(lhs),
        MathImport::Tan => libm::tan(lhs),
        MathImport::Asin => libm::asin(lhs),
        MathImport::Acos => libm::acos(lhs),
        MathImport::Atan => libm::atan(lhs),
        MathImport::Sinh => libm::sinh(lhs),
        MathImport::Cosh => libm::cosh(lhs),
        MathImport::Tanh => libm::tanh(lhs),
        MathImport::Exp => libm::exp(lhs),
        MathImport::Log => libm::log(lhs),
        MathImport::Log10 => libm::log10(lhs),
        MathImport::Pow => libm::pow(lhs, rhs),
        MathImport::Atan2 => libm::atan2(lhs, rhs),
    }
}

/// Exported ABI symbols, only in guests that provide them to a linked model.
#[cfg(any(
    all(target_arch = "wasm32", target_os = "unknown"),
    all(
        feature = "fixed-native-guest-receiver",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    )
))]
mod exports {
    #![allow(unsafe_code)]
    use super::{MathImport, evaluate};

    macro_rules! unary {
        ($($name:ident => $import:ident),* $(,)?) => {$(
            #[unsafe(no_mangle)]
            pub(super) extern "C" fn $name(x: f64) -> f64 {
                evaluate(MathImport::$import, x, 0.0)
            }
        )*};
    }
    macro_rules! binary {
        ($($name:ident => $import:ident),* $(,)?) => {$(
            #[unsafe(no_mangle)]
            pub(super) extern "C" fn $name(x: f64, y: f64) -> f64 {
                evaluate(MathImport::$import, x, y)
            }
        )*};
    }
    unary! {
        abs => Abs, sin => Sin, cos => Cos, tan => Tan, asin => Asin, acos => Acos,
        atan => Atan, sinh => Sinh, cosh => Cosh, tanh => Tanh, exp => Exp,
        log => Log, log10 => Log10,
    }
    binary! { pow => Pow, atan2 => Atan2 }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abi_follows_c99_edge_cases_the_reference_interpreter_uses() {
        let pow = |a, b| evaluate(MathImport::Pow, a, b);
        assert_eq!(pow(1.0, f64::INFINITY), 1.0);
        assert_eq!(pow(-1.0, f64::NEG_INFINITY), 1.0);
        assert_eq!(pow(1.0, f64::NAN), 1.0);
        assert_eq!(pow(f64::NAN, 0.0), 1.0);
        assert_eq!(evaluate(MathImport::Atan2, 0.0, -1.0), std::f64::consts::PI);
        assert_eq!(
            evaluate(MathImport::Abs, -0.0, 0.0).to_bits(),
            0.0f64.to_bits()
        );
    }

    #[test]
    fn every_abi_symbol_is_total_over_special_inputs() {
        for import in MathImport::ALL {
            for value in [0.0, -0.0, 1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
                let _ = evaluate(import, value, value);
            }
        }
    }
}
