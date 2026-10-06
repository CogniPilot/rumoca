//! Test imports bind the canonical Rust binary64 intrinsic used by eval-solve.
use super::*;

type UnaryIntrinsic = (&'static str, fn(f64) -> f64);

pub(crate) fn bind<T: 'static>(linker: &mut Linker<T>) {
    let unary: [UnaryIntrinsic; 12] = [
        ("sin", f64::sin),
        ("cos", f64::cos),
        ("tan", f64::tan),
        ("asin", f64::asin),
        ("acos", f64::acos),
        ("atan", f64::atan),
        ("sinh", f64::sinh),
        ("cosh", f64::cosh),
        ("tanh", f64::tanh),
        ("exp", f64::exp),
        ("log", f64::ln),
        ("log10", f64::log10),
    ];
    for (symbol, function) in unary {
        linker
            .func_wrap("env", symbol, move |value: f64| function(value))
            .unwrap();
    }
    linker
        .func_wrap("env", "atan2", |lhs: f64, rhs: f64| lhs.atan2(rhs))
        .unwrap();
}
