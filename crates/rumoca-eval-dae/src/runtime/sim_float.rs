//! SimFloat trait: abstraction over f64 and Dual for generic evaluation.

use std::fmt::Debug;
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Neg, Sub, SubAssign};

use crate::runtime::dual::Dual;

/// Trait for numeric types usable in the expression evaluator.
///
/// Implemented for `f64` (standard evaluation) and `Dual` (forward-mode AD).
pub trait SimFloat:
    Copy
    + Debug
    + Default
    + Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + Neg<Output = Self>
    + AddAssign
    + SubAssign
    + MulAssign
{
    fn from_f64(v: f64) -> Self;
    fn real(&self) -> f64;
    fn to_bool(self) -> bool;
    fn from_bool(v: bool) -> Self;

    // Math functions
    fn sin(self) -> Self;
    fn cos(self) -> Self;
    fn tan(self) -> Self;
    fn asin(self) -> Self;
    fn acos(self) -> Self;
    fn atan(self) -> Self;
    fn atan2(self, x: Self) -> Self;
    fn sinh(self) -> Self;
    fn cosh(self) -> Self;
    fn tanh(self) -> Self;
    fn exp(self) -> Self;
    fn ln(self) -> Self;
    fn log10(self) -> Self;
    fn sqrt(self) -> Self;
    fn powf(self, exp: Self) -> Self;
    fn abs(self) -> Self;
    fn sign(self) -> Self;
    fn floor(self) -> Self;
    fn ceil(self) -> Self;
    fn trunc(self) -> Self;
    fn min(self, rhs: Self) -> Self;
    fn max(self, rhs: Self) -> Self;
    fn modulo(self, rhs: Self) -> Self;

    // Comparison (on real part only for Dual)
    fn lt(self, rhs: Self) -> bool;
    fn le(self, rhs: Self) -> bool;
    fn gt(self, rhs: Self) -> bool;
    fn ge(self, rhs: Self) -> bool;
    fn eq_approx(self, rhs: Self) -> bool;

    // Constants
    fn zero() -> Self;
    fn one() -> Self;
    fn infinity() -> Self;
    fn nan() -> Self;
    fn epsilon() -> Self;
}

// =============================================================================
// f64 implementation
// =============================================================================

impl SimFloat for f64 {
    fn from_f64(v: f64) -> Self {
        v
    }
    fn real(&self) -> f64 {
        *self
    }
    fn to_bool(self) -> bool {
        self != 0.0
    }
    fn from_bool(v: bool) -> Self {
        if v { 1.0 } else { 0.0 }
    }

    fn sin(self) -> Self {
        f64::sin(self)
    }
    fn cos(self) -> Self {
        f64::cos(self)
    }
    fn tan(self) -> Self {
        f64::tan(self)
    }
    fn asin(self) -> Self {
        f64::asin(self)
    }
    fn acos(self) -> Self {
        f64::acos(self)
    }
    fn atan(self) -> Self {
        f64::atan(self)
    }
    fn atan2(self, x: Self) -> Self {
        f64::atan2(self, x)
    }
    fn sinh(self) -> Self {
        f64::sinh(self)
    }
    fn cosh(self) -> Self {
        f64::cosh(self)
    }
    fn tanh(self) -> Self {
        f64::tanh(self)
    }
    fn exp(self) -> Self {
        f64::exp(self)
    }
    fn ln(self) -> Self {
        f64::ln(self)
    }
    fn log10(self) -> Self {
        f64::log10(self)
    }
    fn sqrt(self) -> Self {
        f64::sqrt(self)
    }
    fn powf(self, exp: Self) -> Self {
        f64::powf(self, exp)
    }
    fn abs(self) -> Self {
        f64::abs(self)
    }
    fn sign(self) -> Self {
        if self > 0.0 {
            1.0
        } else if self < 0.0 {
            -1.0
        } else {
            0.0
        }
    }
    fn floor(self) -> Self {
        f64::floor(self)
    }
    fn ceil(self) -> Self {
        f64::ceil(self)
    }
    fn trunc(self) -> Self {
        f64::trunc(self)
    }
    fn min(self, rhs: Self) -> Self {
        f64::min(self, rhs)
    }
    fn max(self, rhs: Self) -> Self {
        f64::max(self, rhs)
    }
    fn modulo(self, rhs: Self) -> Self {
        self % rhs
    }

    fn lt(self, rhs: Self) -> bool {
        self < rhs
    }
    fn le(self, rhs: Self) -> bool {
        self <= rhs
    }
    fn gt(self, rhs: Self) -> bool {
        self > rhs
    }
    fn ge(self, rhs: Self) -> bool {
        self >= rhs
    }
    fn eq_approx(self, rhs: Self) -> bool {
        (self - rhs).abs() < f64::EPSILON
    }

    fn zero() -> Self {
        0.0
    }
    fn one() -> Self {
        1.0
    }
    fn infinity() -> Self {
        f64::INFINITY
    }
    fn nan() -> Self {
        f64::NAN
    }
    fn epsilon() -> Self {
        f64::EPSILON
    }
}

// =============================================================================
// Dual implementation
// =============================================================================

impl SimFloat for Dual {
    fn from_f64(v: f64) -> Self {
        Dual { re: v, du: 0.0 }
    }
    fn real(&self) -> f64 {
        self.re
    }
    fn to_bool(self) -> bool {
        self.re != 0.0
    }
    fn from_bool(v: bool) -> Self {
        Dual {
            re: if v { 1.0 } else { 0.0 },
            du: 0.0,
        }
    }

    fn sin(self) -> Self {
        Dual {
            re: self.re.sin(),
            du: self.du * self.re.cos(),
        }
    }
    fn cos(self) -> Self {
        Dual {
            re: self.re.cos(),
            du: -self.du * self.re.sin(),
        }
    }
    fn tan(self) -> Self {
        let c = self.re.cos();
        Dual {
            re: self.re.tan(),
            du: self.du / (c * c),
        }
    }
    fn asin(self) -> Self {
        let denom = (1.0 - self.re * self.re).sqrt();
        Dual {
            re: self.re.asin(),
            // Guard 0/0 at singular constants (e.g. asin(1.0) with du=0).
            du: if self.du == 0.0 { 0.0 } else { self.du / denom },
        }
    }
    fn acos(self) -> Self {
        let denom = (1.0 - self.re * self.re).sqrt();
        Dual {
            re: self.re.acos(),
            // Guard 0/0 at singular constants (e.g. acos(1.0) with du=0).
            du: if self.du == 0.0 {
                0.0
            } else {
                -self.du / denom
            },
        }
    }
    fn atan(self) -> Self {
        Dual {
            re: self.re.atan(),
            du: self.du / (1.0 + self.re * self.re),
        }
    }
    fn atan2(self, x: Self) -> Self {
        let denom = self.re * self.re + x.re * x.re;
        Dual {
            re: self.re.atan2(x.re),
            du: (self.du * x.re - self.re * x.du) / denom,
        }
    }
    fn sinh(self) -> Self {
        Dual {
            re: self.re.sinh(),
            du: self.du * self.re.cosh(),
        }
    }
    fn cosh(self) -> Self {
        Dual {
            re: self.re.cosh(),
            du: self.du * self.re.sinh(),
        }
    }
    fn tanh(self) -> Self {
        let c = self.re.cosh();
        Dual {
            re: self.re.tanh(),
            du: self.du / (c * c),
        }
    }
    fn exp(self) -> Self {
        let e = self.re.exp();
        Dual {
            re: e,
            du: self.du * e,
        }
    }
    fn ln(self) -> Self {
        let du = if self.re != 0.0 {
            self.du / self.re
        } else {
            0.0
        };
        Dual {
            re: self.re.ln(),
            du,
        }
    }
    fn log10(self) -> Self {
        let du = if self.re != 0.0 {
            self.du / (self.re * std::f64::consts::LN_10)
        } else {
            0.0
        };
        Dual {
            re: self.re.log10(),
            du,
        }
    }
    fn sqrt(self) -> Self {
        let s = self.re.sqrt();
        let du = if self.re != 0.0 {
            self.du / (2.0 * s)
        } else {
            0.0
        };
        Dual { re: s, du }
    }
    fn powf(self, exp: Self) -> Self {
        // d/dx (x^n) = n * x^(n-1) * dx  +  x^n * ln(x) * dn
        let val = self.re.powf(exp.re);
        let du = if exp.du == 0.0 {
            // Constant exponent: use power rule n * x^(n-1) * dx
            // This avoids ln(x) which is NaN for x < 0.
            if self.re != 0.0 {
                exp.re * self.re.powf(exp.re - 1.0) * self.du
            } else if exp.re == 1.0 {
                self.du
            } else {
                0.0
            }
        } else if self.re > 0.0 {
            // Variable exponent, positive base: full formula
            val * (exp.du * self.re.ln() + exp.re * self.du / self.re)
        } else {
            // Variable exponent with non-positive base: undefined
            0.0
        };
        Dual { re: val, du }
    }
    fn abs(self) -> Self {
        if self.re >= 0.0 { self } else { -self }
    }
    fn sign(self) -> Self {
        // Piecewise constant — derivative is 0
        Dual {
            re: if self.re > 0.0 {
                1.0
            } else if self.re < 0.0 {
                -1.0
            } else {
                0.0
            },
            du: 0.0,
        }
    }
    fn floor(self) -> Self {
        Dual {
            re: self.re.floor(),
            du: 0.0,
        }
    }
    fn ceil(self) -> Self {
        Dual {
            re: self.re.ceil(),
            du: 0.0,
        }
    }
    fn trunc(self) -> Self {
        Dual {
            re: self.re.trunc(),
            du: 0.0,
        }
    }
    fn min(self, rhs: Self) -> Self {
        if self.re <= rhs.re { self } else { rhs }
    }
    fn max(self, rhs: Self) -> Self {
        if self.re >= rhs.re { self } else { rhs }
    }
    fn modulo(self, rhs: Self) -> Self {
        // d/dx (x mod y) = 1 (treating y as constant for simplicity)
        Dual {
            re: self.re % rhs.re,
            du: self.du,
        }
    }

    fn lt(self, rhs: Self) -> bool {
        self.re < rhs.re
    }
    fn le(self, rhs: Self) -> bool {
        self.re <= rhs.re
    }
    fn gt(self, rhs: Self) -> bool {
        self.re > rhs.re
    }
    fn ge(self, rhs: Self) -> bool {
        self.re >= rhs.re
    }
    fn eq_approx(self, rhs: Self) -> bool {
        (self.re - rhs.re).abs() < f64::EPSILON
    }

    fn zero() -> Self {
        Dual { re: 0.0, du: 0.0 }
    }
    fn one() -> Self {
        Dual { re: 1.0, du: 0.0 }
    }
    fn infinity() -> Self {
        Dual {
            re: f64::INFINITY,
            du: 0.0,
        }
    }
    fn nan() -> Self {
        Dual {
            re: f64::NAN,
            du: 0.0,
        }
    }
    fn epsilon() -> Self {
        Dual {
            re: f64::EPSILON,
            du: 0.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f64_sin() {
        let v = SimFloat::sin(std::f64::consts::FRAC_PI_2);
        assert!((v - 1.0).abs() < 1e-12);
    }

    #[test]
    fn test_dual_sin_at_zero() {
        // sin(0+ε) = sin(0) + ε·cos(0) = 0 + ε·1
        let x = Dual::new(0.0, 1.0);
        let y = SimFloat::sin(x);
        assert!(y.re.abs() < 1e-12);
        assert!((y.du - 1.0).abs() < 1e-12);
    }

    #[test]
    fn test_dual_cos_at_zero() {
        // cos(0+ε) = cos(0) + ε·(-sin(0)) = 1 + 0
        let x = Dual::new(0.0, 1.0);
        let y = SimFloat::cos(x);
        assert!((y.re - 1.0).abs() < 1e-12);
        assert!(y.du.abs() < 1e-12);
    }

    #[test]
    fn test_dual_exp() {
        // exp(1+ε) = e + ε·e
        let x = Dual::new(1.0, 1.0);
        let y = SimFloat::exp(x);
        let e = std::f64::consts::E;
        assert!((y.re - e).abs() < 1e-12);
        assert!((y.du - e).abs() < 1e-12);
    }

    #[test]
    fn test_dual_sqrt() {
        // sqrt(4+ε) = 2 + ε/(2*2) = 2 + ε·0.25
        let x = Dual::new(4.0, 1.0);
        let y = SimFloat::sqrt(x);
        assert!((y.re - 2.0).abs() < 1e-12);
        assert!((y.du - 0.25).abs() < 1e-12);
    }

    #[test]
    fn test_dual_asin_constant_at_singularity_has_zero_du() {
        let y = SimFloat::asin(Dual::new(1.0, 0.0));
        assert!(y.re.is_finite());
        assert_eq!(y.du, 0.0);
    }

    #[test]
    fn test_dual_acos_constant_at_singularity_has_zero_du() {
        let y = SimFloat::acos(Dual::new(1.0, 0.0));
        assert!(y.re.is_finite());
        assert_eq!(y.du, 0.0);
    }

    #[test]
    fn test_dual_floor() {
        // floor is piecewise constant: dual part should be 0
        let x = Dual::new(3.7, 5.0);
        let y = SimFloat::floor(x);
        assert_eq!(y.re, 3.0);
        assert_eq!(y.du, 0.0);
    }

    #[test]
    fn test_dual_comparison_ignores_dual() {
        assert!(SimFloat::lt(Dual::new(1.0, 100.0), Dual::new(2.0, -50.0)));
        assert!(!SimFloat::gt(Dual::new(1.0, 100.0), Dual::new(2.0, -50.0)));
    }

    #[test]
    fn test_dual_min_max() {
        let a = Dual::new(3.0, 1.0);
        let b = Dual::new(5.0, 2.0);
        let mn = SimFloat::min(a, b);
        assert_eq!(mn.re, 3.0);
        assert_eq!(mn.du, 1.0);
        let mx = SimFloat::max(a, b);
        assert_eq!(mx.re, 5.0);
        assert_eq!(mx.du, 2.0);
    }

    // =========================================================================
    // Comprehensive AD vs finite-difference tests for every Dual function
    // =========================================================================

    /// Compute df/dx via central finite differences: (f(x+h) - f(x-h)) / (2h).
    fn fd_derivative(f: impl Fn(f64) -> f64, x: f64) -> f64 {
        let h = 1e-7;
        (f(x + h) - f(x - h)) / (2.0 * h)
    }

    /// Assert AD derivative matches FD for a unary function f(x).
    fn check_unary_ad(
        name: &str,
        f_dual: impl Fn(Dual) -> Dual,
        f_f64: impl Fn(f64) -> f64,
        x: f64,
        tol: f64,
    ) {
        let ad = f_dual(Dual::new(x, 1.0));
        let fd = fd_derivative(&f_f64, x);
        let diff = (ad.du - fd).abs();
        let scale = fd.abs().max(ad.du.abs()).max(1.0);
        assert!(
            diff / scale < tol,
            "{name}({x}): AD derivative={:.8e}, FD={:.8e}, rel_err={:.2e}",
            ad.du,
            fd,
            diff / scale,
        );
    }

    /// Assert AD derivative matches FD for a binary function f(x, y), varying x.
    fn check_binary_ad_dx(
        name: &str,
        f_dual: impl Fn(Dual, Dual) -> Dual,
        f_f64: impl Fn(f64, f64) -> f64,
        x: f64,
        y: f64,
        tol: f64,
    ) {
        let ad = f_dual(Dual::new(x, 1.0), Dual::new(y, 0.0));
        let fd = fd_derivative(|xv| f_f64(xv, y), x);
        let diff = (ad.du - fd).abs();
        let scale = fd.abs().max(ad.du.abs()).max(1.0);
        assert!(
            diff / scale < tol,
            "{name}({x},{y}) d/dx: AD={:.8e}, FD={:.8e}, rel_err={:.2e}",
            ad.du,
            fd,
            diff / scale,
        );
    }

    /// Assert AD derivative matches FD for a binary function f(x, y), varying y.
    fn check_binary_ad_dy(
        name: &str,
        f_dual: impl Fn(Dual, Dual) -> Dual,
        f_f64: impl Fn(f64, f64) -> f64,
        x: f64,
        y: f64,
        tol: f64,
    ) {
        let ad = f_dual(Dual::new(x, 0.0), Dual::new(y, 1.0));
        let fd = fd_derivative(|yv| f_f64(x, yv), y);
        let diff = (ad.du - fd).abs();
        let scale = fd.abs().max(ad.du.abs()).max(1.0);
        assert!(
            diff / scale < tol,
            "{name}({x},{y}) d/dy: AD={:.8e}, FD={:.8e}, rel_err={:.2e}",
            ad.du,
            fd,
            diff / scale,
        );
    }

    #[test]
    fn test_dual_ad_vs_fd_sin() {
        for &x in &[0.0, 0.5, 1.0, -0.7, 2.5] {
            check_unary_ad("sin", SimFloat::sin, f64::sin, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_cos() {
        for &x in &[0.0, 0.5, 1.0, -0.7, 2.5] {
            check_unary_ad("cos", SimFloat::cos, f64::cos, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_tan() {
        for &x in &[0.0, 0.3, -0.5, 1.0] {
            check_unary_ad("tan", SimFloat::tan, f64::tan, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_asin() {
        for &x in &[0.0, 0.3, -0.5, 0.9] {
            check_unary_ad("asin", SimFloat::asin, f64::asin, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_acos() {
        for &x in &[0.0, 0.3, -0.5, 0.9] {
            check_unary_ad("acos", SimFloat::acos, f64::acos, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_atan() {
        for &x in &[0.0, 0.5, -1.0, 3.0] {
            check_unary_ad("atan", SimFloat::atan, f64::atan, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_atan2() {
        let pairs = [(1.0, 2.0), (0.5, -1.0), (-0.3, 0.7), (2.0, 0.1)];
        for &(y, x) in &pairs {
            check_binary_ad_dx("atan2", SimFloat::atan2, f64::atan2, y, x, 1e-6);
            check_binary_ad_dy("atan2", SimFloat::atan2, f64::atan2, y, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_sinh() {
        for &x in &[0.0, 0.5, -1.0, 2.0] {
            check_unary_ad("sinh", SimFloat::sinh, f64::sinh, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_cosh() {
        for &x in &[0.0, 0.5, -1.0, 2.0] {
            check_unary_ad("cosh", SimFloat::cosh, f64::cosh, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_tanh() {
        for &x in &[0.0, 0.5, -1.0, 2.0] {
            check_unary_ad("tanh", SimFloat::tanh, f64::tanh, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_exp() {
        for &x in &[0.0, 1.0, -1.0, 3.0] {
            check_unary_ad("exp", SimFloat::exp, f64::exp, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_ln() {
        for &x in &[0.5, 1.0, 2.0, 10.0] {
            check_unary_ad("ln", SimFloat::ln, f64::ln, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_log10() {
        for &x in &[0.5, 1.0, 2.0, 10.0] {
            check_unary_ad("log10", SimFloat::log10, f64::log10, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_sqrt() {
        for &x in &[0.25, 1.0, 4.0, 9.0] {
            check_unary_ad("sqrt", SimFloat::sqrt, f64::sqrt, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_abs() {
        // abs(x) = x for x>0, -x for x<0; derivative = sign(x)
        for &x in &[0.5, 2.0, -0.5, -3.0] {
            check_unary_ad("abs", SimFloat::abs, f64::abs, x, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_powf_const_exp() {
        // x^n with constant n, including negative base
        for &n in &[2.0, 3.0, 0.5] {
            for &x in &[0.5, 2.0, 3.0] {
                check_binary_ad_dx(
                    &format!("powf(n={n})"),
                    SimFloat::powf,
                    f64::powf,
                    x,
                    n,
                    1e-6,
                );
            }
        }
        // Negative base with integer exponent
        for &n in &[2.0, 3.0, 4.0] {
            for &x in &[-0.5, -2.0, -3.0] {
                check_binary_ad_dx(
                    &format!("powf_neg(n={n})"),
                    SimFloat::powf,
                    f64::powf,
                    x,
                    n,
                    1e-6,
                );
            }
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_powf_variable_exp() {
        // x^y with variable y (only positive base)
        for &(x, y) in &[(2.0, 3.0), (1.5, 0.5), (3.0, 2.5)] {
            check_binary_ad_dx("powf_var_dx", SimFloat::powf, f64::powf, x, y, 1e-6);
            check_binary_ad_dy("powf_var_dy", SimFloat::powf, f64::powf, x, y, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_min() {
        // min(x, y): derivative is 1 w.r.t. the smaller, 0 w.r.t. the larger
        let f = |a: Dual, b: Dual| SimFloat::min(a, b);
        let ff = |a: f64, b: f64| f64::min(a, b);

        // x < y: d/dx min(x,y) = 1, d/dy min(x,y) = 0
        check_binary_ad_dx("min", f, ff, 1.0, 3.0, 1e-6);
        check_binary_ad_dy("min", f, ff, 1.0, 3.0, 1e-6);

        // x > y: d/dx min(x,y) = 0, d/dy min(x,y) = 1
        check_binary_ad_dx("min", f, ff, 5.0, 2.0, 1e-6);
        check_binary_ad_dy("min", f, ff, 5.0, 2.0, 1e-6);
    }

    #[test]
    fn test_dual_ad_vs_fd_max() {
        let f = |a: Dual, b: Dual| SimFloat::max(a, b);
        let ff = |a: f64, b: f64| f64::max(a, b);

        // x > y: d/dx max(x,y) = 1, d/dy max(x,y) = 0
        check_binary_ad_dx("max", f, ff, 5.0, 2.0, 1e-6);
        check_binary_ad_dy("max", f, ff, 5.0, 2.0, 1e-6);

        // x < y: d/dx max(x,y) = 0, d/dy max(x,y) = 1
        check_binary_ad_dx("max", f, ff, 1.0, 3.0, 1e-6);
        check_binary_ad_dy("max", f, ff, 1.0, 3.0, 1e-6);
    }

    #[test]
    fn test_dual_ad_vs_fd_modulo() {
        // x mod y: d/dx = 1 (our implementation treats y as constant)
        for &(x, y) in &[(5.3, 2.0), (7.1, 3.0), (10.0, 3.5)] {
            check_binary_ad_dx("modulo", SimFloat::modulo, |a, b| a % b, x, y, 1e-6);
        }
    }

    #[test]
    fn test_dual_sign_derivative_is_zero() {
        // sign is piecewise constant, derivative should be 0
        for &x in &[0.5, 2.0, -0.5, -3.0] {
            let d = SimFloat::sign(Dual::new(x, 1.0));
            assert_eq!(d.du, 0.0, "sign({x}) should have zero derivative");
        }
    }

    #[test]
    fn test_dual_floor_ceil_trunc_derivative_is_zero() {
        // Piecewise constant functions: derivative should be 0
        for &x in &[1.3, 2.7, -0.5, -3.2] {
            let f = SimFloat::floor(Dual::new(x, 1.0));
            assert_eq!(f.du, 0.0, "floor({x}) derivative should be 0");

            let c = SimFloat::ceil(Dual::new(x, 1.0));
            assert_eq!(c.du, 0.0, "ceil({x}) derivative should be 0");

            let t = SimFloat::trunc(Dual::new(x, 1.0));
            assert_eq!(t.du, 0.0, "trunc({x}) derivative should be 0");
        }
    }

    // =========================================================================
    // Arithmetic operator AD vs FD tests
    // =========================================================================

    #[test]
    fn test_dual_ad_vs_fd_add() {
        let f = |a: Dual, b: Dual| a + b;
        let ff = |a: f64, b: f64| a + b;
        check_binary_ad_dx("add", f, ff, 3.0, 2.0, 1e-6);
        check_binary_ad_dy("add", f, ff, 3.0, 2.0, 1e-6);
    }

    #[test]
    fn test_dual_ad_vs_fd_sub() {
        let f = |a: Dual, b: Dual| a - b;
        let ff = |a: f64, b: f64| a - b;
        check_binary_ad_dx("sub", f, ff, 3.0, 2.0, 1e-6);
        check_binary_ad_dy("sub", f, ff, 3.0, 2.0, 1e-6);
    }

    #[test]
    fn test_dual_ad_vs_fd_mul() {
        let f = |a: Dual, b: Dual| a * b;
        let ff = |a: f64, b: f64| a * b;
        for &(x, y) in &[(3.0, 2.0), (0.5, -1.0), (-2.0, -3.0)] {
            check_binary_ad_dx("mul", f, ff, x, y, 1e-6);
            check_binary_ad_dy("mul", f, ff, x, y, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_div() {
        let f = |a: Dual, b: Dual| a / b;
        let ff = |a: f64, b: f64| a / b;
        for &(x, y) in &[(6.0, 3.0), (1.0, 2.0), (-3.0, 4.0)] {
            check_binary_ad_dx("div", f, ff, x, y, 1e-6);
            check_binary_ad_dy("div", f, ff, x, y, 1e-6);
        }
    }

    #[test]
    fn test_dual_ad_vs_fd_neg() {
        for &x in &[0.5, -2.0, 3.0] {
            check_unary_ad("neg", |d| -d, |v| -v, x, 1e-6);
        }
    }
}
