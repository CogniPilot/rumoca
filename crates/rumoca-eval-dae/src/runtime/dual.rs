//! Forward-mode automatic differentiation using dual numbers.
//!
//! A dual number `Dual { re, du }` represents `re + ε·du` where ε² = 0.

use std::fmt;
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Neg, Sub, SubAssign};

/// A dual number for forward-mode AD: `re + ε·du`.
#[derive(Clone, Copy)]
pub struct Dual {
    /// Real part.
    pub re: f64,
    /// Dual (derivative) part.
    pub du: f64,
}

impl Dual {
    /// Create a new dual number.
    pub fn new(re: f64, du: f64) -> Self {
        Self { re, du }
    }
}

impl Default for Dual {
    fn default() -> Self {
        Self { re: 0.0, du: 0.0 }
    }
}

impl fmt::Debug for Dual {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Dual({}, {})", self.re, self.du)
    }
}

impl Add for Dual {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            re: self.re + rhs.re,
            du: self.du + rhs.du,
        }
    }
}

impl Sub for Dual {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            re: self.re - rhs.re,
            du: self.du - rhs.du,
        }
    }
}

impl Mul for Dual {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            re: self.re * rhs.re,
            du: self.du * rhs.re + self.re * rhs.du,
        }
    }
}

impl Div for Dual {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        let inv = 1.0 / rhs.re;
        Self {
            re: self.re * inv,
            du: (self.du * rhs.re - self.re * rhs.du) * inv * inv,
        }
    }
}

impl Neg for Dual {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            re: -self.re,
            du: -self.du,
        }
    }
}

impl AddAssign for Dual {
    fn add_assign(&mut self, rhs: Self) {
        self.re += rhs.re;
        self.du += rhs.du;
    }
}

impl SubAssign for Dual {
    fn sub_assign(&mut self, rhs: Self) {
        self.re -= rhs.re;
        self.du -= rhs.du;
    }
}

impl MulAssign for Dual {
    fn mul_assign(&mut self, rhs: Self) {
        let new_du = self.du * rhs.re + self.re * rhs.du;
        self.re *= rhs.re;
        self.du = new_du;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dual_add() {
        let a = Dual::new(3.0, 1.0);
        let b = Dual::new(2.0, 0.0);
        let c = a + b;
        assert_eq!(c.re, 5.0);
        assert_eq!(c.du, 1.0);
    }

    #[test]
    fn test_dual_mul() {
        let a = Dual::new(3.0, 1.0);
        let b = Dual::new(2.0, 0.0);
        let c = a * b;
        assert_eq!(c.re, 6.0);
        assert_eq!(c.du, 2.0);
    }

    #[test]
    fn test_dual_div() {
        let a = Dual::new(6.0, 1.0);
        let b = Dual::new(3.0, 0.0);
        let c = a / b;
        assert!((c.re - 2.0).abs() < 1e-12);
        assert!((c.du - 1.0 / 3.0).abs() < 1e-12);
    }

    #[test]
    fn test_dual_neg() {
        let a = Dual::new(3.0, 1.0);
        let b = -a;
        assert_eq!(b.re, -3.0);
        assert_eq!(b.du, -1.0);
    }

    #[test]
    fn test_dual_x_squared() {
        // f(x) = x^2, f'(x) = 2x, at x=3: f=9, f'=6
        let x = Dual::new(3.0, 1.0);
        let y = x * x;
        assert_eq!(y.re, 9.0);
        assert_eq!(y.du, 6.0);
    }
}
