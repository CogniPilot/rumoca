use rumoca_ir_galec::package::AlgorithmCodeRealFormat;

/// Executable Real semantics retained by the owning Algorithm Code package.
///
/// `Value::Real` uses `f64` only as a lossless carrier for both admitted
/// formats. Every constructor and primitive operation passes through this
/// closed helper, so a Binary32 package cannot accidentally inherit host-f64
/// intermediates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RealArithmetic {
    Binary32,
    Binary64,
}

impl RealArithmetic {
    pub(crate) const fn from_format(format: AlgorithmCodeRealFormat) -> Self {
        match format {
            AlgorithmCodeRealFormat::Binary32 => Self::Binary32,
            AlgorithmCodeRealFormat::Binary64 => Self::Binary64,
        }
    }

    pub(crate) fn round(self, value: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from(value as f32),
            Self::Binary64 => value,
        }
    }

    pub(crate) fn convert_integer(self, value: i64) -> f64 {
        match self {
            Self::Binary32 => f64::from(value as f32),
            Self::Binary64 => value as f64,
        }
    }

    pub(crate) fn add(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32) + (rhs as f32)),
            Self::Binary64 => lhs + rhs,
        }
    }

    pub(crate) fn sub(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32) - (rhs as f32)),
            Self::Binary64 => lhs - rhs,
        }
    }

    pub(crate) fn mul(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32) * (rhs as f32)),
            Self::Binary64 => lhs * rhs,
        }
    }

    pub(crate) fn div(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32) / (rhs as f32)),
            Self::Binary64 => lhs / rhs,
        }
    }

    pub(crate) fn remainder(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32) % (rhs as f32)),
            Self::Binary64 => lhs % rhs,
        }
    }

    pub(crate) fn pow(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32).powf(rhs as f32)),
            Self::Binary64 => lhs.powf(rhs),
        }
    }

    pub(crate) fn atan2(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from((lhs as f32).atan2(rhs as f32)),
            Self::Binary64 => lhs.atan2(rhs),
        }
    }

    pub(crate) fn unary(self, operation: RealUnary, value: f64) -> f64 {
        match self {
            Self::Binary32 => f64::from(operation.apply_f32(value as f32)),
            Self::Binary64 => operation.apply_f64(value),
        }
    }

    pub(crate) const fn max_finite(self) -> f64 {
        match self {
            Self::Binary32 => f32::MAX as f64,
            Self::Binary64 => f64::MAX,
        }
    }

    pub(crate) const fn min_positive(self) -> f64 {
        match self {
            Self::Binary32 => f32::MIN_POSITIVE as f64,
            Self::Binary64 => f64::MIN_POSITIVE,
        }
    }

    pub(crate) const fn epsilon(self) -> f64 {
        match self {
            Self::Binary32 => f32::EPSILON as f64,
            Self::Binary64 => f64::EPSILON,
        }
    }

    pub(crate) const fn euler(self) -> f64 {
        match self {
            Self::Binary32 => std::f32::consts::E as f64,
            Self::Binary64 => std::f64::consts::E,
        }
    }

    pub(crate) const fn pi(self) -> f64 {
        match self {
            Self::Binary32 => std::f32::consts::PI as f64,
            Self::Binary64 => std::f64::consts::PI,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RealUnary {
    Floor,
    Ceil,
    RoundTiesEven,
    Abs,
    Fract,
    Sqrt,
    Exp,
    Ln,
    Log10,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
}

impl RealUnary {
    fn apply_f32(self, value: f32) -> f32 {
        match self {
            Self::Floor => value.floor(),
            Self::Ceil => value.ceil(),
            Self::RoundTiesEven => value.round_ties_even(),
            Self::Abs => value.abs(),
            Self::Fract => value.fract(),
            Self::Sqrt => value.sqrt(),
            Self::Exp => value.exp(),
            Self::Ln => value.ln(),
            Self::Log10 => value.log10(),
            Self::Sin => value.sin(),
            Self::Cos => value.cos(),
            Self::Tan => value.tan(),
            Self::Asin => value.asin(),
            Self::Acos => value.acos(),
            Self::Atan => value.atan(),
            Self::Sinh => value.sinh(),
            Self::Cosh => value.cosh(),
            Self::Tanh => value.tanh(),
        }
    }

    fn apply_f64(self, value: f64) -> f64 {
        match self {
            Self::Floor => value.floor(),
            Self::Ceil => value.ceil(),
            Self::RoundTiesEven => value.round_ties_even(),
            Self::Abs => value.abs(),
            Self::Fract => value.fract(),
            Self::Sqrt => value.sqrt(),
            Self::Exp => value.exp(),
            Self::Ln => value.ln(),
            Self::Log10 => value.log10(),
            Self::Sin => value.sin(),
            Self::Cos => value.cos(),
            Self::Tan => value.tan(),
            Self::Asin => value.asin(),
            Self::Acos => value.acos(),
            Self::Atan => value.atan(),
            Self::Sinh => value.sinh(),
            Self::Cosh => value.cosh(),
            Self::Tanh => value.tanh(),
        }
    }
}
