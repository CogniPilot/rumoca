//! The complete GALEC builtin catalog (eFMI 1.0.0 Beta 1, §3.2.6) as data,
//! plus the Appendix C reserved-name list and the keyword/reserved-word
//! lists.
//!
//! This module is the normative parity source (SPEC_0034 GAL-005): the
//! future validator checks name collisions and call signatures against it,
//! and the Phase 3 Modelica→GALEC mapping table renders only names present
//! here. Signatures were authored independently from the standard's catalog
//! description (no standard text is reproduced).
//!
//! Only four builtins can signal (trap T14): `integer` (`NAN`, `OVERFLOW`)
//! and the three linear-solver builtins (`SOLVE_LINEAR_EQUATIONS_FAILED`).
//! Everything else relies on quiet-NaN propagation.

use crate::ast::PredefinedSignal;

/// Type of one builtin parameter, shape included (array-native, GAL-026).
/// Dimension *relations* between parameters (e.g. a right-hand side sized by
/// the matrix) are semantic detail beyond what the collision/mapping checks
/// need and are documented per builtin in the standard, not encoded here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    Boolean,
    Integer,
    Real,
    /// `Integer …[:]`
    IntegerVector,
    /// `Real …[:]`
    RealVector,
    /// `Real …[:, :]`
    RealMatrix,
    /// `Real …[:, :, :]` (only `interpolation3D`'s table).
    RealArray3,
}

impl BuiltinType {
    /// True for the scalar types (participate in `C_builtin4` lifting).
    #[must_use]
    pub const fn is_scalar(self) -> bool {
        matches!(self, Self::Boolean | Self::Integer | Self::Real)
    }
}

/// One parameter of a builtin signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinParam {
    pub name: &'static str,
    pub ty: BuiltinType,
}

const fn param(name: &'static str, ty: BuiltinType) -> BuiltinParam {
    BuiltinParam { name, ty }
}

/// One builtin function with its exact signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Builtin {
    pub name: &'static str,
    pub inputs: &'static [BuiltinParam],
    pub outputs: &'static [BuiltinParam],
    /// Error signals the builtin can raise (empty for almost all, trap T14).
    pub signals: &'static [PredefinedSignal],
}

impl Builtin {
    /// True when `C_builtin4` implicitly defines element-wise `<name>1D` and
    /// `<name>2D` variants: one or two scalar inputs and one scalar output.
    #[must_use]
    pub fn is_lifted(&self) -> bool {
        let scalar_inputs =
            matches!(self.inputs.len(), 1 | 2) && self.inputs.iter().all(|p| p.ty.is_scalar());
        let scalar_output = self.outputs.len() == 1 && self.outputs[0].ty.is_scalar();
        scalar_inputs && scalar_output
    }
}

use BuiltinType::{Boolean, Integer, IntegerVector, Real, RealArray3, RealMatrix, RealVector};

const R_TO_R: &[BuiltinParam] = &[param("x", Real)];
const R_OUT: &[BuiltinParam] = &[param("y", Real)];
const RR_TO_R: &[BuiltinParam] = &[param("u1", Real), param("u2", Real)];
const II_TO_I: &[BuiltinParam] = &[param("u1", Integer), param("u2", Integer)];
const I_OUT: &[BuiltinParam] = &[param("y", Integer)];
const B_OUT: &[BuiltinParam] = &[param("b", Boolean)];
const NO_PARAMS: &[BuiltinParam] = &[];
const SOLVE_SIGNALS: &[PredefinedSignal] = &[PredefinedSignal::SolveLinearEquationsFailed];

const fn simple(
    name: &'static str,
    inputs: &'static [BuiltinParam],
    outputs: &'static [BuiltinParam],
) -> Builtin {
    Builtin {
        name,
        inputs,
        outputs,
        signals: &[],
    }
}

/// The complete mandatory builtin catalog (`C_builtin1..3`), in the
/// standard's presentation order. `C_builtin4` lifted variants are derived,
/// not listed — see [`Builtin::is_lifted`] and [`all_builtin_names`].
pub const BUILTINS: &[Builtin] = &[
    // Properties of Integer.
    simple("minInteger", NO_PARAMS, I_OUT),
    simple("maxInteger", NO_PARAMS, I_OUT),
    // Properties of Real.
    simple("minReal", NO_PARAMS, R_OUT),
    simple("maxReal", NO_PARAMS, R_OUT),
    simple("posMinReal", NO_PARAMS, R_OUT),
    simple("epsReal", NO_PARAMS, R_OUT),
    simple("nan", NO_PARAMS, R_OUT),
    simple("isNaN", R_TO_R, B_OUT),
    simple("minusInfinite", NO_PARAMS, R_OUT),
    simple("plusInfinite", NO_PARAMS, R_OUT),
    simple("isInfinite", R_TO_R, B_OUT),
    simple("isFinite", R_TO_R, B_OUT),
    // Multi-dimensional properties of Real.
    simple("hasNaN1D", &[param("r", RealVector)], B_OUT),
    simple("hasNaN2D", &[param("r", RealMatrix)], B_OUT),
    // Numeric type conversions.
    simple("real", &[param("i", Integer)], R_OUT),
    Builtin {
        name: "integer",
        inputs: &[param("r", Real)],
        outputs: &[param("i", Integer)],
        signals: &[PredefinedSignal::Nan, PredefinedSignal::Overflow],
    },
    // Rounding.
    simple("roundDown", R_TO_R, R_OUT),
    simple("roundUp", R_TO_R, R_OUT),
    simple("roundHalfToEven", R_TO_R, R_OUT),
    // Relational.
    simple("imin", II_TO_I, I_OUT),
    simple("imax", II_TO_I, I_OUT),
    simple("min", RR_TO_R, R_OUT),
    simple("max", RR_TO_R, R_OUT),
    // Mathematical constants and functions.
    simple("euler", NO_PARAMS, R_OUT),
    simple("sign", R_TO_R, R_OUT),
    simple("absolute", R_TO_R, R_OUT),
    simple("fractional", R_TO_R, R_OUT),
    simple("sqrt", R_TO_R, R_OUT),
    simple("exp", R_TO_R, R_OUT),
    simple("ln", R_TO_R, R_OUT),
    simple("lg", R_TO_R, R_OUT),
    simple(
        "safe_posdiv",
        &[param("xn", Real), param("xd", Real), param("eps", Real)],
        R_OUT,
    ),
    simple("safe_sqrt", R_TO_R, R_OUT),
    simple("safe_ln", R_TO_R, R_OUT),
    simple("safe_lg", R_TO_R, R_OUT),
    // Trigonometric constants and functions.
    simple("pi", NO_PARAMS, R_OUT),
    simple("sin", R_TO_R, R_OUT),
    simple("cos", R_TO_R, R_OUT),
    simple("tan", R_TO_R, R_OUT),
    simple("asin", R_TO_R, R_OUT),
    simple("acos", R_TO_R, R_OUT),
    simple("atan", R_TO_R, R_OUT),
    simple(
        "atan2",
        &[param("y", Real), param("x", Real)],
        &[param("z", Real)],
    ),
    simple("sinh", R_TO_R, R_OUT),
    simple("cosh", R_TO_R, R_OUT),
    simple("tanh", R_TO_R, R_OUT),
    simple("safe_tan", R_TO_R, R_OUT),
    simple("safe_asin", R_TO_R, R_OUT),
    simple("safe_acos", R_TO_R, R_OUT),
    // Systems of linear equations.
    Builtin {
        name: "solveLinearEquations",
        inputs: &[param("A", RealMatrix), param("b", RealVector)],
        outputs: &[param("x", RealVector)],
        signals: SOLVE_SIGNALS,
    },
    Builtin {
        name: "luFactorize",
        inputs: &[param("A", RealMatrix)],
        outputs: &[param("LU", RealMatrix), param("pivots", IntegerVector)],
        signals: SOLVE_SIGNALS,
    },
    Builtin {
        name: "luSolve",
        inputs: &[
            param("LU", RealMatrix),
            param("pivots", IntegerVector),
            param("b", RealVector),
        ],
        outputs: &[param("x", RealVector)],
        signals: SOLVE_SIGNALS,
    },
    // Interpolation.
    simple(
        "interpolation1D",
        &[
            param("x1", Real),
            param("x1_data", RealVector),
            param("nx1", Integer),
            param("y_data", RealVector),
            param("interpolation", Integer),
            param("extrapolation", Integer),
        ],
        R_OUT,
    ),
    simple(
        "interpolation2D",
        &[
            param("x1", Real),
            param("x2", Real),
            param("x1_data", RealVector),
            param("nx1", Integer),
            param("x2_data", RealVector),
            param("nx2", Integer),
            param("y_data", RealMatrix),
            param("interpolation", Integer),
            param("extrapolation", Integer),
        ],
        R_OUT,
    ),
    simple(
        "interpolation3D",
        &[
            param("x1", Real),
            param("x2", Real),
            param("x3", Real),
            param("x1_data", RealVector),
            param("nx1", Integer),
            param("x2_data", RealVector),
            param("nx2", Integer),
            param("x3_data", RealVector),
            param("nx3", Integer),
            param("y_data", RealArray3),
            param("interpolation", Integer),
            param("extrapolation", Integer),
        ],
        R_OUT,
    ),
    // Integer division/remainder (C_builtin2).
    simple(
        "divisionTowardsZero",
        &[param("dividend", Integer), param("divisor", Integer)],
        &[param("quotient", Integer)],
    ),
    simple(
        "remainderTowardsZero",
        &[param("dividend", Integer), param("divisor", Integer)],
        &[param("remainder", Integer)],
    ),
    // Real remainder (C_builtin3).
    simple(
        "realRemainderTowardsZero",
        &[param("dividend", Real), param("divisor", Real)],
        &[param("remainder", Real)],
    ),
];

/// Active GALEC keywords (G-1.19), excluding the future-reserved words.
pub const KEYWORDS: &[&str] = &[
    "block",
    "protected",
    "public",
    "end",
    "record",
    "function",
    "method",
    "signals",
    "algorithm",
    "input",
    "output",
    "Boolean",
    "Integer",
    "Real",
    "limit",
    "if",
    "signal",
    "in",
    "then",
    "elseif",
    "else",
    "for",
    "loop",
    "and",
    "or",
    "not",
    "size",
    "self",
];

/// Keywords reserved for future extensions (G-1.19); anything starting with
/// `__` is also reserved — see [`is_reserved_name`].
pub const RESERVED_KEYWORDS: &[&str] = &["while", "do", "until", "break", "return", "enumeration"];

/// Appendix C reserved builtin names: designed but not yet standard; users
/// must not reuse them and emitters must not call them (e.g. Modelica `mod`
/// maps to reserved `remainderDown` and is therefore unlowerable, trap T8).
pub const APPENDIX_C_RESERVED: &[&str] = &[
    // Reserved rounding (appended to C_builtin1).
    "roundTowardsZero",
    "roundAwayZero",
    "roundHalfDown",
    "roundHalfUp",
    "roundHalfTowardsZero",
    "roundHalfAwayZero",
    "roundHalfToOdd",
    // Reserved Integer division/remainder (redefine C_builtin2).
    "divisionDown",
    "divisionUp",
    "divisionAwayZero",
    "divisionHalfDown",
    "divisionHalfUp",
    "divisionHalfTowardsZero",
    "divisionHalfAwayZero",
    "divisionHalfToEven",
    "divisionHalfToOdd",
    "divisionEuclidean",
    "remainderDown",
    "remainderUp",
    "remainderAwayZero",
    "remainderHalfDown",
    "remainderHalfUp",
    "remainderHalfTowardsZero",
    "remainderHalfAwayZero",
    "remainderHalfToEven",
    "remainderHalfToOdd",
    "remainderEuclidean",
    // Reserved Real remainder (redefine C_builtin3).
    "realRemainderDown",
    "realRemainderUp",
    "realRemainderAwayZero",
    "realRemainderHalfDown",
    "realRemainderHalfUp",
    "realRemainderHalfTowardsZero",
    "realRemainderHalfAwayZero",
    "realRemainderHalfToEven",
    "realRemainderHalfToOdd",
];

/// Names the Beta-1 text references in conceptual builtin bodies without
/// defining them; reserved defensively so mangling/user names never collide
/// with a plausible future builtin.
pub const REFERENCED_UNDEFINED_NAMES: &[&str] = &["abs", "allNaN1D", "allNaN2D", "remainder"];

/// Look up a base-catalog builtin by exact name.
#[must_use]
pub fn find_builtin(name: &str) -> Option<&'static Builtin> {
    BUILTINS.iter().find(|b| b.name == name)
}

/// If `name` is a `C_builtin4` lifted variant (`<base>1D` / `<base>2D` of a
/// lifted base builtin), return the base builtin.
#[must_use]
pub fn find_lifted_base(name: &str) -> Option<&'static Builtin> {
    let base = name
        .strip_suffix("1D")
        .or_else(|| name.strip_suffix("2D"))?;
    find_builtin(base).filter(|b| b.is_lifted())
}

/// True when `name` is a builtin, including implicit lifted variants.
#[must_use]
pub fn is_builtin_name(name: &str) -> bool {
    find_builtin(name).is_some() || find_lifted_base(name).is_some()
}

/// True when `name` collides with an Appendix C reserved builtin, including
/// the lifted variants Appendix C adoption would create (all Appendix C
/// functions are 1- or 2-scalar-input scalar functions).
#[must_use]
pub fn is_appendix_c_reserved(name: &str) -> bool {
    let base = name
        .strip_suffix("1D")
        .or_else(|| name.strip_suffix("2D"))
        .unwrap_or(name);
    APPENDIX_C_RESERVED.contains(&base) || REFERENCED_UNDEFINED_NAMES.contains(&base)
}

/// True when `name` is lexically unusable as ANY identifier: keywords,
/// future-reserved keywords, and the `__` prefix space. This is the only
/// surface that applies to state entities, which are a separate namespace
/// always accessed via `self.` and MAY share names with functions and
/// builtins (S-2.5 R-2).
#[must_use]
pub fn is_lexically_reserved(name: &str) -> bool {
    KEYWORDS.contains(&name) || RESERVED_KEYWORDS.contains(&name) || name.starts_with("__")
}

/// True when `name` may not be used for a declaration in the function
/// namespace (functions, compartments, parameters, locals, error signals):
/// the lexical surface plus builtins (incl. lifted), Appendix C reserved
/// names (incl. lifted), and the six predefined error signals. This full
/// surface is also the collision surface for GAL-015 mangling (mangled
/// names must stay clear of it regardless of declaration position).
#[must_use]
pub fn is_reserved_name(name: &str) -> bool {
    is_lexically_reserved(name)
        || is_builtin_name(name)
        || is_appendix_c_reserved(name)
        || PredefinedSignal::ALL.iter().any(|s| s.name() == name)
}

/// All callable builtin names: the base catalog plus derived `1D`/`2D`
/// lifted variants, in catalog order (base, then its lifted names).
pub fn all_builtin_names() -> impl Iterator<Item = String> {
    BUILTINS.iter().flat_map(|b| {
        let mut names = vec![b.name.to_string()];
        if b.is_lifted() {
            names.push(format!("{}1D", b.name));
            names.push(format!("{}2D", b.name));
        }
        names.into_iter()
    })
}
