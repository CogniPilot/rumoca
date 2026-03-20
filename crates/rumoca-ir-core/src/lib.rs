//! Shared IR primitives used by multiple Rumoca IR crates.

use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Location {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub start: u32,
    pub end: u32,
    pub file_name: String,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file_name, self.start_line, self.start_column
        )
    }
}

#[derive(Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token {
    /// Token text.
    pub text: Arc<str>,
    /// Source location.
    pub location: Location,
    pub token_number: u32,
    pub token_type: u16,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.text)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum OpBinary {
    #[default]
    Empty,
    Add(Token),
    Sub(Token),
    Mul(Token),
    Div(Token),
    Eq(Token),
    Neq(Token),
    Lt(Token),
    Le(Token),
    Gt(Token),
    Ge(Token),
    And(Token),
    Or(Token),
    Exp(Token),
    ExpElem(Token),
    AddElem(Token),
    SubElem(Token),
    MulElem(Token),
    DivElem(Token),
    Assign(Token),
}

impl Display for OpBinary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpBinary::Empty => write!(f, ""),
            OpBinary::Add(_) => write!(f, "+"),
            OpBinary::Sub(_) => write!(f, "-"),
            OpBinary::Mul(_) => write!(f, "*"),
            OpBinary::Div(_) => write!(f, "/"),
            OpBinary::Eq(_) => write!(f, "=="),
            OpBinary::Neq(_) => write!(f, "<>"),
            OpBinary::Lt(_) => write!(f, "<"),
            OpBinary::Le(_) => write!(f, "<="),
            OpBinary::Gt(_) => write!(f, ">"),
            OpBinary::Ge(_) => write!(f, ">="),
            OpBinary::And(_) => write!(f, "and"),
            OpBinary::Or(_) => write!(f, "or"),
            OpBinary::Exp(_) => write!(f, "^"),
            OpBinary::ExpElem(_) => write!(f, ".^"),
            OpBinary::AddElem(_) => write!(f, ".+"),
            OpBinary::SubElem(_) => write!(f, ".-"),
            OpBinary::MulElem(_) => write!(f, ".*"),
            OpBinary::DivElem(_) => write!(f, "./"),
            OpBinary::Assign(_) => write!(f, "="),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum OpUnary {
    #[default]
    Empty,
    Minus(Token),
    Plus(Token),
    DotMinus(Token),
    DotPlus(Token),
    Not(Token),
}

impl Display for OpUnary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpUnary::Empty => write!(f, ""),
            OpUnary::Minus(_) => write!(f, "-"),
            OpUnary::Plus(_) => write!(f, "+"),
            OpUnary::DotMinus(_) => write!(f, ".-"),
            OpUnary::DotPlus(_) => write!(f, ".+"),
            OpUnary::Not(_) => write!(f, "not "),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum Variability {
    #[default]
    Empty,
    Constant(Token),
    Discrete(Token),
    Parameter(Token),
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum Causality {
    #[default]
    Empty,
    Input(Token),
    Output(Token),
}

/// Type of class (model, function, connector, etc.).
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum ClassType {
    #[default]
    Model,
    Class,
    Block,
    Connector,
    Record,
    Type,
    Package,
    Function,
    Operator,
}

impl ClassType {
    /// Get the human-readable name for this class type.
    pub fn as_str(&self) -> &'static str {
        match self {
            ClassType::Model => "model",
            ClassType::Class => "class",
            ClassType::Block => "block",
            ClassType::Connector => "connector",
            ClassType::Record => "record",
            ClassType::Type => "type",
            ClassType::Package => "package",
            ClassType::Function => "function",
            ClassType::Operator => "operator",
        }
    }
}

/// A globally unique, fully-qualified variable name (e.g., `"body.position.x"`).
///
/// Shared by the flat and DAE IRs.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VarName(pub String);

impl VarName {
    /// Create a new variable name.
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    /// Get the variable name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for VarName {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for VarName {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// Modelica builtin functions (shared by flat and DAE IRs).
///
/// These are distinguished from user functions because they have
/// special semantics (e.g., `der()` identifies state variables).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinFunction {
    // Differential operators
    /// Time derivative: der(x)
    Der,
    /// Previous value (discrete): pre(x)
    Pre,

    // Math functions
    /// Absolute value: abs(x)
    Abs,
    /// Sign function: sign(x)
    Sign,
    /// Square root: sqrt(x)
    Sqrt,
    /// Integer division: div(x, y)
    Div,
    /// Modulo: mod(x, y)
    Mod,
    /// Remainder: rem(x, y)
    Rem,
    /// Floor: floor(x)
    Floor,
    /// Ceiling: ceil(x)
    Ceil,
    /// Minimum: min(x, y)
    Min,
    /// Maximum: max(x, y)
    Max,

    // Trigonometric functions
    /// Sine: sin(x)
    Sin,
    /// Cosine: cos(x)
    Cos,
    /// Tangent: tan(x)
    Tan,
    /// Arcsine: asin(x)
    Asin,
    /// Arccosine: acos(x)
    Acos,
    /// Arctangent: atan(x)
    Atan,
    /// Two-argument arctangent: atan2(y, x)
    Atan2,

    // Hyperbolic functions
    /// Hyperbolic sine: sinh(x)
    Sinh,
    /// Hyperbolic cosine: cosh(x)
    Cosh,
    /// Hyperbolic tangent: tanh(x)
    Tanh,

    // Exponential and logarithmic
    /// Exponential: exp(x)
    Exp,
    /// Natural logarithm: log(x)
    Log,
    /// Base-10 logarithm: log10(x)
    Log10,

    // Event-related
    /// Edge detection: edge(b) - true when b changes to true
    Edge,
    /// Change detection: change(v) - true when v changes
    Change,
    /// Reinitialize state: reinit(x, expr)
    Reinit,
    /// Sample clock: sample(start, interval)
    Sample,
    /// Initial condition: initial() - true during initialization
    Initial,
    /// Terminal condition: terminal() - true during termination
    Terminal,
    /// Suppress event generation: noEvent(expr) - pass-through
    NoEvent,
    /// Smooth operator: smooth(p, expr) - pass-through expr
    Smooth,
    /// Homotopy: homotopy(actual, simplified) - returns actual
    Homotopy,
    /// Semi-linear: semiLinear(x, k1, k2) = if x >= 0 then k1*x else k2*x
    SemiLinear,
    /// Delay: delay(expr, delayTime) - returns expr (no delay in continuous sim)
    Delay,
    /// Integer conversion: integer(x)
    Integer,

    // Reduction operators
    /// Sum of array elements: sum(A)
    Sum,
    /// Product of array elements: product(A)
    Product,

    // Array functions
    /// Number of dimensions: ndims(A)
    Ndims,
    /// Size of dimension: size(A, i)
    Size,
    /// Scalar from single-element array: scalar(A)
    Scalar,
    /// Vector from array: vector(A)
    Vector,
    /// Matrix from array: matrix(A)
    Matrix,
    /// Identity matrix: identity(n)
    Identity,
    /// Diagonal matrix: diagonal(v)
    Diagonal,
    /// Zero array: zeros(n1, n2, ...)
    Zeros,
    /// Ones array: ones(n1, n2, ...)
    Ones,
    /// Fill array: fill(s, n1, n2, ...)
    Fill,
    /// Linearly spaced vector: linspace(x1, x2, n)
    Linspace,
    /// Transpose: transpose(A)
    Transpose,
    /// Outer product: outerProduct(v1, v2)
    OuterProduct,
    /// Symmetric matrix: symmetric(A)
    Symmetric,
    /// Cross product: cross(x, y)
    Cross,
    /// Skew symmetric matrix: skew(x)
    Skew,

    // Linear algebra
    /// Concatenate arrays: cat(dim, A, B, ...)
    Cat,
}

impl BuiltinFunction {
    /// Try to parse a function name as a builtin.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            // Differential
            "der" => Some(Self::Der),
            "pre" => Some(Self::Pre),
            // Math
            "abs" => Some(Self::Abs),
            "sign" => Some(Self::Sign),
            "sqrt" => Some(Self::Sqrt),
            "div" => Some(Self::Div),
            "mod" => Some(Self::Mod),
            "rem" => Some(Self::Rem),
            "floor" => Some(Self::Floor),
            "ceil" => Some(Self::Ceil),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            // Trig
            "sin" => Some(Self::Sin),
            "cos" => Some(Self::Cos),
            "tan" => Some(Self::Tan),
            "asin" => Some(Self::Asin),
            "acos" => Some(Self::Acos),
            "atan" => Some(Self::Atan),
            "atan2" => Some(Self::Atan2),
            // Hyperbolic
            "sinh" => Some(Self::Sinh),
            "cosh" => Some(Self::Cosh),
            "tanh" => Some(Self::Tanh),
            // Exp/Log
            "exp" => Some(Self::Exp),
            "log" => Some(Self::Log),
            "log10" => Some(Self::Log10),
            // Event
            "edge" => Some(Self::Edge),
            "change" => Some(Self::Change),
            "reinit" => Some(Self::Reinit),
            "sample" => Some(Self::Sample),
            "initial" => Some(Self::Initial),
            "terminal" => Some(Self::Terminal),
            "noEvent" => Some(Self::NoEvent),
            "smooth" => Some(Self::Smooth),
            "homotopy" => Some(Self::Homotopy),
            "semiLinear" => Some(Self::SemiLinear),
            "delay" => Some(Self::Delay),
            "integer" => Some(Self::Integer),
            // Reduction
            "sum" => Some(Self::Sum),
            "product" => Some(Self::Product),
            // Array
            "ndims" => Some(Self::Ndims),
            "size" => Some(Self::Size),
            "scalar" => Some(Self::Scalar),
            "vector" => Some(Self::Vector),
            "matrix" => Some(Self::Matrix),
            "identity" => Some(Self::Identity),
            "diagonal" => Some(Self::Diagonal),
            "zeros" => Some(Self::Zeros),
            "ones" => Some(Self::Ones),
            "fill" => Some(Self::Fill),
            "linspace" => Some(Self::Linspace),
            "transpose" => Some(Self::Transpose),
            "outerProduct" => Some(Self::OuterProduct),
            "symmetric" => Some(Self::Symmetric),
            "cross" => Some(Self::Cross),
            "skew" => Some(Self::Skew),
            "cat" => Some(Self::Cat),
            _ => None,
        }
    }

    /// Get the function name as a string.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Der => "der",
            Self::Pre => "pre",
            Self::Abs => "abs",
            Self::Sign => "sign",
            Self::Sqrt => "sqrt",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Rem => "rem",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Min => "min",
            Self::Max => "max",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
            Self::Atan2 => "atan2",
            Self::Sinh => "sinh",
            Self::Cosh => "cosh",
            Self::Tanh => "tanh",
            Self::Exp => "exp",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Edge => "edge",
            Self::Change => "change",
            Self::Reinit => "reinit",
            Self::Sample => "sample",
            Self::Initial => "initial",
            Self::Terminal => "terminal",
            Self::NoEvent => "noEvent",
            Self::Smooth => "smooth",
            Self::Homotopy => "homotopy",
            Self::SemiLinear => "semiLinear",
            Self::Delay => "delay",
            Self::Integer => "integer",
            Self::Sum => "sum",
            Self::Product => "product",
            Self::Ndims => "ndims",
            Self::Size => "size",
            Self::Scalar => "scalar",
            Self::Vector => "vector",
            Self::Matrix => "matrix",
            Self::Identity => "identity",
            Self::Diagonal => "diagonal",
            Self::Zeros => "zeros",
            Self::Ones => "ones",
            Self::Fill => "fill",
            Self::Linspace => "linspace",
            Self::Transpose => "transpose",
            Self::OuterProduct => "outerProduct",
            Self::Symmetric => "symmetric",
            Self::Cross => "cross",
            Self::Skew => "skew",
            Self::Cat => "cat",
        }
    }
}

/// State selection hint for variables.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateSelect {
    /// Default behavior.
    #[default]
    Default,
    /// Never use as state.
    Never,
    /// Avoid using as state.
    Avoid,
    /// Prefer using as state.
    Prefer,
    /// Always use as state.
    Always,
}

/// A Modelica literal value (shared by flat and DAE IRs).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    /// Real number literal.
    Real(f64),
    /// Integer literal.
    Integer(i64),
    /// Boolean literal.
    Boolean(bool),
    /// String literal.
    String(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Real(v) => write!(f, "{}", v),
            Literal::Integer(v) => write!(f, "{}", v),
            Literal::Boolean(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "\"{}\"", v),
        }
    }
}

/// External function declaration (MLS §12.9).
///
/// For functions declared with `external` to call C/Fortran code.
/// Shared by the flat and DAE IRs.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ExternalFunction {
    /// Language specification (e.g., "C", "FORTRAN 77"). Default is "C".
    pub language: String,
    /// External function name (defaults to Modelica function name if not specified).
    pub function_name: Option<String>,
    /// Output variable that receives the return value (if any).
    pub output_name: Option<String>,
    /// Argument names passed to the external function.
    pub arg_names: Vec<String>,
}

/// Function derivative annotation (MLS §12.7.1).
///
/// Specifies the derivative function for automatic differentiation.
/// Shared by the flat and DAE IRs.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct DerivativeAnnotation {
    /// Name of the derivative function.
    pub derivative_function: String,
    /// Derivative order (default is 1).
    pub order: u32,
    /// Input variables whose derivatives are zero (treated as constants).
    pub zero_derivative: Vec<String>,
    /// Input variables with no derivative (not differentiated at all).
    pub no_derivative: Vec<String>,
}
