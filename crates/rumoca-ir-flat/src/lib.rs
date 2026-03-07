//! Flat Model IR for the Rumoca compiler.
//!
//! This crate defines the Flat Model (MLS §5.6), which represents
//! the flat equation system with globally unique variable names.
//!
//! The Flat Model is produced by the flatten phase from the Instance Tree.

pub mod clocks;
#[cfg(test)]
mod component_ref_helpers;
pub mod connections;
#[cfg(test)]
mod convert_from_ast;
mod function;
pub mod name_utils;
#[cfg(test)]
mod subscripts;
pub mod visitor;
mod when_equations;

#[cfg(test)]
mod tests;

#[cfg(test)]
use convert_from_ast::{
    convert_array_comprehension_with_def_map, convert_class_modification_with_def_map,
    convert_comprehension_indices, convert_constructor_arg, convert_expr_vec_with_def_map,
    convert_function_call, convert_function_call_with_def_map, convert_if_with_def_map,
    convert_terminal, function_component_ref_from_ast,
};
use indexmap::{IndexMap, IndexSet};
use rumoca_core::{DefId, Span, TypeId};
#[cfg(test)]
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};

pub type Causality = rumoca_ir_core::Causality;
pub type ClassType = rumoca_ir_core::ClassType;
pub type OpBinary = rumoca_ir_core::OpBinary;
pub type OpUnary = rumoca_ir_core::OpUnary;
pub type StateSelect = rumoca_ir_core::StateSelect;
pub type Token = rumoca_ir_core::Token;
pub type Variability = rumoca_ir_core::Variability;

fn token_from_ast(token: &rumoca_ir_core::Token) -> Token {
    token.clone()
}

pub fn op_binary_from_ast(op: &rumoca_ir_core::OpBinary) -> OpBinary {
    match op {
        rumoca_ir_core::OpBinary::Empty => OpBinary::Empty,
        rumoca_ir_core::OpBinary::Add(token) => OpBinary::Add(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Sub(token) => OpBinary::Sub(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Mul(token) => OpBinary::Mul(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Div(token) => OpBinary::Div(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Eq(token) => OpBinary::Eq(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Neq(token) => OpBinary::Neq(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Lt(token) => OpBinary::Lt(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Le(token) => OpBinary::Le(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Gt(token) => OpBinary::Gt(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Ge(token) => OpBinary::Ge(token_from_ast(token)),
        rumoca_ir_core::OpBinary::And(token) => OpBinary::And(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Or(token) => OpBinary::Or(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Exp(token) => OpBinary::Exp(token_from_ast(token)),
        rumoca_ir_core::OpBinary::ExpElem(token) => OpBinary::ExpElem(token_from_ast(token)),
        rumoca_ir_core::OpBinary::AddElem(token) => OpBinary::AddElem(token_from_ast(token)),
        rumoca_ir_core::OpBinary::SubElem(token) => OpBinary::SubElem(token_from_ast(token)),
        rumoca_ir_core::OpBinary::MulElem(token) => OpBinary::MulElem(token_from_ast(token)),
        rumoca_ir_core::OpBinary::DivElem(token) => OpBinary::DivElem(token_from_ast(token)),
        rumoca_ir_core::OpBinary::Assign(token) => OpBinary::Assign(token_from_ast(token)),
    }
}

pub fn op_unary_from_ast(op: &rumoca_ir_core::OpUnary) -> OpUnary {
    match op {
        rumoca_ir_core::OpUnary::Empty => OpUnary::Empty,
        rumoca_ir_core::OpUnary::Minus(token) => OpUnary::Minus(token_from_ast(token)),
        rumoca_ir_core::OpUnary::Plus(token) => OpUnary::Plus(token_from_ast(token)),
        rumoca_ir_core::OpUnary::DotMinus(token) => OpUnary::DotMinus(token_from_ast(token)),
        rumoca_ir_core::OpUnary::DotPlus(token) => OpUnary::DotPlus(token_from_ast(token)),
        rumoca_ir_core::OpUnary::Not(token) => OpUnary::Not(token_from_ast(token)),
    }
}

pub fn variability_from_ast(variability: &rumoca_ir_core::Variability) -> Variability {
    match variability {
        rumoca_ir_core::Variability::Empty => Variability::Empty,
        rumoca_ir_core::Variability::Constant(token) => {
            Variability::Constant(token_from_ast(token))
        }
        rumoca_ir_core::Variability::Discrete(token) => {
            Variability::Discrete(token_from_ast(token))
        }
        rumoca_ir_core::Variability::Parameter(token) => {
            Variability::Parameter(token_from_ast(token))
        }
    }
}

pub fn causality_from_ast(causality: &rumoca_ir_core::Causality) -> Causality {
    match causality {
        rumoca_ir_core::Causality::Empty => Causality::Empty,
        rumoca_ir_core::Causality::Input(token) => Causality::Input(token_from_ast(token)),
        rumoca_ir_core::Causality::Output(token) => Causality::Output(token_from_ast(token)),
    }
}

#[cfg(test)]
use component_ref_helpers::from_component_ref_with_def_map_impl;
#[cfg(test)]
use subscripts::subscript_to_string;

// Re-export connection types
pub use connections::{
    ConnectedVariable, ConnectionGraph, ConnectionSet, ConnectionSets, EqualityConstraint,
    GraphEdge, GraphNode, RootStatus, SpanningTree, SpanningTreeEdge,
};

// Re-export clock types
pub use clocks::{
    BaseClock, BaseClockPartition, ClockKind, ClockPartitions, SubClock, SubClockPartition,
};
pub use name_utils::component_base_name;

// Re-export visitor types
pub use visitor::{
    ContainsDerChecker, ExpressionVisitor, FunctionCallCollector, StateVariableCollector,
    VarRefCollector,
};

/// A globally unique variable name in the flat model.
///
/// Example: "body.position.x" for a nested component path.
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

// =============================================================================
// Expression: Flattened expression with globally qualified names
// =============================================================================

/// A flattened expression with globally qualified variable names.
///
/// This is the expression type used after flattening, where all variable
/// references use globally unique names (e.g., "body.position.x").
///
/// Key differences from AST Expression:
/// - Variable references use VarName (globally qualified)
/// - Builtin functions (der, pre, sin, cos, etc.) are distinguished from user functions
/// - Literals are concrete values
/// - Parentheses are eliminated (precedence is in tree structure)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    /// Binary operation: lhs op rhs
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    /// Unary operation: op rhs
    Unary { op: OpUnary, rhs: Box<Expression> },
    /// Variable reference with optional subscripts.
    VarRef {
        name: VarName,
        subscripts: Vec<Subscript>,
    },
    /// Builtin function call (der, pre, sin, cos, etc.)
    BuiltinCall {
        function: BuiltinFunction,
        args: Vec<Expression>,
    },
    /// User-defined function call.
    FunctionCall {
        name: VarName,
        args: Vec<Expression>,
        /// True when originated from AST `ClassModification` syntax.
        /// These represent constructor-style calls (e.g., record constructors).
        #[serde(default)]
        is_constructor: bool,
    },
    /// Literal value.
    Literal(Literal),
    /// Conditional expression: if cond then expr elseif ... else expr
    If {
        branches: Vec<(Expression, Expression)>,
        else_branch: Box<Expression>,
    },
    /// Array literal: {e1, e2, ...}
    Array {
        elements: Vec<Expression>,
        is_matrix: bool,
    },
    /// Tuple: (e1, e2, ...)
    Tuple { elements: Vec<Expression> },
    /// Range expression: start:step:end or start:end
    Range {
        start: Box<Expression>,
        step: Option<Box<Expression>>,
        end: Box<Expression>,
    },
    /// Array comprehension: `{expr for i in range ... if filter}` (MLS §10.4.1).
    ///
    /// Unlike `Array`, this preserves symbolic comprehension structure and avoids
    /// eager scalar expansion when consumers can reason structurally.
    ArrayComprehension {
        expr: Box<Expression>,
        indices: Vec<ComprehensionIndex>,
        filter: Option<Box<Expression>>,
    },
    /// Array indexing: `base[subscripts]`
    ///
    /// Used for indexing into expressions that aren't simple variables,
    /// e.g., `(a+b)[i]`, `func()[1]`.
    Index {
        base: Box<Expression>,
        subscripts: Vec<Subscript>,
    },
    /// Field access on expressions: base.field
    ///
    /// Used for accessing record fields from expressions that aren't simple variables,
    /// e.g., `func().field`, `(if cond then a else b).field`.
    FieldAccess {
        base: Box<Expression>,
        field: String,
    },
    /// Empty expression placeholder.
    Empty,
}

/// Namespaced short flat-IR surface used by downstream crates.
pub mod flat {
    pub use super::{
        Algorithm, AssertEquation, ComprehensionIndex, DerivativeAnnotation, Equation, Expression,
        ExternalFunction, ForEquation, ForEquationIteration, Function, FunctionParam, Literal,
        Model, Subscript, Variable, WhenClause, WhenEquation,
    };
}

pub use when_equations::{WhenClause, WhenEquation};

/// Builtin functions in Modelica.
///
/// These are distinguished from user functions because they have
/// special semantics (e.g., der() identifies state variables).
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

/// A subscript in a flattened expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Subscript {
    /// Concrete index value.
    Index(i64),
    /// Colon (full range): `:`
    Colon,
    /// Expression subscript (for dynamic indexing).
    Expr(Box<Expression>),
}

/// One index iterator in a flat array-comprehension expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComprehensionIndex {
    /// Local iterator name.
    pub name: String,
    /// Iterator range expression.
    pub range: Expression,
}

/// A literal value in a flattened expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

/// MLS §5.6: "flat equation system with globally unique variable names"
///
/// The Flat Model is the result of flattening, containing all variables
/// with globally unique names and all equations ready for analysis.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Model {
    /// All variables with globally unique names.
    pub variables: IndexMap<VarName, Variable>,
    /// Declared flat-output type name for each variable (e.g., Boolean, Integer, MyEnum).
    ///
    /// Keys match `variables` and values preserve resolved type identity for rendering.
    #[serde(default)]
    pub variable_type_names: IndexMap<VarName, String>,
    /// Flat-output `final` qualifier flags keyed by variable name (MLS §7.2.6).
    ///
    /// When present and true, codegen should emit `final` before the declaration prefix.
    #[serde(default)]
    pub variable_final_flags: IndexMap<VarName, bool>,
    /// Regular equations (0 = residual form).
    pub equations: Vec<Equation>,
    /// Preserved `for`-equation grouping metadata for regular equations.
    #[serde(default)]
    pub for_equations: Vec<ForEquation>,
    /// Runtime assertion equations from regular equation sections (MLS §8.3.7).
    ///
    /// Assertions are preserved for flat output but do not contribute to DAE
    /// equation balance/unknown counts.
    #[serde(default)]
    pub assert_equations: Vec<AssertEquation>,
    /// Initial equations (0 = residual form).
    pub initial_equations: Vec<Equation>,
    /// Preserved `for`-equation grouping metadata for initial equations.
    #[serde(default)]
    pub initial_for_equations: Vec<ForEquation>,
    /// Runtime assertion equations from initial equation sections (MLS §8.6, §8.3.7).
    #[serde(default)]
    pub initial_assert_equations: Vec<AssertEquation>,
    /// Algorithm sections.
    pub algorithms: Vec<Algorithm>,
    /// Initial algorithm sections.
    pub initial_algorithms: Vec<Algorithm>,
    /// When clauses.
    pub when_clauses: Vec<WhenClause>,
    /// User-defined functions used by this model (MLS §12).
    pub functions: IndexMap<VarName, Function>,
    /// True if the model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// The class type of the root model (model, connector, record, etc.)
    #[serde(default)]
    pub class_type: ClassType,
    /// Optional description string from the root class declaration.
    pub model_description: Option<String>,
    /// Connectors with Connections.root() declarations (MLS §9.4.1).
    /// These are definite roots for overconstrained connectors, providing
    /// implicit equations that don't need to come from external connections.
    /// Stores the full path to the overconstrained record (e.g., "pin_p.reference").
    #[serde(default)]
    pub definite_roots: std::collections::HashSet<String>,
    /// Branches from Connections.branch(a, b) calls (MLS §9.4).
    /// Required edges in the virtual connection graph.
    #[serde(default)]
    pub branches: Vec<(String, String)>,
    /// Optional edges derived from connect() statements for overconstrained nodes (MLS §9.4).
    /// These are used together with `branches` when building the virtual connection graph.
    #[serde(default)]
    pub optional_edges: Vec<(String, String)>,
    /// Potential roots from Connections.potentialRoot(a, priority) calls (MLS §9.4).
    #[serde(default)]
    pub potential_roots: Vec<(String, i64)>,
    /// Names of top-level components whose class type is `connector` (MLS §4.7).
    /// Per MLS §4.7, only flow variables in top-level public connector components
    /// count toward the local equation size for balance checking. Components of
    /// type `model` or `block` (like Delta in transformers) are NOT interface
    /// connectors even if they contain connectors internally.
    #[serde(default)]
    pub top_level_connectors: std::collections::HashSet<String>,
    /// Names of top-level components declared with `input` causality.
    /// Fields of these components (e.g., `state.phase` from `input Record state`)
    /// are external inputs and should NOT be promoted to algebraic unknowns,
    /// unlike sub-component inputs from type interfaces (MLS §4.4.2.2).
    #[serde(default)]
    pub top_level_input_components: std::collections::HashSet<String>,
    /// Scalar count of excess equations from VCG break edges (MLS §9.4).
    /// Break edges in the overconstrained connection graph generate equality equations
    /// that should be replaced by `equalityConstraint()` calls. Until that's implemented,
    /// this correction tracks how many excess equation scalars exist.
    #[serde(default)]
    pub oc_break_edge_scalar_count: usize,
    /// Enumeration literal ordinal map (MLS §4.9.5, 1-based ordinals).
    ///
    /// Keys are canonical literal paths (e.g.
    /// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`), values are
    /// integer ordinals used by runtime numeric evaluation.
    #[serde(default)]
    pub enum_literal_ordinals: IndexMap<String, i64>,
}

impl Model {
    /// Create a new empty flat model.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a variable to the model.
    pub fn add_variable(&mut self, name: VarName, var: Variable) {
        self.variables.insert(name, var);
    }

    /// Add an equation to the model.
    pub fn add_equation(&mut self, eq: Equation) {
        self.equations.push(eq);
    }

    /// Add preserved for-equation metadata for regular equations.
    pub fn add_for_equation(&mut self, for_eq: ForEquation) {
        self.for_equations.push(for_eq);
    }

    /// Add an initial equation to the model.
    pub fn add_initial_equation(&mut self, eq: Equation) {
        self.initial_equations.push(eq);
    }

    /// Add preserved for-equation metadata for initial equations.
    pub fn add_initial_for_equation(&mut self, for_eq: ForEquation) {
        self.initial_for_equations.push(for_eq);
    }

    /// Add a function definition to the model.
    pub fn add_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func);
    }

    /// Get a function definition by name.
    pub fn get_function(&self, name: &VarName) -> Option<&Function> {
        self.functions.get(name)
    }

    /// Get the number of variables.
    pub fn num_variables(&self) -> usize {
        self.variables.len()
    }

    /// Get the number of equations.
    pub fn num_equations(&self) -> usize {
        self.equations.len()
    }

    /// Get the number of functions.
    pub fn num_functions(&self) -> usize {
        self.functions.len()
    }

    /// Return parameter names that are fixed at initialization but have no binding equation.
    ///
    /// Per MLS §8.6, parameters default to `fixed=true`. For standalone simulation,
    /// fixed parameters should have explicit bindings.
    pub fn unbound_fixed_parameters(&self) -> Vec<VarName> {
        self.variables
            .iter()
            .filter_map(|(name, var)| {
                if matches!(var.variability, Variability::Parameter(_))
                    && var.fixed.unwrap_or(true)
                    && var.binding.is_none()
                {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// True if any fixed parameter has no binding equation.
    pub fn has_unbound_fixed_parameters(&self) -> bool {
        self.variables.values().any(|var| {
            matches!(var.variability, Variability::Parameter(_))
                && var.fixed.unwrap_or(true)
                && var.binding.is_none()
        })
    }
}

/// Flat variable with globally unique name.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Variable {
    /// Globally unique name (e.g., "body.position.x").
    pub name: VarName,
    /// Reference to the type in the TypeTable.
    pub type_id: TypeId,
    /// Variability (constant, parameter, discrete, continuous).
    pub variability: Variability,
    /// Causality (input, output, or empty).
    pub causality: Causality,
    /// Flow prefix.
    pub flow: bool,
    /// Stream prefix.
    pub stream: bool,
    /// Resolved array dimensions (preserved per SPEC_0019).
    pub dims: Vec<i64>,
    /// True if this variable is used in connection equations.
    pub connected: bool,

    // Resolved attributes
    /// Start value attribute.
    pub start: Option<Expression>,
    /// Fixed attribute.
    pub fixed: Option<bool>,
    /// Minimum value attribute.
    pub min: Option<Expression>,
    /// Maximum value attribute.
    pub max: Option<Expression>,
    /// Nominal value attribute.
    pub nominal: Option<Expression>,
    /// Quantity string attribute.
    pub quantity: Option<String>,
    /// Unit string attribute.
    pub unit: Option<String>,
    /// Display-unit string attribute.
    pub display_unit: Option<String>,
    /// Optional declaration description string (`"..."` after declaration).
    pub description: Option<String>,
    /// State selection hint.
    pub state_select: StateSelect,

    /// Binding equation value.
    pub binding: Option<Expression>,
    /// True if binding came from a modification rather than declaration.
    pub binding_from_modification: bool,
    /// True if this parameter has annotation(Evaluate=true) or is declared final.
    /// Structural parameters can be evaluated at compile time for if-equation
    /// branch selection (MLS §18.3).
    pub evaluate: bool,

    /// True if this variable's base type is Integer or Boolean (MLS §4.5).
    /// Such variables are discrete by default even without explicit `discrete` prefix.
    /// This is used during variable classification to correctly identify discrete
    /// variables for the DAE balance calculation.
    #[serde(default)]
    pub is_discrete_type: bool,

    /// True if this variable is a primitive type (Real, Integer, Boolean, String).
    /// Record-typed variables (like Complex with .re and .im fields) are not primitive.
    /// Non-primitive variables should not be counted as unknowns since their fields
    /// are counted separately. MLS §4.8: Balance checking uses expanded scalar counts.
    #[serde(default)]
    pub is_primitive: bool,

    /// True if this variable comes from an expandable connector (MLS §9.1.3).
    /// Unconnected expandable connector members without bindings are unused and
    /// shouldn't count as unknowns in the DAE balance calculation.
    #[serde(default)]
    pub from_expandable_connector: bool,

    /// True if this variable belongs to an overconstrained connector (MLS §9.4).
    /// A connector is overconstrained if its type defines an `equalityConstraint` function.
    #[serde(default)]
    pub is_overconstrained: bool,

    /// True if this component is declared in a protected section (MLS §4.7).
    /// Protected components are not part of the public interface and their flow
    /// variables should not count as interface flows for balance checking.
    #[serde(default)]
    pub is_protected: bool,

    /// The path of the enclosing overconstrained record (MLS §9.4).
    /// E.g., "frame_a.R" for variables frame_a.R.T and frame_a.R.w.
    /// Used to group OC variables into VCG nodes for balance correction.
    #[serde(default)]
    pub oc_record_path: Option<String>,

    /// The output size of the enclosing record's equalityConstraint function.
    /// E.g., 3 for Orientation (returns `Real[3]`).
    #[serde(default)]
    pub oc_eq_constraint_size: Option<usize>,
}

/// A single name segment in a flattened component reference.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ComponentRefPart {
    /// Identifier text for this segment.
    pub ident: String,
    /// Optional explicit subscripts on this segment.
    #[serde(default)]
    pub subs: Vec<Subscript>,
}

/// A flattened component reference used in algorithm statements.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ComponentReference {
    /// Whether this reference originated from an `inner/outer` local lookup.
    #[serde(default)]
    pub local: bool,
    /// Dotted path segments.
    pub parts: Vec<ComponentRefPart>,
    /// Optional resolved definition ID carried from resolve.
    #[serde(default)]
    pub def_id: Option<DefId>,
}

impl ComponentReference {
    /// Build a flattened component reference from AST.
    #[cfg(test)]
    pub fn from_ast_with_def_map(
        comp: &ast::ComponentReference,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        // Some resolved/local references can carry only def_id with empty path parts.
        // In that case, use def_map as a fallback to recover a concrete name.
        if comp.parts.is_empty()
            && let Some(def_id) = comp.def_id
            && let Some(path) = def_map.and_then(|map| map.get(&def_id))
        {
            return Self {
                local: comp.local,
                parts: path
                    .split('.')
                    .map(|segment| ComponentRefPart {
                        ident: segment.to_string(),
                        subs: Vec::new(),
                    })
                    .collect(),
                def_id: Some(def_id),
            };
        }

        // Preserve already-qualified instance paths from flattening.
        // Def-map canonicalization is only valid for function names, not
        // variable targets in algorithm statements.
        Self {
            local: comp.local,
            parts: comp
                .parts
                .iter()
                .map(|part| ComponentRefPart {
                    ident: part.ident.text.to_string(),
                    subs: part
                        .subs
                        .as_ref()
                        .map(|subs| subs.iter().map(Subscript::from_ast).collect())
                        .unwrap_or_default(),
                })
                .collect(),
            def_id: comp.def_id,
        }
    }

    /// Build a flattened component reference from AST without a def-map.
    #[cfg(test)]
    pub fn from_ast(comp: &ast::ComponentReference) -> Self {
        Self::from_ast_with_def_map(comp, None)
    }

    /// Convert to a dotted variable name.
    pub fn to_var_name(&self) -> VarName {
        let name = self
            .parts
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>()
            .join(".");
        VarName::new(name)
    }
}

impl std::fmt::Display for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_var_name())
    }
}

/// A single `for` iterator in a flattened algorithm statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForIndex {
    /// Iterator identifier (e.g., `i` in `for i in 1:n loop`).
    pub ident: String,
    /// Iterator range expression.
    pub range: Expression,
}

/// Conditional block used by `if`, `when`, and `while`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatementBlock {
    /// Block condition.
    pub cond: Expression,
    /// Block body statements.
    pub stmts: Vec<Statement>,
}

/// Flattened algorithm statement tree.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub enum Statement {
    #[default]
    Empty,
    Assignment {
        comp: ComponentReference,
        value: Expression,
    },
    Return,
    Break,
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Statement>,
    },
    While(StatementBlock),
    If {
        cond_blocks: Vec<StatementBlock>,
        else_block: Option<Vec<Statement>>,
    },
    When(Vec<StatementBlock>),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
        outputs: Vec<Expression>,
    },
    Reinit {
        variable: ComponentReference,
        value: Expression,
    },
    Assert {
        condition: Expression,
        message: Expression,
        level: Option<Expression>,
    },
}

/// Extract output variables assigned by algorithm statements.
///
/// This collects left-hand side targets from assignments (including inside control-flow
/// statements) and deduplicates them while preserving first-seen order.
///
/// For array element assignments like `x[i] := ...`, the base variable `x` is returned.
pub fn extract_algorithm_outputs(statements: &[Statement]) -> Vec<VarName> {
    let mut outputs = IndexSet::new();
    for stmt in statements {
        collect_algorithm_outputs_from_statement(stmt, &mut outputs);
    }
    outputs.into_iter().collect()
}

/// Convert a component reference to its base variable name.
///
/// Subscripts are intentionally dropped so assignments like `x[i] := ...` map to `x`.
pub fn component_ref_to_base_var_name(comp: &ComponentReference) -> VarName {
    let parts: Vec<String> = comp.parts.iter().map(|p| p.ident.to_string()).collect();
    VarName::new(parts.join("."))
}

fn collect_algorithm_outputs_from_statement(stmt: &Statement, outputs: &mut IndexSet<VarName>) {
    match stmt {
        Statement::Assignment { comp, .. } => {
            outputs.insert(component_ref_to_base_var_name(comp));
        }
        Statement::For { equations, .. } => {
            for s in equations {
                collect_algorithm_outputs_from_statement(s, outputs);
            }
        }
        Statement::While(block) => {
            for s in &block.stmts {
                collect_algorithm_outputs_from_statement(s, outputs);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for s in &block.stmts {
                    collect_algorithm_outputs_from_statement(s, outputs);
                }
            }
            if let Some(stmts) = else_block {
                for s in stmts {
                    collect_algorithm_outputs_from_statement(s, outputs);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for s in &block.stmts {
                    collect_algorithm_outputs_from_statement(s, outputs);
                }
            }
        }
        Statement::FunctionCall { outputs: outs, .. } => {
            for out in outs {
                if let Expression::VarRef { name, .. } = out {
                    outputs.insert(name.clone());
                }
            }
        }
        Statement::Reinit { variable, .. } => {
            outputs.insert(component_ref_to_base_var_name(variable));
        }
        Statement::Empty | Statement::Return | Statement::Break | Statement::Assert { .. } => {}
    }
}

/// Strip surrounding quotes from a string if present.
#[cfg(test)]
fn strip_quotes(text: &str) -> String {
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        text[1..text.len() - 1].to_string()
    } else {
        text.to_string()
    }
}

#[cfg(test)]
impl ForIndex {
    /// Convert from an AST for-index.
    pub fn from_ast_with_def_map(
        index: &ast::ForIndex,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        Self {
            ident: index.ident.text.to_string(),
            range: Expression::from_ast_with_def_map(&index.range, def_map),
        }
    }
}

#[cfg(test)]
impl StatementBlock {
    /// Convert from an AST statement block.
    pub fn from_ast_with_def_map(
        block: &ast::StatementBlock,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        Self {
            cond: Expression::from_ast_with_def_map(&block.cond, def_map),
            stmts: block
                .stmts
                .iter()
                .map(|stmt| Statement::from_ast_with_def_map(stmt, def_map))
                .collect(),
        }
    }
}

#[cfg(test)]
impl Statement {
    /// Convert from an AST statement.
    pub fn from_ast_with_def_map(
        stmt: &ast::Statement,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        match stmt {
            ast::Statement::Empty => Statement::Empty,
            ast::Statement::Assignment { comp, value } => Statement::Assignment {
                comp: ComponentReference::from_ast_with_def_map(comp, def_map),
                value: Expression::from_ast_with_def_map(value, def_map),
            },
            ast::Statement::Return { .. } => Statement::Return,
            ast::Statement::Break { .. } => Statement::Break,
            ast::Statement::For { indices, equations } => Statement::For {
                indices: indices
                    .iter()
                    .map(|index| ForIndex::from_ast_with_def_map(index, def_map))
                    .collect(),
                equations: equations
                    .iter()
                    .map(|inner| Statement::from_ast_with_def_map(inner, def_map))
                    .collect(),
            },
            ast::Statement::While(block) => {
                Statement::While(StatementBlock::from_ast_with_def_map(block, def_map))
            }
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| StatementBlock::from_ast_with_def_map(block, def_map))
                    .collect(),
                else_block: else_block.as_ref().map(|stmts| {
                    stmts
                        .iter()
                        .map(|inner| Statement::from_ast_with_def_map(inner, def_map))
                        .collect()
                }),
            },
            ast::Statement::When(blocks) => Statement::When(
                blocks
                    .iter()
                    .map(|block| StatementBlock::from_ast_with_def_map(block, def_map))
                    .collect(),
            ),
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => Statement::FunctionCall {
                comp: function_component_ref_from_ast(comp, def_map),
                args: args
                    .iter()
                    .map(|arg| Expression::from_ast_with_def_map(arg, def_map))
                    .collect(),
                outputs: outputs
                    .iter()
                    .map(|output| Expression::from_ast_with_def_map(output, def_map))
                    .collect(),
            },
            ast::Statement::Reinit { variable, value } => Statement::Reinit {
                variable: ComponentReference::from_ast_with_def_map(variable, def_map),
                value: Expression::from_ast_with_def_map(value, def_map),
            },
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => Statement::Assert {
                condition: Expression::from_ast_with_def_map(condition, def_map),
                message: Expression::from_ast_with_def_map(message, def_map),
                level: level
                    .as_ref()
                    .map(|expr| Expression::from_ast_with_def_map(expr, def_map)),
            },
        }
    }
}

impl Expression {
    /// Convert an AST Expression to a Expression.
    ///
    /// This performs the conversion from AST representation to flat representation:
    /// - ComponentReference → VarRef with qualified name
    /// - FunctionCall → BuiltinCall or FunctionCall
    /// - Terminal → Literal
    /// - Parenthesized is unwrapped
    #[cfg(test)]
    pub fn from_ast(expr: &ast::Expression) -> Self {
        match expr {
            ast::Expression::Empty => Expression::Empty,

            ast::Expression::Binary { op, lhs, rhs } => Expression::Binary {
                op: op_binary_from_ast(op),
                lhs: Box::new(Expression::from_ast(lhs)),
                rhs: Box::new(Expression::from_ast(rhs)),
            },

            ast::Expression::Unary { op, rhs } => Expression::Unary {
                op: op_unary_from_ast(op),
                rhs: Box::new(Expression::from_ast(rhs)),
            },

            ast::Expression::ComponentReference(cr) => {
                Self::from_component_ref_with_def_map(cr, None)
            }

            ast::Expression::FunctionCall { comp, args } => convert_function_call(comp, args),

            ast::Expression::Terminal {
                terminal_type,
                token,
            } => Expression::Literal(convert_terminal(terminal_type, token)),

            ast::Expression::If {
                branches,
                else_branch,
            } => Expression::If {
                branches: branches
                    .iter()
                    .map(|(cond, then_expr)| {
                        (Expression::from_ast(cond), Expression::from_ast(then_expr))
                    })
                    .collect(),
                else_branch: Box::new(Expression::from_ast(else_branch)),
            },

            ast::Expression::Array {
                elements,
                is_matrix,
            } => Expression::Array {
                elements: elements.iter().map(Expression::from_ast).collect(),
                is_matrix: *is_matrix,
            },

            ast::Expression::Tuple { elements } => Expression::Tuple {
                elements: elements.iter().map(Expression::from_ast).collect(),
            },

            ast::Expression::Range { start, step, end } => Expression::Range {
                start: Box::new(Expression::from_ast(start)),
                step: step.as_ref().map(|s| Box::new(Expression::from_ast(s))),
                end: Box::new(Expression::from_ast(end)),
            },

            ast::Expression::Parenthesized { inner } => Expression::from_ast(inner),

            ast::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => Expression::ArrayComprehension {
                expr: Box::new(Expression::from_ast(expr)),
                indices: convert_comprehension_indices(indices, None),
                filter: filter
                    .as_ref()
                    .map(|cond| Box::new(Expression::from_ast(cond))),
            },

            ast::Expression::ClassModification {
                target,
                modifications,
            } => {
                let name_parts: Vec<_> =
                    target.parts.iter().map(|p| p.ident.text.clone()).collect();
                Expression::FunctionCall {
                    name: VarName::new(name_parts.join(".")),
                    args: modifications.iter().map(convert_constructor_arg).collect(),
                    is_constructor: true,
                }
            }

            ast::Expression::NamedArgument { value, .. } => Expression::from_ast(value),

            ast::Expression::Modification { value, .. } => Expression::from_ast(value),

            ast::Expression::ArrayIndex { base, subscripts } => {
                // Convert to nested expression with subscripts applied
                // For now, wrap in a special variant - base[subscripts]
                let base_flat = Box::new(Expression::from_ast(base));
                let flat_subs = subscripts.iter().map(Subscript::from_ast).collect();
                Expression::Index {
                    base: base_flat,
                    subscripts: flat_subs,
                }
            }

            ast::Expression::FieldAccess { base, field } => Expression::FieldAccess {
                base: Box::new(Expression::from_ast(base)),
                field: field.clone(),
            },
        }
    }

    /// Convert an AST Expression to a Expression, using def_map for function name resolution.
    ///
    /// This variant uses the def_id from function calls to resolve fully qualified names,
    /// which is essential for correctly looking up user-defined functions that were imported.
    #[cfg(test)]
    pub fn from_ast_with_def_map(
        expr: &ast::Expression,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        match expr {
            ast::Expression::Empty => Expression::Empty,

            ast::Expression::Binary { op, lhs, rhs } => Expression::Binary {
                op: op_binary_from_ast(op),
                lhs: Box::new(Expression::from_ast_with_def_map(lhs, def_map)),
                rhs: Box::new(Expression::from_ast_with_def_map(rhs, def_map)),
            },

            ast::Expression::Unary { op, rhs } => Expression::Unary {
                op: op_unary_from_ast(op),
                rhs: Box::new(Expression::from_ast_with_def_map(rhs, def_map)),
            },

            ast::Expression::ComponentReference(cr) => {
                Self::from_component_ref_with_def_map(cr, def_map)
            }

            ast::Expression::FunctionCall { comp, args } => {
                convert_function_call_with_def_map(comp, args, def_map)
            }

            ast::Expression::Terminal {
                terminal_type,
                token,
            } => Expression::Literal(convert_terminal(terminal_type, token)),

            ast::Expression::If {
                branches,
                else_branch,
            } => convert_if_with_def_map(branches, else_branch, def_map),

            ast::Expression::Array {
                elements,
                is_matrix,
            } => Expression::Array {
                elements: convert_expr_vec_with_def_map(elements, def_map),
                is_matrix: *is_matrix,
            },

            ast::Expression::Tuple { elements } => Expression::Tuple {
                elements: convert_expr_vec_with_def_map(elements, def_map),
            },

            ast::Expression::Range { start, step, end } => Expression::Range {
                start: Box::new(Expression::from_ast_with_def_map(start, def_map)),
                step: step
                    .as_ref()
                    .map(|s| Box::new(Expression::from_ast_with_def_map(s, def_map))),
                end: Box::new(Expression::from_ast_with_def_map(end, def_map)),
            },

            ast::Expression::Parenthesized { inner } => {
                Expression::from_ast_with_def_map(inner, def_map)
            }

            ast::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => convert_array_comprehension_with_def_map(expr, indices, filter, def_map),

            ast::Expression::ClassModification {
                target,
                modifications,
            } => convert_class_modification_with_def_map(target, modifications, def_map),

            ast::Expression::NamedArgument { value, .. } => {
                Expression::from_ast_with_def_map(value, def_map)
            }

            ast::Expression::Modification { value, .. } => {
                Expression::from_ast_with_def_map(value, def_map)
            }

            ast::Expression::ArrayIndex { base, subscripts } => {
                let base_flat = Box::new(Expression::from_ast_with_def_map(base, def_map));
                let flat_subs = subscripts.iter().map(Subscript::from_ast).collect();
                Expression::Index {
                    base: base_flat,
                    subscripts: flat_subs,
                }
            }

            ast::Expression::FieldAccess { base, field } => Expression::FieldAccess {
                base: Box::new(Expression::from_ast_with_def_map(base, def_map)),
                field: field.clone(),
            },
        }
    }

    /// Convert a component reference to a VarRef.
    ///
    /// MLS §10.1: Array subscripts are part of the variable identity.
    /// For `r[1].p.v`, the name is "r[1].p.v" (subscripts included in name).
    #[cfg(test)]
    fn from_component_ref(cr: &ast::ComponentReference) -> Self {
        // Build name with subscripts included for each part
        let name_parts: Vec<String> = cr
            .parts
            .iter()
            .map(|p| {
                let base = p.ident.text.to_string();
                match &p.subs {
                    Some(subs) if !subs.is_empty() => {
                        let sub_strs: Vec<String> = subs.iter().map(subscript_to_string).collect();
                        format!("{}[{}]", base, sub_strs.join(","))
                    }
                    _ => base,
                }
            })
            .collect();
        let name = VarName::new(name_parts.join("."));

        // Subscripts field is for additional trailing subscripts not in the name
        // (typically empty since subscripts are now in the name)
        Expression::VarRef {
            name,
            subscripts: vec![],
        }
    }

    #[cfg(test)]
    fn from_component_ref_with_def_map(
        cr: &ast::ComponentReference,
        def_map: Option<&IndexMap<DefId, String>>,
    ) -> Self {
        from_component_ref_with_def_map_impl(cr, def_map)
    }

    /// Check if this expression contains a call to der().
    ///
    /// Uses the `ContainsDerChecker` visitor with short-circuit evaluation.
    pub fn contains_der(&self) -> bool {
        ContainsDerChecker::check(self)
    }

    /// Check if this expression contains der() applied to a state variable.
    ///
    /// This is used to classify equations:
    /// - `der(state_var) = ...` → ODE equation
    /// - `y = der(input_var)` → algebraic equation (defines y using derivative)
    pub fn contains_der_of_state(&self, state_vars: &std::collections::HashSet<VarName>) -> bool {
        crate::visitor::ContainsDerOfStateChecker::check(self, state_vars)
    }

    /// Extract the variable name from a der() call if this is one.
    pub fn get_der_variable(&self) -> Option<&VarName> {
        match self {
            Expression::BuiltinCall { function, args } if *function == BuiltinFunction::Der => {
                args.first().and_then(|arg| match arg {
                    Expression::VarRef { name, .. } => Some(name),
                    _ => None,
                })
            }
            _ => None,
        }
    }

    /// Collect all state variables (variables passed to der()).
    ///
    /// Uses the `StateVariableCollector` visitor for traversal.
    pub fn collect_state_variables(&self, states: &mut std::collections::HashSet<VarName>) {
        use crate::visitor::{ExpressionVisitor, StateVariableCollector};

        let mut collector = StateVariableCollector::new();
        collector.visit_expression(self);
        states.extend(collector.into_states());
    }

    /// Collect all variable references in this expression.
    ///
    /// Uses the `VarRefCollector` visitor for traversal.
    pub fn collect_var_refs(&self, vars: &mut std::collections::HashSet<VarName>) {
        use crate::visitor::{ExpressionVisitor, VarRefCollector};

        let mut collector = VarRefCollector::new();
        collector.visit_expression(self);
        vars.extend(collector.into_vars());
    }
}

/// Typed origin for equations, replacing free-form string classification.
///
/// Each variant represents a specific equation source, enabling
/// pattern matching instead of `starts_with()` string checks.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EquationOrigin {
    /// Equation from a component instance (e.g., `equation from resistor[1]`).
    ComponentEquation { component: String },
    /// Connection equality equation: `lhs = rhs` (MLS §9.2).
    Connection { lhs: String, rhs: String },
    /// Flow sum equation: `sum of signed flows = 0` (MLS §9.2).
    FlowSum { description: String },
    /// Unconnected flow variable set to zero (MLS §9.2).
    UnconnectedFlow { variable: String },
    /// Algorithm section from a component.
    Algorithm { component: String },
    /// Reinit equation (MLS §8.3.5).
    Reinit { state: String },
    /// When-clause assignment.
    WhenAssignment { target: String },
    /// Binding equation from variable declaration (MLS §4.4.1).
    Binding { variable: String },
}

impl std::fmt::Display for EquationOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EquationOrigin::ComponentEquation { component } => {
                write!(f, "equation from {}", component)
            }
            EquationOrigin::Connection { lhs, rhs } => {
                write!(f, "connection equation: {} = {}", lhs, rhs)
            }
            EquationOrigin::FlowSum { description } => {
                write!(f, "flow sum equation: {}", description)
            }
            EquationOrigin::UnconnectedFlow { variable } => {
                write!(f, "unconnected flow: {} = 0", variable)
            }
            EquationOrigin::Algorithm { component } => {
                write!(f, "algorithm from {}", component)
            }
            EquationOrigin::Reinit { state } => {
                write!(f, "reinit equation for {}", state)
            }
            EquationOrigin::WhenAssignment { target } => {
                write!(f, "when assignment for {}", target)
            }
            EquationOrigin::Binding { variable } => {
                write!(f, "binding equation for {}", variable)
            }
        }
    }
}

impl EquationOrigin {
    /// Check if this origin represents a connection equation.
    pub fn is_connection(&self) -> bool {
        matches!(self, EquationOrigin::Connection { .. })
    }

    /// Check if this origin represents a component equation.
    pub fn is_component_equation(&self) -> bool {
        matches!(self, EquationOrigin::ComponentEquation { .. })
    }

    /// Get the component name if this is a component equation origin.
    pub fn component_name(&self) -> Option<&str> {
        match self {
            EquationOrigin::ComponentEquation { component } => Some(component),
            _ => None,
        }
    }

    /// Get the variable name if this is a binding equation origin.
    pub fn binding_variable(&self) -> Option<&str> {
        match self {
            EquationOrigin::Binding { variable } => Some(variable),
            _ => None,
        }
    }
}

/// Equation in residual form: 0 = residual
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Equation {
    /// The residual expression (equation is: 0 = residual).
    pub residual: Expression,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Typed origin indicating where this equation came from.
    pub origin: EquationOrigin,
    /// Number of scalar equations this represents (MLS §8.4).
    /// For array equations like `x[n] = expr`, this is n.
    /// For scalar equations, this is 1.
    /// Used for balance checking per MLS §4.7.
    #[serde(default = "default_scalar_count")]
    pub scalar_count: usize,
}

/// One expanded iteration inside a preserved for-equation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForEquationIteration {
    /// Concrete index values for this iteration, in declaration order.
    pub index_values: Vec<i64>,
    /// Number of flattened equations produced for this iteration.
    pub equation_count: usize,
}

/// Structured metadata for a flattened `for`-equation (MLS §8.3.3).
///
/// The residual equations remain in `Model::equations` or
/// `Model::initial_equations`; this metadata preserves iteration grouping.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForEquation {
    /// Index variable names in declaration order.
    pub index_names: Vec<String>,
    /// First equation index in the corresponding flat equation vector.
    #[serde(default)]
    pub first_equation_index: usize,
    /// Per-iteration equation counts.
    pub iterations: Vec<ForEquationIteration>,
    /// Source span for diagnostics.
    pub span: Span,
    /// Typed origin for traceability.
    pub origin: EquationOrigin,
}

/// Default scalar count for equations (1 for serde deserialization).
fn default_scalar_count() -> usize {
    1
}

impl Equation {
    /// Create a new flat equation with span information.
    pub fn new(residual: Expression, span: Span, origin: EquationOrigin) -> Self {
        Self {
            residual,
            span,
            origin,
            scalar_count: 1,
        }
    }

    /// Create a new flat equation with explicit scalar count for array equations.
    pub fn new_array(
        residual: Expression,
        span: Span,
        origin: EquationOrigin,
        scalar_count: usize,
    ) -> Self {
        Self {
            residual,
            span,
            origin,
            scalar_count,
        }
    }

    /// Create a flat equation with a dummy span (for testing only).
    #[cfg(test)]
    pub fn new_without_span(residual: Expression, origin: EquationOrigin) -> Self {
        Self {
            residual,
            span: Span::DUMMY,
            origin,
            scalar_count: 1,
        }
    }
}

/// Runtime assertion equation preserved from `equation` / `initial equation` sections.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssertEquation {
    /// Assertion condition expression.
    pub condition: Expression,
    /// Assertion message expression.
    pub message: Expression,
    /// Optional assertion level expression.
    pub level: Option<Expression>,
    /// Source span for diagnostics and traceability.
    pub span: Span,
}

impl AssertEquation {
    /// Create a new flat assertion equation.
    pub fn new(
        condition: Expression,
        message: Expression,
        level: Option<Expression>,
        span: Span,
    ) -> Self {
        Self {
            condition,
            message,
            level,
            span,
        }
    }
}

/// Algorithm section with preserved structure (SPEC_0020).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Algorithm {
    /// The statements in this algorithm section.
    pub statements: Vec<Statement>,
    /// Output variables (left-hand side variables).
    pub outputs: Vec<VarName>,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl Algorithm {
    /// Create a new flat algorithm with span information.
    pub fn new(statements: Vec<Statement>, span: Span, origin: impl Into<String>) -> Self {
        Self {
            statements,
            outputs: Vec::new(),
            span,
            origin: origin.into(),
        }
    }
}

/// External function declaration (MLS §12.9).
///
/// For functions declared with `external` to call C/Fortran code.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
/// A function may have multiple derivative annotations for different orders.
///
/// Example Modelica syntax:
/// ```text
/// annotation(derivative = myFunc_der)
/// annotation(derivative(order=2) = myFunc_der2)
/// annotation(derivative(zeroDerivative=k) = myFunc_der)
/// annotation(derivative(noDerivative=u) = myFunc_der)
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DerivativeAnnotation {
    /// Name of the derivative function.
    pub derivative_function: String,
    /// Derivative order (default is 1).
    pub order: u32,
    /// Input variables whose derivatives are zero (treated as constants).
    /// These inputs don't get additional derivative arguments.
    pub zero_derivative: Vec<String>,
    /// Input variables with no derivative (not differentiated at all).
    /// Similar to zeroDerivative but stronger - the variable is not
    /// considered for differentiation.
    pub no_derivative: Vec<String>,
}

/// A flattened function definition (MLS §12).
///
/// Functions in Modelica are callable units with input/output parameters
/// and an algorithm body. They are compiled separately from the model
/// and called during simulation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    /// Qualified function name (e.g., "Modelica.Math.sin" or "MyPackage.myFunc").
    pub name: VarName,
    /// Input parameters in declaration order.
    pub inputs: Vec<FunctionParam>,
    /// Output parameters in declaration order.
    pub outputs: Vec<FunctionParam>,
    /// Protected local variables.
    pub locals: Vec<FunctionParam>,
    /// The algorithm body (statements).
    pub body: Vec<Statement>,
    /// True if the function is pure (no side effects).
    /// MLS §12.3: Pure functions always return the same result for same inputs.
    pub pure: bool,
    /// External function declaration (MLS §12.9).
    /// If Some, this function calls external C/Fortran code.
    pub external: Option<ExternalFunction>,
    /// Derivative annotations (MLS §12.7.1).
    /// A function may have multiple derivative annotations for different orders.
    pub derivatives: Vec<DerivativeAnnotation>,
    /// Source span for error reporting.
    pub span: Span,
}

/// A function parameter or local variable (MLS §12.1).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionParam {
    /// Parameter name.
    pub name: String,
    /// Type name (e.g., "Real", "Integer", "Boolean").
    pub type_name: String,
    /// Array dimensions (empty for scalars).
    pub dims: Vec<i64>,
    /// Default value expression (for optional inputs).
    pub default: Option<Expression>,
    /// Description string.
    pub description: Option<String>,
}
