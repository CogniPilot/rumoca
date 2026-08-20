//! The slice-1 model: flat scalar variables and flat scalar equations.
//!
//! There is no parser and there will not be one. A reference model is written
//! as a Rust value beside the Modelica text it mirrors, and the differential
//! harness checks that the two agree. Writing both by hand is the point: two
//! independent renderings of one model disagree loudly, where a shared parser
//! would make them agree by construction and prove nothing.

use crate::value::{Type, Value};

/// A binary operator slice 1 admits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Numeric addition.
    Add,
    /// Numeric subtraction.
    Sub,
    /// Numeric multiplication.
    Mul,
    /// Numeric division.
    Div,
    /// `<` on numbers.
    Less,
    /// `<=` on numbers.
    LessEqual,
    /// `>` on numbers.
    Greater,
    /// `>=` on numbers.
    GreaterEqual,
    /// `==`.
    Equal,
    /// `<>`.
    NotEqual,
    /// `and` on booleans.
    And,
    /// `or` on booleans.
    Or,
}

/// A slice-1 scalar expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A literal.
    Literal(Value),
    /// A variable reference.
    Variable(String),
    /// MLS §3.7.5 `pre(v)`: the left limit of `v` at the current event.
    Pre(String),
    /// MLS §3.7.5 `edge(b)`: `b and not pre(b)`, defined only on `Boolean`.
    Edge(String),
    /// MLS §3.7.5 `initial()`: true during initialization and nowhere else.
    Initial,
    /// MLS §3.6.7 `time`.
    Time,
    /// `not e`.
    Not(Box<Expr>),
    /// `-e`.
    Negate(Box<Expr>),
    /// A binary operation.
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    /// `if c then a else b`.
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    /// A `Real` literal.
    #[must_use]
    pub fn real(value: f64) -> Self {
        Expr::Literal(Value::Real(value))
    }

    /// An `Integer` literal.
    #[must_use]
    pub fn integer(value: i64) -> Self {
        Expr::Literal(Value::Integer(value))
    }

    /// A `Boolean` literal.
    #[must_use]
    pub fn boolean(value: bool) -> Self {
        Expr::Literal(Value::Boolean(value))
    }

    /// A variable reference.
    #[must_use]
    pub fn var(name: &str) -> Self {
        Expr::Variable(name.to_owned())
    }

    /// `pre(name)`.
    #[must_use]
    pub fn pre(name: &str) -> Self {
        Expr::Pre(name.to_owned())
    }

    /// A binary operation.
    #[must_use]
    pub fn binary(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Expr::Binary(op, Box::new(lhs), Box::new(rhs))
    }

    /// `if condition then consequent else alternative`.
    #[must_use]
    pub fn if_then_else(condition: Expr, consequent: Expr, alternative: Expr) -> Self {
        Expr::If(
            Box::new(condition),
            Box::new(consequent),
            Box::new(alternative),
        )
    }
}

/// How a variable's value is produced.
///
/// Slice 1 does not integrate, so a `Continuous` variable is not solved for: it
/// is read from the externally supplied trajectory. See the crate header on why
/// integration is deliberately outside the reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variability {
    /// MLS §4.5 `parameter`: fixed for the whole run.
    Parameter,
    /// MLS §4.5 discrete-time: changes only at events.
    Discrete,
    /// MLS §4.5 continuous-time: supplied by the host trajectory in slice 1.
    Continuous,
}

/// A flat scalar variable declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    /// The flat name.
    pub name: String,
    /// The declared type.
    pub ty: Type,
    /// The declared variability.
    pub variability: Variability,
    /// The `start` attribute expression (MLS §4.4.2.1).
    ///
    /// Slice 1 requires this to be evaluable without reading another variable,
    /// so the start environment is a single pass and not a fixed point. The
    /// interpreter rejects anything else rather than guessing an order.
    pub start: Expr,
}

impl Variable {
    /// A discrete variable with a literal start value.
    #[must_use]
    pub fn discrete(name: &str, start: Value) -> Self {
        Self {
            name: name.to_owned(),
            ty: start.type_of(),
            variability: Variability::Discrete,
            start: Expr::Literal(start),
        }
    }

    /// A continuous variable whose values come from the supplied trajectory.
    #[must_use]
    pub fn continuous(name: &str, start: f64) -> Self {
        Self {
            name: name.to_owned(),
            ty: Type::Real,
            variability: Variability::Continuous,
            start: Expr::real(start),
        }
    }

    /// A parameter with a literal value.
    #[must_use]
    pub fn parameter(name: &str, start: Value) -> Self {
        Self {
            name: name.to_owned(),
            ty: start.type_of(),
            variability: Variability::Parameter,
            start: Expr::Literal(start),
        }
    }
}

/// One `when` or `elsewhen` branch.
#[derive(Debug, Clone, PartialEq)]
pub struct WhenBranch {
    /// The activation condition.
    pub condition: Expr,
    /// The assignments the branch makes, in source order.
    pub body: Vec<(String, Expr)>,
}

impl WhenBranch {
    /// A branch assigning one variable.
    #[must_use]
    pub fn assigning(condition: Expr, target: &str, value: Expr) -> Self {
        Self {
            condition,
            body: vec![(target.to_owned(), value)],
        }
    }
}

/// A flat scalar equation.
#[derive(Debug, Clone, PartialEq)]
pub enum Equation {
    /// `target = value`, already in the almost-solved form MLS Appendix B.1c
    /// requires of discrete-valued equations. Slice 1 does not solve implicit
    /// systems; it iterates assignments to a fixed point.
    Assign {
        /// The variable the equation determines.
        target: String,
        /// The right-hand side.
        value: Expr,
    },
    /// `when .. then .. elsewhen .. then .. end when`, in branch order.
    ///
    /// Branch order is priority order (MLS §8.3.5): the first branch with a
    /// rising edge wins.
    When(Vec<WhenBranch>),
    /// `initial equation` clause: active only during initialization (MLS §8.6).
    InitialAssign {
        /// The variable the equation determines.
        target: String,
        /// The right-hand side.
        value: Expr,
    },
}

/// A flat slice-1 model.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Model {
    /// Declared variables, in declaration order.
    pub variables: Vec<Variable>,
    /// Equations, in source order.
    pub equations: Vec<Equation>,
}

impl Model {
    /// An empty model.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a variable.
    #[must_use]
    pub fn with_variable(mut self, variable: Variable) -> Self {
        self.variables.push(variable);
        self
    }

    /// Adds an equation.
    #[must_use]
    pub fn with_equation(mut self, equation: Equation) -> Self {
        self.equations.push(equation);
        self
    }

    /// The declaration for `name`, if the model declares one.
    #[must_use]
    pub fn variable(&self, name: &str) -> Option<&Variable> {
        self.variables.iter().find(|entry| entry.name == name)
    }
}
