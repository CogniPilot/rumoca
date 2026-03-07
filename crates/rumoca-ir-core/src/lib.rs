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
