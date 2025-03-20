//! This module defines the Abstract Syntax Tree (AST) and Intermediate Representation (IR) 
//! structures for a custom language or model representation. It provides a comprehensive set 
//! of data structures to represent various components, expressions, equations, and statements 
//! in the language. The module also includes serialization and deserialization support via 
//! `serde` and custom implementations of `Debug` and `Display` traits for better debugging 
//! and formatting.
//!
//! # Key Structures
//!
//! - **Location**: Represents the location of a token or element in the source file, including 
//!   line and column numbers.
//! - **Token**: Represents a lexical token with its text, location, type, and number.
//! - **Name**: Represents a hierarchical name composed of multiple tokens.
//! - **StoredDefinition**: Represents a collection of class definitions and an optional 
//!   "within" clause.
//! - **Component**: Represents a component with its name, type, variability, causality, 
//!   connection, description, and initial value.
//! - **ClassDefinition**: Represents a class definition with its name, components, equations, 
//!   and algorithms.
//! - **ComponentReference**: Represents a reference to a component, including its parts and 
//!   optional subscripts.
//! - **Equation**: Represents various types of equations, such as simple equations, connect 
//!   equations, and conditional equations.
//! - **Expression**: Represents various types of expressions, including binary, unary, 
//!   terminal, and function call expressions.
//! - **Statement**: Represents various types of statements, such as assignments, loops, and 
//!   function calls.
//!
//! # Enums
//!
//! - **OpBinary**: Represents binary operators like addition, subtraction, multiplication, etc.
//! - **OpUnary**: Represents unary operators like negation and logical NOT.
//! - **TerminalType**: Represents the type of a terminal expression, such as real, integer, 
//!   string, or boolean.
//! - **Variability**: Represents the variability of a component (e.g., constant, discrete, 
//!   parameter).
//! - **Connection**: Represents the connection type of a component (e.g., flow, stream).
//! - **Causality**: Represents the causality of a component (e.g., input, output).
//!
//! This module is designed to be extensible and serves as the foundation for parsing, 
//! analyzing, and generating code for the custom language or model representation.
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, fmt::Display};

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub start: u32,
    pub end: u32,
    pub file_name: String,
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct Token {
    pub text: String,
    pub location: Location,
    pub token_number: u32,
    pub token_type: u16,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.text)
    }
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct Name {
    pub name: Vec<Token>,
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{}", s.join("."))
    }
}

impl Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{:?}", s.join("."))
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<Name>,
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct Component {
    pub name: String,
    pub type_name: Name,
    pub variability: Variability,
    pub causality: Causality,
    pub connection: Connection,
    pub description: Vec<Token>,
    pub start: Expression,
    //pub annotation: Option<Token>,
}

impl Debug for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("Component");
        builder
            .field("name", &self.name)
            .field("type_name", &self.type_name);
        if self.variability != Variability::Empty {
            builder.field("variability", &self.variability);
        }
        if self.causality != Causality::Empty {
            builder.field("causality", &self.causality);
        }
        if self.connection != Connection::Empty {
            builder.field("connection", &self.connection);
        }
        if self.description.len() > 0 {
            builder.field("description", &self.description);
        }
        builder.finish()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct ClassDefinition {
    pub name: Token,
    pub encapsulated: bool,
    //pub extends: Vec<Extend>,
    //pub imports: Vec<Import>,
    //pub classes: IndexMap<String, ClassDefinition>,
    pub components: IndexMap<String, Component>,
    pub equations: Vec<Equation>,
    pub initial_equations: Vec<Equation>,
    pub algorithms: Vec<Vec<Statement>>,
    pub initial_algorithms: Vec<Vec<Statement>>,
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct ComponentRefPart {
    pub ident: Token,
    pub subs: Option<Vec<Subscript>>,
}

impl Debug for ComponentRefPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = self.ident.text.clone();
        match &self.subs {
            None => {}
            Some(subs) => {
                let mut v = Vec::new();
                for sub in subs {
                    v.push(format!("{:?}", sub));
                }
                s += &format!("[{:?}]", v.join(", "));
            }
        }
        write!(f, "{}", s)
    }
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct ComponentReference {
    pub local: bool,
    pub parts: Vec<ComponentRefPart>,
}

impl Display for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for part in &self.parts {
            s.push(format!("{:?}", part));
        }
        write!(f, "{:?}", s.join("."))
    }
}

impl Debug for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for part in &self.parts {
            s.push(format!("{:?}", part));
        }
        write!(f, "{:?}", s.join("."))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct EquationBlock {
    pub cond: Expression,
    pub eqs: Vec<Equation>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct StatementBlock {
    pub cond: Expression,
    pub stmts: Vec<Statement>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub struct ForIndex {
    pub ident: Token,
    pub range: Expression,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Equation {
    #[default]
    Empty,
    Simple {
        lhs: Expression,
        rhs: Expression,
    },
    Connect {
        lhs: ComponentReference,
        rhs: ComponentReference,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Equation>,
    },
    When(Vec<EquationBlock>),
    If {
        cond_blocks: Vec<EquationBlock>,
        else_block: Option<Vec<Equation>>,
    },
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
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
    AddElem(Token),
    SubElem(Token),
    MulElem(Token),
    DivElem(Token),
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

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum TerminalType {
    #[default]
    Empty,
    UnsignedReal,
    UnsignedInteger,
    String,
    Bool,
    End,
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Expression {
    #[default]
    Empty,
    Range {
        start: Box<Expression>,
        step: Option<Box<Expression>>,
        end: Box<Expression>,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
    },
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Terminal {
        terminal_type: TerminalType,
        token: Token,
    },
    ComponentReference(ComponentReference),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
    Array {
        elements: Vec<Expression>,
    },
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Empty => write!(f, "Empty"),
            Expression::Range { start, step, end } => f
                .debug_struct("Range")
                .field("start", start)
                .field("step", step)
                .field("end", end)
                .finish(),
            Expression::ComponentReference(comp) => write!(f, "{:?}", comp),
            Expression::FunctionCall { comp, args } => f
                .debug_struct("FunctionCall")
                .field("comp", comp)
                .field("args", args)
                .finish(),
            Expression::Binary { op, lhs, rhs } => f
                .debug_struct(&format!("{:?}", op))
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
            Expression::Unary { op, rhs } => f
                .debug_struct(&format!("{:?}", op))
                .field("rhs", rhs)
                .finish(),
            Expression::Terminal {
                terminal_type,
                token,
            } => write!(f, "{:?}({:?})", terminal_type, token),
            Expression::Array { elements } => f.debug_list().entries(elements.iter()).finish(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Statement {
    #[default]
    Empty,
    Assignment {
        comp: ComponentReference,
        value: Expression,
    },
    Return {
        token: Token,
    },
    Break {
        token: Token,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Statement>,
    },
    While(StatementBlock),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Subscript {
    #[default]
    Empty,
    Expression(Expression),
    Range {
        token: Token,
    },
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Variability {
    #[default]
    Empty,
    Constant(Token),
    Discrete(Token),
    Parameter(Token),
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Connection {
    #[default]
    Empty,
    Flow(Token),
    Stream(Token),
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[allow(unused)]
pub enum Causality {
    #[default]
    Empty,
    Input(Token),
    Output(Token),
}
