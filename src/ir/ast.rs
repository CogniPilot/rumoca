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

pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<Name>,
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct Component {
    pub name: String,
    /// The token for the component name with exact source location
    pub name_token: Token,
    pub type_name: Name,
    pub variability: Variability,
    pub causality: Causality,
    pub connection: Connection,
    pub description: Vec<Token>,
    pub start: Expression,
    /// True if start value is from a modification (start=x), false if from binding (= x)
    pub start_is_modification: bool,
    /// True if the start modifier has `each` prefix (for array components)
    pub start_has_each: bool,
    /// Array dimensions - empty for scalars, e.g., [2, 3] for a 2x3 matrix
    /// Only populated when dimensions are known literal integers
    pub shape: Vec<usize>,
    /// Raw array dimension subscripts (e.g., `n` in `Real x[n]` or `:` in `Real a[:]`)
    /// Used for parameter-dependent dimensions that need runtime evaluation
    /// Can be Subscript::Expression for explicit dimensions or Subscript::Range for `:`
    pub shape_expr: Vec<Subscript>,
    /// True if shape is from a modification (shape=x), false if from subscript \[x\]
    pub shape_is_modification: bool,
    /// Annotation arguments (e.g., from `annotation(Icon(...), Dialog(...))`)
    pub annotation: Vec<Expression>,
    /// Component modifications (e.g., R=10 in `Resistor R1(R=10)`)
    /// Maps parameter name to its modified value expression
    pub modifications: IndexMap<String, Expression>,
    /// Full source location for the component declaration
    pub location: Location,
    /// Conditional component expression (e.g., `if use_reset` in `BooleanInput reset if use_reset`)
    /// None means the component is unconditional (always present)
    pub condition: Option<Expression>,
    /// True if declared with 'inner' prefix (provides instance to outer references)
    pub inner: bool,
    /// True if declared with 'outer' prefix (references an inner instance from enclosing scope)
    pub outer: bool,
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
        if !self.description.is_empty() {
            builder.field("description", &self.description);
        }
        if !self.shape.is_empty() {
            builder.field("shape", &self.shape);
        }
        if !self.shape_expr.is_empty() {
            builder.field("shape_expr", &self.shape_expr);
        }
        if !self.annotation.is_empty() {
            builder.field("annotation", &self.annotation);
        }
        if !self.modifications.is_empty() {
            builder.field("modifications", &self.modifications);
        }
        if self.condition.is_some() {
            builder.field("condition", &self.condition);
        }
        if self.inner {
            builder.field("inner", &self.inner);
        }
        if self.outer {
            builder.field("outer", &self.outer);
        }
        builder.finish()
    }
}

/// Type of class (model, function, connector, etc.)
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

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct ClassDefinition {
    pub name: Token,
    pub class_type: ClassType,
    /// Token for the class type keyword (model, class, function, etc.)
    pub class_type_token: Token,
    pub encapsulated: bool,
    /// True if the class is declared with the `partial` keyword
    pub partial: bool,
    /// Causality from type alias definition (e.g., `connector RealInput = input Real`)
    /// Components of this type inherit this causality
    pub causality: Causality,
    /// Description string for this class (e.g., "A test model")
    pub description: Vec<Token>,
    /// Full source location spanning from class keyword to end statement
    pub location: Location,
    pub extends: Vec<Extend>,
    pub imports: Vec<Import>,
    /// Nested class definitions (functions, models, packages, etc.)
    pub classes: IndexMap<String, ClassDefinition>,
    pub components: IndexMap<String, Component>,
    pub equations: Vec<Equation>,
    pub initial_equations: Vec<Equation>,
    pub algorithms: Vec<Vec<Statement>>,
    pub initial_algorithms: Vec<Vec<Statement>>,
    /// Token for "equation" keyword (if present)
    pub equation_keyword: Option<Token>,
    /// Token for "initial equation" keyword (if present)
    pub initial_equation_keyword: Option<Token>,
    /// Token for "algorithm" keyword (if present)
    pub algorithm_keyword: Option<Token>,
    /// Token for "initial algorithm" keyword (if present)
    pub initial_algorithm_keyword: Option<Token>,
    /// Token for the class name in "end ClassName;" (for rename support)
    pub end_name_token: Option<Token>,
    /// Enumeration literals for enum types (e.g., `type MyEnum = enumeration(A, B, C)`)
    pub enum_literals: Vec<Token>,
    /// Annotation clause for this class (e.g., Documentation, Icon, Diagram)
    pub annotation: Vec<Expression>,
}

impl ClassDefinition {
    /// Iterate over all component declarations with their names.
    ///
    /// This provides a convenient way to iterate over components without
    /// directly accessing the `components` field.
    pub fn iter_components(&self) -> impl Iterator<Item = (&str, &Component)> {
        self.components
            .iter()
            .map(|(name, comp)| (name.as_str(), comp))
    }

    /// Iterate over all nested class definitions with their names.
    ///
    /// This includes functions, models, packages, types, etc. defined within this class.
    pub fn iter_classes(&self) -> impl Iterator<Item = (&str, &ClassDefinition)> {
        self.classes
            .iter()
            .map(|(name, class)| (name.as_str(), class))
    }

    /// Iterate over all equations (regular + initial).
    ///
    /// This chains `equations` and `initial_equations` into a single iterator.
    pub fn iter_all_equations(&self) -> impl Iterator<Item = &Equation> {
        self.equations.iter().chain(self.initial_equations.iter())
    }

    /// Iterate over all statements from all algorithm sections (regular + initial).
    ///
    /// This flattens all algorithm blocks and chains regular and initial algorithms.
    pub fn iter_all_statements(&self) -> impl Iterator<Item = &Statement> {
        self.algorithms
            .iter()
            .flatten()
            .chain(self.initial_algorithms.iter().flatten())
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct Extend {
    pub comp: Name,
    /// Source location of the extends clause
    pub location: Location,
    /// Modifications applied to the extends clause (e.g., extends Foo(bar=1))
    pub modifications: Vec<Expression>,
}

/// Import clause for bringing names into scope
/// Modelica supports several import styles:
/// - `import A.B.C;` - qualified import (use as C)
/// - `import D = A.B.C;` - renamed import (use as D)
/// - `import A.B.*;` - unqualified import (all names from A.B)
/// - `import A.B.{C, D, E};` - selective import (specific names)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Import {
    /// Qualified import: `import A.B.C;` - imports C, accessed as C
    Qualified { path: Name, location: Location },
    /// Renamed import: `import D = A.B.C;` - imports C, accessed as D
    Renamed {
        alias: Token,
        path: Name,
        location: Location,
    },
    /// Unqualified import: `import A.B.*;` - imports all from A.B
    Unqualified { path: Name, location: Location },
    /// Selective import: `import A.B.{C, D};` - imports specific names
    Selective {
        path: Name,
        names: Vec<Token>,
        location: Location,
    },
}

impl Import {
    /// Get the base path for this import
    pub fn base_path(&self) -> &Name {
        match self {
            Import::Qualified { path, .. } => path,
            Import::Renamed { path, .. } => path,
            Import::Unqualified { path, .. } => path,
            Import::Selective { path, .. } => path,
        }
    }

    /// Get the source location of this import
    pub fn location(&self) -> &Location {
        match self {
            Import::Qualified { location, .. } => location,
            Import::Renamed { location, .. } => location,
            Import::Unqualified { location, .. } => location,
            Import::Selective { location, .. } => location,
        }
    }
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]

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
        write!(f, "{}", s.join("."))
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

impl ComponentReference {
    /// Get the source location of the first token in this component reference.
    pub fn get_location(&self) -> Option<&Location> {
        self.parts.first().map(|part| &part.ident.location)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct EquationBlock {
    pub cond: Expression,
    pub eqs: Vec<Equation>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct StatementBlock {
    pub cond: Expression,
    pub stmts: Vec<Statement>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct ForIndex {
    pub ident: Token,
    pub range: Expression,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

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

impl Equation {
    /// Get the source location of the first token in this equation.
    /// Returns None for Empty equations.
    pub fn get_location(&self) -> Option<&Location> {
        match self {
            Equation::Empty => None,
            Equation::Simple { lhs, .. } => lhs.get_location(),
            Equation::Connect { lhs, .. } => lhs.get_location(),
            Equation::For { indices, .. } => indices.first().map(|i| &i.ident.location),
            Equation::When(blocks) => blocks.first().and_then(|b| b.cond.get_location()),
            Equation::If { cond_blocks, .. } => {
                cond_blocks.first().and_then(|b| b.cond.get_location())
            }
            Equation::FunctionCall { comp, .. } => comp.get_location(),
        }
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
    AddElem(Token),
    SubElem(Token),
    MulElem(Token),
    DivElem(Token),
    /// Assignment/modification operator `=` (not equality `==`)
    /// Used in named arguments and modifications like `annotation(HideResult=true)`
    Assign(Token),
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
    /// Tuple expression for multi-output function calls: (a, b) = func()
    Tuple {
        elements: Vec<Expression>,
    },
    /// If expression: if cond then expr elseif cond2 then expr2 else expr3
    If {
        /// List of (condition, expression) pairs for if and elseif branches
        branches: Vec<(Expression, Expression)>,
        /// The else branch expression
        else_branch: Box<Expression>,
    },
    /// Parenthesized expression to preserve explicit parentheses from source
    Parenthesized {
        inner: Box<Expression>,
    },
    /// Array comprehension: {expr for i in range}
    ArrayComprehension {
        expr: Box<Expression>,
        indices: Vec<ForIndex>,
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
            Expression::Tuple { elements } => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", e)?;
                }
                write!(f, ")")
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                write!(f, "if ")?;
                for (i, (cond, expr)) in branches.iter().enumerate() {
                    if i > 0 {
                        write!(f, " elseif ")?;
                    }
                    write!(f, "{:?} then {:?}", cond, expr)?;
                }
                write!(f, " else {:?}", else_branch)
            }
            Expression::Parenthesized { inner } => {
                write!(f, "({:?})", inner)
            }
            Expression::ArrayComprehension { expr, indices } => {
                write!(f, "{{{{ {:?} for {:?} }}}}", expr, indices)
            }
        }
    }
}

impl Expression {
    /// Get the source location of the first token in this expression.
    /// Returns None for Empty expressions.
    pub fn get_location(&self) -> Option<&Location> {
        match self {
            Expression::Empty => None,
            Expression::Range { start, .. } => start.get_location(),
            Expression::Unary { rhs, .. } => rhs.get_location(),
            Expression::Binary { lhs, .. } => lhs.get_location(),
            Expression::Terminal { token, .. } => Some(&token.location),
            Expression::ComponentReference(comp) => {
                comp.parts.first().map(|part| &part.ident.location)
            }
            Expression::FunctionCall { comp, .. } => {
                comp.parts.first().map(|part| &part.ident.location)
            }
            Expression::Array { elements } => elements.first().and_then(|e| e.get_location()),
            Expression::Tuple { elements } => elements.first().and_then(|e| e.get_location()),
            Expression::If { branches, .. } => {
                branches.first().and_then(|(cond, _)| cond.get_location())
            }
            Expression::Parenthesized { inner } => inner.get_location(),
            Expression::ArrayComprehension { expr, .. } => expr.get_location(),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Empty => write!(f, ""),
            Expression::Range { start, step, end } => {
                if let Some(s) = step {
                    write!(f, "{}:{}:{}", start, s, end)
                } else {
                    write!(f, "{}:{}", start, end)
                }
            }
            Expression::Unary { op, rhs } => {
                let op_str = match op {
                    OpUnary::Minus(_) => "-",
                    OpUnary::Plus(_) => "+",
                    OpUnary::DotMinus(_) => ".-",
                    OpUnary::DotPlus(_) => ".+",
                    OpUnary::Not(_) => "not ",
                    OpUnary::Empty => "",
                };
                write!(f, "{}{}", op_str, rhs)
            }
            Expression::Binary { op, lhs, rhs } => {
                let op_str = match op {
                    OpBinary::Add(_) => "+",
                    OpBinary::Sub(_) => "-",
                    OpBinary::Mul(_) => "*",
                    OpBinary::Div(_) => "/",
                    OpBinary::Eq(_) => "==",
                    OpBinary::Neq(_) => "<>",
                    OpBinary::Lt(_) => "<",
                    OpBinary::Le(_) => "<=",
                    OpBinary::Gt(_) => ">",
                    OpBinary::Ge(_) => ">=",
                    OpBinary::And(_) => "and",
                    OpBinary::Or(_) => "or",
                    OpBinary::Exp(_) => "^",
                    OpBinary::AddElem(_) => ".+",
                    OpBinary::SubElem(_) => ".-",
                    OpBinary::MulElem(_) => ".*",
                    OpBinary::DivElem(_) => "./",
                    OpBinary::Assign(_) => "=",
                    OpBinary::Empty => "?",
                };
                write!(f, "{} {} {}", lhs, op_str, rhs)
            }
            Expression::Terminal {
                terminal_type,
                token,
            } => match terminal_type {
                TerminalType::String => write!(f, "\"{}\"", token.text),
                TerminalType::Bool => write!(f, "{}", token.text),
                _ => write!(f, "{}", token.text),
            },
            Expression::ComponentReference(comp) => write!(f, "{}", comp),
            Expression::FunctionCall { comp, args } => {
                write!(f, "{}(", comp)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expression::Array { elements } => {
                write!(f, "{{")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, "}}")
            }
            Expression::Tuple { elements } => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                write!(f, "if ")?;
                for (i, (cond, expr)) in branches.iter().enumerate() {
                    if i > 0 {
                        write!(f, " elseif ")?;
                    }
                    write!(f, "{} then {}", cond, expr)?;
                }
                write!(f, " else {}", else_branch)
            }
            Expression::Parenthesized { inner } => write!(f, "({})", inner),
            Expression::ArrayComprehension { expr, indices } => {
                write!(f, "{{ {} for ", expr)?;
                for (i, idx) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} in {}", idx.ident.text, idx.range)?;
                }
                write!(f, " }}")
            }
        }
    }
}

impl std::fmt::Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Equation::Empty => write!(f, ""),
            Equation::Simple { lhs, rhs } => write!(f, "{} = {}", lhs, rhs),
            Equation::Connect { lhs, rhs } => write!(f, "connect({}, {})", lhs, rhs),
            Equation::For { indices, equations } => {
                write!(f, "for ")?;
                for (i, idx) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} in {}", idx.ident.text, idx.range)?;
                }
                writeln!(f, " loop")?;
                for eq in equations {
                    writeln!(f, "  {};", eq)?;
                }
                write!(f, "end for")
            }
            Equation::When(blocks) => {
                for (i, block) in blocks.iter().enumerate() {
                    if i == 0 {
                        write!(f, "when {} then", block.cond)?;
                    } else {
                        write!(f, " elsewhen {} then", block.cond)?;
                    }
                    for eq in &block.eqs {
                        write!(f, " {};", eq)?;
                    }
                }
                write!(f, " end when")
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for (i, block) in cond_blocks.iter().enumerate() {
                    if i == 0 {
                        write!(f, "if {} then", block.cond)?;
                    } else {
                        write!(f, " elseif {} then", block.cond)?;
                    }
                    for eq in &block.eqs {
                        write!(f, " {};", eq)?;
                    }
                }
                if let Some(else_eqs) = else_block {
                    write!(f, " else")?;
                    for eq in else_eqs {
                        write!(f, " {};", eq)?;
                    }
                }
                write!(f, " end if")
            }
            Equation::FunctionCall { comp, args } => {
                write!(f, "{}(", comp)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

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
    /// If statement: if cond then stmts elseif cond2 then stmts2 else stmts3
    If {
        cond_blocks: Vec<StatementBlock>,
        else_block: Option<Vec<Statement>>,
    },
    /// When statement: when cond then stmts elsewhen cond2 then stmts2
    When(Vec<StatementBlock>),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Subscript {
    #[default]
    Empty,
    Expression(Expression),
    Range {
        token: Token,
    },
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

pub enum Connection {
    #[default]
    Empty,
    Flow(Token),
    Stream(Token),
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Causality {
    #[default]
    Empty,
    Input(Token),
    Output(Token),
}
