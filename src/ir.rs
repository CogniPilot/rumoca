use indexmap::IndexMap;
use parol_runtime::Span;

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Name {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<Name>,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassDefinition {
    pub name: String,
    pub encapsulated: bool,
    pub equations: Vec<Equation>,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
pub struct ComponentReference {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Equation {
    #[default]
    Empty,
    Assignment {
        lhs: Expression,
        rhs: Expression,
        span: Span,
    },
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct EquationSection {
    pub private: bool,
    pub equations: Vec<Equation>,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
pub enum OpBinary {
    #[default]
    Add,
    Sub,
    Mul,
    Div,
    // Pow,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    // Exp,
    AddElem,
    SubElem,
    MulElem,
    DivElem,
}

#[derive(Debug, Default, Clone)]
pub enum OpUnary {
    #[default]
    Plus,
    // Minus,
    Not,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Expression {
    #[default]
    Empty,
    Range {
        start: Box<Expression>,
        step: Option<Box<Expression>>,
        end: Box<Expression>,
        span: Span,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
        span: Span,
    },
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
    },
    UnsignedReal {
        value: String,
        span: Span,
    },
    UnsignedInteger {
        value: String,
        span: Span,
    },
    ComponentReference {
        comp: String,
        span: Span,
    },
}
