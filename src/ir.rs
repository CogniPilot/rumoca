use indexmap::IndexMap;
use parol_runtime::Location;
use std::fmt::Debug;
use std::sync::Mutex;

#[derive(Default, Clone)]
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

#[derive(Default, Clone)]
#[allow(unused)]
pub struct NodeData {
    pub id: usize,
}

static ID_COUNTER: Mutex<usize> = Mutex::new(0);

impl NodeData {
    pub fn new() -> Self {
        let mut counter = ID_COUNTER.lock().unwrap();
        *counter += 1;
        NodeData { id: *counter }
    }
}

impl Debug for NodeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

#[derive(Default, Clone)]
#[allow(unused)]
pub struct Name {
    pub name: Vec<Token>,
    pub node: NodeData,
}

impl Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{:?}", s.join(""))
    }
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<Name>,
    pub node: NodeData,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassDefinition {
    pub name: Token,
    pub encapsulated: bool,
    pub equations: Vec<Equation>,
    pub node: NodeData,
}

#[derive(Default, Clone)]
#[allow(unused)]
pub struct ComponentReference {
    pub name: Vec<Token>,
    pub node: NodeData,
}

impl Debug for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{:?}", s.join(""))
    }
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Equation {
    #[default]
    Empty,
    Simple {
        lhs: Expression,
        rhs: Expression,
        node: NodeData,
    },
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
        node: NodeData,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
        node: NodeData,
    },
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        node: NodeData,
    },
    UnsignedReal {
        value: Token,
        node: NodeData,
    },
    UnsignedInteger {
        value: Token,
        node: NodeData,
    },
    ComponentReference(ComponentReference),
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Statement {
    #[default]
    Empty,
    Assignment {
        comp: ComponentReference,
        value: Expression,
        node: NodeData,
    },
}
