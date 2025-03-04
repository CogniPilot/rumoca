use indexmap::IndexMap;
use parol_runtime::Location;
use std::fmt::Debug;

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
pub struct Name {
    pub name: Vec<Token>,
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

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<Name>,
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Component {
    pub name: String,
}

#[derive(Debug, Default, Clone)]
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

#[derive(Default, Debug, Clone)]
#[allow(unused)]
pub struct ComponentRefPart {
    pub ident: Token,
    pub subs: Option<Vec<Subscript>>,
}

// impl Debug for ComponentRefPart {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut s = self.ident.text.clone();
//         match &self.subs {
//             None => {}
//             Some(subs) => {
//                 let mut v = Vec::new();
//                 for sub in subs {
//                     v.push(format!("{:?}", sub));
//                 }
//                 s += &format!("[{:?}]", v.join(", "));
//             }
//         }
//         write!(f, "{}", s)
//     }
// }

#[derive(Default, Debug, Clone)]
#[allow(unused)]
pub struct ComponentReference {
    pub local: bool,
    pub parts: Vec<ComponentRefPart>,
}

// impl Debug for ComponentReference {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut s = Vec::new();
//         for part in &self.parts {
//             s.push(format!("{:?}", part));
//         }
//         write!(f, "{:?}", s.join("."))
//     }
// }

#[derive(Debug, Default, Clone)]
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
        index: Token,
        range: Expression,
        equations: Vec<Equation>,
    },
    When {
        condition: Expression,
        equations: Vec<Equation>,
    },
}

#[derive(Debug, Default, Clone)]
pub enum OpBinary {
    #[default]
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Exp,
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
    UnsignedReal {
        value: Token,
    },
    UnsignedInteger {
        value: Token,
    },
    String {
        value: Token,
    },
    Bool {
        value: Token,
    },
    End {
        value: Token,
    },
    ComponentReference(ComponentReference),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Default, Clone)]
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
}

#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Subscript {
    #[default]
    Empty,
    Expression(Expression),
    Range {
        token: Token,
    },
}
