// galec.rs, specifies the structure of the Galec intermediate representation

use serde::Serialize;

#[derive(Serialize)]
pub struct Galec {
    pub name: String,
    pub period: f64,
    pub variables: Vec<GalecVariable>,
    pub startup: Vec<GalecStatement>,
    pub recalibrate: Vec<GalecStatement>,
    pub do_step: Vec<GalecGroup>,
}

#[derive(Serialize)]
pub struct GalecVariable {
    pub name: String,
    pub role: DataRole,
    pub ty: GalecLiteral,
    pub dims: Vec<usize>,
    pub start: Option<GalecExpr>,
}

#[derive(Serialize)]
pub enum DataRole {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    State,
}

#[derive(Serialize)]
pub enum GalecStatement {
    Assign { lhs: String, rhs: GalecExpr },
}

#[derive(Serialize)]
pub struct GalecGroup {
    pub condition: Option<GalecExpr>,
    pub statements: Vec<GalecStatement>,
}

#[derive(Serialize)]
pub enum GalecExpr {
    Literal(GalecLiteral),
    SelfRef {
        name: String,
    },
    PreviousRef {
        name: String,
    },
    Binary {
        op: GalecBinaryOp,
        lhs: Box<GalecExpr>,
        rhs: Box<GalecExpr>,
    },
    Negate(Box<GalecExpr>),
    Call {
        name: String,
        args: Vec<GalecExpr>,
    },
    If {
        condition: Box<GalecExpr>,
        then_branch: Box<GalecExpr>,
        else_branch: Box<GalecExpr>,
    },
    Index {
        base: Box<GalecExpr>,
        subscripts: Vec<GalecExpr>,
    },
}

#[derive(Serialize)]
pub enum GalecLiteral {
    Real(f64),
    Integer(i64),
    Boolean(bool),
}

#[derive(Serialize)]
pub enum GalecBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}
