use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecAlgorithmPackage {
    pub model: GalecModel,
    pub manifest: GalecManifest,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecManifest {
    pub clock: GalecSampleTime,
    pub variables: Vec<GalecVariable>,
    pub methods: Vec<GalecMethod>,
    pub error_signal_status_id: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecMethod {
    pub kind: GalecMethodKind,
    pub name: String,
    pub block: GalecBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum GalecMethodKind {
    Startup,
    Recalibrate,
    DoStep,
}

impl GalecModel {
    pub fn methods(&self) -> Vec<GalecMethod> {
        vec![
            GalecMethod {
                kind: GalecMethodKind::Startup,
                name: "Startup".to_string(),
                block: self.init.clone(),
            },
            GalecMethod {
                kind: GalecMethodKind::Recalibrate,
                name: "Recalibrate".to_string(),
                block: self.recalibrate.clone(),
            },
            GalecMethod {
                kind: GalecMethodKind::DoStep,
                name: "DoStep".to_string(),
                block: self.step.clone(),
            },
        ]
    }

    pub fn manifest_variables(&self) -> Vec<GalecVariable> {
        self.interface
            .inputs
            .iter()
            .chain(self.interface.outputs.iter())
            .chain(self.interface.parameters.iter())
            .chain(self.interface.states.iter())
            .cloned()
            .collect()
    }

    pub fn algorithm_package(&self) -> GalecAlgorithmPackage {
        GalecAlgorithmPackage {
            model: self.clone(),
            manifest: GalecManifest {
                clock: self.sample_time.clone(),
                variables: self.manifest_variables(),
                methods: self.methods(),
                error_signal_status_id: "errorSignalStatus".to_string(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecModel {
    pub name: String,
    pub interface: GalecInterface,
    pub sample_time: GalecSampleTime,
    pub declarations: Vec<GalecDecl>,
    pub init: GalecBlock,
    pub recalibrate: GalecBlock,
    pub step: GalecBlock,
}

#[derive(Debug, Clone, PartialEq, Default, Serialize)]
pub struct GalecInterface {
    pub inputs: Vec<GalecVariable>,
    pub outputs: Vec<GalecVariable>,
    pub parameters: Vec<GalecVariable>,
    pub states: Vec<GalecVariable>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecDecl {
    pub variable: GalecVariable,
    pub role: GalecDeclRole,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum GalecDeclRole {
    Temporary,
    Local,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecVariable {
    pub name: String,
    pub ty: GalecType,
    pub role: GalecVariableRole,
    pub description: Option<String>,
    pub start: Option<GalecExpr>,
    pub min: Option<GalecExpr>,
    pub max: Option<GalecExpr>,
    pub nominal: Option<GalecExpr>,
    pub unit: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum GalecVariableRole {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    State,
    Local,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum GalecType {
    Real,
    Integer,
    Boolean,
    Array {
        element: Box<GalecType>,
        dims: Vec<usize>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GalecSampleTime {
    pub period_seconds: f64,
    pub variable_name: String,
}

#[derive(Debug, Clone, PartialEq, Default, Serialize)]
pub struct GalecBlock {
    pub statements: Vec<GalecStmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum GalecStmt {
    Assign {
        lhs: String,
        rhs: GalecExpr,
    },
    If {
        branches: Vec<(GalecExpr, Vec<GalecStmt>)>,
        else_branch: Vec<GalecStmt>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum GalecExpr {
    // primitives
    RealLiteral(f64),
    IntegerLiteral(i64),
    BooleanLiteral(bool),

    // variable name
    Variable(String),

    // binary arithmetic ops
    Add(Box<GalecExpr>, Box<GalecExpr>),
    Sub(Box<GalecExpr>, Box<GalecExpr>),
    Mul(Box<GalecExpr>, Box<GalecExpr>),
    Div(Box<GalecExpr>, Box<GalecExpr>),

    // binary logical ops
    Eq(Box<GalecExpr>, Box<GalecExpr>),
    Neq(Box<GalecExpr>, Box<GalecExpr>),
    Lt(Box<GalecExpr>, Box<GalecExpr>),
    Le(Box<GalecExpr>, Box<GalecExpr>),
    Gt(Box<GalecExpr>, Box<GalecExpr>),
    Ge(Box<GalecExpr>, Box<GalecExpr>),
    And(Box<GalecExpr>, Box<GalecExpr>),
    Or(Box<GalecExpr>, Box<GalecExpr>),
    Neg(Box<GalecExpr>),
    Not(Box<GalecExpr>),

    // control flow
    If {
        branches: Vec<(GalecExpr, GalecExpr)>,
        else_expr: Box<GalecExpr>,
    },
}
