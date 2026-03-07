use indexmap::IndexSet;
use rumoca_core::{DefId, Span};
use rumoca_ir_core::{OpBinary, OpUnary};
use serde::{Deserialize, Serialize};

/// Return a component-path base name with all bracketed subscripts removed.
pub fn component_base_name(name: &str) -> Option<String> {
    let mut parts = Vec::new();
    let mut segment_start = 0usize;
    let mut depth = 0i32;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            '.' if depth == 0 => {
                if segment_start >= idx {
                    return None;
                }
                let segment = &name[segment_start..idx];
                let base = segment
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(segment);
                if base.is_empty() {
                    return None;
                }
                parts.push(base.to_string());
                segment_start = idx + 1;
            }
            _ => {}
        }
    }

    if depth != 0 || segment_start >= name.len() {
        return None;
    }
    let tail = &name[segment_start..];
    let base = tail.split_once('[').map(|(base, _)| base).unwrap_or(tail);
    if base.is_empty() {
        return None;
    }
    parts.push(base.to_string());
    Some(parts.join("."))
}

/// A globally unique variable name.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VarName(pub String);

impl VarName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
    },
    VarRef {
        name: VarName,
        subscripts: Vec<Subscript>,
    },
    BuiltinCall {
        function: BuiltinFunction,
        args: Vec<Expression>,
    },
    FunctionCall {
        name: VarName,
        args: Vec<Expression>,
        #[serde(default)]
        is_constructor: bool,
    },
    Literal(Literal),
    If {
        branches: Vec<(Expression, Expression)>,
        else_branch: Box<Expression>,
    },
    Array {
        elements: Vec<Expression>,
        is_matrix: bool,
    },
    Tuple {
        elements: Vec<Expression>,
    },
    Range {
        start: Box<Expression>,
        step: Option<Box<Expression>>,
        end: Box<Expression>,
    },
    ArrayComprehension {
        expr: Box<Expression>,
        indices: Vec<ComprehensionIndex>,
        filter: Option<Box<Expression>>,
    },
    Index {
        base: Box<Expression>,
        subscripts: Vec<Subscript>,
    },
    FieldAccess {
        base: Box<Expression>,
        field: String,
    },
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinFunction {
    Der,
    Pre,
    Abs,
    Sign,
    Sqrt,
    Div,
    Mod,
    Rem,
    Floor,
    Ceil,
    Min,
    Max,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Atan2,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
    Edge,
    Change,
    Reinit,
    Sample,
    Initial,
    Terminal,
    NoEvent,
    Smooth,
    Homotopy,
    SemiLinear,
    Delay,
    Integer,
    Sum,
    Product,
    Ndims,
    Size,
    Scalar,
    Vector,
    Matrix,
    Identity,
    Diagonal,
    Zeros,
    Ones,
    Fill,
    Linspace,
    Transpose,
    OuterProduct,
    Symmetric,
    Cross,
    Skew,
    Cat,
}

impl BuiltinFunction {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "der" => Some(Self::Der),
            "pre" => Some(Self::Pre),
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
            "sin" => Some(Self::Sin),
            "cos" => Some(Self::Cos),
            "tan" => Some(Self::Tan),
            "asin" => Some(Self::Asin),
            "acos" => Some(Self::Acos),
            "atan" => Some(Self::Atan),
            "atan2" => Some(Self::Atan2),
            "sinh" => Some(Self::Sinh),
            "cosh" => Some(Self::Cosh),
            "tanh" => Some(Self::Tanh),
            "exp" => Some(Self::Exp),
            "log" => Some(Self::Log),
            "log10" => Some(Self::Log10),
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
            "sum" => Some(Self::Sum),
            "product" => Some(Self::Product),
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Subscript {
    Index(i64),
    Colon,
    Expr(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComprehensionIndex {
    pub name: String,
    pub range: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Real(f64),
    Integer(i64),
    Boolean(bool),
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

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ComponentRefPart {
    pub ident: String,
    #[serde(default)]
    pub subs: Vec<Subscript>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ComponentReference {
    #[serde(default)]
    pub local: bool,
    pub parts: Vec<ComponentRefPart>,
    #[serde(default)]
    pub def_id: Option<DefId>,
}

impl ComponentReference {
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForIndex {
    pub ident: String,
    pub range: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StatementBlock {
    pub cond: Expression,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
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

pub fn extract_algorithm_outputs(statements: &[Statement]) -> Vec<VarName> {
    let mut outputs = IndexSet::new();
    for stmt in statements {
        collect_algorithm_outputs_from_statement(stmt, &mut outputs);
    }
    outputs.into_iter().collect()
}

pub(crate) fn component_ref_to_base_var_name(comp: &ComponentReference) -> VarName {
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

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ExternalFunction {
    pub language: String,
    pub function_name: Option<String>,
    pub output_name: Option<String>,
    pub arg_names: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct DerivativeAnnotation {
    pub derivative_function: String,
    pub order: u32,
    pub zero_derivative: Vec<String>,
    pub no_derivative: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub name: VarName,
    pub inputs: Vec<FunctionParam>,
    pub outputs: Vec<FunctionParam>,
    pub locals: Vec<FunctionParam>,
    pub body: Vec<Statement>,
    pub pure: bool,
    pub external: Option<ExternalFunction>,
    pub derivatives: Vec<DerivativeAnnotation>,
    pub span: Span,
}

impl Function {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: VarName::new(name),
            inputs: Vec::new(),
            outputs: Vec::new(),
            locals: Vec::new(),
            body: Vec::new(),
            pure: true,
            external: None,
            derivatives: Vec::new(),
            span,
        }
    }

    pub fn add_input(&mut self, param: FunctionParam) {
        self.inputs.push(param);
    }

    pub fn add_output(&mut self, param: FunctionParam) {
        self.outputs.push(param);
    }

    pub fn add_local(&mut self, local: FunctionParam) {
        self.locals.push(local);
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionParam {
    pub name: String,
    pub type_name: String,
    pub dims: Vec<i64>,
    pub default: Option<Expression>,
    pub description: Option<String>,
}

impl FunctionParam {
    pub fn new(name: impl Into<String>, type_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            type_name: type_name.into(),
            dims: Vec::new(),
            default: None,
            description: None,
        }
    }

    pub fn with_dims(mut self, dims: Vec<i64>) -> Self {
        self.dims = dims;
        self
    }

    pub fn with_default(mut self, default: Expression) -> Self {
        self.default = Some(default);
        self
    }
}

impl Expression {
    pub fn contains_der(&self) -> bool {
        match self {
            Expression::BuiltinCall { function, .. } if *function == BuiltinFunction::Der => true,
            _ => self.walk().any(|expr| {
                matches!(
                    expr,
                    Expression::BuiltinCall { function, .. } if *function == BuiltinFunction::Der
                )
            }),
        }
    }

    pub fn contains_der_of_state(&self, state_vars: &std::collections::HashSet<VarName>) -> bool {
        self.walk().any(|expr| {
            if let Expression::BuiltinCall { function, args } = expr
                && *function == BuiltinFunction::Der
                && let Some(Expression::VarRef { name, .. }) = args.first()
            {
                state_vars.contains(name)
            } else {
                false
            }
        })
    }

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

    pub fn collect_state_variables(&self, states: &mut std::collections::HashSet<VarName>) {
        for expr in self.walk() {
            if let Expression::BuiltinCall { function, args } = expr
                && *function == BuiltinFunction::Der
                && let Some(Expression::VarRef { name, .. }) = args.first()
            {
                states.insert(name.clone());
            }
        }
    }

    pub fn collect_var_refs(&self, vars: &mut std::collections::HashSet<VarName>) {
        for expr in self.walk() {
            if let Expression::VarRef { name, .. } = expr {
                vars.insert(name.clone());
            }
        }
    }

    fn walk(&self) -> ExpressionWalker<'_> {
        ExpressionWalker { stack: vec![self] }
    }
}

struct ExpressionWalker<'a> {
    stack: Vec<&'a Expression>,
}

impl<'a> Iterator for ExpressionWalker<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.stack.pop()?;
        match expr {
            Expression::Binary { lhs, rhs, .. } => {
                self.stack.push(rhs);
                self.stack.push(lhs);
            }
            Expression::Unary { rhs, .. } => {
                self.stack.push(rhs);
            }
            Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
                for arg in args.iter().rev() {
                    self.stack.push(arg);
                }
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                self.stack.push(else_branch);
                for (cond, value) in branches.iter().rev() {
                    self.stack.push(value);
                    self.stack.push(cond);
                }
            }
            Expression::Array { elements, .. } | Expression::Tuple { elements } => {
                for element in elements.iter().rev() {
                    self.stack.push(element);
                }
            }
            Expression::Range { start, step, end } => {
                self.stack.push(end);
                if let Some(step) = step {
                    self.stack.push(step);
                }
                self.stack.push(start);
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                if let Some(filter) = filter {
                    self.stack.push(filter);
                }
                for index in indices.iter().rev() {
                    self.stack.push(&index.range);
                }
                self.stack.push(expr);
            }
            Expression::Index { base, subscripts } => {
                for expr in subscripts.iter().rev().filter_map(|sub| match sub {
                    Subscript::Expr(expr) => Some(expr.as_ref()),
                    Subscript::Index(_) | Subscript::Colon => None,
                }) {
                    self.stack.push(expr);
                }
                self.stack.push(base);
            }
            Expression::FieldAccess { base, .. } => {
                self.stack.push(base);
            }
            Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
        }
        Some(expr)
    }
}
