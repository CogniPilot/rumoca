use rumoca_core::{DefId, Span};
pub use rumoca_ir_core::BuiltinFunction;
pub use rumoca_ir_core::DerivativeAnnotation;
pub use rumoca_ir_core::ExternalFunction;
pub use rumoca_ir_core::Literal;
pub use rumoca_ir_core::VarName;
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
    use crate::visitor::{AlgorithmOutputCollector, StatementVisitor};

    let mut collector = AlgorithmOutputCollector::new();
    for statement in statements {
        collector.visit_statement(statement);
    }
    collector.into_outputs()
}

pub(crate) fn component_ref_to_base_var_name(comp: &ComponentReference) -> VarName {
    let parts: Vec<String> = comp.parts.iter().map(|p| p.ident.to_string()).collect();
    VarName::new(parts.join("."))
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
        crate::visitor::ContainsDerChecker::check(self)
    }

    pub fn contains_der_of_state(&self, state_vars: &std::collections::HashSet<VarName>) -> bool {
        crate::visitor::ContainsDerOfStateChecker::check(self, state_vars)
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
        use crate::visitor::{ExpressionVisitor, StateVariableCollector};

        let mut collector = StateVariableCollector::new();
        collector.visit_expression(self);
        states.extend(collector.into_states());
    }

    pub fn collect_var_refs(&self, vars: &mut std::collections::HashSet<VarName>) {
        use crate::visitor::{ExpressionVisitor, VarRefCollector};

        let mut collector = VarRefCollector::new();
        collector.visit_expression(self);
        vars.extend(collector.into_vars());
    }
}
