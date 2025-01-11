use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct StoredDefinition {
    pub classes: Vec<ClassDefinition>,
    pub within: Option<Name>,
    pub model_md5: String,
    pub rumoca_git_hash: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentDeclaration {
    pub declaration: Declaration,
    pub condition_attribute: Option<Expression>,
    pub description: Description,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentDeclaration1 {
    pub declaration: Declaration,
    pub description: Description,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct ClassPrefixes {
    pub is_partial: bool,
    pub class_type: ClassType,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct ClassSpecifier {
    pub name: String,
    pub description: Vec<String>,
    pub composition: Vec<CompositionPart>,
    pub name_end: String,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct ClassDefinition {
    pub is_encapsulated: bool,
    pub is_final: bool,
    pub class_prefixes: ClassPrefixes,
    pub class_specifier: ClassSpecifier,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum CompositionPart {
    ElementList {
        visibility: Visibility,
        elements: Vec<Element>,
    },
    EquationSection {
        initial: bool,
        equations: Vec<Equation>,
    },
    AlgorithmSection {
        initial: bool,
        statements: Vec<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Element {
    ComponentClause {
        type_prefix: TypePrefix,
        type_specifier: TypeSpecifier,
        array_subscripts: Option<Vec<Subscript>>,
        components: Vec<ComponentDeclaration>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Causality {
    None,
    Input,
    Output,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Variability {
    Continuous,
    Discrete,
    Parameter,
    Constant,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Protected,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Connection {
    None,
    Flow,
    Stream,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Assignment {
        comp: ComponentReference,
        rhs: Expression,
    },
    If {
        if_cond: Expression,
        if_stmts: Vec<Statement>,
        else_if_blocks: Vec<ElseIfStatementBlock>,
        else_stmts: Vec<Statement>,
    },
    For {
        indices: Vec<ForIndex>,
        stmts: Vec<Statement>,
    },
    While {
        cond: Expression,
        stmts: Vec<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RefPart {
    pub name: String,
    pub array_subscripts: Vec<Subscript>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentReference {
    pub local: bool,
    pub parts: Vec<RefPart>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Equation {
    Simple {
        lhs: Expression,
        rhs: Expression,
    },
    If {
        if_cond: Expression,
        if_eqs: Vec<Equation>,
        else_if_blocks: Vec<ElseIfEquationBlock>,
        else_eqs: Vec<Equation>,
    },
    For {
        indices: Vec<ForIndex>,
        eqs: Vec<Equation>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ForIndex {
    pub ident: String,
    pub in_expr: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfEquationBlock {
    pub cond: Expression,
    pub eqs: Vec<Equation>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfStatementBlock {
    pub cond: Expression,
    pub eqs: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfExpressionBlock {
    pub cond: Expression,
    pub then: Expression,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    UnsignedInteger(i64),
    UnsignedReal(f64),
    Boolean(bool),
    //String(String),
    Ref {
        comp: ComponentReference,
    },
    // unary
    Negative {
        rhs: Box<Expression>,
    },
    Parenthesis {
        rhs: Box<Expression>,
    },
    // arithmetic
    Add {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ElemAdd {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Sub {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ElemSub {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Mul {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ElemMul {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Div {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ElemDiv {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Exp {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ElemExp {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    // logical
    Or {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    And {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Not {
        rhs: Box<Expression>,
    },
    LessThan {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    LessThanOrEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    GreaterThan {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    GreaterThanOrEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Equal {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    NotEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Range {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    If {
        if_cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_if_blocks: Vec<ElseIfExpressionBlock>,
        else_expr: Box<Option<Expression>>,
    },
    ArrayArguments {
        args: Vec<Expression>,
    },
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
    Der {
        args: Vec<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Description {
    pub strings: Vec<String>,
    pub annotation: Vec<Argument>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Modification {
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Declaration {
    pub name: String,
    pub array_subscripts: Option<Vec<Subscript>>,
    pub modification: Option<Modification>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Argument {
    Modification {
        name: Name,
        each: bool,
        is_final: bool,
        modification: Option<Modification>,
        description: Description,
    },
    Replaceable,
    Redeclaration,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypePrefix {
    pub connection: Connection,
    pub variability: Variability,
    pub causality: Causality,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Component {
    pub connection: Connection,
    pub variability: Variability,
    pub causality: Causality,
    pub type_specifier: TypeSpecifier,
    pub name: String,
    pub array_subscripts: Vec<Subscript>,
    pub modification: Option<Modification>,
    pub condition_attribute: Option<Expression>,
    pub description: Description,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentClause1 {
    pub type_prefix: TypePrefix,
    pub type_specifier: TypeSpecifier,
    pub component_declaration1: ComponentDeclaration1,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Name {
    pub ident: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeSpecifier {
    pub leading_period: bool,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Subscript {
    Colon,
    Expression(Expression),
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub enum ClassType {
    #[default]
    Unknown,
    Model,
    Record,
    OperatorRecord,
    Block,
    ExpandableConnector,
    Connector,
    Type,
    Package,
    PureFunction,
    ImpureFunction,
    OperatorFunction,
    Function,
    Operator,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {
        let mut def = StoredDefinition::default();

        // class ball
        let class_ball = ClassDefinition {
            ..Default::default()
        };
        def.classes.push(class_ball);
    }
}
