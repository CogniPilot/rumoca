use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct StoredDefinition {
    pub classes: Vec<ClassDefinition>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub struct ComponentDeclaration {
    pub name: String,
    pub class: String,
    pub connection: Connection,
    pub variability: Variability,
    pub causality: Causality,
    pub array_subscripts: Vec<Box<Expression>>,
    pub modification: Modification,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct ClassDefinition {
    pub name: String,
    pub description: String,
    pub class_type: ClassType,
    pub partial: bool,
    pub encapsulated: bool,
    pub compositions: Vec<Composition>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Composition {
    ElementList {
        visibility: Visibility,
        elements: Vec<ComponentDeclaration>,
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
        rhs: Box<Expression>,
    },
    If {
        if_cond: Box<Expression>,
        if_eqs: Vec<Statement>,
        else_if_blocks: Vec<ElseIfStatementBlock>,
        else_eqs: Vec<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub struct ComponentReference {
    pub name: String,
    pub array_subscripts: Vec<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Equation {
    Der {
        comp: ComponentReference,
        rhs: Box<Expression>,
    },
    Simple {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    If {
        if_cond: Box<Expression>,
        if_eqs: Vec<Equation>,
        else_if_blocks: Vec<ElseIfEquationBlock>,
        else_eqs: Vec<Equation>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfEquationBlock {
    pub cond: Box<Expression>,
    pub eqs: Vec<Equation>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfStatementBlock {
    pub cond: Box<Expression>,
    pub eqs: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ElseIfExpressionBlock {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub enum Expression {
    UnsignedInteger(i64),
    UnsignedReal(f64),
    Boolean(bool),
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
        if_eq: Box<Expression>,
        else_if_blocks: Vec<ElseIfExpressionBlock>,
        else_eq: Option<Box<Expression>>,
    },
    ArrayArguments {
        args: Vec<Box<Expression>>,
    },
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Box<Expression>>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Modification {
    pub expression: Box<Expression>,
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
            name: String::from("Ball"),
            ..Default::default()
        };
        def.classes.push(class_ball);
    }
}
