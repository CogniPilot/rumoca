use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct StoredDefinition {
    pub classes: Vec<ClassDefinition>,
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ClassDefinition {
    pub name: String,
    pub class_type: ClassType,
    pub partial: bool,
    pub components: Vec<ComponentDeclaration>,
    pub equations: Option<Vec<Equation>>,
    pub algorithms: Option<Vec<Statement>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentDeclaration {
    pub name: String,
    pub class: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Assignment {
        comp: ComponentReference,
        expr: Box<Expression>,
    },
    If {
        if_cond: Box<Expression>,
        if_eqs: Vec<Statement>,
        else_if_blocks: Vec<StatementBlock>,
        else_eqs: Option<Vec<Statement>>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComponentReference {
    pub name: String,
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
        else_if_blocks: Vec<EquationBlock>,
        else_eqs: Option<Vec<Equation>>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct EquationBlock {
    pub cond: Box<Expression>,
    pub eqs: Vec<Equation>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StatementBlock {
    pub cond: Box<Expression>,
    pub eqs: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    UnsignedInteger(i64),
    UnsignedReal(f64),
    Boolean(bool),
    Ref {
        comp: ComponentReference,
    },
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
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ClassPrefixes {
    pub class_type: ClassType,
    pub partial: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub enum ClassType {
    #[default]
    Model,
    Record,
    OperatorRecord,
    Block,
    ExpandableConnector,
    Connector,
    Type,
    Package,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {
        let mut def = StoredDefinition::default();

        // class ball
        let mut class_ball = ClassDefinition::default();
        class_ball.name = String::from("Ball");
        def.classes.push(class_ball);
    }
}
