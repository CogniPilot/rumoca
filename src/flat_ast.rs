use crate::ast::{ClassType, Expression, Statement};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Class {
    pub name: String,
    pub class_type: ClassType,
    pub description: String,
    pub c: Vec<Component>,
    pub x: Vec<Component>,
    pub z: Vec<Component>,
    pub u: Vec<Component>,
    pub y: Vec<Component>,
    pub p: Vec<Component>,
    pub ode: Vec<Expression>,
    pub alg: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub struct Component {
    pub name: String,
    pub start: Box<Expression>,
    pub array_subscripts: Vec<Box<Expression>>,
}
