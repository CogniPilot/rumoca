use serde::{Deserialize, Serialize};
use crate::ast::Expression;

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Model {
    pub name: String,
    pub c: Vec<Constant>,
    pub x: Vec<ContinuousVariable>,
    pub z: Vec<DiscreteVariable>,
    pub u: Vec<ContinuousVariable>,
    pub y: Vec<ContinuousVariable>,
    pub p: Vec<Parameter>,
    pub ode: Vec<Expression>,
    pub alg: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ContinuousVariable {
    pub name: String,
    pub start: Box<Expression>
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct DiscreteVariable {
    pub name: String,
    pub start: Box<Expression>
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub value: Box<Expression>
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Constant {
    pub name: String,
    pub value: Box<Expression>
}