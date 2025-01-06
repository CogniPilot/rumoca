use crate::s1_parser::ast::{ClassType, Expression, Statement};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Class {
    pub name: String,
    pub class_type: ClassType,
    pub description: String,
    pub states: HashSet<String>,
    pub c: Vec<Component>, // constants
    pub x: Vec<Component>, // continuous states
    pub z: Vec<Component>, // discrete states
    pub w: Vec<Component>, // continuous internal variables
    pub u: Vec<Component>, // input
    pub y: Vec<Component>, // continuous output variables
    pub p: Vec<Component>, // parameters
    pub ode: Vec<Expression>, // ordinary diff equation
    pub alg: Vec<Statement>,  // algebraic eq
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub struct Component {
    pub name: String,
    pub start: Box<Expression>,
    pub array_subscripts: Vec<Box<Expression>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {
        // class ball
        #[allow(unused_variables)]
        let class_ball = Class {
            name: String::from("Ball"),
            ..Default::default()
        };
    }
}
