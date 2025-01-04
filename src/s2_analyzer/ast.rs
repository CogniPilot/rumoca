use crate::s1_parser::ast::{ClassType, Expression, Statement};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Class {
    pub name: String,
    pub class_type: ClassType,
    pub description: String,
    pub states: HashSet<String>,
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
