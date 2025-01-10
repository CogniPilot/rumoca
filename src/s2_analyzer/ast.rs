use crate::s1_parser::ast::{ClassType, Equation, Expression, Modification, Statement, Subscript};
use ndarray::{ArrayBase, IxDyn, OwnedRepr};
use ordermap::{OrderMap, OrderSet};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Def {
    pub classes: OrderMap<String, Class>,
    pub model_md5: String,
    pub rumoca_version: String,
    pub rumoca_git_hash: String,
    pub template_md5: String,
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Class {
    pub name: String,
    pub class_type: ClassType,
    pub description: Vec<String>,
    pub components: OrderMap<String, Component>, // dictinoary of components
    pub c: OrderSet<String>,                     // constants
    pub x: OrderSet<String>,                     // continuous states
    pub z: OrderSet<String>,                     // discrete states
    pub w: OrderSet<String>,                     // continuous internal variables
    pub u: OrderSet<String>,                     // input
    pub y: OrderSet<String>,                     // continuous output variables
    pub p: OrderSet<String>,                     // parameters
    pub ode: OrderMap<String, Box<Expression>>,  // ordinary diff equation
    pub algebraic: Vec<Equation>,                // algebraic eq
    pub algorithm: Vec<Statement>,               // algorithm stms
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[allow(clippy::vec_box)]
pub struct Component {
    pub name: String,
    pub start: Option<Modification>,
    pub start_value: ArrayBase<OwnedRepr<f64>, IxDyn>,
    pub array_subscripts: Vec<Subscript>,
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
