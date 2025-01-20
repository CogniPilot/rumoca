use ordermap::{OrderMap, OrderSet};
use rumoca_parser::s1_parser::ast::{
    ClassType, Equation, Expression, Modification, Statement, Subscript,
};
use serde::{Deserialize, Serialize};

derive_alias! {
    #[derive(CommonTraits!)] = #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)];
}

//=============================================================================
/// A file level definition holding multiple flat models
#[derive(CommonTraits!, Default)]
pub struct Def {
    pub classes: OrderMap<String, Class>,
    pub model_md5: String,
    pub rumoca_parser_version: String,
    pub rumoca_parser_git: String,
    pub rumoca_version: String,
    pub rumoca_git: String,
    pub template_md5: String,
}

//=============================================================================
/// A flat definition of a DAE
#[derive(CommonTraits!, Default)]
pub struct Class {
    pub name: String,
    pub class_type: ClassType,
    pub description: Vec<String>,
    /// dictinoary of components
    pub components: OrderMap<String, Component>,
    /// constants
    pub c: OrderSet<String>,
    /// continuous states
    pub x: OrderSet<String>,
    /// discrete states
    pub z: OrderSet<String>,
    /// continuous internal variables
    pub w: OrderSet<String>,
    /// input
    pub u: OrderSet<String>,
    /// continuous output variables
    pub y: OrderSet<String>,
    /// parameters
    pub p: OrderSet<String>,
    /// ordinary diff equation
    pub ode: OrderMap<String, Expression>,
    /// algebraic eq
    pub algebraic: Vec<Equation>,
    /// algorithm stms
    pub algorithm: Vec<Statement>,
}

#[derive(CommonTraits!, Default)]
pub struct Component {
    pub name: String,
    pub start: Option<Modification>,
    // pub start_value: ArrayBase<OwnedRepr<f64>, IxDyn>,
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
