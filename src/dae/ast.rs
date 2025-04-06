//! # DAE: Differential Algebraic Equations
//!
//! v := [p; t; x_dot; x; y; z; m; pre(z); pre(m)]
//!
//! 0 = fx(v, c)                                         (B.1a)
//!
//! z = {                                                (B.1b)
//!     fz(v, c) at events
//!     pre(z)   otherwise
//! }
//!
//! m := fm(v, c)                                        (B.1c)
//!
//! c := fc(relation(v))                                 (B.1d)
//!
//! ### where:
//!
//! * `p`           : Modelica variables declared as parameters or constants,
//!                   i.e., variables without any time-dependency.
//! * `t`           : Modelica variable representing time, the independent (real) variable.
//! * `x(t)`        : Modelica variables of type `Real` that appear differentiated.
//! * `y(t)`        : Continuous-time Modelica variables of type `Real` that do not
//!                   appear differentiated (= algebraic variables).
//! * `z(t_e)`      : Discrete-time Modelica variables of type `Real`. These
//!                   variables change their value only at event instants `t_e`. `pre(z)`
//!                   are the values immediately before the current event occurred.
//! * `m(t_e)`      : Modelica variables of discrete-valued types (Boolean,
//!                   Integer, etc) which are unknown. These variables change their value
//!                   only at event instants
//! * `pre(m)`      : The values of `m` immediately before the current event occurred.
//!
//! [For equations in when-clauses with discrete-valued variables on the left-hand side,
//! the form (B.1c) relies upon the conceptual rewriting of equations described
//! in section 8.3.5.1.]
//!
//! * `c(t_e)`      : The conditions of all if-expressions generated including
//!                   when-clauses after conversion, see section 8.3.5).
//! * `relation(v)` : A relation containing variables v_i, (e.g. v1 > v2, v3 >= 0).
//!
//! For simplicity, the special cases of noEvent and reinit are not contained
//! in the equations above and are not discussed below.
//!
//! reinit:
//!
//! v = fr (v, c)    : happens at event time

use indexmap::IndexMap;

use crate::ir::ast::{Component, Equation, Expression, Statement};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Dae {
    pub p: Vec<Component>,                // parameters
    pub cp: Vec<Component>,               // constant parameters (ADDED)
    pub t: Component,                     // time
    pub x: Vec<Component>,                // continous states
    pub x_dot: Vec<Component>,            // derivatives of continuous states
    pub y: Vec<Component>,                // alg. variables
    pub u: Vec<Component>,                // input (ADDED)
    pub pre_z: Vec<Component>,            // z before event time t_e
    pub pre_x: Vec<Component>,            // x before event time t_e
    pub pre_m: Vec<Component>,            // m before event time t_e
    pub z: Vec<Component>,                // real discrete variables, only change at t_e
    pub m: Vec<Component>,                // variables of discrete-value types, only change at t_e
    pub c: Vec<Component>,                // conditions of all if-expressions/ when-clauses
    pub fx: Vec<Equation>,                // continuous time equations
    pub fz: Vec<Equation>,                // event update equations
    pub fm: Vec<Equation>,                // discrete update equations
    pub fr: IndexMap<String, Statement>,  // reset expressions, condition -> assignment statements
    pub fc: IndexMap<String, Expression>, // condition updates, condition -> expression
}
