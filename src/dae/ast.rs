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
//! * `p`: Modelica variables declared as parameters or constants,
//!   i.e., variables without any time-dependency.
//! * `t`: Modelica variable representing time, the independent (real) variable.
//! * `x(t)`: Modelica variables of type `Real` that appear differentiated.
//! * `y(t)`: Continuous-time Modelica variables of type `Real` that do not
//!   appear differentiated (= algebraic variables).
//! * `z(t_e)`: Discrete-time Modelica variables of type `Real`. These
//!   variables change their value only at event instants `t_e`. `pre(z)`
//!   are the values immediately before the current event occurred.
//! * `m(t_e)`: Modelica variables of discrete-valued types (Boolean,
//!   Integer, etc) which are unknown. These variables change their value
//!   only at event instants
//! * `pre(m)`: The values of `m` immediately before the current event occurred.
//!
//! [For equations in when-clauses with discrete-valued variables on the left-hand side,
//! the form (B.1c) relies upon the conceptual rewriting of equations described
//! in section 8.3.5.1.]
//!
//! * `c(t_e)`: The conditions of all if-expressions generated including
//!   when-clauses after conversion, see section 8.3.5).
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
    pub rumoca_version: String, // version of rumoca used to generate this DAE
    pub git_version: String,    // git hash of rumoca used to generate this DAE
    pub model_hash: String,     // md5 hash of the model used to generate this DAE
    pub template_hash: String,  // md5 hash of the template used to generate this
    pub t: Component,           // time
    pub p: IndexMap<String, Component>, // parameters
    pub cp: IndexMap<String, Component>, // constant parameters (ADDED)
    pub x: IndexMap<String, Component>, // continuous states
    // NOTE: x_dot removed - derivatives remain as der(x) function calls in equations
    // for Base Modelica compliance. Templates extract derivatives as needed.
    pub y: IndexMap<String, Component>,     // alg. variables
    pub u: IndexMap<String, Component>,     // input (ADDED)
    pub pre_z: IndexMap<String, Component>, // z before event time t_e
    pub pre_x: IndexMap<String, Component>, // x before event time t_e
    pub pre_m: IndexMap<String, Component>, // m before event time t_e
    pub z: IndexMap<String, Component>,     // real discrete variables, only change at t_e
    pub m: IndexMap<String, Component>,     // variables of discrete-value types, only change at t_e
    pub c: IndexMap<String, Component>,     // conditions of all if-expressions/ when-clauses
    pub fx: Vec<Equation>,                  // continuous time equations
    pub fz: Vec<Equation>,                  // event update equations
    pub fm: Vec<Equation>,                  // discrete update equations
    pub fr: IndexMap<String, Statement>,    // reset expressions, condition -> assignment statements
    pub fc: IndexMap<String, Expression>,   // condition updates, condition -> expression
}

impl Dae {
    /// Export to Base Modelica JSON format using native serde_json serialization.
    ///
    /// This is the recommended way to export Base Modelica IR, providing fast,
    /// type-safe serialization that conforms to the MCP-0031 specification.
    ///
    /// # Returns
    ///
    /// A pretty-printed JSON string conforming to the Base Modelica IR schema.
    ///
    /// # Errors
    ///
    /// Returns a serialization error if the DAE structure cannot be converted to JSON.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use rumoca::Compiler;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let result = Compiler::new().compile_file("model.mo")?;
    /// let json = result.dae.to_base_modelica_json()?;
    /// println!("{}", json);
    /// # Ok(())
    /// # }
    /// ```
    pub fn to_base_modelica_json(&self) -> Result<String, serde_json::Error> {
        use crate::dae::base_modelica::BaseModelicaIR;

        let ir = BaseModelicaIR::from_dae(self);
        serde_json::to_string_pretty(&ir)
    }
}
