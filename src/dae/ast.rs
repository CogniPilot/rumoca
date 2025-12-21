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
use std::fmt;

use crate::ir::ast::{Component, Equation, Expression, Statement};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Dae {
    pub model_name: String,              // name of the compiled model
    pub rumoca_version: String,          // version of rumoca used to generate this DAE
    pub git_version: String,             // git hash of rumoca used to generate this DAE
    pub model_hash: String,              // md5 hash of the model used to generate this DAE
    pub template_hash: String,           // md5 hash of the template used to generate this
    pub t: Component,                    // time
    pub p: IndexMap<String, Component>,  // parameters
    pub cp: IndexMap<String, Component>, // constant parameters (ADDED)
    pub x: IndexMap<String, Component>,  // continuous states
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
    pub fx_init: Vec<Equation>,             // initial equations (only hold at t=0)
    pub fz: Vec<Equation>,                  // event update equations
    pub fm: Vec<Equation>,                  // discrete update equations
    pub fr: IndexMap<String, Statement>,    // reset expressions, condition -> assignment statements
    pub fc: IndexMap<String, Expression>,   // condition updates, condition -> expression
}

impl Dae {
    /// Export to DAE IR JSON format using serde serialization.
    ///
    /// Serializes the DAE structure directly to JSON, providing a complete
    /// representation matching the Modelica specification's DAE formalism (Appendix B).
    ///
    /// # Returns
    ///
    /// A pretty-printed JSON string of the DAE structure.
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
    /// let json = result.dae.to_dae_ir_json()?;
    /// println!("{}", json);
    /// # Ok(())
    /// # }
    /// ```
    pub fn to_dae_ir_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Export to a human-readable pretty-printed text format.
    ///
    /// This uses the Display trait to generate a nicely formatted text
    /// representation of the DAE, showing equations as readable math
    /// (e.g., `der(x) = v`, `x * y + z`).
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use rumoca::Compiler;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let result = Compiler::new().compile_file("model.mo")?;
    /// let pretty = result.dae.to_pretty_string();
    /// println!("{}", pretty);
    /// # Ok(())
    /// # }
    /// ```
    pub fn to_pretty_string(&self) -> String {
        format!("{}", self)
    }
}

impl fmt::Display for Dae {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== {} ===", self.model_name)?;
        if !self.rumoca_version.is_empty() {
            writeln!(f, "rumoca: {}", self.rumoca_version)?;
        }
        writeln!(f)?;

        // Parameters
        if !self.p.is_empty() {
            writeln!(f, "Parameters:")?;
            for (name, comp) in &self.p {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Constant parameters
        if !self.cp.is_empty() {
            writeln!(f, "Constants:")?;
            for (name, comp) in &self.cp {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Inputs
        if !self.u.is_empty() {
            writeln!(f, "Inputs:")?;
            for (name, comp) in &self.u {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // States (x)
        if !self.x.is_empty() {
            writeln!(f, "States (x):")?;
            for (name, comp) in &self.x {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Algebraic variables (y)
        if !self.y.is_empty() {
            writeln!(f, "Algebraics (y):")?;
            for (name, comp) in &self.y {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Discrete real variables (z)
        if !self.z.is_empty() {
            writeln!(f, "Discrete Real (z):")?;
            for (name, comp) in &self.z {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Discrete-valued variables (m)
        if !self.m.is_empty() {
            writeln!(f, "Discrete (m):")?;
            for (name, comp) in &self.m {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Conditions (c)
        if !self.c.is_empty() {
            writeln!(f, "Conditions (c):")?;
            for (name, comp) in &self.c {
                write!(f, "  {}: {}", name, comp.type_name)?;
                format_shape(f, &comp.shape)?;
                format_start(f, &comp.start)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        // Continuous equations (fx)
        if !self.fx.is_empty() {
            writeln!(f, "Equations (fx):")?;
            for eq in &self.fx {
                writeln!(f, "  {};", eq)?;
            }
            writeln!(f)?;
        }

        // Initial equations (fx_init)
        if !self.fx_init.is_empty() {
            writeln!(f, "Initial Equations (fx_init):")?;
            for eq in &self.fx_init {
                writeln!(f, "  {};", eq)?;
            }
            writeln!(f)?;
        }

        // Algebraic equations (fz)
        if !self.fz.is_empty() {
            writeln!(f, "Algebraic Equations (fz):")?;
            for eq in &self.fz {
                writeln!(f, "  {};", eq)?;
            }
            writeln!(f)?;
        }

        // Discrete update equations (fm)
        if !self.fm.is_empty() {
            writeln!(f, "Discrete Equations (fm):")?;
            for eq in &self.fm {
                writeln!(f, "  {};", eq)?;
            }
            writeln!(f)?;
        }

        // Reset equations (fr)
        if !self.fr.is_empty() {
            writeln!(f, "Reset Statements (fr):")?;
            for (cond, stmt) in &self.fr {
                writeln!(f, "  when {}: {}", cond, format_statement(stmt))?;
            }
            writeln!(f)?;
        }

        // Condition updates (fc)
        if !self.fc.is_empty() {
            writeln!(f, "Condition Updates (fc):")?;
            for (cond, expr) in &self.fc {
                writeln!(f, "  {} := {}", cond, expr)?;
            }
            writeln!(f)?;
        }

        // Summary
        writeln!(f, "Summary:")?;
        writeln!(f, "  States: {}", self.x.len())?;
        writeln!(f, "  Algebraics: {}", self.y.len())?;
        writeln!(
            f,
            "  Equations: {} (continuous) + {} (algebraic)",
            self.fx.len(),
            self.fz.len()
        )?;

        Ok(())
    }
}

/// Helper to format array shape
fn format_shape(f: &mut fmt::Formatter<'_>, shape: &[usize]) -> fmt::Result {
    if !shape.is_empty() {
        write!(
            f,
            "[{}]",
            shape
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
    }
    Ok(())
}

/// Helper to format start value
fn format_start(f: &mut fmt::Formatter<'_>, start: &Expression) -> fmt::Result {
    if !matches!(start, Expression::Empty) {
        write!(f, " = {}", start)?;
    }
    Ok(())
}

/// Helper to format a Statement
fn format_statement(stmt: &Statement) -> String {
    match stmt {
        Statement::Assignment { comp, value } => format!("{} := {}", comp, value),
        Statement::Return { .. } => "return".to_string(),
        Statement::Break { .. } => "break".to_string(),
        Statement::For { indices, equations } => {
            let idx_str = indices
                .iter()
                .map(|i| format!("{} in {}", i.ident.text, i.range))
                .collect::<Vec<_>>()
                .join(", ");
            let eqs_str = equations
                .iter()
                .map(format_statement)
                .collect::<Vec<_>>()
                .join("; ");
            format!("for {} loop {} end for", idx_str, eqs_str)
        }
        Statement::While(block) => {
            format!("while {} loop ... end while", block.cond)
        }
        Statement::When(blocks) => {
            let mut s = String::new();
            for (i, block) in blocks.iter().enumerate() {
                if i == 0 {
                    s.push_str(&format!("when {} then ...", block.cond));
                } else {
                    s.push_str(&format!(" elsewhen {} then ...", block.cond));
                }
            }
            s.push_str(" end when");
            s
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            let args_str = args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            if outputs.is_empty() {
                format!("{}({})", comp, args_str)
            } else {
                let outputs_str = outputs
                    .iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) := {}({})", outputs_str, comp, args_str)
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            let mut s = String::new();
            for (i, block) in cond_blocks.iter().enumerate() {
                if i == 0 {
                    s.push_str(&format!("if {} then ...", block.cond));
                } else {
                    s.push_str(&format!(" elseif {} then ...", block.cond));
                }
            }
            if let Some(eb) = else_block
                && !eb.is_empty()
            {
                s.push_str(" else ...");
            }
            s.push_str(" end if");
            s
        }
        Statement::Empty => String::new(),
    }
}
