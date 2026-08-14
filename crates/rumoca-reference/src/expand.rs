//! The MLS §8.3.5.1 when-equation expansion, written out literally.
//!
//! MLS §8.3.5.1, "Defining When-Equations by If-Expressions", says a
//! when-equation
//!
//! ```text
//! when x > 2 then
//!   y1 = sin(x);
//!   y3 = 2*x + y1+y2;
//! end when;
//! ```
//!
//! is conceptually equivalent to
//!
//! ```text
//! Boolean b(start = x.start > 2);
//! b  = x > 2;
//! y1 = if edge(b) then sin(x) else pre(y1);
//! y3 = if edge(b) then 2*x + y1+y2 else pre(y3);
//! ```
//!
//! This module performs exactly that rewrite and nothing else. Every
//! when-equation becomes an activation buffer, one equation defining the
//! buffer, and one conditional equation per assigned variable. After this pass
//! the model contains no `when` at all, so the interpreter in
//! [`crate::simulate`] never needs a rule for one — the whole activation
//! semantics is carried by `edge` and `pre`, which are ordinary operators.
//!
//! Doing the expansion literally rather than efficiently is the design. Any
//! cleverness here would be cleverness the specification does not describe, and
//! this crate exists to be the thing a disagreement is adjudicated against.

use crate::model::{Equation, Expr, Model, Variability, Variable, WhenBranch};
use crate::value::Type;

/// The prefix every generated activation buffer carries.
///
/// `$` cannot begin a Modelica identifier (MLS §2.3.1), so a generated buffer
/// can never collide with a name the model declares.
pub const BUFFER_PREFIX: &str = "$when";

/// The buffer name for branch `branch` of the when-equation at `equation`.
#[must_use]
pub fn buffer_name(equation: usize, branch: usize) -> String {
    format!("{BUFFER_PREFIX}{equation}b{branch}")
}

/// A model with every when-equation replaced by its §8.3.5.1 equivalent.
#[derive(Debug, Clone, PartialEq)]
pub struct Expanded {
    /// The rewritten model: assignments and initial assignments only.
    pub model: Model,
    /// The activation buffers introduced, as `(name, condition)`, in generation
    /// order.
    ///
    /// The condition is kept beside the name because a buffer is latched at an
    /// instant's two limits rather than solved with the other equations — see
    /// `crate::simulate`'s buffer latching for why that is MLS §8.5 and not an
    /// optimization.
    pub buffers: Vec<(String, Expr)>,
}

/// Rewrites every when-equation in `model` per MLS §8.3.5.1.
///
/// The rewrite is total: the returned model's equations are all `Assign` or
/// `InitialAssign`.
#[must_use]
pub fn expand(model: &Model) -> Expanded {
    let mut expanded = Model {
        variables: model.variables.clone(),
        equations: Vec::new(),
    };
    let mut buffers = Vec::new();
    for (index, equation) in model.equations.iter().enumerate() {
        match equation {
            Equation::When(branches) => {
                expand_when(index, branches, &mut expanded, &mut buffers);
            }
            other => expanded.equations.push(other.clone()),
        }
    }
    Expanded {
        model: expanded,
        buffers,
    }
}

/// Expands one when-equation into its buffers and conditional equations.
fn expand_when(
    index: usize,
    branches: &[WhenBranch],
    expanded: &mut Model,
    buffers: &mut Vec<(String, Expr)>,
) {
    for (branch_index, branch) in branches.iter().enumerate() {
        let name = buffer_name(index, branch_index);
        // `Boolean b(start = x.start > 2);` — the buffer's start attribute is
        // the activation condition itself. `crate::simulate` evaluates start
        // attributes in the start environment, where every variable stands for
        // its own start value, so this expression *is* `x.start > 2`. Seeding
        // the buffer with `false` instead is what manufactures a rising edge at
        // the initial event for a condition that is already true there.
        expanded.variables.push(Variable {
            name: name.clone(),
            ty: Type::Boolean,
            variability: Variability::Discrete,
            start: branch.condition.clone(),
        });
        // `b = x > 2;`
        expanded.equations.push(Equation::Assign {
            target: name.clone(),
            value: branch.condition.clone(),
        });
        buffers.push((name, branch.condition.clone()));
    }
    for target in assigned_targets(branches) {
        expanded.equations.push(Equation::Assign {
            value: branch_chain(index, branches, &target),
            target,
        });
    }
}

/// Every variable the when-equation assigns, in first-appearance order.
///
/// MLS §8.3.5 requires all branches to assign the same set of variables
/// (contract EQN-013). Slice 1 does not enforce that rule — enforcement is the
/// compiler's job, not the reference's — so a variable missing from a branch
/// simply keeps `pre(v)` on that branch, which is what the expansion below
/// produces anyway.
fn assigned_targets(branches: &[WhenBranch]) -> Vec<String> {
    let mut targets: Vec<String> = Vec::new();
    for branch in branches {
        for (target, _) in &branch.body {
            if !targets.iter().any(|seen| seen == target) {
                targets.push(target.clone());
            }
        }
    }
    targets
}

/// The `if edge(b1) then e1 else if edge(b2) then e2 else pre(v)` chain.
///
/// The nesting order is branch order, which is why `elsewhen` priority falls
/// out of the expansion rather than needing a rule of its own: an earlier
/// branch's `edge` is tested first, so it wins when two branches rise at the
/// same instant.
fn branch_chain(index: usize, branches: &[WhenBranch], target: &str) -> Expr {
    // The tail of the chain is `pre(v)`: outside every activation the variable
    // holds its previous value (MLS §8.3.5, and contract EQN-034 "discrete-time
    // variables keep their values until explicitly changed").
    let mut chain = Expr::pre(target);
    for (branch_index, branch) in branches.iter().enumerate().rev() {
        let Some((_, value)) = branch.body.iter().find(|(assigned, _)| assigned == target) else {
            continue;
        };
        chain = Expr::if_then_else(
            Expr::Edge(buffer_name(index, branch_index)),
            value.clone(),
            chain,
        );
    }
    chain
}
