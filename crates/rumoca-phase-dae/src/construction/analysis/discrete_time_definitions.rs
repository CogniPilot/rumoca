//! MLS 3.7 §3.8.4: a discrete-valued variable (Boolean, Integer, String, or
//! enumeration; §4.5) changes value only at events, so an equation or binding
//! outside a when-clause that defines one must be a discrete-time expression.
//! Discrete-time expressions are built from discrete-time variables,
//! parameters, constants, relations and the event-generating built-ins outside
//! `noEvent`/`smooth`, `pre`/`edge`/`change`/`sample`/`initial`/`terminal`, and
//! calls whose arguments are all discrete-time. A definition that reads `time`
//! or a continuous-time variable outside those forms would change between
//! events without one, which the event semantics cannot represent, so it is
//! refused (ED023) instead of being held at its last event value.

use super::*;
use rumoca_core::{BuiltinFunction, ExpressionVisitor};

/// Refuse a definition of the discrete-valued `target` whose `value` is not a
/// discrete-time expression.
pub(super) fn require_discrete_time_definition(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    target: &VarName,
    value: &Expression,
    span: Span,
) -> Result<(), ToDaeError> {
    let mut visitor = ContinuousRead {
        flat,
        roles,
        suppressed: 0,
        found: None,
    };
    visitor.visit_expression(value);
    match visitor.found {
        None => Ok(()),
        Some(read) => Err(ToDaeError::ContinuousDiscreteDefinition {
            detail: format!(
                "`{target}` is discrete-valued but its definition reads the continuous-time \
                 `{read}` outside any event-generating relation"
            ),
            span,
        }),
    }
}

struct ContinuousRead<'a> {
    flat: &'a flat::Model,
    roles: &'a HashMap<VarName, PlannedRole>,
    /// Depth inside `noEvent`/`smooth`, where relations generate no event.
    suppressed: usize,
    found: Option<String>,
}

impl ContinuousRead<'_> {
    /// A read is continuous-time only when proven so: `time`, or a variable
    /// whose role is continuous, whose type is not discrete-valued, and whose
    /// value is not fixed by a binding over time-invariant reads (such a value
    /// cannot change between events).
    fn continuous(&self, name: &rumoca_core::Reference) -> bool {
        self.continuous_name(name.as_str(), &mut HashSet::new())
    }

    fn continuous_name(&self, name: &str, visiting: &mut HashSet<VarName>) -> bool {
        if name == "time" {
            return true;
        }
        let var_name = VarName::new(name);
        let Some(variable) = self.flat.variables.get(&var_name) else {
            return false;
        };
        let continuous_role = !variable.is_discrete_type
            && matches!(
                self.roles.get(&var_name),
                Some(
                    PlannedRole::State
                        | PlannedRole::Algebraic
                        | PlannedRole::Output
                        | PlannedRole::Input
                )
            );
        if !continuous_role {
            return false;
        }
        let Some(binding) = variable.binding.as_ref() else {
            return true;
        };
        if !visiting.insert(var_name) {
            return true;
        }
        let mut reads = BindingReads::default();
        reads.visit_expression(binding);
        reads.time
            || reads
                .names
                .iter()
                .any(|read| self.continuous_name(read, visiting))
    }
}

impl ExpressionVisitor for ContinuousRead<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found.is_none() {
            self.walk_expression(expr);
        }
    }

    fn visit_binary(&mut self, op: &OpBinary, lhs: &Expression, rhs: &Expression) {
        let relation = matches!(
            op,
            OpBinary::Lt
                | OpBinary::Le
                | OpBinary::Gt
                | OpBinary::Ge
                | OpBinary::Eq
                | OpBinary::Neq
        );
        // A relation outside noEvent owns an event (MLS §8.5), so its value is
        // discrete-time whatever its operands.
        if !(relation && self.suppressed == 0) {
            self.walk_binary(op, lhs, rhs);
        }
    }

    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        if self.continuous(name) {
            self.found = Some(name.as_str().to_string());
            return;
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        match function {
            BuiltinFunction::Pre
            | BuiltinFunction::Edge
            | BuiltinFunction::Change
            | BuiltinFunction::Sample
            | BuiltinFunction::Initial
            | BuiltinFunction::Terminal
            // `size` and `ndims` read only the fixed shape (MLS §10.3.1).
            | BuiltinFunction::Size
            | BuiltinFunction::Ndims => {}
            BuiltinFunction::Integer
            | BuiltinFunction::Floor
            | BuiltinFunction::Ceil
            | BuiltinFunction::Div
            | BuiltinFunction::Mod
            | BuiltinFunction::Rem
                if self.suppressed == 0 => {}
            BuiltinFunction::NoEvent | BuiltinFunction::Smooth => {
                self.suppressed += 1;
                self.walk_builtin_call(function, args);
                self.suppressed -= 1;
            }
            _ => self.walk_builtin_call(function, args),
        }
    }
}

/// Every variable name and `time` read a binding makes.
#[derive(Default)]
struct BindingReads {
    names: Vec<String>,
    time: bool,
}

impl ExpressionVisitor for BindingReads {
    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        if name.as_str() == "time" {
            self.time = true;
        } else {
            self.names.push(name.as_str().to_string());
        }
        self.walk_var_ref(name, subscripts);
    }
}
