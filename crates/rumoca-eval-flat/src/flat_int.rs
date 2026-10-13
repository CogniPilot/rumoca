//! Structural integer evaluation against a flat model.
//!
//! Array dimensions, for-loop ranges and array subscripts must be known at
//! compile time (MLS §4.5, §10.1). This module is the single evaluator that
//! resolves such an expression to an `i64` by following parameter/constant
//! bindings recorded in `flat::Model`.

use rumoca_ir_flat as flat;

#[cfg(test)]
mod tests;

/// Maximum binding-chain depth followed while resolving a structural value.
const MAX_EVAL_DEPTH: u8 = 8;

/// Recursively evaluate a flat expression to an integer value.
///
/// Uses the shared constant interpreter with parameter/constant values and
/// fixed shape metadata. Only an exact Integer result is admitted; Real
/// arithmetic requires an explicit language conversion such as `integer`.
pub fn try_eval_flat_expr_i64(
    expr: &rumoca_core::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    if let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    {
        return Some(*value);
    }
    if depth > MAX_EVAL_DEPTH {
        return None;
    }
    let environment = StructuralBindings { flat, depth };
    match crate::constant::eval_expr_with_span(
        expr,
        &environment,
        expr.span().unwrap_or(rumoca_core::Span::DUMMY),
    )
    .ok()?
    {
        crate::constant::Value::Integer(value) => Some(value),
        _ => None,
    }
}

struct StructuralBindings<'a> {
    flat: &'a flat::Model,
    depth: u8,
}

impl StructuralBindings<'_> {
    fn value(&self, variable: &flat::Variable) -> Option<crate::constant::Value> {
        if self.depth >= MAX_EVAL_DEPTH
            || !matches!(
                variable.variability,
                rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
            )
            || variable.evaluate_refused
            || variable.fixed_uniform() == Some(false)
        {
            return None;
        }
        let binding = variable.binding.as_ref()?;
        let next = Self {
            flat: self.flat,
            depth: self.depth + 1,
        };
        crate::constant::eval_expr_with_span(binding, &next, variable.source_span).ok()
    }
}

impl crate::constant::EvalEnvironment for StructuralBindings<'_> {
    fn get_value(&self, name: &str) -> Option<std::borrow::Cow<'_, crate::constant::Value>> {
        self.value(self.flat.variables.get(&rumoca_core::VarName::new(name))?)
            .map(std::borrow::Cow::Owned)
    }

    fn get_reference_value(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<std::borrow::Cow<'_, crate::constant::Value>> {
        let variable = self.flat.variables.get(name.var_name())?;
        if name
            .instance_id()
            .is_some_and(|identity| identity != variable.instance_id)
        {
            return None;
        }
        self.value(variable).map(std::borrow::Cow::Owned)
    }

    fn get_enum(&self, _name: &str) -> Option<&(String, String)> {
        None
    }

    fn get_function(&self, name: &str) -> Option<&rumoca_core::Function> {
        self.flat.functions.get(&rumoca_core::VarName::new(name))
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .map(|variable| variable.dims.as_slice())
    }

    fn deferred_parameter(&self, _name: &str) -> Option<crate::constant::DeferredParameterSource> {
        None
    }
}

/// Evaluate `size(array, dim)` from literal array shape or variable dimensions.
pub fn eval_size_call_i64(
    array_arg: &rumoca_core::Expression,
    dim_arg: &rumoca_core::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    let span = array_arg
        .span()
        .or_else(|| dim_arg.span())
        .unwrap_or(rumoca_core::Span::DUMMY);
    try_eval_flat_expr_i64(
        &rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![array_arg.clone(), dim_arg.clone()],
            span,
        },
        flat,
        depth,
    )
}

/// Size of dimension `dim` (1-based) of a literal array expression.
pub fn literal_array_dim_size(expr: &rumoca_core::Expression, dim: usize) -> Option<i64> {
    let rumoca_core::Expression::Array { elements, .. } = expr else {
        return None;
    };
    match dim {
        1 => Some(elements.len() as i64),
        2 => elements.first().and_then(|first| match first {
            rumoca_core::Expression::Array { elements, .. } => Some(elements.len() as i64),
            _ => None,
        }),
        _ => None,
    }
}

/// Structural integer value of a flat variable that is a `constant` or
/// `parameter` with a compile-time-known binding.
///
/// Returns `None` for continuous/discrete variables, for parameters without a
/// binding, and for bindings that are not structurally constant. Per MLS §4.5
/// only such values may legally appear in a subscript that has to be folded at
/// compile time.
pub fn structural_integer_value(var: &flat::Variable, flat: &flat::Model) -> Option<i64> {
    match (StructuralBindings { flat, depth: 0 }).value(var)? {
        crate::constant::Value::Integer(value) => Some(value),
        _ => None,
    }
}
