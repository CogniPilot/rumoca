//! Array comprehensions `{expr for i in range, ...}` (MLS §10.4.1) in
//! model-scope constant evaluation.
//!
//! Each iterator binds its name for the iterations of its own range only, so a
//! binder shadows any outer name of the same spelling for exactly the
//! sub-expressions it scopes. The first iterator is the outermost array
//! dimension, the same element order the function-body evaluator produces.

use std::borrow::Cow;

use rumoca_core::{ComprehensionIndex, Span};

use super::errors::EvalError;
use super::expr_eval::eval_expr_with_span;
use super::value::Value;
use super::{DeferredParameterSource, EvalEnvironment, Expression, Function};
use crate::translation_reads::ResourceRoots;

/// One comprehension iterator bound to its current value over an outer scope.
struct BinderScope<'scope> {
    outer: &'scope dyn EvalEnvironment,
    name: &'scope str,
    value: Value,
}

impl EvalEnvironment for BinderScope<'_> {
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        if name == self.name {
            return Some(Cow::Borrowed(&self.value));
        }
        self.outer.get_value(name)
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        if name == self.name {
            return None;
        }
        self.outer.get_enum(name)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.outer.get_function(name)
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        if name == self.name {
            return None;
        }
        self.outer.get_array_dimensions(name)
    }

    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
        if name == self.name {
            return None;
        }
        self.outer.deferred_parameter(name)
    }

    fn translation_resources(&self) -> Option<&ResourceRoots> {
        self.outer.translation_resources()
    }
}

/// Evaluate `{expr for indices if filter}` to a nested constant array.
pub(super) fn eval_comprehension(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: Option<&Expression>,
    ctx: &dyn EvalEnvironment,
    span: Span,
) -> Result<Value, EvalError> {
    let Some((index, remaining)) = indices.split_first() else {
        return eval_expr_with_span(expr, ctx, span);
    };
    let range = match eval_expr_with_span(&index.range, ctx, span)? {
        Value::Array(values) => values,
        other => return Err(EvalError::type_mismatch("Array", other.type_name(), span)),
    };
    let mut elements = Vec::with_capacity(range.len());
    for value in range {
        let scope = BinderScope {
            outer: ctx,
            name: &index.name,
            value,
        };
        if !remaining.is_empty() {
            elements.push(eval_comprehension(expr, remaining, filter, &scope, span)?);
            continue;
        }
        if let Some(filter) = filter {
            let keep = eval_expr_with_span(filter, &scope, span)?;
            match keep.as_bool() {
                Some(true) => {}
                Some(false) => continue,
                None => return Err(EvalError::type_mismatch("Boolean", keep.type_name(), span)),
            }
        }
        elements.push(eval_expr_with_span(expr, &scope, span)?);
    }
    Ok(Value::Array(elements))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constant::EvalContext;

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("comprehension_eval_tests.mo"),
            0,
            1,
        )
    }

    fn int(value: i64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: span(),
        }
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: name.into(),
            subscripts: vec![],
            span: span(),
        }
    }

    fn vector(values: &[i64]) -> Expression {
        Expression::Array {
            elements: values.iter().map(|value| int(*value)).collect(),
            kind: rumoca_core::ArrayConstructor::Array,
            span: span(),
        }
    }

    fn index(name: &str, values: &[i64]) -> ComprehensionIndex {
        ComprehensionIndex {
            name: name.to_string(),
            range: vector(values),
        }
    }

    fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span(),
        }
    }

    /// The binder hides exactly its own name from every outer lookup and
    /// forwards every other name to the enclosing scope.
    #[test]
    fn a_binder_shadows_only_its_own_name() {
        let mut outer = EvalContext::new();
        outer.add_parameter("n", Value::Integer(3));
        outer.add_array_dimensions("a", vec![2]);
        outer.add_array_dimensions("i", vec![5]);
        outer.add_function(Function::new("f", span()));
        let scope = BinderScope {
            outer: &outer,
            name: "i",
            value: Value::Integer(1),
        };
        assert_eq!(scope.get_value("i").as_deref(), Some(&Value::Integer(1)));
        assert_eq!(scope.get_value("n").as_deref(), Some(&Value::Integer(3)));
        assert_eq!(scope.get_array_dimensions("a"), Some(&[2_i64][..]));
        assert_eq!(scope.get_array_dimensions("i"), None);
        assert!(scope.get_function("f").is_some());
        assert!(scope.get_enum("i").is_none());
        assert!(scope.deferred_parameter("i").is_none());
    }

    /// Nested iterators give nested arrays with the first iterator outermost,
    /// and a filter keeps only the elements it admits.
    #[test]
    fn nested_iterators_and_filters_shape_the_array() {
        let ctx = EvalContext::new();
        let product = binary(rumoca_core::OpBinary::Mul, var("i"), var("j"));
        let nested = eval_comprehension(
            &product,
            &[index("i", &[1, 2]), index("j", &[10, 20])],
            None,
            &ctx,
            span(),
        );
        let row = |a, b| Value::Array(vec![Value::Integer(a), Value::Integer(b)]);
        assert_eq!(
            nested.ok(),
            Some(Value::Array(vec![row(10, 20), row(20, 40)]))
        );

        let filter = binary(rumoca_core::OpBinary::Gt, var("i"), int(1));
        let filtered = eval_comprehension(
            &var("i"),
            &[index("i", &[1, 2, 3])],
            Some(&filter),
            &ctx,
            span(),
        );
        assert_eq!(
            filtered.ok(),
            Some(Value::Array(vec![Value::Integer(2), Value::Integer(3)]))
        );
    }

    /// A filter must be Boolean, and an iterator range must be an array.
    #[test]
    fn ill_typed_filters_and_ranges_are_reported() {
        let ctx = EvalContext::new();
        let filter = int(1);
        let filtered =
            eval_comprehension(&var("i"), &[index("i", &[1])], Some(&filter), &ctx, span());
        assert!(filtered.is_err());
        let scalar_range = ComprehensionIndex {
            name: "i".to_string(),
            range: int(1),
        };
        assert!(eval_comprehension(&var("i"), &[scalar_range], None, &ctx, span()).is_err());
    }
}
