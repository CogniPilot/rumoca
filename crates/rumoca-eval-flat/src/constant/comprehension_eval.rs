//! Array comprehensions `{expr for i in range, ...}` (MLS §10.4.1) in
//! model-scope constant evaluation.
//!
//! Each iterator binds its name for the iterations of its own range only, so a
//! binder shadows any outer name of the same spelling for exactly the
//! sub-expressions it scopes. The last textual iterator is the outermost
//! array dimension (MLS §10.4.1.2).

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
    binder: rumoca_core::IndexBinderProvenance,
    value: Value,
}

impl EvalEnvironment for BinderScope<'_> {
    fn get_reference_value(&self, reference: &rumoca_core::Reference) -> Option<Cow<'_, Value>> {
        let matches = match self.binder {
            rumoca_core::IndexBinderProvenance::Source(id) => reference.target_def_id() == Some(id),
            rumoca_core::IndexBinderProvenance::Generated => {
                reference.is_generated() && reference.as_str() == self.name
            }
        };
        if matches {
            Some(Cow::Borrowed(&self.value))
        } else {
            self.outer.get_reference_value(reference)
        }
    }

    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        self.outer.get_value(name)
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.outer.get_enum(name)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.outer.get_function(name)
    }

    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.outer.get_array_dimensions(name)
    }

    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
        self.outer.deferred_parameter(name)
    }

    fn translation_resources(&self) -> Option<&ResourceRoots> {
        self.outer.translation_resources()
    }
}

struct IteratorDeclarations<'a> {
    outer: &'a dyn EvalEnvironment,
    indices: &'a [ComprehensionIndex],
}
impl EvalEnvironment for IteratorDeclarations<'_> {
    fn get_reference_value(&self, reference: &rumoca_core::Reference) -> Option<Cow<'_, Value>> {
        if reference.target_def_id().is_some_and(|id| {
            self.indices
                .iter()
                .any(|index| index.binder == rumoca_core::IndexBinderProvenance::Source(id))
        }) {
            None
        } else {
            self.outer.get_reference_value(reference)
        }
    }
    fn get_value(&self, name: &str) -> Option<Cow<'_, Value>> {
        self.outer.get_value(name)
    }
    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.outer.get_enum(name)
    }
    fn get_function(&self, name: &str) -> Option<&Function> {
        self.outer.get_function(name)
    }
    fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.outer.get_array_dimensions(name)
    }
    fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
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
    let Some((index, remaining)) = indices.split_last() else {
        return eval_expr_with_span(expr, ctx, span);
    };
    let declarations = IteratorDeclarations {
        outer: ctx,
        indices,
    };
    let range = match eval_expr_with_span(&index.range, &declarations, span)? {
        Value::Array(values) => values,
        other => return Err(EvalError::type_mismatch("Array", other.type_name(), span)),
    };
    let mut elements = Vec::with_capacity(range.len());
    for value in range {
        let scope = BinderScope {
            outer: &declarations,
            name: &index.name,
            binder: index.binder,
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
            name: rumoca_core::Reference::generated(name),
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
            binder: rumoca_core::IndexBinderProvenance::Generated,
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
            binder: rumoca_core::IndexBinderProvenance::Generated,
            value: Value::Integer(1),
        };
        assert_eq!(
            scope
                .get_reference_value(&rumoca_core::Reference::generated("i"))
                .as_deref(),
            Some(&Value::Integer(1))
        );
        assert_eq!(scope.get_value("n").as_deref(), Some(&Value::Integer(3)));
        assert_eq!(scope.get_array_dimensions("a"), Some(&[2_i64][..]));
        assert_eq!(scope.get_array_dimensions("i"), Some(&[5_i64][..]));
        assert!(scope.get_function("f").is_some());
        assert!(scope.get_enum("i").is_none());
        assert!(scope.deferred_parameter("i").is_none());
    }

    /// Nested iterators give nested arrays with the last iterator outermost,
    /// and a filter keeps only the elements it admits.
    #[test]
    fn nested_iterators_and_filters_shape_the_array() {
        let ctx = EvalContext::new();
        let product = binary(rumoca_core::OpBinary::Mul, var("i"), var("j"));
        let nested = eval_comprehension(
            &product,
            &[index("i", &[1, 2, 3]), index("j", &[10, 20])],
            None,
            &ctx,
            span(),
        );
        let row = |a, b, c| {
            Value::Array(vec![
                Value::Integer(a),
                Value::Integer(b),
                Value::Integer(c),
            ])
        };
        assert_eq!(
            nested.ok(),
            Some(Value::Array(vec![row(10, 20, 30), row(20, 40, 60)]))
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
            binder: rumoca_core::IndexBinderProvenance::Generated,
            name: "i".to_string(),
            range: int(1),
        };
        assert!(eval_comprehension(&var("i"), &[scalar_range], None, &ctx, span()).is_err());
    }
    fn source_var(name: &str, id: u32) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(
                rumoca_core::ComponentReference::construct(
                    false,
                    span(),
                    vec![rumoca_core::ComponentRefPart {
                        ident: name.to_string(),
                        span: span(),
                        subs: vec![],
                        def_id: rumoca_core::DefId::new(id),
                    }],
                )
                .expect("resolved reference"),
            ),
            subscripts: vec![],
            span: span(),
        }
    }

    fn source_index(id: u32, range: Expression) -> ComprehensionIndex {
        ComprehensionIndex {
            binder: rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(id)),
            name: "i".to_string(),
            range,
        }
    }

    #[test]
    fn comprehension_source_identity_keeps_same_name_enclosing_value() {
        let mut context = EvalContext::new();
        context.add_parameter("i", Value::Integer(9));
        let body = binary(
            rumoca_core::OpBinary::Add,
            source_var("i", 42),
            source_var("i", 41),
        );
        let value = eval_comprehension(
            &body,
            &[source_index(42, vector(&[1, 2, 3]))],
            None,
            &context,
            span(),
        )
        .unwrap();
        assert_eq!(
            value,
            Value::Array(vec![
                Value::Integer(10),
                Value::Integer(11),
                Value::Integer(12)
            ])
        );
    }

    #[test]
    fn comprehension_source_identity_last_iterator_scopes_dependent_inner_range() {
        let context = EvalContext::new();
        let range = Expression::Array {
            elements: vec![
                source_var("i", 42),
                binary(rumoca_core::OpBinary::Add, source_var("i", 42), int(1)),
            ],
            kind: rumoca_core::ArrayConstructor::Array,
            span: span(),
        };
        let value = eval_comprehension(
            &source_var("i", 43),
            &[
                source_index(43, range),
                source_index(42, vector(&[1, 2, 3])),
            ],
            None,
            &context,
            span(),
        )
        .unwrap();
        let row = |a, b| Value::Array(vec![Value::Integer(a), Value::Integer(b)]);
        assert_eq!(value, Value::Array(vec![row(1, 2), row(2, 3), row(3, 4)]));
    }
    #[test]
    fn comprehension_source_iterator_unset_does_not_read_same_name_parameter() {
        let mut context = EvalContext::new();
        context.add_parameter("i", Value::Integer(3));
        let indices = [source_index(42, source_var("i", 42))];
        let error =
            eval_comprehension(&source_var("i", 42), &indices, None, &context, span()).unwrap_err();
        assert!(error.to_string().contains("i"));
    }

    #[test]
    fn comprehension_generated_axis_cannot_capture_same_name_source_component() {
        let mut context = EvalContext::new();
        context.add_parameter("i", Value::Integer(9));
        let value = eval_comprehension(
            &source_var("i", 41),
            &[index("i", &[1, 2])],
            None,
            &context,
            span(),
        )
        .unwrap();
        assert_eq!(
            value,
            Value::Array(vec![Value::Integer(9), Value::Integer(9)])
        );
    }

    #[test]
    fn comprehension_ranges_and_bodies_read_outer_record_fields() {
        let field = |name: &str, id| Expression::FieldAccess {
            base: Box::new(source_var("bounds", 40)),
            field: name.into(),
            field_def_id: rumoca_core::DefId::new(id),
            span: span(),
        };
        let range = Expression::Range {
            start: Box::new(int(1)),
            step: None,
            end: Box::new(field("limit", 41)),
            span: span(),
        };
        let comprehension = Expression::ArrayComprehension {
            expr: Box::new(binary(
                rumoca_core::OpBinary::Add,
                source_var("i", 42),
                field("bias", 43),
            )),
            indices: vec![source_index(42, range)],
            filter: None,
            span: span(),
        };
        let mut context = EvalContext::new();
        context.add_parameter("bounds.limit", Value::Integer(3));
        context.add_parameter("bounds.bias", Value::Integer(10));
        context.add_parameter("i", Value::Integer(99));
        assert_eq!(
            eval_expr_with_span(&comprehension, &context, span()).unwrap(),
            Value::Array(vec![
                Value::Integer(11),
                Value::Integer(12),
                Value::Integer(13)
            ])
        );
        let mut missing_bound = EvalContext::new();
        missing_bound.add_parameter("bounds.bias", Value::Integer(10));
        assert!(matches!(
            eval_expr_with_span(&comprehension, &missing_bound, span()),
            Err(EvalError::UnknownVariable { name, span: at })
                if name == "bounds" && at == span()
        ));
    }
}
