use super::*;

/// Iterator bindings local to a definedness query. Source and generated
/// declarations occupy disjoint namespaces. An iterator may be in scope
/// even when its value cannot be settled at translation.
#[derive(Clone, Default)]
pub(in crate::construction::analysis) struct ComprehensionValues {
    source: HashMap<rumoca_core::DefId, Option<i64>>,
    generated: HashMap<VarName, Option<i64>>,
}

impl ComprehensionValues {
    pub(super) fn bind(&mut self, index: &rumoca_core::ComprehensionIndex, value: i64) {
        self.insert(index, Some(value));
    }

    pub(super) fn declare(&mut self, index: &rumoca_core::ComprehensionIndex) {
        self.insert(index, None);
    }

    fn insert(&mut self, index: &rumoca_core::ComprehensionIndex, value: Option<i64>) {
        match index.binder {
            rumoca_core::IndexBinderProvenance::Source(id) => {
                self.source.insert(id, value);
            }
            rumoca_core::IndexBinderProvenance::Generated => {
                self.generated.insert(VarName::new(&index.name), value);
            }
        }
    }

    pub(super) fn contains(&self, reference: &rumoca_core::Reference) -> bool {
        self.binding(reference).is_some()
    }

    pub(super) fn rewrite(&self, expression: &Expression) -> Option<Expression> {
        use rumoca_core::FallibleExpressionRewriter;
        IteratorValueRewriter(self)
            .rewrite_expression(expression)
            .ok()
    }

    fn binding(&self, reference: &rumoca_core::Reference) -> Option<&Option<i64>> {
        if reference.is_generated() {
            self.generated.get(reference.var_name())
        } else {
            reference
                .target_def_id()
                .and_then(|id| self.source.get(&id))
        }
    }
}

struct IteratorValueRewriter<'a>(&'a ComprehensionValues);
impl rumoca_core::FallibleExpressionRewriter for IteratorValueRewriter<'_> {
    type Error = ();

    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Expression, Self::Error> {
        if subscripts.is_empty()
            && let Some(value) = self.0.binding(name)
        {
            // An unsettled iterator cannot use a same-named local's value.
            Ok(Expression::Literal {
                value: rumoca_core::Literal::Integer(value.ok_or(())?),
                span,
            })
        } else {
            self.walk_var_ref_expression(name, subscripts, span)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::construction::analysis::function_ranges::static_integer_expression;

    fn expression(binder: rumoca_core::IndexBinderProvenance) -> Expression {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("iterator_values.mo"),
            1,
            2,
        );
        Expression::VarRef {
            name: binder.reference("i", span).unwrap(),
            subscripts: Vec::new(),
            span,
        }
    }

    #[test]
    fn comprehension_value_source_ids_keep_same_name_parameter_separate() {
        let mut values = ComprehensionValues::default();
        values.source.insert(rumoca_core::DefId::new(42), Some(2));
        let globals = HashMap::from([(VarName::new("i"), 9)]);
        assert_eq!(
            static_integer_expression(
                &values
                    .rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                        rumoca_core::DefId::new(42)
                    )))
                    .unwrap(),
                &globals
            ),
            Some(2)
        );
        assert_eq!(
            static_integer_expression(
                &values
                    .rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                        rumoca_core::DefId::new(41)
                    )))
                    .unwrap(),
                &globals
            ),
            Some(9)
        );
    }

    #[test]
    fn comprehension_value_generated_axis_cannot_capture_source_id() {
        let mut values = ComprehensionValues::default();
        values.generated.insert(VarName::new("i"), Some(5));
        let globals = HashMap::from([(VarName::new("i"), 9)]);
        assert_eq!(
            static_integer_expression(
                &values
                    .rewrite(&expression(rumoca_core::IndexBinderProvenance::Generated))
                    .unwrap(),
                &globals
            ),
            Some(5)
        );
        assert_eq!(
            static_integer_expression(
                &values
                    .rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                        rumoca_core::DefId::new(41)
                    )))
                    .unwrap(),
                &globals
            ),
            Some(9)
        );
    }

    #[test]
    fn an_unsettled_iterator_cannot_borrow_a_same_named_local() {
        let mut values = ComprehensionValues::default();
        let iterator = rumoca_core::DefId::new(42);
        values.source.insert(iterator, None);
        let expression = expression(rumoca_core::IndexBinderProvenance::Source(iterator));
        assert!(values.rewrite(&expression).is_none());
    }
}
