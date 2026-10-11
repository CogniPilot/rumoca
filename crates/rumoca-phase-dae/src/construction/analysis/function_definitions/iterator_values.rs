use super::*;

/// Exact iterator values local to a definedness query; source and generated
/// declarations occupy disjoint namespaces, leaving parameter facts unchanged.
#[derive(Clone, Default)]
pub(in crate::construction::analysis) struct ComprehensionValues {
    source: HashMap<rumoca_core::DefId, i64>,
    generated: HashMap<VarName, i64>,
}

impl ComprehensionValues {
    pub(super) fn bind(&mut self, index: &rumoca_core::ComprehensionIndex, value: i64) {
        match index.binder {
            rumoca_core::IndexBinderProvenance::Source(id) => {
                self.source.insert(id, value);
            }
            rumoca_core::IndexBinderProvenance::Generated => {
                self.generated.insert(VarName::new(&index.name), value);
            }
        }
    }

    pub(super) fn rewrite(&self, expression: &Expression) -> Expression {
        use rumoca_core::ExpressionRewriter;
        IteratorValueRewriter(self).rewrite_expression(expression)
    }

    fn value(&self, reference: &rumoca_core::Reference) -> Option<i64> {
        if reference.is_generated() {
            self.generated.get(reference.var_name()).copied()
        } else {
            reference
                .target_def_id()
                .and_then(|id| self.source.get(&id).copied())
        }
    }
}

struct IteratorValueRewriter<'a>(&'a ComprehensionValues);
impl rumoca_core::ExpressionRewriter for IteratorValueRewriter<'_> {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty()
            && let Some(value) = self.0.value(name)
        {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                span,
            }
        } else {
            Expression::VarRef {
                name: name.clone(),
                subscripts: subscripts
                    .iter()
                    .map(|subscript| self.rewrite_subscript(subscript))
                    .collect(),
                span,
            }
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
        values.source.insert(rumoca_core::DefId::new(42), 2);
        let globals = HashMap::from([(VarName::new("i"), 9)]);
        assert_eq!(
            static_integer_expression(
                &values.rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                    rumoca_core::DefId::new(42)
                ))),
                &globals
            ),
            Some(2)
        );
        assert_eq!(
            static_integer_expression(
                &values.rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                    rumoca_core::DefId::new(41)
                ))),
                &globals
            ),
            Some(9)
        );
    }

    #[test]
    fn comprehension_value_generated_axis_cannot_capture_source_id() {
        let mut values = ComprehensionValues::default();
        values.generated.insert(VarName::new("i"), 5);
        let globals = HashMap::from([(VarName::new("i"), 9)]);
        assert_eq!(
            static_integer_expression(
                &values.rewrite(&expression(rumoca_core::IndexBinderProvenance::Generated)),
                &globals
            ),
            Some(5)
        );
        assert_eq!(
            static_integer_expression(
                &values.rewrite(&expression(rumoca_core::IndexBinderProvenance::Source(
                    rumoca_core::DefId::new(41)
                ))),
                &globals
            ),
            Some(9)
        );
    }
}
