use super::*;

struct ScopedReplacement {
    depth: usize,
    entered: usize,
    replacement: Expression,
}

impl ExpressionRewriter for ScopedReplacement {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if let Expression::VarRef { name, .. } = expression
            && name
                .component_ref()
                .is_some_and(|reference| reference.target_def_id() == DefId::new(INDEX))
        {
            return self.replacement.clone();
        }
        self.walk_expression(expression)
    }
}
impl StatementRewriter for ScopedReplacement {}

impl<'locals> NormalizedStatementRewriter<'locals> for ScopedReplacement {
    fn rewrite_normalized_statement(
        &mut self,
        statement: &NormalizedStatement<'locals>,
    ) -> Result<NormalizedStatement<'locals>, NormalizationError> {
        if matches!(statement, NormalizedStatement::For { .. }) {
            self.entered += 1;
            // A rebinding scope owns its own values. This control deliberately
            // stops at every nested loop to prove the scope hook is dispatched;
            // production index substitution decides rebinding from its owner.
            if self.depth != 0 {
                return Ok(statement.clone());
            }
            self.depth += 1;
            let rewritten = self.walk_normalized_statement(statement);
            self.depth -= 1;
            return rewritten;
        }
        self.walk_normalized_statement(statement)
    }
}

#[test]
fn nested_loops_reusing_a_display_name_obey_explicit_scope_rewrite_overrides() {
    let (sources, span) = source();
    let mut function = owner(span);
    let assign_index = || assignment("result", RESULT, variable("i", INDEX, span), span);
    function.body = vec![for_loop(
        "i",
        2,
        vec![
            assign_index(),
            for_loop("i", 2, vec![assign_index()], span),
            assign_index(),
        ],
        span,
    )];
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let mut rewriter = ScopedReplacement {
            depth: 0,
            entered: 0,
            replacement: integer(9, span),
        };
        let rewritten = rewriter.rewrite_normalized_statements(&normalized).unwrap();
        assert_eq!(rewriter.entered, 2);
        assert_eq!(rewriter.depth, 0);
        let NormalizedStatement::For { statements, .. } = &rewritten[0] else {
            panic!()
        };
        for position in [0, 2] {
            let NormalizedStatement::Source(source) = &statements[position] else {
                panic!()
            };
            assert!(matches!(
                source.source(),
                Statement::Assignment {
                    value: Expression::Literal {
                        value: rumoca_core::Literal::Integer(9),
                        ..
                    },
                    ..
                }
            ));
        }
        assert_eq!(
            statements[1],
            match &normalized[0] {
                NormalizedStatement::For { statements, .. } => statements[1].clone(),
                _ => panic!(),
            },
            "the nested binder's range and body stay in its own scope"
        );
    });
}

struct LeafReplacement {
    replacement: Statement,
    visited: usize,
}
impl ExpressionRewriter for LeafReplacement {}
impl StatementRewriter for LeafReplacement {
    fn rewrite_statement(&mut self, _statement: &Statement) -> Statement {
        self.visited += 1;
        self.replacement.clone()
    }
}
impl NormalizedStatementRewriter<'_> for LeafReplacement {}

#[test]
fn source_leaf_rewrite_overrides_are_executed_through_the_normalized_inventory() {
    let (sources, span) = source();
    let mut function = owner(span);
    function.body = vec![assignment("result", RESULT, integer(1, span), span)];
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let replacement = assignment("result", RESULT, integer(7, span), span);
        let mut rewriter = LeafReplacement {
            replacement: replacement.clone(),
            visited: 0,
        };
        let rewritten = rewriter.rewrite_normalized_statements(&normalized).unwrap();
        assert_eq!(rewriter.visited, 1);
        let NormalizedStatement::Source(source) = &rewritten[0] else {
            panic!()
        };
        assert_eq!(source.source(), &replacement);
    });
}

#[test]
fn a_legacy_leaf_override_cannot_smuggle_new_control_flow_into_an_opaque_leaf() {
    let (sources, span) = source();
    let mut function = owner(span);
    function.body = vec![assignment("result", RESULT, integer(1, span), span)];
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let mut rewriter = LeafReplacement {
            replacement: for_loop("j", 1, vec![], span),
            visited: 0,
        };
        assert_eq!(
            rewriter.rewrite_normalized_statements(&normalized),
            Err(NormalizationError::OpaqueControlLeaf)
        );
        assert_eq!(rewriter.visited, 1);
    });
}
