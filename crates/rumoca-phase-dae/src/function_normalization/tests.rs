mod definedness;
mod execution;
mod rewriting;
mod support;

use std::collections::HashMap;

use rumoca_core::{
    ComponentReference, DefId, Expression, ExpressionRewriter, ExpressionVisitor,
    GeneratedFunctionLocalCatalog, GeneratedFunctionLocalKey, Reference, Statement,
    StatementRewriter, Subscript,
};

use super::*;
use support::*;

#[derive(Default)]
struct Reads<'locals> {
    source: Vec<DefId>,
    generated: Vec<GeneratedFunctionLocalKey<'locals>>,
    definitions: Vec<GeneratedFunctionLocalKey<'locals>>,
    loop_depth: usize,
    definition_depths: Vec<usize>,
    source_counts_at_definition: Vec<usize>,
    generated_counts_at_definition: Vec<usize>,
}

impl ExpressionVisitor for Reads<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.source
            .push(name.component_ref().unwrap().target_def_id());
        self.walk_var_ref(name, subscripts);
    }
}

impl<'locals> GuardVisitor<'locals> for Reads<'locals> {
    fn visit_generated_read(&mut self, key: GeneratedFunctionLocalKey<'locals>) {
        self.generated.push(key);
    }
    fn visit_definition_target(&mut self, key: GeneratedFunctionLocalKey<'locals>) {
        self.definitions.push(key);
        self.definition_depths.push(self.loop_depth);
        self.source_counts_at_definition.push(self.source.len());
        self.generated_counts_at_definition
            .push(self.generated.len());
    }
}

impl<'locals> StatementVisitor<'locals> for Reads<'locals> {
    fn visit_source_target(&mut self, _target: &ComponentReference) {}
    fn enter_loop(&mut self, _span: rumoca_core::Span) {
        self.loop_depth += 1;
    }
    fn exit_loop(&mut self, _span: rumoca_core::Span) {
        self.loop_depth -= 1;
    }
}

#[test]
fn snapshot_definition_stays_inside_its_outer_loop_and_before_the_selected_branch() {
    let (sources, span) = source();
    let function = mutable_gate_function(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let mut reads = Reads::default();
        for statement in &normalized {
            statement.visit(&mut reads);
        }
        assert_eq!(reads.definitions.len(), 1);
        assert_eq!(reads.definition_depths, [1]);
        assert_eq!(reads.loop_depth, 0);
        assert_eq!(reads.generated, reads.definitions);
        assert!(reads.source.contains(&DefId::new(VALID)));
        let NormalizedStatement::For { statements, .. } = &normalized[2] else {
            panic!("outer loop retained")
        };
        let NormalizedStatement::Definition(definition) = &statements[1] else {
            panic!("visible definition before conditional")
        };
        let NormalizedStatement::If { branches, .. } = &statements[2] else {
            panic!("source conditional retained")
        };
        let Guard::Local(selected) = branches[0].condition else {
            panic!("checked local read")
        };
        assert_eq!(selected.key(), definition.target());
        assert!(matches!(
            branches[0].statements[0],
            NormalizedStatement::For { .. }
        ));
    });
}

#[test]
fn a_source_leaf_cannot_hide_control_flow_or_generated_dependencies() {
    let (_, span) = source();
    let variants = [
        for_loop("i", 1, vec![], span),
        Statement::While {
            block: rumoca_core::StatementBlock {
                cond: boolean(true, span),
                stmts: vec![],
            },
            span,
        },
        conditional(boolean(true, span), vec![], span),
        Statement::When {
            blocks: vec![],
            span,
        },
    ];
    for statement in variants {
        assert_eq!(
            SourceStatement::construct(statement),
            Err(NormalizationError::OpaqueControlLeaf)
        );
    }
}

struct ReplaceInput {
    replacement: Expression,
}
impl ExpressionRewriter for ReplaceInput {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if let Expression::VarRef { name, .. } = expression
            && name
                .component_ref()
                .is_some_and(|reference| reference.target_def_id() == DefId::new(VALID))
        {
            return self.replacement.clone();
        }
        self.walk_expression(expression)
    }
}
impl StatementRewriter for ReplaceInput {}
impl NormalizedStatementRewriter<'_> for ReplaceInput {}

#[test]
fn canonical_source_rewriting_preserves_snapshot_keys_and_definition_provenance() {
    let (sources, span) = source();
    let function = mutable_gate_function(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let mut original = Reads::default();
        for statement in &normalized {
            statement.visit(&mut original);
        }
        let rewritten: Vec<_> = normalized
            .iter()
            .map(|statement| {
                statement.rewrite(&mut ReplaceInput {
                    replacement: variable("replacement", REPLACEMENT, span),
                })
            })
            .collect::<Result<_, _>>()
            .unwrap();
        let mut result = Reads::default();
        for statement in &rewritten {
            statement.visit(&mut result);
        }
        assert_eq!(result.generated, original.generated);
        assert_eq!(result.definitions, original.definitions);
        assert_eq!(result.definition_depths, original.definition_depths);
        assert!(!result.source.contains(&DefId::new(VALID)));
        assert!(result.source.contains(&DefId::new(REPLACEMENT)));
        let NormalizedStatement::For { statements, .. } = &rewritten[2] else {
            panic!()
        };
        let NormalizedStatement::Definition(definition) = &statements[1] else {
            panic!()
        };
        assert_eq!(definition.span(), span);
    });
}

#[test]
fn guard_dependencies_visit_index_leaves_and_all_reads_before_the_target_kill() {
    let (sources, span) = source();
    let owner = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&owner).unwrap();
        let a = locals.reserve_boolean(span).unwrap();
        let b = locals.reserve_boolean(span).unwrap();
        let c = locals.reserve_boolean(span).unwrap();
        let rhs = Guard::If {
            condition: Box::new(Guard::Local(a)),
            if_true: Box::new(Guard::And(
                Box::new(Guard::Local(b)),
                Box::new(Guard::Source(Expression::Index {
                    base: Box::new(variable("data", DATA, span)),
                    subscripts: vec![Subscript::Expr {
                        expr: Box::new(variable("i", INDEX, span)),
                        span,
                    }],
                    span,
                })),
            )),
            if_false: Box::new(Guard::Literal(false)),
        };
        let definition = GeneratedDefinition::construct(&locals, c.key(), rhs).unwrap();
        let mut reads = Reads::default();
        definition.visit(&mut reads);
        assert_eq!(reads.generated, [a.key(), b.key()]);
        assert_eq!(reads.source, [DefId::new(DATA), DefId::new(INDEX)]);
        assert_eq!(reads.definitions, [c.key()]);
        assert_eq!(reads.source_counts_at_definition, [2]);
        assert_eq!(reads.generated_counts_at_definition, [2]);
    });
}

#[test]
fn definitions_refuse_another_function_owner_in_target_or_any_rhs_arm() {
    let (sources, span) = source();
    let first = owner(span);
    let mut second = first.clone();
    second.instance_id = Some(rumoca_core::FunctionInstanceId::new(1));
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let foreign = catalog
            .function(&first)
            .unwrap()
            .reserve_boolean(span)
            .unwrap();
        let mut locals = catalog.function(&second).unwrap();
        let own = locals.reserve_boolean(span).unwrap();
        assert_eq!(
            GeneratedDefinition::construct(&locals, foreign.key(), Guard::Literal(true)),
            Err(rumoca_core::GeneratedLocalError::UnknownLocal)
        );
        let rhs = Guard::If {
            condition: Box::new(Guard::Literal(false)),
            if_true: Box::new(Guard::Literal(false)),
            if_false: Box::new(Guard::Local(foreign)),
        };
        assert_eq!(
            GeneratedDefinition::construct(&locals, own.key(), rhs),
            Err(rumoca_core::GeneratedLocalError::UnknownLocal)
        );
    });
}

#[test]
fn exits_and_following_statements_are_retained_for_the_semantic_owner() {
    let (sources, span) = source();
    let mut function = owner(span);
    function.body = vec![for_loop(
        "i",
        1,
        vec![
            Statement::Return { span },
            Statement::Break { span },
            assignment("result", RESULT, integer(42, span), span),
        ],
        span,
    )];
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let normalized = normalize(&function, catalog).unwrap();
        let NormalizedStatement::For { statements, .. } = &normalized[0] else {
            panic!()
        };
        assert_eq!(statements.len(), 3);
        assert!(
            matches!(&statements[0], NormalizedStatement::Source(leaf) if matches!(leaf.source(), Statement::Return { .. }))
        );
        assert!(
            matches!(&statements[1], NormalizedStatement::Source(leaf) if matches!(leaf.source(), Statement::Break { .. }))
        );
    });
}

#[test]
fn separate_planning_calls_never_restart_a_function_owners_ordinal_allocator() {
    let (sources, span) = source();
    let function = mutable_gate_function(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let first = normalize(&function, catalog).unwrap();
        let second = normalize(&function, catalog).unwrap();
        let mut a = Reads::default();
        let mut b = Reads::default();
        for statement in &first {
            statement.visit(&mut a);
        }
        for statement in &second {
            statement.visit(&mut b);
        }
        assert_ne!(a.definitions, b.definitions);
        let mut ids = HashMap::new();
        ids.insert(a.definitions[0], 0);
        ids.insert(b.definitions[0], 1);
        assert_eq!(ids.len(), 2);
    });
}

/// Records only source targets and loop indices; the loop hooks keep their
/// default no-op bodies.
#[derive(Default)]
struct Targets {
    targets: Vec<DefId>,
    indices: Vec<String>,
}

impl ExpressionVisitor for Targets {}

impl<'locals> GuardVisitor<'locals> for Targets {
    fn visit_generated_read(&mut self, _key: GeneratedFunctionLocalKey<'locals>) {}
    fn visit_definition_target(&mut self, _key: GeneratedFunctionLocalKey<'locals>) {}
}

impl<'locals> StatementVisitor<'locals> for Targets {
    fn visit_source_target(&mut self, target: &ComponentReference) {
        self.targets.push(target.root_def_id());
    }
    fn enter_for_index(&mut self, index: &rumoca_core::ForIndex) {
        self.indices.push(index.ident.clone());
    }
}

#[test]
fn default_loop_hooks_still_observe_every_target_in_execution_order() {
    let (sources, span) = source();
    let function = mutable_gate_function(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut targets = Targets::default();
        for statement in &normalize(&function, catalog).unwrap() {
            statement.visit(&mut targets);
        }
        let ids = |ids: &[u32]| ids.iter().copied().map(DefId::new).collect::<Vec<_>>();
        assert_eq!(targets.targets, ids(&[RESULT, VALID, VALID, VALID, RESULT]));
        assert_eq!(targets.indices, ["i", "j"]);
    });
}

#[test]
fn a_function_without_a_local_owner_is_a_local_catalog_error() {
    let (sources, span) = source();
    let mut undeclared = owner(span);
    undeclared.def_id = None;
    let mut uninstantiated = owner(span);
    uninstantiated.instance_id = None;
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        assert_eq!(
            normalize(&undeclared, catalog).unwrap_err(),
            NormalizationError::Local(GeneratedLocalError::MissingFunctionDeclaration)
        );
        assert_eq!(
            normalize(&uninstantiated, catalog).unwrap_err(),
            NormalizationError::Local(GeneratedLocalError::MissingFunctionInstance)
        );
    });
}
