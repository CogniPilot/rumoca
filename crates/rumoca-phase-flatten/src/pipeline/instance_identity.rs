use rumoca_core::{ExpressionRewriter, StatementRewriter};
use rumoca_ir_ast::InstanceOverlay;
use rumoca_ir_flat::{InstanceKind, InstanceRelation, Model};

use crate::FlattenError;

pub(crate) fn transfer_instance_relations(
    flat: &mut Model,
    overlay: &InstanceOverlay,
) -> Result<(), FlattenError> {
    for class in overlay.classes.values() {
        insert_relation(
            flat,
            class.instance_id,
            InstanceRelation {
                owner: class.owner_component_id,
                declaration: class.class_def_id,
                indices: Box::default(),
                kind: InstanceKind::Class,
            },
        )?;
    }
    for component in overlay.components.values() {
        insert_relation(
            flat,
            component.instance_id,
            InstanceRelation {
                owner: component.owner_class_id,
                declaration: component
                    .component_ref
                    .as_ref()
                    .map(|reference| reference.target_def_id()),
                indices: component
                    .qualified_name
                    .parts
                    .last()
                    .map_or_else(Box::default, |(_, indices)| {
                        indices.clone().into_boxed_slice()
                    }),
                kind: if component.is_primitive {
                    InstanceKind::Materialized
                } else {
                    InstanceKind::Aggregate
                },
            },
        )?;
    }
    Ok(())
}

fn insert_relation(
    flat: &mut Model,
    instance_id: rumoca_core::InstanceId,
    relation: InstanceRelation,
) -> Result<(), FlattenError> {
    if flat
        .instance_relations
        .insert(instance_id, relation)
        .is_some()
    {
        return Err(FlattenError::internal(format!(
            "duplicate instantiated occurrence identity {instance_id}"
        )));
    }
    Ok(())
}

pub(crate) fn attach_reference_scope(
    expression: &mut rumoca_core::Expression,
    instance_scope: rumoca_core::InstanceId,
) -> Result<(), FlattenError> {
    let mut rewriter = InstanceScopeRewriter {
        instance_scope,
        conflict: false,
    };
    *expression = rewriter.rewrite_expression(expression);
    if rewriter.conflict {
        return Err(FlattenError::internal(
            "one Flat reference occurrence crossed instantiated class scopes",
        ));
    }
    Ok(())
}

pub(crate) fn attach_statement_reference_scopes(
    statements: &mut [rumoca_core::Statement],
    instance_scope: rumoca_core::InstanceId,
) -> Result<(), FlattenError> {
    let mut rewriter = InstanceScopeRewriter {
        instance_scope,
        conflict: false,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
    if rewriter.conflict {
        return Err(FlattenError::internal(
            "one Flat statement reference crossed instantiated class scopes",
        ));
    }
    Ok(())
}

pub(crate) fn attach_when_chain_reference_scopes(
    chain: &mut rumoca_ir_flat::WhenChain,
    instance_scope: rumoca_core::InstanceId,
) -> Result<(), FlattenError> {
    for branch in chain.branches_mut() {
        attach_reference_scope(&mut branch.condition, instance_scope)?;
        for equation in &mut branch.equations {
            attach_when_equation_reference_scopes(equation, instance_scope)?;
        }
    }
    Ok(())
}

fn attach_when_equation_reference_scopes(
    equation: &mut rumoca_ir_flat::WhenEquation,
    instance_scope: rumoca_core::InstanceId,
) -> Result<(), FlattenError> {
    use rumoca_ir_flat::WhenEquation;
    match equation {
        WhenEquation::Assign { value, .. } | WhenEquation::Reinit { value, .. } => {
            attach_reference_scope(value, instance_scope)
        }
        WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            attach_reference_scope(condition, instance_scope)?;
            attach_reference_scope(message, instance_scope)?;
            if let Some(level) = level {
                attach_reference_scope(level, instance_scope)?;
            }
            Ok(())
        }
        WhenEquation::Terminate { message, .. } => attach_reference_scope(message, instance_scope),
        WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                attach_reference_scope(condition, instance_scope)?;
                for equation in equations {
                    attach_when_equation_reference_scopes(equation, instance_scope)?;
                }
            }
            if let Some(equations) = else_branch {
                for equation in equations {
                    attach_when_equation_reference_scopes(equation, instance_scope)?;
                }
            }
            Ok(())
        }
        WhenEquation::FunctionCallOutputs { function, .. } => {
            attach_reference_scope(function, instance_scope)
        }
    }
}

struct InstanceScopeRewriter {
    instance_scope: rumoca_core::InstanceId,
    conflict: bool,
}

impl ExpressionRewriter for InstanceScopeRewriter {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let name = if name.component_ref().is_none() {
            name.clone()
        } else {
            if name
                .instance_id()
                .is_some_and(|existing| existing != self.instance_scope)
            {
                self.conflict = true;
            }
            name.clone().with_instance_id(self.instance_scope)
        };
        rumoca_core::Expression::VarRef {
            name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl StatementRewriter for InstanceScopeRewriter {}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{
        ComponentRefPart, ComponentReference, Expression, ForIndex, Reference, SourceId, Span,
        Statement, StatementBlock, Subscript,
    };

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name("instance_scope_test.mo"), 1, 2)
    }

    fn reference_expression(name: &str, def_id: u32) -> Expression {
        let span = span();
        Expression::VarRef {
            name: Reference::from_component_reference(
                ComponentReference::construct(
                    false,
                    span,
                    vec![ComponentRefPart {
                        ident: name.to_string(),
                        span,
                        subs: Vec::new(),
                        def_id: rumoca_core::DefId::new(def_id),
                    }],
                )
                .expect("test reference carries exact declaration identity"),
            ),
            subscripts: Vec::new(),
            span,
        }
    }

    fn target_reference_with_index(index: Expression, span: Span) -> ComponentReference {
        ComponentReference::construct(
            false,
            span,
            vec![ComponentRefPart {
                ident: "target".to_string(),
                span,
                subs: vec![Subscript::expr(Box::new(index), span)],
                def_id: rumoca_core::DefId::new(4),
            }],
        )
        .expect("assignment target carries exact declaration identity")
    }

    fn assert_reference(
        expression: &Expression,
        expected_scope: rumoca_core::InstanceId,
        expected_def_id: u32,
    ) {
        let Expression::VarRef { name, span, .. } = expression else {
            panic!("expected reference expression");
        };
        assert_eq!(name.instance_id(), Some(expected_scope));
        assert_eq!(
            name.target_def_id(),
            Some(rumoca_core::DefId::new(expected_def_id))
        );
        assert_eq!(*span, self::span());
    }

    #[test]
    fn nested_algorithm_scopes_ranges_conditions_lhs_subscripts_and_values() {
        let scope = rumoca_core::InstanceId::new(7);
        let span = span();
        let mut statements = vec![Statement::For {
            indices: vec![ForIndex {
                ident: "i".to_string(),
                range: reference_expression("range", 10),
            }],
            equations: vec![Statement::If {
                cond_blocks: vec![StatementBlock {
                    cond: reference_expression("condition", 11),
                    stmts: vec![Statement::Assignment {
                        comp: target_reference_with_index(reference_expression("index", 12), span),
                        value: reference_expression("value", 13),
                        span,
                    }],
                }],
                else_block: None,
                span,
            }],
            span,
        }];

        attach_statement_reference_scopes(&mut statements, scope).expect("scope attachment");

        let Statement::For {
            indices, equations, ..
        } = &statements[0]
        else {
            panic!("expected for statement");
        };
        assert_reference(&indices[0].range, scope, 10);
        let Statement::If { cond_blocks, .. } = &equations[0] else {
            panic!("expected nested if");
        };
        assert_reference(&cond_blocks[0].cond, scope, 11);
        let Statement::Assignment { comp, value, .. } = &cond_blocks[0].stmts[0] else {
            panic!("expected nested assignment");
        };
        let Subscript::Expr { expr, .. } = &comp.parts()[0].subs[0] else {
            panic!("expected expression subscript");
        };
        assert_reference(expr.as_ref(), scope, 12);
        assert_reference(value, scope, 13);
        assert_eq!(comp.target_def_id(), rumoca_core::DefId::new(4));
        assert_eq!(comp.span(), span);
    }

    #[test]
    fn when_chain_scopes_nested_conditions_and_assert_fields() {
        let scope = rumoca_core::InstanceId::new(9);
        let span = span();
        let nested = rumoca_ir_flat::WhenEquation::conditional(
            vec![(
                reference_expression("branch", 20),
                vec![rumoca_ir_flat::WhenEquation::assert(
                    reference_expression("condition", 21),
                    reference_expression("message", 22),
                    Some(reference_expression("level", 23)),
                    span,
                    "test",
                )],
            )],
            None,
            span,
            "test",
        );
        let mut branch = rumoca_ir_flat::WhenBranch::new(reference_expression("trigger", 24), span);
        branch.add_equation(nested);
        let mut chain = rumoca_ir_flat::WhenChain::new(branch, span);

        attach_when_chain_reference_scopes(&mut chain, scope).expect("scope attachment");

        let branch = chain.first();
        assert_reference(&branch.condition, scope, 24);
        let rumoca_ir_flat::WhenEquation::Conditional { branches, .. } = &branch.equations[0]
        else {
            panic!("expected conditional when equation");
        };
        assert_reference(&branches[0].0, scope, 20);
        let rumoca_ir_flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } = &branches[0].1[0]
        else {
            panic!("expected nested assert");
        };
        assert_reference(condition, scope, 21);
        assert_reference(message, scope, 22);
        assert_reference(level.as_deref().expect("assert level"), scope, 23);
    }

    const IDENTITY_AUDIT_SOURCE: &str = r"
        record R
            Real re;
            Real im;
        end R;

        model IdentityAudit
            R phasor(re = 1.0, im = 2.0);
            Real z[0];
            Real s = sum(z);
            Real x(start = 0.0, fixed = true);
        equation
            der(x) = phasor.re + s;
        end IdentityAudit;
    ";

    fn flatten_identity_audit() -> rumoca_ir_flat::Model {
        let file_name = "flat_instance_identity_audit.mo";
        let stored = rumoca_phase_parse::parse_to_ast(IDENTITY_AUDIT_SOURCE, file_name)
            .expect("fixture parses");
        let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, IDENTITY_AUDIT_SOURCE);
        let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
            .expect("fixture resolves");
        let instanced = rumoca_phase_instantiate::instantiate(resolved, "IdentityAudit")
            .expect("fixture instantiates");
        let rumoca_ir_ast::InstancedTree { tree, mut overlay } = instanced;
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "IdentityAudit")
            .expect("fixture typechecks");
        crate::flatten_ref(&tree, &overlay, "IdentityAudit").expect("fixture flattens")
    }

    #[test]
    fn flattening_gives_every_variable_and_record_an_allocated_occurrence_identity() {
        let flat = flatten_identity_audit();

        assert!(
            !flat.variables.is_empty(),
            "audit fixture produced no variables"
        );
        assert!(
            !flat.record_instances.is_empty(),
            "audit fixture produced no record containers"
        );
        for (name, variable) in &flat.variables {
            assert!(
                !variable.instance_id.is_unset(),
                "flat variable {name} carries no allocated occurrence identity"
            );
        }
        for (name, record) in &flat.record_instances {
            assert!(
                !record.instance_id.is_unset(),
                "flat record container {name} carries no allocated occurrence identity"
            );
        }
        assert_eq!(flat.validate_shape_contract(), Ok(()));
    }

    #[test]
    fn transferred_occurrence_graph_never_claims_the_reserved_identity() {
        let flat = flatten_identity_audit();

        assert!(!flat.instance_relations.is_empty());
        for instance_id in flat.instance_relations.keys() {
            assert!(
                !instance_id.is_unset(),
                "the occurrence graph claimed the reserved unset identity"
            );
        }
    }
}
