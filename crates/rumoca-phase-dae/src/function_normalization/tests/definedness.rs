use super::*;

#[test]
fn one_snapshot_identity_cannot_be_assigned_at_two_source_sites() {
    let (sources, span) = source();
    let function = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&function).unwrap();
        let local = locals.reserve_boolean(span).unwrap();
        let definition =
            GeneratedDefinition::construct(&locals, local.key(), Guard::Literal(true)).unwrap();
        let statements = [
            NormalizedStatement::Definition(definition.clone()),
            NormalizedStatement::For {
                indices: vec![],
                statements: vec![NormalizedStatement::Definition(definition)],
                span,
            },
        ];
        assert_eq!(
            super::super::dataflow::check(&statements),
            Err(NormalizationError::DuplicateGeneratedDefinition { span })
        );
    });
}

fn read<'locals>(
    local: rumoca_core::GeneratedBooleanLocal<'locals>,
    span: rumoca_core::Span,
) -> NormalizedStatement<'locals> {
    NormalizedStatement::If {
        branches: vec![Branch {
            condition: Guard::Local(local),
            statements: vec![],
        }],
        fallback: None,
        span,
    }
}

#[test]
fn a_reservation_is_not_a_definition_and_a_self_referential_rhs_is_not_initialized() {
    let (sources, span) = source();
    let function = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&function).unwrap();
        let local = locals.reserve_boolean(span).unwrap();
        let expected = Err(NormalizationError::UndefinedGeneratedLocal { span });
        assert_eq!(
            super::super::dataflow::check(&[read(local, span)]),
            expected
        );
        let self_read =
            GeneratedDefinition::construct(&locals, local.key(), Guard::Local(local)).unwrap();
        assert_eq!(
            super::super::dataflow::check(&[NormalizedStatement::Definition(self_read)]),
            expected
        );
    });
}

#[test]
fn loop_local_definitions_do_not_escape_or_supply_another_iterations_entry() {
    let (sources, span) = source();
    let function = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&function).unwrap();
        let local = locals.reserve_boolean(span).unwrap();
        let definition =
            GeneratedDefinition::construct(&locals, local.key(), Guard::Literal(true)).unwrap();
        let loop_statement = NormalizedStatement::For {
            indices: vec![],
            statements: vec![
                NormalizedStatement::Definition(definition.clone()),
                read(local, span),
            ],
            span,
        };
        assert_eq!(
            super::super::dataflow::check(std::slice::from_ref(&loop_statement)),
            Ok(())
        );
        assert_eq!(
            super::super::dataflow::check(&[loop_statement, read(local, span)]),
            Err(NormalizationError::UndefinedGeneratedLocal { span })
        );
        let read_before_write = NormalizedStatement::For {
            indices: vec![],
            statements: vec![
                read(local, span),
                NormalizedStatement::Definition(definition),
            ],
            span,
        };
        assert_eq!(
            super::super::dataflow::check(&[read_before_write]),
            Err(NormalizationError::UndefinedGeneratedLocal { span })
        );
    });
}

#[test]
fn an_enclosing_snapshot_remains_available_through_inner_loops() {
    let (sources, span) = source();
    let function = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&function).unwrap();
        let local = locals.reserve_boolean(span).unwrap();
        let definition =
            GeneratedDefinition::construct(&locals, local.key(), Guard::Literal(true)).unwrap();
        let nested = NormalizedStatement::For {
            indices: vec![],
            statements: vec![NormalizedStatement::For {
                indices: vec![],
                statements: vec![read(local, span)],
                span,
            }],
            span,
        };
        assert_eq!(
            super::super::dataflow::check(&[
                NormalizedStatement::Definition(definition),
                nested,
                read(local, span)
            ]),
            Ok(())
        );
    });
}

#[test]
fn a_partial_branch_definition_cannot_be_read_after_the_join() {
    let (sources, span) = source();
    let function = owner(span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&function).unwrap();
        let local = locals.reserve_boolean(span).unwrap();
        let definition =
            GeneratedDefinition::construct(&locals, local.key(), Guard::Literal(true)).unwrap();
        let partial = NormalizedStatement::If {
            branches: vec![Branch {
                condition: Guard::Source(boolean(true, span)),
                statements: vec![NormalizedStatement::Definition(definition)],
            }],
            fallback: None,
            span,
        };
        assert_eq!(
            super::super::dataflow::check(&[partial, read(local, span)]),
            Err(NormalizationError::UndefinedGeneratedLocal { span })
        );
    });
}
