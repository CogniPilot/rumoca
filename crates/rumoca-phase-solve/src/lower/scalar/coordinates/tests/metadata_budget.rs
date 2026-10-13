use super::*;

#[test]
fn cached_outputs_cannot_grow_metadata_after_the_last_register_allocation() {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("CachedOutputs.mo", "1.0");
    let span = Span::from_offsets(source, 0, 3);
    let at = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let expression = view.expression_id(0).unwrap();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        compiler.ledger = register_ledger::RegisterLedger::with_budget(32);
        // Only the first projection allocates. The other 31 are cache hits:
        // admission must happen at StoreOutput, not at the next allocation.
        let error = compiler.program_outputs([(expression, 0); 32]).unwrap_err();
        assert!(matches!(error, LowerError::BudgetExceeded { .. }));
        assert_eq!(error.source_span(), Some(span));
        assert_eq!(
            error.code(),
            crate::diagnostic_codes::EL006_SOLVE_RESOURCE_BUDGET
        );
    });
}

#[test]
fn a_refused_last_operation_does_not_grow_the_buffer() {
    with_compiler(|compiler, span| {
        compiler.ledger = register_ledger::RegisterLedger::with_byte_budgets(1024, 200);
        let register = compiler.constant(1.0, span).unwrap();
        let operations = compiler.ops.clone();
        let error = compiler
            .emit(solve::LinearOp::StoreOutput { src: register })
            .unwrap_err();
        assert!(matches!(error, LowerError::BudgetExceeded { .. }));
        assert_eq!(compiler.ops, operations);
        assert_eq!(compiler.next_register, 1);
        compiler.finish_operations().unwrap();
    });
}

#[test]
fn a_refused_last_fact_does_not_insert_or_change_a_fact() {
    with_compiler(|compiler, span| {
        compiler.ledger = register_ledger::RegisterLedger::with_byte_budgets(1024, 40);
        let register = compiler.register(span).unwrap();
        let error = compiler.ledger.set_real(register, 1.0).unwrap_err();
        assert!(matches!(error, LowerError::BudgetExceeded { .. }));
        assert_eq!(compiler.ledger.fact_count(), 0);
        assert_eq!(compiler.real_register(register), None);
        // A clearing update is not an insertion and consumes no metadata.
        compiler.set_integer_register(register, None).unwrap();
        assert_eq!(compiler.ledger.fact_count(), 0);
        compiler.finish_operations().unwrap();
    });
}
