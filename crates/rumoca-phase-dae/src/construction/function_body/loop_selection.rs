use super::*;

/// End a fold at its first iteration whose body selection is false.
///
/// A fold whose body is one conditional without an else (optionally preceded
/// by the generated capture of its condition) changes nothing on an iteration
/// that does not select it. When the condition reads no binder of this loop
/// and calls no function, every later iteration evaluates it on the same
/// state, so it stays false: ending the fold there computes the same values
/// and raises no error the remaining iterations would not (MLS §11.2.3; a
/// bounded `while` loop is lowered to exactly this form, SPEC_0022 ALG-018).
/// The conditional itself is kept, so the continuation is only the early
/// exit.
pub(super) fn continue_while_selected<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    (binders, domain, owner): (
        &HashMap<VarName, dae::DomainBinderId<'dae>>,
        dae::DomainId<'dae>,
        dae::DaeProvenance,
    ),
    (statements, plans): (&[rumoca_core::Statement], &[FunctionStatementPlan]),
) -> Result<(), dae::DaeConstructionError> {
    let Some(condition) = selected_body_condition(statements, plans) else {
        return Ok(());
    };
    let mut references = Vec::new();
    condition.collect_var_refs(&mut references);
    let reads_own_binder = references.iter().any(|reference| {
        binders
            .get(reference)
            .is_some_and(|binder| binder.domain() == domain)
    });
    let calls = condition.contains_subexpression(|node| {
        matches!(node, rumoca_core::Expression::FunctionCall { .. })
    });
    if reads_own_binder || calls {
        return Ok(());
    }
    let continuation = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        condition,
    )?;
    let provenance = match condition.span() {
        Some(span) => dae::DaeProvenance::source(span)?,
        None => owner,
    };
    construction
        .functions(|functions| functions.continue_loop_while(loop_body, continuation, provenance))
}

/// The condition of a body that is exactly one conditional without an else,
/// read through its generated capture when the capture is the statement
/// before it.
pub(super) fn selected_body_condition<'s>(
    statements: &'s [rumoca_core::Statement],
    plans: &'s [FunctionStatementPlan],
) -> Option<&'s rumoca_core::Expression> {
    let (capture, selection) = match (statements, plans) {
        ([selection], [_]) => (None, selection),
        (
            [_, selection],
            [
                FunctionStatementPlan::GeneratedBooleanAssignment { target, value, .. },
                _,
            ],
        ) => (Some((target, value)), selection),
        _ => return None,
    };
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: None,
        ..
    } = selection
    else {
        return None;
    };
    let [block] = cond_blocks.as_slice() else {
        return None;
    };
    match capture {
        None => Some(&block.cond),
        Some((target, value)) => match &block.cond {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() && name.var_name() == target => Some(value),
            _ => None,
        },
    }
}
