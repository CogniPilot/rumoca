//! Function-assertion discovery over checked DAE statement owners.

use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    sync::Arc,
};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[derive(Clone)]
pub(super) struct FunctionAssertion<'dae> {
    pub(super) condition: dae::ExprId<'dae>,
    pub(super) message: dae::ExprId<'dae>,
    pub(crate) level: dae::AssertionLevel,
    pub(super) provenance: dae::DaeProvenance,
}

/// Source-owned iteration statements and their assertion interval. Discovery
/// issues this association while it visits the DAE statement, before lowering.
#[derive(Clone)]
pub(super) struct LoopStatements<'dae> {
    pub(super) statements: dae::FunctionStatements<'dae>,
    pub(super) assertions: Range<usize>,
}

pub(super) struct FunctionAssertionInventory<'dae> {
    pub(super) assertions: Vec<FunctionAssertion<'dae>>,
    pub(super) loops: Arc<HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>>,
}

impl<'dae> FunctionAssertionInventory<'dae> {
    pub(super) fn discover(
        view: dae::DaeView<'dae>,
        function: dae::FunctionView<'dae>,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let mut assertions = Vec::new();
        let mut loops = HashMap::new();
        collect_assertion_conditions(view, function.statements(), &mut assertions, &mut loops)?;
        Ok(Self {
            assertions,
            loops: Arc::new(loops),
        })
    }
}

pub(super) fn assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<FunctionAssertion<'dae>>, solve::SolveProgramConstructionError> {
    Ok(FunctionAssertionInventory::discover(view, function)?.assertions)
}

fn collect_assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    assertions: &mut Vec<FunctionAssertion<'dae>>,
    loops: &mut HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
            dae::FunctionStatementView::Assertion {
                condition,
                message,
                level,
                provenance,
            } => assertions.push(FunctionAssertion {
                condition,
                message,
                level,
                provenance,
            }),
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                view.function_fold(fold)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                let start = assertions.len();
                collect_assertion_conditions(view, statements.clone(), assertions, loops)?;
                if loops
                    .insert(
                        fold,
                        LoopStatements {
                            statements,
                            assertions: start..assertions.len(),
                        },
                    )
                    .is_some()
                {
                    return Err(solve::SolveProgramConstructionError::WireMismatch);
                }
            }
        }
    }
    Ok(())
}

/// Every value an assertion message converts to text that the declaring
/// function's frame evaluates as one owner output, in message order.
///
/// Failed-only regions evaluate these values in the declaring frame, including
/// loop-local binders and message-only calls.
pub(super) fn message_values<'dae>(
    view: dae::DaeView<'dae>,
    assertion: &FunctionAssertion<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut values = Vec::new();
    collect_message_values(view, assertion.message, &mut values);
    values
}

fn collect_message_values<'dae>(
    view: dae::DaeView<'dae>,
    message: dae::ExprId<'dae>,
    values: &mut Vec<dae::ExprId<'dae>>,
) {
    let Some(node) = view.expression(message) else {
        return;
    };
    match node.operation() {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } => {
            collect_message_values(view, lhs, values);
            collect_message_values(view, rhs, values);
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let options = match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => vec![minimum_length, left_justified, significant_digits],
                dae::StringConversionFormatView::Format { .. } => Vec::new(),
            };
            for value in std::iter::once(value).chain(options.into_iter().flatten()) {
                values.push(value);
            }
        }
        _ => {}
    }
}

/// Every call owner the typed body lowering reads.
///
/// The body lowers each statement in order, so a definition no result or
/// assertion reads is still lowered; its calls need registered owners exactly
/// like the calls a result reaches.
pub(super) fn nested_calls<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
) -> Vec<dae::ExprId<'dae>> {
    let mut roots = function.result_values().rhs_iter().collect::<Vec<_>>();
    roots.extend(assertions.iter().map(|assertion| assertion.condition));
    roots.extend(
        assertions
            .iter()
            .flat_map(|assertion| message_values(view, assertion)),
    );
    collect_statement_roots(function.statements(), &mut roots);
    let mut calls = Vec::new();
    let mut seen = HashSet::new();
    for root in roots {
        dae::for_each_expression(view, root, |_, node| {
            if let dae::ExpressionOperation::Call { owner, .. } = node.operation()
                && seen.insert(owner)
            {
                calls.push(owner);
            }
        });
    }
    calls
}

pub(super) fn statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut roots = Vec::new();
    collect_statement_roots(statements, &mut roots);
    roots
}

fn collect_statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { definition } => roots.push(definition.rhs()),
            dae::FunctionStatementView::AssignmentGroup {
                definitions,
                conditional,
            } => {
                roots.extend(definitions.rhs_iter());
                if let Some(conditional) = conditional {
                    collect_conditional_roots(conditional, roots);
                }
            }
            dae::FunctionStatementView::Assertion { condition, .. } => roots.push(condition),
            dae::FunctionStatementView::For { statements, .. } => {
                collect_statement_roots(statements, roots);
            }
        }
    }
}

fn collect_conditional_roots<'dae>(
    conditional: dae::FunctionConditionalView<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
) {
    roots.extend(conditional.conditions());
    for ordinal in 0..conditional.branch_count() {
        roots.extend(conditional.branch(ordinal).into_iter().flatten());
    }
    roots.extend(conditional.fallback());
}
