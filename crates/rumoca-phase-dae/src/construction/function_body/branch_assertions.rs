//! MLS §8.3.7 assertions inside MLS §11.5 runtime function conditionals.
//!
//! A runtime branch lowers to value expressions, which have no place for an
//! action. Its assertions therefore move to the enclosing action owner (the
//! call-scoped body or one compact loop iteration) as the guarded condition
//! `if <branch selected> then <condition> else true`. The branch condition is
//! the one the conditional's own join reads, and the asserted condition reads
//! the branch-local definitions at the assertion's source position, so the
//! guarded action fails exactly when the executed path would.

use super::*;

/// One assertion a runtime branch hands to its enclosing action owner.
#[derive(Clone, Copy)]
pub(super) struct BranchAssertion<'dae> {
    pub(super) condition: dae::ExprId<'dae>,
    pub(super) message: dae::ExprId<'dae>,
    pub(super) level: dae::AssertionLevel,
    pub(super) provenance: dae::DaeProvenance,
}

/// The definitions and pending assertions one runtime branch leaves behind.
#[derive(Clone, Default)]
pub(super) struct BranchState<'dae> {
    pub(super) values: HashMap<VarName, dae::ExprId<'dae>>,
    pub(super) assertions: Vec<BranchAssertion<'dae>>,
}

impl<'dae> BranchState<'dae> {
    /// A branch starts from the incoming definitions and owns no action yet.
    pub(super) fn entering(values: &HashMap<VarName, dae::ExprId<'dae>>) -> Self {
        Self {
            values: values.clone(),
            assertions: Vec::new(),
        }
    }
}

/// Lower one runtime assertion against the branch-local definitions.
pub(super) fn lower_branch_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    state: &mut BranchState<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let assertion = function_assertion(statement, symbols.functions.flat)
        .expect("analysis already validates the branch assertion")
        .expect("a runtime assertion plan owns an assertion statement");
    let mut lower = |expression: &Expression| {
        lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: symbols.coordinates,
                functions: symbols.functions,
                shapes: symbols.shapes,
                function_body: Some(body),
                values: Some(&state.values),
                owner_clock: None,
            },
            binders,
            expression,
            None,
        )
    };
    let condition = lower(assertion.condition)?;
    let message = lower(assertion.message)?;
    let provenance = dae::DaeProvenance::source(assertion.span)?;
    state.assertions.push(BranchAssertion {
        condition,
        message,
        level: assertion.level,
        provenance,
    });
    Ok(())
}

/// Guard the assertions of one branch by the MLS §11.5 selection of that
/// branch: `selected` is the condition ordinal, `None` the else part.
pub(super) fn guard_branch_assertions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    conditions: &[dae::ExprId<'dae>],
    selected: Option<usize>,
    assertions: &[BranchAssertion<'dae>],
    guarded: &mut Vec<BranchAssertion<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for assertion in assertions {
        let provenance = assertion.provenance;
        let holds = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Boolean(true))
        })?;
        let arms = conditions.iter().enumerate().map(|(ordinal, condition)| {
            let value = if selected == Some(ordinal) {
                assertion.condition
            } else {
                holds
            };
            (*condition, value)
        });
        let fallback = if selected.is_none() {
            assertion.condition
        } else {
            holds
        };
        let condition = construction
            .expressions(|expressions| expressions.at(provenance).conditional(arms, fallback))?;
        guarded.push(BranchAssertion {
            condition,
            ..*assertion
        });
    }
    Ok(())
}
