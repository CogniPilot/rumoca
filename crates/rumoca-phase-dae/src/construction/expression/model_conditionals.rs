//! Model conditional activation is issued before either arm is lowered.

use super::*;

pub(super) fn lower_conditional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    parts: (&[(Expression, Expression)], &Expression, Span),
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if symbols.function_body.is_some() {
        return lower_function_conditional_expression(
            construction,
            symbols,
            binders,
            parts,
            provenance,
        );
    }
    let retain = retained_conditional(symbols, parts.0, parts.1, parts.2);
    lower_model_conditional(
        construction,
        symbols,
        binders,
        parts.0,
        parts.1,
        retain,
        provenance,
    )
}

fn lower_model_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    branches: &[(Expression, Expression)],
    fallback: &Expression,
    retain: bool,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Some(((condition, value), remaining)) = branches.split_first() else {
        return lower_expression_scoped(construction, symbols, binders, fallback, None);
    };
    let known = if retain {
        None
    } else {
        symbols.shapes.proven_value(condition)
    };
    match known {
        Some(ProvenValue::Boolean(false)) => lower_model_conditional(
            construction,
            symbols,
            binders,
            remaining,
            fallback,
            retain,
            provenance,
        ),
        Some(ProvenValue::Boolean(true)) => {
            lower_expression_scoped(construction, symbols, binders, value, None)
        }
        _ => {
            let condition =
                lower_expression_scoped(construction, symbols, binders, condition, None)?;
            lower_value_conditional(
                construction,
                condition,
                provenance,
                false,
                |construction| lower_expression_scoped(construction, symbols, binders, value, None),
                |construction| {
                    lower_model_conditional(
                        construction,
                        symbols,
                        binders,
                        remaining,
                        fallback,
                        retain,
                        provenance,
                    )
                },
            )
        }
    }
}

fn lower_function_conditional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    (branches, else_branch, span): (&[(Expression, Expression)], &Expression, Span),
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    // A variable's attribute or binding value keeps a conditional whose guard
    // reads a tunable parameter unfolded, so a change to that parameter
    // re-selects the branch after the code is generated (an eFMI `Recalibrate`
    // runs the same statement). Folding it to the parameter's translation-time
    // value silently freezes the branch. The conditional is preserved only when
    // no arm calls a user function and every arm has one proven shape
    // ([`attribute_conditional_folds`]): shape discovery prunes a dead arm's
    // calls, so preserving an arm whose call carries no shape certificate could
    // not be built, and arms of different sizes select structure. A structural
    // guard, and any such conditional, keep folding.
    // In an equation, such a guard is kept as a run-time branch when its arms
    // are structurally equal (SPEC_0040 DAE-C22); only a structural selection
    // is evaluated at translation.
    let preserve_tunable_conditional = retained_conditional(symbols, branches, else_branch, span);
    let mut lowered = Vec::with_capacity(branches.len());
    for (condition, value) in branches {
        let proven = if preserve_tunable_conditional {
            None
        } else {
            symbols.shapes.proven_value(condition)
        };
        match proven {
            Some(ProvenValue::Boolean(false)) => continue,
            Some(ProvenValue::Boolean(true)) => {
                let taken = lower_expression_scoped(construction, symbols, binders, value, None)?;
                if lowered.is_empty() {
                    return Ok(taken);
                }
                return construction.expressions(|expressions| {
                    expressions.at(provenance).conditional(lowered, taken)
                });
            }
            _ => {
                lowered.push((
                    lower_expression_scoped(construction, symbols, binders, condition, None)?,
                    lower_expression_scoped(construction, symbols, binders, value, None)?,
                ));
            }
        }
    }
    let fallback = lower_expression_scoped(construction, symbols, binders, else_branch, None)?;
    if lowered.is_empty() {
        return Ok(fallback);
    }
    construction
        .expressions(|expressions| expressions.at(provenance).conditional(lowered, fallback))
}

/// Share the existing selected-value owner without constructing a model RHS
/// before its activation arm has been issued.
pub(super) fn lower_value_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    condition: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
    function_value: bool,
    taken: impl FnOnce(
        &mut dae::DaeConstruction<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError>,
    fallback: impl FnOnce(
        &mut dae::DaeConstruction<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if function_value {
        let taken = taken(construction)?;
        let fallback = fallback(construction)?;
        return construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, taken)], fallback)
        });
    }
    let owner = construction.begin_model_conditional(condition, provenance)?;
    let taken = construction.model_conditional_arm(&owner, true, taken)?;
    let fallback = construction.model_conditional_arm(&owner, false, fallback)?;
    construction.finish_model_conditional(owner, [taken, fallback])
}
