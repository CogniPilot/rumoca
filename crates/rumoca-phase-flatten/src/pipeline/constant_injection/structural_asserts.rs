use super::{Context, FlattenError, flat};

/// Fold structurally decided `assert` statements out of the initial
/// algorithms (MLS §8.3.7, §10.4).
///
/// The evaluation context is built so that MEMBERSHIP IS THE STRUCTURAL
/// PROOF: it carries only values whose declarations are translation-frozen —
/// constants, `annotation(Evaluate = true)` parameters, and `final`
/// parameters. An ordinary tunable parameter's default is deliberately
/// absent, so an assertion over it stays undecided and untouched, whatever
/// value the broader structural context happens to know. A condition proven
/// true folds away (and any function it alone referenced stops needing a DAE
/// owner); proven false at error level with an evaluable message is the
/// EF030 translation diagnostic at the assertion's own span; proven false at
/// `AssertionLevel.warning`, or with a message this context cannot evaluate,
/// keeps the statement for the runtime owner — MLS warning-level failures
/// are runtime behavior, and a message must never be silently replaced.
///
/// `error_literal` is the predefined `AssertionLevel.error` declaration
/// identity from the scope tree: an explicit level counts as error only by
/// that exact target identity, never by a rendered enum spelling. `None`
/// fails closed — every explicitly leveled assertion is then kept.
pub(crate) fn fold_structural_initial_asserts(
    flat: &mut flat::Model,
    ctx: &Context,
    error_literal: Option<rumoca_core::DefId>,
) -> Result<(), FlattenError> {
    use rumoca_eval_flat::constant::{EvalContext, Value};

    let frozen = |name: &str| {
        let Some(variable) = flat.variables.get(&rumoca_core::VarName::new(name)) else {
            return false;
        };
        match variable.variability {
            rumoca_core::Variability::Constant(_) => true,
            rumoca_core::Variability::Parameter(_) => {
                variable.evaluate
                    || flat
                        .variable_final_flags
                        .get(&rumoca_core::VarName::new(name))
                        .copied()
                        .unwrap_or(false)
            }
            _ => false,
        }
    };

    let mut eval_ctx = EvalContext::structural_preidentity();
    for (name, value) in &ctx.parameter_values {
        if frozen(name) {
            eval_ctx.add_parameter(name.clone(), Value::Integer(*value));
        }
    }
    for (name, value) in &ctx.real_parameter_values {
        if frozen(name) {
            eval_ctx.add_parameter(name.clone(), Value::Real(*value));
        }
    }
    for (name, value) in &ctx.boolean_parameter_values {
        if frozen(name) {
            eval_ctx.add_parameter(name.clone(), Value::Bool(*value));
        }
    }
    // The Flat catalog is the authority here: every pre-collected callable was
    // seeded into it under the identity it still carries, and collection may
    // since have re-converted an entry under that same identity, so issuing
    // the context copy as well would present one instance twice.
    crate::equations::try_issue_eval_function_facts(&mut eval_ctx, flat.functions.values())
        .map_err(|error| FlattenError::internal(error.to_string()))?;

    for algorithm in &mut flat.initial_algorithms {
        let mut retained = Vec::with_capacity(algorithm.statements.len());
        for statement in std::mem::take(&mut algorithm.statements) {
            let rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                span,
            } = &statement
            else {
                retained.push(statement);
                continue;
            };
            let Some(holds) = crate::constant_eval::evaluate_optional_boolean(
                condition,
                &eval_ctx,
                "folding a structural assertion condition",
                *span,
            )?
            else {
                retained.push(statement);
                continue;
            };
            if holds {
                continue;
            }
            // MLS §8.3.7: an omitted level defaults to error. An explicit
            // level is error exactly when its structured reference targets
            // the predefined `AssertionLevel.error` declaration; anything
            // else — the warning literal, a spoofed user enum, a level this
            // pass cannot identify — keeps the statement for the runtime.
            let error_level = match level.as_deref() {
                None => true,
                Some(rumoca_core::Expression::VarRef { name, .. }) => name
                    .component_ref()
                    .is_some_and(|reference| Some(reference.target_def_id()) == error_literal),
                Some(_) => false,
            };
            if !error_level {
                retained.push(statement);
                continue;
            }
            let Some(text) = crate::constant_eval::evaluate_optional_string(
                message,
                &eval_ctx,
                "folding a structural assertion message",
                *span,
            )?
            else {
                // The runtime owner keeps the exact message the model wrote.
                retained.push(statement);
                continue;
            };
            return Err(FlattenError::structural_assertion_failed(text, *span));
        }
        algorithm.statements = retained;
    }
    Ok(())
}
