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
    use rumoca_eval_flat::constant::{EvalContext, Value, eval_expr};

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

    let mut eval_ctx = EvalContext::new();
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
    for func in ctx.functions.values() {
        eval_ctx.add_function(func.clone());
    }
    for func in flat.functions.values() {
        eval_ctx.add_function(func.clone());
    }

    let mut failure: Option<(String, rumoca_core::Span)> = None;
    for algorithm in &mut flat.initial_algorithms {
        algorithm.statements.retain(|statement| {
            let rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                span,
            } = statement
            else {
                return true;
            };
            if failure.is_some() {
                return true;
            }
            let Ok(Value::Bool(holds)) = eval_expr(condition, &eval_ctx) else {
                return true;
            };
            if holds {
                return false;
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
                return true;
            }
            let Ok(Value::String(text)) = eval_expr(message, &eval_ctx) else {
                // The runtime owner keeps the exact message the model wrote.
                return true;
            };
            failure = Some((text, *span));
            true
        });
    }
    if let Some((message, span)) = failure {
        return Err(FlattenError::structural_assertion_failed(message, span));
    }
    Ok(())
}
