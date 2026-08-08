//! MLS §12.3 impure call-context proof over the exact Flat function table.
//!
//! Resolve already rejects the syntactic form of this violation, but the DAE
//! is the authority that owns executable semantics, and it resolves callees by
//! exact Flat identity rather than by name. This pass re-proves the rule where
//! it can name the callee exactly: a continuous equation, a non-initial
//! algorithm, or a pure function body may not reach an impure function.
use super::*;

/// Reject every impure call that reaches a context MLS §12.3 forbids.
///
/// The rule is stated of the written prefix — "With the prefix keyword impure
/// it is stated that a Modelica function is impure and it is only allowed to
/// call such a function from within: …" — so it is proven against
/// `Function::pure`, the written prefix. The bare external form is not proven
/// against it: MLS 3.7 §12.3 makes that form "treated as impure" for
/// transformations while stating only that it "is deprecated", so it is
/// reported (WR001) and compiled. The same section's recursive case — a
/// function treated as impure because it calls one — is a call-graph closure
/// this pass does not compute either; that is task #76.
///
/// The contexts that list permits are not visited here; the DAE owns them
/// through their own checked owners. Two of them are not reachable in this
/// compiler yet and so are not advertised by the diagnostics: `pure(…)` is
/// erased during lowering without carrying its bypass this far (task #57), and
/// an external-object binding cannot be constructed at all (ED019).
pub(super) fn validate_impure_call_contexts(flat: &flat::Model) -> Result<(), ToDaeError> {
    for equation in &flat.equations {
        // `when` assignments and `reinit` are event actions: MLS §12.3 permits
        // an impure call there even though Flat renders them as equations.
        if matches!(
            equation.origin,
            flat::EquationOrigin::WhenAssignment { .. } | flat::EquationOrigin::Reinit { .. }
        ) {
            continue;
        }
        reject_impure_calls(&equation.residual, flat, "a continuous-time equation")?;
    }
    for algorithm in &flat.algorithms {
        for statement in &algorithm.statements {
            reject_impure_calls_in_statement(statement, flat, "a non-initial algorithm")?;
        }
    }
    for function in flat.functions.values() {
        if !function.pure {
            continue;
        }
        let context = "a pure function body";
        for statement in &function.body {
            reject_impure_calls_in_statement(statement, flat, context)?;
        }
        for value in function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
        {
            if let Some(default) = &value.default {
                reject_impure_calls(default, flat, context)?;
            }
        }
        if let Some(external) = &function.external {
            for argument in &external.args {
                reject_impure_calls(argument, flat, context)?;
            }
        }
    }
    Ok(())
}

fn reject_impure_calls_in_statement(
    statement: &rumoca_core::Statement,
    flat: &flat::Model,
    context: &str,
) -> Result<(), ToDaeError> {
    // A `when` statement body is an MLS §12.3 legal context, so its actions
    // are not visited here.
    if matches!(statement, rumoca_core::Statement::When { .. }) {
        return Ok(());
    }
    match statement {
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => {
            reject_impure_calls(value, flat, context)?;
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                reject_impure_calls(&index.range, flat, context)?;
            }
            for child in equations {
                reject_impure_calls_in_statement(child, flat, context)?;
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            reject_impure_calls(&block.cond, flat, context)?;
            for child in &block.stmts {
                reject_impure_calls_in_statement(child, flat, context)?;
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                reject_impure_calls(&block.cond, flat, context)?;
                for child in &block.stmts {
                    reject_impure_calls_in_statement(child, flat, context)?;
                }
            }
            for child in else_block.iter().flatten() {
                reject_impure_calls_in_statement(child, flat, context)?;
            }
        }
        rumoca_core::Statement::When { .. } => {}
        rumoca_core::Statement::FunctionCall {
            comp, args, span, ..
        } => {
            reject_impure_target(comp, flat, context, *span)?;
            for argument in args {
                reject_impure_calls(argument, flat, context)?;
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            reject_impure_calls(condition, flat, context)?;
            reject_impure_calls(message, flat, context)?;
            if let Some(level) = level {
                reject_impure_calls(level, flat, context)?;
            }
        }
    }
    Ok(())
}

/// A multi-output statement call names its callee directly, not through an
/// expression node, so it is proven against the same Flat identity.
fn reject_impure_target(
    target: &rumoca_core::Reference,
    flat: &flat::Model,
    context: &str,
    span: Span,
) -> Result<(), ToDaeError> {
    let function = target
        .resolved_function()
        .and_then(|resolved| flat.get_function_instance(resolved.instance_id));
    match function {
        Some(function) if !function.pure => Err(ToDaeError::unsupported_flat(
            "impure call context",
            format!(
                "impure function `{}` is called from {context}; MLS §12.3 permits an impure \
                 call only in an impure function, a `when` equation or statement, an initial \
                 equation or initial algorithm, or a `parameter` binding",
                function.name
            ),
            span,
        )),
        _ => Ok(()),
    }
}

fn reject_impure_calls(
    expression: &Expression,
    flat: &flat::Model,
    context: &str,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall { name, span, .. } = expression
        && let Some(function) = flat.functions.get(name.var_name())
        && !function.pure
    {
        return Err(ToDaeError::unsupported_flat(
            "impure call context",
            format!(
                "impure function `{}` is called from {context}; MLS §12.3 permits an impure \
                 call only in an impure function, a `when` equation or statement, an initial \
                 equation or initial algorithm, or a `parameter` binding",
                function.name
            ),
            *span,
        ));
    }
    for child in expression_children(expression) {
        reject_impure_calls(child, flat, context)?;
    }
    Ok(())
}
