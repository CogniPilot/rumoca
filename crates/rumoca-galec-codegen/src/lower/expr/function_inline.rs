use std::collections::{HashMap, HashSet};

use rumoca_core::{
    ComponentReference, Expression, ExpressionRewriter, Function, Span, Statement, StatementBlock,
    Subscript,
};

use crate::diagnostic::GalecTargetError;

pub(super) fn inline_function_call(
    function: &Function,
    args: &[Expression],
    span: Span,
) -> Result<Expression, GalecTargetError> {
    let [output] = function.outputs.as_slice() else {
        return Err(unsupported(
            format!("multi-output-user-function:{}", function.name.as_str()),
            format!(
                "function `{}` has {} outputs; GALEC inline lowering expects one",
                function.name.as_str(),
                function.outputs.len()
            ),
            Some(span),
        ));
    };
    inline_function_output_call(function, args, output.name.as_str(), span)
}

pub(super) fn inline_function_output_call(
    function: &Function,
    args: &[Expression],
    output_name: &str,
    span: Span,
) -> Result<Expression, GalecTargetError> {
    validate_inlineable_function(function, span)?;
    let mut env = bind_function_inputs(function, args, span)?;
    if !function
        .outputs
        .iter()
        .any(|output| output.name.as_str() == output_name)
    {
        return Err(unsupported(
            format!("user-function-output:{}", function.name.as_str()),
            format!(
                "function `{}` has no output `{output_name}`",
                function.name.as_str()
            ),
            Some(span),
        ));
    }
    let assignable_names = function
        .locals
        .iter()
        .chain(function.outputs.iter())
        .map(|param| param.name.as_str())
        .collect::<HashSet<_>>();
    for statement in &function.body {
        inline_statement(function, statement, &assignable_names, &mut env)?;
    }
    env.get(output_name).cloned().ok_or_else(|| {
        unsupported(
            format!("user-function-output:{}", function.name.as_str()),
            format!(
                "function `{}` does not assign its single output `{output_name}`",
                function.name.as_str()
            ),
            Some(span),
        )
    })
}

fn validate_inlineable_function(function: &Function, span: Span) -> Result<(), GalecTargetError> {
    if !function.pure {
        return Err(unsupported(
            format!("impure-user-function:{}", function.name.as_str()),
            format!(
                "impure function `{}` in a lowered expression",
                function.name.as_str()
            ),
            Some(span),
        ));
    }
    if function.external.is_some() {
        return Err(unsupported(
            format!("external-user-function:{}", function.name.as_str()),
            format!(
                "external function `{}` in a lowered expression",
                function.name.as_str()
            ),
            Some(span),
        ));
    }
    Ok(())
}

fn inline_statement(
    function: &Function,
    statement: &Statement,
    assignable_names: &HashSet<&str>,
    env: &mut HashMap<String, Expression>,
) -> Result<(), GalecTargetError> {
    match statement {
        Statement::Empty { .. } => Ok(()),
        Statement::Assignment { comp, value, span } => {
            inline_assignment(function, comp, value, *span, assignable_names, env)
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => inline_if_statement(
            function,
            cond_blocks,
            else_block,
            *span,
            assignable_names,
            env,
        ),
        _ => Err(unsupported(
            format!("user-function-body:{}", function.name.as_str()),
            format!(
                "function `{}` contains an unsupported statement in GALEC inline lowering",
                function.name.as_str()
            ),
            statement.source_span(),
        )),
    }
}

fn inline_assignment(
    function: &Function,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
    assignable_names: &HashSet<&str>,
    env: &mut HashMap<String, Expression>,
) -> Result<(), GalecTargetError> {
    let Some(target) = simple_function_assignment_target(comp) else {
        return Err(unsupported(
            format!("user-function-target:{}", function.name.as_str()),
            format!(
                "function `{}` assigns to a non-scalar or qualified target",
                function.name.as_str()
            ),
            Some(span),
        ));
    };
    if !assignable_names.contains(target) {
        return Err(unsupported(
            format!("user-function-target:{}", function.name.as_str()),
            format!(
                "function `{}` assigns to `{target}`, which is not its output or a local",
                function.name.as_str()
            ),
            Some(span),
        ));
    }
    let value = FunctionInlineSubstituter { env }.rewrite_expression(value);
    env.insert(target.to_owned(), value);
    Ok(())
}

fn inline_if_statement(
    function: &Function,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    span: Span,
    assignable_names: &HashSet<&str>,
    env: &mut HashMap<String, Expression>,
) -> Result<(), GalecTargetError> {
    let mut branches = Vec::with_capacity(cond_blocks.len());
    for block in cond_blocks {
        let condition = FunctionInlineSubstituter { env }.rewrite_expression(&block.cond);
        let branch_env = inline_statement_block(function, &block.stmts, assignable_names, env)?;
        branches.push((condition, branch_env));
    }
    let else_env = match else_block {
        Some(statements) => inline_statement_block(function, statements, assignable_names, env)?,
        None => env.clone(),
    };
    merge_if_environments(assignable_names, env, branches, else_env, span)
}

fn inline_statement_block(
    function: &Function,
    statements: &[Statement],
    assignable_names: &HashSet<&str>,
    env: &HashMap<String, Expression>,
) -> Result<HashMap<String, Expression>, GalecTargetError> {
    let mut branch_env = env.clone();
    for statement in statements {
        inline_statement(function, statement, assignable_names, &mut branch_env)?;
    }
    Ok(branch_env)
}

fn merge_if_environments(
    assignable_names: &HashSet<&str>,
    env: &mut HashMap<String, Expression>,
    branches: Vec<(Expression, HashMap<String, Expression>)>,
    else_env: HashMap<String, Expression>,
    span: Span,
) -> Result<(), GalecTargetError> {
    let original = env.clone();
    for target in assignable_names {
        if !environment_value_changed(target, &original, &branches, &else_env) {
            continue;
        }
        let target_branches = branches
            .iter()
            .map(|(condition, branch_env)| {
                Ok((
                    condition.clone(),
                    branch_env_value(target, branch_env, &original, span)?,
                ))
            })
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        let else_branch = branch_env_value(target, &else_env, &original, span)?;
        env.insert(
            (*target).to_owned(),
            Expression::If {
                branches: target_branches,
                else_branch: Box::new(else_branch),
                span,
            },
        );
    }
    Ok(())
}

fn environment_value_changed(
    target: &str,
    original: &HashMap<String, Expression>,
    branches: &[(Expression, HashMap<String, Expression>)],
    else_env: &HashMap<String, Expression>,
) -> bool {
    branches
        .iter()
        .any(|(_, branch_env)| branch_env.get(target) != original.get(target))
        || else_env.get(target) != original.get(target)
}

fn branch_env_value(
    target: &str,
    branch_env: &HashMap<String, Expression>,
    original: &HashMap<String, Expression>,
    span: Span,
) -> Result<Expression, GalecTargetError> {
    branch_env
        .get(target)
        .or_else(|| original.get(target))
        .cloned()
        .ok_or_else(|| {
            unsupported(
                "user-function-conditional-output".to_owned(),
                format!("conditional function assignment leaves `{target}` undefined"),
                Some(span),
            )
        })
}

fn bind_function_inputs(
    function: &Function,
    args: &[Expression],
    span: Span,
) -> Result<HashMap<String, Expression>, GalecTargetError> {
    if args.len() > function.inputs.len() {
        return Err(unsupported(
            format!("user-function-arity:{}", function.name.as_str()),
            format!(
                "function `{}` called with {} argument(s), expected at most {}",
                function.name.as_str(),
                args.len(),
                function.inputs.len()
            ),
            Some(span),
        ));
    }
    let mut env = HashMap::new();
    for (index, input) in function.inputs.iter().enumerate() {
        let value = match args.get(index) {
            Some(arg) => arg.clone(),
            None => input.default.clone().ok_or_else(|| {
                unsupported(
                    format!("user-function-arity:{}", function.name.as_str()),
                    format!(
                        "function `{}` call omitted required input `{}`",
                        function.name.as_str(),
                        input.name
                    ),
                    Some(span),
                )
            })?,
        };
        let value = FunctionInlineSubstituter { env: &env }.rewrite_expression(&value);
        env.insert(input.name.clone(), value);
    }
    Ok(env)
}

fn simple_function_assignment_target(comp: &ComponentReference) -> Option<&str> {
    let [part] = comp.parts.as_slice() else {
        return None;
    };
    part.subs.is_empty().then_some(part.ident.as_str())
}

struct FunctionInlineSubstituter<'a> {
    env: &'a HashMap<String, Expression>,
}

impl ExpressionRewriter for FunctionInlineSubstituter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if let Some(value) = self.env.get(name.as_str()) {
            if subscripts.is_empty() {
                return value.clone();
            }
            return Expression::Index {
                base: Box::new(value.clone()),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

fn unsupported(feature: String, detail: String, span: Option<Span>) -> GalecTargetError {
    GalecTargetError::UnsupportedFeature {
        feature,
        detail,
        span: span.filter(|span| !span.is_dummy()),
    }
}
