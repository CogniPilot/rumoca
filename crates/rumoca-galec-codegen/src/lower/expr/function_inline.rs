use std::collections::{HashMap, HashSet};

use rumoca_core::{
    ComponentReference, Expression, ExpressionRewriter, Function, Span, Statement, Subscript,
};

use crate::diagnostic::GalecTargetError;

pub(super) fn inline_function_call(
    function: &Function,
    args: &[Expression],
    span: Span,
) -> Result<Expression, GalecTargetError> {
    validate_inlineable_function(function, span)?;
    let mut env = bind_function_inputs(function, args, span)?;
    let output_name = function.outputs[0].name.as_str();
    let local_names = function
        .locals
        .iter()
        .map(|param| param.name.as_str())
        .collect::<HashSet<_>>();
    for statement in &function.body {
        let Statement::Assignment { comp, value, span } = statement else {
            if matches!(statement, Statement::Empty { .. }) {
                continue;
            }
            return Err(unsupported(
                format!("user-function-body:{}", function.name.as_str()),
                format!(
                    "function `{}` contains a non-assignment statement in GALEC inline lowering",
                    function.name.as_str()
                ),
                statement.source_span(),
            ));
        };
        let Some(target) = simple_function_assignment_target(comp) else {
            return Err(unsupported(
                format!("user-function-target:{}", function.name.as_str()),
                format!(
                    "function `{}` assigns to a non-scalar or qualified target",
                    function.name.as_str()
                ),
                Some(*span),
            ));
        };
        if target != output_name && !local_names.contains(target) {
            return Err(unsupported(
                format!("user-function-target:{}", function.name.as_str()),
                format!(
                    "function `{}` assigns to `{target}`, which is not its output or a local",
                    function.name.as_str()
                ),
                Some(*span),
            ));
        }
        let value = FunctionInlineSubstituter { env: &env }.rewrite_expression(value);
        env.insert(target.to_owned(), value);
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
    if function.outputs.len() != 1 {
        return Err(unsupported(
            format!("multi-output-user-function:{}", function.name.as_str()),
            format!(
                "function `{}` has {} outputs; GALEC inline lowering expects one",
                function.name.as_str(),
                function.outputs.len()
            ),
            Some(span),
        ));
    }
    Ok(())
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
