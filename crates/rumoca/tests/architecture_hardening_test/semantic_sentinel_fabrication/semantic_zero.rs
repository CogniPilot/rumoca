//! Syntax helpers for the identity-aware semantic-zero fabrication scan.

pub(super) fn expression_returns_successful_zero(expression: &syn::Expr) -> bool {
    if is_zero_construction(expression) {
        return true;
    }
    match expression {
        syn::Expr::Return(return_expression) => return_expression
            .expr
            .as_deref()
            .is_some_and(is_zero_construction),
        syn::Expr::Block(block) => block_contains_successful_zero(&block.block),
        syn::Expr::Group(group) => expression_returns_successful_zero(&group.expr),
        syn::Expr::Paren(paren) => expression_returns_successful_zero(&paren.expr),
        syn::Expr::If(expression) => {
            block_contains_successful_zero(&expression.then_branch)
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_returns_successful_zero(branch))
        }
        syn::Expr::Match(expression) => expression
            .arms
            .iter()
            .any(|arm| expression_returns_successful_zero(&arm.body)),
        _ => false,
    }
}

pub(super) fn block_contains_successful_zero(block: &syn::Block) -> bool {
    block.stmts.iter().any(|statement| match statement {
        syn::Stmt::Expr(expression, semicolon) => {
            if semicolon.is_none() {
                expression_returns_successful_zero(expression)
            } else {
                matches!(expression, syn::Expr::Return(_))
                    && expression_returns_successful_zero(expression)
            }
        }
        syn::Stmt::Local(_) | syn::Stmt::Item(_) | syn::Stmt::Macro(_) => false,
    })
}

pub(super) fn expression_returns_ok_result_deep(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Call(call) => super::path_expression_ends_with(call.func.as_ref(), "Ok"),
        syn::Expr::Return(expression) => expression
            .expr
            .as_deref()
            .is_some_and(expression_returns_ok_result_deep),
        syn::Expr::Block(expression) => block_returns_ok_result_deep(&expression.block),
        syn::Expr::Group(expression) => expression_returns_ok_result_deep(&expression.expr),
        syn::Expr::Paren(expression) => expression_returns_ok_result_deep(&expression.expr),
        syn::Expr::If(expression) => {
            block_returns_ok_result_deep(&expression.then_branch)
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_returns_ok_result_deep(branch))
        }
        syn::Expr::Match(expression) => expression
            .arms
            .iter()
            .any(|arm| expression_returns_ok_result_deep(&arm.body)),
        _ => false,
    }
}

fn block_returns_ok_result_deep(block: &syn::Block) -> bool {
    if block.stmts.iter().any(|statement| match statement {
        syn::Stmt::Expr(expression, _) => expression_contains_ok_return(expression),
        syn::Stmt::Local(local) => local
            .init
            .as_ref()
            .is_some_and(|init| expression_contains_ok_return(&init.expr)),
        syn::Stmt::Item(_) | syn::Stmt::Macro(_) => false,
    }) {
        return true;
    }
    block.stmts.last().is_some_and(|statement| {
        matches!(statement, syn::Stmt::Expr(expression, None) if expression_returns_ok_result_deep(expression))
    })
}

fn expression_contains_ok_return(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Return(_) => expression_returns_ok_result_deep(expression),
        syn::Expr::Block(expression) => block_returns_ok_result_deep(&expression.block),
        syn::Expr::Group(expression) => expression_contains_ok_return(&expression.expr),
        syn::Expr::Paren(expression) => expression_contains_ok_return(&expression.expr),
        syn::Expr::If(expression) => {
            block_returns_ok_result_deep(&expression.then_branch)
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_contains_ok_return(branch))
        }
        syn::Expr::Match(expression) => expression
            .arms
            .iter()
            .any(|arm| expression_contains_ok_return(&arm.body)),
        syn::Expr::Loop(expression) => block_returns_ok_result_deep(&expression.body),
        syn::Expr::While(expression) => block_returns_ok_result_deep(&expression.body),
        syn::Expr::ForLoop(expression) => block_returns_ok_result_deep(&expression.body),
        _ => false,
    }
}

pub(super) fn expression_looks_semantic(expression: &syn::Expr) -> bool {
    let name = match expression {
        syn::Expr::Path(path) => path.path.segments.last().map(|part| part.ident.to_string()),
        syn::Expr::Field(field) => match &field.member {
            syn::Member::Named(name) => Some(name.to_string()),
            syn::Member::Unnamed(_) => None,
        },
        _ => None,
    };
    name.is_some_and(|name| {
        matches!(
            name.as_str(),
            "expr" | "expression" | "body_expr" | "lhs" | "rhs"
        )
    })
}

fn is_zero_construction(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Lit(literal) => match &literal.lit {
            syn::Lit::Int(value) => value.base10_parse::<u128>().is_ok_and(|value| value == 0),
            syn::Lit::Float(value) => value.base10_parse::<f64>().is_ok_and(|value| value == 0.0),
            syn::Lit::Str(value) => value.value() == "0",
            _ => false,
        },
        syn::Expr::Call(call) => {
            if is_numeric_default_call(call) {
                return call.args.is_empty();
            }
            call.args.len() == 1
                && call_path_ends_with_zero_wrapper(call)
                && call.args.first().is_some_and(is_zero_construction)
        }
        syn::Expr::MethodCall(call) => {
            matches!(
                call.method.to_string().as_str(),
                "into" | "to_owned" | "to_string"
            ) && call.args.is_empty()
                && is_zero_construction(&call.receiver)
        }
        syn::Expr::Cast(cast) => is_zero_construction(&cast.expr),
        syn::Expr::Group(group) => is_zero_construction(&group.expr),
        syn::Expr::Paren(paren) => is_zero_construction(&paren.expr),
        _ => false,
    }
}

fn call_path_ends_with_zero_wrapper(call: &syn::ExprCall) -> bool {
    let syn::Expr::Path(path) = call.func.as_ref() else {
        return false;
    };
    path.path.segments.last().is_some_and(|segment| {
        matches!(
            segment.ident.to_string().as_str(),
            "Ok" | "Some" | "Integer" | "Real" | "Scalar" | "Number" | "Float"
        )
    })
}

fn is_numeric_default_call(call: &syn::ExprCall) -> bool {
    let syn::Expr::Path(path) = call.func.as_ref() else {
        return false;
    };
    let mut segments = path.path.segments.iter().rev();
    let Some(method) = segments.next() else {
        return false;
    };
    let Some(ty) = segments.next() else {
        return false;
    };
    method.ident == "default"
        && matches!(
            ty.ident.to_string().as_str(),
            "i8" | "i16"
                | "i32"
                | "i64"
                | "i128"
                | "isize"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "u128"
                | "usize"
                | "f32"
                | "f64"
        )
}
