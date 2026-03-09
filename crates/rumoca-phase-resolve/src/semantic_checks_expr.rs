use super::*;

pub(super) fn run_chained_relational_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for class in def.classes.values() {
        check_chained_in_class(class, &mut diags);
    }
    diags
}

fn check_chained_in_class(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for eq in &class.equations {
        check_chained_in_eq(eq, diags);
    }
    for eq in &class.initial_equations {
        check_chained_in_eq(eq, diags);
    }
    for alg in &class.algorithms {
        for stmt in alg {
            check_chained_in_stmt(stmt, diags);
        }
    }
    for nested in class.classes.values() {
        check_chained_in_class(nested, diags);
    }
}

fn check_chained_in_eq(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_chained_in_expr(lhs, diags);
            check_chained_in_expr(rhs, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_chained_in_eq(inner, diags);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_chained_in_stmt(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::Assignment { value, .. } => check_chained_in_expr(value, diags),
        Statement::For { equations, .. } => {
            for inner in equations {
                check_chained_in_stmt(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
        }
        _ => {}
    }
}

fn is_relational(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Lt(_)
            | OpBinary::Le(_)
            | OpBinary::Gt(_)
            | OpBinary::Ge(_)
            | OpBinary::Eq(_)
            | OpBinary::Neq(_)
    )
}

fn expr_is_relational(expr: &Expression) -> bool {
    matches!(expr, Expression::Binary { op, .. } if is_relational(op))
}

fn relational_op_token(op: &OpBinary) -> Option<&Token> {
    match op {
        OpBinary::Lt(token)
        | OpBinary::Le(token)
        | OpBinary::Gt(token)
        | OpBinary::Ge(token)
        | OpBinary::Eq(token)
        | OpBinary::Neq(token) => Some(token),
        _ => None,
    }
}

fn check_chained_in_expr(expr: &Expression, diags: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            if is_relational(op) && (expr_is_relational(lhs) || expr_is_relational(rhs)) {
                let Some(token) = relational_op_token(op) else {
                    return;
                };
                diags.push(semantic_error(
                    ER039_CHAINED_RELATIONAL_OPERATOR,
                    "chained relational operators are not allowed (MLS §3.2)",
                    label_from_token(
                        token,
                        "check_chained_in_expr/chained_relational",
                        "chained relational operator",
                    ),
                ));
            }
            check_chained_in_expr(lhs, diags);
            check_chained_in_expr(rhs, diags);
        }
        Expression::Unary { rhs, .. } => check_chained_in_expr(rhs, diags),
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_chained_in_expr(arg, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_chained_in_expr(cond, diags);
                check_chained_in_expr(then_expr, diags);
            }
            check_chained_in_expr(else_branch, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_chained_in_expr(elem, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => check_chained_in_expr(inner, diags),
        _ => {}
    }
}

pub(super) fn run_der_in_function_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for class in def.classes.values() {
        check_der_in_func_class(class, &mut diags);
    }
    diags
}

fn check_der_in_func_class(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type == ClassType::Function {
        for alg in &class.algorithms {
            for stmt in alg {
                check_der_in_stmt(stmt, diags);
            }
        }
    }
    for nested in class.classes.values() {
        check_der_in_func_class(nested, diags);
    }
}

fn check_der_in_stmt(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::Assignment { value, .. } => check_der_in_expr(value, diags),
        Statement::For { equations, .. } => {
            for inner in equations {
                check_der_in_stmt(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_der_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_der_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_der_in_expr(arg, diags);
            }
        }
        _ => {}
    }
}

fn check_der_in_expr(expr: &Expression, diags: &mut Vec<Diagnostic>) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            if let Some(first) = comp.parts.first()
                && &*first.ident.text == "der"
            {
                diags.push(semantic_error(
                    ER030_DER_IN_FUNCTION,
                    "der() is not allowed in functions (MLS §12.2)",
                    label_from_token(
                        &first.ident,
                        "check_der_in_expr/der_in_function",
                        "der() is not allowed in function algorithms",
                    ),
                ));
            }
            for arg in args {
                check_der_in_expr(arg, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_der_in_expr(lhs, diags);
            check_der_in_expr(rhs, diags);
        }
        Expression::Unary { rhs, .. } => check_der_in_expr(rhs, diags),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_der_in_expr(cond, diags);
                check_der_in_expr(then_expr, diags);
            }
            check_der_in_expr(else_branch, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_der_in_expr(elem, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => check_der_in_expr(inner, diags),
        _ => {}
    }
}

// ============================================================================
// DECL-020: der() on discrete variables
// ============================================================================

/// Check equations for der() applied to discrete variables.
pub(super) fn check_der_on_discrete_eq(
    eq: &Equation,
    discrete_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_der_on_discrete_expr(lhs, discrete_vars, diags);
            check_der_on_discrete_expr(rhs, discrete_vars, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_der_on_discrete_eq(inner, discrete_vars, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_der_on_discrete_expr(
    expr: &Expression,
    discrete_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            if let Some(first) = comp.parts.first()
                && &*first.ident.text == "der"
            {
                // Check if the argument is a discrete variable
                if let Some(arg) = args.first()
                    && let Expression::ComponentReference(cref) = arg
                    && let Some(part) = cref.parts.first()
                    && discrete_vars.contains(&*part.ident.text)
                {
                    diags.push(semantic_error(
                        ER026_DER_ON_DISCRETE,
                        format!(
                            "der() cannot be applied to discrete variable '{}' (MLS §3.8.5)",
                            part.ident.text
                        ),
                        label_from_token(
                            &part.ident,
                            "check_der_on_discrete_expr/discrete_argument",
                            format!("discrete variable '{}' passed to der()", part.ident.text),
                        ),
                    ));
                }
            }
            for arg in args {
                check_der_on_discrete_expr(arg, discrete_vars, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_der_on_discrete_expr(lhs, discrete_vars, diags);
            check_der_on_discrete_expr(rhs, discrete_vars, diags);
        }
        Expression::Unary { rhs, .. } => check_der_on_discrete_expr(rhs, discrete_vars, diags),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_der_on_discrete_expr(cond, discrete_vars, diags);
                check_der_on_discrete_expr(then_expr, discrete_vars, diags);
            }
            check_der_on_discrete_expr(else_branch, discrete_vars, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_der_on_discrete_expr(elem, discrete_vars, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_der_on_discrete_expr(inner, discrete_vars, diags);
        }
        _ => {}
    }
}

// ============================================================================
// DECL-009: Protected dot access
// ============================================================================

/// Check equations for protected component access (e.g., `a.x` where x is protected).
pub(super) fn check_protected_access_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_protected_access_expr(lhs, class, def, diags);
            check_protected_access_expr(rhs, class, def, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_protected_access_eq(inner, class, def, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_protected_access_expr(
    expr: &Expression,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::ComponentReference(cref) => {
            check_cref_protected_access(cref, class, def, diags);
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_protected_access_expr(lhs, class, def, diags);
            check_protected_access_expr(rhs, class, def, diags);
        }
        Expression::Unary { rhs, .. } => {
            check_protected_access_expr(rhs, class, def, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_protected_access_expr(arg, class, def, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_protected_access_expr(cond, class, def, diags);
                check_protected_access_expr(then_expr, class, def, diags);
            }
            check_protected_access_expr(else_branch, class, def, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_protected_access_expr(elem, class, def, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_protected_access_expr(inner, class, def, diags);
        }
        _ => {}
    }
}

// ============================================================================
// CONN-029: Connect requires connectors
// ============================================================================

/// Check that connect() arguments refer to connector types.
/// Check if a component reference accesses a protected member.
fn check_cref_protected_access(
    cref: &ComponentReference,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if cref.parts.len() < 2 {
        return;
    }
    let first_name = &*cref.parts[0].ident.text;
    let second_name = &*cref.parts[1].ident.text;

    let Some(comp) = class.components.get(first_name) else {
        return;
    };
    let type_name = comp.type_name.to_string();
    let Some(type_class) = find_class_by_name(def, &type_name) else {
        return;
    };
    if let Some(target) = type_class.components.get(second_name)
        && target.is_protected
    {
        diags.push(semantic_error(
            ER025_PROTECTED_DOT_ACCESS,
            format!(
                "cannot access protected component '{}.{}' (MLS §5.3)",
                first_name, second_name
            ),
            label_from_token(
                &cref.parts[1].ident,
                "check_cref_protected_access/protected_member_access",
                format!("protected member '{}'", second_name),
            ),
        ));
    }
}

pub(super) fn check_connect_requires_connectors_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Connect { lhs, rhs, .. } => {
            check_connect_arg_is_connector(lhs, class, def, diags);
            check_connect_arg_is_connector(rhs, class, def, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_connect_requires_connectors_eq(inner, class, def, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_connect_requires_connectors_eq(inner, class, def, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_connect_requires_connectors_eq(inner, class, def, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_connect_arg_is_connector(
    cref: &ComponentReference,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    // Only check single-part references (e.g., connect(x, y)).
    // Multi-part references like connect(r1.p, r2.n) access sub-components
    // which may be connectors even if the parent is a model.
    if cref.parts.len() != 1 {
        return;
    }
    if let Some(first) = cref.parts.first() {
        let comp_name = &*first.ident.text;
        if let Some(comp) = class.components.get(comp_name) {
            let type_name = comp.type_name.to_string();
            // Built-in types (Real, Integer, Boolean, String) are not connectors
            if matches!(
                type_name.as_str(),
                "Real" | "Integer" | "Boolean" | "String"
            ) {
                diags.push(semantic_error(
                    ER009_CONNECT_ARG_NOT_CONNECTOR,
                    format!(
                        "connect argument '{}' must be a connector, but has type '{}' (MLS §9.1)",
                        comp_name, type_name
                    ),
                    label_from_token(
                        &first.ident,
                        "check_connect_arg_is_connector/builtin_non_connector",
                        format!("'{}' is not a connector type", comp_name),
                    ),
                ));
                return;
            }
            // Look up the type class
            if let Some(type_class) = find_class_by_name(def, &type_name)
                && type_class.class_type != ClassType::Connector
            {
                diags.push(semantic_error(
                    ER009_CONNECT_ARG_NOT_CONNECTOR,
                    format!(
                        "connect argument '{}' must be a connector, but '{}' is a {} (MLS §9.1)",
                        comp_name,
                        type_name,
                        type_class.class_type.as_str()
                    ),
                    label_from_token(
                        &first.ident,
                        "check_connect_arg_is_connector/non_connector_type",
                        format!("'{}' does not resolve to a connector", comp_name),
                    ),
                ));
            }
        }
    }
}

// ============================================================================
// EXPR-013: 'end' outside subscript context
// ============================================================================

/// Check equations for 'end' used outside of array subscripts.
pub(super) fn check_end_outside_subscript_eq(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_end_outside_subscript_expr(lhs, false, diags);
            check_end_outside_subscript_expr(rhs, false, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_end_outside_subscript_eq(inner, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
        }
        _ => {}
    }
}

/// Check an expression for 'end' used outside subscript context.
/// `in_subscript` tracks whether we're inside a subscript.
fn check_end_outside_subscript_expr(
    expr: &Expression,
    in_subscript: bool,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::End,
            token,
        } if !in_subscript => {
            diags.push(semantic_error(
                ER031_END_OUTSIDE_SUBSCRIPT,
                "'end' can only be used within array subscripts (MLS §10.5.1)",
                label_from_token(
                    token,
                    "check_end_outside_subscript_expr/end_outside_subscript",
                    "'end' outside subscript",
                ),
            ));
        }
        Expression::ComponentReference(cref) => {
            // Subscripts inside component references count as subscript context
            let subs = cref.parts.iter().filter_map(|p| p.subs.as_ref()).flatten();
            for sub in subs {
                check_end_outside_subscript_subscript(sub, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_end_outside_subscript_expr(lhs, in_subscript, diags);
            check_end_outside_subscript_expr(rhs, in_subscript, diags);
        }
        Expression::Unary { rhs, .. } => {
            check_end_outside_subscript_expr(rhs, in_subscript, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_end_outside_subscript_expr(arg, in_subscript, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_end_outside_subscript_expr(cond, in_subscript, diags);
                check_end_outside_subscript_expr(then_expr, in_subscript, diags);
            }
            check_end_outside_subscript_expr(else_branch, in_subscript, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_end_outside_subscript_expr(elem, in_subscript, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_end_outside_subscript_expr(inner, in_subscript, diags);
        }
        _ => {}
    }
}

fn check_end_outside_subscript_subscript(sub: &Subscript, diags: &mut Vec<Diagnostic>) {
    match sub {
        Subscript::Expression(expr) => {
            // Inside a subscript, 'end' is valid
            check_end_outside_subscript_expr(expr, true, diags);
        }
        Subscript::Empty | Subscript::Range { .. } => {}
    }
}

// ============================================================================
// EXPR-002: Real equality, EXPR-016: non-Boolean if, TYPE-005: class as value
// ============================================================================

/// Check equations for expression-level type issues that can be detected without
/// full type inference.
pub(super) fn check_expr_type_issues_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    real_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_expr_type_issues(lhs, class, def, real_vars, diags);
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_expr_type_issues_eq(inner, class, def, real_vars, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_expr_type_issues(
    expr: &Expression,
    class: &ClassDef,
    def: &StoredDefinition,
    real_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        // EXPR-002: Real equality/inequality comparison
        Expression::Binary { op, lhs, rhs, .. } => {
            if matches!(op, OpBinary::Eq(_) | OpBinary::Neq(_))
                && (expr_is_real(lhs, real_vars) || expr_is_real(rhs, real_vars))
            {
                let op_token = match op {
                    OpBinary::Eq(token) | OpBinary::Neq(token) => token,
                    _ => unreachable!(),
                };
                diags.push(semantic_error(
                    ER029_REAL_EQUALITY_COMPARISON,
                    "equality comparison on Real values is not allowed \
                     outside functions (MLS §3.5)",
                    label_from_token(
                        op_token,
                        "check_expr_type_issues/real_equality",
                        "Real equality comparison",
                    ),
                ));
            }
            check_expr_type_issues(lhs, class, def, real_vars, diags);
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        // EXPR-016: non-Boolean if-expression condition
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                if expr_is_numeric_literal(cond)
                    && let Some(label) = label_from_expression(
                        cond,
                        "check_expr_type_issues/non_boolean_if_condition",
                        "non-Boolean if-expression condition",
                    )
                {
                    diags.push(semantic_error(
                        ER010_IF_CONDITION_NOT_BOOLEAN,
                        "if-expression condition must be Boolean, \
                         not a numeric value (MLS §3.6.5)",
                        label,
                    ));
                }
                check_expr_type_issues(cond, class, def, real_vars, diags);
                check_expr_type_issues(then_expr, class, def, real_vars, diags);
            }
            check_expr_type_issues(else_branch, class, def, real_vars, diags);
        }
        // TYPE-005: Class name used as value in equation
        Expression::ComponentReference(cref) if cref.parts.len() == 1 => {
            let name = &*cref.parts[0].ident.text;
            // Check if this refers to a class rather than a component
            if !class.components.contains_key(name) && find_class_by_name(def, name).is_some() {
                diags.push(semantic_error(
                    ER011_CLASS_USED_AS_VALUE,
                    format!(
                        "'{}' is a class, not a variable; cannot be used as a value (MLS §4.4)",
                        name
                    ),
                    label_from_token(
                        &cref.parts[0].ident,
                        "check_expr_type_issues/class_used_as_value",
                        format!("class '{}' used as value", name),
                    ),
                ));
            }
        }
        Expression::Unary { rhs, .. } => {
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_type_issues(arg, class, def, real_vars, diags);
            }
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expr_type_issues(elem, class, def, real_vars, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_expr_type_issues(inner, class, def, real_vars, diags);
        }
        _ => {}
    }
}

/// Check if an expression is clearly a Real-typed component reference.
fn expr_is_real(expr: &Expression, real_vars: &HashSet<String>) -> bool {
    if let Expression::ComponentReference(cref) = expr
        && let Some(first) = cref.parts.first()
    {
        return real_vars.contains(&*first.ident.text);
    }
    // Real literals
    if let Expression::Terminal {
        terminal_type: TerminalType::UnsignedReal,
        ..
    } = expr
    {
        return true;
    }
    false
}

/// Check if an expression is a numeric literal (not Boolean).
fn expr_is_numeric_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger | TerminalType::UnsignedReal,
            ..
        }
    )
}
