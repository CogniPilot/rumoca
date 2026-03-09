use super::*;
use std::ops::ControlFlow::Continue;

pub(super) fn run_chained_relational_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = ChainedRelationalVisitor { diags: &mut diags };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct ChainedRelationalVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ChainedRelationalVisitor<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> std::ops::ControlFlow<()> {
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for algorithm in &class.algorithms {
            self.visit_each(algorithm, Self::visit_statement)?;
        }
        for nested in class.classes.values() {
            self.visit_class_def(nested)?;
        }
        Continue(())
    }

    fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
        if let Expression::Binary { op, lhs, rhs, .. } = expr
            && is_relational(op)
            && (expr_is_relational(lhs) || expr_is_relational(rhs))
            && let Some(token) = relational_op_token(op)
        {
            self.diags.push(semantic_error(
                ER039_CHAINED_RELATIONAL_OPERATOR,
                "chained relational operators are not allowed (MLS §3.2)",
                label_from_token(
                    token,
                    "check_chained_in_expr/chained_relational",
                    "chained relational operator",
                ),
            ));
        }
        walk_expression_default(self, expr)
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

pub(super) fn run_der_in_function_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = DerInFunctionVisitor {
        diags: &mut diags,
        in_function_class: false,
    };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct DerInFunctionVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
    in_function_class: bool,
}

impl ast::Visitor for DerInFunctionVisitor<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> std::ops::ControlFlow<()> {
        let previous = self.in_function_class;
        self.in_function_class = class.class_type == ClassType::Function;

        if self.in_function_class {
            for algorithm in &class.algorithms {
                self.visit_each(algorithm, Self::visit_statement)?;
            }
        }
        for nested in class.classes.values() {
            self.visit_class_def(nested)?;
        }

        self.in_function_class = previous;
        Continue(())
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if self.in_function_class
            && matches!(ctx, ast::FunctionCallContext::Expression)
            && let Some(first) = comp.parts.first()
            && &*first.ident.text == "der"
        {
            self.diags.push(semantic_error(
                ER030_DER_IN_FUNCTION,
                "der() is not allowed in functions (MLS §12.2)",
                label_from_token(
                    &first.ident,
                    "check_der_in_expr/der_in_function",
                    "der() is not allowed in function algorithms",
                ),
            ));
        }
        self.visit_each(args, Self::visit_expression)
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
    let mut visitor = DerOnDiscreteVisitor {
        discrete_vars,
        diags,
    };
    let _ = visitor.visit_equation(eq);
}

struct DerOnDiscreteVisitor<'a> {
    discrete_vars: &'a HashSet<String>,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for DerOnDiscreteVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        match eq {
            Equation::Simple { lhs, rhs } => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)
            }
            Equation::For { equations, .. } => self.visit_each(equations, Self::visit_equation),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                if let Some(else_eqs) = else_block {
                    self.visit_each(else_eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            Equation::When(blocks) => {
                for block in blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            _ => Continue(()),
        }
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ast::FunctionCallContext::Expression)
            && let Some(first) = comp.parts.first()
            && &*first.ident.text == "der"
            && let Some(arg) = args.first()
            && let Expression::ComponentReference(cref) = arg
            && let Some(part) = cref.parts.first()
            && self.discrete_vars.contains(&*part.ident.text)
        {
            self.diags.push(semantic_error(
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
        self.visit_each(args, Self::visit_expression)
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
    let mut visitor = ProtectedAccessVisitor { class, def, diags };
    let _ = visitor.visit_equation(eq);
}

struct ProtectedAccessVisitor<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ProtectedAccessVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        match eq {
            Equation::Simple { lhs, rhs } => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)
            }
            Equation::For { equations, .. } => self.visit_each(equations, Self::visit_equation),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                if let Some(else_eqs) = else_block {
                    self.visit_each(else_eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            Equation::When(blocks) => {
                for block in blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            _ => Continue(()),
        }
    }

    fn visit_component_reference_ctx(
        &mut self,
        cref: &ComponentReference,
        ctx: ast::ComponentReferenceContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ast::ComponentReferenceContext::Expression) {
            check_cref_protected_access(cref, self.class, self.def, self.diags);
        }
        ast::Visitor::visit_component_reference(self, cref)
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
    let mut visitor = ConnectRequiresConnectorsVisitor { class, def, diags };
    let _ = visitor.visit_equation(eq);
}

struct ConnectRequiresConnectorsVisitor<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ConnectRequiresConnectorsVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        match eq {
            Equation::Connect { lhs, rhs } => self.visit_connect(lhs, rhs),
            Equation::For { equations, .. } => self.visit_each(equations, Self::visit_equation),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                if let Some(else_eqs) = else_block {
                    self.visit_each(else_eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            _ => Continue(()),
        }
    }

    fn visit_connect(
        &mut self,
        lhs: &ComponentReference,
        rhs: &ComponentReference,
    ) -> std::ops::ControlFlow<()> {
        check_connect_arg_is_connector(lhs, self.class, self.def, self.diags);
        check_connect_arg_is_connector(rhs, self.class, self.def, self.diags);
        Continue(())
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
    let mut visitor = EndOutsideSubscriptVisitor {
        diags,
        subscript_depth: 0,
    };
    let _ = visitor.visit_equation(eq);
}

struct EndOutsideSubscriptVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
    subscript_depth: usize,
}

impl ast::Visitor for EndOutsideSubscriptVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        match eq {
            Equation::Simple { lhs, rhs } => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)
            }
            Equation::For { equations, .. } => self.visit_each(equations, Self::visit_equation),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                if let Some(else_eqs) = else_block {
                    self.visit_each(else_eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            Equation::When(blocks) => {
                for block in blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            _ => Continue(()),
        }
    }

    fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
        if self.subscript_depth == 0
            && let Expression::Terminal {
                terminal_type: TerminalType::End,
                token,
            } = expr
        {
            self.diags.push(semantic_error(
                ER031_END_OUTSIDE_SUBSCRIPT,
                "'end' can only be used within array subscripts (MLS §10.5.1)",
                label_from_token(
                    token,
                    "check_end_outside_subscript_expr/end_outside_subscript",
                    "'end' outside subscript",
                ),
            ));
        }
        walk_expression_default(self, expr)
    }

    fn visit_subscript_ctx(
        &mut self,
        sub: &Subscript,
        _ctx: ast::SubscriptContext,
    ) -> std::ops::ControlFlow<()> {
        if let Subscript::Expression(expr) = sub {
            self.subscript_depth += 1;
            self.visit_expression(expr)?;
            self.subscript_depth -= 1;
        }
        Continue(())
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
    let mut visitor = ExprTypeIssuesVisitor {
        class,
        def,
        real_vars,
        diags,
    };
    let _ = visitor.visit_equation(eq);
}

struct ExprTypeIssuesVisitor<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    real_vars: &'a HashSet<String>,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ExprTypeIssuesVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        match eq {
            Equation::Simple { lhs, rhs } => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)
            }
            Equation::For { equations, .. } => self.visit_each(equations, Self::visit_equation),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                if let Some(else_eqs) = else_block {
                    self.visit_each(else_eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            Equation::When(blocks) => {
                for block in blocks {
                    self.visit_each(&block.eqs, Self::visit_equation)?;
                }
                Continue(())
            }
            _ => Continue(()),
        }
    }

    fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
        match expr {
            // EXPR-002: Real equality/inequality comparison
            Expression::Binary {
                op: OpBinary::Eq(op_token) | OpBinary::Neq(op_token),
                lhs,
                rhs,
                ..
            } if expr_is_real(lhs, self.real_vars) || expr_is_real(rhs, self.real_vars) => {
                self.diags.push(semantic_error(
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
            // EXPR-016: non-Boolean if-expression condition
            Expression::If { branches, .. } => {
                for (cond, _) in branches {
                    emit_non_boolean_if_condition(cond, self.diags);
                }
            }
            // TYPE-005: Class name used as value in equation
            Expression::ComponentReference(cref) if cref.parts.len() == 1 => {
                let name = &*cref.parts[0].ident.text;
                if !self.class.components.contains_key(name)
                    && find_class_by_name(self.def, name).is_some()
                {
                    self.diags.push(semantic_error(
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
            _ => {}
        }
        walk_expression_default(self, expr)
    }
}

fn emit_non_boolean_if_condition(cond: &Expression, diags: &mut Vec<Diagnostic>) {
    if !expr_is_numeric_literal(cond) {
        return;
    }
    let Some(label) = label_from_expression(
        cond,
        "check_expr_type_issues/non_boolean_if_condition",
        "non-Boolean if-expression condition",
    ) else {
        return;
    };
    diags.push(semantic_error(
        ER010_IF_CONDITION_NOT_BOOLEAN,
        "if-expression condition must be Boolean, \
         not a numeric value (MLS §3.6.5)",
        label,
    ));
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
