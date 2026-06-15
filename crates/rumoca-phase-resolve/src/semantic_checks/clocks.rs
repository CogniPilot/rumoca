use super::*;
use rumoca_ir_ast::ExpressionContext;
use std::ops::ControlFlow::Continue;

pub(super) const ER063_CLOCK_INVALID_PREFIX: &str = "ER063";
pub(super) const ER069_DER_ON_CLOCK_OPERATOR: &str = "ER069";
pub(super) const ER082_CLOCK_INTERVAL_POSITIVE: &str = "ER082";
pub(super) const ER097_CLOCKED_WHEN_IN_ALGORITHM: &str = "ER097";
pub(super) const ER109_CLOCK_OPERATOR_COMPONENT_ARG: &str = "ER109";
pub(super) const ER119_EVENT_CLOCK_RESAMPLING: &str = "ER119";
pub(super) const ER126_CLOCK_PARTITION_CONFLICT: &str = "ER126";
pub(super) const ER127_CLOCKED_VAR_CONTINUOUS_ACCESS: &str = "ER127";
pub(super) const ER128_CLOCK_SUBSCRIPT_EVALUABLE: &str = "ER128";

pub(super) fn check_clock_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    check_clocked_when_in_algorithms(class, def, diags);
    check_event_clock_resampling(class, diags);
    check_clock_partitions(class, def, diags);
    for (name, comp) in &class.components {
        let Some(ResolvedTypeRoot::Builtin("Clock")) = resolve_component_type_root(comp, def)
        else {
            continue;
        };

        match &comp.connection {
            Connection::Flow(token) => diags.push(clock_prefix_error(
                name,
                "flow",
                token,
                "check_clock_restrictions/flow_prefix",
            )),
            Connection::Stream(token) => diags.push(clock_prefix_error(
                name,
                "stream",
                token,
                "check_clock_restrictions/stream_prefix",
            )),
            Connection::Empty => {}
        }

        match &comp.variability {
            Variability::Discrete(token) => diags.push(clock_prefix_error(
                name,
                "discrete",
                token,
                "check_clock_restrictions/discrete_prefix",
            )),
            Variability::Parameter(token) => diags.push(clock_prefix_error(
                name,
                "parameter",
                token,
                "check_clock_restrictions/parameter_prefix",
            )),
            Variability::Constant(token) => diags.push(clock_prefix_error(
                name,
                "constant",
                token,
                "check_clock_restrictions/constant_prefix",
            )),
            Variability::Continuous(_) | Variability::Empty => {}
        }
    }
}

fn clock_prefix_error(
    component_name: &str,
    prefix: &str,
    token: &Token,
    context: &str,
) -> Diagnostic {
    semantic_error(
        ER063_CLOCK_INVALID_PREFIX,
        format!(
            "Clock variable '{}' cannot have '{}' prefix (MLS §16.3)",
            component_name, prefix
        ),
        label_from_token(
            token,
            context,
            format!("invalid '{}' prefix on Clock variable", prefix),
        ),
    )
}

pub(super) fn run_clock_expression_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = ClockExpressionVisitor { diags: &mut diags };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct ClockExpressionVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ClockExpressionVisitor<'_> {
    fn visit_expression_ctx(
        &mut self,
        expr: &Expression,
        ctx: ExpressionContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ExpressionContext::ComponentAnnotation) {
            return Continue(());
        }
        self.visit_expression(expr)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        // MLS §16.4, §16.5.1 / CLK-008, CLK-009: previous() and hold() take a
        // component expression (a component reference), not a general
        // expression.
        if let Some(name @ ("previous" | "hold")) = builtin_name(comp)
            && let Some(first) = args
                .iter()
                .find(|arg| !matches!(arg, Expression::NamedArgument { .. }))
            && !matches!(
                first,
                Expression::ComponentReference(_) | Expression::FieldAccess { .. }
            )
            && let Some(token) = comp.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER109_CLOCK_OPERATOR_COMPONENT_ARG,
                format!("{name}() requires a component expression as its argument (MLS §16.4)"),
                label_from_token(
                    token,
                    "run_clock_expression_semantic_checks/clock_operator_component_arg",
                    "pass a variable reference, not a general expression",
                ),
            ));
        }
        if let Some("Clock") = builtin_name(comp) {
            // MLS §16.3 / CLK-018, CLK-019: Clock(interval) and
            // Clock(intervalCounter, resolution) require strictly positive
            // first arguments; reject literal violations.
            let first_positional = args
                .iter()
                .find(|arg| !matches!(arg, Expression::NamedArgument { .. }));
            if let Some(expr) = first_positional
                && let Some(value) = numeric_literal_value(expr)
                && value <= 0.0
                && let Some(token) = comp.parts.first().map(|part| &part.ident)
            {
                self.diags.push(semantic_error(
                    ER082_CLOCK_INTERVAL_POSITIVE,
                    format!("Clock() interval must be strictly positive, got {value} (MLS §16.3)"),
                    label_from_token(
                        token,
                        "run_clock_expression_semantic_checks/clock_interval_positive",
                        "clock intervals must be > 0",
                    ),
                ));
            }
        }
        if matches!(ctx, ast::FunctionCallContext::Expression)
            && let Some("der") = builtin_name(comp)
            && let Some(Expression::FunctionCall {
                comp: inner_comp, ..
            }) = args.first()
            && let Some(inner_name) = builtin_name(inner_comp)
            && matches!(
                inner_name,
                "sample" | "subSample" | "superSample" | "shiftSample" | "backSample" | "noClock"
            )
            && let Some(inner_token) = inner_comp.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER069_DER_ON_CLOCK_OPERATOR,
                format!(
                    "der() cannot be applied to {}() expressions (MLS §16.5.2)",
                    inner_name
                ),
                label_from_token(
                    inner_token,
                    "run_clock_expression_semantic_checks/der_on_clock_operator",
                    format!("{inner_name}() cannot appear under der()"),
                ),
            ));
        }

        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

/// MLS §16.6 / CLK-015: clocked when-clauses cannot appear inside algorithm
/// sections; only when-equations may be clocked.
fn check_clocked_when_in_algorithms(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for section in class
        .algorithms
        .iter()
        .chain(class.initial_algorithms.iter())
    {
        for stmt in section {
            check_clocked_when_statement(stmt, class, def, diags);
        }
    }
}

fn check_clocked_when_statement(
    stmt: &Statement,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let Statement::When(blocks) = stmt else {
        return;
    };
    for block in blocks {
        if let Some(token) = clock_condition_token(&block.cond, class, def) {
            diags.push(semantic_error(
                ER097_CLOCKED_WHEN_IN_ALGORITHM,
                "clocked when-clauses are not allowed inside algorithms (MLS §16.6)".to_string(),
                label_from_token(
                    token,
                    "check_clock_restrictions/clocked_when_in_algorithm",
                    "use a clocked when-equation instead",
                ),
            ));
        }
    }
}

/// Token of a clocked when-condition: a Clock() constructor or a component
/// reference whose declared type resolves to the builtin Clock type.
fn clock_condition_token<'a>(
    cond: &'a Expression,
    class: &ClassDef,
    def: &StoredDefinition,
) -> Option<&'a Token> {
    match cond {
        Expression::FunctionCall { comp, .. } if builtin_name(comp) == Some("Clock") => {
            comp.parts.first().map(|part| &part.ident)
        }
        Expression::ComponentReference(comp) => {
            let [part] = comp.parts.as_slice() else {
                return None;
            };
            let component = class.components.get(part.ident.text.as_ref())?;
            matches!(
                resolve_component_type_root(component, def),
                Some(ResolvedTypeRoot::Builtin("Clock"))
            )
            .then_some(&part.ident)
        }
        _ => None,
    }
}

/// Evaluate Real/Integer literals (including a leading unary minus).
pub(super) fn numeric_literal_value(expr: &Expression) -> Option<f64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger | TerminalType::UnsignedReal,
            token,
            ..
        } => token.text.parse().ok(),
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => numeric_literal_value(rhs).map(|value| -value),
        Expression::Parenthesized { inner, .. } => numeric_literal_value(inner),
        _ => None,
    }
}

/// MLS §16.5.2 / CLK-010, CLK-011, CLK-012: event clocks (Clock(condition))
/// have no fixed interval, so superSample() cannot create faster clocks from
/// them, shiftSample() resolution must stay 1, and backSample() cannot create
/// ticks before the (unknown) base-clock start.
fn check_event_clock_resampling(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let event_clocks: HashSet<&str> = class
        .components
        .iter()
        .filter(|(_, comp)| {
            comp.binding
                .as_ref()
                .is_some_and(|binding| binding_is_event_clock(binding, class))
        })
        .map(|(name, _)| name.as_str())
        .collect();
    if event_clocks.is_empty() {
        return;
    }

    let mut collector = EventClockOpCollector {
        event_clocks: &event_clocks,
        found: Vec::new(),
    };
    for (_, comp) in &class.components {
        if let Some(binding) = comp.binding.as_ref() {
            let _ = collector.visit_expression(binding);
        }
    }
    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        let _ = collector.visit_equation(eq);
    }
    for (operator, token) in collector.found {
        let reason = match operator.as_str() {
            "superSample" => "superSample() cannot create faster clocks from an event clock",
            "shiftSample" => {
                "shiftSample() on an event clock cannot have a resolution other than 1"
            }
            _ => "backSample() on an event clock would create ticks before the base clock starts",
        };
        diags.push(semantic_error(
            ER119_EVENT_CLOCK_RESAMPLING,
            format!("{reason} (MLS §16.5.2)"),
            label_from_token(
                &token,
                "check_clock_restrictions/event_clock_resampling",
                "event clocks have no fixed interval to resample",
            ),
        ));
    }
}

/// A Clock(...) constructor whose first argument is Boolean-valued declares an
/// event clock (MLS §16.3).
fn binding_is_event_clock(binding: &Expression, class: &ClassDef) -> bool {
    let Expression::FunctionCall { comp, args, .. } = binding else {
        return false;
    };
    if builtin_name(comp) != Some("Clock") {
        return false;
    }
    let Some(first) = args
        .iter()
        .find(|arg| !matches!(arg, Expression::NamedArgument { .. }))
    else {
        return false;
    };
    match first {
        Expression::Binary { op, .. } => matches!(
            op,
            OpBinary::Lt
                | OpBinary::Le
                | OpBinary::Gt
                | OpBinary::Ge
                | OpBinary::Eq
                | OpBinary::Neq
        ),
        Expression::ComponentReference(cref) => {
            let [part] = cref.parts.as_slice() else {
                return false;
            };
            class
                .components
                .get(part.ident.text.as_ref())
                .is_some_and(|component| component.type_name.to_string() == "Boolean")
        }
        _ => false,
    }
}

struct EventClockOpCollector<'a> {
    event_clocks: &'a HashSet<&'a str>,
    found: Vec<(String, Token)>,
}

impl ast::Visitor for EventClockOpCollector<'_> {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if let Some(name @ ("superSample" | "shiftSample" | "backSample")) = builtin_name(comp)
            && let Some(Expression::ComponentReference(arg)) = args
                .iter()
                .find(|arg| !matches!(arg, Expression::NamedArgument { .. }))
            && let [part] = arg.parts.as_slice()
            && self.event_clocks.contains(part.ident.text.as_ref())
        {
            self.found.push((name.to_string(), part.ident.clone()));
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

/// AST-tier clocked-partition checks (MLS §16.2, §16.7.4):
/// - CLK-005: every clocked variable associates with exactly one clock
///   (assigned in clocked when-equations with the same clock condition).
/// - CLK-004 / CLK-006: variables of one clocked partition may not be read in
///   a different partition or in continuous equations without going through
///   sample()/hold()/previous()/noClock().
/// - CLK-003: clock-array subscripts in when-conditions must be evaluable.
fn check_clock_partitions(class: &ClassDef, def: &StoredDefinition, diags: &mut Vec<Diagnostic>) {
    // partition key -> assigned variable names; variable -> partition key
    let mut var_partition: HashMap<String, String> = HashMap::new();
    let mut clocked_when_bodies: Vec<(String, &Vec<Equation>)> = Vec::new();

    for eq in &class.equations {
        let Equation::When(blocks) = eq else {
            continue;
        };
        for block in blocks {
            let Some(token) = clock_condition_token(&block.cond, class, def) else {
                continue;
            };
            // CLK-003: subscripted clock conditions need evaluable subscripts.
            if let Expression::ComponentReference(cref) = &block.cond {
                check_clock_condition_subscripts(cref, class, diags);
            }
            let key = clock_partition_key(&block.cond);
            clocked_when_bodies.push((key.clone(), &block.eqs));
            record_clocked_assignments(&block.eqs, &key, token, &mut var_partition, diags);
        }
    }
    if var_partition.is_empty() {
        return;
    }

    check_cross_partition_reads(&clocked_when_bodies, &var_partition, diags);
    check_continuous_clocked_reads(class, &var_partition, diags);
}

/// CLK-005: record which partition assigns each variable; double assignment
/// across partitions is an error.
fn record_clocked_assignments(
    body: &[Equation],
    key: &str,
    token: &Token,
    var_partition: &mut HashMap<String, String>,
    diags: &mut Vec<Diagnostic>,
) {
    for inner in body {
        let Equation::Simple { lhs, .. } = inner else {
            continue;
        };
        let Expression::ComponentReference(target) = lhs else {
            continue;
        };
        let name = target
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".");
        match var_partition.get(&name) {
            Some(previous_key) if previous_key != key => {
                diags.push(semantic_error(
                    ER126_CLOCK_PARTITION_CONFLICT,
                    format!(
                        "clocked variable '{name}' is assigned in two different clock partitions (MLS §16.2.1)"
                    ),
                    label_from_token(
                        token,
                        "check_clock_restrictions/clock_partition_conflict",
                        "a clocked variable associates with exactly one clock",
                    ),
                ));
            }
            Some(_) => {}
            None => {
                var_partition.insert(name, key.to_string());
            }
        }
    }
}

/// CLK-004: reads in a clocked when-body of vars from another partition.
fn check_cross_partition_reads(
    clocked_when_bodies: &[(String, &Vec<Equation>)],
    var_partition: &HashMap<String, String>,
    diags: &mut Vec<Diagnostic>,
) {
    for (key, body) in clocked_when_bodies {
        let reads = body.iter().flat_map(|inner| match inner {
            Equation::Simple { rhs, .. } => unwrapped_clocked_reads(rhs),
            _ => Vec::new(),
        });
        for (name, token) in reads {
            if var_partition.get(&name).is_some_and(|owner| owner != key) {
                diags.push(semantic_error(
                    ER126_CLOCK_PARTITION_CONFLICT,
                    format!(
                        "clocked variable '{name}' from another sub-clock partition is accessed directly (MLS §16.7.4)"
                    ),
                    label_from_token(
                        &token,
                        "check_clock_restrictions/sub_clock_access",
                        "use subSample/superSample/noClock to cross partitions",
                    ),
                ));
            }
        }
    }
}

/// CLK-006: clocked variables read directly from continuous equations.
fn check_continuous_clocked_reads(
    class: &ClassDef,
    var_partition: &HashMap<String, String>,
    diags: &mut Vec<Diagnostic>,
) {
    let reads = class.equations.iter().flat_map(|eq| match eq {
        Equation::Simple { lhs, rhs, .. } => {
            let mut found = unwrapped_clocked_reads(lhs);
            found.extend(unwrapped_clocked_reads(rhs));
            found
        }
        _ => Vec::new(),
    });
    for (name, token) in reads {
        if var_partition.contains_key(&name) {
            diags.push(semantic_error(
                ER127_CLOCKED_VAR_CONTINUOUS_ACCESS,
                format!(
                    "clocked variable '{name}' is accessed from continuous-time equations without sample()/hold() (MLS §16.2.1)"
                ),
                label_from_token(
                    &token,
                    "check_clock_restrictions/clocked_var_continuous_access",
                    "wrap the access in hold() or sample()",
                ),
            ));
        }
    }
}

/// Textual partition key for a clocked when-condition.
fn clock_partition_key(cond: &Expression) -> String {
    format!("{cond:?}")
}

/// Component reads in an expression that are NOT wrapped in a clock-crossing
/// operator (sample/hold/previous/noClock/subSample/superSample/shiftSample/
/// backSample). Returns dotted names with their tokens.
fn unwrapped_clocked_reads(expr: &Expression) -> Vec<(String, Token)> {
    let mut collector = UnwrappedReadCollector { found: Vec::new() };
    let _ = collector.visit_expression(expr);
    collector.found
}

struct UnwrappedReadCollector {
    found: Vec<(String, Token)>,
}

impl ast::Visitor for UnwrappedReadCollector {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        _args: &[Expression],
        _ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        // Do not descend into clock-crossing operators: their arguments are
        // legal cross-partition accesses.
        if let Some(name) = builtin_name(comp)
            && matches!(
                name,
                "sample"
                    | "hold"
                    | "previous"
                    | "noClock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "pre"
            )
        {
            // Skip the operator's arguments entirely.
            return std::ops::ControlFlow::Continue(());
        }
        for arg in _args {
            self.visit_expression(arg)?;
        }
        std::ops::ControlFlow::Continue(())
    }

    fn visit_component_reference(
        &mut self,
        comp: &ComponentReference,
    ) -> std::ops::ControlFlow<()> {
        if let Some(first) = comp.parts.first() {
            let name = comp
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            self.found.push((name, first.ident.clone()));
        }
        ast::visitor::walk_component_reference_default(self, comp)
    }
}

/// CLK-003: clock-array conditions like `when c[i]` need evaluable subscripts.
fn check_clock_condition_subscripts(
    cref: &ComponentReference,
    class: &ClassDef,
    diags: &mut Vec<Diagnostic>,
) {
    for part in &cref.parts {
        let Some(subs) = part.subs.as_ref() else {
            continue;
        };
        for sub in subs {
            let Subscript::Expression(expr) = sub else {
                continue;
            };
            let evaluable = rumoca_ir_ast::collect_component_refs(expr)
                .iter()
                .all(|inner| subscript_ref_is_evaluable(class, inner));
            if !evaluable {
                diags.push(semantic_error(
                    ER128_CLOCK_SUBSCRIPT_EVALUABLE,
                    "clock-array subscripts in when-conditions must be evaluable during translation (MLS §16.3)"
                        .to_string(),
                    label_from_token(
                        &part.ident,
                        "check_clock_restrictions/clock_subscript_evaluable",
                        "use a parameter or constant subscript",
                    ),
                ));
            }
        }
    }
}

/// Conservative evaluability of a subscript reference: unqualified names must
/// be parameters/constants (or unknown); `time` is continuous.
fn subscript_ref_is_evaluable(class: &ClassDef, cref: &ComponentReference) -> bool {
    let [part] = cref.parts.as_slice() else {
        return true;
    };
    let name = part.ident.text.as_ref();
    name != "time"
        && class.components.get(name).is_none_or(|component| {
            matches!(
                component.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
}
