//! MLS §17 state-machine graph validations (SM-002..SM-004, SM-006, SM-007).
//!
//! State machines are not lowered yet (every use also reports ER073), but the
//! graph restrictions are validated here so violating models get the precise
//! diagnostic instead of only the blanket "unsupported" error.

use super::*;

pub(super) const ER077_TRANSITION_DUPLICATE_PRIORITY: &str = "ER077";
pub(super) const ER078_TRANSITION_PRIORITY_MINIMUM: &str = "ER078";
pub(super) const ER079_STATE_MACHINE_INITIAL_STATE_COUNT: &str = "ER079";
pub(super) const ER080_STATE_MACHINE_OPERATOR_CONTEXT: &str = "ER080";
pub(super) const ER081_ACTIVE_STATE_NOT_A_STATE: &str = "ER081";

pub(super) fn run_state_machine_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for (_, class) in &def.classes {
        check_class_state_machines(class, &mut diags);
    }
    diags
}

fn check_class_state_machines(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (_, nested) in &class.classes {
        check_class_state_machines(nested, diags);
    }
    // Function classes are handled by the ER056 forbidden-operator check.
    if class.class_type == ClassType::Function {
        return;
    }

    let mut machine = StateMachineGraph::default();
    scan_equations(
        &class.equations,
        EquationContext::Plain,
        class,
        &mut machine,
        diags,
    );
    scan_equations(
        &class.initial_equations,
        EquationContext::Plain,
        class,
        &mut machine,
        diags,
    );
    if machine.is_empty() {
        return;
    }

    check_transition_priorities(&machine, diags);
    check_initial_state_counts(&machine, diags);
    check_active_state_targets(class, &machine, diags);
}

#[derive(Default)]
struct StateMachineGraph {
    transitions: Vec<Transition>,
    initial_states: Vec<(String, Token)>,
}

struct Transition {
    from: String,
    from_token: Token,
    to: String,
    /// Transition priority when it is statically evaluable; `priority`
    /// defaults to 1 per MLS §17.1.
    priority: Option<i64>,
}

impl StateMachineGraph {
    fn is_empty(&self) -> bool {
        self.transitions.is_empty() && self.initial_states.is_empty()
    }

    fn states(&self) -> HashSet<&str> {
        self.transitions
            .iter()
            .flat_map(|t| [t.from.as_str(), t.to.as_str()])
            .collect()
    }
}

#[derive(Clone, Copy, PartialEq)]
enum EquationContext {
    /// Directly in the equation section (possibly under a for-equation).
    Plain,
    /// Inside a when-equation or an if-equation without parameter conditions,
    /// where MLS §17.1 forbids transition()/initialState() (SM-006).
    Conditional(&'static str),
}

fn scan_equations(
    equations: &[Equation],
    ctx: EquationContext,
    class: &ClassDef,
    machine: &mut StateMachineGraph,
    diags: &mut Vec<Diagnostic>,
) {
    for eq in equations {
        match eq {
            Equation::FunctionCall { comp, args } => {
                scan_state_machine_call(comp, args, ctx, machine, diags);
            }
            Equation::For { equations, .. } => {
                scan_equations(equations, ctx, class, machine, diags);
            }
            Equation::When(blocks) => {
                for block in blocks {
                    scan_equations(
                        &block.eqs,
                        EquationContext::Conditional("when-equation"),
                        class,
                        machine,
                        diags,
                    );
                }
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                let parameter_conditions = cond_blocks
                    .iter()
                    .all(|block| is_parameter_expression(&block.cond, class));
                let inner_ctx = if parameter_conditions {
                    ctx
                } else {
                    EquationContext::Conditional("if-equation with non-parameter condition")
                };
                for block in cond_blocks {
                    scan_equations(&block.eqs, inner_ctx, class, machine, diags);
                }
                if let Some(else_eqs) = else_block {
                    scan_equations(else_eqs, inner_ctx, class, machine, diags);
                }
            }
            _ => {}
        }
    }
}

fn scan_state_machine_call(
    comp: &ComponentReference,
    args: &[Expression],
    ctx: EquationContext,
    machine: &mut StateMachineGraph,
    diags: &mut Vec<Diagnostic>,
) {
    let operator = match builtin_name(comp) {
        Some(name @ ("transition" | "initialState")) => name,
        _ => return,
    };
    if reject_conditional_context(comp, operator, ctx, diags) {
        return;
    }
    if operator == "transition" {
        collect_transition(comp, args, machine, diags);
    } else if let Some((name, token)) = state_argument(args.first()) {
        machine.initial_states.push((name, token));
    }
}

/// SM-006: transition()/initialState() may not appear inside when-equations or
/// if-equations with non-parameter conditions (MLS §17.1).
fn reject_conditional_context(
    comp: &ComponentReference,
    operator: &str,
    ctx: EquationContext,
    diags: &mut Vec<Diagnostic>,
) -> bool {
    let EquationContext::Conditional(context_name) = ctx else {
        return false;
    };
    let Some(token) = comp.parts.first().map(|part| &part.ident) else {
        return true;
    };
    diags.push(semantic_error(
        ER080_STATE_MACHINE_OPERATOR_CONTEXT,
        format!("{operator}() is not allowed inside a {context_name} (MLS §17.1)"),
        label_from_token(
            token,
            "state_machines/operator_context",
            format!("{operator}() must appear directly in the equation section"),
        ),
    ));
    true
}

fn collect_transition(
    comp: &ComponentReference,
    args: &[Expression],
    machine: &mut StateMachineGraph,
    diags: &mut Vec<Diagnostic>,
) {
    let positional: Vec<&Expression> = args
        .iter()
        .take_while(|arg| !matches!(arg, Expression::NamedArgument { .. }))
        .collect();
    let Some((from, from_token)) = state_argument(positional.first().copied()) else {
        return;
    };
    let Some((to, _)) = state_argument(positional.get(1).copied()) else {
        return;
    };

    // MLS §17.1: transition(from, to, condition, immediate, reset,
    // synchronize, priority); priority defaults to 1.
    let priority_expr = args
        .iter()
        .find_map(|arg| match arg {
            Expression::NamedArgument { name, value, .. } if name.text.as_ref() == "priority" => {
                Some(value.as_ref())
            }
            _ => None,
        })
        .or_else(|| positional.get(6).copied());
    let priority = match priority_expr {
        None => Some(1),
        Some(expr) => integer_literal_value(expr),
    };

    if let Some(priority_value) = priority
        && priority_value < 1
        && let Some(token) = comp.parts.first().map(|part| &part.ident)
    {
        // SM-004: priority ≥ 1 required (MLS §17.1).
        diags.push(semantic_error(
            ER078_TRANSITION_PRIORITY_MINIMUM,
            format!("transition() priority must be ≥ 1, got {priority_value} (MLS §17.1)"),
            label_from_token(
                token,
                "state_machines/priority_minimum",
                "transition priorities start at 1",
            ),
        ));
    }

    machine.transitions.push(Transition {
        from,
        from_token,
        to,
        priority,
    });
}

/// SM-002: all transitions leaving one state must have different priorities.
fn check_transition_priorities(machine: &StateMachineGraph, diags: &mut Vec<Diagnostic>) {
    let mut seen: HashMap<(&str, i64), &Transition> = HashMap::new();
    for transition in &machine.transitions {
        let Some(priority) = transition.priority else {
            continue;
        };
        if let Some(previous) = seen.insert((transition.from.as_str(), priority), transition) {
            diags.push(semantic_error(
                ER077_TRANSITION_DUPLICATE_PRIORITY,
                format!(
                    "transitions leaving state '{}' (to '{}' and '{}') share priority {priority}; \
                     all transitions leaving one state must have different priorities (MLS §17.1)",
                    transition.from, previous.to, transition.to
                ),
                label_from_token(
                    &transition.from_token,
                    "state_machines/duplicate_priority",
                    "give each outgoing transition a distinct priority",
                ),
            ));
        }
    }
}

/// SM-003: exactly one instance in each state machine must be marked initial.
/// State machines within a class are the weakly-connected components of the
/// transition graph (parallel state machines are separate components).
fn check_initial_state_counts(machine: &StateMachineGraph, diags: &mut Vec<Diagnostic>) {
    let mut components = UnionFind::default();
    for transition in &machine.transitions {
        components.union(&transition.from, &transition.to);
    }
    for (state, _) in &machine.initial_states {
        components.insert(state);
    }

    let mut initial_count: HashMap<String, usize> = HashMap::new();
    for (state, _) in &machine.initial_states {
        *initial_count.entry(components.root_of(state)).or_default() += 1;
    }

    for (state, token) in &machine.initial_states {
        if initial_count[&components.root_of(state)] > 1 {
            diags.push(semantic_error(
                ER079_STATE_MACHINE_INITIAL_STATE_COUNT,
                format!(
                    "state machine containing '{state}' has more than one initialState() (MLS §17.1)"
                ),
                label_from_token(
                    token,
                    "state_machines/initial_state_count",
                    "exactly one state per state machine may be marked initial",
                ),
            ));
        }
    }

    let marked_roots: HashSet<String> = machine
        .initial_states
        .iter()
        .map(|(state, _)| components.root_of(state))
        .collect();
    let mut reported_roots: HashSet<String> = HashSet::new();
    for transition in &machine.transitions {
        let root = components.root_of(&transition.from);
        if !marked_roots.contains(&root) && reported_roots.insert(root) {
            diags.push(semantic_error(
                ER079_STATE_MACHINE_INITIAL_STATE_COUNT,
                format!(
                    "state machine containing '{}' has no initialState() (MLS §17.1)",
                    transition.from
                ),
                label_from_token(
                    &transition.from_token,
                    "state_machines/initial_state_missing",
                    "mark exactly one state with initialState()",
                ),
            ));
        }
    }
}

/// SM-007: activeState(s) requires `s` to be a state of a state machine,
/// i.e. an endpoint of some transition in the same class (MLS §17.3.1).
fn check_active_state_targets(
    class: &ClassDef,
    machine: &StateMachineGraph,
    diags: &mut Vec<Diagnostic>,
) {
    let mut collector = ActiveStateCollector { uses: Vec::new() };
    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        let _ = collector.visit_equation(eq);
    }

    let states = machine.states();
    for (target, token) in collector.uses {
        if !states.contains(target.as_str()) {
            diags.push(semantic_error(
                ER081_ACTIVE_STATE_NOT_A_STATE,
                format!(
                    "activeState() argument '{target}' is not a state of a state machine (MLS §17.3.1)"
                ),
                label_from_token(
                    &token,
                    "state_machines/active_state_target",
                    "activeState() requires a transition() endpoint",
                ),
            ));
        }
    }
}

struct ActiveStateCollector {
    uses: Vec<(String, Token)>,
}

impl ast::Visitor for ActiveStateCollector {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if builtin_name(comp) == Some("activeState")
            && let Some((name, token)) = state_argument(args.first())
        {
            self.uses.push((name, token));
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

fn state_argument(arg: Option<&Expression>) -> Option<(String, Token)> {
    let Some(Expression::ComponentReference(comp)) = arg else {
        return None;
    };
    let token = comp.parts.first().map(|part| part.ident.clone())?;
    let name = comp
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    Some((name, token))
}

fn integer_literal_value(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => integer_literal_value(rhs).map(|value| -value),
        Expression::Parenthesized { inner, .. } => integer_literal_value(inner),
        _ => None,
    }
}

/// Minimal union-find over state names for weakly-connected components.
#[derive(Default)]
struct UnionFind {
    parent: HashMap<String, String>,
}

impl UnionFind {
    fn insert(&mut self, name: &str) {
        if !self.parent.contains_key(name) {
            self.parent.insert(name.to_string(), name.to_string());
        }
    }

    fn root_of(&self, name: &str) -> String {
        let mut current = name;
        while let Some(parent) = self.parent.get(current) {
            if parent == current {
                break;
            }
            current = parent;
        }
        current.to_string()
    }

    fn union(&mut self, left: &str, right: &str) {
        self.insert(left);
        self.insert(right);
        let left_root = self.root_of(left);
        let right_root = self.root_of(right);
        if left_root != right_root {
            self.parent.insert(left_root, right_root);
        }
    }
}
