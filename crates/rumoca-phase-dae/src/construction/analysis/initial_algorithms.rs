//! MLS §8.6 `initial algorithm` sections lowered to declarative owners.
//!
//! An initial algorithm runs once, before the trajectory exists, so its exact
//! declarative meaning is the value each written coordinate holds when
//! initialization finishes. This pass replays the section symbolically — one
//! substitution map from written coordinate to the expression that determines
//! it — and hands the result to owners that already exist:
//!
//! * an `assert` becomes a checked assertion owner with every enclosing branch
//!   condition folded into its condition, so a guarded check keeps its guard
//!   instead of becoming unconditional;
//! * a `parameter` declared `fixed = false` becomes a calculated parameter
//!   whose binding is the replayed expression. Its determining expression reads
//!   only parameters and constants. Where every one of those parameters is
//!   itself settled when the parameter set runs, the value the initialization
//!   system would compute and the value the parameter set computes are the same
//!   number — evaluating it at parameter-set time is exact, not an
//!   approximation. A `fixed = false` parameter is *not* settled then: MLS 3.6
//!   §8.6 says such a parameter is "treated as unknown during the
//!   initialization phase" and "the start-value can be used as a guess-value",
//!   so the parameter set only has that guess. A replayed expression that reads
//!   one is still accepted, and `rumoca-phase-solve`'s
//!   `lower::initial_parameters` re-applies its binding as an initialization
//!   update row after the projection that solves the unknown, which is what
//!   makes the value equivalent — the parameter-set number is then the
//!   iteration seed, not the answer;
//! * a discrete-time coordinate becomes an initialization-partition definition
//!   of the value it holds when initialization finishes. MLS §8.6 lets an
//!   initial section determine a discrete-time variable, and the equation
//!   section keeps its own owner for every later instant, so the two are
//!   different partitions rather than two owners of one coordinate.
//!
//! A zero-output call statement to a collected function is replayed the same
//! way once its body is proven to have no effect other than raising
//! assertions: the call is replaced by exactly those assertions, under the
//! same guard, so it reaches the assertion owner above. [`checking_calls`]
//! states that acceptance contract and rejects every call outside it by name.
//!
//! Every other written coordinate keeps a typed rejection: the initialization
//! system has no checked owner that solves for a state, algebraic, output, or
//! input coordinate from an algorithm, and inventing one would replace a
//! missing capability with an unproven guess.
mod checking_calls;

use super::*;
use checking_calls::{checking_call, expand_checking_call, reject_unsupported_checking_call};
use rumoca_core::ExpressionRewriter;

pub(super) struct InitialAlgorithmAnalysis {
    /// Determining expression of each `fixed = false` parameter the section
    /// assigns, with every earlier assignment already substituted.
    pub(super) parameters: HashMap<VarName, Expression>,
    /// Initialization-instant value of each discrete-time coordinate the
    /// section assigns, with every earlier assignment already substituted.
    pub(super) discrete_values: HashMap<VarName, InitialDiscreteValue>,
    pub(super) assertions: Vec<flat::AssertEquation>,
}

/// One discrete coordinate's initialization-instant value.
pub(in crate::construction) struct InitialDiscreteValue {
    pub(in crate::construction) value: Expression,
    pub(in crate::construction) span: Span,
}

/// The declarative owner one replayed target resolves to.
enum InitialTarget {
    /// MLS §8.6 calculated parameter; the parameter set evaluates it.
    Parameter(Expression),
    /// MLS §8.6 discrete-time initial value; the initialization system owns it.
    Discrete(InitialDiscreteValue),
}

/// Prove the statement grammar of every initial algorithm section.
///
/// This runs before function-shape discovery so an unsupported form is reported
/// as the missing algorithm owner rather than as a consequence of it: Flat
/// renders a statement `assert(...)` as a call to the predefined operator, and
/// a checking call such as `isValidTable(table)` names a callee the Flat
/// function table never registers, so shape discovery would otherwise report
/// `ED008` for an owner that is simply absent.
pub(super) fn reject_unsupported_initial_algorithm_statements(
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    for algorithm in &flat.initial_algorithms {
        require_span(algorithm.span, "initial algorithm")?;
        reject_unsupported_statements(flat, &algorithm.statements)?;
    }
    Ok(())
}

fn reject_unsupported_statements(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
) -> Result<(), ToDaeError> {
    for statement in statements {
        if let Some(assertion) = assertion_call(flat, statement) {
            require_span(assertion.span, "initial algorithm assertion")?;
            continue;
        }
        // A zero-output call to a collected function is a checking call when
        // its body can only raise assertions; that proof is what admits it, so
        // it is taken before the call-statement rejection below.
        if let Some(call) = checking_call(flat, statement) {
            reject_unsupported_checking_call(&call)?;
            continue;
        }
        match statement {
            rumoca_core::Statement::Empty { .. } => {}
            rumoca_core::Statement::Assignment { comp, span, .. } => {
                require_span(*span, "initial algorithm assignment")?;
                if comp.parts().is_empty() || comp.parts().iter().any(|part| !part.subs.is_empty())
                {
                    return Err(unsupported(
                        "an assignment target must be one whole, unsubscripted coordinate",
                        *span,
                    ));
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => {
                require_span(*span, "initial algorithm if statement")?;
                if cond_blocks.is_empty() {
                    return Err(unsupported(
                        "an if statement must declare at least one guarded block",
                        *span,
                    ));
                }
                for block in cond_blocks {
                    reject_unsupported_statements(flat, &block.stmts)?;
                }
                if let Some(statements) = else_block {
                    reject_unsupported_statements(flat, statements)?;
                }
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                outputs,
                span,
                ..
            } => {
                let callee = comp.to_var_name();
                let detail = if outputs.iter().all(Option::is_none) {
                    format!(
                        "a call statement to `{callee}` names a callee the Flat function table \
                         does not register, so its body cannot be proven to only raise assertions"
                    )
                } else {
                    format!(
                        "a call statement to `{callee}` binds outputs; the initialization \
                         partition has no owner that solves a coordinate from a call statement"
                    )
                };
                return Err(unsupported(detail, *span));
            }
            _ => {
                let span =
                    required_statement_span(statement, "unsupported initial algorithm statement")?;
                return Err(unsupported(
                    "an initial algorithm is accepted as sequential scalar assignments, `if` \
                     conditionals, and `assert` statements; loops, `when`, and `reinit` carry \
                     implicit memory with no checked initialization owner",
                    span,
                ));
            }
        }
    }
    Ok(())
}

/// Replay every accepted initial algorithm into its declarative owners.
pub(super) fn analyze_initial_algorithms(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
) -> Result<InitialAlgorithmAnalysis, ToDaeError> {
    let mut analysis = InitialAlgorithmAnalysis {
        parameters: HashMap::new(),
        discrete_values: HashMap::new(),
        assertions: Vec::new(),
    };
    for algorithm in &flat.initial_algorithms {
        let mut replay = Replay {
            flat,
            constants,
            origin: flat::EquationOrigin::Algorithm {
                component: algorithm.origin.clone(),
            },
            assertions: Vec::new(),
        };
        let mut values = ReplayValues::new();
        replay.statements(&algorithm.statements, None, &mut values)?;
        analysis.assertions.append(&mut replay.assertions);
        for target in sorted_targets(&values) {
            let value = values
                .remove(&target)
                .expect("a replayed target keeps its value");
            let duplicated = match plan_initial_target(flat, roles, states, &target, value)? {
                InitialTarget::Parameter(value) => {
                    analysis.parameters.insert(target.clone(), value).is_some()
                }
                InitialTarget::Discrete(value) => analysis
                    .discrete_values
                    .insert(target.clone(), value)
                    .is_some(),
            };
            if duplicated {
                return Err(unsupported(
                    format!(
                        "`{target}` is determined by more than one initial algorithm; an \
                         initialization-determined coordinate has exactly one determining owner"
                    ),
                    algorithm.span,
                ));
            }
        }
    }
    for assertion in &analysis.assertions {
        validate_expression(&assertion.condition, roles, states)?;
        validate_expression(&assertion.message, roles, states)?;
        if let Some(level) = &assertion.level {
            validate_expression(level, roles, states)?;
        }
    }
    reject_competing_initial_equations(flat, roles, &analysis.parameters)?;
    Ok(analysis)
}

/// Reject a deferred parameter that an initial equation also determines.
///
/// MLS §8.6 gives every unknown exactly one determining owner. An initial
/// equation whose residual reads only parameters and constants is a row the
/// initialization projection solves for the deferred parameters it contains, so
/// such a row and a calculated-parameter binding would both claim the same
/// coordinate — the trajectory would then depend on which owner ran last
/// instead of on the model.
fn reject_competing_initial_equations(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    parameters: &HashMap<VarName, Expression>,
) -> Result<(), ToDaeError> {
    if parameters.is_empty() {
        return Ok(());
    }
    for equation in &flat.initial_equations {
        let mut references = Vec::new();
        equation.residual.collect_var_refs(&mut references);
        if !references.iter().all(|name| {
            matches!(
                roles.get(name),
                Some(
                    PlannedRole::Parameter
                        | PlannedRole::Constant
                        | PlannedRole::EnumerationLiteral
                )
            )
        }) {
            continue;
        }
        if let Some(target) = references
            .iter()
            .find(|name| parameters.contains_key(*name))
        {
            return Err(unsupported(
                format!(
                    "`{target}` is determined by an initial algorithm and by an initial \
                     equation over parameters; one deferred parameter has exactly one \
                     determining owner"
                ),
                equation.span,
            ));
        }
    }
    Ok(())
}

/// Prove that one replayed target has a declarative owner, and say which.
fn plan_initial_target(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    target: &VarName,
    value: ReplayedValue,
) -> Result<InitialTarget, ToDaeError> {
    match roles[target] {
        PlannedRole::Parameter => {
            plan_initial_parameter(flat, roles, states, target, value).map(InitialTarget::Parameter)
        }
        PlannedRole::DiscreteReal | PlannedRole::DiscreteValue => {
            plan_initial_discrete_value(flat, roles, states, target, value)
                .map(InitialTarget::Discrete)
        }
        role => Err(unsupported(
            format!(
                "initial algorithm target `{target}` has role {role:?}; the initialization \
                 system owns an algorithm-determined coordinate only as a `parameter` declared \
                 `fixed = false` or as a discrete-time coordinate, because a state, algebraic, \
                 output, or input coordinate is solved from residual rows rather than assigned"
            ),
            value.span,
        )),
    }
}

/// Prove that one replayed discrete target is a coordinate the initialization
/// system can define.
///
/// MLS §8.6 lets an initial section determine a discrete-time variable's value
/// at the initialization instant. That instant is the one point where `time`
/// is already known and no trajectory exists yet, so the determining
/// expression may read `time`, parameters, and constants — and nothing else,
/// because no other coordinate has a proven value there.
fn plan_initial_discrete_value(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    target: &VarName,
    value: ReplayedValue,
) -> Result<InitialDiscreteValue, ToDaeError> {
    let variable = &flat.variables[target];
    if variable.binding.is_some() {
        return Err(unsupported(
            format!(
                "discrete coordinate `{target}` already has a declaration binding, which defines \
                 it at the initialization instant too, so the initial algorithm would give it a \
                 second determining owner there"
            ),
            value.span,
        ));
    }
    if !variable.dims.is_empty() {
        return Err(unsupported(
            format!(
                "discrete coordinate `{target}` is an array; the initialization system defines \
                 one scalar discrete coordinate per algorithm target and has no vector \
                 definition owner"
            ),
            value.span,
        ));
    }
    reject_unsettled_reads(flat, &value.expression, target, roles)?;
    validate_expression(&value.expression, roles, states)?;
    Ok(InitialDiscreteValue {
        value: value.expression,
        span: value.span,
    })
}

/// Prove that one replayed target is a coordinate the parameter set can own.
fn plan_initial_parameter(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    target: &VarName,
    value: ReplayedValue,
) -> Result<Expression, ToDaeError> {
    let variable = &flat.variables[target];
    if variable.fixed != Some(false) {
        return Err(unsupported(
            format!(
                "parameter `{target}` is not declared `fixed = false`; MLS §8.6 lets an initial \
                 section determine only a parameter whose declaration defers its value"
            ),
            value.span,
        ));
    }
    if variable.binding.is_some() {
        return Err(unsupported(
            format!(
                "parameter `{target}` already has a declaration binding, so the initial \
                 algorithm would give it a second determining owner"
            ),
            value.span,
        ));
    }
    if !variable.dims.is_empty() {
        return Err(unsupported(
            format!(
                "parameter `{target}` is an array; an array-valued initial algorithm target \
                 requires a vector-equation owner"
            ),
            value.span,
        ));
    }
    reject_runtime_reads(&value.expression, target, roles)?;
    validate_expression(&value.expression, roles, states)?;
    Ok(value.expression)
}

/// The initialization instant settles `time`, parameters, and constants and
/// nothing else, so a discrete initial value may read only those.
///
/// A read of a state, algebraic, output, input, `pre`, or another discrete
/// coordinate has no proven value at that instant: the initialization update
/// rows run before any trajectory exists, and accepting such a read would make
/// the initial value depend on evaluation order rather than on the model.
///
/// An MLS §12.3 impure call is rejected for the same reason from the other
/// side: the runtime applies initialization updates until they stop changing,
/// and a value that answers differently each time it runs has no fixed point
/// to reach.
///
/// That is a fact about the *body*, not about the written prefix, so it is
/// proven with [`rumoca_core::Function::body_is_pure`]: MLS 3.7 §12.3 treats an
/// external function without explicit purity as impure, and such a call settles
/// no better than one that wrote `impure`. Naming it here gives the owner its
/// exact span instead of leaving a later stage to report the same call as an
/// unsupported initialization form.
fn reject_unsettled_reads(
    flat: &flat::Model,
    expression: &Expression,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall { name, span, .. } = expression
        && let Some(function) = flat.functions.get(name.var_name())
        && !function.body_is_pure()
    {
        return Err(unsupported(
            format!(
                "`{target}` is determined by a call to the impure function `{}`; the \
                 initialization system applies an algorithm-determined discrete value \
                 until it stops changing, which an impure call never does",
                function.name
            ),
            *span,
        ));
    }
    if let Expression::VarRef { name, span, .. } = expression {
        let referenced = name.var_name();
        let settled = referenced.as_str() == "time"
            || matches!(
                roles.get(referenced),
                Some(
                    PlannedRole::Parameter
                        | PlannedRole::Constant
                        | PlannedRole::EnumerationLiteral
                )
            );
        if !settled {
            return Err(unsupported(
                format!(
                    "`{target}` is determined from `{referenced}`, which has no proven value at \
                     the initialization instant; an algorithm-determined discrete coordinate \
                     reads only `time`, parameters, and constants"
                ),
                *span,
            ));
        }
    }
    for child in expression_children(expression) {
        reject_unsettled_reads(flat, child, target, roles)?;
    }
    Ok(())
}

/// A calculated parameter is evaluated once, before the trajectory exists, so
/// its determining expression may read only parameters and constants.
///
/// `PlannedRole::Parameter` covers a `fixed = false` parameter the
/// initialization projection solves, whose parameter-set number is the MLS 3.6
/// §8.6 `start` *guess* rather than its value. That read is accepted, not
/// rejected: `rumoca-phase-solve`'s `lower::initial_parameters` re-applies this
/// binding as an initialization update row after the solve, so the value the
/// trajectory reads is the solved one. What this check still owns is the
/// boundary against a *coordinate* — a state, algebraic, output, input, or
/// discrete read — which no ordering can settle at parameter time.
fn reject_runtime_reads(
    expression: &Expression,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    if let Expression::VarRef { name, span, .. } = expression {
        let referenced = name.var_name();
        if !matches!(
            roles.get(referenced),
            Some(PlannedRole::Parameter | PlannedRole::Constant | PlannedRole::EnumerationLiteral)
        ) {
            return Err(unsupported(
                format!(
                    "`{target}` is determined from `{referenced}`, which is not settled when \
                     parameters are computed; an algorithm-determined parameter reads only \
                     parameters and constants"
                ),
                *span,
            ));
        }
    }
    for child in expression_children(expression) {
        reject_runtime_reads(child, target, roles)?;
    }
    Ok(())
}

/// One replayed coordinate value and the statement that last wrote it.
#[derive(Clone)]
struct ReplayedValue {
    expression: Expression,
    span: Span,
}

type ReplayValues = HashMap<VarName, ReplayedValue>;

fn sorted_targets(values: &ReplayValues) -> Vec<VarName> {
    let mut targets = values.keys().cloned().collect::<Vec<_>>();
    targets.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    targets
}

struct Replay<'flat> {
    flat: &'flat flat::Model,
    /// Parameter values and array shapes, so a checking call's loop bounds
    /// resolve to the iterations the model actually has.
    constants: &'flat EvalContext,
    origin: flat::EquationOrigin,
    assertions: Vec<flat::AssertEquation>,
}

impl Replay<'_> {
    fn statements(
        &mut self,
        statements: &[rumoca_core::Statement],
        guard: Option<&Expression>,
        values: &mut ReplayValues,
    ) -> Result<(), ToDaeError> {
        for statement in statements {
            self.statement(statement, guard, values)?;
        }
        Ok(())
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        guard: Option<&Expression>,
        values: &mut ReplayValues,
    ) -> Result<(), ToDaeError> {
        if let Some(assertion) = assertion_call(self.flat, statement) {
            let condition = guard_condition(
                guard,
                substitute(assertion.condition, values),
                assertion.span,
            );
            self.assertions.push(flat::AssertEquation::new(
                condition,
                substitute(assertion.message, values),
                assertion.level.map(|level| substitute(level, values)),
                assertion.span,
                self.origin.clone(),
            ));
            return Ok(());
        }
        // A checking call stands for exactly the assertions its body raises,
        // so it is replaced by them and replayed under the same guard. Every
        // enclosing branch condition therefore reaches each one through the
        // owner a guarded check already has.
        if let Some(call) = checking_call(self.flat, statement) {
            let expanded = expand_checking_call(&call, self.constants)?;
            return self.statements(&expanded, guard, values);
        }
        match statement {
            rumoca_core::Statement::Empty { .. } => Ok(()),
            rumoca_core::Statement::Assignment { comp, value, span } => {
                let target = rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone();
                if !self.flat.variables.contains_key(&target) {
                    return Err(unsupported(
                        format!("assignment target `{target}` is not a declared coordinate"),
                        *span,
                    ));
                }
                let expression = substitute(value, values);
                values.insert(
                    target,
                    ReplayedValue {
                        expression,
                        span: *span,
                    },
                );
                Ok(())
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => self.conditional(cond_blocks, else_block.as_deref(), guard, *span, values),
            _ => unreachable!("the statement grammar is proven before analysis replays it"),
        }
    }

    /// Replay one `if` chain into a conditional value per written coordinate.
    ///
    /// The chain is sequential per MLS §11.5: branch `k` runs when every
    /// earlier condition is false and `c_k` is true. A nested
    /// `Expression::If` encodes exactly that for the merged value, and the
    /// folded guard encodes it for an assertion inside the branch.
    fn conditional(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        fallback: Option<&[rumoca_core::Statement]>,
        guard: Option<&Expression>,
        span: Span,
        values: &mut ReplayValues,
    ) -> Result<(), ToDaeError> {
        let entry = values.clone();
        let mut branches = Vec::with_capacity(blocks.len());
        let mut unreached = Vec::with_capacity(blocks.len());
        for block in blocks {
            let condition = substitute(&block.cond, &entry);
            let reached = conjunction(
                unreached
                    .iter()
                    .cloned()
                    .chain([condition.clone()])
                    .collect::<Vec<_>>(),
                span,
            );
            let branch_guard = conjunction(
                guard
                    .cloned()
                    .into_iter()
                    .chain(reached)
                    .collect::<Vec<_>>(),
                span,
            );
            let mut branch = entry.clone();
            self.statements(&block.stmts, branch_guard.as_ref(), &mut branch)?;
            unreached.push(negate(&condition, span));
            branches.push((condition, branch));
        }
        let mut otherwise = entry.clone();
        if let Some(statements) = fallback {
            let reached = conjunction(unreached.clone(), span);
            let branch_guard = conjunction(
                guard
                    .cloned()
                    .into_iter()
                    .chain(reached)
                    .collect::<Vec<_>>(),
                span,
            );
            self.statements(statements, branch_guard.as_ref(), &mut otherwise)?;
        }
        let mut merged = entry.keys().cloned().collect::<HashSet<_>>();
        for branch in branches
            .iter()
            .map(|(_, branch)| branch)
            .chain([&otherwise])
        {
            merged.extend(branch.keys().cloned());
        }
        let mut merged = merged.into_iter().collect::<Vec<_>>();
        merged.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        for target in merged {
            let value = merge_target(&target, &entry, &branches, &otherwise, span)?;
            values.insert(target, value);
        }
        Ok(())
    }
}

fn merge_target(
    target: &VarName,
    entry: &ReplayValues,
    branches: &[(Expression, ReplayValues)],
    otherwise: &ReplayValues,
    span: Span,
) -> Result<ReplayedValue, ToDaeError> {
    let fallback = branch_value(target, otherwise, entry, span)?;
    let mut merged = Vec::with_capacity(branches.len());
    for (condition, branch) in branches {
        let value = branch_value(target, branch, entry, span)?;
        merged.push((condition.clone(), value.expression));
    }
    if merged
        .iter()
        .all(|(_, value)| rumoca_core::expressions_semantically_equal(value, &fallback.expression))
    {
        return Ok(fallback);
    }
    Ok(ReplayedValue {
        expression: Expression::If {
            branches: merged,
            else_branch: Box::new(fallback.expression),
            span,
        },
        span,
    })
}

fn branch_value(
    target: &VarName,
    branch: &ReplayValues,
    entry: &ReplayValues,
    span: Span,
) -> Result<ReplayedValue, ToDaeError> {
    branch
        .get(target)
        .or_else(|| entry.get(target))
        .cloned()
        .ok_or_else(|| {
            unsupported(
                format!(
                    "`{target}` is not defined on every path through the initial algorithm; a \
                     declarative owner needs one value per coordinate"
                ),
                span,
            )
        })
}

struct AssertionCall<'statement> {
    condition: &'statement Expression,
    message: &'statement Expression,
    level: Option<&'statement Expression>,
    span: Span,
}

/// Recognize MLS §8.3.7 `assert` in both forms Flat produces for a statement.
///
/// A statement `assert(...)` inside an algorithm section reaches Flat as a call
/// to the predefined operator, while an equation-section `assert` reaches it as
/// the dedicated statement. A user function may not shadow the operator here: a
/// callee the Flat function table registers is a user call, not the operator.
fn assertion_call<'statement>(
    flat: &flat::Model,
    statement: &'statement rumoca_core::Statement,
) -> Option<AssertionCall<'statement>> {
    match statement {
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            span,
        } => Some(AssertionCall {
            condition,
            message,
            level: level.as_deref(),
            span: *span,
        }),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } if outputs.iter().all(Option::is_none) => {
            let name = comp.to_var_name();
            if flat.functions.contains_key(&name)
                || rumoca_core::runtime_flow_action_function_short_name(name.as_str())
                    != Some("assert")
            {
                return None;
            }
            let (condition, message, level) = match args.as_slice() {
                [condition, message] => (condition, message, None),
                [condition, message, level] => (condition, message, Some(level)),
                _ => return None,
            };
            Some(AssertionCall {
                condition,
                message,
                level,
                span: *span,
            })
        }
        _ => None,
    }
}

/// `guard implies condition`, which is what a guarded check asserts.
fn guard_condition(guard: Option<&Expression>, condition: Expression, span: Span) -> Expression {
    match guard {
        None => condition,
        Some(guard) => Expression::Binary {
            op: OpBinary::Or,
            lhs: Box::new(negate(guard, span)),
            rhs: Box::new(condition),
            span,
        },
    }
}

fn conjunction(terms: Vec<Expression>, span: Span) -> Option<Expression> {
    terms.into_iter().reduce(|lhs, rhs| Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    })
}

fn negate(condition: &Expression, span: Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(condition.clone()),
        span,
    }
}

fn unsupported(detail: impl Into<String>, span: Span) -> ToDaeError {
    ToDaeError::unsupported_algorithm("initial", detail, span)
}

/// Substitute every coordinate the section has already written.
struct Substitution<'values> {
    values: &'values ReplayValues,
}

impl ExpressionRewriter for Substitution<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty()
            && let Some(value) = self.values.get(name.var_name())
        {
            return value.expression.clone();
        }
        Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

fn substitute(expression: &Expression, values: &ReplayValues) -> Expression {
    if values.is_empty() {
        return expression.clone();
    }
    Substitution { values }.rewrite_expression(expression)
}
