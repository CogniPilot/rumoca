//! The §8.6 initialization instant and the Appendix B event loop.
//!
//! One loop carries the whole of slice 1: the Appendix B event iteration, which
//! makes a single ordered pass over the equations and then advances `pre`,
//! stopping when `z == pre(z)` and `m == pre(m)`.
//!
//! There is deliberately no second, inner loop solving the equations to a fixed
//! point before `pre` advances. Slice 1's equations are already in Appendix
//! B.1c's almost-solved form, so the outer loop propagates values on its own —
//! and some inner systems have no fixed point to find, which an inner loop
//! would spin on rather than report. `event_iteration` carries the argument.
//!
//! The loop is bounded and reports non-convergence as an error. A reference
//! that spun forever would be useless as an oracle.

use std::collections::{BTreeMap, BTreeSet};

use crate::eval::{Environment, EvalError, eval};
use crate::expand::{Expanded, expand};
use crate::model::{Equation, Expr, Model, Variability};
use crate::schedule::next_event_time;
use crate::trajectory::Trajectory;
use crate::value::Value;

/// How far the reference will iterate before declaring a model outside slice 1.
#[derive(Debug, Clone, Copy)]
pub struct Options {
    /// Start of the run.
    pub t_start: f64,
    /// End of the run.
    pub t_stop: f64,
    /// Sweep and event-iteration ceiling.
    pub max_iterations: usize,
    /// Ceiling on the number of events in one run.
    pub max_events: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            t_start: 0.0,
            t_stop: 1.0,
            max_iterations: 64,
            max_events: 1024,
        }
    }
}

/// Why a run could not be completed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RefError {
    /// An expression could not be evaluated.
    Eval(EvalError),
    /// A loop hit its ceiling.
    NotConverged {
        /// Which loop.
        phase: &'static str,
    },
    /// The run produced more events than [`Options::max_events`].
    TooManyEvents,
    /// A `start` attribute read a variable that no earlier `start` bound.
    ///
    /// Slice 1 evaluates start attributes in one pass over literal starts and
    /// one pass over the rest. A model needing a third pass is outside the
    /// slice, and saying so is better than inventing an evaluation order.
    UnorderedStart(String),
    /// An equation determines a continuous variable.
    ///
    /// Continuous values come from the supplied trajectory in slice 1, so an
    /// equation writing one would make the trajectory and the equation both
    /// authoritative. Refusing keeps the reference's claim narrow and true.
    ContinuousIsDetermined(String),
    /// An equation determines a parameter (MLS §4.4.4).
    ParameterIsDetermined(String),
    /// An equation determines a name the model never declares.
    UndeclaredTarget(String),
    /// Two equations determine the same variable.
    TargetIsOverDetermined(String),
    /// A condition over a continuous variable changed value between two
    /// instants, so it crossed somewhere slice 1 never scheduled.
    ///
    /// Slice 1 locates time crossings only. Firing at the next scheduled
    /// instant instead would put the activation at a time the model does not
    /// put it, which is a worse answer than refusing. Locating continuous
    /// crossings is slice 2.
    UnlocatedCrossing(String),
}

impl From<EvalError> for RefError {
    fn from(error: EvalError) -> Self {
        RefError::Eval(error)
    }
}

impl std::fmt::Display for RefError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefError::Eval(error) => write!(f, "{error}"),
            RefError::NotConverged { phase } => write!(f, "{phase} did not converge"),
            RefError::TooManyEvents => f.write_str("run exceeded its event ceiling"),
            RefError::UnorderedStart(name) => {
                write!(f, "start attribute of `{name}` is not evaluable in order")
            }
            RefError::ContinuousIsDetermined(name) => {
                write!(f, "equation determines continuous variable `{name}`")
            }
            RefError::ParameterIsDetermined(name) => {
                write!(f, "equation determines parameter `{name}`")
            }
            RefError::UndeclaredTarget(name) => {
                write!(f, "equation determines undeclared variable `{name}`")
            }
            RefError::TargetIsOverDetermined(name) => {
                write!(f, "two equations determine `{name}`")
            }
            RefError::UnlocatedCrossing(name) => write!(
                f,
                "condition `{name}` crossed between instants; slice 1 locates \
                 time crossings only"
            ),
        }
    }
}

impl std::error::Error for RefError {}

/// One recorded instant.
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    /// The instant.
    pub time: f64,
    /// Every variable's value after the instant settled.
    pub values: BTreeMap<String, Value>,
}

impl Record {
    /// The value of `name` at this instant.
    #[must_use]
    pub fn value(&self, name: &str) -> Option<Value> {
        self.values.get(name).copied()
    }
}

/// The result of a run: the initialization instant, then one record per event.
#[derive(Debug, Clone, PartialEq)]
pub struct Trace {
    /// Records in time order. The first is the initialization instant.
    pub records: Vec<Record>,
}

impl Trace {
    /// The instants at which events occurred, excluding initialization.
    #[must_use]
    pub fn event_times(&self) -> Vec<f64> {
        self.records
            .iter()
            .skip(1)
            .map(|entry| entry.time)
            .collect()
    }

    /// The last recorded value of `name` at or before `time`.
    #[must_use]
    pub fn value_at(&self, name: &str, time: f64) -> Option<Value> {
        self.records
            .iter()
            .take_while(|entry| entry.time <= time)
            .filter_map(|entry| entry.value(name))
            .last()
    }

    /// The final recorded value of `name`.
    #[must_use]
    pub fn final_value(&self, name: &str) -> Option<Value> {
        self.records.last().and_then(|entry| entry.value(name))
    }
}

/// Runs `model` against `trajectory` over `options`.
pub fn simulate(
    model: &Model,
    trajectory: &dyn Trajectory,
    options: Options,
) -> Result<Trace, RefError> {
    let expanded = expand(model);
    check_admissible(&expanded.model)?;
    let mut env = initialize(&expanded, trajectory, options)?;
    let mut records = vec![record(&env)];
    env.committed = env.current.clone();
    // `initial()` is true at exactly one instant. Every later instant is an
    // ordinary event, and nothing else about the machinery changes.
    env.initial = false;
    for _ in 0..options.max_events {
        let Some(instant) = next_event_time(&expanded.model, &env, options.t_stop)? else {
            return Ok(Trace { records });
        };
        env.time = instant;
        refresh_continuous(&expanded.model, trajectory, &mut env);
        seed_condition_memory(&expanded, &mut env, trajectory, false)?;
        event_iteration(&expanded, &mut env, options, false)?;
        records.push(record(&env));
        env.committed = env.current.clone();
    }
    Err(RefError::TooManyEvents)
}

/// The §8.6 initialization instant.
///
/// Structurally this is an ordinary event with two differences: `initial()` is
/// true, and `pre` reads the start-value environment rather than a previous
/// event. Seeding `committed` from the start values is how §8.6's
/// `v = pre(v)` is realized — see [`Environment`] for why that seeding, rather
/// than making `pre` the identity, is the reading that keeps
/// `when initial() then ...` able to fire.
fn initialize(
    expanded: &Expanded,
    trajectory: &dyn Trajectory,
    options: Options,
) -> Result<Environment, RefError> {
    let current = start_environment(&expanded.model, options)?;
    let mut env = Environment {
        committed: current.clone(),
        current,
        initial: true,
        time: options.t_start,
    };
    refresh_continuous(&expanded.model, trajectory, &mut env);
    seed_condition_memory(expanded, &mut env, trajectory, true)?;
    event_iteration(expanded, &mut env, options, true)?;
    Ok(env)
}

/// Evaluates every `start` attribute.
///
/// Two passes: literals first, then the attributes that read them. The second
/// pass is what evaluates an activation buffer's `start = x.start > 2`, which
/// is the expansion's own text and the reason a condition already true at the
/// initial instant presents no edge.
fn start_environment(model: &Model, options: Options) -> Result<BTreeMap<String, Value>, RefError> {
    let mut values = BTreeMap::new();
    let mut deferred = Vec::new();
    for variable in &model.variables {
        if reads_a_variable(&variable.start) {
            deferred.push(variable);
        } else {
            let env = start_env(&values, options);
            values.insert(variable.name.clone(), eval(&variable.start, &env)?);
        }
    }
    for variable in deferred {
        let env = start_env(&values, options);
        let value = eval(&variable.start, &env)
            .map_err(|_| RefError::UnorderedStart(variable.name.clone()))?;
        values.insert(variable.name.clone(), value);
    }
    Ok(values)
}

/// The environment a `start` attribute is evaluated in: every bound name stands
/// for its own start value, which is exactly what `x.start` means.
///
/// `initial()` is **false** here. A start attribute describes the value a
/// variable carries on entry to the initial instant, and `initial()` is true
/// only *at* that instant — so an activation buffer whose condition is
/// `initial()` is seeded false and therefore has a rising edge there, which is
/// what makes `when initial() then ...` run.
fn start_env(values: &BTreeMap<String, Value>, options: Options) -> Environment {
    Environment {
        current: values.clone(),
        committed: values.clone(),
        initial: false,
        time: options.t_start,
    }
}

/// Whether `expr` reads any variable.
fn reads_a_variable(expr: &Expr) -> bool {
    match expr {
        Expr::Variable(_) | Expr::Pre(_) | Expr::Edge(_) => true,
        Expr::Literal(_) | Expr::Initial | Expr::Time => false,
        Expr::Not(operand) | Expr::Negate(operand) => reads_a_variable(operand),
        Expr::Binary(_, lhs, rhs) => reads_a_variable(lhs) || reads_a_variable(rhs),
        Expr::If(condition, consequent, alternative) => {
            reads_a_variable(condition)
                || reads_a_variable(consequent)
                || reads_a_variable(alternative)
        }
    }
}

/// Overwrites every continuous variable from the trajectory at `env.time`.
fn refresh_continuous(model: &Model, trajectory: &dyn Trajectory, env: &mut Environment) {
    for variable in &model.variables {
        if variable.variability != Variability::Continuous {
            continue;
        }
        if let Some(value) = trajectory.value_at(&variable.name, env.time) {
            env.current
                .insert(variable.name.clone(), Value::Real(value));
        }
    }
}

/// Refuses every model shape slice 1 cannot mean something definite about.
///
/// The reference is an oracle, so an inadmissible model must be *named*, not
/// approximated. Each rule below exists because the alternative was silent: a
/// parameter target simulated happily where the compiler rejects the model, an
/// undeclared target sprang into existence and then sat outside the Appendix B
/// convergence test, and a duplicated target reported "did not converge", which
/// names a symptom rather than the over-determination causing it.
fn check_admissible(model: &Model) -> Result<(), RefError> {
    let mut seen: BTreeSet<&str> = BTreeSet::new();
    for equation in &model.equations {
        let (Equation::Assign { target, .. } | Equation::InitialAssign { target, .. }) = equation
        else {
            continue;
        };
        let Some(declared) = model.variable(target) else {
            return Err(RefError::UndeclaredTarget(target.clone()));
        };
        match declared.variability {
            // The trajectory and an equation would both claim it.
            Variability::Continuous => {
                return Err(RefError::ContinuousIsDetermined(target.clone()));
            }
            // MLS §4.4.4: a parameter is fixed for the run. The compiler
            // rejects a model that assigns one; simulating it here would let
            // the reference disagree with a rejection.
            Variability::Parameter => {
                return Err(RefError::ParameterIsDetermined(target.clone()));
            }
            Variability::Discrete => {}
        }
        if !seen.insert(target.as_str()) {
            return Err(RefError::TargetIsOverDetermined(target.clone()));
        }
    }
    Ok(())
}

/// Whether `expr` reads a variable this model declares continuous.
///
/// Used only to decide whether a condition's value could have changed between
/// two events without slice 1 scheduling the crossing.
fn reads_continuous(expr: &Expr, model: &Model) -> bool {
    match expr {
        Expr::Variable(name) | Expr::Pre(name) | Expr::Edge(name) => model
            .variable(name)
            .is_some_and(|entry| entry.variability == Variability::Continuous),
        Expr::Literal(_) | Expr::Initial | Expr::Time => false,
        Expr::Not(operand) | Expr::Negate(operand) => reads_continuous(operand, model),
        Expr::Binary(_, lhs, rhs) => reads_continuous(lhs, model) || reads_continuous(rhs, model),
        Expr::If(condition, consequent, alternative) => {
            reads_continuous(condition, model)
                || reads_continuous(consequent, model)
                || reads_continuous(alternative, model)
        }
    }
}

/// Seeds each activation buffer's `pre` value for the instant `env.time`.
///
/// This sets **only** `pre(b)`. The buffer's own value stays live: it is solved
/// by the equations like anything else, so `edge(b)` is "the condition holds
/// now, and did not hold entering this instant". That is the rule the compiler
/// implements, and getting there took a correction worth recording.
///
/// An earlier version latched *both* limits and froze the buffer for the whole
/// event iteration. That is broader than MLS §8.5 supports. §8.5 holds a
/// relation constant *between* events; Appendix B re-solves the system *within*
/// one, and a condition variable `c := f(relation(v))` is part of what it
/// re-solves. Freezing it breaks a cascade — `when time >= 0.5 then x = 3` next
/// to `when x > 2 then y = 1` never fires the second clause, because `x > 2` was
/// sampled before `x` was written, and never fires later either, since by then
/// the condition holds on both limits. Appendix B solves `b1`, `x`, `b2`, `y`
/// together and reaches `y = 1`.
///
/// The self-rescheduling shape that motivated the latch is handled instead by
/// [`event_iteration`] performing one ordered pass per Appendix B iteration
/// rather than an inner fixed point — see there.
///
/// The initialization instant has no left limit, so the seed is evaluated at
/// `t_start` on the start values. That is §8.3.5.1's
/// `Boolean b(start = x.start > 2)`: a condition already true there presents no
/// edge (registry FS-EQN-001, FS-EQN-002). `initial()` is false in the seed in
/// every case, because it is true at exactly one instant and the instant being
/// entered is not yet it — which is what still lets `when initial() then ...`
/// fire (FS-EQN-003).
fn seed_condition_memory(
    expanded: &Expanded,
    env: &mut Environment,
    trajectory: &dyn Trajectory,
    at_initialization: bool,
) -> Result<(), RefError> {
    let mut left = Environment {
        current: env.committed.clone(),
        committed: env.committed.clone(),
        initial: false,
        time: if at_initialization {
            env.time
        } else {
            env.time.next_down()
        },
    };
    // Continuous variables are read at the left limit too. Leaving them at the
    // previous event's values manufactures an edge for any condition whose
    // continuous operand moved in between, which reports a crossing at the
    // wrong instant — worse than not reporting it.
    refresh_continuous(&expanded.model, trajectory, &mut left);
    for (name, condition) in &expanded.buffers {
        let seed = eval(condition, &left)?;
        // Slice 1 schedules time crossings only. If a condition over a
        // continuous variable changed value since the last instant settled,
        // a crossing happened that was never located, and reporting it here
        // would report it at this instant rather than the real one.
        if !at_initialization
            && reads_continuous(condition, &expanded.model)
            && env.committed.get(name) != Some(&seed)
        {
            return Err(RefError::UnlocatedCrossing(name.clone()));
        }
        env.committed.insert(name.clone(), seed);
    }
    Ok(())
}

/// Applies one equation, reporting whether it moved its target.
fn apply(equation: &Equation, env: &mut Environment, initial: bool) -> Result<bool, RefError> {
    let (target, value) = match equation {
        Equation::Assign { target, value } => (target, value),
        // MLS §8.6: "The equations of a when-clause are active during
        // initialization, if and only if they are explicitly enabled with
        // initial()." An `initial equation` clause is active there and nowhere
        // else, which is this line.
        Equation::InitialAssign { target, value } if initial => (target, value),
        _ => return Ok(false),
    };
    let next = eval(value, env)?;
    Ok(env.current.insert(target.clone(), next) != Some(next))
}

/// The MLS Appendix B event iteration.
///
/// ```text
/// loop
///   solve equations for unknowns, with pre(z) and pre(m) fixed
///   if z == pre(z) and m == pre(m) then break
///   pre(z) := z
///   pre(m) := m
/// end loop
/// ```
///
/// "Solve equations for unknowns" is one ordered pass here, not an inner fixed
/// point, and the difference is load-bearing rather than a shortcut. Slice 1's
/// equations are already in Appendix B.1c's almost-solved form, so repeated
/// outer iterations propagate a value exactly as an inner solve would — but
/// only the outer loop advances `pre`, and some inner systems have no solution
/// to find. `when time >= nextTime` whose body advances `nextTime` is one: with
/// `pre` held fixed, the buffer is true when the body has not run and false
/// once it has, so an inner fixed point does not exist and searching for one
/// spins. The outer loop reaches `nextTime` advanced once and `pre(b)` true,
/// which is the state the specification's loop is defined to stop at.
fn event_iteration(
    expanded: &Expanded,
    env: &mut Environment,
    options: Options,
    initial: bool,
) -> Result<(), RefError> {
    for _ in 0..options.max_iterations {
        for equation in &expanded.model.equations {
            apply(equation, env, initial)?;
        }
        if discrete_agrees_with_pre(&expanded.model, env) {
            return Ok(());
        }
        env.committed = env.current.clone();
    }
    Err(RefError::NotConverged {
        phase: "event iteration",
    })
}

/// Whether `z == pre(z)` and `m == pre(m)` for every discrete variable.
///
/// Continuous variables are excluded because Appendix B excludes them: the
/// event iteration's termination test is over the discrete state only.
fn discrete_agrees_with_pre(model: &Model, env: &Environment) -> bool {
    model
        .variables
        .iter()
        .filter(|variable| variable.variability == Variability::Discrete)
        .all(|variable| env.current.get(&variable.name) == env.committed.get(&variable.name))
}

/// Snapshots the settled environment.
fn record(env: &Environment) -> Record {
    Record {
        time: env.time,
        values: env.current.clone(),
    }
}
