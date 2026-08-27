//! Tangent rules for builtin operators.
//!
//! Three sets partition the builtins this engine meets: operators with a
//! tangent rule, operators whose result cannot move with a Real perturbation,
//! and operators that are stated non-differentiable and refuse. A name in none
//! of the three is not a builtin at all and is looked up as a function.
//!
//! **Shape of the rule text (JAC-T2).** A rule that joins two terms whose
//! shapes the call does not have to match writes the join elementwise. Every
//! `+`, `-`, `*` and `/` that survives below joins operands of provably equal
//! rank: `log(10.0)*v` and `2.0*sqrt(v)` are a scalar spread over one array
//! (MLS 10.6.3), and `cross`/`outerProduct` join two rank-1 and two rank-2
//! results. `atan2` is the rule whose operands may differ in rank, so both its
//! sum and its difference are elementwise.

use rumoca_ir_ast as ast;

/// Builtins whose value cannot move under a Real perturbation.
///
/// `String`, `Integer` and `Boolean` are not here. They return a String, an
/// Integer and a Boolean, so no Real expression can contain one, and a claim
/// no probe can reach is a claim nothing checks (JAC-T4). A call to one of
/// them inside a differentiated expression refuses as a name this engine
/// carries no rule for.
///
/// `ndims` is not here either, for the same reason `scalar` and `matrix` are
/// not in the rule table: this compiler's lowering has no owner for it at any
/// shape (`ED018`), so a probe cannot run the primal the derivative would be
/// compared against.
pub(crate) const CONSTANT: &[&str] = &["zeros", "ones", "identity", "size"];

/// Builtins stated non-differentiable, or not admitted in a differentiated
/// body at all.
pub(crate) const REFUSED: &[&str] = &[
    "sign",
    "floor",
    "ceil",
    "integer",
    "div",
    "mod",
    "rem",
    "max",
    "min",
    "noEvent",
    "smooth",
    "pre",
    "edge",
    "change",
    "sample",
    "semiLinear",
    "homotopy",
    "der",
    "delay",
    "terminal",
    "initial",
    "product",
    "linspace",
    "cardinality",
    "reinit",
];

/// A builtin's tangent rule shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Rule {
    /// One argument, one rule.
    Unary,
    /// Two arguments, one rule.
    Binary,
    /// A rule that maps every trailing argument through the derivative.
    Mapped,
}

/// Whether `name` is a builtin whose derivative is the structural zero.
pub(crate) fn is_constant(name: &str) -> bool {
    CONSTANT.contains(&name)
}

/// Whether `name` is a builtin this engine refuses to differentiate.
pub(crate) fn is_refused(name: &str) -> bool {
    REFUSED.contains(&name)
}

/// Builtins carrying a one-argument rule.
pub(crate) const UNARY_RULE: &[&str] = &[
    "sin",
    "cos",
    "tan",
    "exp",
    "log",
    "log10",
    "sqrt",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
    "abs",
    "transpose",
    "sum",
    "diagonal",
];

/// Builtins carrying a two-argument rule.
pub(crate) const BINARY_RULE: &[&str] = &["atan2", "cross", "outerProduct"];

/// Builtins whose rule maps the derivative over their value arguments.
pub(crate) const MAPPED_RULE: &[&str] = &["cat", "fill"];

/// The rule shape carried for `name`, if this engine carries one.
///
/// `scalar`, `vector`, `matrix` and `symmetric` are linear and would carry the
/// obvious rule, but this compiler has no runtime owner for them, so no
/// finite-difference row can check one. They are left out rather than stated
/// unchecked, and a call to one refuses as a name this engine does not know.
pub(crate) fn rule(name: &str) -> Option<Rule> {
    if UNARY_RULE.contains(&name) {
        return Some(Rule::Unary);
    }
    if BINARY_RULE.contains(&name) {
        return Some(Rule::Binary);
    }
    if MAPPED_RULE.contains(&name) {
        return Some(Rule::Mapped);
    }
    None
}

/// Whether `name` is any builtin this engine recognizes.
pub(crate) fn is_builtin(name: &str) -> bool {
    is_constant(name) || is_refused(name) || rule(name).is_some()
}

/// Whether a builtin's rule is the derivative of the branch its value takes.
///
/// Such a rule states one condition, so it is the rule of a scalar argument
/// only; a vectorized call has one condition per element.
pub(crate) fn branches_on_its_argument(name: &str) -> bool {
    name == "abs"
}

/// The rank `name(args)` returns, when this engine states one.
///
/// The vectorized-call rule (MLS 10.6.4) is what makes the elementary
/// functions rank-preserving: called on an array they map over it. A builtin
/// that is not defined at the shapes it was called at has no rank here, which
/// is JAC-T1: a rank this engine cannot state is a refusal, never a guess.
/// Tools differ on how far they vectorize a call like `cross` over an extra
/// dimension, so a rank claimed on one tool's reading would be a claim the
/// exported artifact could not keep.
pub(crate) fn result_rank(
    name: &str,
    args: &[ast::Expression],
    rank: &dyn Fn(&ast::Expression) -> Option<usize>,
) -> Option<usize> {
    match name {
        "sin" | "cos" | "tan" | "exp" | "log" | "log10" | "sqrt" | "asin" | "acos" | "atan"
        | "sinh" | "cosh" | "tanh" | "abs" => rank(args.first()?),
        "atan2" => crate::engine::paired_rank(rank(args.first()?)?, rank(args.get(1)?)?),
        "sum" | "size" => (rank(args.first()?)? >= 1).then_some(0),
        "transpose" => {
            let held = rank(args.first()?)?;
            (held >= 2).then_some(held)
        }
        "diagonal" => (rank(args.first()?)? == 1).then_some(2),
        "outerProduct" => vector_pair(args, rank).then_some(2),
        "cross" => vector_pair(args, rank).then_some(1),
        "identity" => Some(2),
        "zeros" | "ones" => Some(args.len()),
        "fill" => Some(rank(args.first()?)? + args.len().checked_sub(1)?),
        "cat" => {
            let held = rank(args.get(1)?)?;
            (held >= 1 && args[1..].iter().all(|value| rank(value) == Some(held))).then_some(held)
        }
        _ => None,
    }
}

/// Whether a two-argument call was given two rank-1 operands.
fn vector_pair(args: &[ast::Expression], rank: &dyn Fn(&ast::Expression) -> Option<usize>) -> bool {
    let [left, right] = args else {
        return false;
    };
    rank(left) == Some(1) && rank(right) == Some(1)
}

/// The derivative of `name(args)` given each argument's derivative.
///
/// `slopes` is parallel to `args`; a [`None`] entry is a structurally zero
/// argument. The caller guarantees at least one entry is [`Some`], so a rule
/// that returns [`None`] here is a rule this engine does not carry.
pub(crate) fn apply(
    name: &str,
    args: &[ast::Expression],
    slopes: &[Option<String>],
) -> Option<String> {
    match rule(name)? {
        Rule::Unary => unary(name, args.first()?, slopes.first()?.as_deref()?),
        Rule::Binary => binary(name, args, slopes),
        Rule::Mapped => mapped(name, args, slopes),
    }
}

/// The tangent of a one-argument builtin.
///
/// Every product and quotient below is elementwise, because a vectorized call
/// applies the scalar rule at every entry and `*` between two arrays is the
/// scalar product. On rank-zero values the elementwise operators are the
/// ordinary ones (MLS 10.6.5), so one spelling serves both shapes.
///
/// Three rules are written through a trigonometric identity rather than
/// through the textbook `1 +/- v*v`: a literal `1` in those formulas is a
/// rank-0 constant the rule would have to give the argument's shape, and
/// Modelica states no such constant. `cos(atan(v))^2` is `1/(1 + v*v)`, and
/// `cos(asin(v))` and `sin(acos(v))` are both `sqrt(1 - v*v)`; each holds
/// exactly, at every entry, for any shape. The three are `atan`, `asin` and
/// `acos`; `tan` and `tanh` need no identity, because `cos(v)` and `cosh(v)`
/// already take the argument's shape.
fn unary(name: &str, argument: &ast::Expression, slope: &str) -> Option<String> {
    let value = format!("({argument})");
    let text = match name {
        "sin" => format!("cos{value} .* ({slope})"),
        "cos" => format!("-sin{value} .* ({slope})"),
        "tan" => format!("({slope}) ./ (cos{value} .* cos{value})"),
        "exp" => format!("exp{value} .* ({slope})"),
        "log" => format!("({slope}) ./ {value}"),
        // The natural logarithm of ten is written `log(10.0)`, not `log(10)`:
        // the argument of a Real function has to be Real here.
        "log10" => format!("({slope}) ./ (log(10.0)*{value})"),
        "sqrt" => format!("({slope}) ./ (2.0*sqrt{value})"),
        "asin" => format!("({slope}) ./ cos(asin{value})"),
        "acos" => format!("-({slope}) ./ sin(acos{value})"),
        "atan" => format!("({slope}) .* cos(atan{value}) .* cos(atan{value})"),
        "sinh" => format!("cosh{value} .* ({slope})"),
        "cosh" => format!("sinh{value} .* ({slope})"),
        "tanh" => format!("({slope}) ./ (cosh{value} .* cosh{value})"),
        // Taken-branch convention: the derivative at a kink is the derivative
        // of the branch the value takes, and `abs` is not differentiable at 0.
        // The one condition is why this rule admits a rank-zero argument only.
        "abs" => format!("(if {value} >= 0 then ({slope}) else -({slope}))"),
        "transpose" | "sum" | "diagonal" => format!("{name}({slope})"),
        _ => return None,
    };
    Some(text)
}

fn binary(name: &str, args: &[ast::Expression], slopes: &[Option<String>]) -> Option<String> {
    let [left, right] = args else {
        return None;
    };
    let [left_slope, right_slope] = slopes else {
        return None;
    };
    match name {
        // `d atan2(y, x) = (x*dy - y*dx)/(x*x + y*y)`, elementwise so a
        // vectorized call keeps its shape.
        //
        // The denominator's sum is elementwise too. `atan2` pairs its operands
        // (MLS 10.6.4), so one may be a scalar while the other is an array,
        // and Modelica has no `+` between those two shapes; each product below
        // takes the shape of its own operand, which is why only `.+` states a
        // sum that holds at every admitted pair of shapes.
        "atan2" => {
            let left_term = left_slope
                .as_ref()
                .map(|slope| format!("({right}) .* ({slope})"));
            let right_term = right_slope
                .as_ref()
                .map(|slope| format!("({left}) .* ({slope})"));
            let numerator = match (left_term, right_term) {
                (Some(first), Some(second)) => format!("{first} .- {second}"),
                (Some(first), None) => first,
                (None, Some(second)) => format!("-{second}"),
                (None, None) => return None,
            };
            Some(format!(
                "({numerator}) ./ (({left}) .* ({left}) .+ ({right}) .* ({right}))"
            ))
        }
        "cross" | "outerProduct" => {
            let terms: Vec<String> = [
                left_slope
                    .as_ref()
                    .map(|slope| format!("{name}({slope}, {right})")),
                right_slope
                    .as_ref()
                    .map(|slope| format!("{name}({left}, {slope})")),
            ]
            .into_iter()
            .flatten()
            .collect();
            (!terms.is_empty()).then(|| terms.join(" + "))
        }
        _ => None,
    }
}

/// `cat(n, a, b, …)` and `fill(v, n, …)` differentiate by mapping the
/// derivative over the value arguments and keeping the shape arguments.
fn mapped(name: &str, args: &[ast::Expression], slopes: &[Option<String>]) -> Option<String> {
    let value_range = match name {
        "cat" => 1..args.len(),
        "fill" => 0..1,
        _ => return None,
    };
    let mut rendered: Vec<String> = Vec::with_capacity(args.len());
    for (position, argument) in args.iter().enumerate() {
        if value_range.contains(&position) {
            let slope = slopes.get(position)?.clone();
            rendered.push(slope.unwrap_or_else(|| crate::engine::zero_like(argument)));
        } else {
            rendered.push(argument.to_string());
        }
    }
    Some(format!("{name}({})", rendered.join(", ")))
}
