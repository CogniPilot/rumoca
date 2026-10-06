//! Real `min`/`max` (SPEC_0040 SOLVE-C65) in every generated language.
//!
//! The rule is written once, as [`rule`], and printed per language, so each
//! target defines `rumoca_real_min`/`rumoca_real_max` helpers with the value
//! `rumoca_ir_solve::real_extremum` computes: a single NaN operand is ignored,
//! two NaN operands give the quiet NaN `a + b`, and otherwise the strictly
//! smaller (larger) operand is returned, `b` on a tie. Generated code calls the
//! helpers instead of a library `fmin`/`fmax`/`minimum`/`min`, whose NaN and
//! signed-zero results differ between libraries and targets.

/// The helper names generated code calls for `min` and `max`.
pub(crate) const fn helper_name(minimum: bool) -> &'static str {
    if minimum {
        "rumoca_real_min"
    } else {
        "rumoca_real_max"
    }
}

enum Rule {
    A,
    B,
    IsNan(Box<Rule>),
    And(Box<Rule>, Box<Rule>),
    Or(Box<Rule>, Box<Rule>),
    Add(Box<Rule>, Box<Rule>),
    /// `a < b` for the minimum, `a > b` for the maximum.
    Strict,
    Select(Box<Rule>, Box<Rule>, Box<Rule>),
}

/// `select(isnan(a) && isnan(b), a + b, select(isnan(b) || strict(a, b), a, b))`.
fn rule() -> Rule {
    use Rule::{A, Add, And, B, IsNan, Or, Select, Strict};
    let nan = |operand| Box::new(IsNan(Box::new(operand)));
    Select(
        Box::new(And(nan(A), nan(B))),
        Box::new(Add(Box::new(A), Box::new(B))),
        Box::new(Select(
            Box::new(Or(nan(B), Box::new(Strict))),
            Box::new(A),
            Box::new(B),
        )),
    )
}

/// One expression syntax: how each node of [`rule`] prints.
struct Syntax {
    is_nan: fn(String) -> String,
    and: fn(String, String) -> String,
    or: fn(String, String) -> String,
    select: fn(String, String, String) -> String,
}

fn print(rule: &Rule, syntax: &Syntax, minimum: bool) -> String {
    match rule {
        Rule::A => "a".into(),
        Rule::B => "b".into(),
        Rule::IsNan(operand) => (syntax.is_nan)(print(operand, syntax, minimum)),
        Rule::And(lhs, rhs) => {
            (syntax.and)(print(lhs, syntax, minimum), print(rhs, syntax, minimum))
        }
        Rule::Or(lhs, rhs) => (syntax.or)(print(lhs, syntax, minimum), print(rhs, syntax, minimum)),
        Rule::Add(lhs, rhs) => format!(
            "({} + {})",
            print(lhs, syntax, minimum),
            print(rhs, syntax, minimum)
        ),
        Rule::Strict => format!("(a {} b)", if minimum { "<" } else { ">" }),
        Rule::Select(condition, then, otherwise) => (syntax.select)(
            print(condition, syntax, minimum),
            print(then, syntax, minimum),
            print(otherwise, syntax, minimum),
        ),
    }
}

const C: Syntax = Syntax {
    is_nan: |x| format!("isnan({x})"),
    and: |x, y| format!("({x} && {y})"),
    or: |x, y| format!("({x} || {y})"),
    select: |c, t, f| format!("({c} ? {t} : {f})"),
};

const RUST: Syntax = Syntax {
    is_nan: |x| format!("{x}.is_nan()"),
    and: |x, y| format!("({x} && {y})"),
    or: |x, y| format!("({x} || {y})"),
    select: |c, t, f| format!("(if {c} {{ {t} }} else {{ {f} }})"),
};

const JAX: Syntax = Syntax {
    is_nan: |x| format!("jnp.isnan({x})"),
    and: |x, y| format!("({x} & {y})"),
    or: |x, y| format!("({x} | {y})"),
    select: |c, t, f| format!("jnp.where({c}, {t}, {f})"),
};

const CASADI: Syntax = Syntax {
    // CasADi has no isnan; NaN is the only value unequal to itself.
    is_nan: |x| format!("({x} != {x})"),
    and: |x, y| format!("ca.logic_and({x}, {y})"),
    or: |x, y| format!("ca.logic_or({x}, {y})"),
    select: |c, t, f| format!("ca.if_else({c}, {t}, {f})"),
};

const WGSL: Syntax = Syntax {
    is_nan: |x| format!("({x} != {x})"),
    and: |x, y| format!("({x} && {y})"),
    or: |x, y| format!("({x} || {y})"),
    select: |c, t, f| format!("select({f}, {t}, {c})"),
};

/// The helper definitions a generated `language` source declares before its
/// first use: `c`, `cuda`, `rust`, `jax`, `casadi`, `wgsl` or `mlir`. WebGPU lets
/// a WGSL implementation assume no NaN occurs, so only the NaN-free cases of
/// the rule are guaranteed there.
pub(crate) fn prelude(language: &str) -> Option<String> {
    let helpers = |each: &dyn Fn(bool) -> String| format!("{}\n{}\n", each(true), each(false));
    Some(match language {
        "c" | "cuda" => {
            let qualifier = if language == "cuda" {
                "__host__ __device__ static inline"
            } else {
                "static inline"
            };
            helpers(&|minimum| {
                format!(
                    "{qualifier} double {}(double a, double b) {{ return {}; }}",
                    helper_name(minimum),
                    print(&rule(), &C, minimum)
                )
            })
        }
        "rust" => helpers(&|minimum| {
            format!(
                "#[allow(dead_code, unused_parens)]\n#[inline]\nfn {}(a: f64, b: f64) -> f64 {{\n    {}\n}}",
                helper_name(minimum),
                print(&rule(), &RUST, minimum)
            )
        }),
        "jax" | "casadi" => {
            let syntax = if language == "jax" { &JAX } else { &CASADI };
            helpers(&|minimum| {
                format!(
                    "def {}(a, b):\n    return {}\n",
                    helper_name(minimum),
                    print(&rule(), syntax, minimum)
                )
            })
        }
        "wgsl" => helpers(&|minimum| {
            format!(
                "fn {}(a: f32, b: f32) -> f32 {{ return {}; }}",
                helper_name(minimum),
                print(&rule(), &WGSL, minimum)
            )
        }),
        "mlir" => helpers(&|minimum| mlir_function(minimum)),
        _ => return None,
    })
}

/// MLIR has no expression syntax; the rule is printed as SSA.
fn mlir_function(minimum: bool) -> String {
    let strict = if minimum { "olt" } else { "ogt" };
    format!(
        "  func.func private @{name}(%a: f64, %b: f64) -> f64 {{\n\
         \x20   %an = arith.cmpf uno, %a, %a : f64\n\
         \x20   %bn = arith.cmpf uno, %b, %b : f64\n\
         \x20   %both = arith.andi %an, %bn : i1\n\
         \x20   %strict = arith.cmpf {strict}, %a, %b : f64\n\
         \x20   %keep = arith.ori %bn, %strict : i1\n\
         \x20   %chosen = arith.select %keep, %a, %b : f64\n\
         \x20   %quiet = arith.addf %a, %b : f64\n\
         \x20   %r = arith.select %both, %quiet, %chosen : f64\n\
         \x20   func.return %r : f64\n\
         \x20 }}",
        name = helper_name(minimum)
    )
}

/// One MLIR call of the helper, `dst = func.call ...`.
pub(crate) fn mlir_call(minimum: bool, dst: &str, lhs: &str, rhs: &str) -> String {
    format!(
        "{dst} = func.call @{}({lhs}, {rhs}) : (f64, f64) -> f64",
        helper_name(minimum)
    )
}

pub(super) fn real_extremum_prelude_function(language: String) -> Result<String, minijinja::Error> {
    prelude(&language).ok_or_else(|| {
        minijinja::Error::new(
            minijinja::ErrorKind::InvalidOperation,
            format!("no real min/max helper for language `{language}`"),
        )
    })
}

#[cfg(test)]
mod tests;
