//! Real `min`/`max` (SPEC_0040 SOLVE-C66) in every generated language.
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
#[cfg(test)]
pub(crate) fn prelude(language: &str) -> Option<String> {
    prelude_for(language, ExtremumDemand::BOTH)
}

/// The helper definitions of `language` for exactly the demanded operators, so
/// a translation unit never defines an unused helper.
pub(crate) fn prelude_for(language: &str, demand: ExtremumDemand) -> Option<String> {
    let helpers = |each: &dyn Fn(bool) -> String| {
        [(true, demand.minimum), (false, demand.maximum)]
            .into_iter()
            .filter(|(_, wanted)| *wanted)
            .map(|(minimum, _)| format!("{}\n", each(minimum)))
            .collect::<String>()
    };
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

/// Which Real extremum operators a rendered product applies, so each helper is
/// defined only when its own operator is used.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, serde::Serialize)]
pub(crate) struct ExtremumDemand {
    pub(crate) minimum: bool,
    pub(crate) maximum: bool,
}

impl ExtremumDemand {
    pub(crate) const BOTH: Self = Self {
        minimum: true,
        maximum: true,
    };

    pub(crate) const fn of(minimum: bool) -> Self {
        Self {
            minimum,
            maximum: !minimum,
        }
    }

    #[cfg(test)]
    pub(crate) const fn any(self) -> bool {
        self.minimum || self.maximum
    }

    fn merge(&mut self, other: Self) {
        self.minimum |= other.minimum;
        self.maximum |= other.maximum;
    }
}

/// The render-context key that carries the [`ExtremumDemand`] of the rendered
/// Solve problem and its artifacts.
pub(super) const USES_CONTEXT_KEY: &str = "uses_real_extremum";

/// Which Real extrema the rendered problem, artifacts or canonical pure-call
/// owners apply, including typed nested regions and directional bodies.
///
/// `typed_calls` are the owners rendered as typed bodies (an FMI component);
/// `inlined_calls` are the owners a scalar target inlines.
pub(super) fn used_by(
    problem: &rumoca_ir_solve::SolveProblem,
    artifacts: &rumoca_ir_solve::SolveArtifacts,
    typed_calls: Option<&rumoca_ir_solve::SolvePureCallTable>,
    inlined_calls: Option<&rumoca_ir_solve::SolvePureCallTable>,
) -> ExtremumDemand {
    use rumoca_ir_solve::{BinaryOp, LinearOp, LinearOpSliceKind, SolveVisitor};

    struct Uses(ExtremumDemand);

    impl SolveVisitor for Uses {
        type Error = std::convert::Infallible;

        fn visit_typed_operation(
            &mut self,
            program: &rumoca_ir_solve::TypedProgram,
            _index: usize,
            op: &rumoca_ir_solve::SolveSpannedOperation,
        ) -> Result<(), Self::Error> {
            use rumoca_ir_solve::{
                SolveBinaryOperator, SolveOperation, SolveReductionOperator, SolveScalarType,
            };
            let (destination, minimum) = match op.operation() {
                SolveOperation::Binary {
                    destination,
                    operator: operator @ (SolveBinaryOperator::Min | SolveBinaryOperator::Max),
                    ..
                }
                | SolveOperation::BroadcastBinary {
                    destination,
                    operator: operator @ (SolveBinaryOperator::Min | SolveBinaryOperator::Max),
                    ..
                } => (destination, matches!(operator, SolveBinaryOperator::Min)),
                SolveOperation::Reduce {
                    destination,
                    operator:
                        operator @ (SolveReductionOperator::Minimum | SolveReductionOperator::Maximum),
                    ..
                } => (
                    destination,
                    matches!(operator, SolveReductionOperator::Minimum),
                ),
                _ => return Ok(()),
            };
            if matches!(
                program.register_types()[destination.index()].element_type(),
                SolveScalarType::Real { .. }
            ) {
                self.0.merge(ExtremumDemand::of(minimum));
            }
            Ok(())
        }

        fn visit_linear_op(
            &mut self,
            _kind: LinearOpSliceKind,
            _index: usize,
            op: &LinearOp,
        ) -> Result<(), Self::Error> {
            if let LinearOp::Binary {
                op: op @ (BinaryOp::Min | BinaryOp::Max),
                ..
            }
            | LinearOp::TensorBinary {
                op: op @ (BinaryOp::Min | BinaryOp::Max),
                ..
            } = op
            {
                self.0
                    .merge(ExtremumDemand::of(matches!(op, BinaryOp::Min)));
            }
            Ok(())
        }
    }

    let mut uses = Uses(ExtremumDemand::default());
    let Ok(()) = uses.visit_solve_problem(problem);
    let Ok(()) = uses.visit_solve_artifacts(artifacts);
    if let Some(table) = typed_calls {
        let Ok(()) = uses.visit_pure_call_table(table);
    }
    // A scalar target that inlines pure calls emits their bodies' min/max as
    // scalar ops the Solve scan above cannot see. Unlike `typed_calls`
    // (typed C bodies), every element type and every min/max reduction counts:
    // inlining lowers all of them to the scalar `Min`/`Max` the helper renders.
    if let Some(table) = inlined_calls {
        for owner in table.owners() {
            uses.0.merge(program_extremum_demand(owner.body()));
        }
    }
    uses.0
}

/// Which of `min`/`max` a typed program, or any region nested in it, applies.
fn program_extremum_demand(program: &rumoca_ir_solve::TypedProgram) -> ExtremumDemand {
    use rumoca_ir_solve::{SolveBinaryOperator, SolveOperation, SolveReductionOperator};
    let mut demand = ExtremumDemand::default();
    for operation in program.operations() {
        match operation.operation() {
            SolveOperation::Binary { operator, .. }
            | SolveOperation::BroadcastBinary { operator, .. } => match operator {
                SolveBinaryOperator::Min => demand.merge(ExtremumDemand::of(true)),
                SolveBinaryOperator::Max => demand.merge(ExtremumDemand::of(false)),
                _ => {}
            },
            SolveOperation::Reduce { operator, .. } => match operator {
                SolveReductionOperator::Minimum => demand.merge(ExtremumDemand::of(true)),
                SolveReductionOperator::Maximum => demand.merge(ExtremumDemand::of(false)),
                _ => {}
            },
            SolveOperation::Map { body, .. } => {
                demand.merge(program_extremum_demand(body.body()));
            }
            SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => {
                demand.merge(program_extremum_demand(transition.body()));
                if let Some(region) = continuation.as_deref() {
                    demand.merge(program_extremum_demand(region.body()));
                }
            }
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                demand.merge(program_extremum_demand(if_true.body()));
                demand.merge(program_extremum_demand(if_false.body()));
            }
            _ => {}
        }
    }
    demand
}

/// The template function `real_extremum_prelude(language)`: the helper
/// definitions the Solve render context proves the product applies, each only
/// when its own operator is used, so a translation unit never defines an
/// unused helper. Contexts without a Solve problem always declare both.
pub(super) fn real_extremum_prelude_function(
    state: &minijinja::State,
    language: String,
    always: Option<bool>,
) -> Result<String, minijinja::Error> {
    // `always`: the caller also renders code the solve-IR scan cannot see
    // (typed pure-function bodies), so emit the inline helpers regardless.
    let demand = match state.lookup(USES_CONTEXT_KEY) {
        Some(uses) if always != Some(true) => {
            let wanted = |name| uses.get_attr(name).is_ok_and(|flag| flag.is_true());
            ExtremumDemand {
                minimum: wanted("minimum"),
                maximum: wanted("maximum"),
            }
        }
        _ => ExtremumDemand::BOTH,
    };
    prelude_for(&language, demand).ok_or_else(|| {
        minijinja::Error::new(
            minijinja::ErrorKind::InvalidOperation,
            format!("no real min/max helper for language `{language}`"),
        )
    })
}

#[cfg(test)]
mod tests;
