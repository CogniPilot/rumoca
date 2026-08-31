//! Synthesized Jacobians against central finite differences.
//!
//! Every supported statement form appears in the battery below, each case is
//! evaluated at pseudo-random points at three scales, and the comparison runs
//! inside the compiled model: the probe computes the synthesized Jacobian, the
//! central-difference Jacobian of the same function, their largest entrywise
//! gap, and the size of the Jacobian itself. A synthesized derivative that
//! collapsed to zero would match nothing, so the magnitude is asserted too.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

/// One differentiated function and the shape of its Jacobian.
struct Case {
    /// Name used in failure messages and as the probe model name.
    name: &'static str,
    /// Modelica function declarations the probe needs.
    functions: &'static str,
    /// Probe declarations the call refers to, verbatim.
    context: &'static str,
    /// The differentiated call, with `{x}` where the differentiated argument
    /// goes, so the probe can pass a perturbed argument to the same call.
    call: &'static str,
    /// Jacobian rows.
    rows: usize,
    /// Jacobian columns, and the width of the differentiated argument.
    width: usize,
    /// True when the differentiated argument is a scalar.
    scalar_argument: bool,
    /// True when the differentiated function returns a scalar.
    scalar_result: bool,
}

const ARITHMETIC: Case = Case {
    name: "Arithmetic",
    functions: r"
function arith
  input Real u;
  input Real v;
  output Real y;
protected
  Real t;
algorithm
  t := (u + 2.0*v)*(u - v);
  y := t/(1.0 + u*u) + u^3 - (-v);
end arith;
",
    context: "  parameter Real v = 0.7;\n",
    call: "arith({x}, v)",
    rows: 1,
    width: 1,
    scalar_argument: true,
    scalar_result: true,
};

const ELEMENTARY: Case = Case {
    name: "Elementary",
    functions: r"
function elementary
  input Real u[4];
  output Real y[3];
algorithm
  y[1] := sin(u[1])*cos(u[2]) + exp(u[3]/4.0);
  y[2] := sqrt(1.0 + u[2]*u[2]) + tanh(u[4]) + atan(u[1]);
  y[3] := log(2.0 + u[3]*u[3])*abs(u[4] + 6.0);
end elementary;
",
    context: "",
    call: "elementary({x})",
    rows: 3,
    width: 4,
    scalar_argument: false,
    scalar_result: false,
};

const FOR_LOOP: Case = Case {
    name: "ForLoop",
    functions: r"
function accumulate
  input Real u[4];
  input Integer n;
  output Real y[2];
protected
  Real running;
algorithm
  running := 1.0;
  for i in 1:4 loop
    running := running + u[i]*u[i]*(0.5 + 0.1*i);
  end for;
  y[1] := running;
  y[2] := 0.0;
  for i in 1:n loop
    y[2] := y[2] + sin(u[i])*running;
  end for;
end accumulate;
",
    context: "  parameter Integer n = 3;\n",
    call: "accumulate({x}, n)",
    rows: 2,
    width: 4,
    scalar_argument: false,
    scalar_result: false,
};

const BRANCH: Case = Case {
    name: "Branch",
    functions: r"
function branched
  input Real u[3];
  output Real y[2];
protected
  Real gain;
algorithm
  if u[1] > 0.0 then
    gain := 2.0 + u[2];
  elseif u[2] > 0.0 then
    gain := 1.0 - u[3];
  else
    gain := 0.5*u[1]*u[3];
  end if;
  y[1] := gain*u[1];
  y[2] := (if u[3] > 0.0 then gain*u[3] else gain - u[3]);
end branched;
",
    context: "",
    call: "branched({x})",
    rows: 2,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

const COMPOSED: Case = Case {
    name: "Composed",
    functions: r"
function stage
  input Real u[3];
  input Real gain;
  output Real y[3];
algorithm
  y[1] := gain*sin(u[1]) + u[2];
  y[2] := u[2]*u[3];
  y[3] := exp(u[3]/3.0);
end stage;

function composite
  input Real u[3];
  output Real y[2];
protected
  Real mid[3];
algorithm
  mid := stage(u, 1.5);
  y[1] := mid[1]*mid[3] + mid[2];
  y[2] := sum(mid);
end composite;
",
    context: "",
    call: "composite({x})",
    rows: 2,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

const MATRIX: Case = Case {
    name: "Matrix",
    functions: r"
function rotate
  input Real u[3];
  input Real m[3, 3];
  output Real y[3];
protected
  Real skewed[3, 3];
algorithm
  skewed := transpose(m)*m;
  y := skewed*u + cross(u, {1.0, 0.5, -0.25});
end rotate;
",
    context: "  parameter Real m[3, 3] = {{1.0, 0.2, -0.3}, {0.1, 0.9, 0.4}, {-0.2, 0.3, 1.1}};\n",
    call: "rotate({x}, m)",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

const CONCATENATED: Case = Case {
    name: "Concatenated",
    functions: r#"
function stacked
  input Real u[2];
  output Real y[4];
protected
  Real head[2];
  Real tail[2];
algorithm
  head := {u[1]*u[2], sin(u[1])};
  tail := {exp(u[2]), u[1] - u[2]};
  y := cat(1, head, tail);
  assert(size(y, 1) == 4, "stacked builds four rows");
end stacked;
"#,
    context: "",
    call: "stacked({x})",
    rows: 4,
    width: 2,
    scalar_argument: false,
    scalar_result: false,
};

/// `.^` on a vector, whose tangent is elementwise in both factors.
///
/// The scalar `*` that the chain rule reads for a rank-0 base is Modelica's
/// scalar product here (MLS 10.6.3), so a tangent that used it would collapse
/// a diagonal Jacobian to a rank-one one and still elaborate everywhere.
const ELEMENTWISE_POWER: Case = Case {
    name: "ElementwisePower",
    functions: r"
function powelem
  input Real u[3];
  input Real k[3];
  output Real y[3];
algorithm
  y := k .* (u .^ 2) .+ (u .^ 3) ./ (k .* k);
end powelem;
",
    context: "  parameter Real k[3] = {1.5, 3.0, -2.5};\n",
    call: "powelem({x}, k)",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

/// The elementwise arithmetic operators applied to arrays.
const ELEMENTWISE_OPS: Case = Case {
    name: "ElementwiseOps",
    functions: r"
function elementwise
  input Real u[3];
  input Real k[3];
  output Real y[3];
protected
  Real t[3];
algorithm
  t := (u .* k) .+ (u ./ k);
  y := (t .- u) .* (u .+ k) ./ (k .* k);
end elementwise;
",
    context: "  parameter Real k[3] = {1.5, 3.0, -2.5};\n",
    call: "elementwise({x}, k)",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

/// Every rule-carrying elementary builtin, called on a whole array.
///
/// A vectorized call (MLS 10.6.4) maps the scalar function over the array, so
/// its tangent has to stay elementwise too. `g` is at least `c`, which keeps
/// `sqrt`, `log` and `log10` inside their domains, and `w` is bounded by
/// `1/(2*sqrt(c))`, which keeps `asin` and `acos` inside theirs.
const VECTORIZED: Case = Case {
    name: "Vectorized",
    functions: r"
function vectorized
  input Real u[3];
  input Real c[3];
  output Real y[3];
protected
  Real g[3];
  Real w[3];
algorithm
  g := c .+ u .* u;
  w := u ./ g;
  y := atan(u) .* tanh(u) .+ exp(u ./ 4.0) .- sin(u) .* cos(u);
  y := y .+ sqrt(g) ./ log(g) .+ asin(w) .- acos(w);
  y := y .+ sinh(u ./ 3.0) .* cosh(u ./ 3.0) .+ log10(g) .* atan2(u, g);
end vectorized;
",
    context: "  parameter Real c[3] = {2.0, 2.0, 2.0};\n",
    call: "vectorized({x}, c)",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

/// The shape-carrying builtins, the builtins whose tangent is the structural
/// zero, and the unary operators.
///
/// `identity`, `zeros`, `ones` and `size` read or build constants, so their
/// tangent vanishes; the row is here because a vanishing tangent is a claim
/// like any other, and the gate's own magnitude assertion would catch one that
/// swallowed the terms around it. `u*u` between two vectors is Modelica's
/// scalar product, whose product rule this row is the only one to exercise.
const SHAPED: Case = Case {
    name: "Shaped",
    functions: r"
function shaped
  input Real u[3];
  output Real y[3];
protected
  Real m[3, 3];
  Real s[3, 3];
algorithm
  m := diagonal(u) + outerProduct(u, u);
  s := m + transpose(m);
  y := s*u + fill(u[3], 3) + tan(u/4.0) + cross(u, {1.0, 0.5, -0.25})*sum(u);
  y := y + identity(3)*u/(1.0*size(u, 1)) + zeros(3) + ones(3)*(u*u);
  y := y .+ (.-(0.5*y)) .+ (.+(0.25*y)) - (-(0.125*y));
end shaped;
",
    context: "",
    call: "shaped({x})",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

/// The shapes the retrodiction chain is built from, in one function.
///
/// A quaternion product, a rotation matrix assembled entry by entry, a skew
/// matrix built as an array literal, a small-angle series selected by a branch
/// on the angle, a matrix-matrix product, a transpose-multiply and a `cat`:
/// each of those is a step some estimator function takes, and the battery
/// carries them together so a change that breaks the chain fails here.
const RETRODICT: Case = Case {
    name: "Retrodict",
    functions: r"
function quat_product
  input Real a[4];
  input Real b[4];
  output Real c[4];
algorithm
  c[1] := a[1]*b[1] - a[2]*b[2] - a[3]*b[3] - a[4]*b[4];
  c[2] := a[1]*b[2] + a[2]*b[1] + a[3]*b[4] - a[4]*b[3];
  c[3] := a[1]*b[3] - a[2]*b[4] + a[3]*b[1] + a[4]*b[2];
  c[4] := a[1]*b[4] + a[2]*b[3] - a[3]*b[2] + a[4]*b[1];
end quat_product;

function quat_to_dcm
  input Real q[4];
  output Real R[3, 3];
protected
  Real a;
  Real b;
  Real c;
  Real d;
algorithm
  a := q[1];
  b := q[2];
  c := q[3];
  d := q[4];
  R[1, 1] := a*a + b*b - c*c - d*d;
  R[1, 2] := 2*(b*c - a*d);
  R[1, 3] := 2*(b*d + a*c);
  R[2, 1] := 2*(b*c + a*d);
  R[2, 2] := a*a - b*b + c*c - d*d;
  R[2, 3] := 2*(c*d - a*b);
  R[3, 1] := 2*(b*d - a*c);
  R[3, 2] := 2*(c*d + a*b);
  R[3, 3] := a*a - b*b - c*c + d*d;
end quat_to_dcm;

function wedge
  input Real w[3];
  output Real S[3, 3];
algorithm
  S := {{0.0, -w[3], w[2]}, {w[3], 0.0, -w[1]}, {-w[2], w[1], 0.0}};
end wedge;

function retrodict
  input Real u[6];
  input Real q0[4];
  output Real y[6];
protected
  Real omega[3];
  Real theta_sq;
  Real theta;
  Real c1;
  Real c2;
  Real om[3, 3];
  Real om2[3, 3];
  Real q[4];
  Real dcm[3, 3];
  Real w[3];
algorithm
  omega := {u[1], u[2], u[3]};
  theta_sq := omega[1]^2 + omega[2]^2 + omega[3]^2;
  if theta_sq < 0.01 then
    c1 := 0.5 - theta_sq/24.0;
    c2 := 1.0/6.0 - theta_sq/120.0;
  else
    theta := sqrt(theta_sq);
    c1 := (1.0 - cos(theta))/theta_sq;
    c2 := (theta - sin(theta))/(theta_sq*theta);
  end if;
  om := wedge(omega);
  om2 := om*om;
  q := quat_product(q0, {1.0, 0.5*u[1], 0.5*u[2], 0.5*u[3]});
  dcm := quat_to_dcm(q);
  w := {u[4], u[5], u[6]};
  y := cat(1, transpose(dcm)*w + c1*(om*w) + c2*(om2*w), dcm*w);
end retrodict;
",
    context: "  parameter Real q0[4] = {0.9, 0.2, -0.3, 0.25};\n",
    call: "retrodict({x}, q0)",
    rows: 6,
    width: 6,
    scalar_argument: false,
    scalar_result: false,
};

/// Declaration bindings, on a local, on the output, and on a `constant`.
///
/// A binding is an assignment Modelica performs before the algorithm runs. A
/// tangent function that redeclared these without their bindings would compute
/// a different primal, and one that minted their companions without assigning
/// them would read whatever memory holds: the gate's own `size > 1.0e-6`
/// control catches the all-zero case, and the comparison catches the rest.
/// `constant Real eps = 1e-8` guarding a small-angle branch is the in-repo
/// estimator idiom this row exists for.
const BOUND_DECLARATIONS: Case = Case {
    name: "BoundDeclarations",
    functions: r"
function bound
  input Real u[3];
  output Real y[3] = {5.0, 1.0, 2.0};
protected
  constant Real eps = 4.0;
  parameter Real gain = 2.5;
  Real t = 2.0*u[1];
  Real s = t*u[2] + eps;
algorithm
  y[1] := y[1]*t*u[1] + gain*s;
  y[2] := y[2]*s*u[2] + eps*t;
  y[3] := y[3]*u[3] + t + s;
end bound;
",
    context: "",
    call: "bound({x})",
    rows: 3,
    width: 3,
    scalar_argument: false,
    scalar_result: false,
};

const CASES: &[&Case] = &[
    &BOUND_DECLARATIONS,
    &ARITHMETIC,
    &ELEMENTARY,
    &FOR_LOOP,
    &BRANCH,
    &COMPOSED,
    &MATRIX,
    &CONCATENATED,
    &ELEMENTWISE_POWER,
    &ELEMENTWISE_OPS,
    &VECTORIZED,
    &SHAPED,
    &RETRODICT,
];

/// Deterministic point generator: a fixed-seed xorshift, so a failing case is
/// reproducible from its name and scale alone.
pub(crate) struct Points(u64);

impl Points {
    pub(crate) fn new(seed: u64) -> Self {
        Self(seed | 1)
    }

    /// A coordinate in `[-1, -0.25] ∪ [0.25, 1]`.
    ///
    /// The magnitude floor keeps every point away from the kinks the battery
    /// contains (`abs`, and the branch conditions at zero), where a central
    /// difference straddles two branches and answers a question about neither.
    pub(crate) fn next(&mut self) -> f64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        let unit = ((self.0 >> 11) as f64) / ((1u64 << 53) as f64);
        let sign = if self.0 & 1 == 0 { 1.0 } else { -1.0 };
        sign * (0.25 + 0.75 * unit)
    }
}

/// The probe model for one case, point and step.
fn probe_source(case: &Case, point: &[f64], step: f64) -> String {
    let argument = if case.scalar_argument {
        format!("  parameter Real x = {:.12};\n", point[0])
    } else {
        let entries: Vec<String> = point.iter().map(|value| format!("{value:.12}")).collect();
        format!(
            "  parameter Real x[{}] = {{{}}};\n",
            case.width,
            entries.join(", ")
        )
    };
    let mut equations = String::new();
    for column in 1..=case.width {
        let plus = perturbed(case, column, true);
        let minus = perturbed(case, column, false);
        let target = if case.scalar_result {
            format!("Jfd[1, {column}]")
        } else {
            format!("Jfd[:, {column}]")
        };
        equations.push_str(&format!(
            "  {target} = ({} - {})/(2*h);\n",
            case.call.replace("{x}", &plus),
            case.call.replace("{x}", &minus)
        ));
    }
    format!(
        "{}\nmodel {}\n{}{}  parameter Real h = {step:.12};\n  Real J[{}, {}] = jacobian({}, x);\n  \
         Real Jfd[{}, {}];\n  Real gap;\n  Real size_of_jacobian;\n  Real clock(start = 0, fixed = true);\n\
         equation\n  der(clock) = 0;\n{}  gap = max(abs(J - Jfd));\n  \
         size_of_jacobian = max(abs(J));\nend {};\n",
        case.functions,
        case.name,
        case.context,
        argument,
        case.rows,
        case.width,
        case.call.replace("{x}", "x"),
        case.rows,
        case.width,
        equations,
        case.name,
    )
}

/// The differentiated argument, perturbed forward or backward in one coordinate.
fn perturbed(case: &Case, column: usize, forward: bool) -> String {
    let sign = if forward { "+" } else { "-" };
    if case.scalar_argument {
        return format!("(x {sign} h)");
    }
    let entries: Vec<String> = (1..=case.width)
        .map(|index| {
            if index == column {
                "h".to_string()
            } else {
                "0.0".to_string()
            }
        })
        .collect();
    format!("(x {sign} {{{}}})", entries.join(", "))
}

fn first_value(result: &SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("probe result is missing column {name}"));
    *result.data[index]
        .first()
        .unwrap_or_else(|| panic!("column {name} has no samples"))
}

/// Compile and run one probe, returning `(gap, size_of_jacobian)`.
fn run_probe(case: &Case, source: &str) -> (f64, f64) {
    let compiled = Compiler::new()
        .model(case.name)
        .compile_str(source, &format!("{}.mo", case.name))
        .unwrap_or_else(|error| panic!("{} probe must compile: {error:#}\n{source}", case.name));
    let result = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 0.01,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{} probe must simulate: {error:#}", case.name));
    (
        first_value(&result, "gap"),
        first_value(&result, "size_of_jacobian"),
    )
}

#[test]
fn synthesized_jacobians_match_central_differences() {
    // Three scales: a small point where curvature is mild, a unit point, and a
    // large point where the step has to grow with the value it perturbs.
    let scales = [0.05_f64, 1.0, 3.0];
    let mut checked = 0usize;
    for case in CASES {
        for (index, scale) in scales.iter().enumerate() {
            let mut points = Points::new(0x5eed_0000 + index as u64 * 7 + case.name.len() as u64);
            let point: Vec<f64> = (0..case.width).map(|_| scale * points.next()).collect();
            let step = 1.0e-5 * scale.max(1.0);
            let source = probe_source(case, &point, step);
            let (gap, size) = run_probe(case, &source);

            let tolerance = 2.0e-5 * size.max(1.0);
            assert!(
                gap <= tolerance,
                "{} at scale {scale}: synthesized Jacobian and central differences differ by \
                 {gap:e} (tolerance {tolerance:e}, |J|max {size:e}) at point {point:?}\n{source}",
                case.name
            );
            assert!(
                size > 1.0e-6,
                "{} at scale {scale}: the synthesized Jacobian is all zeros, so the comparison \
                 proves nothing",
                case.name
            );
            checked += 1;
        }
    }
    assert_eq!(
        checked,
        CASES.len() * scales.len(),
        "every case must be checked at every scale"
    );
}

/// The battery itself, exported so the OpenModelica elaboration row
/// differentiates exactly the sources this gate compares.
pub(crate) fn battery_sources() -> Vec<(&'static str, String)> {
    CASES
        .iter()
        .map(|case| {
            let point: Vec<f64> = (0..case.width)
                .map(|index| 0.3 + 0.1 * index as f64)
                .collect();
            (case.name, probe_source(case, &point, 1.0e-5))
        })
        .collect()
}
