//! The rows of the admission table.
//!
//! A family whose whole surface takes one verdict is stated as that verdict
//! over its shapes, and only the constructs that differ from their family
//! carry a row of their own. Reading this file top to bottom is reading
//! the Jacobian synthesis rules and the refusal set.

use super::Shape::{Matrix, Scalar, Vector};
use super::{
    Carrier, FILL_MATRIX_OUT, FILL_SCALAR_OUT, FILL_VECTOR_OUT, Family, MATRIX_OUT, MATRIX63_OUT,
    PAIR_OUT, Row, SCALAR_OUT, Shape, VECTOR_OUT, VECTOR6_OUT, Verdict,
};

/// The rule id every expression-form refusal cites.
const EXPRESSION_FORM: &str = "JAC-R5";

/// The rule id every declaration-binding refusal cites.
const DECLARATION_BINDING: &str = "JAC-R9";

/// The rule id every statement-form refusal cites.
const STATEMENT_FORM: &str = "JAC-R4";

/// The rule id every call-boundary shape refusal cites.
const ACTUAL_SHAPE: &str = "JAC-R10";

/// `abs` states one condition, so its rule is the rule of a rank-0 argument.
const BRANCHING: Verdict = Verdict::Refused {
    rule: EXPRESSION_FORM,
    says: "abs",
};

/// The power rules write `exponent - 1`, which needs a rank-0 exponent.
const EXPONENT: Verdict = Verdict::Refused {
    rule: EXPRESSION_FORM,
    says: "exponent",
};

/// `^` states the scalar chain rule, so its base must be rank 0.
const SCALAR_BASE: Verdict = Verdict::Refused {
    rule: EXPRESSION_FORM,
    says: "scalar",
};

/// Differentiated, and checked here.
const HERE: Verdict = Verdict::Differentiated;

/// Differentiated, and checked in OpenModelica because this compiler's
/// canonical-DAE construction carries no primal for the pair to run.
const ELSEWHERE: Verdict = Verdict::DifferentiatedInOpenModelica;

/// The elementary functions of one argument, which map over any shape.
const ELEMENTARY: &[&str] = &[
    "sin", "cos", "tan", "exp", "log", "log10", "sqrt", "asin", "acos", "atan", "sinh", "cosh",
    "tanh",
];

/// The carrier a result of the same shape as its operand leaves through.
fn carrier_of(shape: Shape) -> Carrier {
    match shape {
        Scalar => SCALAR_OUT,
        Vector => VECTOR_OUT,
        Matrix => MATRIX_OUT,
    }
}

/// One row of the table.
fn row(
    family: Family,
    construct: &'static str,
    operands: &[Shape],
    form: &str,
    carrier: Carrier,
    verdict: Verdict,
) -> Row {
    Row {
        family,
        construct,
        operands: operands.to_vec(),
        form: form.to_string(),
        carrier,
        body: None,
        verdict,
    }
}

/// The whole admission table.
pub fn table() -> Vec<Row> {
    let mut rows = Vec::new();
    rows.extend(elementary());
    rows.extend(shaping());
    rows.extend(constant());
    rows.extend(non_differentiable());
    rows.extend(arithmetic());
    rows.extend(power());
    rows.extend(unary());
    rows.extend(declaration());
    rows.extend(composite());
    rows.extend(calls());
    rows.extend(statements());
    rows
}

/// The elementary functions map over any shape (MLS 10.6.4), so their rules
/// hold at every shape. `abs` states one condition and holds at one shape, and
/// `atan2` pairs its two operands the way an elementwise operator does.
fn elementary() -> Vec<Row> {
    let mut rows = Vec::new();
    for name in ELEMENTARY {
        for shape in Shape::ALL {
            rows.push(row(
                Family::Elementary,
                name,
                &[*shape],
                &format!("{name}({{a}})"),
                carrier_of(*shape),
                Verdict::Differentiated,
            ));
        }
    }
    rows.push(row(
        Family::Elementary,
        "abs",
        &[Scalar],
        "abs({a})",
        SCALAR_OUT,
        Verdict::Differentiated,
    ));
    for shape in [Vector, Matrix] {
        rows.push(row(
            Family::Elementary,
            "abs",
            &[shape],
            "abs({a})",
            carrier_of(shape),
            BRANCHING,
        ));
    }
    for (left, right, carrier) in PAIRED {
        rows.push(row(
            Family::Elementary,
            "atan2",
            &[*left, *right],
            "atan2({a}, {b})",
            *carrier,
            checked_where(*left, *right),
        ));
    }
    let stated = rows.clone();
    rows.extend(unstated_shape_rest(
        Family::Elementary,
        &[("atan2", 2)],
        "atan2({a}, {b})",
        &stated,
    ));
    rows
}

/// Where a pairing construct's finite-difference row is taken.
///
/// This compiler's canonical DAE carries a pairing construct only at equal
/// ranks, so the primal of a pair that spreads a rank-0 operand over an array
/// does not run here; the exported expansion runs in OpenModelica, and that is
/// where its row is taken.
fn checked_where(left: Shape, right: Shape) -> Verdict {
    if left == right { HERE } else { ELSEWHERE }
}

/// The shapes an operator that pairs entries admits, with the shape it
/// produces: a rank-0 operand spreads over the other, and equal ranks pair
/// entry by entry (MLS 10.6.5). A vector and a matrix pair at neither.
const PAIRED: &[(Shape, Shape, Carrier)] = &[
    (Scalar, Scalar, SCALAR_OUT),
    (Scalar, Vector, VECTOR_OUT),
    (Vector, Scalar, VECTOR_OUT),
    (Scalar, Matrix, MATRIX_OUT),
    (Matrix, Scalar, MATRIX_OUT),
    (Vector, Vector, VECTOR_OUT),
    (Matrix, Matrix, MATRIX_OUT),
];

/// The builtins that read or rearrange a shape, each admitted at the shapes
/// Modelica defines it at and nowhere else.
fn shaping() -> Vec<Row> {
    let admitted: &[(&'static str, &[Shape], &str, Carrier)] = &[
        ("transpose", &[Matrix], "transpose({a})", MATRIX_OUT),
        ("sum", &[Vector], "sum({a})", SCALAR_OUT),
        ("sum", &[Matrix], "sum({a})", SCALAR_OUT),
        ("diagonal", &[Vector], "diagonal({a})", MATRIX_OUT),
        ("cross", &[Vector, Vector], "cross({a}, {b})", VECTOR_OUT),
        (
            "outerProduct",
            &[Vector, Vector],
            "outerProduct({a}, {b})",
            MATRIX_OUT,
        ),
        ("cat", &[Vector, Vector], "cat(1, {a}, {b})", VECTOR6_OUT),
        ("cat", &[Matrix, Matrix], "cat(1, {a}, {b})", MATRIX63_OUT),
        ("fill", &[Scalar], "fill({a}, 2, 2)", FILL_SCALAR_OUT),
    ];
    let mut rows: Vec<Row> = admitted
        .iter()
        .map(|(construct, operands, form, carrier)| {
            row(Family::Shaping, construct, operands, form, *carrier, HERE)
        })
        .collect();
    // `fill` given an array builds a rank the canonical DAE does not carry, so
    // those two rows are taken in OpenModelica.
    rows.push(row(
        Family::Shaping,
        "fill",
        &[Vector],
        "fill({a}, 2)",
        FILL_VECTOR_OUT,
        ELSEWHERE,
    ));
    rows.push(row(
        Family::Shaping,
        "fill",
        &[Matrix],
        "fill({a}, 2)",
        FILL_MATRIX_OUT,
        ELSEWHERE,
    ));
    let stated = rows.clone();
    rows.extend(unstated_shape_rest(
        Family::Shaping,
        &[("transpose", 1), ("sum", 1), ("diagonal", 1), ("fill", 1)],
        "{f}({a})",
        &stated,
    ));
    rows.extend(unstated_shape_rest(
        Family::Shaping,
        &[("cross", 2), ("outerProduct", 2)],
        "{f}({a}, {b})",
        &stated,
    ));
    rows.extend(unstated_shape_rest(
        Family::Shaping,
        &[("cat", 2)],
        "cat(1, {a}, {b})",
        &stated,
    ));
    rows
}

/// The builtins whose tangent is the structural zero.
///
/// Each row combines the call with a moving value of the row's own shape, so
/// the claim under test is that the call contributes nothing while the
/// derivative around it stays right.
fn constant() -> Vec<Row> {
    let admitted: &[(&'static str, Shape, &str, Carrier, Verdict)] = &[
        ("zeros", Scalar, "({a}) .+ zeros(3)", VECTOR_OUT, ELSEWHERE),
        ("zeros", Vector, "({a}) .+ zeros(3)", VECTOR_OUT, HERE),
        ("zeros", Matrix, "({a}) .+ zeros(3, 3)", MATRIX_OUT, HERE),
        ("ones", Scalar, "({a}) .* ones(3)", VECTOR_OUT, HERE),
        ("ones", Vector, "({a}) .* ones(3)", VECTOR_OUT, HERE),
        ("ones", Matrix, "({a}) .* ones(3, 3)", MATRIX_OUT, HERE),
        (
            "identity",
            Scalar,
            "({a}) .+ identity(3)",
            MATRIX_OUT,
            ELSEWHERE,
        ),
        ("identity", Matrix, "({a}) .+ identity(3)", MATRIX_OUT, HERE),
        (
            "size",
            Vector,
            "({a}) .* (1.0*size({a}, 1))",
            VECTOR_OUT,
            HERE,
        ),
        (
            "size",
            Matrix,
            "({a}) .* (1.0*size({a}, 1))",
            MATRIX_OUT,
            HERE,
        ),
    ];
    let mut rows: Vec<Row> = admitted
        .iter()
        .map(|(construct, shape, form, carrier, verdict)| {
            row(
                Family::Constant,
                construct,
                &[*shape],
                form,
                *carrier,
                *verdict,
            )
        })
        .collect();
    let stated = rows.clone();
    rows.extend(untypable_rest(
        Family::Constant,
        &[("identity", 1)],
        "({a}) .+ identity(3)",
        &stated,
    ));
    rows.extend(untypable_rest(
        Family::Constant,
        &[("size", 1)],
        "({a}) .* (1.0*size({a}, 1))",
        &stated,
    ));
    rows
}

/// The builtins stated non-differentiable refuse before any shape is read, so
/// they refuse at every shape.
fn non_differentiable() -> Vec<Row> {
    let mut rows = Vec::new();
    for name in crate::builtins::REFUSED {
        for shape in Shape::ALL {
            rows.push(row(
                Family::NonDifferentiable,
                name,
                &[*shape],
                &format!("{name}({{a}})"),
                carrier_of(*shape),
                Verdict::Refused {
                    rule: EXPRESSION_FORM,
                    says: name,
                },
            ));
        }
    }
    rows
}

/// `+ - * /` and their elementwise forms.
///
/// The plain additive operators pair equal shapes (MLS 10.6.3); `*` is also
/// the matrix product and the scalar product; `/` takes a scalar divisor
/// (MLS 10.6.7); the elementwise forms pair the shapes [`PAIRED`] lists.
fn arithmetic() -> Vec<Row> {
    let mut rows = Vec::new();
    for operator in ["+", "-"] {
        for shape in Shape::ALL {
            rows.push(binary(operator, *shape, *shape, carrier_of(*shape)));
        }
    }
    for (left, right, carrier) in PRODUCT {
        rows.push(binary("*", *left, *right, *carrier));
    }
    for shape in Shape::ALL {
        rows.push(binary("/", *shape, Scalar, carrier_of(*shape)));
    }
    // `.*` and `./` spread a rank-0 operand here; `.+` and `.-` do not, so
    // their spreading rows are checked in OpenModelica.
    for operator in [".*", "./"] {
        for (left, right, carrier) in PAIRED {
            rows.push(binary(operator, *left, *right, *carrier));
        }
    }
    for operator in [".+", ".-"] {
        for (left, right, carrier) in PAIRED {
            rows.push(row(
                Family::Arithmetic,
                operator,
                &[*left, *right],
                &format!("({{a}}) {operator} ({{b}})"),
                *carrier,
                checked_where(*left, *right),
            ));
        }
    }
    let stated = rows.clone();
    let constructs: Vec<(&'static str, usize)> = ["+", "-", "*", "/", ".+", ".-", ".*", "./"]
        .into_iter()
        .map(|operator| (operator, 2))
        .collect();
    rows.extend(untypable_rest(
        Family::Arithmetic,
        &constructs,
        "({a}) {f} ({b})",
        &stated,
    ));
    rows
}

/// The shapes Modelica's `*` is defined at, with the shape it produces
/// (MLS 10.6.3): two vectors give the scalar product, and a vector against a
/// matrix gives a vector.
const PRODUCT: &[(Shape, Shape, Carrier)] = &[
    (Scalar, Scalar, SCALAR_OUT),
    (Scalar, Vector, VECTOR_OUT),
    (Vector, Scalar, VECTOR_OUT),
    (Scalar, Matrix, MATRIX_OUT),
    (Matrix, Scalar, MATRIX_OUT),
    (Vector, Vector, SCALAR_OUT),
    (Vector, Matrix, VECTOR_OUT),
    (Matrix, Vector, VECTOR_OUT),
    (Matrix, Matrix, MATRIX_OUT),
];

/// One admitted binary-operator row.
fn binary(operator: &'static str, left: Shape, right: Shape, carrier: Carrier) -> Row {
    row(
        Family::Arithmetic,
        operator,
        &[left, right],
        &format!("({{a}}) {operator} ({{b}})"),
        carrier,
        Verdict::Differentiated,
    )
}

/// `^` and `.^`, whose exponent is a held-still value.
///
/// `^` states the scalar chain rule, so it admits a rank-0 base only; `.^`
/// states it at every entry, so it admits any base. Neither can write
/// `exponent - 1` at a shape Modelica has no elementwise difference for, so
/// neither admits a non-scalar exponent. A moving exponent has its own row.
fn power() -> Vec<Row> {
    let mut rows = Vec::new();
    for (operator, admitted_bases) in [("^", &[Scalar][..]), (".^", Shape::ALL)] {
        for (base, exponent) in shape_pairs() {
            rows.push(row(
                Family::Power,
                operator,
                &[base, exponent],
                &format!("({{a}}) {operator} ({{c}})"),
                carrier_of(base),
                power_verdict(admitted_bases, base, exponent),
            ));
        }
    }
    for (construct, form) in [
        (
            "^ with a moving exponent",
            "(0.5 + 0.2*u[1]) ^ (0.8 + 0.15*u[2])",
        ),
        (
            ".^ with a moving exponent",
            "(0.5 + 0.2*u[1]) .^ (0.8 + 0.15*u[2])",
        ),
    ] {
        rows.push(row(
            Family::Power,
            construct,
            &[],
            form,
            SCALAR_OUT,
            Verdict::Refused {
                rule: EXPRESSION_FORM,
                says: "carries a tangent",
            },
        ));
    }
    rows
}

/// Every ordered pair of shapes, as a base and an exponent.
fn shape_pairs() -> Vec<(Shape, Shape)> {
    Shape::ALL
        .iter()
        .flat_map(|base| Shape::ALL.iter().map(move |exponent| (*base, *exponent)))
        .collect()
}

/// What a power rule does with one base and one exponent shape.
fn power_verdict(admitted_bases: &[Shape], base: Shape, exponent: Shape) -> Verdict {
    if exponent != Scalar {
        return EXPONENT;
    }
    if admitted_bases.contains(&base) {
        return HERE;
    }
    SCALAR_BASE
}

/// The unary operators keep their operand's shape at every shape.
fn unary() -> Vec<Row> {
    let mut rows = Vec::new();
    for (_, operator) in crate::engine::UNARY_OPERATORS {
        for shape in Shape::ALL {
            rows.push(row(
                Family::Unary,
                operator,
                &[*shape],
                &format!("{operator}({{a}})"),
                carrier_of(*shape),
                Verdict::Differentiated,
            ));
        }
    }
    rows
}

/// Declaration bindings: admitted with their tangent, and refused where that
/// tangent has no position or no companion to land in.
fn declaration() -> Vec<Row> {
    let stated: &[(&'static str, &'static str, Verdict)] = &[
        ("a bound local", BOUND_LOCAL, Verdict::Differentiated),
        ("a bound output", BOUND_OUTPUT, Verdict::Differentiated),
        ("a constant local", CONSTANT_LOCAL, Verdict::Differentiated),
        (
            "a parameter local",
            PARAMETER_LOCAL,
            Verdict::Differentiated,
        ),
        (
            "a binding reading a later declaration",
            FORWARD_BINDING,
            Verdict::Refused {
                rule: DECLARATION_BINDING,
                says: "declares no earlier",
            },
        ),
        (
            "a binding that moves under a variability that holds still",
            MOVING_CONSTANT_BINDING,
            Verdict::Refused {
                rule: DECLARATION_BINDING,
                says: "hold still",
            },
        ),
    ];
    stated
        .iter()
        .map(|(construct, body, verdict)| Row {
            family: Family::Declaration,
            construct,
            operands: Vec::new(),
            form: String::new(),
            carrier: VECTOR_OUT,
            body: Some(body),
            verdict: *verdict,
        })
        .collect()
}

const BOUND_LOCAL: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  Real t = 2.0*u[1];
  Real s = t*u[2];
algorithm
  y[1] := t*u[2];
  y[2] := s*u[3];
  y[3] := t + s;
end {name};";

const BOUND_OUTPUT: &str = "\
function {name}
  input Real u[3];
  output Real y[3] = {5.0, 1.0, 2.0};
algorithm
  y[1] := y[1]*u[1];
  y[2] := y[2]*u[2] + y[3];
  y[3] := y[3]*u[3];
end {name};";

const CONSTANT_LOCAL: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  constant Real eps = 4.0;
algorithm
  y[1] := eps*u[1];
  y[2] := u[2]*u[2] + eps;
  y[3] := u[3]/eps;
end {name};";

const PARAMETER_LOCAL: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  parameter Real gain = 2.5;
algorithm
  y[1] := gain*u[1];
  y[2] := gain*u[2]*u[2];
  y[3] := u[3]/gain;
end {name};";

const FORWARD_BINDING: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  Real early = late*u[1];
  Real late = 2.0*u[2];
algorithm
  y[1] := early;
  y[2] := late;
  y[3] := u[3];
end {name};";

const MOVING_CONSTANT_BINDING: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  constant Real held = u[1];
algorithm
  y[1] := held*u[1];
  y[2] := u[2];
  y[3] := u[3];
end {name};";

/// The expression forms that build a value out of other expressions.
///
/// An array literal stacks its elements under a new first dimension and an
/// `if` expression keeps the shape of every branch, so both are stated at
/// every shape their elements can have. Neither reaches the engine through a
/// name, which is why both are enumerated in `admission::SHAPED_FORMS`.
fn composite() -> Vec<Row> {
    let mut rows = Vec::new();
    for (shape, carrier) in [
        (Scalar, PAIR_OUT),
        (Vector, FILL_VECTOR_OUT),
        (Matrix, FILL_MATRIX_OUT),
    ] {
        rows.push(row(
            Family::Composite,
            "an array literal",
            &[shape],
            "{{a}, {b}}",
            carrier,
            HERE,
        ));
    }
    for shape in Shape::ALL {
        rows.push(row(
            Family::Composite,
            "an if expression",
            &[*shape],
            "(if u[1] > 0.0 then {a} else {b})",
            carrier_of(*shape),
            HERE,
        ));
    }
    rows
}

/// Calls to a function declared in the file.
///
/// The admitted rows differentiate a call at each shape its argument can have,
/// which is the surface `engine.rs::user_call_derivative` states a rule over.
/// The refused row is the call the rule has no shape for: an actual wider than
/// its formal is not one call but one call per element (MLS 12.4.6), so its
/// result is not the shape JAC-S3 has the wrapper state.
///
/// Every admitted row here is checked in OpenModelica. JAC-G1 makes a tangent
/// body call both the callee's tangent and the callee itself, and a function
/// that calls two others, is called from a third and returns an array is a
/// program this compiler's Solve IR declines (`EL005`, "pure-call owner was
/// not issued by this table"). The primal of each probe runs here; its
/// expansion does not, so there is no in-process program to compare against
/// and the gate takes the row where one exists.
fn calls() -> Vec<Row> {
    let admitted: &[(Shape, &'static str, Carrier)] = &[
        (Scalar, CALL_SCALAR, SCALAR_OUT),
        (Vector, CALL_VECTOR, VECTOR_OUT),
        (Matrix, CALL_MATRIX, MATRIX_OUT),
    ];
    let mut rows: Vec<Row> = admitted
        .iter()
        .map(|(shape, body, carrier)| Row {
            family: Family::Call,
            construct: "a call to a function in scope",
            operands: vec![*shape],
            form: String::new(),
            carrier: *carrier,
            body: Some(body),
            verdict: ELSEWHERE,
        })
        .collect();
    rows.push(Row {
        family: Family::Call,
        construct: "a vectorized call to a function in scope",
        operands: Vec::new(),
        form: String::new(),
        carrier: SCALAR_OUT,
        body: Some(VECTORIZED_CALL),
        verdict: Verdict::Refused {
            rule: ACTUAL_SHAPE,
            says: "vectorizes",
        },
    });
    rows
}

/// The statement forms, admitted and refused.
///
/// Each row is a whole function body, because a statement form is not written
/// over operands the way an operator is. The admitted four are the forms
/// section 4 lists; the refused two are the ones a differentiated body has no
/// tangent for, and they are stated here so the statement surface is closed in
/// the same table rather than only in the named battery.
fn statements() -> Vec<Row> {
    let stated: &[(&'static str, &'static str, Verdict)] = &[
        ("an assignment statement", ASSIGNMENT_BODY, HERE),
        ("a for statement", FOR_BODY, HERE),
        ("an if statement", IF_BODY, HERE),
        ("an assert statement", ASSERT_BODY, HERE),
        (
            "a while statement",
            WHILE_BODY,
            Verdict::Refused {
                rule: STATEMENT_FORM,
                says: "while loop",
            },
        ),
        (
            "a call statement",
            CALL_STATEMENT_BODY,
            Verdict::Refused {
                rule: STATEMENT_FORM,
                says: "call statement",
            },
        ),
    ];
    stated
        .iter()
        .map(|(construct, body, verdict)| Row {
            family: Family::Statement,
            construct,
            operands: Vec::new(),
            form: String::new(),
            carrier: VECTOR_OUT,
            body: Some(body),
            verdict: *verdict,
        })
        .collect()
}

const CALL_SCALAR: &str = "\
function {name}_inner
  input Real a;
  output Real b;
algorithm
  b := a*a + sin(a);
end {name}_inner;

function {name}
  input Real u[3];
  output Real y[1];
protected
  Real r;
algorithm
  r := {name}_inner(0.5 + 0.2*u[1]);
  y[1] := r;
end {name};";

const CALL_VECTOR: &str = "\
function {name}_inner
  input Real a[3];
  output Real b[3];
algorithm
  b := a .* a + sin(a);
end {name}_inner;

function {name}
  input Real u[3];
  output Real y[3];
protected
  Real r[3];
algorithm
  r := {name}_inner({0.5 + 0.2*u[1], 0.6 + 0.2*u[2], 0.7 + 0.2*u[3]});
  y[1] := r[1];
  y[2] := r[2];
  y[3] := r[3];
end {name};";

const CALL_MATRIX: &str = "\
function {name}_inner
  input Real a[3, 3];
  output Real b[3, 3];
algorithm
  b := a .* a + sin(a);
end {name}_inner;

function {name}
  input Real u[3];
  output Real y[9];
protected
  Real r[3, 3];
algorithm
  r := {name}_inner({{0.5 + 0.2*u[1], 0.6 + 0.2*u[2], 0.7 + 0.2*u[3]}, \
{0.4 + 0.2*u[2], 0.65 + 0.2*u[3], 0.35 + 0.2*u[1]}, \
{0.55 + 0.2*u[3], 0.45 + 0.2*u[1], 0.6 + 0.2*u[2]}});
  y[1] := r[1, 1];
  y[2] := r[1, 2];
  y[3] := r[1, 3];
  y[4] := r[2, 1];
  y[5] := r[2, 2];
  y[6] := r[2, 3];
  y[7] := r[3, 1];
  y[8] := r[3, 2];
  y[9] := r[3, 3];
end {name};";

/// A scalar formal reached by a `Real[3]` actual: the probe model passes the
/// differentiated `x[3]`, so this call is one call per element.
const VECTORIZED_CALL: &str = "\
function {name}
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end {name};";

const ASSIGNMENT_BODY: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  Real t;
algorithm
  t := u[1]*u[2];
  y[1] := t;
  y[2] := t + u[3]*u[3];
  y[3] := u[1]/u[2];
end {name};";

const FOR_BODY: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  Real acc;
algorithm
  acc := 1.0;
  for i in 1:3 loop
    acc := acc*u[i];
    y[i] := acc + u[i]*u[i];
  end for;
end {name};";

const IF_BODY: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
algorithm
  if u[1] > 0.0 then
    y[1] := u[1]*u[2];
  else
    y[1] := u[1]*u[3];
  end if;
  y[2] := u[2]*u[2];
  y[3] := u[3]*u[3];
end {name};";

const ASSERT_BODY: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
algorithm
  assert(u[1] > -2.0, \"the probe point is inside the stated range\");
  y[1] := u[1]*u[2];
  y[2] := u[2]*u[3];
  y[3] := u[3]*u[1];
end {name};";

const WHILE_BODY: &str = "\
function {name}
  input Real u[3];
  output Real y[3];
protected
  Real step;
algorithm
  y := u;
  step := 0;
  while step < 3 loop
    y[1] := y[1]*u[1];
    step := step + 1;
  end while;
end {name};";

const CALL_STATEMENT_BODY: &str = "\
function {name}_used
  input Real a;
  output Real b;
algorithm
  b := a*a;
end {name}_used;

function {name}
  input Real u[3];
  output Real y[3];
algorithm
  {name}_used(u[1]);
  y := u;
end {name};";

/// Every shape tuple of the named rule-carrying builtins that `stated` does
/// not carry, as a row saying the call refuses because its result has no rank
/// this engine states (JAC-T1).
fn unstated_shape_rest(
    family: Family,
    constructs: &[(&'static str, usize)],
    form: &str,
    stated: &[Row],
) -> Vec<Row> {
    remaining_shapes(constructs, stated)
        .into_iter()
        .map(|(construct, operands)| Row {
            family,
            construct,
            carrier: carrier_of(*operands.first().unwrap_or(&Scalar)),
            operands,
            form: form.replace("{f}", construct),
            body: None,
            verdict: Verdict::Refused {
                rule: EXPRESSION_FORM,
                says: construct,
            },
        })
        .collect()
}

/// Every shape tuple of the named constructs that `stated` does not carry, as
/// a row saying that pair is not a Modelica program.
fn untypable_rest(
    family: Family,
    constructs: &[(&'static str, usize)],
    form: &str,
    stated: &[Row],
) -> Vec<Row> {
    remaining_shapes(constructs, stated)
        .into_iter()
        .map(|(construct, operands)| Row {
            family,
            construct,
            carrier: carrier_of(*operands.first().unwrap_or(&Scalar)),
            operands,
            form: form.replace("{f}", construct),
            body: None,
            verdict: Verdict::Untypable,
        })
        .collect()
}

/// The shape tuples of `constructs` that `stated` has no row for.
fn remaining_shapes(
    constructs: &[(&'static str, usize)],
    stated: &[Row],
) -> Vec<(&'static str, Vec<Shape>)> {
    let mut remaining = Vec::new();
    for (construct, arity) in constructs {
        for operands in super::shape_tuples(*arity) {
            if stated
                .iter()
                .any(|held| held.construct == *construct && held.operands == operands)
            {
                continue;
            }
            remaining.push((*construct, operands));
        }
    }
    remaining
}
