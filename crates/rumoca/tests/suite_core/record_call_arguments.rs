//! A record-valued call passed to a record parameter is one evaluation.
//!
//! Flatten decomposes a record parameter into one argument per leaf field and
//! gives every field argument its own copy of the record-valued expression.
//! Lowering must not turn each copy into a call occurrence of its own: every
//! occurrence is a full evaluation of the callee, so a record with `n` leaf
//! fields would cost `n` evaluations of the call that builds it, and the cost
//! multiplies through nested calls. The copies among one argument list, and the
//! discrete fields of one record equation, are one occurrence.

use std::collections::HashSet;

use rumoca::Compiler;
use rumoca_ir_dae as dae;

const SOURCE: &str = r#"
record Inner
  Real a[3];
  Real b[2];
  Integer k;
end Inner;

record Outer
  Inner first;
  Inner second;
  Boolean flag;
end Outer;

function MakeInner
  input Real x;
  output Inner r;
algorithm
  r.a := {x, 2*x, 3*x};
  r.b := {x, -x};
  r.k := 2;
end MakeInner;

function MakeOuter
  input Inner seed;
  output Outer o;
algorithm
  o.first := seed;
  o.second.a := 2*seed.a;
  o.second.b := seed.b;
  o.second.k := seed.k + 1;
  o.flag := seed.k > 1;
end MakeOuter;

function Total
  input Outer o;
  output Real y;
algorithm
  y := sum(o.first.a) + sum(o.second.a) + sum(o.second.b) + o.second.k
    + (if o.flag then 1 else 0);
end Total;

model Nested
  input Real x = 1;
  Outer packed;
  Real y;
equation
  packed = MakeOuter(MakeInner(x));
  y = Total(MakeOuter(MakeInner(x)));
end Nested;
"#;

fn call_occurrences(view: dae::DaeView<'_>, function: &str) -> usize {
    let mut owners = HashSet::new();
    let mut index = 0;
    while let Some(expression) = view.expression_id(index) {
        index += 1;
        let Some(node) = view.expression(expression) else {
            continue;
        };
        let dae::ExpressionOperation::Call {
            function: callee,
            owner,
            ..
        } = node.operation()
        else {
            continue;
        };
        if view
            .function(callee)
            .is_some_and(|definition| definition.name().as_str() == function)
        {
            owners.insert(format!("{owner:?}"));
        }
    }
    owners.len()
}

#[test]
fn a_record_valued_call_argument_is_one_call_occurrence() {
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(SOURCE, "RecordCallArguments.mo")
        .unwrap_or_else(|error| panic!("Nested compiles: {error:?}"));
    compiled.dae.inspect(|view| {
        // `y` and `packed` each evaluate the chain once; the many leaf fields
        // of `packed` and of the decomposed `seed` and `o` parameters add none.
        assert_eq!(call_occurrences(view, "MakeInner"), 2);
        assert_eq!(call_occurrences(view, "MakeOuter"), 2);
        assert_eq!(call_occurrences(view, "Total"), 1);
    });
}

#[test]
fn the_discrete_fields_of_one_record_call_are_one_program() {
    use rumoca_ir_solve::LinearOp;

    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(SOURCE, "RecordCallArguments.mo")
        .unwrap_or_else(|error| panic!("Nested compiles: {error:?}"));
    let package = rumoca_phase_solve::lower_solve_package(&compiled.dae).expect("Nested lowers");
    let discrete = &package.problem.discrete;
    // `packed.first.k`, `packed.second.k` and `packed.flag` are three Integer
    // and Boolean scalars, all projections of the one `MakeOuter` call: one
    // row program evaluates `MakeInner` and `MakeOuter` once between them.
    assert_eq!(
        discrete.rhs.programs().len(),
        1,
        "one program owns the rows"
    );
    let calls = discrete.rhs.programs()[0]
        .iter()
        .filter(|op| matches!(op, LinearOp::PureCall { .. }))
        .count();
    assert_eq!(calls, 2);
}

#[test]
fn a_call_projected_by_one_row_keeps_its_row_program() {
    const ONE_ROW: &str = r#"
record One
  Real a[3];
end One;

function MakeOne
  input Real x;
  output One r;
algorithm
  r.a := {x, 2*x, 3*x};
end MakeOne;

model Single
  input Real x = 1;
  One r;
equation
  r = MakeOne(x);
end Single;
"#;
    let compiled = Compiler::new()
        .model("Single")
        .compile_str(ONE_ROW, "RecordCallSingle.mo")
        .unwrap_or_else(|error| panic!("Single compiles: {error:?}"));
    rumoca_phase_solve::lower_solve_package(&compiled.dae).expect("Single lowers");
}
