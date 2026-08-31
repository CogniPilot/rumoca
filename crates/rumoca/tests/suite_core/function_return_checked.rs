use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, simulate_dae};

const GUARDED_RETURN: &str = r#"
function magnitude
  input Real x;
  output Real y;
algorithm
  if x > 0 then
    y := x;
    return;
  end if;
  y := -x;
end magnitude;

model GuardedReturn
  Real x(start = -0.5, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = magnitude(x);
end GuardedReturn;
"#;

#[test]
fn guarded_return_round_trips_and_remains_computable() {
    let compiled = Compiler::new()
        .model("GuardedReturn")
        .compile_str(GUARDED_RETURN, "guarded_return.mo")
        .expect("proved guarded return should construct checked DAE");
    let wire =
        serde_json::to_string(compiled.dae()).expect("guarded return should serialize through v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 reconstructs conditional result ownership");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("guarded return should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("function result y is visible");
    for (x, y) in simulation.data[x].iter().zip(&simulation.data[y]) {
        assert!((y - x.abs()).abs() <= 1.0e-8, "magnitude({x}) = {y}");
    }
}

#[test]
fn partial_return_definition_fails_at_the_function_owner() {
    let error = Compiler::new()
        .model("InvalidReturn")
        .compile_str(
            r#"
function partial_output
  input Real x;
  output Real y;
algorithm
  if x > 0 then
    return;
  end if;
  y := -x;
end partial_output;
model InvalidReturn
  Real y;
equation
  y = partial_output(1);
end InvalidReturn;
"#,
            "invalid_return.mo",
        )
        .expect_err("return without a total output definition must fail closed");
    let message = error.to_string();
    assert!(
        message.contains("function return") && message.contains("define every output"),
        "the return owner should explain its missing definition: {message}"
    );
}

/// A `return` normalization must not seed over a declared default.
///
/// MLS §12.4.4 makes a declaration binding the value the algorithm starts
/// from, so `output Real y = 7.0` is a value the non-returning path may simply
/// keep. The generated return-guard lowering seeds every output with a
/// proven-dead zero before the body runs; seeding an output whose default
/// already certifies it replaces the declared value with `0`, which is
/// observable and silent. `magnitude` above has no default and so cannot
/// detect it.
const DEFAULTED_RETURN: &str = r#"
function defaultedReturn
  input Real x;
  output Real y = 7.0;
algorithm
  if x > 0 then
    y := x;
    return;
  end if;
end defaultedReturn;

model DefaultedReturn
  Real x(start = -0.5, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = defaultedReturn(x);
end DefaultedReturn;
"#;

#[test]
fn a_declared_output_default_survives_the_return_guard_lowering() {
    let compiled = Compiler::new()
        .model("DefaultedReturn")
        .compile_str(DEFAULTED_RETURN, "defaulted_return.mo")
        .expect("a defaulted output with a guarded return should construct a checked DAE");
    let simulation = simulate_dae(compiled.dae(), &SimOptions::default())
        .expect("defaulted return should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("function result y is visible");
    let mut kept_default = 0usize;
    for (x, y) in simulation.data[x].iter().zip(&simulation.data[y]) {
        let expected = if *x > 0.0 { *x } else { 7.0 };
        if *x <= 0.0 {
            kept_default += 1;
        }
        assert!(
            (y - expected).abs() <= 1.0e-8,
            "defaultedReturn({x}) = {y}, expected {expected}"
        );
    }
    assert!(
        kept_default > 0,
        "the run must reach the non-returning path that keeps the default"
    );
}

/// The record form of the same rule: a record-typed output whose declaration
/// binding is a record constructor must not be zeroed field-by-field by the
/// return seed.
const DEFAULTED_RECORD_RETURN: &str = r#"
record Pair
  Real a;
  Real b;
end Pair;

function defaultedRecordReturn
  input Real x;
  output Pair p = Pair(10.0, 20.0);
algorithm
  if x > 0 then
    p := Pair(x, 2.0 * x);
    return;
  end if;
end defaultedRecordReturn;

function pairSum
  input Real x;
  output Real y;
protected
  Pair q;
algorithm
  q := defaultedRecordReturn(x);
  y := q.a + q.b;
end pairSum;

model DefaultedRecordReturn
  Real x(start = -0.5, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = pairSum(x);
end DefaultedRecordReturn;
"#;

#[test]
fn a_record_output_default_survives_the_return_guard_lowering() {
    let compiled = Compiler::new()
        .model("DefaultedRecordReturn")
        .compile_str(DEFAULTED_RECORD_RETURN, "defaulted_record_return.mo")
        .expect("a defaulted record output with a guarded return should construct a checked DAE");
    let simulation = simulate_dae(compiled.dae(), &SimOptions::default())
        .expect("defaulted record return should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("function result y is visible");
    let mut kept_default = 0usize;
    for (x, y) in simulation.data[x].iter().zip(&simulation.data[y]) {
        let expected = if *x > 0.0 { 3.0 * *x } else { 30.0 };
        if *x <= 0.0 {
            kept_default += 1;
        }
        assert!(
            (y - expected).abs() <= 1.0e-8,
            "pairSum({x}) = {y}, expected {expected}"
        );
    }
    assert!(
        kept_default > 0,
        "the run must reach the non-returning path that keeps the record default"
    );
}

/// One store into a function value, as the lowered body ordered them.
struct OutputStore<'dae> {
    rhs: dae::ExprId<'dae>,
    /// Which arm of a conditional group the store sits in (its fallback is
    /// `Some(branch_count)`); `None` for a store that is not inside one.
    branch: Option<usize>,
    /// Whether the store sits inside a generated `for` body.
    ///
    /// Tracked separately from `branch` because a loop body is *also* not
    /// unconditional: a seed emitted inside one runs zero or more times, so
    /// treating it as a plain leading statement would let a loop-nested seed
    /// satisfy the "the default is the body's first unconditional store"
    /// claim below.
    in_loop: bool,
}

impl OutputStore<'_> {
    /// A plain top-level body statement: not in a conditional arm, not in a
    /// loop. Only such a store can be the §12.4.4 declaration binding.
    fn is_unconditional(&self) -> bool {
        self.branch.is_none() && !self.in_loop
    }
}

fn append_assignment_group_stores<'dae>(
    definitions: dae::FunctionDefinitionValues<'dae>,
    conditional: Option<dae::FunctionConditionalView<'dae>>,
    target: dae::FunctionValueId<'dae>,
    in_loop: bool,
    stores: &mut Vec<OutputStore<'dae>>,
) {
    for (ordinal, definition) in definitions.iter().enumerate() {
        if definition.target() != target {
            continue;
        }
        let Some(conditional) = conditional else {
            stores.push(OutputStore {
                rhs: definition.rhs(),
                branch: None,
                in_loop,
            });
            continue;
        };
        for branch in 0..conditional.branch_count() {
            let values = conditional
                .branch(branch)
                .expect("a checked conditional group resolves every branch");
            stores.extend(values.into_iter().nth(ordinal).map(|rhs| OutputStore {
                rhs,
                branch: Some(branch),
                in_loop,
            }));
        }
        stores.extend(conditional.fallback().nth(ordinal).map(|rhs| OutputStore {
            rhs,
            branch: Some(conditional.branch_count()),
            in_loop,
        }));
    }
}

/// Every store into `target`, in lowered-body order, including the arms of a
/// conditional group and the bodies of generated loops.
///
/// Written against `FunctionStatementView` exhaustively rather than against the
/// seeding code, so a seed emitted through a *different* statement form than
/// today's is still collected instead of silently skipped.
fn output_stores<'dae>(
    statements: dae::FunctionStatements<'dae>,
    target: dae::FunctionValueId<'dae>,
    in_loop: bool,
    stores: &mut Vec<OutputStore<'dae>>,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { definition } => {
                if definition.target() == target {
                    stores.push(OutputStore {
                        rhs: definition.rhs(),
                        branch: None,
                        in_loop,
                    });
                }
            }
            dae::FunctionStatementView::AssignmentGroup {
                definitions,
                conditional,
            } => append_assignment_group_stores(definitions, conditional, target, in_loop, stores),
            dae::FunctionStatementView::For {
                statements: body, ..
            } => output_stores(body, target, true, stores),
            dae::FunctionStatementView::Assertion { .. } => {}
        }
    }
}

/// The `Real` constant a store writes, when it writes one.
fn stored_real<'dae>(view: dae::DaeView<'dae>, rhs: dae::ExprId<'dae>) -> Option<f64> {
    match view
        .expression(rhs)
        .expect("a checked definition resolves its right-hand side")
        .operation()
    {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => Some(*value),
        _ => None,
    }
}

/// A readable rendering of the stores into one output, for failure messages.
fn describe_stores<'dae>(view: dae::DaeView<'dae>, stores: &[OutputStore<'dae>]) -> String {
    stores
        .iter()
        .map(|store| {
            let where_ = match (store.branch, store.in_loop) {
                (None, false) => "body".to_owned(),
                (None, true) => "loop body".to_owned(),
                (Some(branch), false) => format!("branch {branch}"),
                (Some(branch), true) => format!("loop body, branch {branch}"),
            };
            match stored_real(view, store.rhs) {
                Some(value) => format!("{where_}: constant {value}"),
                None => format!(
                    "{where_}: {:?}",
                    view.expression(store.rhs)
                        .expect("a checked definition resolves its right-hand side")
                        .kind()
                ),
            }
        })
        .collect::<Vec<_>>()
        .join("\n  ")
}

/// The POSITIVE half of the §12.4.4 rule: the declared default must *be* the
/// live seed of the lowered body, not merely happen to be the answer.
///
/// `a_declared_output_default_survives_the_return_guard_lowering` above reads
/// simulated values, so it passes for any lowering that arrives at 7 — one that
/// zero-seeds `y` and repairs it later, one that folds a second constant in
/// beside the default, one that reroutes the guard so the seed is dead by
/// accident. Value equality cannot tell those apart from the fixed lowering,
/// and the next reintroduction of this bug will not look like the last one.
///
/// This pins the shape instead, at the checked-DAE level the emitters read:
///
/// * exactly one constant is ever stored into `y`, and it is the declared
///   `7.0` — a regenerated zero seed makes the observed list `[7.0, 0.0]`
///   (seed after default), `[0.0, 7.0]` (seed before default), or `[0.0]`
///   (seed instead of default), all of which fail here; and
/// * that constant is the body's *first* store into `y`, so a seed cannot be
///   reintroduced ahead of the declaration binding and leave the default
///   looking like an ordinary later assignment; and
/// * that first store is a plain top-level statement — neither in a
///   conditional arm nor inside a generated `for` body. A loop-nested store
///   runs zero or more times, so a seed emitted inside one is not the value
///   the non-returning path keeps, and must not be able to pass as the
///   declaration binding.
///
/// The claims are about the emitted store list, so they hold whether the return
/// normalization keeps the default as a leading statement or folds it into a
/// conditional fallback.
#[test]
fn a_declared_output_default_is_the_live_seed_of_the_lowered_body() {
    let compiled = Compiler::new()
        .model("DefaultedReturn")
        .compile_str(DEFAULTED_RETURN, "defaulted_return.mo")
        .expect("a defaulted output with a guarded return should construct a checked DAE");
    compiled.dae().inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index))
            .filter_map(|id| view.function(id))
            .find(|function| function.name().as_str() == "defaultedReturn")
            .expect("the checked DAE keeps the defaulted return function");
        let output = function
            .values()
            .find(|value| {
                value.role() == dae::FunctionValueRole::Output && value.name().as_str() == "y"
            })
            .expect("`y` is the declared output of defaultedReturn");
        let mut stores = Vec::new();
        output_stores(function.statements(), output.id(), false, &mut stores);
        let rendered = describe_stores(view, &stores);

        let constants = stores
            .iter()
            .filter_map(|store| stored_real(view, store.rhs))
            .collect::<Vec<_>>();
        assert_eq!(
            constants,
            vec![7.0],
            "the declaration binding must be the only constant stored into `y`; \
             a return seed would add a 0 beside it. Stores:\n  {rendered}"
        );

        let first = stores
            .first()
            .expect("the lowered body must store into its output");
        assert_eq!(
            stored_real(view, first.rhs),
            Some(7.0),
            "the §12.4.4 declaration binding must be the first store into `y`, \
             so nothing can seed the output ahead of it. Stores:\n  {rendered}"
        );
        assert!(
            first.is_unconditional(),
            "the declaration binding must be a plain top-level statement — not \
             a conditional arm, and not inside a generated loop, either of \
             which runs on some paths only. Stores:\n  {rendered}"
        );
    });
}
