//! End-to-end regressions for `homotopy(actual, simplified)`.
//!
//! MLS 3.6 §3.7.4.3 defines the operator through the blend
//! `lambda*actual + (1 - lambda)*simplified` and *permits*, but does not
//! require, a tool to walk λ from `0` to `1` while solving the initialization
//! problem. It explicitly sanctions the trivial implementation
//! `homotopy(actual, simplified) = actual`, so a conforming tool may land on any
//! root of a multi-rooted initialization problem. Everything below is therefore
//! two separate claims:
//!
//! * **MLS-required** — at simulation time the expression reads exactly
//!   `actual`. rumoca lowers the blend with λ as a hidden `P` slot seeded to
//!   `1.0`, so this holds on the Solve runtime, and the rendered backends emit
//!   `actual` directly. `endpoint_*` and the `reads_actual_*` tests below pin
//!   this.
//! * **rumoca policy** — the Solve runtime additionally runs the λ continuation
//!   over the solves its coverage certificate names, so a multi-rooted
//!   initialization lands on the root the `simplified` operand selects. That is
//!   what `Modelica.Electrical.Analog.Examples.OpAmps.SignalGenerator` needs
//!   (it sets `opAmp1(homotopyType = LimiterHomotopy.LowerLimit)`, which
//!   `IdealizedOpAmpLimited` maps to `simplified = vns = -15`) and it is what
//!   the `branch_selection_*` tests pin. A tool taking the trivial
//!   implementation would be equally conforming; these tests guard rumoca's
//!   choice, not the standard's demand.
//!
//! The fixtures are the probe models from the homotopy review: `BistableLoop`
//! is the eight-line shape of SignalGenerator's Schmitt trigger (`x = sat(6000·x)`
//! has roots `-15`, `0`, `+15`), and `E1`..`E11` cover every row family the
//! Solve lowering can put a λ read in.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn simulate(
    source: &str,
    model: &str,
    t_end: f64,
) -> Result<SimResult, Box<dyn std::error::Error>> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))?;
    let opts = SimOptions {
        t_end,
        ..SimOptions::default()
    };
    Ok(simulate_dae_with_diagnostics(&compiled.dae, &opts)?)
}

fn channel<'sim>(sim: &'sim SimResult, name: &str) -> &'sim [f64] {
    let index = sim
        .names
        .iter()
        .position(|channel| channel == name)
        .unwrap_or_else(|| panic!("`{name}` missing from simulation channels {:?}", sim.names));
    &sim.data[index]
}

fn first(sim: &SimResult, name: &str) -> f64 {
    *channel(sim, name)
        .first()
        .expect("simulation produced no samples")
}

fn last(sim: &SimResult, name: &str) -> f64 {
    *channel(sim, name)
        .last()
        .expect("simulation produced no samples")
}

fn assert_close(actual: f64, expected: f64, tol: f64, what: &str) {
    assert!(
        (actual - expected).abs() <= tol,
        "{what}: expected {expected} within {tol}, got {actual}"
    );
}

// ---------------------------------------------------------------------------
// Branch selection (rumoca policy: the continuation steers the algebraic loop)
// ---------------------------------------------------------------------------

const BISTABLE_LOOP: &str = r#"
model BistableLoop
  Real x "saturating amplifier output";
  Real vin "feedback divider output";
  Real y(start = 0, fixed = true) "integral of x, so the branch reaches a state";
equation
  vin = 0.4 * x;
  x = homotopy(
        actual = if 15000 * vin > 15.0 then 15.0
                 else if 15000 * vin < -15.0 then -15.0
                 else 15000 * vin,
        simplified = SIMPLIFIED);
  der(y) = x;
end BistableLoop;
"#;

fn bistable_loop(simplified: &str) -> String {
    BISTABLE_LOOP.replace("SIMPLIFIED", simplified)
}

#[test]
fn branch_selection_follows_the_simplified_operand() -> Result<(), Box<dyn std::error::Error>> {
    let sim = simulate(&bistable_loop("-15.0"), "BistableLoop", 0.1)?;

    assert_close(
        first(&sim, "x"),
        -15.0,
        1.0e-6,
        "homotopy names -15 as the simplified branch, so initialization must \
         land there rather than on the 0 or +15 root",
    );
    assert_close(
        first(&sim, "vin"),
        -6.0,
        1.0e-6,
        "the whole loop must settle on the selected branch, not just its head",
    );
    assert_close(
        last(&sim, "y"),
        -1.5,
        1.0e-4,
        "the selected branch must drive the state for the whole run \
         (der(y) = -15 over 0.1 s)",
    );
    Ok(())
}

/// The load-bearing half of the branch-selection claim: change *only* the
/// `simplified` operand and the run must settle on a different root. Without
/// this, the `-15` result above is equally explained by "the cold algebraic
/// guess happened to land there".
#[test]
fn branch_selection_changes_root_when_simplified_changes() -> Result<(), Box<dyn std::error::Error>>
{
    let lower = simulate(&bistable_loop("-15.0"), "BistableLoop", 0.1)?;
    let upper = simulate(&bistable_loop("15.0"), "BistableLoop", 0.1)?;

    assert_close(
        first(&lower, "x"),
        -15.0,
        1.0e-6,
        "simplified = -15 selects x = -15",
    );
    assert_close(
        first(&upper, "x"),
        15.0,
        1.0e-6,
        "simplified = +15 selects x = +15",
    );
    assert_close(
        last(&lower, "y"),
        -1.5,
        1.0e-4,
        "the -15 branch integrates to -1.5",
    );
    assert_close(
        last(&upper, "y"),
        1.5,
        1.0e-4,
        "the +15 branch integrates to +1.5",
    );
    Ok(())
}

/// `E11_Shifted`: the same loop behind a vector equation, so the λ-reading
/// program is no longer at the equation index that shares its number. Its
/// implicit block emits `output_indices = [5, 4, 1, 2, 3]`; resolving the λ row
/// by program index would check equation 1 instead of equation 4.
#[test]
fn branch_selection_survives_a_shifted_equation_index() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E11_Shifted "BistableLoop preceded by a vector equation that shifts program indices"
  Real v[3];
  Real x "saturating amplifier output";
  Real vin "feedback divider output";
  Real y(start = 0, fixed = true);
equation
  v = {1.0, 2.0, 3.0} * x;
  vin = 0.4 * x;
  x = homotopy(
        actual = if 15000 * vin > 15.0 then 15.0
                 else if 15000 * vin < -15.0 then -15.0
                 else 15000 * vin,
        simplified = -15.0);
  der(y) = x;
end E11_Shifted;
"#;
    let sim = simulate(SOURCE, "E11_Shifted", 0.1)?;

    assert_close(first(&sim, "x"), -15.0, 1.0e-6, "the loop head selects -15");
    assert_close(
        first(&sim, "vin"),
        -6.0,
        1.0e-6,
        "the divider follows the branch",
    );
    assert_close(
        first(&sim, "v[1]"),
        -15.0,
        1.0e-6,
        "the vector row follows the branch",
    );
    assert_close(
        first(&sim, "v[3]"),
        -45.0,
        1.0e-6,
        "the vector row follows the branch",
    );
    assert_close(last(&sim, "y"), -1.5, 1.0e-4, "the branch drives the state");
    Ok(())
}

// ---------------------------------------------------------------------------
// MLS-required: λ = 1 reproduces `actual` exactly, in every row family
// ---------------------------------------------------------------------------

/// `E7_Endpoint`: the endpoint has to be *exact*, not merely algebraically
/// equal. `simplified + lambda*(actual - simplified)` evaluates
/// `1e17 + 1.0*(1.0 - 1e17)` to `0.0` in IEEE double and pins the whole run at
/// zero; `lambda*actual + (1 - lambda)*simplified` returns `1.0`.
#[test]
fn endpoint_is_exact_when_simplified_dwarfs_actual() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E7_Endpoint "lambda=1 must reproduce `actual` exactly"
  Real y(start = 0, fixed = true);
  Real a;
equation
  a = homotopy(actual = 1.0, simplified = 1e17);
  der(y) = a;
end E7_Endpoint;
"#;
    let sim = simulate(SOURCE, "E7_Endpoint", 1.0)?;

    assert_eq!(
        first(&sim, "a"),
        1.0,
        "homotopy(1.0, 1e17) must be exactly 1.0 at lambda = 1"
    );
    assert_eq!(
        last(&sim, "a"),
        1.0,
        "lambda stays pinned at 1 for the whole run"
    );
    assert_close(last(&sim, "y"), 1.0, 1.0e-6, "der(y) = 1 over 1 s");
    Ok(())
}

/// `E4_AlgHomotopy`: the accepted, steered shape — `homotopy` on an algebraic
/// row. At λ = 1 the row reads `actual`.
#[test]
fn reads_actual_on_an_algebraic_row() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E4_AlgHomotopy "homotopy on an algebraic row"
  Real y(start = 0, fixed = true);
  Real a;
equation
  a = homotopy(actual = 1.0, simplified = 2.0);
  der(y) = a;
end E4_AlgHomotopy;
"#;
    let sim = simulate(SOURCE, "E4_AlgHomotopy", 1.0)?;

    assert_close(
        first(&sim, "a"),
        1.0,
        1.0e-9,
        "a reads `actual`, not `simplified`",
    );
    assert_close(last(&sim, "y"), 1.0, 1.0e-6, "der(y) = 1 over 1 s");
    Ok(())
}

/// `E1_DerHomotopy`: λ lowers into `continuous.derivative_rhs`. No solve owns
/// that row, so the continuation steers nothing and the row evaluates at λ = 1.
/// Legal MLS; it must not be rejected.
#[test]
fn reads_actual_on_a_derivative_row() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E1_DerHomotopy "homotopy directly on an explicit derivative row"
  Real y(start = 0, fixed = true);
equation
  der(y) = homotopy(actual = 1.0, simplified = 2.0);
end E1_DerHomotopy;
"#;
    let sim = simulate(SOURCE, "E1_DerHomotopy", 1.0)?;

    assert_close(
        last(&sim, "y"),
        1.0,
        1.0e-6,
        "der(y) = homotopy(1, 2) integrates `actual` = 1, not `simplified` = 2",
    );
    Ok(())
}

/// `E2_WhenHomotopy`: λ lowers into `discrete.rhs`. Same contract as the
/// derivative row.
///
/// The trigger is spelled `time >= 0.5` rather than the review probe's
/// `time > 0.5`: in this tree the strict form produces no event at all, so the
/// body never fires and `z` stays at its start for any right-hand side,
/// homotopy or not. That is a `when`/event defect with nothing to do with
/// `homotopy`, and pinning it here would only hide this contract behind it.
#[test]
fn reads_actual_inside_a_when_clause() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E2_WhenHomotopy "homotopy inside a when-clause (discrete row only)"
  Real x(start = 0, fixed = true);
  discrete Real z(start = 0, fixed = true);
equation
  der(x) = z;
  when time >= 0.5 then
    z = homotopy(actual = 1.0, simplified = 2.0);
  end when;
end E2_WhenHomotopy;
"#;
    let sim = simulate(SOURCE, "E2_WhenHomotopy", 1.0)?;

    assert_close(
        first(&sim, "z"),
        0.0,
        1.0e-9,
        "the when body has not fired at t = 0",
    );
    assert_close(
        last(&sim, "z"),
        1.0,
        1.0e-9,
        "the fired body reads `actual`",
    );
    assert_close(
        last(&sim, "x"),
        0.5,
        1.0e-3,
        "der(x) = 1 over the last half second",
    );
    Ok(())
}

/// `E8_Closure`: a non-fixed state whose `der()` row carries a λ read that
/// closes over the state itself. `der(x) = x^3 - x` from `x(0) = 0.5` has the
/// closed-form solution `x0*e^-t / sqrt(1 - x0^2 + x0^2*e^-2t)`.
#[test]
fn reads_actual_on_a_state_closing_derivative_row() -> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E8_Closure "non-fixed state whose der() row carries the homotopy"
  Real x(start = 0.5);
equation
  der(x) = homotopy(actual = x^3 - x, simplified = x + 1);
end E8_Closure;
"#;
    let sim = simulate(SOURCE, "E8_Closure", 1.0)?;

    assert_close(
        first(&sim, "x"),
        0.5,
        1.0e-6,
        "the start attribute is honored",
    );
    let decay = f64::exp(-1.0);
    let expected = 0.5 * decay / (1.0 - 0.25 + 0.25 * decay * decay).sqrt();
    assert_close(
        last(&sim, "x"),
        expected,
        5.0e-3,
        "the run must integrate `actual` = x^3 - x; `simplified` = x + 1 would grow",
    );
    Ok(())
}

/// `E9_StateAndAlg`: λ read from a derivative row *and* a steered algebraic row
/// in the same model.
#[test]
fn reads_actual_on_a_state_and_an_algebraic_row_together() -> Result<(), Box<dyn std::error::Error>>
{
    const SOURCE: &str = r#"
model E9_StateAndAlg "homotopy on both a state row and an algebraic row"
  Real x(start = 0, fixed = true);
  Real a;
equation
  a = homotopy(actual = 1.0, simplified = 2.0);
  der(x) = homotopy(actual = 3.0, simplified = 4.0) + a;
end E9_StateAndAlg;
"#;
    let sim = simulate(SOURCE, "E9_StateAndAlg", 1.0)?;

    assert_close(
        first(&sim, "a"),
        1.0,
        1.0e-9,
        "the algebraic row reads `actual`",
    );
    assert_close(
        last(&sim, "x"),
        4.0,
        1.0e-5,
        "der(x) = 3 + 1 over 1 s; the simplified system would give 6",
    );
    Ok(())
}

/// `E10_Mixed`: a vector equation ahead of the λ row, so the λ-reading program
/// is index 0 while its equation index is 4. `implicit_row_targets[0]` is
/// `None` and `implicit_row_targets[4]` is `Y4`, which is why resolving the row
/// in the wrong index space drops it out of the coverage requirement entirely.
#[test]
fn reads_actual_when_a_vector_row_shifts_the_equation_index()
-> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E10_Mixed "vector row ahead of the homotopy row"
  Real v[3];
  Real a;
  Real y(start = 0, fixed = true);
equation
  v = {1.0, 2.0, 3.0} * a;
  a = homotopy(actual = 1.0, simplified = 2.0);
  der(y) = v[1] + v[2] + v[3];
end E10_Mixed;
"#;
    let sim = simulate(SOURCE, "E10_Mixed", 1.0)?;

    assert_close(
        first(&sim, "a"),
        1.0,
        1.0e-9,
        "the algebraic row reads `actual`",
    );
    assert_close(first(&sim, "v[2]"), 2.0, 1.0e-9, "the vector row follows a");
    assert_close(
        last(&sim, "y"),
        6.0,
        1.0e-5,
        "der(y) = 1 + 2 + 3 over 1 s; the simplified system would give 12",
    );
    Ok(())
}

/// `E6_SteadyState`: a steady-state `initial equation` against a `der()` row
/// that carries λ. The initialization system must accept it — the row is not
/// owned by any projection block, so the continuation owes it no coverage — and
/// the result must satisfy the steady-state condition it states.
#[test]
fn steady_state_initialization_with_a_homotopy_derivative_row()
-> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E6_SteadyState "steady-state initialization over a homotopy der() row"
  Real x(start = 0.0, fixed = false);
initial equation
  der(x) = 0;
equation
  der(x) = homotopy(actual = x^3 - x, simplified = x + 1);
end E6_SteadyState;
"#;
    let sim = simulate(SOURCE, "E6_SteadyState", 1.0)?;

    let x0 = first(&sim, "x");
    let residual = x0 * x0 * x0 - x0;
    assert!(
        residual.abs() <= 1.0e-6,
        "`initial equation der(x) = 0` requires x(0) to be a root of \
         actual = x^3 - x; got x(0) = {x0} with residual {residual}"
    );
    assert_close(
        last(&sim, "x"),
        x0,
        1.0e-6,
        "a steady state must not drift over the run",
    );
    Ok(())
}

/// `E3_DeadBranch`: `homotopy` written inside a structurally false `if` branch.
/// The DAE still carries the expression node, so the λ slot is allocated or not
/// by the same rule as any other builtin — either way the model must simulate
/// the live branch.
#[test]
fn structurally_dead_homotopy_branch_simulates_the_live_branch()
-> Result<(), Box<dyn std::error::Error>> {
    const SOURCE: &str = r#"
model E3_DeadBranch "homotopy inside a structurally false if-branch"
  parameter Boolean b = false;
  Real x;
  Real y(start = 0, fixed = true);
equation
  if b then
    x = homotopy(actual = 1.0, simplified = 2.0);
  else
    x = 3.0;
  end if;
  der(y) = x;
end E3_DeadBranch;
"#;
    let sim = simulate(SOURCE, "E3_DeadBranch", 1.0)?;

    assert_close(
        first(&sim, "x"),
        3.0,
        1.0e-9,
        "the live branch determines x",
    );
    assert_close(last(&sim, "y"), 3.0, 1.0e-5, "der(y) = 3 over 1 s");
    Ok(())
}
