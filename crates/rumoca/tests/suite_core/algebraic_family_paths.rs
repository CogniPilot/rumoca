//! The synthesized algebraic family owner and the scalar-row path compute the
//! same bits, and a family the admission proofs refuse keeps its scalar rows
//! (SPEC_0043 §6c, SPEC_0040 SOLVE-C70).
//!
//! Every comparison here is between two spellings of one model: a `for`
//! family, which flatten and the Solve lowering own as one owner when their
//! proofs hold, and the same equations written out one scalar equation per
//! element, which no proof can group into a family. The explicit spelling is
//! the scalar-row path by construction and keeps the same arithmetic, so the
//! two simulations must agree to the last bit, at every output time.

use rumoca::Compiler;
use rumoca_ir_flat::FamilyInteriors;
use rumoca_ir_solve::{ComputeNode, LinearOp};
use rumoca_sim::SimOptions;

/// What one compilation of a model shows about its algebraic families.
struct Lowered {
    interiors: Vec<FamilyInteriors>,
    /// The number of binders of each family, in the order of `interiors`.
    ranks: Vec<usize>,
    /// Pure-call operations in the continuous residual programs.
    residual_calls: usize,
    result: rumoca_sim::SimResult,
}

/// Compile, lower, and simulate `source` on the output grid 0, 0.5, 1.
fn lower(model: &str, source: &str) -> Lowered {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "algebraic_family_paths.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..Default::default()
    };
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{model} lowers: {error:?}"));
    let mut residual_calls = 0;
    let mut blocks = 0;
    for node in &lowered.problem.continuous.implicit_rhs.nodes {
        if let ComputeNode::ScalarPrograms(block) = node {
            blocks += 1;
            for program in block.programs() {
                residual_calls += program
                    .iter()
                    .filter(|op| matches!(op, LinearOp::PureCall { .. }))
                    .count();
            }
        }
    }
    assert!(blocks > 0, "{model} has scalar-program residual blocks");
    let result = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let interiors = compiled
        .flat
        .structured_equations
        .iter()
        .map(|family| family.interiors)
        .collect();
    let ranks = compiled
        .flat
        .structured_equations
        .iter()
        .map(|family| family.domain.binders.len())
        .collect();
    Lowered {
        interiors,
        ranks,
        residual_calls,
        result,
    }
}

fn column<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let position = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} in the result"));
    &result.data[position]
}

/// Every named variable agrees bit for bit at every output time, and the
/// grids themselves are the same.
fn assert_bit_identical(family: &Lowered, scalar: &Lowered, names: &[String]) {
    assert_eq!(
        family
            .result
            .times
            .iter()
            .map(|t| t.to_bits())
            .collect::<Vec<_>>(),
        scalar
            .result
            .times
            .iter()
            .map(|t| t.to_bits())
            .collect::<Vec<_>>(),
        "output grids"
    );
    for time in [0.0, 0.5, 1.0] {
        assert!(
            family.result.times.iter().any(|t| (t - time).abs() < 1e-12),
            "the grid holds t = {time}: {:?}",
            family.result.times
        );
    }
    for name in names {
        let (left, right) = (column(&family.result, name), column(&scalar.result, name));
        assert_eq!(left.len(), right.len(), "{name}: sample counts");
        for (index, (a, b)) in left.iter().zip(right).enumerate() {
            assert_eq!(
                a.to_bits(),
                b.to_bits(),
                "{name} at t = {}: family {a:e} != scalar {b:e}",
                family.result.times[index]
            );
        }
    }
}

/// States `s[1:5]` feed every family, so each right-hand side is state
/// dependent and changes with time.
const STATES: &str = "
  Real s[5](start = {0.5, 0.75, 1.25, 2.0, 0.9}, each fixed = true);";

const STATE_EQUATIONS: &str = "
  for j in 1:5 loop
    der(s[j]) = -0.3*s[j] + 0.1*j;
  end for;";

/// One operator family: its array name, right-hand side, and the number of
/// synthesized owners its residual calls. A body with a product reduction has
/// no directional relation, so its rows stay scalar (`algebraic_family_program`).
type Operator = (&'static str, &'static str, usize);

/// The operator families flatten admits (`continuous_algebraic.rs`):
/// arithmetic, `^`, the smooth elementary functions, `sum`, `product`,
/// `transpose`, and `size`. Each right-hand side is written once with `{i}`
/// standing for the family binder, which the scalar spelling replaces by the
/// element's index.
const OPERATORS: &[Operator] = &[
    (
        "arith",
        "(s[{i}] + s[{i}+1])*(s[{i}] - 1)/(2 + s[{i}+1])",
        1,
    ),
    ("pow", "s[{i}]^2.5", 1),
    ("sq", "s[{i}]^2", 1),
    ("cu", "s[{i}]^3", 1),
    ("pow4", "s[{i}]^4", 1),
    ("ex", "exp(-s[{i}])", 1),
    ("lg", "log(s[{i}] + 1)", 1),
    ("lg10", "log10(s[{i}] + 1)", 1),
    ("tn", "tan(0.3*s[{i}])", 1),
    ("at2", "atan2(s[{i}], 1.0 + {i})", 1),
    ("sh", "sinh(s[{i}])", 1),
    ("prod", "product({s[{i}], s[{i}] + 1, 2.0})", 0),
    ("prodc", "product(s[{i}+k] for k in 0:1)", 0),
    ("sm", "sum(s[{i}+k]*k for k in 0:1)", 1),
    ("tr", "sum(transpose({{s[{i}], 2*s[{i}]}}))", 1),
    ("sz", "s[{i}]*size(s, 1) + size({1.0, 2.0, 3.0}, 1)", 1),
];

// A binder-valued exponent (`s[i]^i`) is not in the table: the scalar spelling
// sees a literal Integer exponent, which lowers as a product for 2 and 3
// (`product_power_factors`), while the owner evaluates the binder at run time
// with `powf`, so the two may differ in the last bit by construction.

const FAMILY_SIZE: usize = 4;

fn operator_source(family: bool, (name, rhs, _): Operator) -> String {
    let mut text = String::from("model Operators");
    text.push_str(STATES);
    text.push_str(&format!("\n  Real {name}[{FAMILY_SIZE}];\nequation"));
    text.push_str(STATE_EQUATIONS);
    if family {
        let body = rhs.replace("{i}", "i");
        text.push_str(&format!(
            "\n  for i in 1:{FAMILY_SIZE} loop {name}[i] = {body}; end for;"
        ));
    } else {
        for i in 1..=FAMILY_SIZE {
            let body = rhs.replace("{i}", &i.to_string());
            text.push_str(&format!("\n  {name}[{i}] = {body};"));
        }
    }
    text.push_str("\nend Operators;\n");
    text
}

/// Each admitted operator family is one owner, the explicit spelling keeps
/// its scalar rows, and both compute the same bits from state-dependent
/// inputs at t = 0, 0.5, and 1.
#[test]
fn owner_and_scalar_rows_agree_bit_for_bit_for_every_admitted_operator() {
    for operator in OPERATORS {
        let (name, _, owners) = *operator;
        let family = lower("Operators", &operator_source(true, *operator));
        let scalar = lower("Operators", &operator_source(false, *operator));
        assert_eq!(
            family.residual_calls, owners,
            "{name}: synthesized owners of the family spelling"
        );
        assert!(
            family
                .interiors
                .contains(&FamilyInteriors::ContinuousAlgebraic),
            "{name}: flatten owns the family: {:?}",
            family.interiors
        );
        assert!(
            !scalar
                .interiors
                .contains(&FamilyInteriors::ContinuousAlgebraic),
            "{name}: the explicit spelling is no family: {:?}",
            scalar.interiors
        );
        assert_eq!(
            scalar.residual_calls, 0,
            "{name}: the explicit spelling has no owner"
        );
        let names = (1..=FAMILY_SIZE)
            .map(|i| format!("{name}[{i}]"))
            .collect::<Vec<_>>();
        assert_bit_identical(&family, &scalar, &names);
    }
}

/// A family spelling and its explicit scalar spelling of one array whose
/// states are `s` and `r`.
struct Pair {
    model: &'static str,
    declarations: &'static str,
    family: &'static str,
    explicit: &'static str,
    outputs: Vec<String>,
}

impl Pair {
    fn source(&self, family: bool) -> String {
        format!(
            "model {model}{STATES}\n{decl}\nequation{STATE_EQUATIONS}\n{body}\nend {model};\n",
            model = self.model,
            decl = self.declarations,
            body = if family { self.family } else { self.explicit },
        )
    }

    /// Both spellings, simulated, with their variables bit-identical.
    fn lower_both(&self) -> (Lowered, Lowered) {
        let family = lower(self.model, &self.source(true));
        let scalar = lower(self.model, &self.source(false));
        assert_bit_identical(&family, &scalar, &self.outputs);
        (family, scalar)
    }
}

fn elements(array: &str, indices: impl IntoIterator<Item = usize>) -> Vec<String> {
    indices
        .into_iter()
        .map(|index| format!("{array}[{index}]"))
        .collect()
}

/// A recurrence reads its own target: no owner can evaluate it as one
/// elementwise map, so its rows stay scalar and solve in dependency order.
#[test]
fn a_recurrence_keeps_scalar_rows_and_its_values() {
    let recurrence = Pair {
        model: "Recurrence",
        declarations: "  Real y[4];",
        family: "  y[1] = s[1];\n  for i in 2:4 loop y[i] = y[i-1] + s[i]; end for;",
        explicit: "  y[1] = s[1];\n  y[2] = y[1] + s[2];\n  y[3] = y[2] + s[3];\n  y[4] = y[3] + s[4];",
        outputs: elements("y", 1..=4),
    };
    let (family, _) = recurrence.lower_both();
    assert_eq!(family.residual_calls, 0, "no owner for a recurrence");
    let (y, s) = (
        column(&family.result, "y[4]"),
        column(&family.result, "s[1]"),
    );
    assert!(
        y.iter().zip(s).all(|(y, s)| *y > *s),
        "y[4] sums the states"
    );

    // A family over the target's whole extent that reads its own element
    // is refused by that read alone: unit step, lower bound 1, own binder.
    let self_read = Pair {
        model: "SelfRead",
        declarations: "  Real y[4];",
        family: "  for i in 1:4 loop y[i] = s[i] + 0.5*y[i]; end for;",
        explicit: "  y[1] = s[1] + 0.5*y[1];\n  y[2] = s[2] + 0.5*y[2];\n  y[3] = s[3] + 0.5*y[3];\n  y[4] = s[4] + 0.5*y[4];",
        outputs: elements("y", 1..=4),
    };
    let (family, _) = self_read.lower_both();
    assert_eq!(family.residual_calls, 0, "no owner for a target read");
    for index in 1..=4 {
        let (y, s) = (
            column(&family.result, &format!("y[{index}]")),
            column(&family.result, &format!("s[{index}]")),
        );
        for (y, s) in y.iter().zip(s) {
            assert!(
                (y - 2.0 * s).abs() < 1e-9,
                "y[{index}] = 2 s[{index}]: {y} {s}"
            );
        }
    }
}

/// The owner requires binders `1:n` of unit step over the target's whole
/// extent, in declaration order. Descending, stepped, and non-unit-lower
/// binders are refused and simulate through the template path, bit for bit
/// as the explicit rows; the ascending whole-extent control is one owner.
#[test]
fn binders_off_the_whole_extent_keep_scalar_rows_and_their_values() {
    let control = Pair {
        model: "Ascending",
        declarations: "  Real z[4];",
        family: "  for i in 1:4 loop z[i] = 2*s[i] + i; end for;",
        explicit: "  z[1] = 2*s[1] + 1;\n  z[2] = 2*s[2] + 2;\n  z[3] = 2*s[3] + 3;\n  z[4] = 2*s[4] + 4;",
        outputs: elements("z", 1..=4),
    };
    let (family, _) = control.lower_both();
    assert_eq!(family.residual_calls, 1, "the control is one owner");

    let descending = Pair {
        model: "Descending",
        declarations: "  Real z[4];",
        family: "  for i in 4:-1:1 loop z[i] = 2*s[i] + i; end for;",
        explicit: control.explicit,
        outputs: elements("z", 1..=4),
    };
    let stepped = Pair {
        model: "Stepped",
        declarations: "  Real u[7];",
        family: "  for i in 1:2:7 loop u[i] = s[1]*i + 1; end for;\n  for i in 2:2:6 loop u[i] = s[1]*i + 1; end for;",
        explicit: "  u[1] = s[1]*1 + 1;\n  u[2] = s[1]*2 + 1;\n  u[3] = s[1]*3 + 1;\n  u[4] = s[1]*4 + 1;\n  u[5] = s[1]*5 + 1;\n  u[6] = s[1]*6 + 1;\n  u[7] = s[1]*7 + 1;",
        outputs: elements("u", 1..=7),
    };
    let shifted = Pair {
        model: "Shifted",
        declarations: "  Real z[4];",
        family: "  z[1] = 2*s[1] + 1;\n  for i in 2:4 loop z[i] = 2*s[i] + i; end for;",
        explicit: control.explicit,
        outputs: elements("z", 1..=4),
    };
    for refused in [descending, stepped, shifted] {
        let (family, _) = refused.lower_both();
        assert_eq!(
            family.residual_calls, 0,
            "{}: the owner admission refuses the binders",
            refused.model
        );
    }
}

/// A target a `der` equation reads is no continuous algebraic family
/// (SPEC_0043 §6c row 1): flatten materializes its rows. A nested range that
/// depends on an outer binder makes the nest non-uniform: the outer binder is
/// unrolled and each inner loop is a family of its own, while independent
/// ranges keep one two-binder family. Values are those of the explicit
/// equations in every case.
#[test]
fn a_der_read_target_stays_materialized_and_a_dependent_range_unrolls_its_outer_binder() {
    let der_read = Pair {
        model: "DerRead",
        declarations: "  Real y[4];\n  Real r[4](start = {0.4, 0.8, 1.2, 1.6}, each fixed = true);",
        family: "  for i in 1:4 loop y[i] = 2*s[i]; end for;\n  for j in 1:4 loop der(r[j]) = -y[j]; end for;",
        explicit: "  y[1] = 2*s[1];\n  y[2] = 2*s[2];\n  y[3] = 2*s[3];\n  y[4] = 2*s[4];\n  der(r[1]) = -y[1];\n  der(r[2]) = -y[2];\n  der(r[3]) = -y[3];\n  der(r[4]) = -y[4];",
        outputs: [elements("y", 1..=4), elements("r", 1..=4)].concat(),
    };
    let (family, _) = der_read.lower_both();
    assert!(
        !family
            .interiors
            .contains(&FamilyInteriors::ContinuousAlgebraic),
        "a der-read target keeps its rows: {:?}",
        family.interiors
    );

    let triangular = Pair {
        model: "Triangular",
        declarations: "  Real t[3,3];",
        family: "  for i in 1:3 loop for j in i:3 loop t[i,j] = s[i]*s[j]; end for; end for;\n  for i in 2:3 loop for j in 1:i-1 loop t[i,j] = s[i] + s[j]; end for; end for;",
        explicit: "  t[1,1] = s[1]*s[1];\n  t[1,2] = s[1]*s[2];\n  t[1,3] = s[1]*s[3];\n  t[2,2] = s[2]*s[2];\n  t[2,3] = s[2]*s[3];\n  t[3,3] = s[3]*s[3];\n  t[2,1] = s[2] + s[1];\n  t[3,1] = s[3] + s[1];\n  t[3,2] = s[3] + s[2];",
        outputs: (1..=3)
            .flat_map(|i| (1..=3).map(move |j| format!("t[{i},{j}]")))
            .collect(),
    };
    let (family, _) = triangular.lower_both();
    let owned_ranks = |lowered: &Lowered| {
        lowered
            .interiors
            .iter()
            .zip(&lowered.ranks)
            .filter(|(interiors, _)| **interiors == FamilyInteriors::ContinuousAlgebraic)
            .map(|(_, rank)| *rank)
            .collect::<Vec<_>>()
    };
    // The outer binder is unrolled: each inner loop is a family of one binder.
    assert_eq!(owned_ranks(&family), [1; 5], "{:?}", family.interiors);

    let rectangular = Pair {
        model: "Rectangular",
        declarations: "  Real t[3,3];",
        family: "  for i in 1:3 loop for j in 1:3 loop t[i,j] = s[i]*s[j]; end for; end for;",
        explicit: "  t[1,1] = s[1]*s[1];\n  t[1,2] = s[1]*s[2];\n  t[1,3] = s[1]*s[3];\n  t[2,1] = s[2]*s[1];\n  t[2,2] = s[2]*s[2];\n  t[2,3] = s[2]*s[3];\n  t[3,1] = s[3]*s[1];\n  t[3,2] = s[3]*s[2];\n  t[3,3] = s[3]*s[3];",
        outputs: triangular.outputs.clone(),
    };
    let (family, _) = rectangular.lower_both();
    assert_eq!(owned_ranks(&family), [2], "{:?}", family.interiors);
}

/// A consumer of the scalar equation view rejects a family whose interiors
/// are placeholders at the family's span (SPEC_0043 §6c row 2); the same
/// equations written as scalars render.
#[test]
fn a_flat_template_over_placeholder_rows_rejects_the_family_at_its_span() {
    use rumoca_compile::codegen::{CodegenError, render_flat_template_with_name};
    let source = |family: bool| {
        let body = if family {
            "  for i in 1:4 loop z[i] = 2*time + i; end for;"
        } else {
            "  z[1] = 2*time + 1;\n  z[2] = 2*time + 2;\n  z[3] = 2*time + 3;\n  z[4] = 2*time + 4;"
        };
        format!("model Placeholder\n  Real z[4];\nequation\n{body}\nend Placeholder;\n")
    };
    let flat = |family: bool| {
        Compiler::new()
            .model("Placeholder")
            .compile_str(&source(family), "placeholder.mo")
            .unwrap_or_else(|error| panic!("placeholder compiles: {error:?}"))
            .flat
    };
    let template = "{{ model_name }}";
    match render_flat_template_with_name(&flat(true), template, "Placeholder") {
        Err(CodegenError::NonMaterializedStructuredFamily {
            partition, span, ..
        }) => {
            assert_eq!(partition, "regular equations");
            assert!(span.is_some(), "the rejection carries the family's span");
        }
        other => panic!("expected the placeholder family to be rejected: {other:?}"),
    }
    render_flat_template_with_name(&flat(false), template, "Placeholder")
        .expect("materialized rows render");
}
