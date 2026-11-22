//! An elementwise power `.^` of a Real aggregate base with a literal Integer
//! exponent lowers each element as the literal plan of
//! `modelica_integer_power`, as a scalar base does (SPEC_0040 SOLVE-C86): no
//! pure-call owner is issued, and the values are those of the same equations
//! written one scalar power per element, bit for bit.

use rumoca::Compiler;
use rumoca_ir_solve::{ComputeNode, LinearOp};
use rumoca_sim::SimOptions;

/// Pure-call operations in the continuous residual programs, and the
/// simulation on the grid 0, 0.5, 1.
fn lower(model: &str, source: &str) -> (usize, rumoca_sim::SimResult) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "elementwise_literal_power.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..Default::default()
    };
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{model} lowers: {error:?}"));
    // Every pure-call owner the lowering issues, whatever program reads it.
    let calls = lowered.pure_calls.owners().len();
    let result = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    (calls, result)
}

fn column<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let position = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} in the result"));
    &result.data[position]
}

fn assert_bit_identical(
    (left, right): (&rumoca_sim::SimResult, &rumoca_sim::SimResult),
    names: &[String],
    what: &str,
) {
    assert_eq!(left.times.len(), right.times.len(), "{what}: output grids");
    for name in names {
        for (index, (a, b)) in column(left, name)
            .iter()
            .zip(column(right, name))
            .enumerate()
        {
            assert!(
                a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan()),
                "{what}: {name} at sample {index}: aggregate {a:e} != scalar {b:e}"
            );
        }
    }
}

/// Bases of every sign and scale, moving with time so the powers change.
const BASES: [&str; 4] = [
    "0.5 + time",
    "-1.3333333333333333 - 0.25*time",
    "1.0000000000000002 + 3*time",
    "-0.7310585786300049*(1 + time)",
];

fn vector_source(model: &str, exponent: i64, aggregate: bool) -> String {
    let n = BASES.len();
    let mut text = format!("model {model}\n  Real s[{n}];\n  Real x[{n}];\nequation\n");
    for (i, base) in BASES.iter().enumerate() {
        text.push_str(&format!("  s[{}] = {base};\n", i + 1));
    }
    if aggregate {
        text.push_str(&format!("  x = s .^ ({exponent});\n"));
    } else {
        for i in 1..=n {
            text.push_str(&format!("  x[{i}] = s[{i}] ^ ({exponent});\n"));
        }
    }
    text.push_str(&format!("end {model};\n"));
    text
}

fn names(array: &str, indices: &[&str]) -> Vec<String> {
    indices
        .iter()
        .map(|index| format!("{array}[{index}]"))
        .collect()
}

/// Negative, zero, unit, small, and large literal exponents on a vector base.
#[test]
fn a_vector_base_issues_no_owner_and_matches_scalar_powers_for_every_exponent() {
    for exponent in [-17, -1, 0, 1, 2, 3, 16, 17, 31, 63] {
        let (calls, aggregate) = lower("Pow", &vector_source("Pow", exponent, true));
        assert_eq!(calls, 0, "s .^ {exponent} issues no pure-call owner");
        let (_, scalar) = lower("Pow", &vector_source("Pow", exponent, false));
        assert_bit_identical(
            (&aggregate, &scalar),
            &names("x", &["1", "2", "3", "4"]),
            &format!("exponent {exponent}"),
        );
    }
}

/// A matrix base takes the same rule per element, and an elementwise power
/// inside a function body (a typed body) does too.
#[test]
fn a_matrix_base_and_a_function_body_match_scalar_powers() {
    let matrix = |aggregate: bool| {
        let body = if aggregate {
            "  x = s .^ 3;\n".to_string()
        } else {
            (1..=2)
                .flat_map(|i| (1..=3).map(move |j| format!("  x[{i},{j}] = s[{i},{j}] ^ 3;\n")))
                .collect()
        };
        format!(
            "model Mat\n  Real s[2,3];\n  Real x[2,3];\nequation\n  \
             s = {{{{0.5 + time, -2*time, 1.25}}, {{-0.75, 3 + time, -1.0000000000000002}}}};\n\
             {body}end Mat;\n"
        )
    };
    let (calls, aggregate) = lower("Mat", &matrix(true));
    assert_eq!(calls, 0, "a matrix base issues no pure-call owner");
    let (_, scalar) = lower("Mat", &matrix(false));
    let cells: Vec<String> = (1..=2)
        .flat_map(|i| (1..=3).map(move |j| format!("x[{i},{j}]")))
        .collect();
    assert_bit_identical((&aggregate, &scalar), &cells, "matrix base");

    let function = |body: &str| {
        format!(
            "model Fn\n  function f\n    input Real u[3];\n    output Real y[3];\n  \
             algorithm\n    {body}\n  end f;\n  Real x[3];\nequation\n  \
             x = f({{0.5 + time, -1.5*(1 + time), 2.0000000000000004}});\nend Fn;\n"
        )
    };
    let (_, aggregate) = lower("Fn", &function("y := u .^ (-3);"));
    let (_, scalar) = lower(
        "Fn",
        &function("y := {u[1] ^ (-3), u[2] ^ (-3), u[3] ^ (-3)};"),
    );
    assert_bit_identical(
        (&aggregate, &scalar),
        &names("x", &["1", "2", "3"]),
        "function body",
    );
}

/// Mixed literal exponents in one expression, an Integer base (whose power
/// keeps its existing lowering), and a Real exponent `3.0` (which keeps
/// `pow`) each match their scalar spelling.
#[test]
fn mixed_exponents_an_integer_base_and_a_real_exponent_match_scalar_powers() {
    let model = |name: &str, declarations: &str, aggregate: &str, element: &str| {
        let dynamics = if declarations.contains("Real s[3]") {
            "  der(s) = -0.01 * x;\n"
        } else {
            ""
        };
        let scalar: String = (1..=3)
            .map(|i| format!("  x[{i}] = {};\n", element.replace("{i}", &i.to_string())))
            .collect();
        (
            format!(
                "model {name}\n{declarations}equation\n{dynamics}  x = {aggregate};\nend {name};\n"
            ),
            format!("model {name}\n{declarations}equation\n{dynamics}{scalar}end {name};\n"),
        )
    };
    let states = "  Real s[3](start = {1, 1.5, 2}, each fixed = true);\n  Real x[3];\n";
    let cases = [
        model(
            "Mixed",
            states,
            "s .^ 3 + 0.1 * s .^ (-2) + s .^ 0 + {1, 2, 3} .^ 2 .* s",
            "s[{i}] ^ 3 + 0.1 * s[{i}] ^ (-2) + s[{i}] ^ 0 + {i} ^ 2 * s[{i}]",
        ),
        model(
            "IntBase",
            "  Integer k[3] = {1, 2, 3};\n  Real x[3];\n",
            "k .^ 2 .* (1 + time)",
            "k[{i}] ^ 2 * (1 + time)",
        ),
        model("RealExp", states, "s .^ 3.0", "s[{i}] ^ 3.0"),
    ];
    for (aggregate, scalar) in cases {
        let name = aggregate[6..aggregate.find('\n').unwrap()].to_string();
        let (_, left) = lower(&name, &aggregate);
        let (_, right) = lower(&name, &scalar);
        assert_bit_identical((&left, &right), &names("x", &["1", "2", "3"]), &name);
    }
}

/// An empty aggregate base lowers and simulates with no owner.
#[test]
fn an_empty_base_issues_no_owner() {
    let source = "model Empty\n  Real s[0];\n  Real x[0];\n  Real y(start = 1, fixed = true);\n\
                  equation\n  s = fill(time, 0);\n  x = s .^ 3;\n  der(y) = -y;\nend Empty;\n";
    let (calls, result) = lower("Empty", source);
    assert_eq!(calls, 0, "an empty base issues no pure-call owner");
    assert_eq!(
        result.times.len(),
        3,
        "the run reaches t = 1: {:?}",
        result.times
    );
}

/// The ladder fixture's KCL rows read `v .^ 3`. Lowered per element, no
/// row reads the whole array through an opaque call, so the staged
/// value schedule of its algebraic refresh is certified (no stage changes
/// a value an earlier stage read) and runs block by block instead of as one
/// simultaneous projection (SPEC_0040 SOLVE-C86).
#[test]
fn the_ladder_refresh_runs_its_certified_stages_without_an_owner() {
    let source = include_str!("../fixtures/elementwise_literal_power/PowerLadder.mo");
    for model in ["PowerLadder.Ladder", "PowerLadder.Ladder50"] {
        let lowered = lowered(model, source);
        assert_eq!(
            lowered.pure_calls.owners().len(),
            0,
            "{model}: `v .^ 3` issues no owner"
        );
        let plan = lowered.problem.continuous.refresh_owners.algebraic();
        assert!(
            plan.value_stage_schedule_is_certified(&lowered.artifacts.continuous.structural),
            "{model}: the staged refresh schedule is certified"
        );
    }
}

/// The isolators of every ladder program, derived once per program, are the
/// pairs a per-pair derivation finds when asked about every output and every
/// target, with the same winning rule.
#[test]
fn the_ladder_isolators_match_a_derivation_of_every_pair() {
    let source = include_str!("../fixtures/elementwise_literal_power/PowerLadder.mo");
    for model in ["PowerLadder.Ladder", "PowerLadder.Ladder50"] {
        let lowered = lowered(model, source);
        let mut certified = 0;
        for program in scalar_programs(&lowered) {
            let inventory = rumoca_ir_solve::derive_target_assignment_shapes(program);
            let outputs = stored_outputs(program);
            let targets = y_extent(program);
            let every_pair: Vec<_> = (0..outputs)
                .flat_map(|output| (0..targets).map(move |target| (output, target)))
                .filter_map(|(output, target)| {
                    rumoca_ir_solve::derive_target_assignment_shape_for_output(
                        program, output, target,
                    )
                    .map(|shape| (output, shape))
                })
                .collect();
            assert_eq!(rules(&inventory), rules(&every_pair), "{model}");
            assert_eq!(inventory, every_pair, "{model}");
            certified += inventory.len();
        }
        assert!(certified > 0, "{model}: the ladder certifies isolators");
    }
}

/// Each certificate's output, target, and winning rule.
fn rules(
    shapes: &[(usize, rumoca_ir_solve::TargetAssignmentShape)],
) -> Vec<(
    usize,
    usize,
    std::mem::Discriminant<rumoca_ir_solve::TargetAssignmentShape>,
)> {
    shapes
        .iter()
        .map(|(output, shape)| {
            (
                *output,
                shape.target_y_index(),
                std::mem::discriminant(shape),
            )
        })
        .collect()
}

/// The outputs a program stores, the offsets its certificates name.
fn stored_outputs(program: &[LinearOp]) -> usize {
    program
        .iter()
        .map(|operation| match operation {
            LinearOp::StoreOutput { .. } => 1,
            LinearOp::StoreOutputRange { count, .. } => *count,
            _ => 0,
        })
        .sum()
}

/// One past the largest solver-Y index a program loads.
fn y_extent(program: &[LinearOp]) -> usize {
    program
        .iter()
        .filter_map(|operation| match operation {
            LinearOp::LoadY { index, .. } => Some(index + 1),
            LinearOp::TensorLoad {
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => Some(input_start + count),
            _ => None,
        })
        .max()
        .unwrap_or(0)
}

fn scalar_programs(model: &rumoca_ir_solve::SolveModel) -> impl Iterator<Item = &[LinearOp]> {
    [
        &model.problem.continuous.implicit_rhs.nodes,
        &model.problem.continuous.derivative_rhs.nodes,
    ]
    .into_iter()
    .flatten()
    .filter_map(|node| match node {
        ComputeNode::ScalarPrograms(block) => Some(block),
        _ => None,
    })
    .flat_map(|block| block.programs().iter())
    .map(Vec::as_slice)
}

fn lowered(model: &str, source: &str) -> rumoca_ir_solve::SolveModel {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "elementwise_literal_power.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let opts = SimOptions {
        t_end: 0.001,
        dt: Some(0.0005),
        ..Default::default()
    };
    rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{model} lowers: {error:?}"))
}

/// Operations in the continuous residual and derivative programs.
fn program_operations(model: &rumoca_ir_solve::SolveModel) -> usize {
    [
        &model.problem.continuous.implicit_rhs.nodes,
        &model.problem.continuous.derivative_rhs.nodes,
    ]
    .into_iter()
    .flatten()
    .filter_map(|node| match node {
        ComputeNode::ScalarPrograms(block) => Some(block),
        _ => None,
    })
    .flat_map(|block| block.programs().iter())
    .map(Vec::len)
    .sum()
}

/// The plan of `s .^ n` over a base of thousands of elements is a few packed
/// tensor operations (one shared constant for `n = 0`), not one scalar
/// product or constant per element and step: going
/// from 20 to 2000 elements adds no more operations than it adds to the
/// packed product `s .* s` (SOLVE-C86).
#[test]
fn a_large_base_lowers_in_operations_independent_of_its_extent() {
    let operations = |n: usize, power: &str| {
        program_operations(&lowered(
            "Big",
            &format!(
                "model Big\n  Real s[{n}](each start = 0.5, each fixed = true);\n  Real x[{n}];\n\
                 equation\n  der(s) = -0.01 * x;\n  x = {power};\nend Big;\n"
            ),
        ))
    };
    let product = operations(2000, "s .* s") - operations(20, "s .* s");
    for exponent in [0, -1, 1, 3, 63] {
        let power = format!("s .^ ({exponent})");
        let growth = operations(2000, &power) - operations(20, &power);
        assert!(
            growth <= product,
            "{power}: 1980 more elements add {growth} operations, `s .* s` adds {product}"
        );
    }
}

/// The registers of the continuous programs: the highest register any
/// operation writes, plus one.
fn register_extent(model: &rumoca_ir_solve::SolveModel) -> usize {
    scalar_programs(model)
        .flatten()
        .filter_map(|operation| {
            Some(operation.dst_register()? as usize + operation.dst_register_count())
        })
        .max()
        .unwrap_or(0)
}

/// One `TensorIntegerPower` holds only the registers of its result, so the
/// longest plan, `2^62 - 1` with 122 products (or its reciprocal), takes the
/// registers and operations of `.^ 2`, and no owner: the register budget of an aggregate
/// power no longer depends on its exponent, which the former packed chain
/// multiplied by `1 + steps` (8 526 elements filled its share of the register
/// file, 137 000 the whole file).
#[test]
fn the_longest_plan_takes_the_registers_of_a_square_without_an_owner() {
    let n = 2000;
    let model = |exponent: i64| {
        lowered(
            "Wide",
            &format!(
                "model Wide\n  Real s[{n}](each start = 0.5, each fixed = true);\n  Real x[{n}];\n\
                 equation\n  der(s) = -0.01 * x;\n  x = s .^ ({exponent});\nend Wide;\n"
            ),
        )
    };
    let square = model(2);
    for exponent in [(1i64 << 62) - 1, -((1i64 << 62) - 1)] {
        let longest = model(exponent);
        let powers = scalar_programs(&longest)
            .flatten()
            .filter(|operation| {
                matches!(
                    operation,
                    LinearOp::TensorIntegerPower { count, power, .. }
                        if *count == n && power.exponent() == exponent
                )
            })
            .count();
        assert!(powers >= 1, ".^ {exponent} is one operation per program");
        assert_eq!(
            longest.pure_calls.owners().len(),
            0,
            ".^ {exponent} issues no owner"
        );
        assert_eq!(
            register_extent(&longest),
            register_extent(&square),
            ".^ {exponent} takes the registers of .^ 2"
        );
        assert_eq!(
            program_operations(&longest),
            program_operations(&square),
            ".^ {exponent} takes the operations of .^ 2"
        );
    }
}

/// A literal exponent past the binary64 exact-Integer window is read from
/// the source literal, never from a Real register: an aggregate and a scalar
/// base both lower, with no owner, to the canonical rule's value.
#[test]
fn an_exponent_past_the_exact_integer_window_lowers_from_its_literal() {
    let exponent: i64 = 9_007_199_254_740_993;
    let expected = rumoca_core::modelica_integer_power(-1.0, exponent);
    let cases = [
        (
            "Agg",
            format!(
                "model Agg\n  Real s[2];\n  Real x[2];\nequation\n  \
                 s = {{-1 + 0*time, -1 - 0*time}};\n  x = s .^ {exponent};\nend Agg;\n"
            ),
            "x[1]",
        ),
        (
            "Sca",
            format!(
                "model Sca\n  Real s;\n  Real x;\nequation\n  s = -1 + 0*time;\n  \
                 x = s ^ {exponent};\nend Sca;\n"
            ),
            "x",
        ),
    ];
    for (model, source, output) in cases {
        let (calls, result) = lower(model, &source);
        assert_eq!(calls, 0, "{model}: no pure-call owner");
        for value in column(&result, output) {
            assert_eq!(
                value.to_bits(),
                expected.to_bits(),
                "{model}: {value} vs {expected}"
            );
        }
    }
}

/// In a function body an aggregate base keeps its exact owner, whose one
/// scalar recurrence runs per element, so a large exponent builds and runs
/// like a small one, and each element is the canonical rule's value.
#[test]
fn a_large_aggregate_exponent_in_a_function_body_runs_the_canonical_rule() {
    let bases = [0.999_999, -1.000_000_1];
    for exponent in [65_537_i64, 1_048_577] {
        let source = format!(
            "model Fn\n  function f\n    input Real u[2];\n    output Real y[2];\n  \
             algorithm\n    y := u .^ {exponent};\n  end f;\n  Real x[2];\nequation\n  \
             x = f({{{} + 0*time, {} + 0*time}});\nend Fn;\n",
            bases[0], bases[1]
        );
        let (_, result) = lower("Fn", &source);
        for (index, base) in bases.iter().enumerate() {
            let expected = rumoca_core::modelica_integer_power(*base, exponent);
            for value in column(&result, &format!("x[{}]", index + 1)) {
                assert_eq!(
                    value.to_bits(),
                    expected.to_bits(),
                    "u[{}] .^ {exponent}: {value:e} vs {expected:e}",
                    index + 1
                );
            }
        }
    }
}

/// A zero exponent still evaluates its aggregate base: a call in the base
/// keeps its evaluation and its assertion, as in the scalar spelling and for
/// any other exponent, so each spelling stops at the assertion.
#[test]
fn a_zero_exponent_keeps_the_evaluation_of_its_aggregate_base() {
    let model = |equations: &str| {
        format!(
            "model Zero\n  function f\n    input Real u[2];\n    output Real y[2];\n  \
             algorithm\n    assert(u[1] < 0.5, \"u too large\");\n    y := u;\n  end f;\n  \
             Real x[2];\nequation\n{equations}end Zero;\n"
        )
    };
    let spellings = [
        "  x = f({time, time}) .^ 0;\n",
        "  x = f({time, time}) .^ 2;\n",
        "  x[1] = f({time, time})[1] ^ 0;\n  x[2] = f({time, time})[2] ^ 0;\n",
    ];
    for equations in spellings {
        let source = model(equations);
        let compiled = Compiler::new()
            .model("Zero")
            .compile_str(&source, "elementwise_literal_power.mo")
            .unwrap_or_else(|error| panic!("{equations} compiles: {error:?}"));
        let opts = SimOptions {
            t_end: 1.0,
            dt: Some(0.25),
            ..Default::default()
        };
        let error = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
            .err()
            .unwrap_or_else(|| panic!("{equations}: the assertion stops the simulation"));
        assert!(
            format!("{error:?}").contains("u too large"),
            "{equations}: {error:?}"
        );
    }
}
