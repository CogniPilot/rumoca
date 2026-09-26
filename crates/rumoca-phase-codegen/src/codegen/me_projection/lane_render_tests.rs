//! The generated C of a tangent-lane program equals the linked evaluator bit
//! for bit on every widened aggregate kind.

use rumoca_ir_solve as solve;
use solve::{BinaryOp, LinearOp, TensorInputKind};

use super::table::LaneFamily;
use crate::codegen::codegen_test_support::builtin_template;

const LANES: usize = 3;
const Y: [f64; 6] = [0.7, -1.3, 2.1, 1.9, -0.4, 0.6];

fn load(dst: u32, input_start: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: dst,
        input: TensorInputKind::Y,
        input_start,
        count: 3,
        seed_start: Some(input_start),
        lanes: 2,
    }
}

fn binary(dst: u32, op: BinaryOp, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::TensorBinary {
        dst_start: dst,
        op,
        lhs_start: lhs,
        rhs_start: rhs,
        count: 3,
        lhs_stride: 1,
        rhs_stride: 1,
        lanes: 2,
    }
}

fn tangents(start: u32, count: usize) -> LinearOp {
    LinearOp::StoreOutputRange {
        start: start + 1,
        count,
        stride: 2,
    }
}

/// A dual JVP over two loaded 3-vectors `a` and `b` exercising every
/// aggregate the C kernel widens: the four tensor binaries, a cross
/// product, a matrix product, and a transpose.
fn dual_program() -> Vec<LinearOp> {
    vec![
        load(0, 0),
        load(6, 3),
        binary(12, BinaryOp::Mul, 0, 6),
        binary(18, BinaryOp::Div, 12, 6),
        binary(24, BinaryOp::Sub, 18, 0),
        binary(30, BinaryOp::Add, 24, 6),
        LinearOp::TensorCross {
            dst_start: 36,
            lhs_start: 0,
            rhs_start: 30,
            lanes: 2,
        },
        LinearOp::MatrixMultiply {
            dst_start: 42,
            lhs_start: 0,
            rhs_start: 36,
            rows: 1,
            inner: 3,
            columns: 1,
            lanes: 2,
        },
        LinearOp::TensorTranspose {
            dst_start: 44,
            src_start: 0,
            rows: 2,
            columns: 3,
            element_width: 1,
            lanes: 2,
        },
        tangents(18, 3),
        tangents(30, 3),
        tangents(36, 3),
        tangents(42, 1),
        tangents(44, 6),
    ]
}

fn seed() -> Vec<f64> {
    (0..Y.len() * LANES)
        .map(|index| 0.25 + 0.5 * index as f64 - 0.03 * (index * index) as f64)
        .collect()
}

const HARNESS: &str = r#"{%- from "fmi-c-kernel.jinja" import scalar_op %}
#include <math.h>
#include <stdio.h>
#include <stddef.h>
typedef struct { const double* y; const double* p; double time; } ModelInstance;
{%- set program = plan.programs[plan.programs|length - 1] %}
static int lane_program(ModelInstance* m, const double* seed, double* o) {
    double r[{{ program.temporary_count }}];
{%- for op in program.ops %}
{{ scalar_op(op, "o", "-1") }}
{%- endfor %}
    return 0;
}
int main(void) {
    static const double y[] = { {{ y | join(", ") }} };
    static const double seed[] = { {{ seed | join(", ") }} };
    double o[{{ outputs }}];
    ModelInstance m = { y, y, 0.0 };
    if (lane_program(&m, seed, o) != 0) return 1;
    for (size_t k = 0; k < {{ outputs }}; ++k) printf("%.17g\n", o[k]);
    return 0;
}
"#;

fn render(program: &solve::TangentLaneProgram, outputs: usize) -> String {
    // The rendered program follows another, so its stores must address
    // its own outputs from zero.
    let mut family = LaneFamily::default();
    let span = solve::source_span_from_offsets(1, 0, 1);
    family.push(program.clone(), span);
    family.push(program.clone(), span);
    let plan = family.into_plan().expect("a checked tangent-lane plan");
    let mut environment = crate::codegen::create_environment();
    environment
        .add_template(
            "fmi-c-kernel.jinja",
            builtin_template("fmi3", "scalar_kernel.jinja"),
        )
        .expect("the shared kernel template parses");
    environment
        .add_template("harness.c", HARNESS)
        .expect("the harness parses");
    let decimal = |values: &[f64]| {
        values
            .iter()
            .map(|value| format!("{value:.17e}"))
            .collect::<Vec<_>>()
    };
    environment
        .get_template("harness.c")
        .expect("the harness is registered")
        .render(minijinja::context! {
            plan => plan,
            y => decimal(&Y),
            seed => decimal(&seed()),
            outputs => outputs,
        })
        .expect("the tangent-lane program renders")
}

fn run_c(source: &str) -> Vec<f64> {
    let directory = tempfile::tempdir().expect("a temporary directory");
    let path = directory.path().join("lanes.c");
    std::fs::write(&path, source).expect("write the harness");
    let binary = directory.path().join("lanes");
    let compile = std::process::Command::new("cc")
        .args([
            "-std=c11",
            "-O2",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-Wvla",
        ])
        .arg(&path)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .expect("start the C compiler");
    assert!(
        compile.status.success(),
        "the tangent-lane C did not compile:\n{}\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = std::process::Command::new(&binary)
        .output()
        .expect("run the harness");
    assert!(run.status.success(), "the tangent-lane C failed");
    String::from_utf8(run.stdout)
        .expect("decimal output")
        .lines()
        .map(|line| line.parse().expect("a decimal value"))
        .collect()
}

#[test]
fn every_widened_aggregate_matches_the_linked_evaluator_bit_for_bit() {
    let program = solve::TangentLaneProgram::replicate(&dual_program(), LANES)
        .expect("the dual program widens");
    let widened = program
        .ops()
        .iter()
        .filter_map(solve::tensor_lanes)
        .filter(|&lanes| lanes == LANES + 1)
        .count();
    assert!(widened >= 9, "every aggregate widens: {widened}");
    let outputs = solve::ScalarProgramBlock::program_output_count(program.ops());
    let mut linked = vec![0.0; outputs];
    let seed = seed();
    let context = rumoca_eval_solve::RowEvalContext {
        seed: Some(&seed),
        ..Default::default()
    };
    rumoca_eval_solve::PreparedTangentLaneProgram::new(program.clone())
        .eval(&Y, &Y, 0.0, context, &mut linked)
        .expect("the linked evaluator runs the lanes");
    let generated = run_c(&render(&program, outputs));
    assert_eq!(
        generated
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>(),
        linked
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>(),
        "generated {generated:?} linked {linked:?}"
    );
}
