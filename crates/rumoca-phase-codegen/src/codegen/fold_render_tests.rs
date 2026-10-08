//! The FMI C kernel runs compact function folds exactly as the linked
//! evaluator does, bit for bit, including guarded, nested and aggregate-update
//! folds and runtime-indexed register loads.

use std::sync::Arc;

use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve as solve;
use solve::{BinaryOp, CompareOp, LinearOp, TensorIndex, TensorUpdateSubscript};

use crate::codegen::codegen_test_support::builtin_template;

const Y: [f64; 4] = [0.75, 1.0, 2.0, 0.0];

fn domain(upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

fn binary(dst: u32, op: BinaryOp, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::Binary { dst, op, lhs, rhs }
}

/// `sum += i*scale; product *= i` over `i in 1:4`.
fn accumulate() -> Arc<solve::FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCarried { dst: 1, index: 1 },
        LinearOp::LoadFoldIndex {
            dst: 2,
            dimension: 0,
        },
        LinearOp::LoadFoldCapture { dst: 3, index: 0 },
        binary(4, BinaryOp::Mul, 2, 3),
        binary(5, BinaryOp::Add, 0, 4),
        binary(6, BinaryOp::Mul, 1, 2),
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
    ];
    Arc::new(solve::FunctionFoldProgram::checked(domain(4), 2, 1, update).expect("checked fold"))
}

/// The inner fold of [`outer_with_nested`]: `a += j*c; b -= a` over `j in 1:2`.
fn inner() -> Arc<solve::FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCarried { dst: 1, index: 1 },
        LinearOp::LoadFoldIndex {
            dst: 2,
            dimension: 0,
        },
        LinearOp::LoadFoldCapture { dst: 3, index: 0 },
        binary(4, BinaryOp::Mul, 2, 3),
        binary(5, BinaryOp::Add, 0, 4),
        binary(6, BinaryOp::Sub, 1, 5),
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
    ];
    Arc::new(solve::FunctionFoldProgram::checked(domain(2), 2, 1, update).expect("checked fold"))
}

/// An outer fold whose update is a nested fold seeded from the parent's
/// carried tuple, optionally only where `i > 2`.
fn outer_with_nested(conditional: bool) -> Arc<solve::FunctionFoldProgram> {
    let mut update = vec![
        LinearOp::LoadFoldIndex {
            dst: 0,
            dimension: 0,
        },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
    ];
    update.push(LinearOp::StoreOutputFunctionFold {
        initial: vec![solve::FoldInitialSource::ParentCarried { base: 0, count: 2 }].into(),
        capture_start: 0,
        program: inner(),
        result_base: 0,
        count: 2,
        condition: conditional.then_some(2),
        nested_when_true: true,
    });
    Arc::new(solve::FunctionFoldProgram::checked(domain(4), 2, 0, update).expect("checked fold"))
}

/// `v[i] := i*scale` for `i in 1:3` over a carried 3-vector.
fn tensor_update() -> Arc<solve::FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldIndex {
            dst: 0,
            dimension: 0,
        },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        binary(2, BinaryOp::Mul, 0, 1),
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base: 0,
            source_stride: 1,
            dimensions: vec![3].into(),
            updates: vec![solve::FoldTensorUpdate {
                subscripts: vec![TensorUpdateSubscript::Index(TensorIndex::Runtime(0))].into(),
                condition: None,
                value_start: 2,
                value_stride: 1,
            }]
            .into(),
            nodes: vec![solve::FoldTensorNode::Update { base: 0, update: 0 }].into(),
            result: 1,
            lanes: 1,
        },
    ];
    Arc::new(solve::FunctionFoldProgram::checked(domain(3), 3, 1, update).expect("checked fold"))
}

/// `while x > 0.1 loop x := x*0.5; end while` as a fold over `1:100` that ends
/// at its first false continuation.
fn halving() -> Arc<solve::FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.5 },
        binary(2, BinaryOp::Mul, 0, 1),
        LinearOp::StoreOutput { src: 2 },
    ];
    let continuation = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.1 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    Arc::new(
        solve::FunctionFoldProgram::checked(domain(100), 1, 0, update)
            .and_then(|program| program.with_continuation(continuation))
            .expect("checked continued fold"),
    )
}

fn program() -> Vec<LinearOp> {
    let fold = |dst, program, initial_start| LinearOp::FunctionFold {
        dst_start: dst,
        initial_start,
        capture_start: 0,
        program,
    };
    let guarded = |dst, activation| LinearOp::GuardedFunctionFold {
        dst_start: dst,
        initial_start: 1,
        capture_start: 0,
        activation,
        program: accumulate(),
    };
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::LoadY { dst: 3, index: 1 },
        LinearOp::LoadY { dst: 4, index: 3 },
        LinearOp::Const {
            dst: 5,
            value: -1.0,
        },
        fold(6, accumulate(), 1),
        guarded(8, 3),
        guarded(10, 4),
        fold(12, outer_with_nested(false), 1),
        fold(14, outer_with_nested(true), 1),
        fold(16, tensor_update(), 1),
        LinearOp::LoadY { dst: 19, index: 2 },
        LinearOp::LoadIndexedRegister {
            dst: 20,
            base: 16,
            stride: 1,
            dimensions: vec![3].into(),
            indices: vec![TensorIndex::Runtime(19)].into(),
        },
        fold(21, halving(), 3),
        LinearOp::StoreOutputRange {
            start: 6,
            count: 16,
            stride: 1,
        },
    ]
}

const HARNESS: &str = r#"{%- from "fmi-c-kernel.jinja" import scalar_op %}
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
typedef struct { const double* y; const double* p; double time; } ModelInstance;
{%- set program = plan.programs[0] %}
static int fold_program(ModelInstance* m, double* o) {
    double r[{{ program.temporary_count }}];
{%- for op in program.ops %}
{{ scalar_op(op, "o", "-1") }}
{%- endfor %}
    return 0;
}
int main(void) {
    static const double y[] = { {{ y | join(", ") }} };
    double o[{{ outputs }}];
    ModelInstance m = { y, y, 0.0 };
    if (fold_program(&m, o) != 0) return 1;
    for (size_t k = 0; k < {{ outputs }}; ++k) printf("%.17g\n", o[k]);
    return 0;
}
"#;

fn block() -> Arc<solve::ScalarProgramBlock> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fold_render_fixture.mo"),
        1,
        2,
    )
    .require_provenance("fold render fixture")
    .expect("fixture span is source-backed");
    Arc::new(
        solve::ScalarProgramBlock::with_source_span(vec![program()], span)
            .expect("fixture fold program is checked"),
    )
}

fn render(block: Arc<solve::ScalarProgramBlock>, outputs: usize) -> String {
    let plan = super::scalar_program_plan::ScalarProgramPlan::new(block).expect("a scalar plan");
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
    environment
        .get_template("harness.c")
        .expect("the harness is registered")
        .render(minijinja::context! {
            plan => minijinja::Value::from_object(plan),
            y => Y.iter().map(|value| format!("{value:.17e}")).collect::<Vec<_>>(),
            outputs => outputs,
        })
        .expect("the fold program renders")
}

fn run_c(source: &str) -> Vec<f64> {
    let directory = tempfile::tempdir().expect("a temporary directory");
    let path = directory.path().join("folds.c");
    std::fs::write(&path, source).expect("write the harness");
    let binary = directory.path().join("folds");
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
        "the fold C did not compile:\n{}\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = std::process::Command::new(&binary)
        .output()
        .expect("run the harness");
    assert!(run.status.success(), "the fold C failed");
    String::from_utf8(run.stdout)
        .expect("decimal output")
        .lines()
        .map(|line| line.parse().expect("a decimal value"))
        .collect()
}

#[test]
fn every_fold_form_matches_the_linked_evaluator_bit_for_bit() {
    let block = block();
    let outputs = block.output_count();
    let mut expected = vec![0.0; outputs];
    rumoca_eval_solve::eval_scalar_program_block(&block, &Y, &[], 0.0, None, &mut expected)
        .expect("the linked evaluator runs every fold form");
    let actual = run_c(&render(block, outputs));
    assert_eq!(
        actual
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>(),
        expected
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>(),
        "C {actual:?} vs evaluator {expected:?}"
    );
    // Guards: the inactive fold keeps its seed; the active one folds.
    assert_eq!(expected[2..4], expected[0..2]);
    assert_eq!(expected[4..6], [0.5, 1.0]);
    // The continued fold stops after four halvings of 1.0.
    assert_eq!(expected[15], 0.0625);
}

/// `a[s:s + 1] := {100, 200}` for `s in 1:2` over a carried 4-vector: the
/// window is the coordinate registers `{s, s + 1}`.
fn window_update() -> Arc<solve::FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldIndex {
            dst: 0,
            dimension: 0,
        },
        LinearOp::Const { dst: 2, value: 1.0 },
        binary(1, BinaryOp::Add, 0, 2),
        LinearOp::Const {
            dst: 3,
            value: 100.0,
        },
        LinearOp::Const {
            dst: 4,
            value: 200.0,
        },
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base: 0,
            source_stride: 1,
            dimensions: vec![4].into(),
            updates: vec![solve::FoldTensorUpdate {
                subscripts: vec![TensorUpdateSubscript::Slice {
                    start: 0,
                    dimensions: vec![2].into(),
                }]
                .into(),
                condition: None,
                value_start: 3,
                value_stride: 1,
            }]
            .into(),
            nodes: vec![solve::FoldTensorNode::Update { base: 0, update: 0 }].into(),
            result: 1,
            lanes: 1,
        },
    ];
    Arc::new(solve::FunctionFoldProgram::checked(domain(2), 4, 0, update).expect("checked fold"))
}

#[test]
fn a_run_time_window_patch_matches_the_linked_evaluator_bit_for_bit() {
    let operations = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::LoadY { dst: 3, index: 3 },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: window_update(),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 4,
            stride: 1,
        },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fold_render_fixture.mo"),
        1,
        2,
    )
    .require_provenance("fold render fixture")
    .expect("fixture span is source-backed");
    let block = Arc::new(
        solve::ScalarProgramBlock::with_source_span(vec![operations], span)
            .expect("fixture window program is checked"),
    );
    let mut expected = vec![0.0; 4];
    rumoca_eval_solve::eval_scalar_program_block(&block, &Y, &[], 0.0, None, &mut expected)
        .expect("the linked evaluator runs the window patch");
    // The carried vector starts as Y = {0.75, 1, 2, 0}; start 1 writes {100, 200}
    // at a[1:2] and start 2 writes them at a[2:3].
    assert_eq!(expected, [100.0, 100.0, 200.0, 0.0]);
    let actual = run_c(&render(block, 4));
    assert_eq!(actual, expected, "C {actual:?} vs evaluator {expected:?}");
}
