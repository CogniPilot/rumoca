//! Compact native stores preserve complete target mapping and first faults.

use std::sync::Arc;

use rumoca_ir_solve as solve;

use super::codegen_test_support::builtin_template;
use super::scalar_program_plan::ScalarProgramPlan;

const HARNESS: &str = r#"
{%- from "me.jinja" import assignment_functions %}
#include <math.h>
#include <stddef.h>
#define RMC_API
typedef struct { double y[{{ y_count }}]; double p[{{ count }}]; double time; } ModelInstance;
{{ assignment_functions(me) }}
int main(void) {
    ModelInstance m = {0};
    for (size_t k=0; k<{{ y_count }}; ++k) m.y[k] = -99.0;
    for (size_t k=0; k<{{ count }}; ++k) m.p[k] = (double)k + 0.5;
    if (rmc_assign(&m, 0, 1) != 0) return 1;
{% for target in targets %}
    if (m.y[{{ target }}] != {{ loop.index0 }}.5) return 2;
{% endfor %}
    for (size_t k=0; k<{{ y_count }}; ++k) m.y[k] = -99.0;
    m.p[1] = NAN;
    if (rmc_assign(&m, 0, 1) != 1) return 3;
    if (m.y[{{ targets[0] }}] != 0.5 || !isnan(m.y[{{ targets[1] }}])) return 4;
{% for target in targets[2:] %}
    if (m.y[{{ target }}] != -99.0) return 5;
{% endfor %}
    return 0;
}
"#;

fn render(count: usize, targets: &[usize], with_harness: bool) -> String {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("NativeRange.mo"),
        1,
        2,
    );
    let block = solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count,
                stride: 1,
            },
        ]],
        vec![span],
        targets.to_vec(),
    )
    .unwrap();
    let plan = ScalarProgramPlan::new(Arc::new(block)).unwrap();
    let mut environment = super::create_environment();
    environment
        .add_template(
            "fmi-c-kernel.jinja",
            builtin_template("fmi3", "scalar_kernel.jinja"),
        )
        .unwrap();
    environment
        .add_template("me.jinja", builtin_template("fmi3", "me_projection.jinja"))
        .unwrap();
    environment
        .add_template(
            "control.c",
            if with_harness {
                HARNESS
            } else {
                "{% from 'me.jinja' import assignment_functions %}{{ assignment_functions(me) }}"
            },
        )
        .unwrap();
    environment
        .get_template("control.c")
        .unwrap()
        .render(minijinja::context! {
            me => minijinja::context! { assign => minijinja::context! {
                plan => minijinja::Value::from_object(plan), sequence => vec![0usize],
            } },
            y_count => targets.iter().copied().max().unwrap() + 1,
            targets => targets,
            count => count,
        })
        .unwrap()
}

#[test]
fn native_range_emitted_size_follows_operations_not_tensor_extent() {
    let small = render(12, &(3..27).step_by(2).collect::<Vec<_>>(), false);
    let large = render(120, &(3..243).step_by(2).collect::<Vec<_>>(), false);
    assert!(
        large.len() <= small.len() + 16,
        "{} vs {} bytes",
        large.len(),
        small.len()
    );
    assert!(large.contains("k<120"));
    assert_eq!(large.matches("if (!isfinite(m->y[").count(), 1);
}

#[test]
fn native_range_c_preserves_strided_descending_and_irregular_first_faults() {
    for targets in [vec![3, 5, 7], vec![7, 5, 3], vec![3, 5, 8]] {
        let source = render(3, &targets, true);
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("range.c");
        let binary = directory.path().join("range");
        std::fs::write(&path, &source).unwrap();
        let compiled = std::process::Command::new("cc")
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
            .unwrap();
        assert!(
            compiled.status.success(),
            "{}\n{source}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        assert!(
            std::process::Command::new(&binary)
                .status()
                .unwrap()
                .success()
        );
    }
}
