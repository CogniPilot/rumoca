use rumoca_ir_solve as solve;

use super::codegen_test_support::builtin_template;

fn render(values: &solve::SolveInitialValues, target: &str) -> String {
    let mut environment = minijinja::Environment::new();
    environment
        .add_template("kernel", builtin_template("fmi3", "scalar_kernel.jinja"))
        .unwrap();
    environment.add_template("start", "{%- from 'kernel' import initial_values %}{{ initial_values(target, owner, 'starts') }}").unwrap();
    environment
        .get_template("start")
        .unwrap()
        .render(minijinja::context! {
            target => target, owner => values,
        })
        .unwrap()
}

#[test]
fn native_repeat_start_render_size_follows_source_runs() {
    let small = solve::SolveInitialValues::repeat(-0.0, 12).unwrap();
    let large = solve::SolveInitialValues::repeat(-0.0, 120_000).unwrap();
    let small_c = render(&small, "values");
    let large_c = render(&large, "values");
    assert!(large_c.len().abs_diff(small_c.len()) <= 4);
    assert_eq!(large_c.matches("for (size_t k").count(), 1);
    assert!(!small.has_dense_view());
    assert!(!large.has_dense_view());
}

#[test]
fn native_start_c_copies_complete_ieee_payloads_and_resets_empty_dummy() {
    let mut values = solve::SolveInitialValues::repeat(-0.0, 12).unwrap();
    values
        .replace(
            4,
            &vec![1.0, f64::from_bits(0x7ff8_0000_0000_1234), 0.0].into(),
        )
        .unwrap();
    let expected = (0..values.len())
        .map(|index| format!("UINT64_C({})", values.value(index).unwrap().to_bits()))
        .collect::<Vec<_>>()
        .join(", ");
    let initialization = render(&values, "values");
    let empty = render(&solve::SolveInitialValues::default(), "dummy");
    let source = format!(
        r#"
#include <stdint.h>
#include <stddef.h>
#include <string.h>
int main(void) {{
    double values[12], dummy[1];
    static const uint64_t expected[] = {{ {expected} }};
    for (int pass=0; pass<2; ++pass) {{
        for (size_t k=0; k<12; ++k) values[k] = -99.0;
        dummy[0] = -99.0;
        {initialization}
        {empty}
        for (size_t k=0; k<12; ++k) {{ uint64_t actual; memcpy(&actual, &values[k], sizeof(actual)); if(actual != expected[k]) return 1; }}
        uint64_t dummy_bits; memcpy(&dummy_bits, dummy, sizeof(dummy_bits)); if(dummy_bits != 0) return 2;
    }}
    return 0;
}}
"#
    );
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("initial.c");
    let binary = directory.path().join("initial");
    std::fs::write(&path, &source).unwrap();
    let result = std::process::Command::new("cc")
        .args([
            "-std=c11",
            "-O2",
            "-fno-fast-math",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-Wvla",
        ])
        .arg(&path)
        .arg("-o")
        .arg(&binary)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}\n{source}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(
        std::process::Command::new(&binary)
            .status()
            .unwrap()
            .success()
    );
    assert!(!values.has_dense_view());
}
