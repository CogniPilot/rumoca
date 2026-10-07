use super::*;

/// Operand pairs covering a signaling and a quiet NaN on either or both
/// sides, signed-zero ties, infinities and ordinary values.
fn cases() -> Vec<(f64, f64)> {
    let signaling = f64::from_bits(0x7ff0_0000_0000_0001);
    let values = [
        signaling,
        f64::NAN,
        -0.0,
        0.0,
        -1.5,
        2.0,
        f64::INFINITY,
        f64::NEG_INFINITY,
    ];
    values
        .iter()
        .flat_map(|&a| values.iter().map(move |&b| (a, b)))
        .collect()
}

/// Bits that agree with `real_extremum`: every NaN is one class (its payload
/// is not part of the rule), every other value compares bit for bit.
fn same(actual: f64, expected: f64) -> bool {
    (actual.is_nan() && expected.is_nan()) || actual.to_bits() == expected.to_bits()
}

#[test]
fn the_rendered_c_helpers_compute_real_extremum_on_every_nan_case() {
    let cases = cases();
    let mut source = String::from(
        "#include <math.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <string.h>\n",
    );
    source.push_str(&prelude("c").unwrap());
    source.push_str(
        "static double value(uint64_t bits) { double out; memcpy(&out, &bits, 8); return out; }\n",
    );
    source.push_str("static uint64_t bits(double value) { uint64_t out; memcpy(&out, &value, 8); return out; }\n");
    source.push_str("int main(void) {\n");
    for (a, b) in &cases {
        for name in ["rumoca_real_min", "rumoca_real_max"] {
            source.push_str(&format!(
                "    printf(\"%llu\\n\", (unsigned long long)bits({name}(value({}ULL), value({}ULL))));\n",
                a.to_bits(),
                b.to_bits()
            ));
        }
    }
    source.push_str("    return 0;\n}\n");
    let directory = tempfile::tempdir().expect("a temporary directory");
    let path = directory.path().join("extremum.c");
    std::fs::write(&path, &source).expect("write the harness");
    let binary = directory.path().join("extremum");
    let compile = std::process::Command::new("cc")
        .args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror"])
        .arg(&path)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .expect("start the C compiler");
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = std::process::Command::new(&binary)
        .output()
        .expect("run the harness");
    let actual = String::from_utf8(run.stdout)
        .unwrap()
        .lines()
        .map(|line| f64::from_bits(line.parse().unwrap()))
        .collect::<Vec<_>>();
    let expected = cases
        .iter()
        .flat_map(|&(a, b)| {
            [
                rumoca_ir_solve::real_extremum(a, b, true),
                rumoca_ir_solve::real_extremum(a, b, false),
            ]
        })
        .collect::<Vec<_>>();
    assert_eq!(actual.len(), expected.len());
    for (index, (actual, expected)) in actual.iter().zip(&expected).enumerate() {
        assert!(
            same(*actual, *expected),
            "case {index}: {:?} -> {actual} (bits {:x}), expected {expected}",
            cases[index / 2],
            actual.to_bits()
        );
    }
}

#[test]
fn every_language_prints_the_same_rule() {
    // The helpers of every language are printed from one rule; each names
    // both helpers and the both-NaN sum, and MLIR prints it as SSA.
    for language in ["c", "cuda", "rust", "jax", "casadi", "wgsl", "mlir"] {
        let text = prelude(language).unwrap();
        assert!(text.contains("rumoca_real_min") && text.contains("rumoca_real_max"));
        assert!(
            text.contains("(a + b)") || text.contains("arith.addf %a, %b"),
            "{language}: {text}"
        );
    }
    assert!(prelude("fortran").is_none());
    assert_eq!(
        mlir_call(true, "%d", "%x", "%y"),
        "%d = func.call @rumoca_real_min(%x, %y) : (f64, f64) -> f64"
    );
}

/// A C translation unit defines the helpers only when its problem applies a
/// Real `min` or `max`; an unused `static inline` definition is an error
/// under `-Werror` on clang, which the generated C must compile under.
#[test]
fn the_c_helpers_are_declared_only_for_a_problem_that_applies_min_or_max() {
    use crate::codegen::codegen_test_support::{builtin_template, derivative_problem};
    use rumoca_ir_solve::{BinaryOp, LinearOp, SolveArtifacts};

    let render = |program: Vec<LinearOp>| {
        crate::codegen::render_solve_template_with_name(
            &derivative_problem(program),
            &SolveArtifacts::default(),
            builtin_template("c-ode", "model_ode.c.jinja"),
            "Extremum",
        )
        .expect("the C target renders")
    };
    let operands = || {
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
        ]
    };
    let binary = |op: BinaryOp| {
        let mut program = operands();
        program.push(LinearOp::Binary {
            dst: 2,
            op,
            lhs: 0,
            rhs: 1,
        });
        program.push(LinearOp::StoreOutput { src: 2 });
        program
    };

    let without = render(binary(BinaryOp::Add));
    assert!(!without.contains(helper_name(true)), "{without}");
    assert!(!without.contains(helper_name(false)), "{without}");
    for op in [BinaryOp::Min, BinaryOp::Max] {
        let with = render(binary(op));
        assert!(
            with.contains(&format!("static inline double {}(", helper_name(true)))
                && with.contains(&format!("static inline double {}(", helper_name(false))),
            "{with}"
        );
    }
}
