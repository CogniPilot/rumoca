//! MSL's inverse uses declaration-initialized LAPACK LU and inverse bodies
//! (MLS §12.4.4, §12.9). The foreign symbol alone cannot prove this interface.

use rumoca::Compiler;
use rumoca_sim::{SimExecutionPolicy, SimOptions, SimResult, simulate_dae};

// The ordered interfaces and declaration bindings are those of pinned MSL
// 4.1.0 Math/package.mo; no foreign library is linked by these tests.
const FUNCTIONS: &str = r#"
  pure function dgetrf
    input Real A[:, :];
    output Real LU[size(A, 1), size(A, 2)] = A;
    output Integer pivots[min(size(A, 1), size(A, 2))];
    output Integer info;
  protected
    Integer m = size(A, 1);
    Integer n = size(A, 2);
    Integer lda = max(1, size(A, 1));
  external "FORTRAN 77" dgetrf(m, n, LU, lda, pivots, info);
  end dgetrf;

  pure function dgetri
    input Real LU[:, size(LU, 1)];
    input Integer pivots[size(LU, 1)];
    output Real inv[size(LU, 1), size(LU, 2)] = LU;
    output Integer info;
  protected
    Integer n = size(LU, 1);
    Integer lda = max(1, size(LU, 1));
    Integer lwork = max(1, min(10, size(LU, 1))*size(LU, 1));
    Real work[max(1, min(10, size(LU, 1))*size(LU, 1))];
  external "FORTRAN 77" dgetri(n, inv, lda, pivots, work, lwork, info);
  end dgetri;
"#;

// MLS §3.8.5 / EXPR-012: continuously varying calls may keep Integer
// temporaries inside a function; a model Integer read by another equation
// cannot acquire a continuous-time definition. All model results here are Real.
const INVERSE_WRAPPER: &str = r#"
  function inverseOf
    input Real A[2,2];
    output Real inverse[2,2];
    output Real info;
    output Real inverseInfo;
  protected
    Real LU[2,2];
    Integer pivots[2];
    Integer luStatus;
    Integer inverseStatus;
  algorithm
    (LU,pivots,luStatus) := dgetrf(A);
    (inverse,inverseStatus) := dgetri(LU,pivots);
    info := luStatus;
    inverseInfo := inverseStatus;
  end inverseOf;
"#;

fn run(declarations: &str, equations: &str) -> SimResult {
    let compiled = compile(declarations, equations);
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("checked LAPACK bodies execute");
    assert_eq!(result.times, vec![0.0, 0.1, 0.2]);
    for values in &result.data {
        assert_eq!(
            values.len(),
            3,
            "every recorded coordinate spans the complete grid"
        );
    }
    result
}

fn compile(declarations: &str, equations: &str) -> rumoca::CompilationResult {
    let source =
        format!("model LUProbe\n{FUNCTIONS}\n{declarations}\nequation\n{equations}\nend LUProbe;");
    Compiler::new()
        .model("LUProbe")
        .compile_str(&source, "native_lu_inverse.mo")
        .unwrap_or_else(|error| panic!("the declared LAPACK interface compiles: {error}"))
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result.names.iter().position(|n| n == name).expect(name);
    &result.data[index]
}

#[test]
fn lu_preserves_multipliers_pivot_order_and_first_singular_step() {
    let result = run(
        "Real LU[2,2]; Integer pivots[2]; Integer info; Real singular[2,2]; Integer sp[2]; Integer si;",
        "(LU,pivots,info) = dgetrf([2,1;4,3]*(1+time));\n(singular,sp,si) = dgetrf([0,1;0,2]);",
    );
    for (sample, time) in result.times.iter().enumerate() {
        let scale = 1.0 + time;
        for (name, expected) in [
            ("LU[1,1]", 4.0 * scale),
            ("LU[1,2]", 3.0 * scale),
            ("LU[2,1]", 0.5),
            ("LU[2,2]", -0.5 * scale),
            ("pivots[1]", 2.0),
            ("pivots[2]", 2.0),
            ("info", 0.0),
            ("si", 1.0),
        ] {
            assert!(
                (column(&result, name)[sample] - expected).abs() < 1e-10,
                "{name} at {time}"
            );
        }
    }
}

#[test]
fn inverse_consumes_the_issued_lu_and_pivot_tuple() {
    let result = run(
        &format!("{INVERSE_WRAPPER}\nReal inverse[2,2]; Real info; Real inverseInfo;"),
        "(inverse,info,inverseInfo) = inverseOf([2,1;4,3]*(1+time));",
    );
    for (sample, time) in result.times.iter().enumerate() {
        for (name, numerator) in [
            ("inverse[1,1]", 1.5),
            ("inverse[1,2]", -0.5),
            ("inverse[2,1]", -2.0),
            ("inverse[2,2]", 1.0),
        ] {
            assert!(
                (column(&result, name)[sample] - numerator / (1.0 + time)).abs() < 1e-10,
                "{name} at {time}"
            );
        }
        assert_eq!(column(&result, "info")[sample], 0.0);
        assert_eq!(column(&result, "inverseInfo")[sample], 0.0);
    }
}

#[test]
fn continuous_model_integer_pivots_are_refused_when_read() {
    // Preserve the original failing fixture as a strict variability control.
    let source = format!(
        "model LUProbe\n{FUNCTIONS}\nReal LU[2,2]; Integer pivots[2]; Integer info; Real inverse[2,2]; Integer inverseInfo;\nequation\n(LU,pivots,info) = dgetrf([2,1;4,3]*(1+time));\n(inverse,inverseInfo) = dgetri(LU,pivots);\nend LUProbe;"
    );
    let error = Compiler::new()
        .model("LUProbe")
        .compile_str(&source, "native_lu_inverse.mo")
        .expect_err("model pivots read by the inverse require discrete-time variability");
    let message = error.to_string();
    assert!(message.contains("discrete-time expression"), "{message}");
    assert!(message.contains("`pivots`"), "{message}");
    assert!(message.contains("continuous-time `time`"), "{message}");
}

#[test]
fn one_by_one_lu_has_the_identity_row_interchange() {
    for (matrix, factor, status) in [("[4]", 4.0, 0.0), ("[0]", 0.0, 1.0)] {
        let result = run(
            "Real LU[1,1]; Integer pivots[1]; Integer info;",
            &format!("(LU,pivots,info) = dgetrf({matrix});"),
        );
        for (name, expected) in [("LU[1,1]", factor), ("pivots[1]", 1.0), ("info", status)] {
            assert!(
                column(&result, name).iter().all(|value| *value == expected),
                "{name}"
            );
        }
    }
}

#[test]
fn rectangular_lu_keeps_lower_rows_and_resolves_pivot_ties_first() {
    let result = run(
        "Real LU[3,2]; Integer pivots[2]; Integer info;",
        "(LU,pivots,info) = dgetrf([2,1;-2,3;1,2]);",
    );
    for (name, expected) in [
        ("LU[1,1]", 2.0),
        ("LU[1,2]", 1.0),
        ("LU[2,1]", -1.0),
        ("LU[2,2]", 4.0),
        ("LU[3,1]", 0.5),
        ("LU[3,2]", 0.375),
        ("pivots[1]", 1.0),
        ("pivots[2]", 2.0),
        ("info", 0.0),
    ] {
        for value in column(&result, name) {
            assert_eq!(*value, expected, "{name}");
        }
    }
}

#[test]
fn singular_inverse_returns_the_unchanged_factors_and_first_zero_diagonal() {
    // DGETRI returns before using pivots when U is singular.
    for pivots in ["{1,2}", "{0,3}"] {
        let result = run(
            "Real inverse[2,2]; Integer info;",
            &format!("(inverse,info) = dgetri([0,1;0,2], {pivots});"),
        );
        for (name, expected) in [
            ("inverse[1,1]", 0.0),
            ("inverse[1,2]", 1.0),
            ("inverse[2,1]", 0.0),
            ("inverse[2,2]", 2.0),
            ("info", 1.0),
        ] {
            assert!(
                column(&result, name).iter().all(|value| *value == expected),
                "{name}"
            );
        }
    }
}

#[test]
fn a_later_singular_step_reports_two_and_preserves_factors_for_inverse() {
    let result = run(
        "Real LU[2,2]; Integer pivots[2]; Integer info; Real inverse[2,2]; Integer inverseInfo;",
        "(LU,pivots,info)=dgetrf([1,2;2,4]);\n(inverse,inverseInfo)=dgetri(LU,pivots);",
    );
    for (name, expected) in [
        ("LU[1,1]", 2.0),
        ("LU[1,2]", 4.0),
        ("LU[2,1]", 0.5),
        ("LU[2,2]", 0.0),
        ("inverse[1,1]", 2.0),
        ("inverse[1,2]", 4.0),
        ("inverse[2,1]", 0.5),
        ("inverse[2,2]", 0.0),
        ("info", 2.0),
        ("inverseInfo", 2.0),
    ] {
        assert!(
            column(&result, name).iter().all(|value| *value == expected),
            "{name}"
        );
    }
}

#[test]
fn later_pivots_swap_stored_multipliers_and_compose_in_source_order() {
    let result = run(
        "Real LU[3,3]; Integer pivots[3]; Integer info; Real inverse[3,3]; Integer inverseInfo;",
        "(LU,pivots,info) = dgetrf([2,1,1;4,3,3;1,5,7]);\n(inverse,inverseInfo)=dgetri(LU,pivots);",
    );
    for (name, expected) in [
        ("LU[1,1]", 4.0),
        ("LU[1,2]", 3.0),
        ("LU[1,3]", 3.0),
        ("LU[2,1]", 0.25),
        ("LU[2,2]", 17.0 / 4.0),
        ("LU[2,3]", 25.0 / 4.0),
        ("LU[3,1]", 0.5),
        ("LU[3,2]", -2.0 / 17.0),
        ("LU[3,3]", 4.0 / 17.0),
        ("pivots[1]", 2.0),
        ("pivots[2]", 3.0),
        ("pivots[3]", 3.0),
        ("inverse[1,1]", 1.5),
        ("inverse[1,2]", -0.5),
        ("inverse[1,3]", 0.0),
        ("inverse[2,1]", -25.0 / 4.0),
        ("inverse[2,2]", 13.0 / 4.0),
        ("inverse[2,3]", -0.5),
        ("inverse[3,1]", 17.0 / 4.0),
        ("inverse[3,2]", -9.0 / 4.0),
        ("inverse[3,3]", 0.5),
        ("info", 0.0),
        ("inverseInfo", 0.0),
    ] {
        assert!(
            column(&result, name)
                .iter()
                .all(|value| (*value - expected).abs() < 1e-10),
            "{name}"
        );
    }
}

#[test]
fn inverse_refuses_pivots_outside_the_proven_matrix_extent() {
    let compiled = compile(
        "input Integer firstPivot(start=1); Real inverse[2,2]; Integer info;",
        "(inverse,info) = dgetri([1,0;0,1], {firstPivot,2});",
    );
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let options = |pivot| SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            initial_inputs: vec![("firstPivot".into(), pivot)],
            execution_policy: policy,
            ..SimOptions::default()
        };
        let valid = simulate_dae(&compiled.dae, &options(1.0))
            .expect("the same runtime pivot input admits the identity inverse");
        assert_eq!(valid.times, [0.0, 0.1, 0.2]);
        for (name, expected) in [
            ("inverse[1,1]", 1.0),
            ("inverse[1,2]", 0.0),
            ("inverse[2,1]", 0.0),
            ("inverse[2,2]", 1.0),
            ("info", 0.0),
        ] {
            assert!(column(&valid, name).iter().all(|value| *value == expected));
        }
        for pivot in [0.0, 3.0] {
            let error = simulate_dae(&compiled.dae, &options(pivot))
                .expect_err("a runtime pivot outside storage cannot produce an inverse");
            assert!(
                error
                    .to_string()
                    .contains("LAPACK dgetri pivot index out of range"),
                "{policy:?}: {error}"
            );
        }
    }
}

#[test]
fn constant_invalid_inverse_pivots_are_refused_before_execution() {
    // Preserve the original literal-input fixture separately: its assertion
    // is reached by constant evaluation, before a runtime call can be issued.
    for pivot in [0, 3] {
        let compiled = compile(
            "Real inverse[2,2]; Integer info;",
            &format!("(inverse,info) = dgetri([1,0;0,1], {{{pivot},2}});"),
        );
        let error = simulate_dae(&compiled.dae, &SimOptions::default())
            .expect_err("invalid constant pivots cannot issue an executable inverse");
        let message = error.to_string();
        assert!(message.contains("[EL005]"), "{message}");
        assert!(
            message.contains("constant call of `dgetri` cannot be evaluated"),
            "{message}"
        );
        assert!(message.contains("function assertion failed"), "{message}");
    }
}

#[test]
fn inverse_does_not_consume_the_unused_final_pivot() {
    for pivots in ["{1,1}", "{1,0}"] {
        let result = run(
            "Real inverse[2,2]; Integer info;",
            &format!("(inverse,info) = dgetri([1,0;0,1], {pivots});"),
        );
        for (name, expected) in [
            ("inverse[1,1]", 1.0),
            ("inverse[1,2]", 0.0),
            ("inverse[2,1]", 0.0),
            ("inverse[2,2]", 1.0),
            ("info", 0.0),
        ] {
            assert!(
                column(&result, name).iter().all(|value| *value == expected),
                "{name}"
            );
        }
    }
    let scalar = run(
        "Real inverse[1,1]; Integer info;",
        "(inverse,info) = dgetri([4], {0});",
    );
    assert!(
        column(&scalar, "inverse[1,1]")
            .iter()
            .all(|value| *value == 0.25)
    );
    assert!(column(&scalar, "info").iter().all(|value| *value == 0.0));
}

#[test]
fn the_foreign_symbol_alone_does_not_admit_a_changed_argument_interface() {
    for (before, after) in [
        (
            "dgetrf(m, n, LU, lda, pivots, info)",
            "dgetrf(m, n, LU, lda, info, pivots)",
        ),
        (
            "Integer lda = max(1, size(A, 1));",
            "Integer lda = max(1, size(A, 1)) + 1;",
        ),
        ("Integer m = size(A, 1);", "Integer m = size(A, 1) + 1;"),
        (
            "output Real LU[size(A, 1), size(A, 2)] = A;",
            "output Real LU[size(A, 1), size(A, 2)] = 2*A;",
        ),
    ] {
        let functions = FUNCTIONS.replace(before, after);
        let source = format!(
            "model BadLU\n{functions}\nReal LU[2,2]; Integer pivots[2]; Integer info;\nequation (LU,pivots,info)=dgetrf([2,1;4,3]);\nend BadLU;"
        );
        let error = Compiler::new()
            .model("BadLU")
            .compile_str(&source, "bad_lu.mo")
            .expect_err("a changed ordered ABI must retain strict foreign admission");
        assert!(
            error.to_string().contains("external function argument"),
            "{error}"
        );
    }
}

#[test]
fn inverse_requires_integer_pivots_and_owned_workspace_and_physical_stride() {
    for (before, after) in [
        (
            "input Integer pivots[size(LU, 1)];",
            "input Real pivots[size(LU, 1)];",
        ),
        (
            "Integer lda = max(1, size(LU, 1));",
            "Integer lda = max(1, size(LU, 1)) + 1;",
        ),
        (
            "Integer lwork = max(1, min(10, size(LU, 1))*size(LU, 1));",
            "Integer lwork = 0;",
        ),
        (
            "Real work[max(1, min(10, size(LU, 1))*size(LU, 1))];",
            "Real work[1];",
        ),
    ] {
        let functions = FUNCTIONS.replace(before, after);
        let source = format!(
            "model BadInverse\n{functions}\nReal inverse[2,2]; Integer info;\nequation (inverse,info)=dgetri([1,0;0,1],{{1,2}});\nend BadInverse;"
        );
        let error = Compiler::new()
            .model("BadInverse")
            .compile_str(&source, "bad_inverse.mo")
            .expect_err("the ordered foreign profile must prove its storage and integer ABI");
        assert!(
            error.to_string().contains("external function argument"),
            "{error}"
        );
    }
}
