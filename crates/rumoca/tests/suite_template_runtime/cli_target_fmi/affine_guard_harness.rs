//! The generated torn affine elimination kernel promotes vanished causal
//! pivots to tears in place and solves the reduced system, and its result
//! agrees with an independent dense partial-pivot solve across every promotion
//! path (SPEC_0044 ME-PROJ-001; SPEC_0043 affine elimination row).
//!
//! The generated `AffinePromote` FMI 3 `model.c` carries the shared C kernel;
//! the harness links the component's other translation units beside it.
//! A small C harness includes that source, builds a 4x4 elimination block by
//! copying a real block descriptor and overwriting its fields by name, and runs
//! `rmc_eliminate_prepare`/`rmc_eliminate_solve` against a reference dense solve
//! for six matrices: a triangular order that promotes nothing, a tiny nonzero
//! guard, three guards where one guard's holder is itself promoted, a zero
//! pivot, a weak pivot, and a guard with a weak pivot on the same step. Each
//! case checks that the kernel prepares and solves, promotes the expected
//! number of steps, and matches the dense solution to 1e-12 of max(|x|, 1)
//! relative.

use super::*;

/// C harness compiled against the generated `model.c`. It exercises the shared
/// affine elimination kernel directly, so the promotion arithmetic is checked
/// where it runs rather than only through a full trajectory. Block fields are
/// written by name so a later field on `RmcBlock` does not silently shift a
/// positional initializer.
const HARNESS: &str = r#"#include "model.c"
#include <stdio.h>

/* One 4x4 block: tear column 3 (residual row 3), causal steps (0,0),(1,1),(2,2). */
static const size_t H_ROW_PTR[5] = {0, 4, 8, 12, 16};
static const size_t H_COL[16] = {0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3};
static const size_t H_ELIM_ROW[3] = {0, 1, 2}, H_ELIM_COL[3] = {0, 1, 2};
static const size_t H_RES[1] = {3}, H_TEAR[1] = {3};
/* Guards (0,1), (0,2), (1,2) as pattern positions in H_COL. */
static const size_t H_GUARDS[3] = {1, 2, 6};
static const size_t H_GSTEP[6] = {0, 1, 0, 2, 1, 2};

/* Reference solve of a x = rhs by dense partial-pivot Gaussian elimination. */
static void dense_solve(double a[4][4], double* x) {
    double m[4][5];
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            m[i][j] = a[i][j];
        }
        m[i][4] = x[i];
    }
    for (int i = 0; i < 4; ++i) {
        int pivot = i;
        for (int r = i + 1; r < 4; ++r) {
            if (fabs(m[r][i]) > fabs(m[pivot][i])) {
                pivot = r;
            }
        }
        for (int j = 0; j < 5; ++j) {
            double swap = m[i][j];
            m[i][j] = m[pivot][j];
            m[pivot][j] = swap;
        }
        for (int r = i + 1; r < 4; ++r) {
            double factor = m[r][i] / m[i][i];
            for (int j = i; j < 5; ++j) {
                m[r][j] -= factor * m[i][j];
            }
        }
    }
    for (int i = 3; i >= 0; --i) {
        double sum = m[i][4];
        for (int j = i + 1; j < 4; ++j) {
            sum -= m[i][j] * x[j];
        }
        x[i] = sum / m[i][i];
    }
}

static int run(const char* label, double a[4][4], size_t want_k) {
    ModelInstance* m = calloc(1, sizeof(ModelInstance));
    RmcBlock b = rmc_blocks[0];
    b.n = 4;
    b.row_ptr = H_ROW_PTR;
    b.col_idx = H_COL;
    b.nnz = 16;
    b.elimination = true;
    b.nelim = 3;
    b.elim_row = H_ELIM_ROW;
    b.elim_col = H_ELIM_COL;
    b.nelim_tear = 1;
    b.elim_residual = H_RES;
    b.elim_tear = H_TEAR;
    b.nguards = 3;
    b.guards = H_GUARDS;
    b.guard_step = H_GSTEP;
    b.elim_capacity = 5;

    double jac[16];
    double rscale[4] = {1, 2, 1, 0.5};
    double vscale[4] = {1, 1, 3, 1};
    double residual[4] = {1, -2, 0.5, 3};
    double delta[4];
    for (int r = 0; r < 4; ++r) {
        for (int c = 0; c < 4; ++c) {
            jac[r * 4 + c] = a[r][c];
        }
    }

    RmcElimination e = { .state = -1 };
    int prepared = rmc_eliminate_prepare(m, &b, jac, rscale, vscale, &e);
    int solved = prepared == 1 ? rmc_eliminate_solve(m, &b, &e, residual, rscale, vscale, delta) : -9;

    double reference[4] = {-residual[0], -residual[1], -residual[2], -residual[3]};
    dense_solve(a, reference);
    double error = 0.0;
    for (int i = 0; i < 4 && solved == 1; ++i) {
        error = fmax(error, fabs(delta[i] - reference[i]) / fmax(fabs(reference[i]), 1.0));
    }
    printf("%-28s prepared=%d solved=%d k=%zu (want %zu) promoted=[%zu %zu %zu] maxrel=%.2e\n",
        label, prepared, solved, e.k, want_k, e.promoted[0], e.promoted[1], e.promoted[2], error);
    free(m);
    return prepared == 1 && solved == 1 && e.k == want_k && error < 1e-12 ? 0 : 1;
}

int main(void) {
    int bad = 0;
    double triangular[4][4] = {{4, 0, 0, 1}, {1, 5, 0, 2}, {2, 1, 6, 1}, {1, 2, 3, 7}};
    bad |= run("triangular (no promotion)", triangular, 1);
    double tiny_guard[4][4] = {{4, 1e-300, 0, 1}, {1, 5, 0, 2}, {2, 1, 6, 1}, {1, 2, 3, 7}};
    bad |= run("tiny nonzero guard (0,1)", tiny_guard, 2);
    double three_guards[4][4] = {{4, 0.5, 0.25, 1}, {1, 5, 3, 2}, {2, 1, 6, 1}, {1, 2, 3, 7}};
    bad |= run("guards (0,1),(0,2),(1,2)", three_guards, 3);
    double zero_pivot[4][4] = {{4, 0, 0, 1}, {1, 0, 0, 2}, {2, 1, 6, 1}, {1, 2, 3, 7}};
    bad |= run("zero pivot step1 in place", zero_pivot, 2);
    double weak_pivot[4][4] = {{4, 0, 0, 1}, {1, 1e-9, 0, 2}, {2, 1, 6, 1}, {1, 2, 3, 7}};
    bad |= run("weak pivot 1e-9 in place", weak_pivot, 2);
    double guard_weak[4][4] = {{4, 0, 0, 1}, {1, 5, 1, 2}, {2, 1, 1e-12, 1}, {1, 2, 3, 7}};
    bad |= run("guard (1,2)+weak step2", guard_weak, 2);
    return bad;
}
"#;

/// The FMI 3 headers the generated `model.c` includes, from the pinned FMI
/// standards tree.
fn fmi3_header_directory() -> Option<PathBuf> {
    let headers = standard_roots().1.root.join("headers");
    headers.join("fmi3Functions.h").is_file().then_some(headers)
}

#[test]
fn generated_affine_elimination_kernel_matches_a_dense_solve_across_promotion_paths() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let Some(headers) = fmi3_header_directory() else {
        return;
    };
    let compiled = super::affine_promotion::compile();
    let work = tempdir().expect("affine guard harness work directory");
    let fmu = build_named_fmu(
        work.path(),
        &compiled,
        "fmi3",
        super::affine_promotion::MODEL,
    );
    let sources = fmu.root.join("sources");
    let harness = work.path().join("affine_guard_harness.c");
    fs::write(&harness, HARNESS).expect("write the affine guard harness source");
    let binary = work.path().join("affine_guard_harness");
    checked_output(
        Command::new("cc")
            .arg("-std=c11")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror")
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(
                [
                    "rmc_assign.c",
                    "rmc_rows.c",
                    "rmc_jacobian.c",
                    "rmc_isolators.c",
                    "rmc_functions.c",
                ]
                .map(|unit| sources.join(unit)),
            )
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the affine elimination guard harness",
    );
    checked_output(
        &mut Command::new(&binary),
        "run the affine elimination guard harness",
    );
}
