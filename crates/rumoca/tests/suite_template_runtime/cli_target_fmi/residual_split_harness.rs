//! The generated C kernel's block residual split falls back to the unsplit
//! residual programs exactly where the linked kernel does (SPEC_0043 §6a
//! block residual split row; SPEC_0044 ME-PROJ-001).
//!
//! The generated `AffinePromote` FMI 3 sources carry an affine multi-row block
//! whose residual programs split into invariant and dependent parts. A C
//! harness includes the component's residual-row unit with its function
//! tables renamed and installs wrapper tables in their place, so each part can
//! be made to fail on demand. It then checks, on the settled start point, that
//! the split residual equals the unsplit residual bit for bit; that a failed
//! invariant part and an exhausted arena both leave the block unsplit with the
//! same residual; that a failed dependent part hands the row to its unsplit
//! program with the same residual; and that when the unsplit program fails too
//! the row reports that program's failure.

use super::*;

/// Wrapper function tables for the residual-row unit's `rmc_row_fn`,
/// `rmc_inv_fn`, and `rmc_dep_fn`, each entry forwarding to the generated
/// program unless its family is forced to fail.
fn wrapper_tables(rows: usize, inv: usize, dep: usize) -> String {
    let mut text = String::from("static int fail_row, fail_inv, fail_dep;\n");
    for k in 0..rows {
        text.push_str(&format!(
            "static int h_row{k}(ModelInstance* m, double* o) {{ return fail_row ? -1 : rmc_row_p{k}(m, o); }}\n"
        ));
    }
    for k in 0..inv {
        text.push_str(&format!(
            "static int h_inv{k}(ModelInstance* m, double* o) {{ return fail_inv ? -1 : rmc_inv_p{k}(m, o); }}\n"
        ));
    }
    for k in 0..dep {
        text.push_str(&format!(
            "static int h_dep{k}(ModelInstance* m, const double* s, double* o) {{ return fail_dep ? -1 : rmc_dep_p{k}(m, s, o); }}\n"
        ));
    }
    let table = |name: &str, kind: &str, prefix: &str, count: usize| {
        let entries = (0..count)
            .map(|k| format!("{prefix}{k}"))
            .collect::<Vec<_>>()
            .join(", ");
        format!("const {kind} {name}[{count}] = {{ {entries} }};\n")
    };
    text.push_str(&table("rmc_row_fn", "RmcProgram", "h_row", rows));
    text.push_str(&table("rmc_inv_fn", "RmcProgram", "h_inv", inv));
    text.push_str(&table("rmc_dep_fn", "RmcJvpProgram", "h_dep", dep));
    text
}

const CHECKS: &str = r#"
#include <stdio.h>
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); return 1; } } while (0)
#define MAX_ROWS 256
static bool same(const double* a, const double* b, size_t n) { return memcmp(a, b, n * sizeof(double)) == 0; }
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    CHECK(m != NULL, "allocate the instance");
    initialize_values(m);
    CHECK(settle_values(m) == fmi3OK, "settle the start point");
    const RmcBlock* b = NULL;
    for (size_t k = 0; k < sizeof(rmc_blocks) / sizeof(rmc_blocks[0]); ++k)
        if (!b && rmc_blocks[k].affine && rmc_blocks[k].n > 1 && rmc_blocks[k].nsplit > 0) b = &rmc_blocks[k];
    CHECK(b != NULL, "an affine multi-row block carries a residual split");
    CHECK(b->n <= MAX_ROWS, "the block fits the harness buffers");
    double unsplit[MAX_ROWS], split[MAX_ROWS];
    size_t top = m->rmc_dtop;
    CHECK(rmc_rows(m, b, unsplit) == 0, "the unsplit residual evaluates");
    const double* inv = rmc_block_invariants(m, b);
    CHECK(inv != NULL, "the invariant parts evaluate");
    CHECK(rmc_rows_split(m, b, inv, split) == 0 && same(split, unsplit, b->n), "the split residual equals the unsplit residual");
    m->rmc_dtop = top;
    fail_inv = 1;
    CHECK(rmc_block_invariants(m, b) == NULL, "a failed invariant part disables the split");
    fail_inv = 0;
    CHECK(rmc_rows_split(m, b, NULL, split) == 0 && same(split, unsplit, b->n), "a disabled split evaluates the unsplit residual");
    m->rmc_dtop = top;
    inv = rmc_block_invariants(m, b);
    CHECK(inv != NULL, "the invariant parts evaluate again");
    fail_dep = 1;
    CHECK(rmc_rows_split(m, b, inv, split) == 0 && same(split, unsplit, b->n), "a failed dependent part hands the row to its unsplit program");
    fail_row = 1;
    CHECK(rmc_rows_split(m, b, inv, split) != 0, "a failed unsplit program reports its own failure");
    fail_dep = 0;
    fail_row = 0;
    m->rmc_dtop = RMC_DWORK;
    CHECK(rmc_block_invariants(m, b) == NULL, "an exhausted arena disables the split");
    m->rmc_dtop = top;
    printf("ok nsplit=%zu n=%zu\n", b->nsplit, b->n);
    free(m);
    return 0;
}
"#;

fn count(source: &str, prefix: &str) -> usize {
    source.matches(prefix).count()
}

#[test]
fn generated_residual_split_falls_back_to_the_unsplit_programs() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    if !headers.join("fmi3Functions.h").is_file() {
        return;
    }
    let compiled = super::affine_promotion::compile();
    let work = tempdir().expect("residual split harness work directory");
    let fmu = build_named_fmu(
        work.path(),
        &compiled,
        "fmi3",
        super::affine_promotion::MODEL,
    );
    let sources = fmu.root.join("sources");
    let rows_unit = fs::read_to_string(sources.join("rmc_rows.c")).expect("read the rows unit");
    let tables = wrapper_tables(
        count(&rows_unit, "static int rmc_row_p"),
        count(&rows_unit, "static int rmc_inv_p"),
        count(&rows_unit, "static int rmc_dep_p"),
    );
    let harness_text = format!(
        "#define rmc_row_fn rmc_row_fn_real\n#define rmc_inv_fn rmc_inv_fn_real\n#define rmc_dep_fn rmc_dep_fn_real\n\
         #include \"rmc_rows.c\"\n#undef rmc_row_fn\n#undef rmc_inv_fn\n#undef rmc_dep_fn\n\
         {tables}#include \"model.c\"\n{CHECKS}"
    );
    let harness = work.path().join("residual_split_harness.c");
    fs::write(&harness, harness_text).expect("write the residual split harness");
    let binary = work.path().join("residual_split_harness");
    checked_output(
        Command::new("cc")
            .arg("-std=c11")
            .arg("-ffp-contract=off")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror")
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(
                [
                    "rmc_assign.c",
                    "rmc_jacobian.c",
                    "rmc_isolators.c",
                    "rmc_functions.c",
                ]
                .map(|unit| sources.join(unit)),
            )
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the residual split harness",
    );
    let output = checked_output(&mut Command::new(&binary), "run the residual split harness");
    assert!(
        String::from_utf8_lossy(&output.stdout).starts_with("ok"),
        "{}",
        String::from_utf8_lossy(&output.stdout)
    );
}
