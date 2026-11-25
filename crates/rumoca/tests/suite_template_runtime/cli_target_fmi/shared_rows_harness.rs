//! Rows whose packed slices differ only in their load indices share one row
//! function, which reads those indices from a per-row table. Every row of
//! every block, shared or with its own function, must still return the
//! value the linked evaluator computes for its residual output, bit for bit.

use super::*;
use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};

/// The checks: settle the start point, then print the coordinate and every
/// row of every projection block as the generated kernel evaluates it.
const HARNESS: &str = r#"#include "model.c"
#include <stdio.h>
#include <inttypes.h>
static uint64_t bits(double value) { uint64_t out; memcpy(&out, &value, sizeof out); return out; }
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    if (!m) return 1;
    initialize_values(m);
    if (settle_values(m) != fmi3OK) { printf("settle failed\n"); return 1; }
    printf("t %" PRIu64 "\n", bits(m->time));
    printf("y"); for (size_t k = 0; k < Y_LEN; ++k) printf(" %" PRIu64, bits(m->y[k])); printf("\n");
    printf("p"); for (size_t k = 0; k < P_LEN; ++k) printf(" %" PRIu64, bits(m->p[k])); printf("\n");
    for (size_t id = 0; id < sizeof(rmc_block_desc) / sizeof(rmc_block_desc[0]); ++id) {
        const RmcBlock* b = rmc_block(id);
        for (size_t row = 0; row < RMC_BS(b, n); ++row) {
            double value = 0.0;
            int status = rmc_row(m, b, row, &value);
            printf("row %zu %zu %d %" PRIu64 "\n", RMC_BS(b, canonical), row, status, bits(value));
        }
    }
    free(m);
    return 0;
}
"#;

fn values(stdout: &str, key: &str) -> Vec<f64> {
    stdout
        .lines()
        .find_map(|line| line.strip_prefix(key))
        .unwrap_or_else(|| panic!("no {key:?} line in {stdout}"))
        .split_whitespace()
        .map(|word| f64::from_bits(word.parse().expect("a bit pattern")))
        .collect()
}

/// Compile `model`, check its rows unit shares rows (and keeps own rows when
/// `own` is set), run the harness, and compare every row with the linked
/// evaluator.
fn check_rows(source: &str, file: &str, model: &str, own: bool) {
    let headers = standard_roots().1.root.join("headers");
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, file)
        .unwrap_or_else(|error| panic!("compile {model}: {error:?}"));
    let work = tempdir().expect("shared rows harness work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", model);
    let sources = fmu.root.join("sources");
    let rows = fs::read_to_string(sources.join("rmc_rows.c")).expect("read the rows unit");
    assert!(
        rows.contains("int rmc_row_s0("),
        "{model}: no shared row function"
    );
    assert_eq!(
        own,
        rows.contains("int rmc_row_p"),
        "{model}: rows with their own function"
    );
    let harness = work.path().join("shared_rows.c");
    fs::write(&harness, HARNESS).expect("write the harness");
    let units = [
        "rmc_rows.c",
        "rmc_jacobian.c",
        "rmc_isolators.c",
        "rmc_assign.c",
        "rmc_functions.c",
    ];
    let binary = work.path().join("shared_rows");
    checked_output(
        Command::new("cc")
            .args([
                "-std=c11",
                "-ffp-contract=off",
                "-Wall",
                "-Wextra",
                "-Werror",
            ])
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(units.iter().map(|unit| sources.join(unit)))
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the shared rows harness",
    );
    let output = checked_output(&mut Command::new(&binary), "run the shared rows harness");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let (y, p) = (values(&stdout, "y"), values(&stdout, "p"));
    let t = values(&stdout, "t")[0];
    let component = rumoca_sim::lower_fmi_export_component(&compiled.dae)
        .unwrap_or_else(|error| panic!("lower {model}: {error:?}"));
    let problem = component.problem();
    let implicit = PreparedScalarProgramBlock::new(
        rumoca_eval_solve::to_scalar_program_projection(&problem.continuous.implicit_rhs)
            .expect("scalarize the implicit residual")
            .into_block(),
    )
    .expect("prepare the implicit residual");
    let blocks = problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks();
    let mut compared = 0;
    for line in stdout.lines().filter_map(|line| line.strip_prefix("row ")) {
        let fields: Vec<u64> = line
            .split_whitespace()
            .map(|word| word.parse().expect("a number"))
            .collect();
        let (canonical, row, status, generated) =
            (fields[0] as usize, fields[1] as usize, fields[2], fields[3]);
        assert_eq!(status, 0, "{model}: row {row} of block {canonical} fails");
        let output = blocks[canonical].rows[row];
        let (program, offset) = implicit
            .row_output_position(output)
            .expect("a residual row with a scalar view");
        let linked = implicit
            .eval_row_output_unchecked_with_context(
                program,
                offset,
                &y,
                &p,
                t,
                RowEvalContext::default(),
            )
            .expect("evaluate the linked row");
        assert_eq!(
            generated,
            linked.to_bits(),
            "{model}: row {row} of block {canonical}"
        );
        compared += 1;
    }
    assert!(compared > 0, "{model}: no row compared");
}

/// Every row of an array equation whose rows all share one function, and
/// every row of a ladder whose cells share and whose ends keep their own
/// functions, returns the linked value bit for bit.
#[test]
fn shared_and_own_rows_return_the_linked_values() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let array_loop = "model ArrayLoop\n  Real x[8](each start = 1, each fixed = true);\n  Real u[8](each start = 0.5);\nequation\n  der(x) = -u;\n  u + 0.1*u.*u.*u = x;\nend ArrayLoop;\n";
    check_rows(array_loop, "ArrayLoop.mo", "ArrayLoop", false);
    // The ladder under an undotted name, which names its generated sources.
    let ladder = format!(
        "{}\nmodel LadderRows\n  extends PowerLadder.Ladder;\nend LadderRows;\n",
        include_str!("../../fixtures/elementwise_literal_power/PowerLadder.mo")
    );
    check_rows(&ladder, "LadderRows.mo", "LadderRows", true);
}
