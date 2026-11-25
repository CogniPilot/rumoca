//! The projection arena holds every frame chain the kernel opens: a refresh,
//! the initialization frames around it, a completed-step decision, which
//! keeps the coordinate while it refreshes and reads the conditioning, and a
//! chart transfer, which keeps the coordinate and its row outputs while it
//! refreshes the target chart.
//!
//! The harness compiles its own copy of the generated sources, in which the
//! double allocator also records the highest top it reaches: the doubles a
//! chain allocates, written or not. Every chain must fit the generated arena.
//! The same chains then run in an arena of exactly the highest peak, where
//! all succeed, and of one double less, where the chain that reached the peak
//! reports an exhausted workspace and fails with its frames released. The
//! generated sources themselves are not changed.

use super::*;

/// The checks: settle the start point, refresh, and (for a switching
/// component) decide completed steps and transfer to the chart one latches,
/// a transfer from a non-finite coordinate failing first. Each call is one
/// chain, reported with its status, the doubles it allocated, whether it
/// reported an exhausted workspace, and whether it released its frames; the
/// run stops at the first chain that does not answer as expected.
const CHECKS: &str = r#"
#include <stdio.h>
static int exhausted;
static void capture(fmi3InstanceEnvironment environment, fmi3Status status, fmi3String category, fmi3String message) {
    (void)environment; (void)status; (void)category;
    if (strstr(message, "workspace exhausted")) exhausted += 1;
}
static int chain(const ModelInstance* m, const char* name, fmi3Status status, fmi3Status expected) {
    printf("chain %s status=%d peak=%zu exhausted=%d released=%d\n", name, (int)status, arena_peak, exhausted, m->rmc_dtop == 0 && m->rmc_stop == 0);
    arena_peak = 0; exhausted = 0;
    return status == expected;
}
#define CHAIN(name, call, expected) do { if (!chain(m, name, (call), (expected))) goto done; } while (0)
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    if (!m) return 1;
    initialize_values(m);
    m->logger = capture;
    CHAIN("settle", settle_values(m), fmi3OK);
    CHAIN("refresh", refresh_algebraics(m), fmi3OK);
#ifdef RMC_NCHARTS
    /* Round the circle, an unreachable keep reference makes a completed step
       latch the other chart where it is regular, and the transfer runs to
       it. */
    bool request = false, changed = false, nominals = false;
    for (int step = 1; step < 100 && !request; ++step) {
        m->y[0] = (double)step / 100.0;
        CHAIN("refresh-circle", refresh_algebraics(m), fmi3OK);
        m->rmc_chart_reference = 1e300;
        CHAIN("decide", rmc_chart_completed(m, &request), fmi3OK);
    }
    if (!request) { printf("no switch requested\n"); goto done; }
    double latched[Y_LEN]; memcpy(latched, m->rmc_chart_y, sizeof(latched));
    for (size_t k = 0; k < Y_LEN; ++k) m->rmc_chart_y[k] = NAN;
    CHAIN("transfer-non-finite", rmc_chart_transfer(m, &changed, &nominals), fmi3Error);
    memcpy(m->rmc_chart_y, latched, sizeof(latched)); m->rmc_chart_pending = true;
    CHAIN("transfer", rmc_chart_transfer(m, &changed, &nominals), fmi3OK);
#endif
    printf("complete\n");
done:
    printf("arena %d\n", RMC_DWORK);
    free(m);
    return 0;
}
"#;

/// The allocation of the double arena in the generated `model.c`, in both
/// its zeroing and its raw form.
const ALLOCATION: &str = "m->rmc_dtop += count;";

fn headers() -> Option<PathBuf> {
    if !conformance_prerequisites_are_available() {
        return None;
    }
    let headers = standard_roots().1.root.join("headers");
    headers.join("fmi3Functions.h").is_file().then_some(headers)
}

/// One reported chain.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Chain {
    name: String,
    status: i32,
    peak: usize,
    exhausted: bool,
    released: bool,
}

/// What one run reports: its chains, whether it completed, and its arena.
#[derive(Debug)]
struct Report {
    chains: Vec<Chain>,
    complete: bool,
    arena: usize,
}

fn field<'a>(line: &'a str, key: &str) -> &'a str {
    line.split_whitespace()
        .find_map(|word| word.strip_prefix(key)?.strip_prefix('='))
        .unwrap_or_else(|| panic!("no {key} in {line:?}"))
}

fn parse(stdout: &str) -> Report {
    let chains = stdout
        .lines()
        .filter_map(|line| line.strip_prefix("chain "))
        .map(|line| Chain {
            name: line.split_whitespace().next().unwrap_or("").to_string(),
            status: field(line, "status").parse().expect("a status"),
            peak: field(line, "peak").parse().expect("a peak"),
            exhausted: field(line, "exhausted") != "0",
            released: field(line, "released") == "1",
        })
        .collect();
    let arena = stdout
        .lines()
        .find_map(|line| line.strip_prefix("arena "))
        .and_then(|arena| arena.trim().parse().ok())
        .unwrap_or_else(|| panic!("no arena size in {stdout}"));
    Report {
        chains,
        complete: stdout.lines().any(|line| line == "complete"),
        arena,
    }
}

/// The harness's copy of the sources: the allocator records its peak, and
/// with `arena`, the arena is that many doubles.
fn patch_sources(sources: &Path, arena: Option<usize>) {
    let path = sources.join("model.c");
    let source = fs::read_to_string(&path).expect("read the generated model source");
    assert_eq!(
        source.matches(ALLOCATION).count(),
        2,
        "the generated double allocator changed"
    );
    let mut patched = source.replace(
        ALLOCATION,
        "m->rmc_dtop += count; if (m->rmc_dtop > arena_peak) arena_peak = m->rmc_dtop;",
    );
    if let Some(arena) = arena {
        let define = patched
            .lines()
            .find(|line| line.starts_with("#define RMC_DWORK "))
            .expect("the generated source sizes its arena")
            .to_string();
        patched = patched.replace(&define, &format!("#define RMC_DWORK {arena}"));
    }
    fs::write(&path, patched).expect("write the harness copy of the model source");
}

fn run(compiled: &rumoca::CompilationResult, model: &str, arena: Option<usize>) -> Report {
    let headers = headers().expect("checked by the caller");
    let work = tempdir().expect("arena harness work directory");
    let fmu = build_named_fmu(work.path(), compiled, "fmi3", model);
    let sources = fmu.root.join("sources");
    patch_sources(&sources, arena);
    let harness = work.path().join("arena.c");
    fs::write(
        &harness,
        format!("#include <stddef.h>\nstatic size_t arena_peak;\n#include \"model.c\"\n{CHECKS}"),
    )
    .expect("write the harness");
    let binary = work.path().join("arena");
    let units = [
        "rmc_rows.c",
        "rmc_jacobian.c",
        "rmc_isolators.c",
        "rmc_assign.c",
        "rmc_functions.c",
    ];
    let mut command = Command::new("cc");
    command
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
        .arg(&binary);
    checked_output(&mut command, "compile the arena harness");
    let output = Command::new(&binary)
        .output()
        .expect("run the arena harness");
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(output.status.success(), "{model}: {stdout}");
    parse(&stdout)
}

/// Every chain fits the generated arena; an arena of exactly the highest
/// peak still holds every chain, and one double less exhausts the chain
/// that reached it, which fails with its frames released.
fn check_arena(compiled: &rumoca::CompilationResult, model: &str) -> (usize, usize) {
    let full = run(compiled, model, None);
    assert!(full.complete, "{model}: {full:?}");
    for chain in &full.chains {
        assert!(
            !chain.exhausted && chain.released,
            "{model}: {chain:?} in the generated arena"
        );
    }
    let peak = full
        .chains
        .iter()
        .map(|chain| chain.peak)
        .max()
        .unwrap_or(0);
    assert!(peak > 0, "{model}: no chain allocated");
    let exact = run(compiled, model, Some(peak));
    assert!(exact.complete, "{model}: {exact:?} in an arena of {peak}");
    assert_eq!(exact.chains, full.chains, "{model}: at the peak");
    let short = run(compiled, model, Some(peak - 1));
    let first = full
        .chains
        .iter()
        .position(|chain| chain.peak == peak)
        .expect("a chain reached the peak");
    assert_eq!(
        short.chains[..first],
        full.chains[..first],
        "{model}: the chains before the peak"
    );
    let failed = &short.chains[first];
    assert!(
        failed.exhausted && failed.released && failed.status == 3 && !short.complete,
        "{model}: {failed:?} one double short of the peak"
    );
    (peak, full.arena)
}

/// Every frame chain fits the arena, a chart transfer's included.
#[test]
fn every_frame_chain_fits_the_projection_arena() {
    if headers().is_none() {
        return;
    }
    let chart = include_str!("../../fixtures/index_reduction/SplitCircleChart.mo");
    let models: [(rumoca::CompilationResult, &str); 3] = [
        (
            super::affine_promotion::compile(),
            super::affine_promotion::MODEL,
        ),
        (
            rumoca::Compiler::new()
                .model("SplitCircleChart")
                .compile_str(chart, "SplitCircleChart.mo")
                .expect("compile the split circle"),
            "SplitCircleChart",
        ),
        (
            rumoca::Compiler::new()
                .model(super::torn_tangent_harness::MODEL)
                .compile_str(&super::torn_tangent_harness::source(), "TornTangent.mo")
                .expect("compile the torn loop"),
            super::torn_tangent_harness::MODEL,
        ),
    ];
    for (compiled, model) in &models {
        let (peak, arena) = check_arena(compiled, model);
        eprintln!("{model}: peak {peak} of {arena}");
    }
}
