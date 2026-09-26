//! The generated C kernel's tear Jacobian equals the linked kernel's tangent
//! plan bit for bit (SPEC_0043 torn tangent row; SPEC_0044 ME-PROJ-001).
//!
//! `TornTangent` holds three coupled nonlinear loops torn at a few unknowns,
//! whose tangent plans take the multi-lane form (one, over a vector equation,
//! answers its three causal steps with one evaluation), and one fully coupled loop of
//! 34 unknowns with two causal extras, whose 34 tears exceed the lane width,
//! so its plan takes the directional form. A C harness includes the generated
//! FMI 3 `model.c`, settles the start point, moves every coordinate off it,
//! and evaluates `rmc_reduced_jacobian` on each torn block. It prints the
//! point, the tear and causal coordinates, and the bits of the reduced
//! Jacobian and the recovered sensitivities. The linked
//! `TornTangentEvaluator`, built from the same lowered component, evaluates
//! the same block at the printed point and must return the same bits for both
//! forms.

use super::*;

use rumoca_eval_solve::{RowEvalContext, TangentPoint, TornTangentEvaluator};
use rumoca_ir_solve as solve;

const MODEL: &str = "TornTangent";
const WIDE: usize = 34;

fn source() -> String {
    let mut source = format!(
        "model {MODEL}
  Real x(start=1, fixed=true);
  Real a(start=1);
  Real b(start=1);
  Real c(start=1);
  Real u(start=0.5);
  Real v(start=0.5);
  Real w(start=0.5);
  Real q(start=0.2);
  Real r[3](each start=0.3);
  Real h(start=0.5);
"
    );
    for k in 1..=WIDE {
        source.push_str(&format!("  Real z{k}(start=0.1);\n"));
    }
    source.push_str("  Real e1(start=0.01);\n  Real e2(start=0.01);\n");
    let sum = (1..=WIDE)
        .map(|k| format!("z{k}"))
        .collect::<Vec<_>>()
        .join(" + ");
    source.push_str(
        "equation
  der(x) = -0.5*a + 0.1*w - 0.01*z1;
  a = x + 0.2*sin(c);
  b = a*a + 0.1*a;
  c = 1 + b - 0.3*cos(b) + 0.1*sin(c);
  u = 0.4*cos(v) + 0.1*x*w;
  v = u*u - 0.2*q + 0.3;
  w = exp(-0.5*v) + 0.2*u*q;
  q = 0.3*sin(w) + 0.1*v*v;
  r = {0.1*sin(h), 0.2*cos(h), 0.3*h*h} + 0.01*x*{1, 2, 3};
  h = 0.5 + 0.1*(r[1] + r[2] + r[3])^2 + 0.05*sin(h);
",
    );
    for k in 1..=WIDE {
        source.push_str(&format!(
            "  z{k} = 0.01*sin({sum} + e2) + 0.001*{k}*x*z{};\n",
            k % WIDE + 1
        ));
    }
    source.push_str("  e1 = z1*z2 + 0.1*x;\n  e2 = 0.5*e1 + z3*z3;\n");
    source.push_str(&format!("end {MODEL};\n"));
    source
}

const HARNESS: &str = r#"#include "model.c"
#include <stdio.h>
#include <stdint.h>
static void bits(const char* tag, const double* values, size_t n) {
    printf("%s", tag);
    for (size_t k = 0; k < n; ++k) { uint64_t word; memcpy(&word, &values[k], sizeof word); printf(" %016llx", (unsigned long long)word); }
    printf("\n");
}
int main(void) {
    ModelInstance* m = calloc(1, sizeof *m);
    if (!m) return 1;
    initialize_values(m);
    if (settle_values(m) != fmi3OK) { printf("FAILED: settle\n"); return 1; }
    for (size_t i = 0; i < Y_LEN; ++i) m->y[i] += 1e-2 * (double)((int)(i % 7) - 3) * fmax(fabs(m->y[i]), 1.0);
    m->rmc_certify = true;
    bits("y", m->y, Y_LEN);
    bits("p", m->p, P_LEN);
    bits("t", &m->time, 1);
    for (size_t index = 0; index < sizeof(rmc_blocks) / sizeof(rmc_blocks[0]); ++index) {
        const RmcBlock* b = &rmc_blocks[index];
        if (!b->torn) continue;
        size_t k = b->k;
        double* jac = calloc(k * k + 1, sizeof(double));
        double* recovered = calloc(b->ncausal * k + 1, sizeof(double));
        if (!jac || !recovered) return 1;
        size_t top = m->rmc_dtop;
        int result = rmc_reduced_jacobian(m, b, jac, recovered);
        m->rmc_dtop = top;
        printf("block %d %d", result, b->tangent_directional ? 1 : 0);
        for (size_t c = 0; c < k; ++c) printf(" %zu", b->y[b->tear_col[c]]);
        printf(" |");
        for (size_t s = 0; s < b->ncausal; ++s) printf(" %zu", b->causal_target[s]);
        printf("\n");
        bits("jac", jac, k * k);
        bits("recovered", recovered, b->ncausal * k);
        free(jac);
        free(recovered);
    }
    free(m);
    return 0;
}
"#;

/// One torn block as the generated harness reports it.
struct GeneratedBlock {
    result: i32,
    directional: bool,
    tears: Vec<usize>,
    targets: Vec<usize>,
    jac: Vec<u64>,
    recovered: Vec<u64>,
}

/// The point and the torn blocks the harness prints.
struct Report {
    y: Vec<f64>,
    p: Vec<f64>,
    t: f64,
    blocks: Vec<GeneratedBlock>,
}

fn words(line: &str, tag: &str) -> Vec<u64> {
    line.strip_prefix(tag)
        .unwrap_or_else(|| panic!("a `{tag}` line: {line}"))
        .split_whitespace()
        .map(|word| u64::from_str_radix(word, 16).expect("a hexadecimal word"))
        .collect()
}

fn values(line: &str, tag: &str) -> Vec<f64> {
    words(line, tag).into_iter().map(f64::from_bits).collect()
}

fn indices(text: &str) -> Vec<usize> {
    text.split_whitespace()
        .map(|word| word.parse().expect("an index"))
        .collect()
}

fn parse(stdout: &str) -> Report {
    let mut lines = stdout.lines();
    let mut next = || lines.next().expect("another harness line");
    let y = values(next(), "y");
    let p = values(next(), "p");
    let t = values(next(), "t")[0];
    let mut blocks = Vec::new();
    let mut rest = stdout.lines().skip(3);
    while let Some(header) = rest.next() {
        let header = header.strip_prefix("block ").expect("a block line");
        let (head, targets) = header.split_once('|').expect("tears | targets");
        let head = indices(head);
        blocks.push(GeneratedBlock {
            result: i32::try_from(head[0]).expect("a result"),
            directional: head[1] == 1,
            tears: head[2..].to_vec(),
            targets: indices(targets),
            jac: words(rest.next().expect("a jac line"), "jac"),
            recovered: words(rest.next().expect("a recovered line"), "recovered"),
        });
    }
    Report { y, p, t, blocks }
}

/// The linked tangent plan of the block with `tears` and `targets`.
fn linked_block<'a>(
    model: &'a solve::SolveModel,
    generated: &GeneratedBlock,
) -> &'a solve::BlockTearing {
    model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .filter_map(|block| block.tearing.as_ref())
        .find(|tearing| {
            tearing.tear_y_indices == generated.tears
                && tearing
                    .causal_steps
                    .iter()
                    .map(|step| step.y_index)
                    .eq(generated.targets.iter().copied())
        })
        .unwrap_or_else(|| panic!("a linked block with tears {:?}", generated.tears))
}

/// Check every reported block against the linked plan; the largest step group
/// one evaluation answers.
fn assert_linked(model: &solve::SolveModel, report: &Report) -> usize {
    let jvp =
        rumoca_eval_solve::to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)
            .expect("scalarize the solver-Y JVP");
    let mut largest_group = 0;
    for generated in &report.blocks {
        let tearing = linked_block(model, generated);
        let plan = solve::TornTangentPlan::derive(tearing, &jvp).expect("the tangent plan");
        assert_eq!(plan.directional(), generated.directional);
        largest_group = plan
            .steps()
            .iter()
            .map(|step| step.group)
            .fold(largest_group, usize::max);
        let evaluator = TornTangentEvaluator::new(plan, &jvp).expect("prepare the tangent plan");
        let point = TangentPoint {
            y: &report.y,
            p: &report.p,
            t: report.t,
            context: RowEvalContext {
                pure_calls: Some(&model.pure_calls),
                external_tables: Some(model.external_tables.as_slice()),
                ..RowEvalContext::default()
            },
        };
        let linked = evaluator
            .eval(point)
            .expect("evaluate the linked tangent plan")
            .expect("a regular point");
        assert_eq!(generated.result, 1, "the generated plan evaluates");
        let bits = |values: &[f64]| {
            values
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>()
        };
        assert_eq!(
            generated.jac,
            bits(&linked.residual),
            "reduced Jacobian of {:?}",
            generated.tears
        );
        assert_eq!(
            generated.recovered,
            bits(&linked.recovered),
            "recovered sensitivities of {:?}",
            generated.tears
        );
    }
    largest_group
}

#[test]
fn generated_torn_tangent_matches_the_linked_plan_bit_for_bit() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    if !headers.join("fmi3Functions.h").is_file() {
        return;
    }
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(&source(), &format!("{MODEL}.mo"))
        .unwrap_or_else(|error| panic!("compile {MODEL}: {error:?}"));
    let component = rumoca_sim::lower_fmi_component(&compiled.dae)
        .unwrap_or_else(|error| panic!("lower {MODEL}: {error:?}"));
    let work = tempdir().expect("torn tangent harness work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", MODEL);
    let sources = fmu.root.join("sources");
    let harness = work.path().join("torn_tangent_harness.c");
    fs::write(&harness, HARNESS).expect("write the torn tangent harness");
    let units = fs::read_dir(&sources)
        .expect("list the generated sources")
        .map(|entry| entry.expect("a source entry").path())
        .filter(|path| {
            path.extension().is_some_and(|extension| extension == "c")
                && path.file_name().is_some_and(|name| name != "model.c")
        })
        .collect::<Vec<_>>();
    let binary = work.path().join("torn_tangent_harness");
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
            .args(&units)
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the torn tangent harness",
    );
    let output = checked_output(&mut Command::new(&binary), "run the torn tangent harness");
    let report = parse(&String::from_utf8_lossy(&output.stdout));
    let lanes = report
        .blocks
        .iter()
        .filter(|block| !block.directional)
        .count();
    let directional = report.blocks.len() - lanes;
    let shapes: Vec<(usize, usize, bool)> = report
        .blocks
        .iter()
        .map(|block| (block.tears.len(), block.targets.len(), block.directional))
        .collect();
    eprintln!("{MODEL}: torn blocks (tears, causal steps, directional) {shapes:?}");
    let causal = |directional: bool| {
        report
            .blocks
            .iter()
            .filter(|block| block.directional == directional && !block.targets.is_empty())
            .count()
    };
    assert!(
        lanes >= 2 && directional >= 1 && causal(false) >= 1 && causal(true) >= 1,
        "the fixture renders both forms: {lanes} lane, {directional} directional"
    );
    let largest_group = assert_linked(component.runtime_view().model(), &report);
    assert!(
        largest_group >= 3,
        "one evaluation answers a group of the vector loop's steps: {largest_group}"
    );
}
