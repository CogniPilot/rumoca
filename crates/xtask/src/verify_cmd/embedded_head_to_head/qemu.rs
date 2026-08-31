//! Exact-output execution and exclusive-marker instruction counting in QEMU.

use super::artifact_guard::FrozenArtifactSet;
use super::cross::{self, LinkedArtifact};
use super::manifest::CorrectnessCase;
use super::process;
use anyhow::{Context, Result, bail, ensure};
use std::fs::{self, File};
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::process::Output;

pub(super) const MAX_TRACE_BYTES: u64 = 8 * 1024 * 1024;

pub(super) struct TraceMeasurement {
    frozen: FrozenArtifactSet,
    pub(super) instructions: u64,
    pub(super) output_lines: Vec<String>,
    pub(super) command: process::CommandReceipt,
    pub(super) trace_log: PathBuf,
}

impl TraceMeasurement {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }
}

pub(super) enum MutationExpectation {
    MeasuredPointPreserved,
    AnyCaseRejected,
}

pub(super) fn measure(
    authorization: &cross::GuestInstructionAuthorization<'_>,
    artifact: &LinkedArtifact,
    name: &str,
    expected_cases: &[CorrectnessCase],
    trace_directory: &Path,
) -> Result<TraceMeasurement> {
    artifact.verify()?;
    let plan = authorization.plan();
    let trace_log = artifact.trace_log(trace_directory);
    let symbols = fs::read_to_string(&artifact.symbols)
        .with_context(|| format!("failed to read {}", artifact.symbols.display()))?;
    let begin = unique_symbol(&symbols, "trace_begin")?;
    let end = unique_symbol(&symbols, "trace_end")?;
    ensure!(begin != end, "trace markers resolve to the same address");
    let mut command = cross::qemu_command(plan, artifact, &trace_log)?;
    let (output, rendered) = execute_authenticated(artifact, &mut command, &trace_log)?;
    retain_trace_on_success_with_receipt(&trace_log, rendered.clone(), || {
        ensure!(
            output.status.success(),
            "QEMU execution failed ({})\ncommand: {rendered}\n{}",
            output.status,
            process::tail(&process::combined(&output))
        );
        let expected_lines = expected_output_lines(name, expected_cases);
        let output_lines = exact_output_lines(&process::combined(&output), &expected_lines)
            .with_context(|| format!("QEMU command: {rendered}"))?;
        let trace = read_trace_bounded(&trace_log)
            .with_context(|| format!("QEMU wrote no readable trace at {}", trace_log.display()))?;
        let instructions =
            parse_trace(&trace, begin, end).with_context(|| format!("QEMU command: {rendered}"))?;
        let trace_root = trace_log
            .parent()
            .context("trace log has no artifact directory")?;
        let frozen = FrozenArtifactSet::capture_tree(trace_root)?;
        Ok(TraceMeasurement {
            frozen,
            instructions,
            output_lines,
            command: rendered,
            trace_log: trace_log.clone(),
        })
    })
}

pub(super) fn validate_counter(
    plan: &cross::BoundExecutionPlan,
    artifact: &LinkedArtifact,
    expected: u64,
    trace_directory: &Path,
) -> Result<process::CommandReceipt> {
    artifact.verify()?;
    let trace_log = artifact.trace_log(trace_directory);
    let symbols = fs::read_to_string(&artifact.symbols)?;
    let begin = unique_symbol(&symbols, "trace_begin")?;
    let end = unique_symbol(&symbols, "trace_end")?;
    let mut command = cross::qemu_command(plan, artifact, &trace_log)?;
    let (output, rendered) = execute_authenticated(artifact, &mut command, &trace_log)?;
    retain_trace_on_success_with_receipt(&trace_log, rendered.clone(), || {
        ensure!(
            output.status.success(),
            "QEMU counter acceptance fixture failed\ncommand: {rendered}"
        );
        let trace = read_trace_bounded(&trace_log)?;
        let observed =
            parse_trace(&trace, begin, end).with_context(|| format!("QEMU command: {rendered}"))?;
        ensure!(
            observed == expected,
            "QEMU counter acceptance expected {expected}, observed {observed}\ncommand: {rendered}"
        );
        Ok(rendered)
    })
}

pub(super) fn validate_exit_status(
    plan: &cross::BoundExecutionPlan,
    artifact: &LinkedArtifact,
    expected: i32,
    trace_directory: &Path,
) -> Result<process::CommandReceipt> {
    artifact.verify()?;
    let trace_log = artifact.trace_log(trace_directory);
    let mut command = cross::qemu_command(plan, artifact, &trace_log)?;
    let (output, rendered) = execute_authenticated(artifact, &mut command, &trace_log)?;
    retain_trace_on_success_with_receipt(&trace_log, rendered.clone(), || {
        ensure!(
            output.status.code() == Some(expected),
            "QEMU semihosting status probe expected {expected}, observed {}\ncommand: {rendered}",
            output.status
        );
        read_trace_bounded(&trace_log)?;
        Ok(rendered)
    })
}

pub(super) fn validate_deleted_call_rejected(
    plan: &cross::BoundExecutionPlan,
    artifact: &LinkedArtifact,
    name: &str,
    expected_cases: &[CorrectnessCase],
    trace_directory: &Path,
) -> Result<process::CommandReceipt> {
    artifact.verify()?;
    let trace_log = artifact.trace_log(trace_directory);
    let symbols = fs::read_to_string(&artifact.symbols)?;
    let begin = unique_symbol(&symbols, "trace_begin")?;
    let end = unique_symbol(&symbols, "trace_end")?;
    let mut command = cross::qemu_command(plan, artifact, &trace_log)?;
    let (output, rendered) = execute_authenticated(artifact, &mut command, &trace_log)?;
    retain_trace_on_success_with_receipt(&trace_log, rendered.clone(), || {
        ensure!(
            output.status.code() == Some(1),
            "deleted-call mutant must return oracle mismatch status 1, observed {}\ncommand: {rendered}",
            output.status
        );
        let combined = process::combined(&output);
        let output_lines = combined
            .lines()
            .filter(|line| line.starts_with("OUTPUT "))
            .map(str::to_string)
            .collect::<Vec<_>>();
        let expected_lines = expected_output_lines(name, expected_cases);
        ensure!(
            output_lines.len() == expected_lines.len(),
            "deleted-call mutant emitted the wrong output cohort: {output_lines:?}"
        );
        ensure!(
            output_lines[0] != expected_lines[0],
            "deleted-call mutant reused the accepted output"
        );
        ensure!(
            output_lines[1..] == expected_lines[1..],
            "deleted-call mutant corrupted the unmeasured correctness cohort"
        );
        let trace = read_trace_bounded(&trace_log)?;
        ensure!(
            parse_trace(&trace, begin, end)? == 0,
            "deleted-call mutant unexpectedly executed measured instructions"
        );
        Ok(rendered)
    })
}

pub(super) fn validate_semantic_mutation_rejected(
    plan: &cross::BoundExecutionPlan,
    artifact: &LinkedArtifact,
    name: &str,
    expected_cases: &[CorrectnessCase],
    expectation: MutationExpectation,
    trace_directory: &Path,
) -> Result<process::CommandReceipt> {
    artifact.verify()?;
    let trace_log = artifact.trace_log(trace_directory);
    let symbols = fs::read_to_string(&artifact.symbols)?;
    let begin = unique_symbol(&symbols, "trace_begin")?;
    let end = unique_symbol(&symbols, "trace_end")?;
    let mut command = cross::qemu_command(plan, artifact, &trace_log)?;
    let (output, rendered) = execute_authenticated(artifact, &mut command, &trace_log)?;
    retain_trace_on_success_with_receipt(&trace_log, rendered.clone(), || {
        ensure!(
            output.status.code() == Some(1),
            "semantic mutant must return oracle mismatch status 1, observed {}\ncommand: {rendered}",
            output.status
        );
        let output_lines = process::combined(&output)
            .lines()
            .filter(|line| line.starts_with("OUTPUT "))
            .map(str::to_string)
            .collect::<Vec<_>>();
        let expected_lines = expected_output_lines(name, expected_cases);
        ensure!(
            output_lines.len() == expected_lines.len(),
            "semantic mutant emitted the wrong output cohort: {output_lines:?}"
        );
        for (line, case) in output_lines.iter().zip(expected_cases) {
            ensure!(
                line.starts_with(&format!("OUTPUT name={name} case={} bits=", case.id)),
                "semantic mutant mislabeled its output cohort: {output_lines:?}"
            );
        }
        ensure!(
            output_lines != expected_lines,
            "semantic mutant passed the complete correctness cohort"
        );
        if matches!(expectation, MutationExpectation::MeasuredPointPreserved) {
            ensure!(
                output_lines[0] == expected_lines[0],
                "cohort mutant must preserve the measured point"
            );
        }
        let trace = read_trace_bounded(&trace_log)?;
        ensure!(
            parse_trace(&trace, begin, end)? > 0,
            "semantic mutant did not execute the measured call"
        );
        Ok(rendered)
    })
}

fn retain_trace_on_success<T>(trace_log: &Path, validate: impl FnOnce() -> Result<T>) -> Result<T> {
    match validate() {
        Ok(value) => Ok(value),
        Err(error) => {
            discard_trace(trace_log).with_context(|| {
                format!("failed to discard rejected trace after validation error: {error:#}")
            })?;
            Err(error)
        }
    }
}

fn retain_trace_on_success_with_receipt<T>(
    trace_log: &Path,
    receipt: process::CommandReceipt,
    validate: impl FnOnce() -> Result<T>,
) -> Result<T> {
    retain_trace_on_success(trace_log, validate)
        .map_err(|error| process::attach_attempted_receipt(error, receipt))
}

fn execute(
    command: &mut process::HermeticCommand,
    trace_log: &Path,
) -> Result<(Output, process::CommandReceipt)> {
    prepare_trace(trace_log)?;
    match process::output(command, process::Limit::Guest) {
        Ok(output) => Ok(output),
        Err(error) => {
            if let Err(discard_error) = discard_trace(trace_log).with_context(|| {
                format!("failed to discard incomplete trace after QEMU error: {error:#}")
            }) {
                return Err(process::attach_prior_receipts(
                    discard_error,
                    process::attempted_receipts(&error).to_vec(),
                ));
            }
            Err(error)
        }
    }
}

fn execute_authenticated(
    artifact: &LinkedArtifact,
    command: &mut process::HermeticCommand,
    trace_log: &Path,
) -> Result<(Output, process::CommandReceipt)> {
    artifact.verify()?;
    let result = execute(command, trace_log);
    let post_use = artifact.verify();
    match (result, post_use) {
        (Ok(value), Ok(())) => Ok(value),
        (Ok((_, receipt)), Err(error)) => Err(process::attach_attempted_receipt(error, receipt)),
        (Err(error), _) => Err(error),
    }
}

fn prepare_trace(trace_log: &Path) -> Result<()> {
    if let Some(parent) = trace_log.parent() {
        fs::create_dir_all(parent)?;
    }
    discard_trace(trace_log)
}

fn discard_trace(trace_log: &Path) -> Result<()> {
    match fs::remove_file(trace_log) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => {
            Err(error).with_context(|| format!("failed to clear {}", trace_log.display()))
        }
    }
}

fn read_trace_bounded(trace_log: &Path) -> Result<String> {
    let result = read_trace_bounded_inner(trace_log);
    if result.is_err() {
        discard_trace(trace_log)?;
    }
    result
}

fn read_trace_bounded_inner(trace_log: &Path) -> Result<String> {
    let mut file = File::open(trace_log)
        .with_context(|| format!("failed to open QEMU trace {}", trace_log.display()))?;
    let metadata = file
        .metadata()
        .with_context(|| format!("failed to inspect QEMU trace {}", trace_log.display()))?;
    ensure!(
        metadata.is_file(),
        "QEMU trace is not a regular file: {}",
        trace_log.display()
    );
    ensure!(
        metadata.len() <= MAX_TRACE_BYTES,
        "QEMU trace is {} bytes, limit is {MAX_TRACE_BYTES}",
        metadata.len()
    );
    let mut bytes = Vec::new();
    file.by_ref()
        .take(MAX_TRACE_BYTES + 1)
        .read_to_end(&mut bytes)
        .with_context(|| format!("failed to read QEMU trace {}", trace_log.display()))?;
    ensure!(
        bytes.len() as u64 <= MAX_TRACE_BYTES,
        "QEMU trace grew beyond the {MAX_TRACE_BYTES}-byte limit while reading"
    );
    String::from_utf8(bytes).context("QEMU trace is not valid UTF-8")
}

fn expected_output_lines(name: &str, cases: &[CorrectnessCase]) -> Vec<String> {
    cases
        .iter()
        .map(|case| {
            format!(
                "OUTPUT name={name} case={} bits={}",
                case.id,
                case.expected_output_bits.join(",")
            )
        })
        .collect()
}

fn exact_output_lines(output: &str, expected: &[String]) -> Result<Vec<String>> {
    let lines = output
        .lines()
        .filter(|line| line.starts_with("OUTPUT "))
        .map(str::to_string)
        .collect::<Vec<_>>();
    ensure!(
        lines == expected,
        "expected exact output cohort {expected:?}, observed OUTPUT lines {lines:?}"
    );
    Ok(lines)
}

fn unique_symbol(symbols: &str, name: &str) -> Result<u64> {
    let matches = symbols
        .lines()
        .filter_map(|line| {
            let fields = line.split_whitespace().collect::<Vec<_>>();
            (fields.last().copied() == Some(name))
                .then(|| fields.first().copied())
                .flatten()
        })
        .collect::<Vec<_>>();
    ensure!(
        matches.len() == 1,
        "expected one `{name}` symbol, found {matches:?}"
    );
    u64::from_str_radix(matches[0], 16).with_context(|| format!("invalid `{name}` address"))
}

pub(super) fn parse_trace(trace: &str, begin: u64, end: u64) -> Result<u64> {
    let mut active = false;
    let mut begin_hits = 0_u64;
    let mut end_hits = 0_u64;
    let mut instructions = 0_u64;
    let mut trace_lines = 0_u64;
    for line in trace.lines().filter(|line| line.starts_with("Trace ")) {
        trace_lines += 1;
        let pc = trace_pc(line)?;
        if pc == begin {
            ensure!(!active, "nested trace_begin makes the metric ambiguous");
            begin_hits += 1;
            active = true;
        } else if pc == end {
            ensure!(active, "trace_end appeared outside the measured interval");
            end_hits += 1;
            active = false;
        } else if active {
            instructions = instructions
                .checked_add(1)
                .context("instruction counter overflow")?;
        }
    }
    ensure!(trace_lines > 0, "QEMU trace contains no `Trace ` records");
    ensure!(
        begin_hits == 1 && end_hits == 1 && !active,
        "invalid marker lifecycle: begin_hits={begin_hits}, end_hits={end_hits}, active={active}"
    );
    Ok(instructions)
}

fn trace_pc(line: &str) -> Result<u64> {
    let pc = line
        .split('/')
        .nth(1)
        .with_context(|| format!("malformed QEMU Trace record `{line}`"))?;
    if pc.len() != 16 || !pc.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        bail!("malformed QEMU Trace PC `{pc}` in `{line}`");
    }
    u64::from_str_radix(pc, 16).with_context(|| format!("invalid QEMU Trace PC `{pc}`"))
}

#[cfg(test)]
mod tests {
    use super::{
        MAX_TRACE_BYTES, exact_output_lines, expected_output_lines, parse_trace,
        read_trace_bounded, retain_trace_on_success,
    };
    use crate::verify_cmd::embedded_head_to_head::manifest::CorrectnessCase;
    use std::fs::File;

    fn record(pc: u64) -> String {
        format!("Trace 0: host [00000000/{pc:016x}/00000110/ff000000] x\n")
    }

    #[test]
    fn exclusive_markers_count_called_code_at_any_address() {
        let trace = [record(0x10), record(0x11), record(0x9_0000), record(0x12)].concat();
        assert_eq!(parse_trace(&trace, 0x10, 0x12).unwrap(), 2);
    }

    #[test]
    fn malformed_or_repeated_markers_fail_closed() {
        let nested = [record(0x10), record(0x10), record(0x12)].concat();
        assert!(parse_trace(&nested, 0x10, 0x12).is_err());
        assert!(parse_trace("Trace not/a/pc\n", 0x10, 0x12).is_err());
        assert!(parse_trace(&record(0x10), 0x10, 0x12).is_err());
    }

    #[test]
    fn oversized_trace_is_rejected_before_allocation_and_removed() {
        let temporary = tempfile::tempdir().unwrap();
        let trace = temporary.path().join("oversized.trace");
        File::create(&trace)
            .unwrap()
            .set_len(MAX_TRACE_BYTES + 1)
            .unwrap();
        let error = read_trace_bounded(&trace).expect_err("oversized trace must fail closed");
        assert!(error.to_string().contains("limit"));
        assert!(!trace.exists(), "refused trace must not survive for upload");
    }

    #[test]
    fn only_accepted_trace_evidence_is_retained() {
        let temporary = tempfile::tempdir().unwrap();
        let rejected = temporary.path().join("rejected.trace");
        std::fs::write(&rejected, "bounded but invalid").unwrap();
        let result: anyhow::Result<()> =
            retain_trace_on_success(&rejected, || anyhow::bail!("rejected evidence"));
        assert!(result.is_err());
        assert!(!rejected.exists());

        let accepted = temporary.path().join("accepted.trace");
        std::fs::write(&accepted, "accepted evidence").unwrap();
        retain_trace_on_success(&accepted, || Ok(())).unwrap();
        assert!(accepted.exists());
    }

    #[test]
    fn correctness_cohort_rejects_constant_output_and_layout_mutations() {
        let cases = [
            ("current-closed-closed", "00000001"),
            ("tiny-all-series", "00000002"),
            ("middle-coefficient-series", "00000003"),
            ("different-dense-coupling", "00000004"),
        ]
        .into_iter()
        .map(|(id, word)| CorrectnessCase {
            id: id.into(),
            expected_output_bits: vec![word.into(); 10],
        })
        .collect::<Vec<_>>();
        let expected = expected_output_lines("rumoca", &cases);
        let constant = cases
            .iter()
            .map(|case| {
                format!(
                    "OUTPUT name=rumoca case={} bits={}",
                    case.id,
                    cases[0].expected_output_bits.join(",")
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert!(exact_output_lines(&constant, &expected).is_err());

        let mut layout = expected.clone();
        layout[3] = layout[3].replace("00000004", "00000005");
        assert!(exact_output_lines(&layout.join("\n"), &expected).is_err());
    }
}
