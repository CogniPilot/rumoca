//! Running one pinned row: build the compiler invocation, run it under a
//! deadline, and say what happened without deciding whether that was allowed.
//!
//! Judgement lives in [`super::verdict`]. Keeping the two apart is what lets
//! `--record` reuse the identical invocation to propose new pins: a recorder
//! that ran the model differently from the gate would pin numbers the gate then
//! fails to reproduce.
//!
//! # Why every row runs under a deadline
//!
//! This gate is wired into `verify quick`, so it sits in the developer loop. A
//! model that stops terminating would otherwise hang that loop with no output
//! and no row to blame, and a hang is exactly the shape a fresh non-termination
//! bug takes. The deadline comes from the manifest budget
//! ([`super::manifest::CorpusManifest::row_deadline`]) so it is reviewed data
//! rather than a hidden constant, and it is sized as a hang catcher rather than
//! a performance assertion.
//!
//! # Why the child writes to files rather than pipes
//!
//! A killed process leaves anything it spawned behind, still holding the write
//! end of an inherited pipe. Reading that pipe to end-of-file would then block
//! until the *grandchild* finished, which is precisely the hang the deadline
//! exists to end. Each row's streams go to two files beside its artifacts
//! instead: nothing can block on them, and the raw child output survives the run
//! for whoever reads the report.

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

use super::manifest::{Check, CorpusEntry};
use super::trace::Trace;

/// How often the deadline is rechecked while a row runs. Short enough that a
/// killed row is reported promptly, long enough to cost nothing next to a run
/// measured in seconds.
const POLL_INTERVAL: Duration = Duration::from_millis(25);

/// Everything one corpus row's invocation produced.
pub(crate) struct ModelRun {
    pub(crate) succeeded: bool,
    /// Combined child output, ANSI already stripped, kept for the diagnostic
    /// code and for the failure report.
    pub(crate) output: String,
    /// Where the simulate check wrote its CSV, if it wrote one.
    pub(crate) trace_path: Option<PathBuf>,
    /// Where a compile check was told to write, so the row's declared
    /// artifacts can be read back from it.
    pub(crate) output_dir: Option<PathBuf>,
    pub(crate) elapsed: Duration,
    /// The row outran its deadline and was killed, so nothing it produced is
    /// evidence about anything.
    pub(crate) timed_out: bool,
    /// The deadline this row was given, so the timeout finding can state it.
    pub(crate) deadline: Duration,
    /// The exact command line, so a red row can be reproduced by hand.
    pub(crate) command_line: String,
}

impl ModelRun {
    pub(crate) fn trace(&self) -> Result<Trace> {
        let path = self
            .trace_path
            .as_deref()
            .context("this check wrote no simulation trace")?;
        Trace::read(path)
    }
}

/// Where a row's sources and artifacts live for this invocation, and how long
/// it may run.
pub(crate) struct RunContext<'a> {
    pub(crate) rumoca: &'a Path,
    pub(crate) corpus_root: &'a Path,
    pub(crate) artifact_dir: &'a Path,
    pub(crate) cache_dir: &'a Path,
    pub(crate) deadline: Duration,
}

pub(crate) fn run_entry(entry: &CorpusEntry, context: &RunContext<'_>) -> Result<ModelRun> {
    let stem = artifact_stem(&entry.id);
    let mut command = Command::new(context.rumoca);
    let (trace_path, output_dir) = match &entry.check {
        Check::Compile { target } => {
            let out = context.artifact_dir.join(format!("{stem}.out"));
            compile_command(&mut command, entry, context, target, &out);
            (None, Some(out))
        }
        Check::Simulate { t_end, dt, solver } => {
            let out = context.artifact_dir.join(format!("{stem}.csv"));
            simulate_command(&mut command, entry, context, (*t_end, *dt, solver), &out);
            (Some(out), None)
        }
    };
    // The compiler appends `MODELICAPATH` entries to `--source-root`. The gate
    // resolves the corpus itself and states it explicitly, so an inherited
    // `MODELICAPATH` could only make the run depend on the operator's shell.
    command.env_remove("MODELICAPATH");
    command.current_dir(context.artifact_dir);
    let command_line = describe(&command);
    let logs = Logs::beside(context.artifact_dir, &stem);
    let started = Instant::now();
    let outcome = run_to_completion(&mut command, context.deadline, &logs, &command_line)?;
    let elapsed = started.elapsed();
    Ok(ModelRun {
        succeeded: outcome.succeeded,
        output: logs.read(),
        trace_path: trace_path.filter(|path| path.is_file()),
        output_dir,
        elapsed,
        timed_out: outcome.timed_out,
        deadline: context.deadline,
        command_line,
    })
}

/// Where one row's child streams are captured.
struct Logs {
    standard_error: PathBuf,
    standard_output: PathBuf,
}

impl Logs {
    fn beside(artifact_dir: &Path, stem: &str) -> Self {
        Self {
            standard_error: artifact_dir.join(format!("{stem}.stderr")),
            standard_output: artifact_dir.join(format!("{stem}.stdout")),
        }
    }

    /// Both streams, ANSI stripped, diagnostics first. A stream that could not
    /// be read back contributes nothing rather than failing the row: the
    /// verdict is about the model, not about this file.
    fn read(&self) -> String {
        let mut text = strip_ansi(&read_lossy(&self.standard_error));
        text.push_str(&strip_ansi(&read_lossy(&self.standard_output)));
        text
    }
}

fn read_lossy(path: &Path) -> String {
    fs::read(path)
        .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        .unwrap_or_default()
}

/// What one child process did, before anything judges it.
struct ChildOutcome {
    succeeded: bool,
    timed_out: bool,
}

/// Run the child with its streams captured, and kill it if it outruns
/// `deadline`.
fn run_to_completion(
    command: &mut Command,
    deadline: Duration,
    logs: &Logs,
    command_line: &str,
) -> Result<ChildOutcome> {
    let standard_error = fs::File::create(&logs.standard_error)
        .with_context(|| format!("failed to create {}", logs.standard_error.display()))?;
    let standard_output = fs::File::create(&logs.standard_output)
        .with_context(|| format!("failed to create {}", logs.standard_output.display()))?;
    let mut child = command
        .stdin(Stdio::null())
        .stderr(Stdio::from(standard_error))
        .stdout(Stdio::from(standard_output))
        .spawn()
        .with_context(|| format!("failed to run `{command_line}`"))?;
    let (status, timed_out) = wait_for_child(&mut child, deadline)?;
    Ok(ChildOutcome {
        // A killed child's exit status says "signalled", which is neither a
        // refusal nor a success. The timeout verdict owns it instead.
        succeeded: !timed_out && status.is_some_and(|status| status.success()),
        timed_out,
    })
}

/// Poll the child until it exits or outruns `deadline`, in which case it is
/// killed and reaped. The flag says which of the two happened.
fn wait_for_child(child: &mut Child, deadline: Duration) -> Result<(Option<ExitStatus>, bool)> {
    let started = Instant::now();
    loop {
        if let Some(status) = child
            .try_wait()
            .context("failed to poll the compiler process")?
        {
            return Ok((Some(status), false));
        }
        if started.elapsed() >= deadline {
            let _already_exited = child.kill();
            return Ok((child.wait().ok(), true));
        }
        std::thread::sleep(POLL_INTERVAL);
    }
}

fn compile_command(
    command: &mut Command,
    entry: &CorpusEntry,
    context: &RunContext<'_>,
    target: &str,
    out: &Path,
) {
    command.arg("compile");
    push_model_arguments(command, entry, context);
    command.arg("--target").arg(target).arg("-o").arg(out);
}

fn simulate_command(
    command: &mut Command,
    entry: &CorpusEntry,
    context: &RunContext<'_>,
    run: (f64, f64, &str),
    out: &Path,
) {
    let (t_end, dt, solver) = run;
    command.arg("sim");
    push_model_arguments(command, entry, context);
    command
        .arg("--t-end")
        .arg(format!("{t_end}"))
        .arg("--dt")
        .arg(format!("{dt}"))
        .arg("--solver")
        .arg(solver)
        .arg("-o")
        .arg(out);
}

fn push_model_arguments(command: &mut Command, entry: &CorpusEntry, context: &RunContext<'_>) {
    command
        .arg(context.corpus_root.join(&entry.entry_point))
        .arg("--model")
        .arg(&entry.model)
        .arg("--source-root")
        .arg(context.corpus_root)
        .arg("--cache-dir")
        .arg(context.cache_dir);
}

/// A filesystem-safe stem for a row id like `msl/clocked-back-sample`.
pub(crate) fn artifact_stem(id: &str) -> String {
    id.chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || matches!(character, '-' | '_' | '.') {
                character
            } else {
                '_'
            }
        })
        .collect()
}

fn describe(command: &Command) -> String {
    let mut parts = vec![command.get_program().to_string_lossy().into_owned()];
    parts.extend(
        command
            .get_args()
            .map(|argument| argument.to_string_lossy().into_owned()),
    );
    parts.join(" ")
}

/// Remove ANSI SGR sequences so a diagnostic code can be found in the text
/// whether or not the child decided it was writing to a terminal.
pub(crate) fn strip_ansi(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut characters = text.chars();
    while let Some(character) = characters.next() {
        if character != '\u{1b}' {
            result.push(character);
            continue;
        }
        // Skip up to the terminating byte of the escape sequence.
        for escaped in characters.by_ref() {
            if escaped.is_ascii_alphabetic() {
                break;
            }
        }
    }
    result
}

/// The first rumoca diagnostic code in `text`, e.g. `EX002` or `EGT017`.
///
/// The renderer spells codes both bracketed (`[ED019]`) and bare (`EGT017`), so
/// this scans tokens rather than matching one layout. A code is `E`, one to
/// four uppercase letters, then two to four digits, tight enough that ordinary
/// prose cannot produce one by accident.
pub(crate) fn first_diagnostic_code(text: &str) -> Option<String> {
    text.split(|character: char| !character.is_ascii_alphanumeric())
        .find(|token| is_diagnostic_code(token))
        .map(str::to_string)
}

fn is_diagnostic_code(token: &str) -> bool {
    let Some(rest) = token.strip_prefix('E') else {
        return false;
    };
    let letters = rest.chars().take_while(char::is_ascii_uppercase).count();
    let digits = &rest[letters..];
    (1..=4).contains(&letters)
        && (2..=4).contains(&digits.len())
        && digits.chars().all(|character| character.is_ascii_digit())
}
