//! Running one pinned row: build the compiler invocation, run it, and say what
//! happened without deciding whether that was allowed.
//!
//! Judgement lives in [`super::verdict`]. Keeping the two apart is what lets
//! `--record` reuse the identical invocation to propose new pins: a recorder
//! that ran the model differently from the gate would pin numbers the gate then
//! fails to reproduce.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use super::manifest::{Check, CorpusEntry};
use super::trace::Trace;

/// Everything one corpus row's invocation produced.
pub(crate) struct ModelRun {
    pub(crate) succeeded: bool,
    /// Combined child output, ANSI already stripped, kept for the diagnostic
    /// code and for the failure report.
    pub(crate) output: String,
    /// Where the simulate check wrote its CSV, if it wrote one.
    pub(crate) trace_path: Option<PathBuf>,
    pub(crate) elapsed: Duration,
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

/// Where a row's sources and artifacts live for this invocation.
pub(crate) struct RunContext<'a> {
    pub(crate) rumoca: &'a Path,
    pub(crate) corpus_root: &'a Path,
    pub(crate) artifact_dir: &'a Path,
    pub(crate) cache_dir: &'a Path,
}

pub(crate) fn run_entry(entry: &CorpusEntry, context: &RunContext<'_>) -> Result<ModelRun> {
    let stem = artifact_stem(&entry.id);
    let mut command = Command::new(context.rumoca);
    let trace_path = match &entry.check {
        Check::Compile { target } => {
            let out = context.artifact_dir.join(format!("{stem}.out"));
            compile_command(&mut command, entry, context, target, &out);
            None
        }
        Check::Simulate { t_end, dt, solver } => {
            let out = context.artifact_dir.join(format!("{stem}.csv"));
            simulate_command(&mut command, entry, context, (*t_end, *dt, solver), &out);
            Some(out)
        }
    };
    // The compiler appends `MODELICAPATH` entries to `--source-root`. The gate
    // resolves the corpus itself and states it explicitly, so an inherited
    // `MODELICAPATH` could only make the run depend on the operator's shell.
    command.env_remove("MODELICAPATH");
    command.current_dir(context.artifact_dir);
    let command_line = describe(&command);
    let started = Instant::now();
    let output = command
        .output()
        .with_context(|| format!("failed to run `{command_line}`"))?;
    let elapsed = started.elapsed();
    let mut text = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    text.push_str(&strip_ansi(&String::from_utf8_lossy(&output.stdout)));
    Ok(ModelRun {
        succeeded: output.status.success(),
        output: text,
        trace_path: trace_path.filter(|path| path.is_file()),
        elapsed,
        command_line,
    })
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
