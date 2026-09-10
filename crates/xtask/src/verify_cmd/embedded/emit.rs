//! Getting one row's C out of the compiler, and finding what to measure in it.
//!
//! The two emitting targets lay their files out differently: `embedded-c-galec`
//! writes its two translation units and two headers straight into the output directory, while
//! `galec-production` buries them in an eFMU container under
//! `<Model>/ProductionCode/`. Neither layout is hard-coded here. The gate walks
//! whatever the compiler wrote, takes every `.c` as a translation unit to
//! build, and reads the state struct's name out of the emitted header rather
//! than deriving it from the Modelica class name: a symbol-policy change that
//! renamed the struct would otherwise turn into a confusing probe compile
//! error instead of a clear one.

use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Everything the compiler produced for one row.
pub(crate) struct Emission {
    /// Every emitted `.c`, sorted by path so the report is stable.
    pub(crate) sources: Vec<PathBuf>,
    /// Directories holding emitted headers, for the cross compiler's `-I`.
    pub(crate) include_dirs: Vec<PathBuf>,
    /// The header declaring the block state struct.
    pub(crate) state_header: PathBuf,
    /// The C name of that struct, read out of the header.
    pub(crate) state_type: String,
    /// The exact compiler command line, so a red row is reproducible by hand.
    pub(crate) command_line: String,
}

/// Where a row's sources and artifacts live for this invocation.
pub(crate) struct EmitContext<'a> {
    pub(crate) rumoca: &'a Path,
    pub(crate) models_root: &'a Path,
    pub(crate) out_dir: &'a Path,
    pub(crate) cache_dir: &'a Path,
}

/// Compile one model to C, or fail with the compiler's own output.
pub(crate) fn emit(
    model: &str,
    entry_point: &str,
    target: &str,
    cx: &EmitContext<'_>,
) -> Result<Emission> {
    let mut command = Command::new(cx.rumoca);
    command
        .arg("compile")
        .arg(cx.models_root.join(entry_point))
        .arg("--model")
        .arg(model)
        .arg("--target")
        .arg(target)
        .arg("--source-root")
        .arg(cx.models_root)
        .arg("--cache-dir")
        .arg(cx.cache_dir)
        .arg("-o")
        .arg(cx.out_dir);
    // The compiler appends `MODELICAPATH` entries to `--source-root`. The gate
    // states the root explicitly, so an inherited `MODELICAPATH` could only
    // make the measurement depend on the operator's shell.
    command.env_remove("MODELICAPATH");
    let command_line = describe(&command);
    let output = command
        .output()
        .with_context(|| format!("failed to run `{command_line}`"))?;
    if !output.status.success() {
        bail!(
            "the compiler refused this row, so nothing could be measured\n    reproduce: \
             {command_line}\n{}",
            indent(&tail(&combined(&output))),
        );
    }
    collect(cx.out_dir, &command_line)
}

fn collect(out_dir: &Path, command_line: &str) -> Result<Emission> {
    let mut sources = Vec::new();
    let mut headers = Vec::new();
    walk(out_dir, &mut sources, &mut headers)?;
    sources.sort();
    headers.sort();
    if sources.is_empty() {
        bail!(
            "the compiler exited zero but wrote no .c file under {}\n    reproduce: \
             {command_line}",
            out_dir.display()
        );
    }
    let (state_header, state_type) = find_state_type(&headers, out_dir)?;
    let mut include_dirs: Vec<PathBuf> = headers
        .iter()
        .filter_map(|header| header.parent().map(Path::to_path_buf))
        .collect();
    include_dirs.sort();
    include_dirs.dedup();
    Ok(Emission {
        sources,
        include_dirs,
        state_header,
        state_type,
        command_line: command_line.to_string(),
    })
}

fn walk(dir: &Path, sources: &mut Vec<PathBuf>, headers: &mut Vec<PathBuf>) -> Result<()> {
    let listing = fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))?;
    for entry in listing {
        let path = entry
            .with_context(|| format!("failed to read an entry of {}", dir.display()))?
            .path();
        if path.is_dir() {
            walk(&path, sources, headers)?;
        } else if path.extension().is_some_and(|extension| extension == "c") {
            sources.push(path);
        } else if path.extension().is_some_and(|extension| extension == "h") {
            headers.push(path);
        }
    }
    Ok(())
}

/// The block state struct's header and C name.
///
/// The emitted header closes the typedef with `} <Name>State;` on its own line.
/// Exactly one emitted header may declare one: zero means the artifact no
/// longer has a state struct to size, and more than one means the gate cannot
/// tell which allocation the ceiling is about. Both are refusals, because a
/// probe compiled against a guess would report a number for the wrong struct.
fn find_state_type(headers: &[PathBuf], out_dir: &Path) -> Result<(PathBuf, String)> {
    let mut found: Vec<(PathBuf, String)> = Vec::new();
    for header in headers {
        let text = fs::read_to_string(header)
            .with_context(|| format!("failed to read {}", header.display()))?;
        found.extend(
            state_type_names(&text)
                .into_iter()
                .map(|name| (header.clone(), name)),
        );
    }
    match found.len() {
        1 => Ok(found.remove(0)),
        0 => bail!(
            "no emitted header under {} closes a `}} <Name>State;` typedef, so there is no \
             state struct to size",
            out_dir.display()
        ),
        _ => bail!(
            "{} emitted headers under {} declare a state struct ({}); the gate cannot tell \
             which allocation the state ceiling is about",
            found.len(),
            out_dir.display(),
            found
                .iter()
                .map(|(_, name)| name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// Names closed by a `} <Name>State;` line in one header.
pub(crate) fn state_type_names(header: &str) -> Vec<String> {
    header
        .lines()
        .filter_map(|line| {
            let name = line.trim().strip_prefix('}')?.trim().strip_suffix(';')?;
            let name = name.trim();
            let is_identifier = !name.is_empty()
                && name
                    .chars()
                    .all(|character| character.is_ascii_alphanumeric() || character == '_')
                && !name.starts_with(|character: char| character.is_ascii_digit());
            (is_identifier && name.ends_with("State")).then(|| name.to_string())
        })
        .collect()
}

/// A filesystem-safe stem for a row id like `navigation-estimator/production`.
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

pub(crate) fn describe(command: &Command) -> String {
    let mut parts = vec![command.get_program().to_string_lossy().into_owned()];
    parts.extend(
        command
            .get_args()
            .map(|argument| argument.to_string_lossy().into_owned()),
    );
    parts.join(" ")
}

/// A child's stderr then stdout, with ANSI colouring removed.
///
/// The compiler colours its diagnostics whether or not it is writing to a
/// terminal, and a failure report is read out of a CI log as often as off a
/// console, so the escapes are stripped before the text is ever quoted.
pub(crate) fn combined(output: &std::process::Output) -> String {
    let mut text = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    text.push_str(&strip_ansi(&String::from_utf8_lossy(&output.stdout)));
    text
}

/// Remove ANSI SGR sequences.
///
/// The sibling corpus gate applies the same rule to the same compiler's output
/// and carries its own copy; the two want to be one helper, which is a move to
/// make when that module is not under concurrent edit.
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

/// The last few lines of child output: enough to identify the failure without
/// burying the report under a full diagnostic dump.
pub(crate) fn tail(text: &str) -> String {
    const LINES: usize = 12;
    let lines: Vec<&str> = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    let start = lines.len().saturating_sub(LINES);
    lines[start..].join("\n")
}

pub(crate) fn indent(text: &str) -> String {
    text.lines()
        .map(|line| format!("      {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}
