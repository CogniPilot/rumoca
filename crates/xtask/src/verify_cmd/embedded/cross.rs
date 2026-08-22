//! Cross-compiling one row's C and reading the three numbers off it.
//!
//! # Warnings are failures
//!
//! The generated C advertises warning-free compilation, so the gate holds the
//! cross compiler to that: any output at all from `arm-none-eabi-gcc` fails the
//! row, whatever its exit status. `-Werror` is deliberately not added, because
//! the flags are the measured baseline's flags exactly and a gate that compiled
//! with a different command line would report sizes nobody can reproduce with
//! the documented one.
//!
//! # Why `.text` and not the file size
//!
//! An object file carries relocations, debug sections, and a symbol table, none
//! of which reach the flash. `.text` per translation unit, summed over the
//! artifact, is the number an integrator has to find room for, and it is what
//! `arm-none-eabi-size` reports in its first column.

use anyhow::{Context, Result, bail, ensure};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use super::emit::{Emission, combined, describe, indent, tail};
use super::symbols::{ObjectSymbols, parse_undefined};
use super::toolchain::{ArmToolchain, CROSS_COMPILE_FLAGS};
use super::verdict::Sizes;

/// The name the sizeof probe's storage is given. Long and namespaced so it
/// cannot collide with anything the generated header declares.
const PROBE_SYMBOL: &str = "rumoca_embedded_budget_state_probe";

/// Everything measured off one row's cross-compiled objects.
pub(crate) struct Measurement {
    pub(crate) sizes: Sizes,
    pub(crate) undefined: Vec<ObjectSymbols>,
    /// The cross-compile command line, for the report and for reproduction.
    pub(crate) cross_command_line: String,
}

/// Build every emitted source, size it, probe the state struct, and read the
/// undefined-symbol set.
pub(crate) fn measure(
    emission: &Emission,
    toolchain: &ArmToolchain,
    work_dir: &Path,
) -> Result<Measurement> {
    fs::create_dir_all(work_dir)
        .with_context(|| format!("failed to create {}", work_dir.display()))?;
    let cross_command_line = compile(&emission.sources, emission, toolchain, work_dir)?;
    let objects = object_paths(&emission.sources, work_dir)?;
    let units = size_units(&objects, toolchain)?;
    let state_bytes = probe_state_size(emission, toolchain, work_dir)?;
    let undefined = read_undefined(&objects, toolchain)?;
    Ok(Measurement {
        sizes: Sizes { units, state_bytes },
        undefined,
        cross_command_line,
    })
}

/// Run the baseline cross-compile over `sources`, in `work_dir` so the `.o`
/// files land there. Returns the command line that was run.
fn compile(
    sources: &[PathBuf],
    emission: &Emission,
    toolchain: &ArmToolchain,
    work_dir: &Path,
) -> Result<String> {
    let mut command = Command::new(toolchain.gcc());
    command.args(CROSS_COMPILE_FLAGS);
    for directory in &emission.include_dirs {
        command.arg("-I").arg(directory);
    }
    command.args(sources).current_dir(work_dir);
    let command_line = describe(&command);
    let output = command
        .output()
        .with_context(|| format!("failed to run `{command_line}`"))?;
    let text = combined(&output);
    if !output.status.success() {
        bail!(
            "the cross compiler refused the emitted C\n    reproduce: {command_line}\n{}",
            indent(&tail(&text))
        );
    }
    // Warning-free compilation is one of the artifact's advertised properties,
    // so a zero exit with output on the console is still a failed row.
    ensure!(
        text.trim().is_empty(),
        "the cross compiler warned on the emitted C, and the artifact advertises \
         warning-free compilation\n    reproduce: {command_line}\n{}",
        indent(&tail(&text))
    );
    Ok(command_line)
}

/// The `.o` each source produced, in source order. Objects land in one flat
/// work directory keyed by file stem, so two sources sharing a stem would
/// silently overwrite one another; refuse that layout instead of measuring it.
fn object_paths(sources: &[PathBuf], work_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut stems = std::collections::BTreeSet::new();
    for source in sources {
        let stem = source
            .file_stem()
            .with_context(|| format!("{} has no file stem", source.display()))?;
        ensure!(
            stems.insert(stem.to_os_string()),
            "two emitted sources share the object stem {}; the size accounting \
             cannot tell their objects apart",
            Path::new(stem).display()
        );
    }
    sources
        .iter()
        .map(|source| {
            let stem = source
                .file_stem()
                .with_context(|| format!("{} has no file stem", source.display()))?;
            let object = work_dir.join(Path::new(stem).with_extension("o"));
            ensure!(
                object.is_file(),
                "the cross compiler exited zero but produced no {} for {}",
                object.display(),
                source.display()
            );
            Ok(object)
        })
        .collect()
}

fn size_units(objects: &[PathBuf], toolchain: &ArmToolchain) -> Result<Vec<(String, u64)>> {
    objects
        .iter()
        .map(|object| Ok((object_name(object), text_size(object, toolchain)?)))
        .collect()
}

fn object_name(object: &Path) -> String {
    object
        .file_name()
        .unwrap_or(object.as_os_str())
        .to_string_lossy()
        .into_owned()
}

/// `.text` of one object, from `arm-none-eabi-size`'s Berkeley format.
///
/// The listing is a header line then one row per object: `text data bss dec hex
/// filename`. Only the first column is read; `data` and `bss` for these
/// artifacts are structurally zero and are covered by the state probe instead.
fn text_size(object: &Path, toolchain: &ArmToolchain) -> Result<u64> {
    let mut command = Command::new(toolchain.size());
    command.arg(object);
    let listing = run_capturing(&mut command)?;
    let row = listing
        .lines()
        .nth(1)
        .with_context(|| format!("`{}` printed no size row", describe(&command)))?;
    let field = row
        .split_whitespace()
        .next()
        .with_context(|| format!("`{}` printed an empty size row", describe(&command)))?;
    field
        .parse()
        .with_context(|| format!("`{}` printed `{field}` for .text", describe(&command)))
}

/// `sizeof(<Model>State)` for the target ABI.
///
/// Measured rather than computed: a probe translation unit declares one object
/// of exactly that size, and `nm` reports the size the cross compiler gave it.
/// That number includes the target's own alignment padding, which is the point.
/// Reading it back from `nm` rather than from a `static_assert` bisection keeps
/// the measurement exact and the failure message a number.
fn probe_state_size(emission: &Emission, toolchain: &ArmToolchain, work_dir: &Path) -> Result<u64> {
    let header = emission
        .state_header
        .file_name()
        .with_context(|| format!("{} has no file name", emission.state_header.display()))?
        .to_string_lossy()
        .into_owned();
    let probe = work_dir.join("rumoca_embedded_budget_probe.c");
    fs::write(
        &probe,
        format!(
            "/* Sizing probe for the embedded budget gate: one object of exactly\n \
             * sizeof({state}) bytes, whose size the cross toolchain reports back. */\n\
             #include \"{header}\"\n\
             char {PROBE_SYMBOL}[sizeof({state})];\n",
            state = emission.state_type,
        ),
    )
    .with_context(|| format!("failed to write {}", probe.display()))?;
    compile(std::slice::from_ref(&probe), emission, toolchain, work_dir)?;
    let object = work_dir.join("rumoca_embedded_budget_probe.o");
    symbol_size(&object, toolchain)
}

/// The size `nm --print-size` reports for the probe object.
fn symbol_size(object: &Path, toolchain: &ArmToolchain) -> Result<u64> {
    let mut command = Command::new(toolchain.nm());
    command
        .arg("--print-size")
        .arg("--radix=d")
        .arg("--defined-only")
        .arg(object);
    let listing = run_capturing(&mut command)?;
    for line in listing.lines() {
        let fields: Vec<&str> = line.split_whitespace().collect();
        // `value size type name`, with the size column present because
        // --print-size was asked for.
        if fields.len() == 4 && fields[3] == PROBE_SYMBOL {
            return fields[1].parse().with_context(|| {
                format!("`{}` printed `{}` as a size", describe(&command), fields[1])
            });
        }
    }
    bail!(
        "`{}` did not report a size for `{PROBE_SYMBOL}`, so the state struct could not be \
         measured:\n{}",
        describe(&command),
        indent(&tail(&listing))
    )
}

fn read_undefined(objects: &[PathBuf], toolchain: &ArmToolchain) -> Result<Vec<ObjectSymbols>> {
    objects
        .iter()
        .map(|object| {
            let mut command = Command::new(toolchain.nm());
            command.arg("--undefined-only").arg(object);
            let listing = run_capturing(&mut command)?;
            Ok(ObjectSymbols {
                object: object_name(object),
                undefined: parse_undefined(&listing),
            })
        })
        .collect()
}

/// Run a reader tool and return its stdout, refusing a nonzero exit.
fn run_capturing(command: &mut Command) -> Result<String> {
    let command_line = describe(command);
    let output = command
        .output()
        .with_context(|| format!("failed to run `{command_line}`"))?;
    ensure!(
        output.status.success(),
        "`{command_line}` failed\n{}",
        indent(&tail(&combined(&output)))
    );
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}
