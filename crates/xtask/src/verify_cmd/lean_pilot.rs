//! Experimental replay of one production-derived Rust-to-Lean proof.

mod external_inventory;

use anyhow::{Context, Result, ensure};
use clap::Args;
use sha2::{Digest as _, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const PILOT: &str = "infra/verification/lean-pilot";
const SOURCES: &[&str] = &[
    "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement.rs",
    "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement/diagnostics.rs",
    "crates/rumoca-phase-solve/src/variable_catalog_refinement.rs",
    // Reached by the closed DAE view-to-facts projection, not the DAE root.
    "crates/rumoca-ir-dae/src/model/view.rs",
    "crates/rumoca-ir-dae/src/ids.rs",
    "crates/rumoca-ir-dae/src/model.rs",
    "crates/rumoca-ir-dae/src/expression/value_types.rs",
    "crates/rumoca-ir-dae/src/expression.rs",
    "crates/rumoca-ir-dae/src/discrete_values.rs",
    "crates/rumoca-core/src/lib.rs",
    // Reached by the projection roots. Declared here so their bytes are bound
    // before extraction rather than discovered in its output.
    "crates/rumoca-core/src/structured_domain.rs",
    "crates/rumoca-ir-solve/src/linear_op.rs",
    "crates/rumoca-ir-solve/src/typed_program/call.rs",
    "crates/rumoca-ir-solve/src/typed_program/types.rs",
    // Reached by the outer selector roots.
    "crates/rumoca-ir-solve/src/lib.rs",
    "crates/rumoca-ir-solve/src/tensor.rs",
    "crates/rumoca-ir-solve/src/structural_pattern.rs",
    "crates/rumoca-core/src/ir_primitives.rs",
    // The actual SolveModel root reaches these additional type/getter owners.
    "crates/rumoca-core/src/clock_lattice.rs",
    "crates/rumoca-core/src/ir_primitives/var_name.rs",
    "crates/rumoca-core/src/matrix_multiply.rs",
    "crates/rumoca-ir-solve/src/layout.rs",
    "crates/rumoca-ir-solve/src/model.rs",
    "crates/rumoca-ir-solve/src/model/event_transaction.rs",
    "crates/rumoca-ir-solve/src/refresh.rs",
    "crates/rumoca-ir-solve/src/typed_program/program.rs",
    "crates/rumoca-ir-solve/src/typed_program/reduction.rs",
    "crates/rumoca-ir-solve/src/variable_catalog.rs",
];
const SUBJECTS: &[&str] = &[
    "rumoca_phase_solve::scalar_constant_derivative_refinement::check_kernel",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::exact_program",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::check_owner_counts",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::check_full_jacobian",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::check_visible_rows",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::check_scalar_constant_derivative_refinement",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::operation_fact",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::project_operations",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::project_scalar_block",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::project_kernel",
    "rumoca_phase_solve::scalar_constant_derivative_refinement::project_solve_values_with_owner_premises",
    "rumoca_ir_solve::model::*::variable_refinement",
    "rumoca_phase_solve::variable_catalog_refinement::project_solve_variables",
    "rumoca_phase_solve::variable_catalog_refinement::project_dae_variables",
    "rumoca_core::structured_domain::checked_extent_product",
    "rumoca_ir_dae::expression::value_types::*::scalar_count",
    "rumoca_phase_solve::variable_catalog_refinement::expected_state_initialization",
    "rumoca_phase_solve::variable_catalog_refinement::expected_variability",
    "rumoca_phase_solve::variable_catalog_refinement::expected_causality",
    "rumoca_phase_solve::variable_catalog_refinement::expected_kind",
    "rumoca_phase_solve::variable_catalog_refinement::check_dimensions",
    "rumoca_phase_solve::variable_catalog_refinement::check_variable",
    "rumoca_phase_solve::variable_catalog_refinement::derive_storage",
    "rumoca_ir_dae::model::view::optional_expression_id",
];

// Aeneas uses its own name-pattern grammar, distinct from Charon's selectors.
const RETAINED_FUNCTIONS: &[&str] = &[
    "rumoca_ir_dae::model::view::optional_expression_id",
    "rumoca_ir_solve::model::{rumoca_ir_solve::model::SolveModel}::variable_refinement",
    "rumoca_ir_dae::expression::value_types::{rumoca_ir_dae::expression::value_types::ValueType}::scalar_count",
];

/// Crates whose encountered type bodies are extracted rather than supplied as
/// opaque models. The explicit external inventory remains authoritative for
/// library types and functions outside those captured first-party definitions.
const TRANSPARENT_CRATES: &[&str] = &["rumoca_ir_dae", "rumoca_ir_solve", "rumoca_core"];

/// Hand-written models, copied in only after the inventory admits the
/// boundary they belong to.
const MODELS: &[&str] = &["TypesExternal.lean", "FunsExternal.lean"];

/// Translated files the split backend emits for these roots. The `_Template`
/// files it also writes are deliberately absent: they are the translator's
/// request for a model, and the inventory decides whether that request is
/// permitted before any model is placed.
const GENERATED_PARTS: &[&str] = &["Types.lean", "Funs.lean"];

const PROOF_MODULES: &[&str] = &[
    "RumocaKernelPilot",
    "RumocaFactContract",
    "RumocaCompleteFactContract",
    "RumocaProjectionContract",
    "RumocaRootValueContract",
    "RumocaRootValueFixture",
    "RumocaRootValueWitness",
    "RumocaVariableViewContract",
    "RumocaSolveFactContract",
    "RumocaDaeFactContract",
    "RumocaExtentCountContract",
    "RumocaVariableClassificationContract",
    "RumocaDimensionCheckContract",
    "RumocaVariableEqualityContract",
    "RumocaVariableCheckContract",
    "RumocaOccurrenceContract",
    "RumocaStorageContract",
];
const PROOF_INPUTS: &[&str] = &["lakefile.toml", "lake-manifest.json", "lean-toolchain"];
const TOOL_INPUTS: &[&str] = &[
    "flake.nix",
    "flake.lock",
    "infra/verification/aeneas/package.nix",
    "infra/verification/aeneas/keep-function-roots.patch",
    "infra/verification/aeneas/preserve-shared-aliases.patch",
    "infra/verification/aeneas/preserve-stored-borrows.patch",
    "infra/verification/charon/package.nix",
    "infra/verification/charon/reconstruct-box-borrows.patch",
];

/// Reported Rust source identities and their physical paths in Charon's pinned
/// toolchain. These are captured inputs, not exemptions from source binding.
const TOOLCHAIN_SOURCES: &[(&str, &str)] = &[(
    "/rustc/library/core/src/marker.rs",
    "lib/rustlib/src/rust/library/core/src/marker.rs",
)];

/// The patched package reports both its complete upstream revision and the
/// SHA-256 of all three patches. The pin test binds that response to the checked-in
/// patches; the replay also captures their bytes.
const AENEAS_COMMIT: &str = "f9a8e338188447c77f31246892cb9a7a742e58ef";
const AENEAS_RESPONSE: &str = "aeneas f9a8e338188447c77f31246892cb9a7a742e58ef-rumoca-9977c3508c94785b30cc7247fb34f3393c66dd8ffcfafba9b8d0110a7462685f-alias-2eb1a079b9d0651fbd735874d20d4d42ff93610d2830dd4f0923e75deee1535f-stored-ca4d7deda43a23c507127ae8fe50c067b11e1fb2ae1a03c483dbb004d19c028a";
const CHARON_COMMIT: &str = "b82d2748c1e5bfd9519cd9401f9186d33b44a7f1";
const CHARON_RESPONSE: &str = "0.1.248 (b82d2748c1e5bfd9519cd9401f9186d33b44a7f1-rumoca-642c212103a0e5198acfe97fecc14075114d7e03f3950c5fb41d463653fa3659)";

/// What each pinned tool must answer.
///
/// Extraction output depends on all four tools, so a different one silently
/// changes what the proofs are about.
///
/// This is pin enforcement, not binary attestation. A tool that misreports
/// its own version defeats every entry here, as does substitution after the
/// check. Lean binds its release, not its commit or host metadata. An
/// accidentally different tool stops the replay instead of silently changing
/// the extracted subject.
const VERSION_PINS: &[VersionPin] = &[
    VersionPin {
        directory: ToolDirectory::Aeneas,
        program: "aeneas",
        argument: "-version",
        expected: ExpectedResponse::WholeResponse(AENEAS_RESPONSE),
        full_revision: Some(AENEAS_COMMIT),
    },
    VersionPin {
        directory: ToolDirectory::Aeneas,
        program: "charon",
        argument: "version",
        expected: ExpectedResponse::WholeResponse(CHARON_RESPONSE),
        full_revision: Some(CHARON_COMMIT),
    },
    VersionPin {
        directory: ToolDirectory::Aeneas,
        program: "charon",
        argument: "toolchain-version",
        expected: ExpectedResponse::WholeResponse("nightly-2026-08-18"),
        full_revision: None,
    },
    VersionPin {
        directory: ToolDirectory::Lean,
        program: "lean",
        argument: "--version",
        // Lean reports the host triple and its own commit alongside the
        // version, and both vary without changing the pinned toolchain, so only
        // the `version` field is bound.
        expected: ExpectedResponse::LeanVersionField("4.31.0"),
        full_revision: None,
    },
];

#[derive(Clone, Copy)]
enum ToolDirectory {
    Aeneas,
    Lean,
}

struct VersionPin {
    directory: ToolDirectory,
    program: &'static str,
    argument: &'static str,
    expected: ExpectedResponse,
    /// The full source revision this response identifies, when the pilot
    /// README records one. Named in the refusal for an operator comparing a
    /// rebuilt tool against its source revision.
    full_revision: Option<&'static str>,
}

/// How a response is bound.
///
/// Neither variant searches the whole response for the pinned text. Scanning
/// anywhere would accept a wrong main version that happens to carry the
/// expected token in a build note or commit hash, which is the substring hazard
/// one level up.
#[derive(Clone, Copy)]
enum ExpectedResponse {
    /// The entire trimmed response must equal this, so nothing unexamined
    /// remains in it.
    WholeResponse(&'static str),
    /// The response must be Lean's `Lean (version <expected>, ...)` report, and
    /// only that field is compared. Deliberately specific to the one tool whose
    /// response carries a varying host triple and commit.
    LeanVersionField(&'static str),
}

#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct VerifyLeanPilotArgs {
    /// Directory containing the pinned Aeneas and Charon executables (see pilot README).
    #[arg(long, value_name = "DIR")]
    aeneas_bin: PathBuf,
    /// Directory containing the pinned Lean, Lake and leantar executables.
    #[arg(long, value_name = "DIR")]
    lean_bin: PathBuf,
}

pub(super) fn run(root: &Path, args: &VerifyLeanPilotArgs) -> Result<()> {
    let pilot = root.join(PILOT);
    let output = root.join("target/verification/lean-pilot");
    fs::create_dir_all(&output)?;
    let lock = fs::File::create(output.join("replay.lock"))?;
    lock.try_lock()
        .context("another Lean pilot replay is running")?;
    let aeneas_bin = args.aeneas_bin.canonicalize()?;
    let lean_bin = args.lean_bin.canonicalize()?;
    report_versions(&aeneas_bin, &lean_bin)?;
    let mut sources = capture_inputs(root, &pilot)?;
    let toolchain = capture_tool_response(&aeneas_bin.join("charon"), "toolchain-path")?;
    sources.extend(capture_toolchain_inputs(Path::new(toolchain.trim()))?);
    let bound_sources: Vec<_> = SOURCES
        .iter()
        .copied()
        .chain(TOOLCHAIN_SOURCES.iter().map(|(reported, _)| *reported))
        .collect();

    let extraction = tempfile::Builder::new()
        .prefix("extraction-")
        .tempdir_in(&output)?;
    let llbc = extraction.path().join("rumoca_phase_solve.llbc");
    extract(root, &aeneas_bin, &output, &llbc)?;
    ensure!(llbc.is_file(), "Charon did not produce the selected kernel");
    let fresh_generated = extraction.path().join("generated");
    let mut translate = Command::new(aeneas_bin.join("aeneas"));
    translate
        .current_dir(root)
        .args([
            "-backend",
            "lean",
            "-split-files",
            "-emit-json",
            "-gen-lib-entry",
            "-dest",
        ])
        .arg(&fresh_generated);
    for function in RETAINED_FUNCTIONS {
        translate.args(["-keep-function", function]);
    }
    translate.arg(&llbc);
    crate::run_status(translate)?;

    // The boundary is admitted before any hand-written model is placed, so a
    // new external definition refuses the replay rather than acquiring a model.
    // Sources are the bytes already bound at entry, so a first-party definition
    // cannot come from a file this replay never captured.
    external_inventory::check(&fresh_generated.join("translation.json"), &bound_sources)
        .context("the translation crossed its declared external boundary")?;

    // Split translation emits one file per category; the pilot builds them as
    // submodules of `RumocaPhaseSolve`, so the hand-written models replace the
    // templates under the same module path.
    let module_dir = pilot.join("generated/RumocaPhaseSolve");
    fs::create_dir_all(&module_dir)?;
    for part in GENERATED_PARTS {
        let produced = fresh_generated.join(part);
        ensure!(
            produced.is_file(),
            "Aeneas did not produce {part} for the selected roots"
        );
        let placed = module_dir.join(part);
        fs::copy(&produced, &placed).with_context(|| format!("place {}", placed.display()))?;
        sources.push((placed.clone(), fs::read(&placed)?));
    }
    for model in MODELS {
        let placed = module_dir.join(model);
        fs::copy(pilot.join("models").join(model), &placed)
            .with_context(|| format!("place model {}", placed.display()))?;
        sources.push((placed.clone(), fs::read(&placed)?));
    }
    let entry = pilot.join("generated/RumocaPhaseSolve.lean");
    fs::copy(fresh_generated.join("RumocaPhaseSolve.lean"), &entry)
        .context("Aeneas did not produce a library entry for the selected roots")?;
    sources.push((entry.clone(), fs::read(&entry)?));

    let mut paths = vec![lean_bin.clone()];
    if let Some(existing) = std::env::var_os("PATH") {
        paths.extend(std::env::split_paths(&existing));
    }
    let mut proof = Command::new(lean_bin.join("lake"));
    proof
        .current_dir(&pilot)
        .env("PATH", std::env::join_paths(paths)?)
        .arg("build")
        .args(PROOF_MODULES);
    crate::run_status(proof)?;
    ensure_unchanged(&sources)?;
    for (path, bytes) in &sources {
        println!("SHA-256 {:x} {}", Sha256::digest(bytes), path.display());
    }
    println!(
        "Pilot replay passed; extraction: {}",
        extraction.keep().display()
    );
    println!(
        "This is not golden admission, nor a proof of whole-root projection, \
         production runtime behavior, or floating-point arithmetic."
    );
    Ok(())
}

fn extract(root: &Path, tools: &Path, output: &Path, llbc: &Path) -> Result<()> {
    let mut command = Command::new(tools.join("charon"));
    command
        .current_dir(root)
        .env("CARGO_TARGET_DIR", output.join("cargo"))
        .args(["cargo", "--preset=aeneas"]);
    for subject in SUBJECTS {
        command.args(["--start-from", subject]);
    }
    for krate in TRANSPARENT_CRATES {
        command.args(["--include", krate]);
    }
    command.arg("--dest-file").arg(llbc).args([
        "--",
        "-p",
        "rumoca-phase-solve",
        "--lib",
        "--locked",
    ]);
    crate::run_status(command)
}

fn report_versions(aeneas_bin: &Path, lean_bin: &Path) -> Result<()> {
    for pin in VERSION_PINS {
        let directory = match pin.directory {
            ToolDirectory::Aeneas => aeneas_bin,
            ToolDirectory::Lean => lean_bin,
        };
        let program = directory.join(pin.program);
        let stdout = capture_tool_response(&program, pin.argument)?;
        check_pinned_response(&stdout, pin.expected).with_context(|| match pin.full_revision {
            Some(revision) => format!(
                "{} {} (pinned source revision {revision})",
                program.display(),
                pin.argument
            ),
            None => format!("{} {}", program.display(), pin.argument),
        })?;
        println!(
            "pinned {} {}: {}",
            program.display(),
            pin.argument,
            stdout.trim()
        );
    }
    Ok(())
}

/// Run one tool query and return its UTF-8 standard output alone.
///
/// The streams are kept apart rather than concatenated. Joining them can splice
/// a matching identifier across the boundary out of two halves that neither
/// stream contained, which would be a pin that passes on text no tool ever
/// emitted. All four pinned tools answer on stdout; one that moved its answer to
/// stderr fails this check rather than being silently accommodated.
fn capture_tool_response(program: &Path, argument: &str) -> Result<String> {
    let mut command = Command::new(program);
    command.arg(argument);
    crate::resource_budget::apply_to_child(&mut command);
    let rendered = format!("{command:?}");
    let output = command
        .output()
        .with_context(|| format!("failed to run tool query: {rendered}"))?;
    ensure!(
        output.status.success(),
        "tool query failed (status={}): {rendered}; stderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr).trim()
    );
    String::from_utf8(output.stdout).context("tool query returned non-UTF-8 output")
}

/// Refuse a response that is not the pinned one.
fn check_pinned_response(stdout: &str, expected: ExpectedResponse) -> Result<()> {
    let reported = stdout.trim();
    match expected {
        ExpectedResponse::WholeResponse(pin) => {
            ensure!(
                !reported.is_empty(),
                "tool reported no version text on stdout; expected exactly `{pin}`"
            );
            ensure!(
                reported == pin,
                "pinned tool version mismatch: expected exactly `{pin}`, tool reported `{reported}`"
            );
        }
        ExpectedResponse::LeanVersionField(pin) => {
            let field = version_field(reported).with_context(|| {
                format!("expected a `version {pin}` field, tool reported `{reported}`")
            })?;
            ensure!(
                field == pin,
                "pinned tool version mismatch: expected version field `{pin}`, \
                 tool reported version field `{field}` in `{reported}`"
            );
        }
    }
    Ok(())
}

/// Read Lean's version field out of its known response grammar.
///
/// Keyword searching was wrong three ways and each is now a refusal case:
/// `conversion` contains `version`; a `-rc1` suffix split at the hyphen and
/// passed as the release; and a malformed leading field was skipped instead of
/// refused, letting a later one stand in for it. So the response is parsed as
/// the grammar Lean actually emits, `Lean (version <field>, <rest>)`, and the
/// field is taken whole up to its required comma, suffixes included.
fn version_field(reported: &str) -> Result<&str> {
    const PREFIX: &str = "Lean (version ";
    let rest = reported
        .strip_prefix(PREFIX)
        .with_context(|| format!("response does not begin with `{PREFIX}`"))?;
    let (field, remainder) = rest
        .split_once(',')
        .context("version field is not terminated by the required comma")?;
    ensure!(!field.is_empty(), "version field is empty");
    ensure!(
        !field.contains(char::is_whitespace),
        "version field contains whitespace, so the response is not the expected grammar"
    );
    ensure!(
        remainder.ends_with(')'),
        "response is not the expected parenthesised Lean report"
    );
    Ok(field)
}

fn capture_inputs(root: &Path, pilot: &Path) -> Result<Vec<(PathBuf, Vec<u8>)>> {
    let mut paths: Vec<_> = SOURCES.iter().map(|name| root.join(name)).collect();
    paths.push(root.join("Cargo.lock"));
    paths.extend(TOOL_INPUTS.iter().map(|name| root.join(name)));
    paths.extend(
        PROOF_MODULES
            .iter()
            .map(|name| pilot.join(format!("{name}.lean"))),
    );
    paths.extend(PROOF_INPUTS.iter().map(|name| pilot.join(name)));
    // Hand-written models are trusted base: bind their original bytes at entry
    // so `ensure_unchanged` also proves they were not edited mid-replay.
    paths.extend(MODELS.iter().map(|name| pilot.join("models").join(name)));
    paths
        .into_iter()
        .map(|path| {
            let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
            Ok((path, bytes))
        })
        .collect()
}

fn capture_toolchain_inputs(toolchain: &Path) -> Result<Vec<(PathBuf, Vec<u8>)>> {
    ensure!(
        toolchain.is_absolute(),
        "Charon toolchain path must be absolute"
    );
    TOOLCHAIN_SOURCES
        .iter()
        .map(|(_, relative)| {
            let path = toolchain.join(relative);
            let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
            Ok((path, bytes))
        })
        .collect()
}

fn ensure_unchanged(inputs: &[(PathBuf, Vec<u8>)]) -> Result<()> {
    for (path, bytes) in inputs {
        ensure!(
            fs::read(path)? == *bytes,
            "proof input changed during replay: {}",
            path.display()
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn toolchain_source_is_required_and_checked_for_changes() {
        let directory = tempfile::tempdir().expect("temporary toolchain");
        assert!(capture_toolchain_inputs(Path::new("relative/toolchain")).is_err());
        assert!(capture_toolchain_inputs(directory.path()).is_err());
        let path = directory.path().join(TOOLCHAIN_SOURCES[0].1);
        fs::create_dir_all(path.parent().expect("source directory")).expect("source tree");
        fs::write(&path, b"original Rust source").expect("source fixture");
        let snapshot = capture_toolchain_inputs(directory.path()).expect("bound Rust source");
        assert_eq!(snapshot.len(), 1);
        assert_eq!(snapshot[0].1, b"original Rust source");
        assert!(ensure_unchanged(&snapshot).is_ok());
        fs::write(&path, b"changed Rust source").expect("source mutation");
        assert!(ensure_unchanged(&snapshot).is_err());
        fs::remove_file(&path).expect("remove source fixture");
        assert!(ensure_unchanged(&snapshot).is_err());
    }

    #[cfg(unix)]
    #[test]
    fn toolchain_source_symlink_retarget_is_not_hidden_by_canonicalization() {
        let directory = tempfile::tempdir().expect("temporary toolchain");
        let original = directory.path().join("original.rs");
        let replacement = directory.path().join("replacement.rs");
        fs::write(&original, b"original").expect("original source");
        fs::write(&replacement, b"replacement").expect("replacement source");
        let path = directory.path().join(TOOLCHAIN_SOURCES[0].1);
        fs::create_dir_all(path.parent().expect("source directory")).expect("source tree");
        std::os::unix::fs::symlink(&original, &path).expect("source link");
        let snapshot = capture_toolchain_inputs(directory.path()).expect("bound source link");
        fs::remove_file(&path).expect("unlink source fixture");
        std::os::unix::fs::symlink(&replacement, &path).expect("retarget source link");
        assert!(ensure_unchanged(&snapshot).is_err());
    }

    #[test]
    fn changed_or_deleted_proof_input_refuses() {
        let directory = tempfile::tempdir().expect("temporary directory");
        let path = directory.path().join("proof.lean");
        fs::write(&path, b"original").expect("fixture");
        let snapshot = vec![(path.clone(), b"original".to_vec())];
        assert!(ensure_unchanged(&snapshot).is_ok());
        fs::write(&path, b"changed").expect("mutation");
        assert!(ensure_unchanged(&snapshot).is_err());
        fs::remove_file(&path).expect("delete fixture");
        assert!(ensure_unchanged(&snapshot).is_err());
    }

    /// The exact responses the installed pinned tools give. Recorded from the
    /// real binaries, not invented, so a pin that drifts from the tools fails
    /// here rather than in a replay.
    const REAL_AENEAS: &str = "aeneas f9a8e338188447c77f31246892cb9a7a742e58ef-rumoca-9977c3508c94785b30cc7247fb34f3393c66dd8ffcfafba9b8d0110a7462685f-alias-2eb1a079b9d0651fbd735874d20d4d42ff93610d2830dd4f0923e75deee1535f-stored-ca4d7deda43a23c507127ae8fe50c067b11e1fb2ae1a03c483dbb004d19c028a\n";
    const REAL_CHARON: &str = "0.1.248 (b82d2748c1e5bfd9519cd9401f9186d33b44a7f1-rumoca-642c212103a0e5198acfe97fecc14075114d7e03f3950c5fb41d463653fa3659)\n";
    const REAL_CHARON_TOOLCHAIN: &str = "nightly-2026-08-18\n";
    const REAL_LEAN: &str = "Lean (version 4.31.0, x86_64-unknown-linux-gnu, \
                             commit 68218e876d2a38b1985b8590fff244a83c321783, Release)\n";

    fn expectation(program: &str, argument: &str) -> ExpectedResponse {
        VERSION_PINS
            .iter()
            .find(|pin| pin.program == program && pin.argument == argument)
            .expect("pinned tool query")
            .expected
    }

    #[test]
    fn every_pinned_tool_accepts_its_real_response() {
        for (program, argument, response) in [
            ("aeneas", "-version", REAL_AENEAS),
            ("charon", "version", REAL_CHARON),
            ("charon", "toolchain-version", REAL_CHARON_TOOLCHAIN),
            ("lean", "--version", REAL_LEAN),
        ] {
            check_pinned_response(response, expectation(program, argument))
                .unwrap_or_else(|error| panic!("{program} {argument}: {error:?}"));
        }
    }

    #[test]
    fn aeneas_identity_binds_the_source_revision_and_patch_bytes() {
        let ExpectedResponse::WholeResponse(response) = expectation("aeneas", "-version") else {
            panic!("aeneas pin binds a whole response");
        };
        let root_patch =
            include_bytes!("../../../../infra/verification/aeneas/keep-function-roots.patch");
        let alias_patch =
            include_bytes!("../../../../infra/verification/aeneas/preserve-shared-aliases.patch");
        let stored_patch =
            include_bytes!("../../../../infra/verification/aeneas/preserve-stored-borrows.patch");
        assert_eq!(
            response,
            format!(
                "aeneas {AENEAS_COMMIT}-rumoca-{:x}-alias-{:x}-stored-{:x}",
                Sha256::digest(root_patch),
                Sha256::digest(alias_patch),
                Sha256::digest(stored_patch)
            )
        );
    }

    #[test]
    fn unpatched_or_differently_patched_aeneas_is_refused() {
        let root_digest = Sha256::digest(include_bytes!(
            "../../../../infra/verification/aeneas/keep-function-roots.patch"
        ));
        let alias_digest = Sha256::digest(include_bytes!(
            "../../../../infra/verification/aeneas/preserve-shared-aliases.patch"
        ));
        let stored_digest = Sha256::digest(include_bytes!(
            "../../../../infra/verification/aeneas/preserve-stored-borrows.patch"
        ));
        for response in [
            "aeneas f9a8e33\n".to_owned(),
            format!("aeneas {AENEAS_COMMIT}-rumoca-{root_digest:x}\n"),
            format!("aeneas {AENEAS_COMMIT}-rumoca-{root_digest:x}-alias-{alias_digest:x}\n"),
            format!(
                "aeneas {AENEAS_COMMIT}-rumoca-{root_digest:x}-alias-{}-stored-{stored_digest:x}\n",
                "0".repeat(64)
            ),
            format!(
                "aeneas {AENEAS_COMMIT}-rumoca-{}-alias-{alias_digest:x}-stored-{stored_digest:x}\n",
                "0".repeat(64)
            ),
            format!(
                "aeneas {AENEAS_COMMIT}-rumoca-{root_digest:x}-alias-{alias_digest:x}-stored-{}\n",
                "0".repeat(64)
            ),
        ] {
            assert!(check_pinned_response(&response, expectation("aeneas", "-version")).is_err());
        }
    }

    #[test]
    fn charon_identity_binds_the_source_revision_and_patch_bytes() {
        let ExpectedResponse::WholeResponse(response) = expectation("charon", "version") else {
            panic!("charon pin binds a whole response");
        };
        let patch =
            include_bytes!("../../../../infra/verification/charon/reconstruct-box-borrows.patch");
        assert_eq!(
            response,
            format!(
                "0.1.248 ({CHARON_COMMIT}-rumoca-{:x})",
                Sha256::digest(patch)
            )
        );
    }

    #[test]
    fn unpatched_or_differently_patched_charon_is_refused() {
        for response in [
            format!("0.1.248 ({CHARON_COMMIT})\n"),
            format!("0.1.248 ({CHARON_COMMIT}-rumoca-{})\n", "0".repeat(64)),
        ] {
            assert!(check_pinned_response(&response, expectation("charon", "version")).is_err());
        }
    }

    #[test]
    fn tool_packages_and_patches_are_captured_inputs() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(Path::parent)
            .expect("workspace root");
        let inputs = capture_inputs(root, &root.join(PILOT)).expect("capture proof inputs");
        for relative in [
            "infra/verification/aeneas/package.nix",
            "infra/verification/aeneas/keep-function-roots.patch",
            "infra/verification/aeneas/preserve-shared-aliases.patch",
            "infra/verification/aeneas/preserve-stored-borrows.patch",
            "infra/verification/charon/package.nix",
            "infra/verification/charon/reconstruct-box-borrows.patch",
        ] {
            let path = root.join(relative);
            assert!(inputs.iter().any(|(captured, _)| captured == &path));
        }
    }

    #[test]
    fn missing_version_response_is_refused() {
        for response in ["", "   \n\t "] {
            assert!(check_pinned_response(response, expectation("aeneas", "-version")).is_err());
            assert!(check_pinned_response(response, expectation("lean", "--version")).is_err());
        }
    }

    #[test]
    fn wrong_version_response_is_refused() {
        assert!(
            check_pinned_response("aeneas 0000000\n", expectation("aeneas", "-version")).is_err()
        );
        assert!(
            check_pinned_response(
                "nightly-2026-02-27\n",
                expectation("charon", "toolchain-version")
            )
            .is_err()
        );
        assert!(
            check_pinned_response(
                "Lean (version 4.30.0, x86_64-unknown-linux-gnu, commit abcdef, Release)\n",
                expectation("lean", "--version"),
            )
            .is_err()
        );
    }

    #[test]
    fn malformed_version_response_is_refused() {
        assert!(
            check_pinned_response("error: unknown flag\n", expectation("aeneas", "-version"))
                .is_err()
        );
        // No `version` field at all.
        assert!(check_pinned_response("Lean 4.31.0\n", expectation("lean", "--version")).is_err());
        // A `version` keyword with nothing after it.
        assert!(
            check_pinned_response("Lean (version )\n", expectation("lean", "--version")).is_err()
        );
    }

    /// The control that motivates field anchoring: a wrong main version whose
    /// response carries the expected text somewhere else must still be refused.
    #[test]
    fn expected_text_elsewhere_in_a_wrong_response_is_refused() {
        assert!(
            check_pinned_response(
                "Lean (version 4.30.0, x86_64-unknown-linux-gnu, commit 4.31.0abc, Release)\n",
                expectation("lean", "--version"),
            )
            .is_err()
        );
        assert!(
            check_pinned_response(
                "aeneas 0000000 (was f9a8e33)\n",
                expectation("aeneas", "-version"),
            )
            .is_err()
        );
        assert!(
            check_pinned_response(
                "0.1.999 (b82d2748c1e5bfd9519cd9401f9186d33b44a7f1)\n",
                expectation("charon", "version"),
            )
            .is_err()
        );
    }

    /// The three mechanism-proved counterexamples against keyword searching.
    /// Each passed the previous parser; each must now be refused.
    #[test]
    fn lean_grammar_refuses_keyword_search_counterexamples() {
        // A release candidate is not the release: the suffix is retained, not
        // split off at the hyphen.
        assert!(
            check_pinned_response(
                "Lean (version 4.31.0-rc1, x86_64-unknown-linux-gnu, commit abc, Release)\n",
                expectation("lean", "--version"),
            )
            .is_err()
        );
        // `conversion` contains `version`.
        assert!(
            check_pinned_response("conversion 4.31.0\n", expectation("lean", "--version")).is_err()
        );
        // A malformed leading field must refuse, not be skipped so a later one
        // can stand in for it.
        assert!(
            check_pinned_response(
                "Lean (version ???, then version 4.31.0, Release)\n",
                expectation("lean", "--version"),
            )
            .is_err()
        );
    }

    /// The response must be the whole expected report, not a fragment that
    /// happens to start correctly.
    #[test]
    fn lean_grammar_requires_the_full_report_shape() {
        assert!(
            check_pinned_response("Lean (version 4.31.0\n", expectation("lean", "--version"))
                .is_err()
        );
        assert!(
            check_pinned_response(
                "Lean (version 4.31.0, Release\n",
                expectation("lean", "--version")
            )
            .is_err()
        );
        assert!(
            check_pinned_response(
                "prefix Lean (version 4.31.0, Release)\n",
                expectation("lean", "--version")
            )
            .is_err()
        );
    }

    /// A whole-response pin leaves nothing unexamined: trailing text is refused
    /// even when the expected identity is present and correct.
    #[test]
    fn trailing_text_after_a_whole_response_is_refused() {
        assert!(
            check_pinned_response(
                "aeneas f9a8e33 (patched)\n",
                expectation("aeneas", "-version"),
            )
            .is_err()
        );
    }
}
