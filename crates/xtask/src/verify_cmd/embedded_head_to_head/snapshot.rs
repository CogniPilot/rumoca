//! One-read authenticated snapshot of every checked benchmark input.

use super::artifact_bundle::Source;
use super::artifact_guard::FrozenArtifactSet;
use super::manifest::Entry;
use super::typed_path::{
    CasadiCorrectnessDriver, CasadiMeasuredDriver, ComparatorGenerator, ComparatorWrapper,
    CounterSource, FixtureSource, HarnessInclude, InputHeader, LinkerScript, RolePath,
    RumocaCorrectnessDriver, RumocaMeasuredDriver, StartupSource, TraceSource,
};
use anyhow::{Context, Result, ensure};
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs::{self, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

const HARNESS_ROSTER: [&str; 11] = [
    "counter_fixtures.c",
    "driver_casadi.c",
    "driver_casadi_correctness.c",
    "driver_rumoca.c",
    "driver_rumoca_correctness.c",
    "inputs.h",
    "linker.ld",
    "startup.S",
    "trace_markers.h",
    "trace_output.c",
    "trace_output.h",
];

pub(super) struct AuthenticatedInputs {
    root: PathBuf,
    closure_sha256: String,
    frozen: Arc<FrozenArtifactSet>,
    fixture: RolePath<FixtureSource>,
    generator: RolePath<ComparatorGenerator>,
    wrapper: RolePath<ComparatorWrapper>,
    harness: AuthenticatedHarness,
}

pub(super) struct AuthenticatedHarness {
    frozen: Arc<FrozenArtifactSet>,
    include: RolePath<HarnessInclude>,
    startup: RolePath<StartupSource>,
    trace: RolePath<TraceSource>,
    rumoca_measured: RolePath<RumocaMeasuredDriver>,
    rumoca_correctness: RolePath<RumocaCorrectnessDriver>,
    casadi_measured: RolePath<CasadiMeasuredDriver>,
    casadi_correctness: RolePath<CasadiCorrectnessDriver>,
    counter: RolePath<CounterSource>,
    linker: RolePath<LinkerScript>,
    input_header: RolePath<InputHeader>,
}

struct CapturedFile {
    relative: PathBuf,
    bytes: Vec<u8>,
}

struct CapturedInputs {
    fixture: CapturedFile,
    generator: CapturedFile,
    wrapper: CapturedFile,
    harness: Vec<CapturedFile>,
}

pub(super) fn stage(
    workspace: &Path,
    entry: &Entry,
    destination: &Path,
) -> Result<AuthenticatedInputs> {
    ensure!(
        workspace.is_absolute() && destination.is_absolute(),
        "workspace and snapshot destination must be absolute"
    );
    let captured = capture(workspace, entry)?;
    let closure_sha256 = captured_closure_digest(&captured)?;
    fresh_directory(destination)?;
    write_captured(destination, &captured)?;
    verify_staged(destination, &captured)?;
    let frozen = Arc::new(FrozenArtifactSet::capture_tree(destination)?);
    let authenticated = authenticated_paths(destination, closure_sha256, frozen)?;
    authenticated.verify_routes()?;
    Ok(authenticated)
}

impl AuthenticatedInputs {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }

    pub(super) fn closure_sha256(&self) -> &str {
        &self.closure_sha256
    }

    #[cfg(test)]
    pub(super) fn root(&self) -> &Path {
        &self.root
    }

    pub(super) fn fixture(&self) -> &RolePath<FixtureSource> {
        &self.fixture
    }

    pub(super) fn generator(&self) -> &RolePath<ComparatorGenerator> {
        &self.generator
    }

    pub(super) fn wrapper(&self) -> &RolePath<ComparatorWrapper> {
        &self.wrapper
    }

    pub(super) fn harness(&self) -> &AuthenticatedHarness {
        &self.harness
    }

    pub(super) fn artifact_sources(&self) -> Vec<Source> {
        vec![
            Source::new("inputs/model/ExpMixedStep.mo", self.fixture.as_path()),
            Source::new("inputs/casadi/gen_exp_mixed.py", self.generator.as_path()),
            Source::new("inputs/casadi/float_libm_wrapper.c", self.wrapper.as_path()),
            Source::new(
                "inputs/harness/counter_fixtures.c",
                self.harness.counter.as_path(),
            ),
            Source::new(
                "inputs/harness/driver_casadi.c",
                self.harness.casadi_measured.as_path(),
            ),
            Source::new(
                "inputs/harness/driver_casadi_correctness.c",
                self.harness.casadi_correctness.as_path(),
            ),
            Source::new(
                "inputs/harness/driver_rumoca.c",
                self.harness.rumoca_measured.as_path(),
            ),
            Source::new(
                "inputs/harness/driver_rumoca_correctness.c",
                self.harness.rumoca_correctness.as_path(),
            ),
            Source::new(
                "inputs/harness/inputs.h",
                self.harness.input_header.as_path(),
            ),
            Source::new("inputs/harness/linker.ld", self.harness.linker.as_path()),
            Source::new("inputs/harness/startup.S", self.harness.startup.as_path()),
            Source::new(
                "inputs/harness/trace_markers.h",
                &self.harness.include.as_path().join("trace_markers.h"),
            ),
            Source::new(
                "inputs/harness/trace_output.c",
                self.harness.trace.as_path(),
            ),
            Source::new(
                "inputs/harness/trace_output.h",
                &self.harness.include.as_path().join("trace_output.h"),
            ),
        ]
    }

    #[cfg(test)]
    pub(super) fn staged_paths(&self) -> Vec<&Path> {
        vec![
            self.fixture.as_path(),
            self.generator.as_path(),
            self.wrapper.as_path(),
            self.harness.startup.as_path(),
            self.harness.trace.as_path(),
            self.harness.rumoca_measured.as_path(),
            self.harness.rumoca_correctness.as_path(),
            self.harness.casadi_measured.as_path(),
            self.harness.casadi_correctness.as_path(),
            self.harness.counter.as_path(),
            self.harness.linker.as_path(),
            self.harness.input_header.as_path(),
        ]
    }

    fn verify_routes(&self) -> Result<()> {
        for path in [
            self.fixture.as_path(),
            self.generator.as_path(),
            self.wrapper.as_path(),
            self.harness.include.as_path(),
            self.harness.startup.as_path(),
            self.harness.trace.as_path(),
            self.harness.rumoca_measured.as_path(),
            self.harness.rumoca_correctness.as_path(),
            self.harness.casadi_measured.as_path(),
            self.harness.casadi_correctness.as_path(),
            self.harness.counter.as_path(),
            self.harness.linker.as_path(),
            self.harness.input_header.as_path(),
        ] {
            ensure!(
                path.starts_with(&self.root),
                "authenticated path escaped the gate-owned snapshot: {}",
                path.display()
            );
        }
        Ok(())
    }
}

impl AuthenticatedHarness {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }

    pub(super) fn include(&self) -> &RolePath<HarnessInclude> {
        &self.include
    }

    pub(super) fn startup(&self) -> &RolePath<StartupSource> {
        &self.startup
    }

    pub(super) fn trace(&self) -> &RolePath<TraceSource> {
        &self.trace
    }

    pub(super) fn rumoca_measured(&self) -> &RolePath<RumocaMeasuredDriver> {
        &self.rumoca_measured
    }

    pub(super) fn rumoca_correctness(&self) -> &RolePath<RumocaCorrectnessDriver> {
        &self.rumoca_correctness
    }

    pub(super) fn casadi_measured(&self) -> &RolePath<CasadiMeasuredDriver> {
        &self.casadi_measured
    }

    pub(super) fn casadi_correctness(&self) -> &RolePath<CasadiCorrectnessDriver> {
        &self.casadi_correctness
    }

    pub(super) fn counter(&self) -> &RolePath<CounterSource> {
        &self.counter
    }

    pub(super) fn linker(&self) -> &RolePath<LinkerScript> {
        &self.linker
    }
}

fn capture(workspace: &Path, entry: &Entry) -> Result<CapturedInputs> {
    let harness = harness_dir(workspace, entry)?;
    let input_header = workspace.join(&entry.input_header);
    ensure!(
        input_header == harness.join("inputs.h"),
        "checked input header {} is not the compiled driver header {}",
        input_header.display(),
        harness.join("inputs.h").display()
    );
    let fixture = captured_file(workspace.join(&entry.fixture), "fixture/ExpMixedStep.mo")?;
    let generator = captured_file(
        workspace.join(&entry.comparator_generator),
        "comparator/gen_exp_mixed.py",
    )?;
    let wrapper = captured_file(
        workspace.join(&entry.comparator_wrapper),
        "comparator/float_libm_wrapper.c",
    )?;
    let harness = capture_harness(&harness)?;
    verify_captured(entry, &fixture, &generator, &wrapper, &harness)?;
    Ok(CapturedInputs {
        fixture,
        generator,
        wrapper,
        harness,
    })
}

fn harness_dir(workspace: &Path, entry: &Entry) -> Result<PathBuf> {
    workspace
        .join(&entry.fixture)
        .parent()
        .map(|parent| parent.join("harness"))
        .context("fixture has no parent")
}

fn captured_file(path: PathBuf, relative: &str) -> Result<CapturedFile> {
    let mut file = File::open(&path)
        .with_context(|| format!("failed to open checked input {}", path.display()))?;
    ensure!(
        file.metadata()?.is_file(),
        "checked input is not a regular file: {}",
        path.display()
    );
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)
        .with_context(|| format!("failed to read checked input {}", path.display()))?;
    Ok(CapturedFile {
        relative: PathBuf::from(relative),
        bytes,
    })
}

fn capture_harness(directory: &Path) -> Result<Vec<CapturedFile>> {
    let mut names = fs::read_dir(directory)
        .with_context(|| format!("failed to read harness {}", directory.display()))?
        .map(|entry| entry.map(|value| value.file_name()))
        .collect::<std::io::Result<Vec<_>>>()?;
    names.sort();
    let observed = names
        .iter()
        .map(|name| name.to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    ensure!(
        observed == HARNESS_ROSTER,
        "harness roster is {observed:?}, expected {HARNESS_ROSTER:?}"
    );
    names
        .into_iter()
        .map(|name| {
            let name = name
                .into_string()
                .map_err(|_| anyhow::anyhow!("harness file name is not UTF-8"))?;
            captured_file(directory.join(&name), &format!("harness/{name}"))
        })
        .collect()
}

fn verify_captured(
    entry: &Entry,
    fixture: &CapturedFile,
    generator: &CapturedFile,
    wrapper: &CapturedFile,
    harness: &[CapturedFile],
) -> Result<()> {
    verify_digest("fixture", &fixture.bytes, &entry.fixture_sha256)?;
    verify_digest(
        "comparator generator",
        &generator.bytes,
        &entry.comparator_generator_sha256,
    )?;
    verify_digest(
        "comparator wrapper",
        &wrapper.bytes,
        &entry.comparator_wrapper_sha256,
    )?;
    let input = harness
        .iter()
        .find(|file| file.relative == Path::new("harness/inputs.h"))
        .context("captured harness has no inputs.h")?;
    verify_digest("input header", &input.bytes, &entry.input_sha256)?;
    ensure!(
        harness_digest(harness)? == entry.harness_sha256,
        "harness digest mismatch"
    );
    Ok(())
}

fn verify_digest(label: &str, bytes: &[u8], expected: &str) -> Result<()> {
    let observed = format!("{:x}", Sha256::digest(bytes));
    ensure!(observed == expected, "{label} digest mismatch: {observed}");
    Ok(())
}

fn harness_digest(files: &[CapturedFile]) -> Result<String> {
    let mut digest = Sha256::new();
    for file in files {
        let name = file
            .relative
            .file_name()
            .context("captured harness path has no file name")?;
        digest.update(name.as_encoded_bytes());
        digest.update([0]);
        digest.update(&file.bytes);
        digest.update([0]);
    }
    Ok(format!("{:x}", digest.finalize()))
}

fn captured_closure_digest(captured: &CapturedInputs) -> Result<String> {
    let mut digest = Sha256::new();
    for file in captured_files(captured) {
        let relative = file
            .relative
            .to_str()
            .context("captured input path is not UTF-8")?;
        digest.update(relative.as_bytes());
        digest.update([0]);
        digest.update(&file.bytes);
        digest.update([0]);
    }
    Ok(format!("{:x}", digest.finalize()))
}

fn write_captured(destination: &Path, captured: &CapturedInputs) -> Result<()> {
    for file in captured_files(captured) {
        let path = destination.join(&file.relative);
        fs::create_dir_all(path.parent().context("snapshot file has no parent")?)?;
        fs::write(&path, &file.bytes)
            .with_context(|| format!("failed to stage checked input {}", path.display()))?;
    }
    Ok(())
}

fn verify_staged(destination: &Path, captured: &CapturedInputs) -> Result<()> {
    let expected = captured_files(captured)
        .into_iter()
        .map(|file| file.relative.clone())
        .collect::<BTreeSet<_>>();
    let observed = staged_roster(destination)?;
    ensure!(
        observed == expected,
        "staged input roster differs from captured roster"
    );
    for file in captured_files(captured) {
        let path = destination.join(&file.relative);
        ensure!(
            fs::read(&path)? == file.bytes,
            "staged input bytes differ from the one-read capture: {}",
            path.display()
        );
    }
    Ok(())
}

fn captured_files(captured: &CapturedInputs) -> Vec<&CapturedFile> {
    [&captured.fixture, &captured.generator, &captured.wrapper]
        .into_iter()
        .chain(captured.harness.iter())
        .collect()
}

fn staged_roster(root: &Path) -> Result<BTreeSet<PathBuf>> {
    let mut files = BTreeSet::new();
    for directory in ["fixture", "comparator", "harness"] {
        let path = root.join(directory);
        for entry in fs::read_dir(&path)? {
            let entry = entry?;
            ensure!(
                entry.file_type()?.is_file(),
                "snapshot contains a non-file: {}",
                entry.path().display()
            );
            files.insert(PathBuf::from(directory).join(entry.file_name()));
        }
    }
    Ok(files)
}

fn authenticated_paths(
    root: &Path,
    closure_sha256: String,
    frozen: Arc<FrozenArtifactSet>,
) -> Result<AuthenticatedInputs> {
    let harness = root.join("harness");
    Ok(AuthenticatedInputs {
        root: root.to_path_buf(),
        closure_sha256,
        frozen: Arc::clone(&frozen),
        fixture: RolePath::checked(root.join("fixture/ExpMixedStep.mo"))?,
        generator: RolePath::checked(root.join("comparator/gen_exp_mixed.py"))?,
        wrapper: RolePath::checked(root.join("comparator/float_libm_wrapper.c"))?,
        harness: AuthenticatedHarness {
            frozen,
            include: RolePath::checked(harness.clone())?,
            startup: RolePath::checked(harness.join("startup.S"))?,
            trace: RolePath::checked(harness.join("trace_output.c"))?,
            rumoca_measured: RolePath::checked(harness.join("driver_rumoca.c"))?,
            rumoca_correctness: RolePath::checked(harness.join("driver_rumoca_correctness.c"))?,
            casadi_measured: RolePath::checked(harness.join("driver_casadi.c"))?,
            casadi_correctness: RolePath::checked(harness.join("driver_casadi_correctness.c"))?,
            counter: RolePath::checked(harness.join("counter_fixtures.c"))?,
            linker: RolePath::checked(harness.join("linker.ld"))?,
            input_header: RolePath::checked(harness.join("inputs.h"))?,
        },
    })
}

fn fresh_directory(path: &Path) -> Result<()> {
    if path.exists() {
        fs::remove_dir_all(path)
            .with_context(|| format!("failed to clear stale snapshot {}", path.display()))?;
    }
    fs::create_dir_all(path)
        .with_context(|| format!("failed to create snapshot {}", path.display()))
}
