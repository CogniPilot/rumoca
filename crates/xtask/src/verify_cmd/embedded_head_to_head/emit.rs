//! Fresh compiler and comparator emission. No pre-generated Rumoca C enters.

use super::artifact_guard::FrozenArtifactSet;
use super::cross;
use super::process;
use super::snapshot::AuthenticatedInputs;
use super::typed_path::{
    CasadiEmissionInclude, CasadiGeneratedC, CasadiGeneratedH, ComparatorWrapper, RolePath,
    RumocaCacheDirectory, RumocaEmissionInclude, RumocaHeader, RumocaKernels, RumocaKernelsHeader,
    RumocaModel,
};
use anyhow::{Context, Result, bail, ensure};
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

const RUMOCA_ROSTER: [&str; 5] = [
    ".clang-format",
    "ExpMixedStep.c",
    "ExpMixedStep.h",
    "rumoca_galec_kernels.c",
    "rumoca_galec_kernels.h",
];

pub(super) struct RumocaEmission {
    pub(super) frozen: FrozenArtifactSet,
    pub(super) include: RolePath<RumocaEmissionInclude>,
    pub(super) model_c: RolePath<RumocaModel>,
    pub(super) model_h: RolePath<RumocaHeader>,
    pub(super) kernels_c: RolePath<RumocaKernels>,
    pub(super) kernels_h: RolePath<RumocaKernelsHeader>,
    pub(super) command: process::CommandReceipt,
}

pub(super) struct CasadiEmission {
    pub(super) frozen: FrozenArtifactSet,
    pub(super) include: RolePath<CasadiEmissionInclude>,
    pub(super) generated_c: RolePath<CasadiGeneratedC>,
    pub(super) generated_h: RolePath<CasadiGeneratedH>,
    pub(super) wrapper_c: RolePath<ComparatorWrapper>,
    pub(super) command: process::CommandReceipt,
}

pub(super) fn fresh_directory(path: &Path) -> Result<()> {
    if path.exists() {
        fs::remove_dir_all(path)
            .with_context(|| format!("failed to clear stale {}", path.display()))?;
    }
    fs::create_dir_all(path).with_context(|| format!("failed to create {}", path.display()))
}

pub(super) fn emit_rumoca(
    inputs: &AuthenticatedInputs,
    plan: &cross::BoundExecutionPlan,
    directory: &Path,
    cache: &RolePath<RumocaCacheDirectory>,
) -> Result<RumocaEmission> {
    fresh_directory(directory)?;
    let include = RolePath::checked(directory.to_path_buf())?;
    fs::create_dir_all(cache.as_path())
        .with_context(|| format!("failed to create {}", cache.as_path().display()))?;
    inputs.verify()?;
    let mut command = cross::rumoca_generation_command(plan, inputs.fixture(), cache, &include)?;
    let rendered =
        process::require_success(&mut command, "Rumoca emission", process::Limit::Generate);
    let post_use = inputs.verify().and_then(|()| cross::verify_compiler(plan));
    let rendered = finish_external_use(rendered, post_use)?;
    let attempted = rendered.clone();
    (|| {
        validate_rumoca_roster(directory)?;
        let model_c = RolePath::checked(directory.join("ExpMixedStep.c"))?;
        let frozen = FrozenArtifactSet::capture_tree(directory)?;
        Ok(RumocaEmission {
            frozen,
            include,
            model_c,
            model_h: RolePath::checked(directory.join("ExpMixedStep.h"))?,
            kernels_c: RolePath::checked(directory.join("rumoca_galec_kernels.c"))?,
            kernels_h: RolePath::checked(directory.join("rumoca_galec_kernels.h"))?,
            command: rendered,
        })
    })()
    .map_err(|error| process::attach_attempted_receipt(error, attempted))
}

pub(super) fn emit_casadi(
    inputs: &AuthenticatedInputs,
    plan: &cross::BoundExecutionPlan,
    directory: &Path,
) -> Result<CasadiEmission> {
    fresh_directory(directory)?;
    let include = RolePath::checked(directory.to_path_buf())?;
    inputs.verify()?;
    let mut command = cross::casadi_generation_command(plan, inputs.generator(), &include)?;
    let rendered = process::require_success(
        &mut command,
        "CasADi comparator generation",
        process::Limit::Generate,
    );
    let rendered = finish_external_use(rendered, inputs.verify())?;
    let attempted = rendered.clone();
    (|| {
        let generated_c = RolePath::checked(directory.join("casadi_exp_mixed.c"))?;
        let generated_h = RolePath::checked(directory.join("casadi_exp_mixed.h"))?;
        ensure!(
            generated_c.as_path().is_file() && generated_h.as_path().is_file(),
            "CasADi generator exited zero without both {} and {}",
            generated_c.as_path().display(),
            generated_h.as_path().display()
        );
        let files = regular_file_names(directory)?;
        let expected = BTreeSet::from([
            "casadi_exp_mixed.c".to_string(),
            "casadi_exp_mixed.h".to_string(),
        ]);
        ensure!(
            files == expected,
            "CasADi generator wrote an unexpected roster under {}: {files:?}",
            directory.display()
        );
        cross::validate_casadi_outputs(plan, generated_c.as_path(), generated_h.as_path())?;
        validate_casadi_zero_workspace_contract(generated_c.as_path(), generated_h.as_path())?;
        let frozen = FrozenArtifactSet::capture_tree(directory)?;
        Ok(CasadiEmission {
            frozen,
            include,
            generated_c,
            generated_h,
            wrapper_c: inputs.wrapper().clone(),
            command: rendered,
        })
    })()
    .map_err(|error| process::attach_attempted_receipt(error, attempted))
}

impl RumocaEmission {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }
}

impl CasadiEmission {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }
}

fn finish_external_use(
    command: Result<process::CommandReceipt>,
    post_use: Result<()>,
) -> Result<process::CommandReceipt> {
    match (command, post_use) {
        (Ok(receipt), Ok(())) => Ok(receipt),
        (Ok(receipt), Err(error)) => Err(process::attach_attempted_receipt(error, receipt)),
        (Err(error), _) => Err(error),
    }
}

pub(super) fn validate_casadi_zero_workspace_contract(
    generated_c: &Path,
    generated_h: &Path,
) -> Result<()> {
    let source = fs::read_to_string(generated_c)
        .with_context(|| format!("failed to read {}", generated_c.display()))?;
    let header = fs::read_to_string(generated_h)
        .with_context(|| format!("failed to read {}", generated_h.display()))?;
    for required in [
        "  if (sz_arg) *sz_arg = 4;",
        "  if (sz_res) *sz_res = 1;",
        "  if (sz_iw) *sz_iw = 0;",
        "  if (sz_w) *sz_w = 0;",
        "  if (sz_arg) *sz_arg = 4*sizeof(const casadi_real*);",
        "  if (sz_res) *sz_res = 1*sizeof(casadi_real*);",
        "  if (sz_iw) *sz_iw = 0*sizeof(casadi_int);",
        "  if (sz_w) *sz_w = 0*sizeof(casadi_real);",
    ] {
        ensure!(
            source.lines().filter(|line| *line == required).count() == 1,
            "pinned CasADi work contract lacks unique line `{required}`"
        );
    }
    for required in [
        "#define exp_mixed_full_SZ_ARG 4",
        "#define exp_mixed_full_SZ_RES 1",
        "#define exp_mixed_full_SZ_IW 0",
        "#define exp_mixed_full_SZ_W 0",
    ] {
        ensure!(
            header.lines().filter(|line| *line == required).count() == 1,
            "pinned CasADi header work contract lacks unique line `{required}`"
        );
    }
    Ok(())
}

fn validate_rumoca_roster(directory: &Path) -> Result<()> {
    let actual = regular_file_names(directory)?;
    let expected = RUMOCA_ROSTER
        .into_iter()
        .map(str::to_string)
        .collect::<BTreeSet<_>>();
    ensure!(
        actual == expected,
        "compiler emission roster under {} is {actual:?}, expected {expected:?}; stale, missing, \
         or extra output makes the row unmeasured",
        directory.display()
    );
    Ok(())
}

fn regular_file_names(directory: &Path) -> Result<BTreeSet<String>> {
    let mut names = BTreeSet::new();
    for item in fs::read_dir(directory)
        .with_context(|| format!("failed to read {}", directory.display()))?
    {
        let path = item?.path();
        if !path.is_file() {
            bail!(
                "unexpected non-file output {} under {}",
                path.display(),
                directory.display()
            );
        }
        let name = path
            .file_name()
            .context("emitted path has no file name")?
            .to_string_lossy()
            .into_owned();
        names.insert(name);
    }
    Ok(names)
}
