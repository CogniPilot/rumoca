//! Closed source-correlated direct-native FMU construction and publication.

use anyhow::{Context, Result, bail};
use rumoca_exec_wasm::FixedNativeGuestCore;
use rumoca_ir_solve::fmi::{FmiCarryPair, FmiNativeDeploymentView};
use rumoca_phase_codegen::NativeFmiTemplateRenderer;
use rumoca_sim::{LinkedNativeFmiLsComponent, PreparedNativeFmiLsModel};
use std::{collections::BTreeMap, path::Path};

use crate::packaging::{ArtifactSession, PreparedTargetPackage};

const TARGET: &str = include_str!("native_fmi_ls/target.toml");
const XML: &str =
    include_str!("../../rumoca-phase-codegen/src/templates/fmi3/modelDescription.xml.jinja");
const LAYERED: &str =
    include_str!("../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/manifest.xml.jinja");

/// Same-source metadata, actual linked component and exact original package binding.
/// There is no arbitrary request, XML, bytes or publication-token constructor.
pub struct NativeFmiLsPackage {
    package: PreparedTargetPackage,
    component: LinkedNativeFmiLsComponent<ArtifactSession>,
    access: FmiNativeDeploymentView,
}

impl NativeFmiLsPackage {
    /// Derive every publication input internally from the consumed native source pair.
    /// The fixed core is built once by its existing source-owned builder, not here.
    ///
    /// `carried` declares the host contract that feeds an output to an input
    /// between evaluations (SPEC_0040 SOLVE-C101): the pairs are validated
    /// against the model's storage here and recorded in `modelDescription.xml`,
    /// so a host reads the pairing from the package.
    pub fn construct(
        model: PreparedNativeFmiLsModel,
        core: FixedNativeGuestCore,
        model_name: &str,
        carried: &[FmiCarryPair],
    ) -> Result<Self> {
        let inputs = model.library().inputs();
        let (access, _) = FmiNativeDeploymentView::from_preparation(inputs)?
            .declaring_carried(inputs.schedule(), carried)?;
        let package = prepare_metadata(&access, model_name)?;
        let request = package.build_request("component")?;
        let component = model.link_fixed_guest(core, &request)?;
        check_correlation(&package, &component, &access)?;
        Ok(Self {
            package,
            component,
            access,
        })
    }

    pub fn artifact(&self) -> &ArtifactSession {
        self.package.artifact()
    }
    pub fn component_bytes(&self) -> &[u8] {
        self.component.bytes()
    }
    pub fn model_description(&self) -> &[u8] {
        self.package
            .rendered_file(Path::new("modelDescription.xml"))
            .expect("the closed target requires its original XML")
    }

    /// Publish only this facade's original metadata and linked input, atomically.
    pub fn publish(self, output: &Path) -> Result<()> {
        check_correlation(&self.package, &self.component, &self.access)?;
        let Self {
            package, component, ..
        } = self;
        package.publish(
            BTreeMap::from([("component".to_owned(), component.into_bytes())]),
            output,
        )
    }
}

fn check_correlation(
    package: &PreparedTargetPackage,
    component: &LinkedNativeFmiLsComponent<ArtifactSession>,
    access: &FmiNativeDeploymentView,
) -> Result<()> {
    if !package.accepts_build(component.binding()) {
        bail!("native component belongs to a different publication inventory");
    }
    let root = component.original_model().original().inputs().root();
    if !access.root().shares_root_owner(root)
        || !std::ptr::eq(access.root().metadata(), root.metadata())
    {
        bail!("native metadata belongs to a different emitted source");
    }
    Ok(())
}

fn prepare_metadata(access: &FmiNativeDeploymentView, name: &str) -> Result<PreparedTargetPackage> {
    let manifest = rumoca_compile::codegen::targets::parse_target_manifest(TARGET)?;
    let artifact = ArtifactSession::new(&manifest.files)?;
    let renderer = NativeFmiTemplateRenderer::new(access.clone())?;
    let render = |source: &str| -> Result<String> {
        Ok(renderer.render_with_name_and_artifact(source, name, &artifact)?)
    };
    let files = manifest
        .files
        .iter()
        .map(|file| {
            let source = match file.template.as_str() {
                "metadata" => XML,
                "layered" => LAYERED,
                _ => unreachable!("closed native target has only canonical metadata templates"),
            };
            Ok((render(&file.path)?, render(source)?.into_bytes()))
        })
        .collect::<Result<Vec<_>>>()?;
    PreparedTargetPackage::construct(
        files,
        Vec::new(),
        manifest
            .package
            .as_ref()
            .context("native target package declaration")?,
        render,
        artifact.clone(),
    )
}

#[cfg(test)]
mod tests;
