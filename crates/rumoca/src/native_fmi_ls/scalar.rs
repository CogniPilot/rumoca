//! Closed source-correlated construction and publication of a stateful
//! (scalar-product) FMU through the same fixed Co-Simulation target.

use anyhow::{Result, bail};
use rumoca_core::artifact_build::PreparedBuildRequest;
use rumoca_exec_wasm::FixedNativeGuestCore;
use rumoca_ir_solve::{RetainedAssertionRoot, fmi::FmiScalarDeploymentView};
use rumoca_phase_codegen::NativeFmiTemplateRenderer;
use rumoca_sim::{LinkedScalarFmiLsComponent, PreparedScalarFmiLsModel};
use std::{collections::BTreeMap, path::Path};

use super::prepare_metadata_with;
use crate::packaging::{ArtifactSession, PreparedTargetPackage};

/// Same-source scalar Co-Simulation metadata, the actual linked scalar
/// component and the exact original package binding. There is no arbitrary
/// request, XML, bytes or publication-token constructor.
pub struct ScalarFmiLsPackage {
    package: PreparedTargetPackage,
    component: LinkedScalarFmiLsComponent<ArtifactSession>,
    access: FmiScalarDeploymentView,
    root: RetainedAssertionRoot,
    /// Whether the linked core is the interpreted scalar product's, which
    /// compiles reference execution and is never published.
    interpreted: bool,
}

impl ScalarFmiLsPackage {
    /// Derive every publication input from the consumed scalar source pair.
    /// The scalar view refuses any advertised variable the scalar route has
    /// no owner for, so no unsupported variable is published.
    pub fn construct(
        model: PreparedScalarFmiLsModel,
        core: FixedNativeGuestCore,
        model_name: &str,
    ) -> Result<Self> {
        let interpreted = core.is_scalar_product();
        Self::construct_with(model, model_name, interpreted, |model, request| {
            Ok(model.link_fixed_guest(core, request)?)
        })
    }

    /// [`Self::construct`] for the scalar-linked product: the scalar-linked
    /// core is linked with the modules and dispatch of `artifact`, the linked
    /// artifact one SDK export issued beside the `saved` product. The product
    /// is certified once here, through `certify_core`, before it can be
    /// published; the published guest binds it by position at load.
    pub fn construct_linked(
        model: PreparedScalarFmiLsModel,
        core: FixedNativeGuestCore,
        certify_core: FixedNativeGuestCore,
        model_name: &str,
        artifact: &[u8],
        saved: &[u8],
    ) -> Result<Self> {
        let interpreted = core.is_scalar_product();
        Self::construct_with(model, model_name, interpreted, |model, request| {
            Ok(model.link_linked_guest(core, certify_core, artifact, saved, request)?)
        })
    }

    fn construct_with(
        model: PreparedScalarFmiLsModel,
        model_name: &str,
        interpreted: bool,
        link: impl FnOnce(
            PreparedScalarFmiLsModel,
            &PreparedBuildRequest<'_, ArtifactSession>,
        ) -> Result<LinkedScalarFmiLsComponent<ArtifactSession>>,
    ) -> Result<Self> {
        let root = model.root().clone();
        let access = FmiScalarDeploymentView::from_root(&root)?;
        let package = prepare_metadata_with(
            NativeFmiTemplateRenderer::new_scalar(access.clone())?,
            model_name,
        )?;
        let request = package.build_request("component")?;
        let component = link(model, &request)?;
        check_correlation(&package, &component, &access, &root)?;
        Ok(Self {
            package,
            component,
            access,
            root,
            interpreted,
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
    /// The interpreted scalar product is refused here, before any archive is
    /// written: only the compiled linked product, certified at build, publishes.
    pub fn publish(self, output: &Path) -> Result<()> {
        refuse_interpreted_publication(self.interpreted)?;
        check_correlation(&self.package, &self.component, &self.access, &self.root)?;
        let Self {
            package, component, ..
        } = self;
        package.publish(
            BTreeMap::from([("component".to_owned(), component.into_bytes())]),
            output,
        )
    }
}

/// The publication boundary of the scalar route: a core that compiles
/// reference execution (the interpreted `scalar-product`) never publishes.
fn refuse_interpreted_publication(interpreted: bool) -> Result<()> {
    if interpreted {
        bail!(
            "the interpreted scalar product is not published: \
             publish the scalar-linked product, certified at build"
        );
    }
    Ok(())
}

fn check_correlation(
    package: &PreparedTargetPackage,
    component: &LinkedScalarFmiLsComponent<ArtifactSession>,
    access: &FmiScalarDeploymentView,
    root: &RetainedAssertionRoot,
) -> Result<()> {
    if !package.accepts_build(component.binding()) {
        bail!("scalar component belongs to a different publication inventory");
    }
    if !access.root().shares_root_owner(root)
        || !std::ptr::eq(access.root().metadata(), root.metadata())
    {
        bail!("scalar metadata belongs to a different source");
    }
    Ok(())
}

#[cfg(test)]
mod publication_tests {
    use super::refuse_interpreted_publication;

    #[test]
    fn only_the_compiled_product_reaches_publication() {
        assert!(refuse_interpreted_publication(false).is_ok());
        let refused = refuse_interpreted_publication(true).expect_err("refused");
        assert!(
            refused.to_string().contains("interpreted scalar product is not published"),
            "{refused}"
        );
    }
}
