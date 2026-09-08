//! Read-only packaging facts for Production Code XML.

use serde::Serialize;

use crate::{
    PreparedSolveAlgorithmProduction, ProductionArtifactRepresentationFile,
    ProductionManifestPresentation,
};

#[derive(Debug, Serialize)]
pub(super) struct ProductionCodeManifestView<'a> {
    identifiers: &'a ProductionManifestPresentation,
    header: ProductionCodeFileView<'a>,
    source: ProductionCodeFileView<'a>,
}

#[derive(Debug, Serialize)]
struct ProductionCodeFileView<'a> {
    layout: &'a ProductionArtifactRepresentationFile,
}

impl<'a> ProductionCodeManifestView<'a> {
    pub(super) fn construct(production: &'a PreparedSolveAlgorithmProduction<'_>) -> Self {
        let layout = production.presentation().artifact_layout();
        Self {
            identifiers: production.presentation().manifest(),
            header: ProductionCodeFileView {
                layout: layout.production_header_file(),
            },
            source: ProductionCodeFileView {
                layout: layout.production_source_file(),
            },
        }
    }
}
