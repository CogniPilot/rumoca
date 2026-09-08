//! Serialize-only renderer for checked Solve Algorithm Production C/H.

use serde::Serialize;

use super::{CodegenError, target_template_environment};
use crate::views::ProductionRealAbi;
use crate::{
    PreparedSolveAlgorithmDeclaration, PreparedSolveAlgorithmMethod,
    PreparedSolveAlgorithmProduction, PreparedSolveAlgorithmTemplateFile,
    ProductionPresentationPlan, RenderedProductionCodeFile, TemplateBindings,
};

mod production_code_manifest_view;
#[cfg(test)]
mod tests;

use production_code_manifest_view::ProductionCodeManifestView;

#[derive(Serialize)]
struct ProductionView<'a> {
    code_container: crate::ProductionCodeContainerProfile,
    real_abi: ProductionRealAbi,
    presentation: &'a ProductionPresentationPlan,
    declarations: &'a [PreparedSolveAlgorithmDeclaration],
    methods: &'a [PreparedSolveAlgorithmMethod],
    manifest: ProductionCodeManifestView<'a>,
}

impl<'a> ProductionView<'a> {
    fn construct(production: &'a PreparedSolveAlgorithmProduction<'_>) -> Self {
        Self {
            code_container: production.code_container(),
            real_abi: production.real_abi(),
            presentation: production.presentation(),
            declarations: production.declarations(),
            methods: production.methods(),
            manifest: ProductionCodeManifestView::construct(production),
        }
    }
}

/// Render one C/H file already bound to the owning, check-once Solve Algorithm
/// Production product. Rust supplies typed facts only; the bound Jinja file
/// owns every target-language token.
pub fn render_solve_algorithm_production_file<'inv, 'production>(
    file: PreparedSolveAlgorithmTemplateFile<'inv, 'production, '_>,
    artifact: &TemplateBindings<'inv>,
) -> Result<RenderedProductionCodeFile<'inv, 'production>, CodegenError> {
    let production = ProductionView::construct(file.production());
    let member = file.member();
    let content = render_template(
        "solve_algorithm_body",
        file.body_template(),
        artifact.render_context(minijinja::context! {
            production => minijinja::Value::from_serialize(&production),
        })?,
    )?;
    Ok(RenderedProductionCodeFile::construct(
        file.production().brand(),
        member,
        content,
    ))
}

fn render_template(
    name: &'static str,
    source: &str,
    context: minijinja::Value,
) -> Result<String, CodegenError> {
    let mut environment = target_template_environment();
    environment.add_template(name, source)?;
    Ok(environment.get_template(name)?.render(context)?)
}
