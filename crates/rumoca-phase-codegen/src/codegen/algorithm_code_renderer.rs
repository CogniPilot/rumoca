//! Reusable checked Algorithm Code renderer for multi-file targets.

use minijinja::Value;
use rumoca_core::TargetInvocationBrand;
use rumoca_ir_galec::TracedAlgorithmCodeProduct;

use super::{CodegenError, target_template_environment};
use crate::{
    AlgorithmCodeTemplateFile, PreparedCorrelatedAlgorithmCodeTemplateFile,
    PreparedPackagedAlgorithmCodeTemplateFile, RenderedAlgorithmCodeSourceFile,
    RenderedCorrelatedAlgorithmCodeFile, RenderedPackagedAlgorithmCodeFile, TemplateBindings,
};

#[cfg(test)]
mod tests;

/// One immutable source-only Algorithm Code template context.
///
/// Constructing the presentation view borrows the checked package's retained
/// syntax and artifact facts; serializing it creates an owned MiniJinja value
/// graph. Neither fact changes between files in one target invocation, so
/// rebuilding that graph per file would be redundant work.
#[derive(Debug)]
pub struct AlgorithmCodeTemplateRenderer<'inv> {
    _brand: TargetInvocationBrand<'inv>,
    context: Value,
}

impl<'inv> AlgorithmCodeTemplateRenderer<'inv> {
    /// Render one source-only member while retaining its exact checked file
    /// witness through the caller's checksum close.
    pub fn render_file<'member, 'body>(
        &self,
        file: &'member AlgorithmCodeTemplateFile<'inv, 'body>,
        artifact: &TemplateBindings<'inv>,
    ) -> Result<RenderedAlgorithmCodeSourceFile<'inv, 'member, 'body>, CodegenError> {
        let content = self.render_content(file, artifact)?;
        Ok(RenderedAlgorithmCodeSourceFile::construct(
            self._brand,
            file,
            content,
        ))
    }

    /// Build the shared context for one target invocation.
    ///
    /// The product is the sole trace authority. No source map or model name is
    /// accepted at render time, so an artifact cannot be rebound to a map that
    /// merely happens to contain the same stable source ids.
    ///
    /// ```compile_fail
    /// use rumoca_core::SourceMap;
    /// use rumoca_ir_galec::TracedAlgorithmCodeProduct;
    /// use rumoca_phase_codegen::AlgorithmCodeTemplateRenderer;
    /// fn substitute<'inv>(
    ///     product: &TracedAlgorithmCodeProduct<'inv>,
    ///     wrong_map: &SourceMap,
    /// ) {
    ///     let _ = AlgorithmCodeTemplateRenderer::new(product, wrong_map);
    /// }
    /// ```
    pub fn new(product: &TracedAlgorithmCodeProduct<'inv>) -> Result<Self, CodegenError> {
        let view = crate::views::algorithm_code::AlgorithmCodeSourceView::new(product);
        Ok(Self {
            _brand: product.brand(),
            context: minijinja::context! {
                algorithm_code => Value::from_serialize(view),
                ir_kind => "algorithm_code",
            },
        })
    }

    /// Render the checked output path without exposing Algorithm Code
    /// semantics to path interpolation.
    pub fn render_output_path(
        &self,
        file: &AlgorithmCodeTemplateFile<'inv, '_>,
    ) -> Result<String, CodegenError> {
        Ok(file.output_path().as_str().to_owned())
    }

    /// Render one checked Algorithm Code source body.
    ///
    /// The file plan has already proved the artifact/context relation and
    /// owns the template source.  This API therefore has neither a raw source
    /// argument nor a generic serializable context escape.
    ///
    /// ```compile_fail
    /// use rumoca_phase_codegen::{
    ///     AlgorithmCodeTemplateRenderer, TemplateBindings,
    /// };
    /// fn bypass(
    ///     renderer: &AlgorithmCodeTemplateRenderer<'_>,
    ///     bindings: &TemplateBindings<'_>,
    /// ) {
    ///     let _ = renderer.render_content("copied source", "Model", bindings);
    /// }
    /// ```
    ///
    /// ```compile_fail
    /// use rumoca_phase_codegen::{
    ///     AlgorithmCodeTemplateFile, AlgorithmCodeTemplateRenderer,
    ///     TemplateBindings,
    /// };
    /// fn mix_session<'render, 'session>(
    ///     renderer: &AlgorithmCodeTemplateRenderer<'render>,
    ///     file: &AlgorithmCodeTemplateFile<'render, '_>,
    ///     bindings: &TemplateBindings<'session>,
    /// ) {
    ///     let _ = renderer.render_content(file, bindings);
    /// }
    /// ```
    pub fn render_content(
        &self,
        file: &AlgorithmCodeTemplateFile<'inv, '_>,
        artifact: &TemplateBindings<'inv>,
    ) -> Result<String, CodegenError> {
        render_template(
            file.body_template(),
            artifact.render_context(minijinja::context! {
                ..self.context.clone()
            })?,
        )
    }
}

/// Render one standalone packaged Algorithm Code file already bound to its
/// exact semantic owner and retained package member. The carrier is the sole
/// source of both the semantic package and output path; this function accepts
/// neither an independent package nor a caller-selected name/path.
pub fn render_packaged_algorithm_code_file<'inv, 'package>(
    file: PreparedPackagedAlgorithmCodeTemplateFile<'inv, 'package, '_>,
    artifact: &TemplateBindings<'inv>,
) -> Result<RenderedPackagedAlgorithmCodeFile<'inv, 'package>, CodegenError> {
    let package = file.package();
    let view = crate::views::algorithm_code::AlgorithmCodeView::new(
        package.traced_product(),
        package.artifact_layout(),
    )
    .map_err(CodegenError::template)?;
    let member = file.member();
    let content = render_template(
        file.body_template(),
        artifact.render_context(minijinja::context! {
            algorithm_code => Value::from_serialize(view),
            ir_kind => "algorithm_code",
        })?,
    )?;
    Ok(RenderedPackagedAlgorithmCodeFile::construct(
        package.brand(),
        member,
        content,
    ))
}

/// Render one Algorithm Code-side file already bound to the exact correlated
/// Algorithm/Production owner and package member. The carrier is the sole
/// source of both semantic facts and the output path; this function accepts
/// neither an independent package nor a caller-selected name/path.
pub fn render_correlated_algorithm_code_file<'inv, 'production>(
    file: PreparedCorrelatedAlgorithmCodeTemplateFile<'inv, 'production, '_>,
    artifact: &TemplateBindings<'inv>,
) -> Result<RenderedCorrelatedAlgorithmCodeFile<'inv, 'production>, CodegenError> {
    let view = crate::views::algorithm_code::CorrelatedAlgorithmCodeView::new(file.production())
        .map_err(CodegenError::template)?;
    let member = file.member();
    let content = render_template(
        file.body_template(),
        artifact.render_context(minijinja::context! {
            algorithm_code => Value::from_serialize(view),
            ir_kind => "algorithm_code",
        })?,
    )?;
    Ok(RenderedCorrelatedAlgorithmCodeFile::construct(
        file.production().brand(),
        member,
        content,
    ))
}

fn render_template(template: &str, context: Value) -> Result<String, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    Ok(tmpl.render(context)?)
}
