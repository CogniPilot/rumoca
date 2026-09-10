//! Reusable checked Algorithm Code renderer for multi-file targets.

use minijinja::Value;
use rumoca_ir_galec::package::AlgorithmCodePackage;

use super::{CodegenError, create_environment};

/// One immutable Algorithm Code template context, shared by every artifact in
/// a target invocation.
///
/// Constructing the typed view proves all target-facing expression shapes and
/// serializing it creates an owned MiniJinja value graph. Neither fact changes
/// between a package path, manifest, Algorithm Code file, or generated source,
/// so rebuilding that graph per file would be redundant work.
#[derive(Debug)]
pub struct AlgorithmCodeTemplateRenderer {
    context: Value,
}

impl AlgorithmCodeTemplateRenderer {
    /// Build the shared context for one target invocation.
    ///
    /// `sources` is the session source map the package's spans were created
    /// against; it is what lets the emitted artifacts carry `path:line:column`
    /// traceability instead of a bare source-id hash (SPEC_0034 GAL-032). A
    /// caller with no source map may pass an empty one, and every trace then
    /// degrades to its hash-and-byte-range form rather than inventing a path.
    pub fn new(
        package: &AlgorithmCodePackage,
        sources: &rumoca_core::SourceMap,
    ) -> Result<Self, CodegenError> {
        let view = crate::views::algorithm_code::AlgorithmCodeView::new(package, sources)
            .map_err(CodegenError::template)?;
        Ok(Self {
            context: minijinja::context! {
                algorithm_code => Value::from_serialize(view),
                ir_kind => "algorithm_code",
            },
        })
    }

    /// Render one target template while preserving the invocation-specific
    /// model identity and current checksum-web artifact facts.
    pub fn render_with_name_and_artifact<T: serde::Serialize>(
        &self,
        template: &str,
        model_name: &str,
        artifact: &T,
    ) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(minijinja::context! {
            model_name => model_name,
            artifact => Value::from_serialize(artifact),
            ..self.context.clone()
        })?)
    }
}
