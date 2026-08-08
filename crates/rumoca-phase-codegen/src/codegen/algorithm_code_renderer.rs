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
    pub fn new(package: &AlgorithmCodePackage) -> Result<Self, CodegenError> {
        let view = crate::views::algorithm_code::AlgorithmCodeView::new(package)
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
