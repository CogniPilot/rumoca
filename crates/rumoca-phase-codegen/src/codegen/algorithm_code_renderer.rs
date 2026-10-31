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
    /// Built from a refined Solve block: its C model units carry the
    /// template's unused-marker candidates for the read pass (#483).
    solve_c: bool,
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
            solve_c: false,
        })
    }

    /// [`Self::new`] plus the package's refined Solve block: the context of a
    /// target that prints its C from Solve IR (#483, DAE → GALEC → Solve IR →
    /// C). `block` must be the refinement of `package`.
    pub fn with_solve_block(
        package: &AlgorithmCodePackage,
        block: &rumoca_ir_solve::SolveAlgorithmBlock,
        sources: &rumoca_core::SourceMap,
    ) -> Result<Self, CodegenError> {
        let view = crate::views::algorithm_code::AlgorithmCodeView::new(package, sources)
            .map_err(CodegenError::template)?;
        Ok(Self {
            context: minijinja::context! {
                algorithm_code => Value::from_serialize(view),
                solve_block => super::solve_block_renderer::solve_block_value(block, sources)?,
                ir_kind => "algorithm_code",
            },
            solve_c: true,
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
        self.render_context(
            template,
            minijinja::context! {
                model_name => model_name,
                artifact => Value::from_serialize(artifact),
                ..self.context.clone()
            },
        )
    }

    /// Render source text without packaging artifact facts. Templates that
    /// require timestamps, checksums or container identities are unavailable
    /// at this boundary and fail under the renderer's strict undefined policy.
    pub fn render_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CodegenError> {
        self.render_context(
            template,
            minijinja::context! {
                model_name => model_name,
                ..self.context.clone()
            },
        )
    }

    fn render_context(&self, template: &str, context: Value) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        let text = tmpl.render(context)?;
        Ok(if self.solve_c {
            super::solve_block_renderer::drop_needless_void_uses(&text)
        } else {
            text
        })
    }
}

#[cfg(test)]
mod tests {
    use super::AlgorithmCodeTemplateRenderer;

    /// The unused-marker pass belongs to the Solve C boundary only: any other
    /// render, including a custom template that prints the candidate form
    /// literally, is returned as the template rendered it.
    #[test]
    fn only_a_solve_renderer_resolves_unused_marker_candidates() {
        let template = "void f(void) {\n    float rumoca_galec_r0[1] = {0}; (void)rumoca_galec_r0; \
                        /*rumoca_galec_void_candidate*/\n    g(rumoca_galec_r0);\n}\n";
        let render = |solve_c| {
            AlgorithmCodeTemplateRenderer {
                context: minijinja::context! {},
                solve_c,
            }
            .render_context(template, minijinja::context! {})
            .expect("renders")
        };
        assert_eq!(render(false), template);
        assert_eq!(
            render(true),
            "void f(void) {\n    float rumoca_galec_r0[1] = {0};\n    g(rumoca_galec_r0);\n}\n"
        );
    }
}
