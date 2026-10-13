//! Metadata-only rendering from the checked native deployment profile.

use super::{CodegenError, create_environment, solve_lazy};
use minijinja::Value;
use rumoca_ir_solve::fmi::FmiNativeDeploymentView;

pub struct NativeFmiTemplateRenderer {
    fmi: Value,
}

impl NativeFmiTemplateRenderer {
    pub fn new(view: FmiNativeDeploymentView) -> Result<Self, CodegenError> {
        Ok(Self {
            fmi: solve_lazy::native_fmi_value(view)?,
        })
    }
    pub fn render_with_name_and_artifact<T: serde::Serialize>(
        &self,
        template: &str,
        model_name: &str,
        artifact: &T,
    ) -> Result<String, CodegenError> {
        let mut environment = create_environment();
        environment.add_template("inline", template)?;
        Ok(environment
            .get_template("inline")?
            .render(minijinja::context! {
                fmi => self.fmi.clone(), model_name => model_name,
                artifact => Value::from_serialize(artifact)
            })?)
    }
}
