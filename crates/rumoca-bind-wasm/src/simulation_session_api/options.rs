use crate::WasmError;

#[derive(Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(super) struct InteractiveOptions {
    pub dt: f64,
    pub solver: String,
    pub atol: f64,
    pub rtol: f64,
    pub initial_inputs: Vec<(String, f64)>,
    pub execution_policy: rumoca_sim::SimExecutionPolicy,
}

impl InteractiveOptions {
    pub(super) fn parse(json: &str) -> Result<Self, WasmError> {
        if !json.trim_start().starts_with('{') {
            return Err(WasmError::new("Interactive options must be a JSON object"));
        }
        let options: Self = serde_json::from_str(json)
            .map_err(|error| WasmError::new(format!("Invalid interactive options: {error}")))?;
        for (name, value) in [
            ("dt", options.dt),
            ("atol", options.atol),
            ("rtol", options.rtol),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(WasmError::new(format!(
                    "Interactive {name} must be finite and nonnegative"
                )));
            }
        }
        if options
            .initial_inputs
            .iter()
            .any(|(_, value)| !value.is_finite())
        {
            return Err(WasmError::new("Initial input values must be finite"));
        }
        Ok(options)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interactive_options_preserve_auto_default_and_typed_interpreter_policy() {
        let default = InteractiveOptions::parse("{}").unwrap();
        assert_eq!(
            default.execution_policy,
            rumoca_sim::SimExecutionPolicy::Auto
        );
        let reference = InteractiveOptions::parse(
            r#"{"execution_policy":"interpreter","initial_inputs":[["u",-0.0]]}"#,
        )
        .unwrap();
        assert_eq!(
            reference.execution_policy,
            rumoca_sim::SimExecutionPolicy::Interpreter
        );
        assert_eq!(
            reference.initial_inputs[0].1.to_bits(),
            (-0.0_f64).to_bits()
        );
    }

    #[test]
    fn interactive_options_reject_unknown_policy_fields_and_malformed_values() {
        for json in [
            r#"{"execution_policy":"native_required"}"#,
            r#"{"extra":true}"#,
            r#"{"dt":-1}"#,
            r#"{"rtol":-1}"#,
            r#"{"atol":-1}"#,
            r#"{"dt":null}"#,
            r#"{"initial_inputs":[["u",null]]}"#,
            "[]",
        ] {
            assert!(InteractiveOptions::parse(json).is_err(), "{json}");
        }
    }
}
