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
    #[serde(rename = "parameters", deserialize_with = "deserialize_parameters")]
    pub param_overrides: Vec<(String, f64)>,
}

fn deserialize_parameters<'de, D>(deserializer: D) -> Result<Vec<(String, f64)>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let value = <serde_json::Value as serde::Deserialize>::deserialize(deserializer)?;
    crate::simulation_api::parameter_overrides_from_value(&value).map_err(serde::de::Error::custom)
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
    fn interactive_options_require_compilation_by_default_and_preserve_explicit_policies() {
        let default = InteractiveOptions::parse("{}").unwrap();
        assert_eq!(
            default.execution_policy,
            rumoca_sim::SimExecutionPolicy::NativeRequired
        );
        for policy in [
            rumoca_sim::SimExecutionPolicy::NativeRequired,
            rumoca_sim::SimExecutionPolicy::Auto,
            rumoca_sim::SimExecutionPolicy::Interpreter,
        ] {
            let options = InteractiveOptions::parse(&format!(
                r#"{{"execution_policy":"{}"}}"#,
                policy.label()
            ))
            .unwrap();
            assert_eq!(options.execution_policy, policy);
        }
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
            r#"{"execution_policy":"unknown"}"#,
            r#"{"execution_policy":true}"#,
            r#"{"execution_policy":null}"#,
            r#"{"extra":true}"#,
            r#"{"dt":-1}"#,
            r#"{"rtol":-1}"#,
            r#"{"atol":-1}"#,
            r#"{"dt":null}"#,
            r#"{"initial_inputs":[["u",null]]}"#,
            r#"{"parameters":{"k":true}}"#,
            r#"{"parameters":{"k":null}}"#,
            r#"{"parameters":{"k":"3"}}"#,
            r#"{"parameters":{"k":1e400}}"#,
            r#"{"parameters":[["k",3]]}"#,
            "[]",
        ] {
            assert!(InteractiveOptions::parse(json).is_err(), "{json}");
        }
    }

    #[test]
    fn interactive_options_reuse_the_numeric_parameter_override_reader() {
        for json in ["{}", r#"{"parameters":{}}"#, r#"{"parameters":null}"#] {
            assert!(
                InteractiveOptions::parse(json)
                    .unwrap()
                    .param_overrides
                    .is_empty()
            );
        }
        let parsed = InteractiveOptions::parse(r#"{"parameters":{"k":3,"zero":-0.0}}"#).unwrap();
        assert_eq!(parsed.param_overrides[0], ("k".to_owned(), 3.0));
        assert_eq!(parsed.param_overrides[1].1.to_bits(), (-0.0_f64).to_bits());
    }
}
