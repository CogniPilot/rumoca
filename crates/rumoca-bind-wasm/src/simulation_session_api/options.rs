use crate::WasmError;

#[derive(Default, serde::Deserialize, serde::Serialize)]
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
    /// Explicit configuration of the `fixed-step` solver. Absent, the solver
    /// named by `solver` runs at its defaults, and the serialized options (so
    /// every options-derived key) are unchanged.
    #[serde(
        skip_serializing_if = "Option::is_none",
        deserialize_with = "deserialize_fixed_step"
    )]
    pub fixed_step: Option<FixedStepOptions>,
}

/// The `fixed-step` solver's options as plain data. `step` and `order` are
/// required once the object is given; the backend that owns them validates
/// the values before any model work.
#[derive(Clone, Copy, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FixedStepOptions {
    /// The fixed internal step in seconds.
    pub step: f64,
    /// 1 (explicit Euler), 2 (Heun) or 4 (classical Runge-Kutta).
    pub order: u32,
    /// `"explicit"` (absent) or `"semi-implicit"`, the partitioned Euler
    /// step over the compiler-issued kinematic states, which the backend
    /// admits with order 1 only. Absent, the serialized options are unchanged.
    #[serde(default, skip_serializing_if = "FixedStepScheme::is_explicit")]
    pub scheme: FixedStepScheme,
}

/// The `fixed-step` scheme by name; any other name is a parse error.
#[derive(Clone, Copy, Default, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub(super) enum FixedStepScheme {
    #[default]
    Explicit,
    SemiImplicit,
}

impl FixedStepScheme {
    fn is_explicit(&self) -> bool {
        *self == Self::Explicit
    }
}

/// Only an object names its members: a positional array would bind `step`
/// and `order` by order alone.
fn deserialize_fixed_step<'de, D>(deserializer: D) -> Result<Option<FixedStepOptions>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let value = <serde_json::Value as serde::Deserialize>::deserialize(deserializer)?;
    match value {
        serde_json::Value::Null => Ok(None),
        serde_json::Value::Object(_) => serde_json::from_value(value)
            .map(Some)
            .map_err(serde::de::Error::custom),
        _ => Err(serde::de::Error::custom(
            "fixed_step must be an object with `step` and `order`",
        )),
    }
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
            r#"{"fixed_step":{"step":0.005}}"#,
            r#"{"fixed_step":{"order":1}}"#,
            r#"{"fixed_step":{"step":0.005,"order":1,"extra":true}}"#,
            r#"{"fixed_step":{"step":"0.005","order":1}}"#,
            r#"{"fixed_step":{"step":0.005,"order":1.5}}"#,
            r#"{"fixed_step":{"step":0.005,"order":-1}}"#,
            r#"{"fixed_step":{"step":1e400,"order":1}}"#,
            r#"{"fixed_step":[0.005,1]}"#,
            r#"{"fixedStep":{"step":0.005,"order":1}}"#,
            r#"{"fixed_step":{"step":0.005,"order":1,"scheme":"implicit"}}"#,
            r#"{"fixed_step":{"step":0.005,"order":1,"scheme":1}}"#,
            "[]",
        ] {
            assert!(InteractiveOptions::parse(json).is_err(), "{json}");
        }
    }

    #[test]
    fn fixed_step_options_are_typed_and_leave_other_requests_serialized_unchanged() {
        let json = r#"{"solver":"fixed-step","fixed_step":{"step":0.005,"order":1}}"#;
        let parsed = InteractiveOptions::parse(json).unwrap();
        let fixed_step = parsed.fixed_step.unwrap();
        assert_eq!(fixed_step.step.to_bits(), 0.005_f64.to_bits());
        assert_eq!(fixed_step.order, 1);
        assert!(
            serde_json::to_string(&parsed)
                .unwrap()
                .contains(r#""fixed_step":{"step":0.005,"order":1}"#)
        );
        for json in ["{}", r#"{"solver":"fixed-step"}"#, r#"{"fixed_step":null}"#] {
            let absent = InteractiveOptions::parse(json).unwrap();
            assert!(absent.fixed_step.is_none());
            assert!(
                !serde_json::to_string(&absent)
                    .unwrap()
                    .contains("fixed_step")
            );
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

    #[test]
    fn the_fixed_step_scheme_is_named_and_absent_when_explicit() {
        let json = r#"{"fixed_step":{"step":0.005,"order":1,"scheme":"semi-implicit"}}"#;
        let parsed = InteractiveOptions::parse(json).unwrap();
        assert!(parsed.fixed_step.unwrap().scheme == FixedStepScheme::SemiImplicit);
        assert!(
            serde_json::to_string(&parsed)
                .unwrap()
                .contains(r#""scheme":"semi-implicit""#)
        );
        let json = r#"{"fixed_step":{"step":0.005,"order":1,"scheme":"explicit"}}"#;
        let parsed = InteractiveOptions::parse(json).unwrap();
        assert!(parsed.fixed_step.unwrap().scheme == FixedStepScheme::Explicit);
        assert!(
            serde_json::to_string(&parsed)
                .unwrap()
                .contains(r#""fixed_step":{"step":0.005,"order":1}"#)
        );
    }
}
