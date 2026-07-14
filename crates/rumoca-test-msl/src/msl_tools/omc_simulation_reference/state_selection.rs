use super::{MslPaths, TraceQuantification};
use anyhow::{Result, bail};
use rumoca_sim::sim_trace_compare::SimTrace;
use serde::Serialize;
use std::collections::BTreeSet;

#[derive(Debug, Clone, Serialize)]
pub(super) struct StateSelectionMetric {
    pub rumoca_state_count: usize,
    pub omc_state_count: usize,
    pub matching_state_count: usize,
    pub rumoca_only_state_count: usize,
    pub omc_only_state_count: usize,
    pub state_count_match: bool,
    pub exact_state_set_match: bool,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub rumoca_only_states: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub omc_only_states: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub(super) struct StateSelectionSummary {
    pub models_compared: usize,
    pub exact_state_set_match_models: usize,
    pub state_count_match_models: usize,
    pub exact_state_set_match_percent: f64,
    pub state_count_match_percent: f64,
    pub total_rumoca_states: usize,
    pub total_omc_states: usize,
    pub total_matching_states: usize,
    pub total_rumoca_only_states: usize,
    pub total_omc_only_states: usize,
    pub max_model_state_set_difference: usize,
}

pub(super) fn compare_model_state_selection(
    paths: &MslPaths,
    model_name: &str,
    rumoca_trace: &SimTrace,
) -> Result<Option<StateSelectionMetric>> {
    let rumoca_states = rumoca_state_names(rumoca_trace)?;
    let Some(omc_states) = load_omc_state_names(paths, model_name) else {
        return Ok(None);
    };
    Ok(Some(compare_state_sets(&rumoca_states, &omc_states)))
}

pub(super) fn validate_rumoca_state_metadata(trace: &SimTrace) -> Result<()> {
    rumoca_state_names(trace).map(|_| ())
}

pub(super) fn state_selection_summary(report: &TraceQuantification) -> StateSelectionSummary {
    let mut summary = StateSelectionSummary::default();
    for metric in report
        .models
        .values()
        .filter_map(|item| item.state_selection.as_ref())
    {
        summary.models_compared += 1;
        summary.total_rumoca_states += metric.rumoca_state_count;
        summary.total_omc_states += metric.omc_state_count;
        summary.total_matching_states += metric.matching_state_count;
        summary.total_rumoca_only_states += metric.rumoca_only_state_count;
        summary.total_omc_only_states += metric.omc_only_state_count;
        summary.max_model_state_set_difference = summary
            .max_model_state_set_difference
            .max(metric.rumoca_only_state_count + metric.omc_only_state_count);
        if metric.state_count_match {
            summary.state_count_match_models += 1;
        }
        if metric.exact_state_set_match {
            summary.exact_state_set_match_models += 1;
        }
    }
    let model_count = summary.models_compared.max(1) as f64;
    summary.exact_state_set_match_percent =
        summary.exact_state_set_match_models as f64 * 100.0 / model_count;
    summary.state_count_match_percent =
        summary.state_count_match_models as f64 * 100.0 / model_count;
    summary
}

fn compare_state_sets(
    rumoca_states: &BTreeSet<String>,
    omc_states: &BTreeSet<String>,
) -> StateSelectionMetric {
    let rumoca_only_states = rumoca_states
        .difference(omc_states)
        .cloned()
        .collect::<Vec<_>>();
    let omc_only_states = omc_states
        .difference(rumoca_states)
        .cloned()
        .collect::<Vec<_>>();
    let matching_state_count = rumoca_states.intersection(omc_states).count();
    StateSelectionMetric {
        rumoca_state_count: rumoca_states.len(),
        omc_state_count: omc_states.len(),
        matching_state_count,
        rumoca_only_state_count: rumoca_only_states.len(),
        omc_only_state_count: omc_only_states.len(),
        state_count_match: rumoca_states.len() == omc_states.len(),
        exact_state_set_match: rumoca_only_states.is_empty() && omc_only_states.is_empty(),
        rumoca_only_states,
        omc_only_states,
    }
}

fn rumoca_state_names(trace: &SimTrace) -> Result<BTreeSet<String>> {
    let Some(expected_state_count) = trace.n_states else {
        bail!("rumoca trace is missing the n_states metadata contract");
    };
    let Some(variable_meta) = trace.variable_meta.as_ref() else {
        bail!("rumoca trace is missing variable_meta state metadata");
    };
    let states = variable_meta
        .iter()
        .filter(|meta| meta.role.as_deref() == Some("state"))
        .map(|meta| meta.name.clone())
        .collect::<BTreeSet<_>>();
    if expected_state_count != states.len() {
        bail!(
            "rumoca trace declares {expected_state_count} states but metadata reports {}",
            states.len()
        );
    }
    Ok(states)
}

fn load_omc_state_names(paths: &MslPaths, model_name: &str) -> Option<BTreeSet<String>> {
    let init_xml = paths.sim_work_dir.join(format!("{model_name}_init.xml"));
    let xml = std::fs::read_to_string(init_xml).ok()?;
    Some(extract_omc_state_names_from_init_xml(&xml))
}

pub(super) fn extract_omc_state_names_from_init_xml(xml: &str) -> BTreeSet<String> {
    scalar_variable_tags(xml)
        .filter(|tag| xml_attr(tag, "classType").as_deref() == Some("rSta"))
        .filter_map(|tag| xml_attr(tag, "name"))
        .filter(|name| !name.starts_with("der("))
        .map(|name| xml_unescape(&name))
        .collect()
}

fn scalar_variable_tags(xml: &str) -> impl Iterator<Item = &str> {
    xml.match_indices("<ScalarVariable")
        .filter_map(|(start, _)| {
            let tail = &xml[start..];
            let end = tail.find('>')?;
            Some(&tail[..=end])
        })
}

fn xml_attr(tag: &str, attr: &str) -> Option<String> {
    for (idx, _) in tag.match_indices(attr) {
        if !attr_name_boundary(tag, idx, attr.len()) {
            continue;
        }
        let value_start = tag[idx + attr.len()..].trim_start();
        let value_start = value_start.strip_prefix('=')?.trim_start();
        let value_start = value_start.strip_prefix('"')?;
        let value_end = value_start.find('"')?;
        return Some(value_start[..value_end].to_string());
    }
    None
}

fn attr_name_boundary(tag: &str, idx: usize, attr_len: usize) -> bool {
    let before_ok = tag[..idx]
        .chars()
        .next_back()
        .is_none_or(|ch| ch.is_whitespace() || ch == '<');
    let after_ok = tag[idx + attr_len..]
        .chars()
        .next()
        .is_some_and(|ch| ch.is_whitespace() || ch == '=');
    before_ok && after_ok
}

fn xml_unescape(value: &str) -> String {
    value
        .replace("&quot;", "\"")
        .replace("&apos;", "'")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&amp;", "&")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_omc_selected_states_from_init_xml() {
        let xml = r#"
        <ModelVariables>
          <ScalarVariable
            name = "x"
            classType = "rSta">
            <Real />
          </ScalarVariable>
          <ScalarVariable name = "der(x)" classType = "rDer" />
          <ScalarVariable name = "y" classType = "rAlg" />
          <ScalarVariable name = "a&amp;b" classType = "rSta" />
        </ModelVariables>
        "#;

        let states = extract_omc_state_names_from_init_xml(xml);

        assert_eq!(states, BTreeSet::from(["a&b".to_string(), "x".to_string()]));
    }

    #[test]
    fn rejects_state_metadata_count_that_disagrees_with_trace_contract() {
        let trace = serde_json::from_value::<SimTrace>(serde_json::json!({
            "model_name": "BrokenMetadata",
            "n_states": 2,
            "times": [0.0],
            "names": ["x"],
            "data": [[0.0]],
            "variable_meta": [{"name": "x", "role": "state"}]
        }))
        .expect("trace fixture should deserialize");

        assert!(rumoca_state_names(&trace).is_err());
    }

    #[test]
    fn rejects_state_metadata_without_trace_state_count_contract() {
        let trace = serde_json::from_value::<SimTrace>(serde_json::json!({
            "model_name": "MissingContract",
            "times": [0.0],
            "names": ["x"],
            "data": [[0.0]],
            "variable_meta": [{"name": "x", "role": "state"}]
        }))
        .expect("trace fixture should deserialize");

        assert!(rumoca_state_names(&trace).is_err());
    }

    #[test]
    fn invalid_state_metadata_contract_is_not_dropped_as_missing_comparison() {
        let trace = serde_json::from_value::<SimTrace>(serde_json::json!({
            "model_name": "BrokenMetadata",
            "n_states": 2,
            "times": [0.0],
            "names": ["x"],
            "data": [[0.0]],
            "variable_meta": [{"name": "x", "role": "state"}]
        }))
        .expect("trace fixture should deserialize");
        let root = std::path::PathBuf::from("/nonexistent-state-contract-test");
        let paths = MslPaths {
            repo_root: root.clone(),
            msl_dir: root.clone(),
            results_dir: root.clone(),
            flat_dir: root.clone(),
            work_dir: root.clone(),
            sim_work_dir: root.clone(),
            omc_trace_dir: root.clone(),
            rumoca_trace_dir: root,
        };

        let error = compare_model_state_selection(&paths, "BrokenMetadata", &trace)
            .expect_err("invalid producer metadata must fail the parity comparison");

        assert!(error.to_string().contains("declares 2 states"));
    }

    #[test]
    fn summarizes_state_selection_agreement() {
        let exact = StateSelectionMetric {
            rumoca_state_count: 1,
            omc_state_count: 1,
            matching_state_count: 1,
            rumoca_only_state_count: 0,
            omc_only_state_count: 0,
            state_count_match: true,
            exact_state_set_match: true,
            rumoca_only_states: Vec::new(),
            omc_only_states: Vec::new(),
        };
        let mismatch = StateSelectionMetric {
            rumoca_state_count: 2,
            omc_state_count: 1,
            matching_state_count: 1,
            rumoca_only_state_count: 1,
            omc_only_state_count: 0,
            state_count_match: false,
            exact_state_set_match: false,
            rumoca_only_states: vec!["z".to_string()],
            omc_only_states: Vec::new(),
        };
        let mut report = TraceQuantification::default();
        report.models.insert(
            "A".to_string(),
            super::super::TraceModelMetric {
                metric: minimal_metric("A"),
                state_selection: Some(exact),
                rumoca_sim_wall_seconds: None,
                rumoca_sim_seconds: None,
                rumoca_sim_build_seconds: None,
                rumoca_sim_run_seconds: None,
                omc_sim_system_seconds: None,
                omc_total_system_seconds: None,
                omc_wall_seconds: None,
            },
        );
        report.models.insert(
            "B".to_string(),
            super::super::TraceModelMetric {
                metric: minimal_metric("B"),
                state_selection: Some(mismatch),
                rumoca_sim_wall_seconds: None,
                rumoca_sim_seconds: None,
                rumoca_sim_build_seconds: None,
                rumoca_sim_run_seconds: None,
                omc_sim_system_seconds: None,
                omc_total_system_seconds: None,
                omc_wall_seconds: None,
            },
        );

        let summary = state_selection_summary(&report);

        assert_eq!(summary.models_compared, 2);
        assert_eq!(summary.exact_state_set_match_models, 1);
        assert_eq!(summary.state_count_match_models, 1);
        assert_eq!(summary.total_rumoca_only_states, 1);
        assert_eq!(summary.max_model_state_set_difference, 1);
    }

    fn minimal_metric(model_name: &str) -> rumoca_sim::sim_trace_compare::ModelDeviationMetric {
        rumoca_sim::sim_trace_compare::ModelDeviationMetric {
            model_name: model_name.to_string(),
            compared_variables: 0,
            samples_compared: 0,
            bounded_normalized_l1_score: 0.0,
            mean_channel_bounded_normalized_l1: 0.0,
            max_channel_bounded_normalized_l1: 0.0,
            channel_high_count: 0,
            channel_minor_count: 0,
            channel_deviation_count: 0,
            channel_severe_count: 0,
            channel_high_percent: 0.0,
            channel_minor_percent: 0.0,
            channel_deviation_percent: 0.0,
            channel_severe_percent: 0.0,
            channel_violation_mass: 0.0,
            initial_condition: Default::default(),
            worst_variables: Vec::new(),
        }
    }
}
