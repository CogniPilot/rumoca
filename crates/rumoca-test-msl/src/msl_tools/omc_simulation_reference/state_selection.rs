use super::{MslPaths, TraceQuantification};
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
) -> Option<StateSelectionMetric> {
    let rumoca_states = rumoca_state_names(rumoca_trace)?;
    let omc_states = load_omc_state_names(paths, model_name)?;
    Some(compare_state_sets(&rumoca_states, &omc_states))
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

/// The states a Rumoca trace integrates, each named as the source scalar it
/// is: a generated reduced-selection state scalar by the source scalar its
/// Solve IR state coordinate map equates it to (a formal derivative order `k`
/// wraps that name in `k` `der`s, which no OMC state name matches), every other
/// state by its own name.
fn rumoca_state_names(trace: &SimTrace) -> Option<BTreeSet<String>> {
    let states = trace
        .variable_meta
        .as_ref()?
        .iter()
        .filter(|meta| meta.role.as_deref() == Some("state"))
        .map(|meta| {
            meta.state_coordinate
                .as_ref()
                .map_or_else(|| meta.name.clone(), |source| source.source_name())
        })
        .collect::<BTreeSet<_>>();
    Some(states)
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

    /// A worker trace whose `variable_meta` lists `states`, each with the
    /// optional source scalar and order of a generated state coordinate.
    fn trace(states: &[(&str, Option<(&str, u32)>)]) -> SimTrace {
        let meta = states
            .iter()
            .map(|(name, source)| {
                let mut meta = serde_json::json!({ "name": name, "role": "state" });
                if let Some((variable, order)) = source {
                    meta["state_coordinate"] =
                        serde_json::json!({ "variable": variable, "derivative_order": order });
                }
                meta
            })
            .chain([serde_json::json!({ "name": "y", "role": "algebraic" })])
            .collect::<Vec<_>>();
        serde_json::from_value(serde_json::json!({
            "times": [0.0],
            "names": [],
            "data": [],
            "variable_meta": meta,
        }))
        .expect("worker trace decodes")
    }

    fn omc(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|name| (*name).to_string()).collect()
    }

    fn compare(trace: &SimTrace, omc_states: &[&str]) -> StateSelectionMetric {
        compare_state_sets(&rumoca_state_names(trace).unwrap(), &omc(omc_states))
    }

    #[test]
    fn generated_state_coordinates_compare_by_the_source_scalars_they_equal() {
        let charted = trace(&[
            ("$state_coordinates[1]", Some(("mass.s", 0))),
            ("$state_coordinates[2]", Some(("mass.v", 0))),
            ("spring.phi_rel", None),
        ]);
        let metric = compare(&charted, &["mass.s", "mass.v", "spring.phi_rel"]);
        assert!(metric.exact_state_set_match, "{metric:?}");
        assert_eq!(metric.matching_state_count, 3);
    }

    #[test]
    fn a_charted_selection_of_other_variables_is_still_a_mismatch() {
        // The same chart integrating the damper instead of the mass: a real
        // state-selection difference stays visible on both sides.
        let charted = trace(&[
            ("$state_coordinates[1]", Some(("damper.s_rel", 0))),
            ("$state_coordinates[2]", Some(("mass.v", 0))),
        ]);
        let metric = compare(&charted, &["mass.s", "mass.v"]);
        assert!(!metric.exact_state_set_match);
        assert!(metric.state_count_match);
        assert_eq!(metric.rumoca_only_states, ["damper.s_rel"]);
        assert_eq!(metric.omc_only_states, ["mass.s"]);
    }

    #[test]
    fn a_formal_derivative_coordinate_matches_no_omc_state_name() {
        // `der(s_rel)` is integrated in place of the declared `v_rel`; OMC
        // names no state `der(...)`, so the substitution is reported.
        let charted = trace(&[
            ("$state_coordinates[1]", Some(("s_rel", 0))),
            ("$state_coordinates[2]", Some(("s_rel", 1))),
        ]);
        let metric = compare(&charted, &["s_rel", "v_rel"]);
        assert_eq!(metric.matching_state_count, 1);
        assert_eq!(metric.rumoca_only_states, ["der(s_rel)"]);
        assert_eq!(metric.omc_only_states, ["v_rel"]);
    }

    #[test]
    fn a_generated_state_without_a_source_keeps_its_own_name() {
        let unmapped = trace(&[("$state_coordinates[1]", None)]);
        let metric = compare(&unmapped, &["x"]);
        assert_eq!(metric.rumoca_only_states, ["$state_coordinates[1]"]);
        assert_eq!(metric.omc_only_states, ["x"]);
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
            undefined_phasor_channels: Vec::new(),
        }
    }
}
