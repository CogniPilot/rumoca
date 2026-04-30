use serde_json::{Value, json};

use crate::{SimResult, SimVariableMeta};

#[derive(Debug, Clone)]
pub struct SimulationRequestSummary {
    pub solver: String,
    pub t_start: f64,
    pub t_end: f64,
    pub dt: Option<f64>,
    pub rtol: f64,
    pub atol: f64,
}

#[derive(Debug, Clone, Default)]
pub struct SimulationRunMetrics {
    pub compile_seconds: Option<f64>,
    pub simulate_seconds: Option<f64>,
    pub prepare_context_seconds: Option<f64>,
    pub build_snapshot_seconds: Option<f64>,
    pub strict_compile_seconds: Option<f64>,
    pub strict_resolve_seconds: Option<f64>,
    pub instantiate_seconds: Option<f64>,
    pub typecheck_seconds: Option<f64>,
    pub flatten_seconds: Option<f64>,
    pub todae_seconds: Option<f64>,
}

pub fn build_simulation_metrics_value(sim: &SimResult, metrics: &SimulationRunMetrics) -> Value {
    json!({
        "compileSeconds": metrics.compile_seconds,
        "simulateSeconds": metrics.simulate_seconds,
        "points": sim.times.len(),
        "variables": sim.names.len(),
        "compilePhaseSeconds": {
            "prepareContext": metrics.prepare_context_seconds,
            "buildSnapshot": metrics.build_snapshot_seconds,
            "strictCompile": metrics.strict_compile_seconds,
            "strictResolve": metrics.strict_resolve_seconds,
            "instantiate": metrics.instantiate_seconds,
            "typecheck": metrics.typecheck_seconds,
            "flatten": metrics.flatten_seconds,
            "todae": metrics.todae_seconds,
        },
    })
}

pub fn build_simulation_payload(
    sim: &SimResult,
    request: &SimulationRequestSummary,
    metrics: &SimulationRunMetrics,
) -> Value {
    let t_start_actual = sim.times.first().copied().unwrap_or(request.t_start);
    let t_end_actual = sim.times.last().copied().unwrap_or(request.t_start);
    let mut all_data = Vec::with_capacity(1 + sim.data.len());
    all_data.push(sim.times.clone());
    all_data.extend(sim.data.clone());

    json!({
        "version": 1,
        "names": sim.names,
        "allData": all_data,
        "nStates": sim.n_states,
        "variableMeta": sim.variable_meta.iter().map(build_variable_meta_value).collect::<Vec<_>>(),
        "simDetails": {
            "actual": {
                "t_start": t_start_actual,
                "t_end": t_end_actual,
                "points": sim.times.len(),
                "variables": sim.names.len(),
            },
            "requested": {
                "solver": request.solver,
                "t_start": request.t_start,
                "t_end": request.t_end,
                "dt": request.dt,
                "rtol": request.rtol,
                "atol": request.atol,
            },
            "timing": {
                "compile_seconds": metrics.compile_seconds,
                "simulate_seconds": metrics.simulate_seconds,
                "compile_phase_seconds": {
                    "prepare_context": metrics.prepare_context_seconds,
                    "build_snapshot": metrics.build_snapshot_seconds,
                    "strict_compile": metrics.strict_compile_seconds,
                    "strict_resolve": metrics.strict_resolve_seconds,
                    "instantiate": metrics.instantiate_seconds,
                    "typecheck": metrics.typecheck_seconds,
                    "flatten": metrics.flatten_seconds,
                    "todae": metrics.todae_seconds,
                },
            },
        },
    })
}

fn build_variable_meta_value(meta: &SimVariableMeta) -> Value {
    json!({
        "name": meta.name,
        "role": meta.role,
        "is_state": meta.is_state,
        "value_type": meta.value_type,
        "variability": meta.variability,
        "time_domain": meta.time_domain,
        "unit": meta.unit,
        "start": meta.start,
        "min": meta.min,
        "max": meta.max,
        "nominal": meta.nominal,
        "fixed": meta.fixed,
        "description": meta.description,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_result() -> SimResult {
        SimResult {
            times: vec![0.0, 1.0],
            names: vec!["x".to_string(), "y".to_string()],
            data: vec![vec![1.0, 2.0], vec![3.0, 4.0]],
            n_states: 1,
            variable_meta: vec![SimVariableMeta {
                name: "x".to_string(),
                role: "state".to_string(),
                is_state: true,
                value_type: None,
                variability: None,
                time_domain: None,
                unit: None,
                start: None,
                min: None,
                max: None,
                nominal: None,
                fixed: None,
                description: None,
            }],
        }
    }

    #[test]
    fn builds_canonical_simulation_payload() {
        let payload = build_simulation_payload(
            &sample_result(),
            &SimulationRequestSummary {
                solver: "auto".to_string(),
                t_start: 0.0,
                t_end: 1.0,
                dt: Some(0.1),
                rtol: 1e-6,
                atol: 1e-6,
            },
            &SimulationRunMetrics {
                simulate_seconds: Some(0.25),
                ..SimulationRunMetrics::default()
            },
        );

        assert_eq!(payload["nStates"], 1);
        assert_eq!(payload["names"][0], "x");
        assert_eq!(payload["allData"][0][1], 1.0);
        assert_eq!(payload["simDetails"]["timing"]["simulate_seconds"], 0.25);
    }

    #[test]
    fn builds_metrics_with_extended_compile_timings() {
        let metrics = build_simulation_metrics_value(
            &sample_result(),
            &SimulationRunMetrics {
                compile_seconds: Some(1.0),
                prepare_context_seconds: Some(0.1),
                build_snapshot_seconds: Some(0.2),
                strict_compile_seconds: Some(0.3),
                strict_resolve_seconds: Some(0.4),
                instantiate_seconds: Some(0.5),
                typecheck_seconds: Some(0.6),
                flatten_seconds: Some(0.7),
                todae_seconds: Some(0.8),
                ..SimulationRunMetrics::default()
            },
        );

        assert_eq!(metrics["compileSeconds"], 1.0);
        assert_eq!(metrics["compilePhaseSeconds"]["prepareContext"], 0.1);
        assert_eq!(metrics["compilePhaseSeconds"]["strictResolve"], 0.4);
        assert_eq!(metrics["compilePhaseSeconds"]["todae"], 0.8);
    }
}
