use std::time::Instant;

use rumoca_ir_dae as dae;

pub type MassMatrix = Vec<Vec<f64>>;

pub struct PreparedSimulation {
    pub dae: dae::Dae,
    pub has_dummy_state: bool,
    pub elimination: rumoca_phase_solve::eliminate::EliminationResult,
    pub ic_blocks: Vec<rumoca_phase_solve::IcBlock>,
    pub mass_matrix: MassMatrix,
}

pub fn run_logged_phase<E, F>(trace: bool, name: &str, mut step: F) -> Result<(), E>
where
    F: FnMut() -> Result<(), E>,
{
    if trace {
        eprintln!("[sim-trace] prepare phase start: {name}");
    }
    let t0 = trace_timer_start_if(trace);
    let result = step();
    if trace {
        eprintln!(
            "[sim-trace] prepare phase done: {name} elapsed={:.3}s",
            trace_timer_elapsed_seconds(t0)
        );
    }
    result
}

#[inline]
fn trace_timer_start_if(trace: bool) -> Option<Instant> {
    if !trace {
        return None;
    }
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
fn trace_timer_elapsed_seconds(start: Option<Instant>) -> f64 {
    start.map_or(0.0, |t0| t0.elapsed().as_secs_f64())
}
