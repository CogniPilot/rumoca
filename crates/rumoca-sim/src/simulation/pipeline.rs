use rumoca_core::{maybe_elapsed_seconds, maybe_start_timer_if};
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
    let t0 = maybe_start_timer_if(trace);
    let result = step();
    if trace {
        eprintln!(
            "[sim-trace] prepare phase done: {name} elapsed={:.3}s",
            maybe_elapsed_seconds(t0)
        );
    }
    result
}
