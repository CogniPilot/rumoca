use super::*;
use std::collections::HashSet;

pub(super) fn simulation_solver_override() -> Option<String> {
    // No solver override; use the model's experiment annotation (else auto).
    None
}

pub(super) fn simulation_stop_time_override() -> Option<f64> {
    // No stop-time override; use the model's experiment annotation.
    None
}

pub(super) struct RenderSimSetup {
    sim_attempted: AtomicUsize,
    sim_completed: AtomicUsize,
    sim_ok_live: AtomicUsize,
    sim_nan_live: AtomicUsize,
    sim_timeout_live: AtomicUsize,
    sim_solver_fail_live: AtomicUsize,
    sim_balance_fail_live: AtomicUsize,
    sim_target_names: Option<HashSet<String>>,
    sim_target_models: Vec<String>,
    total_sim_targets: usize,
}

impl RenderSimSetup {
    fn new_from_compile_scope(compile_scope_names: &[String], run_simulation: bool) -> Self {
        let sim_target_names =
            select_sim_target_names_from_compile_scope(compile_scope_names, run_simulation);
        let sim_target_models = match sim_target_names.as_ref() {
            Some(names) => names.clone(),
            None => Vec::new(),
        };
        let sim_target_name_set = sim_target_models.iter().cloned().collect();
        let total_sim_targets = sim_target_models.len();
        if run_simulation {
            println!(
                "Target standalone models for simulation: {}",
                total_sim_targets
            );
        }

        Self {
            sim_attempted: AtomicUsize::new(0),
            sim_completed: AtomicUsize::new(0),
            sim_ok_live: AtomicUsize::new(0),
            sim_nan_live: AtomicUsize::new(0),
            sim_timeout_live: AtomicUsize::new(0),
            sim_solver_fail_live: AtomicUsize::new(0),
            sim_balance_fail_live: AtomicUsize::new(0),
            sim_target_names: if run_simulation {
                Some(sim_target_name_set)
            } else {
                None
            },
            sim_target_models,
            total_sim_targets,
        }
    }

    pub(super) fn context(&self, run_simulation: bool) -> RenderSimContext<'_> {
        RenderSimContext {
            run_simulation,
            sim_target_names: self.sim_target_names.as_ref(),
            total_sim_targets: self.total_sim_targets,
            sim_attempted: &self.sim_attempted,
            sim_completed: &self.sim_completed,
            sim_ok_live: &self.sim_ok_live,
            sim_nan_live: &self.sim_nan_live,
            sim_timeout_live: &self.sim_timeout_live,
            sim_solver_fail_live: &self.sim_solver_fail_live,
            sim_balance_fail_live: &self.sim_balance_fail_live,
        }
    }

    pub(super) fn print_summary(&self, run_simulation: bool) {
        if run_simulation {
            println!(
                "Simulated {} standalone selected models (target={})",
                self.sim_attempted.load(Ordering::Relaxed),
                self.total_sim_targets,
            );
        } else {
            println!("Simulation skipped (compile+balance mode).");
        }
    }

    pub(super) fn sim_target_models(&self) -> Vec<String> {
        self.sim_target_models.clone()
    }
}

fn select_sim_target_names_from_compile_scope(
    compile_scope_names: &[String],
    run_simulation: bool,
) -> Option<Vec<String>> {
    if !run_simulation {
        return None;
    }

    let mut names: Vec<String> = compile_scope_names.to_vec();
    let subset_requested = apply_sim_subset_filters(&mut names, "Simulation");
    if !subset_requested {
        apply_default_sim_set_mode_selection(&mut names);
    }

    Some(names)
}

fn apply_default_sim_set_mode_selection(names: &mut Vec<String>) {
    let mode = sim_set_mode();
    if mode == SimSetMode::Full {
        println!(
            "Simulation set mode ({mode}): keeping all {} compile-scope models",
            names.len()
        );
        return;
    }

    let limit = sim_set_limit();
    if limit >= names.len() {
        println!(
            "Simulation set mode ({mode}) selected {}/{} compile-scope models (sim_set_limit={})",
            names.len(),
            names.len(),
            limit
        );
        return;
    }

    eprintln!(
        "WARNING: simulation set mode ({mode}) is using compile-scope order fallback for streaming compile (full live state-count ranking still requires retaining all compile results)"
    );
    apply_lexical_mode_limit(names, mode, limit);
}

fn apply_lexical_mode_limit(names: &mut Vec<String>, mode: SimSetMode, limit: usize) {
    match mode {
        SimSetMode::Short => names.truncate(limit),
        SimSetMode::Long => {
            let keep_from = names.len().saturating_sub(limit);
            *names = names.split_off(keep_from);
        }
        SimSetMode::Full => {}
    }
}

pub(super) fn begin_chunked_render_sim_setup(
    compile_scope_names: &[String],
    run_simulation: bool,
) -> RenderSimSetup {
    RenderSimSetup::new_from_compile_scope(compile_scope_names, run_simulation)
}
