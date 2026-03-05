mod integration;

use rumoca_eval_flat::eval;
use rumoca_ir_dae as dae;

pub use rumoca_sim_core::{
    OutputBuffers, SimError, SimOptions, SimResult, SimSolverMode, SimVariableMeta,
    SolverStartupProfile,
};

type Dae = dae::Dae;

const DUMMY_STATE_NAME: &str = "_rumoca_dummy_state";

fn validate_simulation_function_support(dae_model: &Dae) -> Result<(), SimError> {
    rumoca_sim_core::function_validation::validate_simulation_function_support(dae_model).map_err(
        |err| SimError::UnsupportedFunction {
            name: err.name,
            reason: err.reason,
        },
    )
}

fn causal_form_error(err: rumoca_sim_core::CausalOdeFormError) -> SimError {
    SimError::SolverError(format!("causal RK45 form check failed: {err}"))
}

pub fn simulate(dae_model: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    eval::clear_pre_values();
    let budget = rumoca_sim_core::TimeoutBudget::new(opts.max_wall_seconds);

    if opts.t_end < opts.t_start {
        return Err(SimError::SolverError(format!(
            "RK45 backend supports forward-time simulation only (t_start={} > t_end={})",
            opts.t_start, opts.t_end
        )));
    }

    validate_simulation_function_support(dae_model)?;
    let prepared = rumoca_sim_core::prepare_dae(dae_model, opts.scalarize, &budget)?;

    let mut dae = prepared.dae;
    let has_dummy_state = prepared.has_dummy_state;
    let elimination = prepared.elimination;
    let ic_blocks = prepared.ic_blocks;

    if has_dummy_state {
        return rumoca_sim_core::run_timeout_result(&budget, || {
            rumoca_sim_core::build_algebraic_result(
                &dae,
                opts,
                &elimination,
                &budget,
                rumoca_sim_core::build_visible_result_names(&dae),
                DUMMY_STATE_NAME,
            )
        });
    }

    let n_x = rumoca_sim_core::count_states(&dae);
    rumoca_sim_core::solve_initial_conditions(&mut dae, &ic_blocks, n_x, opts.atol, &budget)?;

    let causal_form =
        rumoca_sim_core::extract_causal_ode_form(&dae, n_x).map_err(causal_form_error)?;
    let param_values = rumoca_sim_core::default_params_with_budget(&dae, &budget)?;

    let mut y0 = vec![0.0; n_x];
    rumoca_sim_core::initialize_state_vector(&dae, y0.as_mut_slice());
    let _ = rumoca_sim_core::apply_initial_section_assignments(
        &dae,
        y0.as_mut_slice(),
        param_values.as_slice(),
        opts.t_start,
    );
    rumoca_sim_core::refresh_pre_values_from_state_with_initial_assignments(
        &dae,
        y0.as_slice(),
        param_values.as_slice(),
        opts.t_start,
    );

    let buf = integration::integrate_with_rk45(
        &dae,
        causal_form.state_rhs.as_slice(),
        param_values.as_slice(),
        opts,
        &budget,
        y0,
        n_x,
    )?;

    let mut output_names = rumoca_sim_core::equation_scalarize::build_output_names(&dae);
    output_names.truncate(n_x);

    Ok(rumoca_sim_core::finalize_dynamic_result(
        &dae,
        &elimination,
        param_values.as_slice(),
        n_x,
        output_names,
        buf,
    ))
}
