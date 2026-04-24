use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

pub type SolverNameIndexMaps = solve::SolverNameIndexMaps;

#[derive(Clone, Debug)]
pub struct SimulationContext {
    layout: solve::SolveLayout,
}

impl SimulationContext {
    pub fn from_dae(dae_model: &dae::Dae, solver_len: usize) -> Self {
        Self {
            layout: rumoca_phase_solve_lower::lower_solve_layout(dae_model, solver_len),
        }
    }

    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        self.layout.solver_maps()
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        self.layout.solver_idx_for_target(target)
    }

    pub fn input_scalar_names(&self) -> &[String] {
        self.layout.input_scalar_names()
    }

    pub fn has_runtime_parameter_tail(&self) -> bool {
        self.layout.has_runtime_parameter_tail()
    }

    pub fn compiled_parameter_vector_from_env(
        &self,
        parameters: &[f64],
        env: &VarEnv<f64>,
    ) -> Vec<f64> {
        let mut compiled = Vec::new();
        self.fill_compiled_parameter_vector_from_env(&mut compiled, parameters, env);
        compiled
    }

    pub fn fill_compiled_parameter_vector_from_env(
        &self,
        out: &mut Vec<f64>,
        parameters: &[f64],
        env: &VarEnv<f64>,
    ) {
        out.clear();
        out.reserve(self.layout.compiled_parameter_len);
        extend_from_prefix_with_zero_fill(out, parameters, self.layout.parameter_count);
        extend_env_scalars(out, &self.layout.input_scalar_names, env);
        extend_env_scalars(out, &self.layout.discrete_real_scalar_names, env);
        extend_env_scalars(out, &self.layout.discrete_valued_scalar_names, env);
    }

    pub fn sync_solver_values_from_env(&self, y: &mut [f64], env: &VarEnv<f64>) -> usize {
        sync_solver_values_from_env_with_names(&self.layout.solver_maps.names, y, env)
    }
}

fn extend_from_prefix_with_zero_fill(out: &mut Vec<f64>, source: &[f64], count: usize) {
    let available = source.len().min(count);
    out.extend_from_slice(&source[..available]);
    if count > available {
        out.resize(out.len() + (count - available), 0.0);
    }
}

fn extend_env_scalars(out: &mut Vec<f64>, names: &[String], env: &VarEnv<f64>) {
    out.extend(names.iter().map(|name| env.get(name)));
}

pub fn solver_vector_names(dae_model: &dae::Dae, n_total: usize) -> Vec<String> {
    rumoca_phase_solve_lower::solver_vector_names(dae_model, n_total)
}

pub fn solver_idx_for_target(
    target: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
) -> Option<usize> {
    solve::solver_idx_for_target(target, name_to_idx)
}

pub fn build_solver_name_index_maps(dae_model: &dae::Dae, y_len: usize) -> SolverNameIndexMaps {
    rumoca_phase_solve_lower::build_solver_name_index_maps(dae_model, y_len)
}

pub fn sync_solver_values_from_env_with_names(
    solver_names: &[String],
    y: &mut [f64],
    env: &VarEnv<f64>,
) -> usize {
    let mut updates = 0usize;
    for (idx, name) in solver_names.iter().enumerate().take(y.len()) {
        let Some(value) = env.vars.get(name).copied() else {
            continue;
        };
        if (y[idx] - value).abs() <= 1.0e-12 {
            continue;
        }
        y[idx] = value;
        updates += 1;
    }
    updates
}

pub fn sync_solver_values_from_env(
    dae_model: &dae::Dae,
    y: &mut [f64],
    env: &VarEnv<f64>,
) -> usize {
    let context = SimulationContext::from_dae(dae_model, y.len());
    context.sync_solver_values_from_env(y, env)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solver_idx_for_target_maps_first_element_to_base() {
        let map = std::collections::HashMap::from([(String::from("x"), 3usize)]);
        assert_eq!(solver_idx_for_target("x", &map), Some(3));
        assert_eq!(solver_idx_for_target("x[1]", &map), Some(3));
        assert_eq!(solver_idx_for_target("x[2]", &map), None);
    }

    #[test]
    fn build_solver_name_index_maps_uses_component_bases() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable {
                name: dae::VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        let maps = build_solver_name_index_maps(&dae_model, 2);
        assert_eq!(maps.names, vec!["x[1]".to_string(), "x[2]".to_string()]);
        assert_eq!(
            maps.base_to_indices.get("x").cloned().unwrap_or_default(),
            vec![0, 1]
        );
    }

    #[test]
    fn sync_solver_values_from_env_with_names_updates_changed_slots_only() {
        let solver_names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let mut y = vec![1.0, 2.0, 3.0];
        let mut env = VarEnv::new();
        env.set("x", 1.0);
        env.set("y", 5.0);
        env.set("unused", 7.0);
        let updates = sync_solver_values_from_env_with_names(&solver_names, &mut y, &env);
        assert_eq!(updates, 1);
        assert_eq!(y, vec![1.0, 5.0, 3.0]);
    }

    #[test]
    fn simulation_context_maps_scalar_array_and_field_names() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae_model.states.insert(
            dae::VarName::new("arr"),
            dae::Variable {
                name: dae::VarName::new("arr"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.states.insert(
            dae::VarName::new("rec.im"),
            dae::Variable::new(dae::VarName::new("rec.im")),
        );
        dae_model.algebraics.insert(
            dae::VarName::new("a"),
            dae::Variable::new(dae::VarName::new("a")),
        );

        let context = SimulationContext::from_dae(&dae_model, 5);
        assert_eq!(context.solver_idx_for_target("x"), Some(0));
        assert_eq!(context.solver_idx_for_target("x[1]"), Some(0));
        assert_eq!(context.solver_idx_for_target("arr[1]"), Some(1));
        assert_eq!(context.solver_idx_for_target("arr[2]"), Some(2));
        assert_eq!(context.solver_idx_for_target("rec.im"), Some(3));
        assert_eq!(context.solver_idx_for_target("a"), Some(4));
    }

    #[test]
    fn compiled_parameter_vector_from_env_uses_scalar_array_and_field_names() {
        let mut dae_model = dae::Dae::default();
        dae_model.parameters.insert(
            dae::VarName::new("p"),
            dae::Variable::new(dae::VarName::new("p")),
        );
        dae_model.inputs.insert(
            dae::VarName::new("inSig"),
            dae::Variable {
                name: dae::VarName::new("inSig"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("plant.z"),
            dae::Variable::new(dae::VarName::new("plant.z")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("plant.mode"),
            dae::Variable::new(dae::VarName::new("plant.mode")),
        );

        let context = SimulationContext::from_dae(&dae_model, 0);
        let mut env = VarEnv::new();
        env.set("inSig[1]", 1.0);
        env.set("inSig[2]", 2.0);
        env.set("plant.z", 3.0);
        env.set("plant.mode", 4.0);

        let compiled = context.compiled_parameter_vector_from_env(&[42.0], &env);
        assert_eq!(compiled, vec![42.0, 1.0, 2.0, 3.0, 4.0]);
        assert_eq!(compiled.len(), 5);
    }
}
