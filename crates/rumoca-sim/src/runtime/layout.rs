use std::collections::HashMap;
use std::ops::Range;

use rumoca_eval_dae::runtime::{VarEnv, build_env};
use rumoca_ir_dae as dae;

#[derive(Clone, Debug)]
pub struct SolverNameIndexMaps {
    pub names: Vec<String>,
    pub name_to_idx: HashMap<String, usize>,
    pub base_to_indices: HashMap<String, Vec<usize>>,
}

#[derive(Clone, Debug)]
pub struct SimulationContext {
    solver_maps: SolverNameIndexMaps,
    state_range: Range<usize>,
    algebraic_range: Range<usize>,
    output_range: Range<usize>,
    parameter_range: Range<usize>,
    input_range: Range<usize>,
    discrete_real_range: Range<usize>,
    discrete_valued_range: Range<usize>,
    constant_range: Range<usize>,
    pre_discrete_real_range: Range<usize>,
    pre_discrete_valued_range: Range<usize>,
    input_scalar_names: Vec<String>,
    discrete_real_scalar_names: Vec<String>,
    discrete_valued_scalar_names: Vec<String>,
    constant_scalar_names: Vec<String>,
}

impl SimulationContext {
    pub fn from_dae(dae_model: &dae::Dae, solver_len: usize) -> Self {
        let state_scalars = scalar_count(dae_model.states.values());
        let algebraic_scalars = scalar_count(dae_model.algebraics.values());
        let output_scalars = scalar_count(dae_model.outputs.values());
        let parameter_scalars = scalar_count(dae_model.parameters.values());
        let input_scalars = scalar_count(dae_model.inputs.values());
        let discrete_real_scalars = scalar_count(dae_model.discrete_reals.values());
        let discrete_valued_scalars = scalar_count(dae_model.discrete_valued.values());
        let constant_scalar_names = collect_scalar_names(dae_model.constants.iter());
        let constant_scalars = constant_scalar_names.len();

        let state_end = state_scalars.min(solver_len);
        let algebraic_end = (state_scalars + algebraic_scalars).min(solver_len);
        let output_end = (state_scalars + algebraic_scalars + output_scalars).min(solver_len);

        let parameter_end = parameter_scalars;
        let input_end = parameter_end + input_scalars;
        let discrete_real_end = input_end + discrete_real_scalars;
        let discrete_valued_end = discrete_real_end + discrete_valued_scalars;

        let pre_discrete_real_end = discrete_real_scalars;
        let pre_discrete_valued_end = discrete_real_scalars + discrete_valued_scalars;

        let solver_names = collect_solver_names(dae_model, solver_len);
        let name_to_idx = solver_names
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.clone(), idx))
            .collect();
        let mut base_to_indices: HashMap<String, Vec<usize>> = HashMap::new();
        for (idx, name) in solver_names.iter().enumerate() {
            let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
            base_to_indices.entry(base).or_default().push(idx);
        }

        Self {
            solver_maps: SolverNameIndexMaps {
                names: solver_names,
                name_to_idx,
                base_to_indices,
            },
            state_range: 0..state_end,
            algebraic_range: state_end..algebraic_end,
            output_range: algebraic_end..output_end,
            parameter_range: 0..parameter_end,
            input_range: parameter_end..input_end,
            discrete_real_range: input_end..discrete_real_end,
            discrete_valued_range: discrete_real_end..discrete_valued_end,
            constant_range: 0..constant_scalars,
            pre_discrete_real_range: 0..pre_discrete_real_end,
            pre_discrete_valued_range: pre_discrete_real_end..pre_discrete_valued_end,
            input_scalar_names: collect_scalar_names(dae_model.inputs.iter()),
            discrete_real_scalar_names: collect_scalar_names(dae_model.discrete_reals.iter()),
            discrete_valued_scalar_names: collect_scalar_names(dae_model.discrete_valued.iter()),
            constant_scalar_names,
        }
    }

    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn solver_names(&self) -> &[String] {
        &self.solver_maps.names
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        solver_idx_for_target(target, &self.solver_maps.name_to_idx)
    }

    pub fn state_view<'a>(&self, y: &'a [f64]) -> &'a [f64] {
        slice_by_range(y, &self.state_range)
    }

    pub fn algebraic_view<'a>(&self, y: &'a [f64]) -> &'a [f64] {
        slice_by_range(y, &self.algebraic_range)
    }

    pub fn output_view<'a>(&self, y: &'a [f64]) -> &'a [f64] {
        slice_by_range(y, &self.output_range)
    }

    pub fn parameter_view<'a>(&self, compiled_p: &'a [f64]) -> &'a [f64] {
        slice_by_range(compiled_p, &self.parameter_range)
    }

    pub fn input_view<'a>(&self, compiled_p: &'a [f64]) -> &'a [f64] {
        slice_by_range(compiled_p, &self.input_range)
    }

    pub fn discrete_real_view<'a>(&self, compiled_p: &'a [f64]) -> &'a [f64] {
        slice_by_range(compiled_p, &self.discrete_real_range)
    }

    pub fn discrete_valued_view<'a>(&self, compiled_p: &'a [f64]) -> &'a [f64] {
        slice_by_range(compiled_p, &self.discrete_valued_range)
    }

    pub fn constants_view<'a>(&self, constants: &'a [f64]) -> &'a [f64] {
        slice_by_range(constants, &self.constant_range)
    }

    pub fn pre_discrete_real_view<'a>(&self, pre_values: &'a [f64]) -> &'a [f64] {
        slice_by_range(pre_values, &self.pre_discrete_real_range)
    }

    pub fn pre_discrete_valued_view<'a>(&self, pre_values: &'a [f64]) -> &'a [f64] {
        slice_by_range(pre_values, &self.pre_discrete_valued_range)
    }

    pub fn constant_scalar_names(&self) -> &[String] {
        &self.constant_scalar_names
    }

    pub fn compiled_parameter_len(&self) -> usize {
        self.discrete_valued_range.end
    }

    pub fn has_runtime_parameter_tail(&self) -> bool {
        !self.input_scalar_names.is_empty()
            || !self.discrete_real_scalar_names.is_empty()
            || !self.discrete_valued_scalar_names.is_empty()
    }

    pub fn compiled_parameter_vector(
        &self,
        dae_model: &dae::Dae,
        y: &[f64],
        parameters: &[f64],
        t: f64,
    ) -> Vec<f64> {
        if !self.has_runtime_parameter_tail() {
            return parameters.to_vec();
        }
        let env = build_env(dae_model, y, parameters, t);
        self.compiled_parameter_vector_from_env(parameters, &env)
    }

    pub fn compiled_parameter_vector_from_env(
        &self,
        parameters: &[f64],
        env: &VarEnv<f64>,
    ) -> Vec<f64> {
        let mut compiled = Vec::with_capacity(self.compiled_parameter_len());
        extend_from_prefix_with_zero_fill(
            &mut compiled,
            parameters,
            self.parameter_range
                .end
                .saturating_sub(self.parameter_range.start),
        );
        extend_env_scalars(&mut compiled, &self.input_scalar_names, env);
        extend_env_scalars(&mut compiled, &self.discrete_real_scalar_names, env);
        extend_env_scalars(&mut compiled, &self.discrete_valued_scalar_names, env);
        compiled
    }

    pub fn sync_solver_values_from_env(&self, y: &mut [f64], env: &VarEnv<f64>) -> usize {
        sync_solver_values_from_env_with_names(self.solver_names(), y, env)
    }
}

fn scalar_count<'a>(vars: impl Iterator<Item = &'a dae::Variable>) -> usize {
    vars.map(dae::Variable::size).sum()
}

fn var_scalar_names(name: &str, var: &dae::Variable) -> Vec<String> {
    let size = var.size();
    if size <= 1 {
        return vec![name.to_string()];
    }
    (1..=size).map(|idx| format!("{name}[{idx}]")).collect()
}

fn collect_scalar_names<'a>(
    vars: impl Iterator<Item = (&'a dae::VarName, &'a dae::Variable)>,
) -> Vec<String> {
    vars.flat_map(|(name, var)| var_scalar_names(name.as_str(), var))
        .collect()
}

fn collect_solver_names(dae_model: &dae::Dae, solver_len: usize) -> Vec<String> {
    let mut names = collect_scalar_names(
        dae_model
            .states
            .iter()
            .chain(dae_model.algebraics.iter())
            .chain(dae_model.outputs.iter()),
    );
    names.truncate(solver_len);
    names
}

fn slice_by_range<'a>(values: &'a [f64], range: &Range<usize>) -> &'a [f64] {
    if range.start >= values.len() {
        return &[];
    }
    let end = range.end.min(values.len());
    &values[range.start..end]
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
    SimulationContext::from_dae(dae_model, n_total)
        .solver_maps
        .names
}

pub fn solver_idx_for_target(target: &str, name_to_idx: &HashMap<String, usize>) -> Option<usize> {
    if let Some(&idx) = name_to_idx.get(target) {
        return Some(idx);
    }
    if let Some((base, raw_indices)) = target.split_once('[') {
        let indices = raw_indices.strip_suffix(']').unwrap_or(raw_indices);
        let all_one = indices
            .split(',')
            .all(|part| part.trim() == "1" || part.trim().is_empty());
        if all_one {
            return name_to_idx.get(base).copied();
        }
    }
    None
}

pub fn build_solver_name_index_maps(dae_model: &dae::Dae, y_len: usize) -> SolverNameIndexMaps {
    SimulationContext::from_dae(dae_model, y_len).solver_maps
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
        let map = HashMap::from([(String::from("x"), 3usize)]);
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
    fn simulation_context_zero_copy_partition_views_match_layout() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable {
                name: dae::VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.algebraics.insert(
            dae::VarName::new("a"),
            dae::Variable::new(dae::VarName::new("a")),
        );
        dae_model.outputs.insert(
            dae::VarName::new("w"),
            dae::Variable::new(dae::VarName::new("w")),
        );
        dae_model.parameters.insert(
            dae::VarName::new("p"),
            dae::Variable {
                name: dae::VarName::new("p"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable {
                name: dae::VarName::new("u"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("m"),
            dae::Variable::new(dae::VarName::new("m")),
        );
        dae_model.constants.insert(
            dae::VarName::new("c"),
            dae::Variable::new(dae::VarName::new("c")),
        );

        let context = SimulationContext::from_dae(&dae_model, 4);
        let y = vec![10.0, 20.0, 30.0, 40.0];
        let compiled_p = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
        let pre = vec![7.0, 8.0];
        let constants = vec![9.0];

        assert_eq!(context.state_view(&y), &[10.0, 20.0]);
        assert_eq!(context.algebraic_view(&y), &[30.0]);
        assert_eq!(context.output_view(&y), &[40.0]);
        assert_eq!(context.parameter_view(&compiled_p), &[1.0, 2.0]);
        assert_eq!(context.input_view(&compiled_p), &[3.0, 4.0]);
        assert_eq!(context.discrete_real_view(&compiled_p), &[5.0]);
        assert_eq!(context.discrete_valued_view(&compiled_p), &[6.0]);
        assert_eq!(context.pre_discrete_real_view(&pre), &[7.0]);
        assert_eq!(context.pre_discrete_valued_view(&pre), &[8.0]);
        assert_eq!(context.constants_view(&constants), &[9.0]);
        assert_eq!(context.constant_scalar_names(), &[String::from("c")]);
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
        assert_eq!(context.compiled_parameter_len(), compiled.len());
    }
}
