use rumoca_ir_dae as dae;

pub fn build_state_name_to_idx(dae_model: &dae::Dae) -> Vec<(String, usize)> {
    dae_model
        .states
        .keys()
        .enumerate()
        .map(|(idx, name)| (name.as_str().to_string(), idx))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_state_name_to_idx_tracks_state_order() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae_model.states.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );
        let index = build_state_name_to_idx(&dae_model);
        assert_eq!(index, vec![("x".to_string(), 0), ("y".to_string(), 1)]);
    }
}
