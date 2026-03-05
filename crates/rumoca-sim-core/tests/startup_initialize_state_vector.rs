use rumoca_ir_dae as dae;
use rumoca_sim_core::runtime::startup::initialize_state_vector;

#[test]
fn test_initialize_state_vector_respects_state_then_algebraic_order() {
    let mut dae_model = dae::Dae::new();
    dae_model.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae_model.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    let mut y = vec![1.0, 2.0];
    initialize_state_vector(&dae_model, &mut y);
    assert_eq!(y, vec![0.0, 0.0]);
}
