use super::*;

#[test]
fn prepared_simulation_retains_its_admitted_engine_without_guessing() {
    for policy in [
        SimExecutionPolicy::Interpreter,
        SimExecutionPolicy::NativeRequired,
    ] {
        let opts = sim_opts(policy);
        let model = state_fixture(&opts);
        let receipt = rumoca_solver::SimExecutionReceipt::admission(
            policy,
            1,
            0,
            rumoca_solver::SimExecutionEngine::Cranelift,
        );
        let backend = policy.allows_native().then(|| counting_handle(&model).2);
        let prepared = instantiate_prepared_simulation(
            model.artifact().with_execution_receipt(receipt.clone()),
            &opts,
            backend,
        )
        .expect("the selected engine prepares");
        assert_eq!(prepared.execution_receipt(), Some(receipt));
        assert_eq!(prepared.backend(), rumoca_solver::SimBackend::Diffsol);
    }
}
