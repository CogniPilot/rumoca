//! An assertion whose condition is a pure call's Boolean result holds when the
//! call returns true, on the portable backend as on the reference.
use super::*;

const SOURCE: &str = r#"
function holds
  output Boolean ok;
algorithm
  ok := true;
end holds;

model AssertedCall
  Real x;
equation
  der(x) = 1;
  assert(holds(), "a call returning true failed its assertion");
end AssertedCall;
"#;

#[test]
fn an_assertion_on_a_pure_call_result_holds_on_the_portable_backend() {
    let mut session = rumoca_compile::Session::new(Default::default());
    session.update_document("asserted_call.mo", SOURCE);
    let compiled = session.compile_model("AssertedCall").unwrap();
    let opts = rumoca_solver::SimOptions {
        t_end: 0.1,
        ..Default::default()
    };
    let lowered =
        crate::lower_correlated_for_simulation_with_overrides(&compiled.dae, &opts).unwrap();
    let backend = admitted_native_execution_backend(&opts, lowered.model())
        .unwrap()
        .expect("the portable backend admits the model");
    let receipt = execution_receipt(&opts, lowered.model());
    let component = rumoca_phase_solve::fmi::finish_fmi_component(lowered).unwrap();
    let artifact =
        rumoca_solver::fmi_me::MeModelArtifact::new(component).with_execution_receipt(receipt);
    let result = crate::rk45::simulate_artifact(artifact, &opts, Some(backend));
    assert!(result.is_ok(), "{:?}", result.err());
}
