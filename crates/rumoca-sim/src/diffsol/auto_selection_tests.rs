use super::{BdfCapability, classify_bdf_capability};
use crate::SimError;

#[test]
fn only_directional_derivative_unavailability_is_a_capability_result() {
    let ordinary_failure = classify_bdf_capability(Err(SimError::EmptySystem));
    assert!(matches!(ordinary_failure, Err(SimError::EmptySystem)));

    let unavailable = classify_bdf_capability(Err(SimError::ModelExchangeSession(
        rumoca_solver::fmi_me::session::MeSessionError::Component(
            rumoca_solver::fmi_me::MeError::DirectionalDerivativeUnavailable {
                reason: "undefined local sensitivity".to_owned(),
            },
        ),
    )))
    .expect("the one typed capability result selects the explicit host");
    assert_eq!(
        unavailable,
        BdfCapability::InitialLinearizationUnavailable {
            reason: "undefined local sensitivity".to_owned(),
        }
    );
}
