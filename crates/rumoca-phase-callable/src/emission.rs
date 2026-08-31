use rumoca_ir_dae::DaeView;
use rumoca_plan_callable::CallablePlanConstruction;

use crate::CallablePhaseError;

/// Fail closed until the nested callable-region construction capability is
/// complete. Returning from here exposes no partial plan because
/// `CallablePlan::construct` owns and discards the in-progress aggregate.
pub(crate) fn emit(
    view: DaeView<'_>,
    _construction: &mut CallablePlanConstruction<'_, '_>,
) -> Result<(), CallablePhaseError> {
    if let Some(function) = view.function_id(0).and_then(|id| view.function(id)) {
        return Err(CallablePhaseError::unsupported(
            "callable-region-construction-in-progress",
            function.declaration().span(),
        ));
    }
    Ok(())
}
