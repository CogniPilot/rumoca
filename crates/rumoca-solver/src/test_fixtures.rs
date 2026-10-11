//! Canonical structural issuance for directly constructed solver test models.

use rumoca_ir_solve::SolveModel;

pub(crate) fn derive_structural_artifacts(model: &mut SolveModel) {
    let (continuous, initialization) =
        rumoca_eval_solve::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .expect("fixture programs derive their structural artifacts");
    model.artifacts.continuous.structural = continuous;
    model.artifacts.initialization.structural = initialization;
}
