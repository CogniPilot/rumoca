//! Output demand and liveness are owned by the typed program (SOLVE-C95, SOLVE-C102).
pub(crate) use rumoca_ir_solve::SolveOutputDemand as Demand;
pub(in crate::typed_call) use rumoca_ir_solve::{
    SolveLiveness as Liveness, visit_demanded_calls as visit_calls,
    visit_demanded_reads as visit_reads,
};
