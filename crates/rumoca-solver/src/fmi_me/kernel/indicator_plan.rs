//! Runtime-local names for immutable event-indicator facts issued by checked
//! [`FmiComponent`](rumoca_ir_solve::fmi::FmiComponent) construction.
//!
//! There is deliberately no constructor or `Clone` implementation here. The
//! linked component shares the construction-owned table and performs only
//! positional reads during lifecycle operations.

pub(crate) use rumoca_ir_solve::fmi::{
    FmiEventIndicatorPlan as FmiIndicatorPlan, FmiIndicatorReading as IndicatorReading,
    FmiIndicatorZeroSide as IndicatorZeroSide,
};
