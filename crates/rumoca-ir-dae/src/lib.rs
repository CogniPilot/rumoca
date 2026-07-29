//! Valid-by-construction MLS Appendix B DAE representation.
//!
//! [`Dae::construct`] is the only root constructor. It lends sequential
//! semantic-owner capabilities one generatively branded aggregate and returns
//! an immutable DAE only after every permitted forward definition is complete.
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::Dae;
//! # let dae = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! let escaped = dae.inspect(|view| view.expression_id(0));
//! ```
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::Dae;
//! # let left = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! # let right = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! left.inspect(|left_view| {
//!     right.inspect(|right_view| {
//!         right_view.expression(left_view.expression_id(0).unwrap());
//!     })
//! });
//! ```
//!
//! Expression insertion cannot omit provenance:
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::{Dae, DaeLiteral};
//! # Dae::construct(SourceMap::new(), |dae| {
//! dae.expressions(|expr| expr.literal(DaeLiteral::Integer(1)))?;
//! # Ok(())
//! # });
//! ```

mod clocks;
mod conditions;
mod equations;
mod error;
mod events;
mod expr_query;
mod expression;
mod ids;
mod model;
mod provenance;
mod temporal;

pub use clocks::{ClockOperation, ClockOwnershipView, ClockView, ClockedVariableKind, Clocks};
pub use conditions::{
    ConditionInput, ConditionOperation, ConditionView, Conditions, RelationView, RootView,
};
pub use equations::{
    ContinuousEquations, DiscreteAssignmentView, DiscreteEquations, InitializationEquations,
    ResidualEquation, StructuredResiduals,
};
pub use error::DaeConstructionError;
pub use events::{EventActionOperation, EventActionView, Events, TimeEventView};
pub use expr_query::{
    expr_contains_der_of, expr_contains_der_of_any, expr_contains_var, expr_refers_to_var,
    for_each_expression,
};
pub use expression::{
    BinaryOperator, CoordinateInput, DaeLiteral, ExpressionAt, ExpressionVariability, Expressions,
    PureBuiltin, ScalarType, Subscript, UnaryOperator, ValueType,
};
pub use ids::{
    AlgebraicId, ClockId, ClockOwnershipId, ConditionId, ContinuousEquationId, ContinuousFamilyId,
    DelayId, DiscreteAssignmentId, DiscreteRealEquationId, DiscreteRealId, DiscreteValueId,
    DomainBinderId, DomainId, EventActionId, ExprId, FunctionDefinitionId, FunctionFoldId,
    FunctionId, FunctionParameterId, FunctionValueId, InitializationEquationId,
    InitializationFamilyId, InputId, ParameterId, PreviousId, RelationId, RootId, StateId,
    TerminalId, TimeEventId, ValueTypeId, VariableId,
};
pub use model::{
    ContinuousOwnerView, CoordinateView, DAE_SCHEMA_VERSION, Dae, DaeConstruction, DaeView,
    DomainView, Domains, ExpressionKind, ExpressionOperands, ExpressionOperation, ExpressionView,
    FunctionBody, FunctionDefinitionValues, FunctionDefinitionView, FunctionFoldView, FunctionLoop,
    FunctionParameterView, FunctionReservation, FunctionStatementView, FunctionStatements,
    FunctionValueRole, FunctionValueView, FunctionView, Functions, InitializationOwnerView,
    InputVariability, ResidualEquationView, StructuredFamilyView, SubscriptView, SubscriptsView,
    ValueTypeOperands, ValueTypes, VariableAttributes, VariableCausality, VariableIdentity,
    VariableOrigin, VariableReservation, VariableRole, VariableView, Variables,
};
pub use provenance::{DaeGeneration, DaeProvenance, DaeProvenanceOrigin};
pub use temporal::{
    DelayCoordinate, DelayView, PositiveParameter, PositiveParameterView, PreviousView, Temporal,
    TerminalView,
};

#[cfg(test)]
mod tests;
