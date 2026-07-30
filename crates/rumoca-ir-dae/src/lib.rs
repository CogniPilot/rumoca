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
//!
//! Range construction accepts only branded literal-expression occurrences,
//! never raw integer values:
//!
//! ```compile_fail
//! # use rumoca_ir_dae::{
//! #     DaeConstructionError, DaeProvenance, Expressions,
//! # };
//! # fn old_range<'dae>(
//! #     expressions: &mut Expressions<'_, 'dae>,
//! #     provenance: DaeProvenance,
//! # ) -> Result<(), DaeConstructionError> {
//! expressions.at(provenance).range(1, Some(1), 3)?;
//! # Ok(())
//! # }
//! ```
//!
//! Delay timing evidence is consumed by the one provenance-selected
//! coordinate occurrence and cannot be reused:
//!
//! ```compile_fail
//! # use rumoca_ir_dae::{
//! #     DaeConstruction, DaeConstructionError, DaeProvenance, ExprId, PositiveParameter,
//! # };
//! # fn duplicate_delay<'dae>(
//! #     dae: &mut DaeConstruction<'dae>,
//! #     source: ExprId<'dae>,
//! #     timing: PositiveParameter<'dae>,
//! #     owner: DaeProvenance,
//! # ) -> Result<(), DaeConstructionError> {
//! dae.expressions(|expr| expr.at(owner).delay(source, timing, owner))?;
//! dae.expressions(|expr| expr.at(owner).delay(source, timing, owner))?;
//! # Ok(())
//! # }
//! ```
//!
//! A function-construction capability cannot escape its lexical owner:
//!
//! ```compile_fail
//! # use rumoca_ir_dae::{
//! #     DaeConstruction, DaeConstructionError, FunctionSignature,
//! # };
//! # fn escape<'dae>(
//! #     dae: &mut DaeConstruction<'dae>,
//! #     signature: FunctionSignature<'dae>,
//! # ) -> Result<(), DaeConstructionError> {
//! let escaped = dae.function(signature, |_, reservation| Ok(reservation))?;
//! drop(escaped);
//! # Ok(())
//! # }
//! ```
//!
//! An active function loop owns its parent body until the loop is finished:
//!
//! ```compile_fail
//! # use rumoca_ir_dae::{
//! #     DaeProvenance, DomainId, FunctionBody, FunctionValueId, Functions,
//! # };
//! # fn cannot_mutate_parent<'dae>(
//! #     functions: &mut Functions<'_, 'dae>,
//! #     body: FunctionBody<'dae>,
//! #     domain: DomainId<'dae>,
//! #     target: FunctionValueId<'dae>,
//! #     provenance: DaeProvenance,
//! # ) {
//! let _active = functions.begin_loop(body, domain, [target], provenance);
//! drop(body);
//! # }
//! ```

mod clocks;
mod conditions;
mod discrete_values;
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
pub use discrete_values::{
    DiscreteBranchActivation, DiscreteValueBranchValues, DiscreteValueBranchView,
    DiscreteValueBranches, DiscreteValueOwner, DiscreteValueOwnerView, DiscreteValueTargets,
    DiscreteValueTopology,
};
pub use equations::{
    ContinuousEquations, DiscreteEquations, DiscreteRealActivation, DiscreteRealEquationView,
    InitializationEquations, ResidualEquation, StructuredResiduals,
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
    DelayId, DiscreteRealEquationId, DiscreteRealId, DiscreteValueId, DiscreteValueOwnerId,
    DomainBinderId, DomainId, EventActionId, ExprId, FunctionDefinitionId, FunctionFoldId,
    FunctionId, FunctionParameterId, FunctionValueId, InitializationEquationId,
    InitializationFamilyId, InputId, ParameterId, PreviousId, RelationId, RootId, StateId,
    TerminalId, TimeEventId, ValueTypeId, VariableId,
};
pub use model::{
    ContinuousOwnerView, CoordinateView, DAE_SCHEMA_VERSION, Dae, DaeConstruction, DaeView,
    DomainView, Domains, ExpressionKind, ExpressionOperands, ExpressionOperation, ExpressionView,
    FunctionBody, FunctionDefinitionValues, FunctionDefinitionView, FunctionFoldView, FunctionLoop,
    FunctionParameterView, FunctionReservation, FunctionSignature, FunctionStatementView,
    FunctionStatements, FunctionValueRole, FunctionValueView, FunctionView, Functions,
    InitializationOwnerView, InputVariability, RangeBoundView, RangeView, ResidualEquationView,
    StructuredFamilyView, SubscriptView, SubscriptsView, ValueTypeOperands, ValueTypes,
    VariableAttributes, VariableCausality, VariableIdentity, VariableOrigin, VariableReservation,
    VariableRole, VariableView, Variables,
};
pub use provenance::{DaeGeneration, DaeProvenance, DaeProvenanceOrigin};
pub use temporal::{
    DelayCoordinate, DelayOperation, DelayView, PositiveParameter, PositiveParameterView,
    PreviousView, Temporal, TerminalView,
};

#[cfg(test)]
mod tests;
