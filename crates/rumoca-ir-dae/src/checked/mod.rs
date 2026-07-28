//! Provenance-preserving, valid-by-construction DAE prototype.
//!
//! Construction is scoped by a generative lifetime. IDs cannot escape an
//! inspection scope:
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::checked::Dae;
//! # let dae = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! let escaped = dae.inspect(|view| view.expression_id(0));
//! ```
//!
//! IDs from two DAEs also cannot be mixed:
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::checked::Dae;
//! # let left = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! # let right = Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
//! left.inspect(|left_view| {
//!     right.inspect(|right_view| {
//!         right_view.expression(left_view.expression_id(0).unwrap());
//!     })
//! });
//! ```
//!
//! Expression insertion has no provenance-free operation:
//!
//! ```compile_fail
//! # use rumoca_core::SourceMap;
//! # use rumoca_ir_dae::checked::{Dae, DaeLiteral};
//! # Dae::construct(SourceMap::new(), |dae| {
//! dae.expressions(|expr| expr.literal(DaeLiteral::Integer(1)))?;
//! # Ok(())
//! # });
//! ```

mod error;
mod expression;
mod ids;
mod model;
mod provenance;

pub use error::DaeConstructionError;
pub use expression::{
    BinaryOperator, CoordinateInput, DaeLiteral, ExpressionAt, Expressions, PureBuiltin,
    ScalarType, Subscript, UnaryOperator, ValueType,
};
pub use ids::{ConditionId, DomainId, EquationId, ExprId, FunctionId, ValueTypeId, VariableId};
pub use model::{
    CHECKED_DAE_SCHEMA_VERSION, Conditions, Dae, DaeConstruction, DaeView, Domains, Equation,
    EquationView, ExpressionKind, ExpressionView, Functions, ValueTypes, VariableDefinition,
    Variables,
};
pub use provenance::{DaeGeneration, DaeProvenance, DaeProvenanceOrigin};

#[cfg(test)]
mod tests;
