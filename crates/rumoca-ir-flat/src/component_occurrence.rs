//! Which component class equation a flat equation instantiates, and for which component.
//!
//! Flattening instantiates a class equation once per component of that class. The
//! occurrence keeps that relation, so later phases can carry repeated component
//! structure (one body per class equation, called per component) instead of
//! rediscovering it from rendered names, spans or emitted code.
use rumoca_core::{InstanceId, ScopeId, Span};
use serde::{Deserialize, Serialize};

/// One equation as written in a class: its declaring scope and its source span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ClassEquation {
    pub scope: ScopeId,
    pub span: Span,
}

/// The class equation a flat equation instantiates, for one component occurrence.
///
/// Flat equations with the same `class_equation` and `view` but different
/// `instance` are the same equation of the same class, instantiated for different
/// components. `view` orders the scalar views one class equation flattens into.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ComponentOccurrence {
    pub class_equation: ClassEquation,
    pub instance: InstanceId,
    pub view: u32,
}
