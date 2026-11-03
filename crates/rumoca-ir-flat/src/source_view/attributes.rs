//! Source attribute presentation from exact finalized Flat occurrence facts.

use crate::{Model, ModelShapeContractError, Variable};
use rumoca_core::{ComponentPath, Expression, InstanceId, Reference, Span};
use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceAttributeKind {
    Start,
    Min,
    Max,
    Nominal,
}

/// Original attribute expression plus its checked source presentation choice.
#[derive(Debug)]
pub struct SourceAttributeValue<'model> {
    kind: SourceAttributeKind,
    expression: &'model Expression,
    dimensions: Box<[i64]>,
    each: bool,
}

impl<'model> SourceAttributeValue<'model> {
    pub fn kind(&self) -> SourceAttributeKind {
        self.kind
    }

    pub fn expression(&self) -> &'model Expression {
        self.expression
    }

    pub fn dimensions(&self) -> &[i64] {
        &self.dimensions
    }

    pub fn each(&self) -> bool {
        self.each
    }
}

#[derive(Debug)]
pub enum SourceAttributeError {
    MissingVariable(InstanceId),
    UnknownShape {
        variable: InstanceId,
        attribute: SourceAttributeKind,
        span: Span,
    },
    ShapeMismatch {
        variable: InstanceId,
        attribute: SourceAttributeKind,
        span: Span,
    },
}

/// One immutable borrow of a finalized canonical model; no cached shape sidecar.
pub struct SourceAttributeCatalog<'model> {
    variables: HashMap<InstanceId, &'model Variable>,
}

impl Model {
    pub fn source_attributes(&self) -> Result<SourceAttributeCatalog<'_>, ModelShapeContractError> {
        self.validate()?;
        Ok(SourceAttributeCatalog {
            variables: self
                .variables
                .values()
                .map(|variable| (variable.instance_id, variable))
                .collect(),
        })
    }
}

impl<'model> SourceAttributeCatalog<'model> {
    /// No partial list is returned when an attribute lacks a canonical shape.
    pub fn variable(
        &self,
        instance: InstanceId,
    ) -> Result<Vec<SourceAttributeValue<'model>>, SourceAttributeError> {
        let variable = self
            .variables
            .get(&instance)
            .ok_or(SourceAttributeError::MissingVariable(instance))?;
        let sources = [
            (SourceAttributeKind::Start, variable.start.as_ref()),
            (SourceAttributeKind::Min, variable.min.as_ref()),
            (SourceAttributeKind::Max, variable.max.as_ref()),
            (SourceAttributeKind::Nominal, variable.nominal.as_ref()),
        ];
        sources
            .into_iter()
            .filter_map(|(kind, expression)| expression.map(|expression| (kind, expression)))
            .map(|(kind, expression)| self.attribute(variable, kind, expression))
            .collect()
    }

    fn attribute(
        &self,
        variable: &Variable,
        kind: SourceAttributeKind,
        expression: &'model Expression,
    ) -> Result<SourceAttributeValue<'model>, SourceAttributeError> {
        let dimensions = self
            .dimensions(expression)
            .ok_or(SourceAttributeError::UnknownShape {
                variable: variable.instance_id,
                attribute: kind,
                span: expression.span().unwrap_or(variable.source_span),
            })?;
        if !dimensions.is_empty() && dimensions != variable.dims {
            return Err(SourceAttributeError::ShapeMismatch {
                variable: variable.instance_id,
                attribute: kind,
                span: expression.span().unwrap_or(variable.source_span),
            });
        }
        Ok(SourceAttributeValue {
            kind,
            expression,
            each: !variable.dims.is_empty() && dimensions.is_empty(),
            dimensions: dimensions.into_boxed_slice(),
        })
    }

    fn dimensions(&self, expression: &Expression) -> Option<Vec<i64>> {
        match expression {
            Expression::Literal { .. } => Some(Vec::new()),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() && unselected_reference(name) => {
                let variable = self.variables.get(&name.instance_id()?)?;
                // An occurrence identity cannot authorize a differently named reference.
                (ComponentPath::from_reference(name).var_name() == &variable.name)
                    .then(|| variable.dims.clone())
            }
            Expression::Array { elements, kind, .. } => {
                let operands = elements
                    .iter()
                    .map(|element| {
                        self.dimensions(element)?
                            .into_iter()
                            .map(|dimension| usize::try_from(dimension).ok())
                            .collect::<Option<Vec<_>>>()
                    })
                    .collect::<Option<Vec<_>>>()?;
                kind.checked_dimensions(&operands)?
                    .into_iter()
                    .map(|dimension| i64::try_from(dimension).ok())
                    .collect()
            }
            // Calls, selections and compound expressions need their own retained
            // canonical shape facts. Unknown is never treated as scalar here.
            _ => None,
        }
    }
}

impl std::fmt::Display for SourceAttributeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "source attribute presentation refused: {self:?}")
    }
}

impl std::error::Error for SourceAttributeError {}

fn unselected_reference(reference: &Reference) -> bool {
    reference
        .component_ref()
        .is_none_or(|component| component.parts().iter().all(|part| part.subs.is_empty()))
}
