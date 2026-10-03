//! Operator-record operators on vectors (MLS 3.7 §14.5 with §10.6).
//!
//! When no operator function accepts a vector operand directly (as
//! `Complex.'*'.scalarProduct` accepts two), the operator applies element by
//! element: `v + w` is `{v[1] + w[1], ...}`, and a scalar factor multiplies or
//! divides every element. `sum` of a record vector is the chained `'+'` of its
//! elements, and of an empty vector the record's `'0'` element.

use std::collections::BTreeMap;

use rumoca_core::{DefId, Expression, OpBinary, OpUnary, Span};
use rumoca_ir_flat as flat;

use super::{OperandType, Resolver};

/// The one-dimensional record arrays whose elements (`v[1]`, `v[2]`, ...)
/// Flat declares, keyed by the array name.
pub(super) fn record_arrays(
    flat: &flat::Model,
) -> rustc_hash::FxHashMap<String, (DefId, Vec<rumoca_core::ComponentReference>)> {
    let mut arrays: rustc_hash::FxHashMap<
        String,
        (DefId, BTreeMap<i64, rumoca_core::ComponentReference>),
    > = rustc_hash::FxHashMap::default();
    for (name, record) in &flat.record_instances {
        let Some((base, index)) = name
            .as_str()
            .strip_suffix(']')
            .and_then(|head| head.rsplit_once('['))
        else {
            continue;
        };
        let Ok(index) = index.parse::<i64>() else {
            continue;
        };
        let entry = arrays
            .entry(base.to_string())
            .or_insert_with(|| (record.type_def_id, BTreeMap::new()));
        entry.1.insert(index, record.component_ref.clone());
    }
    arrays
        .into_iter()
        .filter(|(_, (_, elements))| {
            elements
                .keys()
                .copied()
                .eq(1..=i64::try_from(elements.len()).unwrap_or(0))
        })
        .map(|(name, (def, elements))| (name, (def, elements.into_values().collect())))
        .collect()
}

impl Resolver<'_, '_, '_> {
    /// The element expressions of a record vector this pass can enumerate.
    pub(super) fn record_elements(
        &self,
        expression: &Expression,
        span: Span,
    ) -> Option<Vec<Expression>> {
        match expression {
            Expression::Array {
                elements,
                kind: rumoca_core::ArrayConstructor::Array,
                ..
            } => Some(elements.clone()),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let Some((_, elements)) = self.scope.record_array(name.as_str()) else {
                    let declaration = name.component_ref()?.target_def_id();
                    return self
                        .catalog
                        .declared_empty_vector(declaration)
                        .then(Vec::new);
                };
                Some(
                    elements
                        .iter()
                        .map(|component| {
                            let element_name =
                                rumoca_core::ComponentPath::from_component_reference(component)
                                    .to_flat_string();
                            Expression::VarRef {
                                name: rumoca_core::Reference::with_component_reference(
                                    element_name,
                                    component.clone(),
                                ),
                                subscripts: Vec::new(),
                                span,
                            }
                        })
                        .collect(),
                )
            }
            _ => None,
        }
    }

    /// Apply a binary operator element by element when an operand is a record
    /// vector and no operator function accepts it whole.
    pub(super) fn elementwise_binary(
        &mut self,
        op: &OpBinary,
        (lhs, left): (&Expression, OperandType),
        (rhs, right): (&Expression, OperandType),
        span: Span,
    ) -> Option<Expression> {
        let scalar_op = match op {
            OpBinary::AddElem => OpBinary::Add,
            OpBinary::SubElem => OpBinary::Sub,
            OpBinary::MulElem => OpBinary::Mul,
            OpBinary::DivElem => OpBinary::Div,
            other => other.clone(),
        };
        let pairs = match (left, right) {
            (OperandType::RecordVector(_), OperandType::RecordVector(_)) => {
                let lhs = self.record_elements(lhs, span)?;
                let rhs = self.record_elements(rhs, span)?;
                (lhs.len() == rhs.len()).then(|| lhs.into_iter().zip(rhs).collect::<Vec<_>>())?
            }
            (OperandType::RecordVector(_), _)
                if matches!(scalar_op, OpBinary::Mul | OpBinary::Div) =>
            {
                self.record_elements(lhs, span)?
                    .into_iter()
                    .map(|element| (element, rhs.clone()))
                    .collect()
            }
            (_, OperandType::RecordVector(_)) if matches!(scalar_op, OpBinary::Mul) => self
                .record_elements(rhs, span)?
                .into_iter()
                .map(|element| (lhs.clone(), element))
                .collect(),
            _ => return None,
        };
        let elements = pairs
            .into_iter()
            .map(|(lhs, rhs)| self.resolve_binary(&scalar_op, lhs, rhs, span))
            .collect();
        Some(Expression::Array {
            elements,
            kind: rumoca_core::ArrayConstructor::Array,
            span,
        })
    }

    /// The element pairs of an equation between two record vectors of equal
    /// length, such as `s = v + w` once `v + w` is an element array.
    pub(super) fn vector_equation_pairs(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Option<Vec<(Expression, Expression)>> {
        let lhs = self.record_elements(lhs, span)?;
        let rhs = self.record_elements(rhs, span)?;
        (lhs.len() == rhs.len()).then(|| lhs.into_iter().zip(rhs).collect())
    }

    /// Negate a record vector element by element.
    pub(super) fn elementwise_negate(
        &mut self,
        rhs: &Expression,
        span: Span,
    ) -> Option<Expression> {
        let elements = self
            .record_elements(rhs, span)?
            .into_iter()
            .map(|element| self.resolve_unary(&OpUnary::Minus, element, span))
            .collect();
        Some(Expression::Array {
            elements,
            kind: rumoca_core::ArrayConstructor::Array,
            span,
        })
    }

    /// `sum` of a record vector: the chained `'+'` of its elements, or the
    /// record's `'0'` element when it has none.
    pub(super) fn record_sum(&mut self, argument: &Expression, span: Span) -> Option<Expression> {
        let OperandType::RecordVector(owner) = self.operand_type(argument) else {
            return None;
        };
        let mut elements = self.record_elements(argument, span)?.into_iter();
        let Some(first) = elements.next() else {
            let zero = self
                .catalog
                .functions(owner, "'0'")
                .into_iter()
                .find(|function| function.required == 0)?;
            return Some(self.call(zero.def_id, Vec::new(), span));
        };
        Some(elements.fold(first, |total, element| {
            self.resolve_binary(&OpBinary::Add, total, element, span)
        }))
    }
}
