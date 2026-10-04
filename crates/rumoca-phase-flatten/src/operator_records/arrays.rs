//! Operator-record operators on vectors (MLS 3.7 §14.5 with §10.6).
//!
//! When no operator function accepts a vector operand directly (as
//! `Complex.'*'.scalarProduct` accepts two), `+`, `-`, and the element-wise
//! operators apply element by element: `v + w` is `{v[1] + w[1], ...}`, and a
//! scalar factor multiplies or divides every element. `v * w` of two vectors
//! is their scalar product (§14.4, §10.6.4): the chained `'+'` of the element
//! products, or the record's `'0'` element for empty vectors. Any other
//! operator between two record vectors is left unresolved. `sum` of a record
//! vector is the chained `'+'` of its elements, and of an empty vector the
//! record's `'0'` element.

use std::collections::BTreeMap;

use rumoca_core::{DefId, Expression, OpBinary, OpUnary, Span};
use rumoca_ir_flat as flat;

use super::{OperandType, Resolver};

/// The one-dimensional record vectors whose elements Flat declares, keyed by
/// the vector's name: a record array `v` of elements `v[1]`, `v[2]`, ..., and
/// a record field read through one array of components, `plug.pin.v` of
/// elements `plug.pin[1].v`, `plug.pin[2].v`, ... (MLS §10.1).
pub(super) fn record_arrays(
    flat: &flat::Model,
) -> rustc_hash::FxHashMap<String, (DefId, Vec<rumoca_core::ComponentReference>)> {
    let mut arrays: rustc_hash::FxHashMap<
        String,
        (DefId, BTreeMap<i64, rumoca_core::ComponentReference>),
    > = rustc_hash::FxHashMap::default();
    for record in flat.record_instances.values() {
        let Some((base, index)) = vector_element(&record.component_ref) else {
            continue;
        };
        let entry = arrays
            .entry(base)
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

/// The repeated element `c` of a vector `fill(c, n)`.
fn vector_fill_value(expression: &Expression) -> Option<&Expression> {
    match expression {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            ..
        } => match args.as_slice() {
            [value, _length] => Some(value),
            _ => None,
        },
        _ => None,
    }
}

/// The vector name and index of a record element whose reference carries
/// exactly one subscript, a literal index on one of its parts: `plug.pin[2].v`
/// is element 2 of `plug.pin.v`.
fn vector_element(reference: &rumoca_core::ComponentReference) -> Option<(String, i64)> {
    let mut subscripted = reference
        .parts()
        .iter()
        .filter(|part| !part.subs.is_empty());
    let part = subscripted.next()?;
    if subscripted.next().is_some() {
        return None;
    }
    let [rumoca_core::Subscript::Index { value, .. }] = part.subs.as_slice() else {
        return None;
    };
    // Every other part is unsubscripted, so the vector's name is the parts'
    // identifiers alone.
    let base = rumoca_core::ComponentPath::from_parts(
        reference.parts().iter().map(|part| part.ident.clone()),
    );
    Some((base.to_flat_string(), *value))
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
            (OperandType::RecordVector(owner), OperandType::RecordVector(_))
                if matches!(op, OpBinary::Mul) =>
            {
                return self.record_scalar_product(owner, lhs, rhs, span);
            }
            (OperandType::RecordVector(_), OperandType::RecordVector(_))
                if matches!(
                    op,
                    OpBinary::Add
                        | OpBinary::Sub
                        | OpBinary::AddElem
                        | OpBinary::SubElem
                        | OpBinary::MulElem
                        | OpBinary::DivElem
                ) =>
            {
                let lhs = self.record_elements(lhs, span)?;
                let rhs = self.record_elements(rhs, span)?;
                (lhs.len() == rhs.len()).then(|| lhs.into_iter().zip(rhs).collect::<Vec<_>>())?
            }
            // No other operator between two record vectors is element-wise
            // (MLS 3.7 §10.6).
            (OperandType::RecordVector(_), OperandType::RecordVector(_)) => return None,
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

    /// `v * w` of two record vectors of equal length, MLS 3.7 §14.4 with
    /// §10.6.4: the chained `'+'` of the element products `v[i] * w[i]`, or
    /// the record's `'0'` element when both are empty.
    fn record_scalar_product(
        &mut self,
        owner: DefId,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Option<Expression> {
        let lhs = self.record_elements(lhs, span)?;
        let rhs = self.record_elements(rhs, span)?;
        if lhs.len() != rhs.len() {
            return None;
        }
        let mut products = lhs
            .into_iter()
            .zip(rhs)
            .map(|(lhs, rhs)| self.resolve_binary(&OpBinary::Mul, lhs, rhs, span))
            .collect::<Vec<_>>()
            .into_iter();
        let Some(first) = products.next() else {
            return self.record_zero(owner, span);
        };
        Some(products.fold(first, |total, product| {
            self.resolve_binary(&OpBinary::Add, total, product, span)
        }))
    }

    /// The record's `'0'` element, a call of its argument-free `'0'` operator.
    fn record_zero(&mut self, owner: DefId, span: Span) -> Option<Expression> {
        let zero = self
            .catalog
            .functions(owner, "'0'")
            .into_iter()
            .find(|function| function.required == 0)?;
        Some(self.call(zero.def_id, Vec::new(), span))
    }

    /// The element pairs of an equation between two record vectors of equal
    /// length, such as `s = v + w` once `v + w` is an element array. A side
    /// `fill(c, n)` pairs `c` with every element of the other side, whose
    /// length `n` equals by the equation's size rule (MLS §8.3.1, §10.3.3).
    pub(super) fn vector_equation_pairs(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Option<Vec<(Expression, Expression)>> {
        match (
            self.record_elements(lhs, span),
            self.record_elements(rhs, span),
        ) {
            (Some(lhs), Some(rhs)) => {
                (lhs.len() == rhs.len()).then(|| lhs.into_iter().zip(rhs).collect())
            }
            (Some(lhs), None) => {
                let value = vector_fill_value(rhs)?;
                Some(
                    lhs.into_iter()
                        .map(|element| (element, value.clone()))
                        .collect(),
                )
            }
            (None, Some(rhs)) => {
                let value = vector_fill_value(lhs)?;
                Some(
                    rhs.into_iter()
                        .map(|element| (value.clone(), element))
                        .collect(),
                )
            }
            (None, None) => None,
        }
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
            return self.record_zero(owner, span);
        };
        Some(elements.fold(first, |total, element| {
            self.resolve_binary(&OpBinary::Add, total, element, span)
        }))
    }
}
