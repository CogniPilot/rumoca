use indexmap::IndexMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Default)]
pub(super) struct UniqueExpressions {
    expressions: Vec<rumoca_core::Expression>,
    buckets: IndexMap<u64, Vec<usize>>,
}

impl UniqueExpressions {
    pub(super) fn push(&mut self, candidate: rumoca_core::Expression) {
        let fingerprint = expression_semantic_fingerprint(&candidate);
        if self.has_semantic_match(fingerprint, &candidate) {
            return;
        }

        let idx = self.expressions.len();
        self.expressions.push(candidate);
        self.buckets.entry(fingerprint).or_default().push(idx);
    }

    fn has_semantic_match(&self, fingerprint: u64, candidate: &rumoca_core::Expression) -> bool {
        self.buckets.get(&fingerprint).is_some_and(|bucket| {
            bucket.iter().any(|idx| {
                rumoca_core::expressions_semantically_equal(&self.expressions[*idx], candidate)
            })
        })
    }

    pub(super) fn into_vec(self) -> Vec<rumoca_core::Expression> {
        self.expressions
    }
}

fn expression_semantic_fingerprint(expr: &rumoca_core::Expression) -> u64 {
    let mut hasher = DefaultHasher::new();
    hash_expression_semantics(expr, &mut hasher);
    hasher.finish()
}

fn hash_discriminant<T>(value: &T, hasher: &mut impl Hasher) {
    std::mem::discriminant(value).hash(hasher);
}

fn hash_expression_semantics(expr: &rumoca_core::Expression, hasher: &mut impl Hasher) {
    hash_discriminant(expr, hasher);
    match expr {
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            hash_discriminant(op, hasher);
            hash_expression_semantics(lhs, hasher);
            hash_expression_semantics(rhs, hasher);
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            hash_discriminant(op, hasher);
            hash_expression_semantics(rhs, hasher);
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            name.as_str().hash(hasher);
            hash_subscripts_semantics(subscripts, hasher);
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            hash_discriminant(function, hasher);
            hash_expression_slice_semantics(args, hasher);
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => {
            name.as_str().hash(hasher);
            is_constructor.hash(hasher);
            hash_expression_slice_semantics(args, hasher);
        }
        rumoca_core::Expression::Literal { value, .. } => hash_literal_semantics(value, hasher),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.len().hash(hasher);
            for (cond, value) in branches {
                hash_expression_semantics(cond, hasher);
                hash_expression_semantics(value, hasher);
            }
            hash_expression_semantics(else_branch, hasher);
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            is_matrix.hash(hasher);
            hash_expression_slice_semantics(elements, hasher);
        }
        rumoca_core::Expression::Tuple { elements, .. } => {
            hash_expression_slice_semantics(elements, hasher);
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            hash_expression_semantics(start, hasher);
            step.is_some().hash(hasher);
            if let Some(step) = step {
                hash_expression_semantics(step, hasher);
            }
            hash_expression_semantics(end, hasher);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            hash_expression_semantics(expr, hasher);
            indices.len().hash(hasher);
            for index in indices {
                index.name.hash(hasher);
                hash_expression_semantics(&index.range, hasher);
            }
            filter.is_some().hash(hasher);
            if let Some(filter) = filter {
                hash_expression_semantics(filter, hasher);
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            hash_expression_semantics(base, hasher);
            hash_subscripts_semantics(subscripts, hasher);
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            hash_expression_semantics(base, hasher);
            field.hash(hasher);
        }
        rumoca_core::Expression::Empty { .. } => {}
    }
}

fn hash_expression_slice_semantics(
    expressions: &[rumoca_core::Expression],
    hasher: &mut impl Hasher,
) {
    expressions.len().hash(hasher);
    for expr in expressions {
        hash_expression_semantics(expr, hasher);
    }
}

fn hash_subscripts_semantics(subscripts: &[rumoca_core::Subscript], hasher: &mut impl Hasher) {
    subscripts.len().hash(hasher);
    for subscript in subscripts {
        hash_discriminant(subscript, hasher);
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => value.hash(hasher),
            rumoca_core::Subscript::Colon { .. } => {}
            rumoca_core::Subscript::Expr { expr, .. } => hash_expression_semantics(expr, hasher),
        }
    }
}

fn hash_literal_semantics(value: &rumoca_core::Literal, hasher: &mut impl Hasher) {
    hash_discriminant(value, hasher);
    match value {
        rumoca_core::Literal::Real(value) => value.to_bits().hash(hasher),
        rumoca_core::Literal::Integer(value) => value.hash(hasher),
        rumoca_core::Literal::Boolean(value) => value.hash(hasher),
        rumoca_core::Literal::String(value) => value.hash(hasher),
    }
}
