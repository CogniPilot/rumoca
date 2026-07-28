use std::collections::HashMap;

use indexmap::IndexSet;
use rumoca_core::{
    Expression, ExpressionRewriter, ExpressionVisitor, Reference, Span, Subscript, VarName,
};
use rumoca_ir_dae::{self as dae, Dae};

/// Return the exact scalar coordinate proven by a statically indexed use site.
///
/// DAE variables retain their compact aggregate identity, but a statically
/// indexed occurrence such as `a[3]` is one scalar unknown. Treating that
/// occurrence as the aggregate `a` loses the component equation that defines
/// it; treating a dynamic or out-of-range occurrence as a coordinate would
/// invent a definition. The base must therefore be a declared non-state
/// continuous aggregate and every index must evaluate structurally inside its
/// declared dimension.
pub(super) fn derivative_coordinate_name(
    dae: &Dae,
    structural_bindings: &HashMap<String, f64>,
    name: &VarName,
    subscripts: &[Subscript],
) -> VarName {
    let variable = dae
        .variables
        .algebraics
        .get(name)
        .or_else(|| dae.variables.outputs.get(name));
    let Some(variable) = variable else {
        return name.clone();
    };
    if variable.dims.is_empty() || subscripts.len() != variable.dims.len() {
        return name.clone();
    }
    let Some(indices) = derivative_coordinate_indices(subscripts, structural_bindings) else {
        return name.clone();
    };
    let Some(flat_index) = derivative_coordinate_flat_index(&variable.dims, &indices) else {
        return name.clone();
    };
    VarName::new(dae::scalar_name_text_for_flat_index(
        name.as_str(),
        &variable.dims,
        flat_index,
    ))
}

pub(super) fn derivative_coordinate_indices(
    subscripts: &[Subscript],
    structural_bindings: &HashMap<String, f64>,
) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| {
            let value = match subscript {
                Subscript::Index { value, .. } => return Some(*value),
                Subscript::Expr { expr, .. } => {
                    crate::static_eval::eval_static_number(expr, structural_bindings)?
                }
                Subscript::Colon { .. } => return None,
            };
            (value.is_finite()
                && value.fract() == 0.0
                && value >= i64::MIN as f64
                && value <= i64::MAX as f64)
                .then_some(value as i64)
        })
        .collect()
}

pub(super) fn derivative_coordinate_flat_index(dims: &[i64], indices: &[i64]) -> Option<usize> {
    if dims.len() != indices.len() || dims.is_empty() {
        return None;
    }
    let mut flat_index = 0usize;
    let mut stride = 1usize;
    for (&dim, &index) in dims.iter().rev().zip(indices.iter().rev()) {
        let dim = usize::try_from(dim).ok().filter(|dim| *dim > 0)?;
        let index = usize::try_from(index).ok().filter(|index| *index > 0)?;
        if index > dim {
            return None;
        }
        flat_index = flat_index.checked_add((index - 1).checked_mul(stride)?)?;
        stride = stride.checked_mul(dim)?;
    }
    Some(flat_index)
}

pub(super) fn normalize_derivative_coordinates(
    dae: &Dae,
    structural_bindings: &HashMap<String, f64>,
    expr: &Expression,
) -> Expression {
    struct CoordinateRewriter<'a> {
        dae: &'a Dae,
        structural_bindings: &'a HashMap<String, f64>,
    }

    impl CoordinateRewriter<'_> {
        fn normalized_ref(
            &self,
            name: &Reference,
            subscripts: &[Subscript],
            span: Span,
        ) -> Option<Expression> {
            if subscripts.is_empty() {
                return None;
            }
            let coordinate = derivative_coordinate_name(
                self.dae,
                self.structural_bindings,
                name.var_name(),
                subscripts,
            );
            (coordinate != *name.var_name()).then(|| Expression::VarRef {
                name: Reference::from_var_name(coordinate),
                subscripts: Vec::new(),
                span,
            })
        }

        fn normalized_index(&self, expr: &Expression) -> Option<Expression> {
            let Expression::Index {
                base,
                subscripts,
                span,
            } = expr
            else {
                return None;
            };
            let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            base_subscripts
                .is_empty()
                .then(|| self.normalized_ref(name, subscripts, *span))
                .flatten()
        }
    }

    impl ExpressionRewriter for CoordinateRewriter<'_> {
        fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
            if let Some(normalized) = self.normalized_index(expr) {
                return normalized;
            }
            if let Expression::VarRef {
                name,
                subscripts,
                span,
            } = expr
                && let Some(normalized) = self.normalized_ref(name, subscripts, *span)
            {
                return normalized;
            }
            self.walk_expression(expr)
        }
    }

    CoordinateRewriter {
        dae,
        structural_bindings,
    }
    .rewrite_expression(expr)
}

/// Collect exact scalar dependencies while excluding `if` predicates.
///
/// A predicate selects an event branch but is not differentiated. Including
/// predicate-only discrete variables would falsely block a branch-local
/// derivative certificate.
pub(super) fn derivative_coordinate_dependencies_with_bindings(
    dae: &Dae,
    structural_bindings: &HashMap<String, f64>,
    expr: &Expression,
) -> IndexSet<VarName> {
    struct Collector<'a> {
        dae: &'a Dae,
        structural_bindings: &'a HashMap<String, f64>,
        names: IndexSet<VarName>,
    }

    impl Collector<'_> {
        fn index_coordinate(&self, base: &Expression, subscripts: &[Subscript]) -> Option<VarName> {
            let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            let coordinate = derivative_coordinate_name(
                self.dae,
                self.structural_bindings,
                name.var_name(),
                subscripts,
            );
            (coordinate != *name.var_name()).then_some(coordinate)
        }

        fn visit_coordinate_subscripts(&mut self, subscripts: &[Subscript]) {
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    impl ExpressionVisitor for Collector<'_> {
        fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
            for (_, value) in branches {
                self.visit_expression(value);
            }
            self.visit_expression(else_branch);
        }

        fn visit_index(&mut self, base: &Expression, subscripts: &[Subscript]) {
            if let Some(coordinate) = self.index_coordinate(base, subscripts) {
                self.names.insert(coordinate);
                self.visit_coordinate_subscripts(subscripts);
                return;
            }
            self.visit_expression(base);
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }

        fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
            self.names.insert(derivative_coordinate_name(
                self.dae,
                self.structural_bindings,
                name.var_name(),
                subscripts,
            ));
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut collector = Collector {
        dae,
        structural_bindings,
        names: IndexSet::new(),
    };
    collector.visit_expression(expr);
    collector.names
}
