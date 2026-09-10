//! Declaration-proven array results available before equation flattening.
//! Scalar results require call-site vectorization analysis (MLS §12.4.6),
//! and input-dependent extents require argument binding; neither is inferred
//! from the declaration's provisional effective dimensions here.

use rumoca_core::{DefId, Function, Subscript};
use rustc_hash::FxHashMap;

#[derive(Default)]
pub(crate) struct FunctionResultShapes {
    by_declaration: FxHashMap<DefId, Option<Vec<i64>>>,
}

impl FunctionResultShapes {
    pub(crate) fn from_functions<'a>(functions: impl IntoIterator<Item = &'a Function>) -> Self {
        let mut result = Self::default();
        for function in functions {
            let Some(declaration) = function.def_id else {
                continue;
            };
            let shape = declared_array_result(function);
            result
                .by_declaration
                .entry(declaration)
                .and_modify(|existing| retain_common_shape(existing, &shape))
                .or_insert(shape);
        }
        result
    }

    pub(crate) fn dimensions(&self, declaration: Option<DefId>) -> Option<&[i64]> {
        self.by_declaration.get(&declaration?)?.as_deref()
    }
}

fn retain_common_shape(existing: &mut Option<Vec<i64>>, incoming: &Option<Vec<i64>>) {
    // Multiple exposures of one declaration must prove the same shape.
    // Ambiguity remains unknown in either order.
    if existing != incoming {
        *existing = None;
    }
}

fn declared_array_result(function: &Function) -> Option<Vec<i64>> {
    // MLS §12.4.3: an expression uses the first output. Automatic
    // vectorization applies only to functions with one scalar result, so a
    // declared array result retains its axes independently of actual values.
    let output = function.outputs.first()?;
    let dimensions = output.dimensions();
    if dimensions.is_empty() {
        return None;
    }
    if !output.shape_expr.is_empty()
        && (output.shape_expr.len() != dimensions.len()
            || !output.shape_expr.iter().zip(dimensions).all(|(shape, extent)| {
                matches!(shape, Subscript::Index { value, .. } if value == extent)
            }))
    {
        return None;
    }
    Some(dimensions.to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn function(name: &str, id: u32, dims: &[i64]) -> Function {
        let mut function = Function::new(name, Span::DUMMY);
        function.def_id = Some(DefId::new(id));
        function.outputs.push(
            crate::test_support::real_param("result", dims.to_vec(), Span::DUMMY).with_shape_expr(
                dims.iter()
                    .map(|extent| Subscript::Index {
                        value: *extent,
                        span: Span::DUMMY,
                    })
                    .collect(),
            ),
        );
        function
    }

    #[test]
    fn declaration_identity_separates_same_spelling_result_shapes() {
        let left = function("make", 1001, &[3]);
        let right = function("make", 1002, &[2, 3, 4]);
        let shapes = FunctionResultShapes::from_functions([&left, &right]);
        assert_eq!(shapes.dimensions(left.def_id), Some([3].as_slice()));
        assert_eq!(shapes.dimensions(right.def_id), Some([2, 3, 4].as_slice()));
        assert_eq!(shapes.dimensions(None), None);
        assert_eq!(shapes.dimensions(Some(DefId::new(1003))), None);
    }

    #[test]
    fn contradictory_exposures_stay_unknown_in_either_order() {
        let left = function("Left.make", 1001, &[3]);
        let right = function("Right.make", 1001, &[4]);
        for functions in [[&left, &right, &left], [&right, &left, &right]] {
            assert_eq!(
                FunctionResultShapes::from_functions(functions).dimensions(left.def_id),
                None
            );
        }
    }

    #[test]
    fn scalar_results_do_not_prejudge_vectorization() {
        let scalar = function("scalar", 1001, &[]);
        assert_eq!(
            FunctionResultShapes::from_functions([&scalar]).dimensions(scalar.def_id),
            None
        );
    }

    #[test]
    fn deferred_shape_does_not_use_provisional_effective_dimensions() {
        let mut deferred = function("sizedByInput", 1001, &[3]);
        deferred.outputs[0].shape_expr = vec![Subscript::Colon { span: Span::DUMMY }];
        let exact = function("Exact.make", 1001, &[3]);
        for functions in [[&deferred, &exact], [&exact, &deferred]] {
            assert_eq!(
                FunctionResultShapes::from_functions(functions).dimensions(deferred.def_id),
                None
            );
        }
    }
}
