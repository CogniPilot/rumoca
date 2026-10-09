//! Nested record packed lanes select depth-first typed leaves.
use super::*;
use rumoca_core::{SourceMap, Span, VarName};

fn record_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("record_layout.mo", "record Bundle end Bundle;");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 25)).unwrap();
    dae::Dae::construct(sources, |model| {
        let bundle = model.types(|types| {
            let real2 = types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?;
            let integer = types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?;
            let boolean = types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?;
            let text = types.derived(dae::ValueType::scalar(dae::ScalarType::String), at)?;
            let empty = types.derived(dae::ValueType::array(dae::ScalarType::Real, [0]), at)?;
            let fields = || {
                [
                    (VarName::new("a"), real2),
                    (VarName::new("k"), integer),
                    (VarName::new("b"), boolean),
                    (VarName::new("label"), text),
                    (VarName::new("empty"), empty),
                ]
            };
            let cell = types.record(VarName::new("Cell"), fields(), at)?;
            let cells = types.record_array(VarName::new("Cell"), fields(), [2, 3], at)?;
            let matrix = types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?;
            let nests = types.record_array(
                VarName::new("Nest"),
                [
                    (VarName::new("cells"), cells),
                    (VarName::new("matrix"), matrix),
                ],
                [2],
                at,
            )?;
            let prefix = types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?;
            let bundle = types.record(
                VarName::new("Bundle"),
                [
                    (VarName::new("prefix"), prefix),
                    (VarName::new("nested"), nests),
                    (VarName::new("tail"), cell),
                ],
                at,
            )?;
            Ok(bundle)
        })?;
        for name in ["first", "second"] {
            model.function(
                dae::FunctionSignature::new(VarName::new(name), [bundle], [bundle], at),
                |model, reservation| {
                    let input =
                        model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                    let output =
                        model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                    let input = model.expressions(|e| e.at(at).function_parameter(input))?;
                    let mut body = model.functions(|f| f.begin(reservation, at))?;
                    model.functions(|f| f.assign(&mut body, output, input, at))?;
                    model.functions(|f| f.define(body, at))
                },
            )?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn nested_record_scalar_projections_follow_packed_lane_oracle() {
    let model = record_model();
    model.inspect(|view| {
        let record = (0..view.value_type_count())
            .map(|i| view.value_type_id(i).unwrap())
            .find(|id| {
                view.value_type(*id)
                    .unwrap()
                    .record_name()
                    .is_some_and(|n| n.as_str() == "Bundle")
            })
            .unwrap();
        let leaves = lower_value_type_leaves(view, record, arithmetic_profile()).unwrap();
        assert_eq!(
            leaves
                .iter()
                .map(|ty| ty.scalar_count())
                .collect::<Vec<_>>(),
            [3, 24, 12, 12, 8, 2, 1, 1]
        );
        assert_eq!(leaves[1].dimensions(), [2, 2, 3, 2]);
        assert_eq!(
            leaves[2].element_type(),
            solve::SolveScalarType::integer(arithmetic_profile())
        );
        assert_eq!(leaves[3].element_type(), solve::SolveScalarType::Boolean);
        let layout = lower_value_type_layout(view, record, arithmetic_profile(), true).unwrap();
        assert_eq!(layout.leaves, leaves);
        let mut queries = 0;
        let mut check = |field, scalar, expected| {
            assert_eq!(
                layout.projection.field_scalar(field, scalar).unwrap(),
                expected
            );
            queries += 1;
        };
        // The oracle uses declared element-major records, including omitted
        // text lanes. It does not call a leaf or field-layout helper.
        for _ in 0..32 {
            for scalar in 0..3 {
                check(0, scalar, (0, scalar));
            }
            for outer in 0..2 {
                check_nested_record_element(outer, &mut check);
            }
            for scalar in 0..2 {
                check(2, scalar, (5, scalar));
            }
            check(2, 2, (6, 0));
            check(2, 3, (7, 0));
        }
        assert_eq!(queries, 2016);
        // Omitted text and empty fields do not acquire a neighboring leaf.
        assert_eq!(layout.projection.field_scalar(2, 4), None);
        assert_eq!(layout.projection.field_scalar(3, 0), None);
    });
}

fn check_nested_record_element(outer: usize, check: &mut impl FnMut(usize, usize, (usize, usize))) {
    for row in 0..2 {
        for column in 0..3 {
            let cell = row * 3 + column;
            for component in 0..2 {
                check(
                    1,
                    outer * 34 + cell * 5 + component,
                    (1, (outer * 6 + cell) * 2 + component),
                );
            }
            check(1, outer * 34 + cell * 5 + 2, (2, outer * 6 + cell));
            check(1, outer * 34 + cell * 5 + 3, (3, outer * 6 + cell));
        }
    }
    for scalar in 0..4 {
        check(1, outer * 34 + 30 + scalar, (4, outer * 4 + scalar));
    }
}

#[test]
fn unavailable_packing_is_refused_only_when_a_packed_projection_demands_it() {
    let child = Projection::Record(vec![ProjectionField {
        leaves: 0..1,
        packing: None,
        projection: Projection::Leaf,
    }]);
    // A direct primitive field needs its typed leaf, not a packed layout.
    assert_eq!(child.field_scalar(0, 0), Some((0, 0)));
    let parent = Projection::Record(vec![ProjectionField {
        leaves: 0..1,
        packing: None,
        projection: child,
    }]);
    // A packed record lane demands the child's absent canonical layout.
    assert_eq!(parent.field_scalar(0, 0), None);
}

#[test]
fn result_layout_binds_exact_function_and_checks_scalar_boundaries() {
    let model = record_model();
    model.inspect(|view| {
        let first = view.function_id(0).unwrap();
        let second = view.function_id(1).unwrap();
        let layout =
            FunctionResultsLayout::lower(view, view.function(first).unwrap(), arithmetic_profile())
                .unwrap();
        assert_eq!(layout.scalar_range(0), Some(0..63));
        assert_eq!(layout.scalar_range(1), None);
        assert_eq!(layout.record_scalar(first, 0, 0, 2), Some(2));
        assert_eq!(layout.record_scalar(first, 0, 0, 3), None);
        assert_eq!(layout.record_scalar(first, 0, 1, 67), Some(58));
        assert_eq!(layout.record_scalar(first, 0, 2, 3), Some(62));
        assert_eq!(layout.record_scalar(first, 0, 2, 4), None);
        assert_eq!(layout.record_scalar(second, 0, 0, 0), None);
    });
}
