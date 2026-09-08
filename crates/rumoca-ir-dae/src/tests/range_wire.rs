use super::*;

#[test]
fn ranges_retain_bound_occurrences_and_explicit_step_syntax() {
    let (dae, omitted_at, explicit_at, generated_at) = range_fixture();
    dae.inspect(|view| {
        let ranges = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| expression.kind() == ExpressionKind::Range)
            .collect::<Vec<_>>();
        assert_eq!(ranges.len(), 3);

        let ExpressionOperation::Range(omitted) = ranges[0].operation() else {
            unreachable!("selected expression is a range")
        };
        assert_eq!(omitted.start().value(), 1);
        assert_eq!(omitted.stop().value(), 1);
        assert_eq!(omitted.explicit_step().map(|step| step.value()), None);
        assert_eq!(omitted.effective_step(), 1);
        assert_eq!(omitted.start().provenance(), omitted_at.0);
        assert_eq!(omitted.stop().provenance(), omitted_at.1);
        assert_eq!(ranges[0].provenance(), omitted_at.2);
        assert_ne!(
            omitted.start().expression(),
            omitted.stop().expression(),
            "equal-valued bounds remain distinct source occurrences"
        );

        let ExpressionOperation::Range(explicit) = ranges[1].operation() else {
            unreachable!("selected expression is a range")
        };
        let step = explicit.explicit_step().expect("source wrote a step");
        assert_eq!(step.value(), 1);
        assert_eq!(explicit.effective_step(), 1);
        assert_eq!(explicit.start().provenance(), explicit_at.0);
        assert_eq!(step.provenance(), explicit_at.1);
        assert_eq!(explicit.stop().provenance(), explicit_at.2);
        assert_eq!(ranges[1].provenance(), explicit_at.3);

        let ExpressionOperation::Range(generated) = ranges[2].operation() else {
            unreachable!("selected expression is a range")
        };
        let generated_step = generated
            .explicit_step()
            .expect("the generated range retains its explicit step");
        assert_eq!(generated_step.value(), 2);
        for provenance in [
            generated.start().provenance(),
            generated_step.provenance(),
            generated.stop().provenance(),
            ranges[2].provenance(),
        ] {
            assert_eq!(
                provenance.origin(),
                DaeProvenanceOrigin::Generated(DaeGeneration::IndexReduction)
            );
            assert_eq!(provenance.span(), generated_at.span());
        }
    });
}

#[test]
fn range_wire_round_trips_and_rejects_noncanonical_shapes() {
    let (dae, _, _, _) = range_fixture();
    let json = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&json).expect("range JSON replays construction");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);

    let binary = bincode::serialize(&dae).expect("range bincode serializes");
    let decoded: Dae = bincode::deserialize(&binary).expect("range bincode replays construction");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);

    let canonical: serde_json::Value = serde_json::from_str(&json).unwrap();
    let omitted = range_node(&canonical, 0);
    assert_eq!(omitted["start_expression"], 0);
    assert!(omitted["explicit_step_expression"].is_null());
    assert_eq!(omitted["stop_expression"], 1);
    for removed in ["start", "step", "stop"] {
        assert!(omitted.get(removed).is_none());
    }

    let mut missing_step_field = canonical.clone();
    range_node_mut(&mut missing_step_field, 0)
        .as_object_mut()
        .unwrap()
        .remove("explicit_step_expression");
    assert!(
        serde_json::from_value::<Dae>(missing_step_field).is_err(),
        "an omitted step is encoded as required explicit null"
    );

    let mut removed_raw_shape = canonical.clone();
    *range_node_mut(&mut removed_raw_shape, 0) = serde_json::json!({
        "start": 1,
        "step": 1,
        "stop": 3,
    });
    assert!(
        serde_json::from_value::<Dae>(removed_raw_shape).is_err(),
        "wire-v12 rejects the removed raw-value range shape"
    );

    let mut future_bound = canonical;
    range_node_mut(&mut future_bound, 0)["start_expression"] = 2.into();
    assert!(
        serde_json::from_value::<Dae>(future_bound).is_err(),
        "wire replay rejects a range bound that is not already constructed"
    );

    let mut non_integer_bound: serde_json::Value = serde_json::from_str(&json).unwrap();
    range_node_mut(&mut non_integer_bound, 1)["start_expression"] = 2.into();
    assert!(
        serde_json::from_value::<Dae>(non_integer_bound).is_err(),
        "wire replay rejects an already-mapped expression that is not an Integer literal"
    );
}

#[test]
fn range_construction_reports_the_exact_bad_bound_occurrence() {
    let source = TestSource::new("1:0:3; 1.0:3;");
    let zero_at = source.source("0", 0);
    let zero_range_at = source.source("1:0:3", 0);
    let real_at = source.source("1.0", 0);
    let invalid_range_at = source.source("1.0:3", 0);
    let one_at = source.source("1", 0);
    let first_stop_at = source.source("3", 0);
    let second_stop_at = source.source("3", 1);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(one_at).literal(DaeLiteral::Integer(1))?;
            let zero = expressions.at(zero_at).literal(DaeLiteral::Integer(0))?;
            let first_stop = expressions
                .at(first_stop_at)
                .literal(DaeLiteral::Integer(3))?;
            let error = expressions
                .at(zero_range_at)
                .range(one, Some(zero), first_stop)
                .expect_err("a zero step is rejected before parent insertion");
            assert_eq!(
                error,
                DaeConstructionError::ZeroRangeStep {
                    span: zero_at.span()
                }
            );

            let real = expressions.at(real_at).literal(DaeLiteral::Real(1.0))?;
            let second_stop = expressions
                .at(second_stop_at)
                .literal(DaeLiteral::Integer(3))?;
            let error = expressions
                .at(invalid_range_at)
                .range(real, None, second_stop)
                .expect_err("a range cannot accept a merely numeric expression");
            assert_eq!(
                error,
                DaeConstructionError::InvalidRangeBound {
                    span: real_at.span()
                }
            );
            Ok(())
        })
    })
    .expect("rejected ranges leave their checked child occurrences available");
    dae.inspect(|view| assert_eq!(view.expression_count(), 5));
}

#[test]
fn nonempty_range_slices_reject_coordinates_outside_the_base_axis() {
    for (start, step, stop) in [
        (0, None, 1),
        (2, None, 4),
        (4, Some(-1), 1),
        (3, Some(-1), 0),
    ] {
        assert!(matches!(
            range_slice_fixture(start, step, stop, RangeSubscriptForm::Slice),
            Err(DaeConstructionError::InvalidSubscript { .. })
        ));
    }
}

#[test]
fn range_slice_endpoint_proof_handles_extreme_arithmetic_without_saturation() {
    assert!(matches!(
        range_slice_fixture(
            -9_000_000_000_000_000_000,
            Some(6_000_000_000_000_000_000),
            9_000_000_000_000_000_000,
            RangeSubscriptForm::Slice,
        ),
        Err(DaeConstructionError::InvalidSubscript { .. })
    ));
}

#[test]
fn valid_range_slice_edges_and_descending_order_construct() {
    let ascending =
        range_slice_fixture(1, None, 3, RangeSubscriptForm::Slice).expect("1:3 is in bounds");
    assert_index_extent(&ascending, 3);

    let descending = range_slice_fixture(3, Some(-1), 1, RangeSubscriptForm::Slice)
        .expect("3:-1:1 is in bounds");
    assert_index_extent(&descending, 3);

    let nonaligned_stop = range_slice_fixture(1, Some(2), 4, RangeSubscriptForm::Slice)
        .expect("1:2:4 selects only the in-bounds coordinates 1 and 3");
    assert_index_extent(&nonaligned_stop, 2);
}

#[test]
fn empty_range_slice_performs_no_index_access() {
    let empty = range_slice_fixture(4, None, 3, RangeSubscriptForm::Slice)
        .expect("an empty slice does not access its out-of-bounds endpoints");
    assert_index_extent(&empty, 0);

    let extreme_empty = range_slice_fixture(i64::MAX, None, i64::MIN, RangeSubscriptForm::Slice)
        .expect("an empty range needs no endpoint arithmetic");
    assert_index_extent(&extreme_empty, 0);
}

#[test]
fn array_valued_value_subscript_cannot_bypass_range_bounds() {
    assert!(matches!(
        range_slice_fixture(0, None, 1, RangeSubscriptForm::Value),
        Err(DaeConstructionError::InvalidSubscript { .. })
    ));
    let valid = range_slice_fixture(1, None, 3, RangeSubscriptForm::Value)
        .expect("an array-valued Value subscript uses the same slice proof");
    assert_index_extent(&valid, 3);
}

#[test]
fn literal_array_slice_rejects_every_statically_out_of_bounds_coordinate() {
    for indices in [[0, 2], [1, 4]] {
        for form in [RangeSubscriptForm::Slice, RangeSubscriptForm::Value] {
            assert!(matches!(
                literal_array_slice_fixture(indices, form),
                Err(DaeConstructionError::InvalidSubscript { .. })
            ));
        }
    }

    let valid = literal_array_slice_fixture([1, 3], RangeSubscriptForm::Slice)
        .expect("the literal selector contains only valid edge coordinates");
    assert_index_extent(&valid, 2);
}

#[test]
fn scalar_integer_subscripts_reject_constructor_known_out_of_bounds_coordinates() {
    for coordinate in [0, 4] {
        for form in [ScalarSubscriptForm::Index, ScalarSubscriptForm::Value] {
            let (result, subscript_span) = scalar_integer_subscript_fixture(coordinate, 3, form);
            let error = result.expect_err("the constructor-known coordinate is out of bounds");
            let DaeConstructionError::InvalidSubscript { span } = error else {
                panic!("expected InvalidSubscript, found {error:?}");
            };
            assert_eq!(
                span, subscript_span,
                "the exact subscript occurrence owns the error"
            );
        }
    }

    for (coordinate, form) in [
        (1, ScalarSubscriptForm::Index),
        (3, ScalarSubscriptForm::Value),
    ] {
        scalar_integer_subscript_fixture(coordinate, 3, form)
            .0
            .expect("valid edge coordinate constructs");
    }
}

#[test]
fn scalar_integer_subscript_rejects_every_coordinate_of_a_zero_extent_axis() {
    for form in [ScalarSubscriptForm::Index, ScalarSubscriptForm::Value] {
        assert!(matches!(
            scalar_integer_subscript_fixture(1, 0, form).0,
            Err(DaeConstructionError::InvalidSubscript { .. })
        ));
    }
}

#[test]
fn parameter_bound_scalar_integer_subscripts_use_the_same_constructor_proof() {
    for binding in [0, 4] {
        for form in [ScalarSubscriptForm::Index, ScalarSubscriptForm::Value] {
            let (result, subscript_span) = parameter_scalar_subscript_fixture(binding, form);
            let error = result.expect_err("the parameter binding is statically out of bounds");
            let DaeConstructionError::InvalidSubscript { span } = error else {
                panic!("expected InvalidSubscript, found {error:?}");
            };
            assert_eq!(span, subscript_span);
        }
    }

    parameter_scalar_subscript_fixture(3, ScalarSubscriptForm::Value)
        .0
        .expect("an in-bounds parameter binding constructs");
}

#[test]
fn forward_parameter_subscript_obligations_reject_out_of_bounds_bindings() {
    for form in DeferredSubscriptForm::ALL {
        deferred_parameter_subscript_fixture(ForwardBinding::Direct(3), form)
            .0
            .expect("an in-bounds forward binding discharges its obligation");
        for coordinate in [0, 4] {
            let (result, subscript_span) =
                deferred_parameter_subscript_fixture(ForwardBinding::Direct(coordinate), form);
            let error = result.expect_err("the deferred binding is out of bounds");
            let DaeConstructionError::InvalidSubscript { span } = error else {
                panic!("expected InvalidSubscript, found {error:?}");
            };
            assert_eq!(span, subscript_span);
        }
    }
}

#[test]
fn chained_forward_parameter_bindings_discharge_the_original_subscript_obligation() {
    for form in DeferredSubscriptForm::ALL {
        deferred_parameter_subscript_fixture(ForwardBinding::Chain(1), form)
            .0
            .expect("a valid parameter chain resolves once at construction completion");
        let (result, subscript_span) =
            deferred_parameter_subscript_fixture(ForwardBinding::Chain(4), form);
        let error = result.expect_err("the terminal parameter binding is out of bounds");
        let DaeConstructionError::InvalidSubscript { span } = error else {
            panic!("expected InvalidSubscript, found {error:?}");
        };
        assert_eq!(span, subscript_span);
    }
}

#[test]
fn defined_unbound_forward_parameter_subscripts_remain_dynamic_and_round_trip() {
    for form in DeferredSubscriptForm::ALL {
        let dae = deferred_parameter_subscript_fixture(ForwardBinding::Unbound, form)
            .0
            .expect("a defined parameter without a binding remains dynamic");
        let wire = serde_json::to_value(&dae).unwrap();
        assert!(
            wire["storage"]
                .get("pending_integer_subscript_bounds")
                .is_none()
        );
        serde_json::from_value::<Dae>(wire)
            .expect("wire replay discharges the temporary obligation as dynamic");
    }
}

#[test]
fn dynamic_scalar_integer_subscripts_remain_explicit_in_dae() {
    for form in [ScalarSubscriptForm::Index, ScalarSubscriptForm::Value] {
        let dae = dynamic_scalar_subscript_fixture(form)
            .expect("a dynamic scalar subscript remains target-visible");
        assert_scalar_index(&dae);
    }
}

#[test]
fn dynamic_array_slice_remains_explicit_in_dae() {
    let source = TestSource::new("Real x[3]; input Integer selector[2]; x[selector]");
    let at = source.source("x[selector]", 0);
    let dae = Dae::construct(source.map, |dae| {
        let selector_type =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Integer, [2]), at))?;
        let selector = dae.variables(|variables| {
            variables.input(
                VarName::new("selector"),
                rumoca_core::InstanceId::new(1),
                selector_type,
                InputVariability::Discrete,
                at,
                VariableAttributes::default(),
            )
        })?;
        dae.expressions(|expressions| {
            let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
            let base = expressions.at(at).array([zero, zero, zero])?;
            let selector = expressions
                .at(at)
                .coordinate(CoordinateInput::Input(selector))?;
            expressions.at(at).index(
                base,
                [Subscript::Value {
                    expression: selector,
                    provenance: at,
                }],
            )?;
            Ok(())
        })
    })
    .expect("DAE preserves the typed dynamic slice for target-specific bounds handling");
    assert_index_extent(&dae, 2);
}

#[test]
fn wire_replay_cannot_bypass_constant_range_slice_bounds() {
    let dae =
        range_slice_fixture(1, None, 3, RangeSubscriptForm::Slice).expect("canonical fixture");
    let canonical = serde_json::to_value(&dae).unwrap();

    let mut below = canonical.clone();
    *range_bound_literal_mut(&mut below, "start_expression") = 0.into();
    let error = serde_json::from_value::<Dae>(below).unwrap_err();
    assert!(error.to_string().contains("invalid array subscript"));

    let mut above = canonical;
    *range_bound_literal_mut(&mut above, "stop_expression") = 4.into();
    let error = serde_json::from_value::<Dae>(above).unwrap_err();
    assert!(error.to_string().contains("invalid array subscript"));

    let literal =
        literal_array_slice_fixture([1, 3], RangeSubscriptForm::Slice).expect("canonical fixture");
    let mut literal_wire = serde_json::to_value(literal).unwrap();
    let selector_element = literal_array_selector_element(&literal_wire, 1);
    literal_wire["storage"]["expressions"]["nodes"][selector_element]["literal"]["integer"] =
        4.into();
    let error = serde_json::from_value::<Dae>(literal_wire).unwrap_err();
    assert!(error.to_string().contains("invalid array subscript"));
}

#[test]
fn wire_replay_cannot_bypass_scalar_integer_bounds() {
    let dae = scalar_integer_subscript_fixture(3, 3, ScalarSubscriptForm::Index)
        .0
        .expect("canonical fixture");
    let canonical = serde_json::to_value(dae).unwrap();
    for coordinate in [0, 4] {
        let mut mutated = canonical.clone();
        *first_integer_literal_mut(&mut mutated) = coordinate.into();
        let error = serde_json::from_value::<Dae>(mutated).unwrap_err();
        assert!(error.to_string().contains("invalid array subscript"));
    }
}

#[test]
fn malicious_wire_parameter_bindings_cannot_bypass_deferred_subscript_bounds() {
    for form in DeferredSubscriptForm::ALL {
        let dae = deferred_parameter_subscript_fixture(ForwardBinding::Direct(3), form)
            .0
            .expect("canonical fixture");
        let canonical = serde_json::to_value(dae).unwrap();
        for coordinate in [0, 4] {
            let mut mutated = canonical.clone();
            *parameter_binding_integer_mut(&mut mutated, 0) = coordinate.into();
            let error = serde_json::from_value::<Dae>(mutated).unwrap_err();
            assert!(error.to_string().contains("invalid array subscript"));
        }

        let chained = deferred_parameter_subscript_fixture(ForwardBinding::Chain(3), form)
            .0
            .expect("canonical chained fixture");
        let mut chained_wire = serde_json::to_value(chained).unwrap();
        *parameter_binding_integer_mut(&mut chained_wire, 1) = 4.into();
        let error = serde_json::from_value::<Dae>(chained_wire).unwrap_err();
        assert!(error.to_string().contains("invalid array subscript"));
    }
}

#[derive(Clone, Copy)]
enum RangeSubscriptForm {
    Slice,
    Value,
}

#[derive(Clone, Copy)]
enum ScalarSubscriptForm {
    Index,
    Value,
}

#[derive(Clone, Copy)]
enum DeferredSubscriptForm {
    ScalarIndex,
    ScalarValue,
    ArraySlice,
    ArrayValue,
}

impl DeferredSubscriptForm {
    const ALL: [Self; 4] = [
        Self::ScalarIndex,
        Self::ScalarValue,
        Self::ArraySlice,
        Self::ArrayValue,
    ];
}

#[derive(Clone, Copy)]
enum ForwardBinding {
    Direct(i64),
    Chain(i64),
    Unbound,
}

fn range_slice_fixture(
    start: i64,
    step: Option<i64>,
    stop: i64,
    form: RangeSubscriptForm,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("Real x[3]; x[range]");
    let at = source.source("x[range]", 0);
    Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
            let base = expressions.at(at).array([zero, zero, zero])?;
            let start = expressions.at(at).literal(DaeLiteral::Integer(start))?;
            let step = step
                .map(|step| expressions.at(at).literal(DaeLiteral::Integer(step)))
                .transpose()?;
            let stop = expressions.at(at).literal(DaeLiteral::Integer(stop))?;
            let range = expressions.at(at).range(start, step, stop)?;
            let subscript = match form {
                RangeSubscriptForm::Slice => Subscript::Slice {
                    expression: range,
                    provenance: at,
                },
                RangeSubscriptForm::Value => Subscript::Value {
                    expression: range,
                    provenance: at,
                },
            };
            expressions.at(at).index(base, [subscript])?;
            Ok(())
        })
    })
}

fn literal_array_slice_fixture(
    indices: [i64; 2],
    form: RangeSubscriptForm,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("Real x[3]; x[{i, j}]");
    let at = source.source("x[{i, j}]", 0);
    Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
            let base = expressions.at(at).array([zero, zero, zero])?;
            let first = expressions
                .at(at)
                .literal(DaeLiteral::Integer(indices[0]))?;
            let second = expressions
                .at(at)
                .literal(DaeLiteral::Integer(indices[1]))?;
            let selector = expressions.at(at).array([first, second])?;
            let subscript = match form {
                RangeSubscriptForm::Slice => Subscript::Slice {
                    expression: selector,
                    provenance: at,
                },
                RangeSubscriptForm::Value => Subscript::Value {
                    expression: selector,
                    provenance: at,
                },
            };
            expressions.at(at).index(base, [subscript])?;
            Ok(())
        })
    })
}

fn scalar_integer_subscript_fixture(
    coordinate: i64,
    axis_extent: u32,
    form: ScalarSubscriptForm,
) -> (Result<Dae, DaeConstructionError>, Span) {
    let source = TestSource::new("Real x[extent]; Integer index; x[index]");
    let base_at = source.source("x[extent]", 0);
    let coordinate_at = source.source("Integer index", 0);
    let subscript_at = source.source("x[index]", 0);
    let subscript_span = subscript_at.span();
    let result = Dae::construct(source.map, |dae| {
        let base_type = dae.types(|types| {
            types.derived(ValueType::array(ScalarType::Real, [axis_extent]), base_at)
        })?;
        dae.expressions(|expressions| {
            let base = if axis_extent == 0 {
                expressions.at(base_at).empty_array(base_type)?
            } else {
                let zero = expressions.at(base_at).literal(DaeLiteral::Real(0.0))?;
                expressions
                    .at(base_at)
                    .array(std::iter::repeat_n(zero, axis_extent as usize))?
            };
            let coordinate = expressions
                .at(coordinate_at)
                .literal(DaeLiteral::Integer(coordinate))?;
            let subscript = match form {
                ScalarSubscriptForm::Index => Subscript::Index {
                    expression: coordinate,
                    provenance: subscript_at,
                },
                ScalarSubscriptForm::Value => Subscript::Value {
                    expression: coordinate,
                    provenance: subscript_at,
                },
            };
            expressions.at(subscript_at).index(base, [subscript])?;
            Ok(())
        })
    });
    (result, subscript_span)
}

fn parameter_scalar_subscript_fixture(
    binding: i64,
    form: ScalarSubscriptForm,
) -> (Result<Dae, DaeConstructionError>, Span) {
    let source = TestSource::new("parameter Integer selector; Real x[3]; x[selector]");
    let parameter_at = source.source("parameter Integer selector", 0);
    let base_at = source.source("x[3]", 0);
    let subscript_at = source.source("x[selector]", 0);
    let subscript_span = subscript_at.span();
    let result = Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), parameter_at))?;
        let binding = dae.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(DaeLiteral::Integer(binding))
        })?;
        let parameter = dae.variables(|variables| {
            variables.parameter(
                VarName::new("selector"),
                rumoca_core::InstanceId::new(1),
                integer,
                parameter_at,
                VariableAttributes {
                    binding: Some(binding),
                    ..VariableAttributes::default()
                },
            )
        })?;
        dae.expressions(|expressions| {
            let zero = expressions.at(base_at).literal(DaeLiteral::Real(0.0))?;
            let base = expressions.at(base_at).array([zero, zero, zero])?;
            let coordinate = expressions
                .at(subscript_at)
                .coordinate(CoordinateInput::Parameter(parameter))?;
            let subscript = match form {
                ScalarSubscriptForm::Index => Subscript::Index {
                    expression: coordinate,
                    provenance: subscript_at,
                },
                ScalarSubscriptForm::Value => Subscript::Value {
                    expression: coordinate,
                    provenance: subscript_at,
                },
            };
            expressions.at(subscript_at).index(base, [subscript])?;
            Ok(())
        })
    });
    (result, subscript_span)
}

fn dynamic_scalar_subscript_fixture(
    form: ScalarSubscriptForm,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("Real x[3]; input Integer selector; x[selector]");
    let at = source.source("x[selector]", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let selector = dae.variables(|variables| {
            variables.input(
                VarName::new("selector"),
                rumoca_core::InstanceId::new(1),
                integer,
                InputVariability::Discrete,
                at,
                VariableAttributes::default(),
            )
        })?;
        dae.expressions(|expressions| {
            let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
            let base = expressions.at(at).array([zero, zero, zero])?;
            let coordinate = expressions
                .at(at)
                .coordinate(CoordinateInput::Input(selector))?;
            let subscript = match form {
                ScalarSubscriptForm::Index => Subscript::Index {
                    expression: coordinate,
                    provenance: at,
                },
                ScalarSubscriptForm::Value => Subscript::Value {
                    expression: coordinate,
                    provenance: at,
                },
            };
            expressions.at(at).index(base, [subscript])?;
            Ok(())
        })
    })
}

fn deferred_parameter_subscript_fixture(
    binding: ForwardBinding,
    form: DeferredSubscriptForm,
) -> (Result<Dae, DaeConstructionError>, Span) {
    let source =
        TestSource::new("parameter Integer p; parameter Integer q; Real x[3]; x[selector]");
    let primary_at = source.source("parameter Integer p", 0);
    let secondary_at = source.source("parameter Integer q", 0);
    let base_at = source.source("x[3]", 0);
    let subscript_at = source.source("x[selector]", 0);
    let subscript_span = subscript_at.span();
    let result = Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), primary_at))?;
        let (primary, primary_reservation) = dae.variables(|variables| {
            variables.reserve_parameter(
                VarName::new("p"),
                rumoca_core::InstanceId::new(1),
                integer,
                primary_at,
            )
        })?;
        let secondary = matches!(binding, ForwardBinding::Chain(_))
            .then(|| {
                dae.variables(|variables| {
                    variables.reserve_parameter(
                        VarName::new("q"),
                        rumoca_core::InstanceId::new(2),
                        integer,
                        secondary_at,
                    )
                })
            })
            .transpose()?;

        issue_deferred_parameter_subscript(dae, primary, form, base_at, subscript_at)?;

        match binding {
            ForwardBinding::Direct(value) => {
                let value = dae.expressions(|expressions| {
                    expressions
                        .at(primary_at)
                        .literal(DaeLiteral::Integer(value))
                })?;
                dae.variables(|variables| {
                    variables.define(
                        primary_reservation,
                        VariableAttributes {
                            binding: Some(value),
                            ..VariableAttributes::default()
                        },
                        primary_at,
                    )
                })
            }
            ForwardBinding::Chain(value) => {
                let (secondary, secondary_reservation) =
                    secondary.expect("the chain reserved its terminal parameter");
                let (secondary_use, value) = dae.expressions(|expressions| {
                    let secondary_use = expressions
                        .at(primary_at)
                        .coordinate(CoordinateInput::Parameter(secondary))?;
                    let value = expressions
                        .at(secondary_at)
                        .literal(DaeLiteral::Integer(value))?;
                    Ok((secondary_use, value))
                })?;
                dae.variables(|variables| {
                    variables.define(
                        primary_reservation,
                        VariableAttributes {
                            binding: Some(secondary_use),
                            ..VariableAttributes::default()
                        },
                        primary_at,
                    )?;
                    variables.define(
                        secondary_reservation,
                        VariableAttributes {
                            binding: Some(value),
                            ..VariableAttributes::default()
                        },
                        secondary_at,
                    )
                })
            }
            ForwardBinding::Unbound => dae.variables(|variables| {
                variables.define(
                    primary_reservation,
                    VariableAttributes::default(),
                    primary_at,
                )
            }),
        }
    });
    (result, subscript_span)
}

fn issue_deferred_parameter_subscript<'dae>(
    dae: &mut DaeConstruction<'dae>,
    primary: ParameterId<'dae>,
    form: DeferredSubscriptForm,
    base_at: DaeProvenance,
    subscript_at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    dae.expressions(|expressions| {
        let zero = expressions.at(base_at).literal(DaeLiteral::Real(0.0))?;
        let base = expressions.at(base_at).array([zero, zero, zero])?;
        let primary = expressions
            .at(subscript_at)
            .coordinate(CoordinateInput::Parameter(primary))?;
        let selector = match form {
            DeferredSubscriptForm::ScalarIndex | DeferredSubscriptForm::ScalarValue => primary,
            DeferredSubscriptForm::ArraySlice | DeferredSubscriptForm::ArrayValue => {
                let two = expressions
                    .at(subscript_at)
                    .literal(DaeLiteral::Integer(2))?;
                expressions.at(subscript_at).array([primary, two])?
            }
        };
        let subscript = match form {
            DeferredSubscriptForm::ScalarIndex => Subscript::Index {
                expression: selector,
                provenance: subscript_at,
            },
            DeferredSubscriptForm::ScalarValue | DeferredSubscriptForm::ArrayValue => {
                Subscript::Value {
                    expression: selector,
                    provenance: subscript_at,
                }
            }
            DeferredSubscriptForm::ArraySlice => Subscript::Slice {
                expression: selector,
                provenance: subscript_at,
            },
        };
        expressions.at(subscript_at).index(base, [subscript])?;
        Ok(())
    })
}

fn assert_index_extent(dae: &Dae, expected: u32) {
    dae.inspect(|view| {
        let index = (0..view.expression_count())
            .filter_map(|ordinal| view.expression_id(ordinal))
            .filter_map(|expression| view.expression(expression))
            .find(|expression| expression.kind() == ExpressionKind::Index)
            .expect("fixture contains its constructed index");
        assert_eq!(index.value_type().dimensions(), [expected]);
    });
}

fn assert_scalar_index(dae: &Dae) {
    dae.inspect(|view| {
        let index = (0..view.expression_count())
            .filter_map(|ordinal| view.expression_id(ordinal))
            .filter_map(|expression| view.expression(expression))
            .find(|expression| expression.kind() == ExpressionKind::Index)
            .expect("fixture contains its constructed index");
        assert!(index.value_type().is_scalar());
    });
}

fn range_bound_literal_mut<'wire>(
    wire: &'wire mut serde_json::Value,
    bound: &str,
) -> &'wire mut serde_json::Value {
    let expression = range_node(wire, 0)[bound].as_u64().unwrap() as usize;
    &mut wire["storage"]["expressions"]["nodes"][expression]["literal"]["integer"]
}

fn first_integer_literal_mut(wire: &mut serde_json::Value) -> &mut serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("literal")?.get_mut("integer"))
        .expect("fixture contains its scalar Integer literal")
}

fn parameter_binding_integer_mut(
    wire: &mut serde_json::Value,
    variable: usize,
) -> &mut serde_json::Value {
    let expression = wire["storage"]["variables"][variable]["attributes"]["binding"]
        .as_u64()
        .expect("fixture parameter owns a binding") as usize;
    &mut wire["storage"]["expressions"]["nodes"][expression]["literal"]["integer"]
}

fn literal_array_selector_element(wire: &serde_json::Value, ordinal: usize) -> usize {
    let arrays = wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .enumerate()
        .filter(|(_, node)| node.get("array").is_some())
        .collect::<Vec<_>>();
    let (selector, _) = arrays
        .get(1)
        .expect("fixture's second array node is the selector");
    let operand_start = wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .take(*selector)
        .filter_map(|node| node.get("array"))
        .map(|array| array["operand_count"].as_u64().unwrap() as usize)
        .sum::<usize>();
    wire["storage"]["expressions"]["operands"][operand_start + ordinal]
        .as_u64()
        .unwrap() as usize
}

type OmittedProvenance = (DaeProvenance, DaeProvenance, DaeProvenance);
type ExplicitProvenance = (DaeProvenance, DaeProvenance, DaeProvenance, DaeProvenance);

fn range_fixture() -> (Dae, OmittedProvenance, ExplicitProvenance, DaeProvenance) {
    let source = TestSource::new("1:1; 4:1:6; generated 7:2:9;");
    let omitted = (
        source.source("1", 0),
        source.source("1", 1),
        source.source("1:1", 0),
    );
    let explicit = (
        source.source("4", 0),
        source.source("1", 2),
        source.source("6", 0),
        source.source("4:1:6", 0),
    );
    let generated =
        DaeProvenance::generated(DaeGeneration::IndexReduction, source.span("7:2:9", 0)).unwrap();
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions.at(omitted.0).literal(DaeLiteral::Integer(1))?;
            let stop = expressions.at(omitted.1).literal(DaeLiteral::Integer(1))?;
            expressions.at(omitted.2).range(start, None, stop)?;

            let start = expressions.at(explicit.0).literal(DaeLiteral::Integer(4))?;
            let step = expressions.at(explicit.1).literal(DaeLiteral::Integer(1))?;
            let stop = expressions.at(explicit.2).literal(DaeLiteral::Integer(6))?;
            expressions.at(explicit.3).range(start, Some(step), stop)?;

            let start = expressions.at(generated).literal(DaeLiteral::Integer(7))?;
            let step = expressions.at(generated).literal(DaeLiteral::Integer(2))?;
            let stop = expressions.at(generated).literal(DaeLiteral::Integer(9))?;
            expressions.at(generated).range(start, Some(step), stop)?;
            Ok(())
        })
    })
    .unwrap();
    (dae, omitted, explicit, generated)
}

fn range_node(wire: &serde_json::Value, ordinal: usize) -> &serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get("range"))
        .nth(ordinal)
        .unwrap()
}

fn range_node_mut(wire: &mut serde_json::Value, ordinal: usize) -> &mut serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("range"))
        .nth(ordinal)
        .unwrap()
}
