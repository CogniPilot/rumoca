use super::*;
use crate::codegen::fmi_projection_tests::{model_with_one_state_run, state_input};

fn component() -> Arc<solve::fmi::FmiCCodegenView> {
    let mut model = model_with_one_state_run(false);
    model.initial_y = vec![-0.0, f64::from_bits(1.0f64.to_bits() + 1)].into();
    model.parameters = vec![3.5].into();
    Arc::new(
        solve::fmi::FmiComponent::construct(model, vec![state_input()])
            .unwrap()
            .into_codegen_view()
            .try_c()
            .unwrap(),
    )
}

#[test]
fn lazy_declaration_metadata_preserves_source_runs_and_fmi3_start_broadcast() {
    let model = model_with_one_state_run(false);
    let mut input = state_input();
    input.start = solve::SolveInitialValues::repeat(-0.0, 2).unwrap().into();
    input.minimum = Some(solve::SolveInitialValues::repeat(0.0, 2).unwrap());
    input.maximum = Some(vec![3.0, 4.0].into());
    input.nominal = Some(solve::SolveInitialValues::repeat(1.0, 2).unwrap());
    let source = Arc::new(
        solve::fmi::FmiComponent::construct(model, vec![input])
            .unwrap()
            .into_codegen_view()
            .try_c()
            .unwrap(),
    );
    let projected = value(Arc::clone(&source)).unwrap();
    let variables = projected.get_attr("variables").unwrap();
    assert!(
        variables
            .downcast_object_ref::<super::super::LazySeq>()
            .is_some()
    );
    let entry = variables.get_item(&Value::from(0)).unwrap();
    let names = entry.get_attr("scalar_names").unwrap();
    assert!(
        names
            .downcast_object_ref::<super::super::LazySeq>()
            .is_some()
    );
    assert_eq!(
        names.get_item(&Value::from(1)).unwrap().as_str(),
        Some("x[2]")
    );
    assert_eq!(entry.get_attr("start").unwrap().len(), Some(2));
    assert_eq!(entry.get_attr("start_fmi3").unwrap().len(), Some(1));
    assert_eq!(
        f64::try_from(
            entry
                .get_attr("start_fmi3")
                .unwrap()
                .get_item(&Value::from(0))
                .unwrap()
        )
        .unwrap()
        .to_bits(),
        (-0.0f64).to_bits()
    );
    assert_eq!(
        f64::try_from(entry.get_attr("minimum_fmi3").unwrap()).unwrap(),
        0.0
    );
    assert_eq!(
        f64::try_from(entry.get_attr("maximum_fmi3").unwrap()).unwrap(),
        4.0
    );
    assert_eq!(
        f64::try_from(entry.get_attr("nominal_fmi3").unwrap()).unwrap(),
        1.0
    );
    for attribute in solve::fmi::FmiNumericAttribute::ALL {
        assert!(
            !source.variables()[0]
                .numeric_attribute(attribute)
                .unwrap()
                .has_dense_view()
        );
    }
}

#[test]
fn equal_literal_starts_do_not_acquire_a_source_broadcast_owner() {
    let mut input = state_input();
    input.start = vec![-0.0, -0.0].into();
    let component =
        solve::fmi::FmiComponent::construct(model_with_one_state_run(false), vec![input]).unwrap();
    let variable = &component.variables()[0];
    assert!(
        variable
            .start()
            .unwrap()
            .run(0)
            .unwrap()
            .literal_bits()
            .is_some()
    );
    assert_eq!(
        variable.numeric_attribute_count(solve::fmi::FmiNumericAttribute::StartFmi3),
        Some(2)
    );
}

fn assert_literal_bits(projected: &Value, bits: &[u64]) {
    let literals = projected.get_attr("bits").unwrap();
    for (ordinal, bits) in bits.iter().enumerate() {
        assert_eq!(
            u64::try_from(literals.get_item(&Value::from(ordinal)).unwrap()).unwrap(),
            *bits
        );
    }
}

#[test]
fn lazy_fmi_fields_match_the_single_canonical_serialization_inventory() {
    let source = component();
    let eager = serde_json::to_value(source.as_ref()).unwrap();
    let lazy = value(Arc::clone(&source)).unwrap();
    assert_eq!(serde_json::to_value(&lazy).unwrap(), eager);
    for buffer in solve::fmi::FmiInstantiationBuffer::ALL {
        let owner = source.instantiation_values(buffer);
        assert!(!owner.has_dense_view());
        let entries = lazy.get_attr(buffer.name()).unwrap();
        assert_eq!(
            usize::try_from(entries.get_attr("count").unwrap()).unwrap(),
            owner.len()
        );
        let runs = entries.get_attr("runs").unwrap();
        assert_eq!(runs.len(), Some(owner.run_count()));
        for (index, run) in owner.runs().enumerate() {
            let projected = runs.get_item(&Value::from(index)).unwrap();
            assert_eq!(
                usize::try_from(projected.get_attr("start").unwrap()).unwrap(),
                run.start()
            );
            if let Some(bits) = run.repeated_bits() {
                assert_eq!(
                    u64::try_from(projected.get_attr("bits").unwrap()).unwrap(),
                    bits
                );
            } else {
                assert_literal_bits(&projected, run.literal_bits().unwrap());
            }
        }
        assert!(!owner.has_dense_view());
    }
}

#[test]
fn lazy_fmi_buffers_retain_the_checked_owner_without_materialized_value_arrays() {
    let source = component();
    let weak = Arc::downgrade(&source);
    let fields = value(Arc::clone(&source)).unwrap();
    // The caller plus the one shared inventory: lazy fields clone the inventory,
    // never the checked view.
    assert_eq!(Arc::strong_count(&source), 2);
    for buffer in solve::fmi::FmiInstantiationBuffer::ALL {
        let entries = fields.get_attr(buffer.name()).unwrap();
        assert!(
            entries
                .downcast_object_ref::<super::super::LazyMap>()
                .is_some()
        );
        let runs = entries.get_attr("runs").unwrap();
        assert!(
            runs.downcast_object_ref::<super::super::LazySeq>()
                .is_some()
        );
        assert!(
            runs.get_item(&Value::from(
                source.instantiation_values(buffer).run_count()
            ))
            .unwrap()
            .is_undefined()
        );
        assert!(!source.instantiation_values(buffer).has_dense_view());
    }
    drop(source);
    let solver = fields
        .get_attr(solve::fmi::FmiInstantiationBuffer::Solver.name())
        .unwrap();
    let runs = solver.get_attr("runs").unwrap();
    let literals = runs
        .get_item(&Value::from(0))
        .unwrap()
        .get_attr("bits")
        .unwrap();
    assert_eq!(
        u64::try_from(literals.get_item(&Value::from(0)).unwrap()).unwrap(),
        (-0.0f64).to_bits()
    );
    drop(literals);
    drop(runs);
    drop(solver);
    drop(fields);
    assert!(weak.upgrade().is_none());
}

#[test]
fn fmi_field_projection_rejects_unpaired_or_nonstring_keys_without_mutation() {
    let mut entries = Entries {
        component: Arc::new(Inventory::C(component())),
        fields: Vec::new(),
    };
    assert!(entries.serialize_key("unpaired").is_err());
    assert!(entries.serialize_value(&1).is_err());
    assert!(entries.serialize_entry(&42, &1).is_err());
    assert!(entries.fields.is_empty());
}

#[test]
fn cloned_render_handles_share_one_exact_fmi_field_projection() {
    let checked = Arc::try_unwrap(component()).unwrap();
    let handle = super::super::SolveRenderHandle::fmi(checked);
    let first = handle.fmi_value().unwrap();
    let second = handle.clone().fmi_value().unwrap();
    for buffer in solve::fmi::FmiInstantiationBuffer::ALL {
        let first_buffer = first.get_attr(buffer.name()).unwrap();
        let second_buffer = second.get_attr(buffer.name()).unwrap();
        assert!(std::ptr::eq(
            first_buffer
                .downcast_object_ref::<super::super::LazyMap>()
                .unwrap(),
            second_buffer
                .downcast_object_ref::<super::super::LazyMap>()
                .unwrap(),
        ));
    }
    drop(handle);
    assert_eq!(
        serde_json::to_value(&first).unwrap(),
        serde_json::to_value(&second).unwrap()
    );
}

#[test]
fn fmi_variable_projection_rejects_unpaired_or_nonstring_keys_without_mutation() {
    let mut entries = VariableEntries {
        component: Arc::new(Inventory::C(component())),
        index: 0,
        fields: Vec::new(),
    };
    assert!(entries.serialize_key("unpaired").is_err());
    assert!(entries.serialize_value(&1).is_err());
    assert!(entries.serialize_entry(&42, &1).is_err());
    assert!(entries.fields.is_empty());
}
