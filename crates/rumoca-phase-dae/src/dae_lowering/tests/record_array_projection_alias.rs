use super::*;

#[test]
fn test_record_array_projection_alias_only_resolves_concrete_indices() {
    let mut dae = Dae::new();
    let lane = record_lane_variable("bus.cells[1].x", 90_121);
    dae.variables.algebraics.insert(lane.name.clone(), lane);
    let aliases = build_record_array_projection_alias_map(&dae).unwrap();

    let mut concrete = component_ref(&[("bus", None), ("cells", None), ("x", Some(1))]);
    concrete.def_id = None;
    assert_eq!(
        aliases.resolve(&concrete).unwrap().as_str(),
        "bus.cells[1].x"
    );
    concrete.def_id = Some(rumoca_core::DefId::new(99_999));
    assert!(
        aliases.resolve(&concrete).is_none(),
        "a resolved but mismatched declaration identity must not fall back to path-only lookup"
    );

    let mut colon = component_ref(&[("bus", None), ("cells", None), ("x", None)]);
    colon.parts.last_mut().unwrap().subs =
        vec![rumoca_core::Subscript::Colon { span: test_span() }];
    assert!(aliases.resolve(&colon).is_none());

    let mut expression = component_ref(&[("bus", None), ("cells", None), ("x", None)]);
    expression.parts.last_mut().unwrap().subs = vec![rumoca_core::Subscript::Expr {
        expr: Box::new(var_ref("i")),
        span: test_span(),
    }];
    assert!(aliases.resolve(&expression).is_none());
}

fn record_lane_alias_identity(
    variable: &dae::Variable,
) -> (
    StructuredProjectionPath,
    StructuredProjectionIdentity,
    rumoca_core::Reference,
) {
    let component_ref = variable.component_ref.as_ref().unwrap();
    let mut projection = component_ref.clone();
    let subscripts = std::mem::take(&mut projection.parts[1].subs);
    projection.parts.last_mut().unwrap().subs.extend(subscripts);
    let path = StructuredProjectionPath::from_component_ref(&projection).unwrap();
    let identity = StructuredProjectionIdentity {
        path: path.clone(),
        declaration: component_ref.def_id.unwrap(),
    };
    let reference = rumoca_core::Reference::with_component_reference(
        variable.name.as_str(),
        component_ref.clone(),
    );
    (path, identity, reference)
}

#[test]
fn test_record_array_projection_alias_duplicate_same_identity_and_target_is_idempotent() {
    let variable = record_lane_variable("bus.cells[1].x", 90_131);
    let mut aliases = RecordArrayProjectionAliases::default();
    let direct = HashMap::new();

    append_record_array_projection_aliases(&mut aliases, &direct, &variable.name, &variable)
        .unwrap();
    append_record_array_projection_aliases(&mut aliases, &direct, &variable.name, &variable)
        .unwrap();

    assert_eq!(aliases.aliases.len(), 1);
    assert_eq!(aliases.unique_identity_by_path.len(), 1);
}

#[test]
fn test_record_array_projection_alias_same_direct_identity_and_target_is_noop() {
    let variable = record_lane_variable("bus.cells[1].x", 90_141);
    let (path, identity, reference) = record_lane_alias_identity(&variable);
    let direct = HashMap::from([(
        path,
        DirectProjectionTarget {
            identity: Some(identity),
            reference,
        },
    )]);
    let mut aliases = RecordArrayProjectionAliases::default();

    append_record_array_projection_aliases(&mut aliases, &direct, &variable.name, &variable)
        .unwrap();

    assert!(aliases.aliases.is_empty());
    assert!(aliases.unique_identity_by_path.is_empty());
}
