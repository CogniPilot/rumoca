use super::Context;

#[test]
fn test_enum_reference_matches_parameter_ignores_fake_suffix_from_subscript_dot() {
    let ctx = Context::new();
    let mut param_names = rustc_hash::FxHashSet::default();
    param_names.insert("medium].energyDynamics");

    assert!(
        !ctx.enum_reference_matches_parameter(
            "pipe1.system[data.medium].energyDynamics",
            &param_names,
        ),
        "dot inside subscript expression must not create synthetic suffix matches",
    );
}

#[test]
fn test_lookup_enum_reference_candidate_ignores_fake_suffix_from_subscript_dot() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "medium].energyDynamics".to_string(),
        "Modelica.Fluid.Types.Dynamics.FixedInitial".to_string(),
    );

    assert_eq!(
        ctx.lookup_enum_reference_candidate("pipe1.system[data.medium].energyDynamics"),
        None,
        "dot inside subscript expression must not create synthetic suffix lookup candidates",
    );
}

#[test]
fn test_resolve_alias_once_ignores_fake_prefix_from_subscript_dot() {
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        "pipe1.system[data".to_string(),
        "badAliasTarget".to_string(),
    );

    assert_eq!(
        ctx.resolve_alias_once("pipe1.system[data.medium].energyDynamics"),
        "pipe1.system[data.medium].energyDynamics".to_string(),
        "dot inside subscript expression must not create synthetic alias prefixes",
    );
}
