use super::Context;

#[test]
fn test_resolve_alias_once_ignores_fake_prefix_from_subscript_dot() {
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("pipe1.system[data"),
        rumoca_core::ComponentPath::from_flat_path("badAliasTarget"),
    );

    assert_eq!(
        ctx.resolve_alias_once("pipe1.system[data.medium].energyDynamics"),
        "pipe1.system[data.medium].energyDynamics".to_string(),
        "dot inside subscript expression must not create synthetic alias prefixes",
    );
}

#[test]
fn record_alias_closure_accepts_a_twelve_edge_chain() {
    let mut aliases = rustc_hash::FxHashMap::default();
    for index in 0..12 {
        aliases.insert(
            rumoca_core::ComponentPath::from_flat_path(&format!("record{index}")),
            rumoca_core::ComponentPath::from_flat_path(&format!("record{}", index + 1)),
        );
    }

    crate::compute_transitive_alias_closure(&mut aliases)
        .expect("a finite twelve-edge alias chain closes exactly");

    assert_eq!(
        aliases[&rumoca_core::ComponentPath::from_flat_path("record0")].as_str(),
        "record12"
    );
}

#[test]
fn record_alias_closure_rejects_an_exact_cycle() {
    let mut aliases = rustc_hash::FxHashMap::default();
    aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("left"),
        rumoca_core::ComponentPath::from_flat_path("right"),
    );
    aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("right"),
        rumoca_core::ComponentPath::from_flat_path("left"),
    );

    assert!(crate::compute_transitive_alias_closure(&mut aliases).is_err());
}
