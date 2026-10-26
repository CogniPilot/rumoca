//! MLS §§7.2, 10.1, 12.6; SPEC_0032 §1: a record binding must determine
//! component-array instances before Flat consumes the occurrence graph.

use super::homogeneous_family_tests::instantiate;
use rumoca_core::ComponentPath;

fn source(binding: &str, modifier: &str) -> String {
    format!(
        r#"
record Settings
  parameter Integer count = 2;
end Settings;
model Cell
  Real value;
end Cell;
model Child
  parameter Settings settings;
  Cell cells[settings.count];
end Child;
model Top
  parameter Settings settings {binding};
  Child child({modifier});
end Top;
"#
    )
}

fn assert_count(binding: &str, modifier: &str, count: usize) {
    let source = source(binding, modifier);
    for compact in [false, true] {
        let overlay = instantiate(&source, "Top", compact);
        let path = ComponentPath::from_flat_path("child.cells");
        assert_eq!(
            overlay.array_parent_dims.get(&path),
            Some(&vec![count as i64]),
            "binding={binding}, modifier={modifier}, compact={compact}"
        );
        for index in 1..=count {
            assert!(overlay.components.values().any(|component| {
                component.qualified_name.to_flat_string() == format!("child.cells[{index}].value")
            }));
        }
        assert!(
            !overlay.components.values().any(|component| {
                component.qualified_name.to_flat_string() == "child.cells.value"
            })
        );
    }
}

#[test]
fn record_constructor_binding_determines_component_array_dimensions() {
    assert_count("= Settings(count = 3)", "settings = settings", 3);
}

#[test]
fn record_constructor_field_modifier_does_not_use_stale_default() {
    assert_count(
        "= Settings(count = 3)",
        "settings(count = settings.count)",
        3,
    );
}

#[test]
fn record_default_binding_determines_component_array_dimensions() {
    assert_count("", "settings = settings", 2);
}

#[test]
fn record_field_modification_determines_component_array_dimensions() {
    assert_count("(count = 3)", "settings = settings", 3);
}

#[test]
fn positional_record_constructor_determines_dimensions() {
    assert_count("= Settings(5)", "settings = settings", 5);
}

#[test]
fn record_constructor_defaults_use_record_scope_and_supplied_arguments() {
    let source = source("= Settings(base = 4)", "settings = settings")
        .replace(
            "parameter Integer count = 2;",
            "parameter Integer base = 2; parameter Integer count = base + 1;",
        )
        .replace("model Top", "model Top\n  parameter Integer base = 99;");
    for compact in [false, true] {
        let overlay = instantiate(&source, "Top", compact);
        assert_eq!(
            overlay
                .array_parent_dims
                .get(&ComponentPath::from_flat_path("child.cells")),
            Some(&vec![5])
        );
    }
}
