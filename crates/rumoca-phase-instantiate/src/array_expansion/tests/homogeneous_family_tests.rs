//! End-to-end coverage for compact component-array instantiation (SPEC_0032 §1).
//!
//! The critical guarantee is differential: the compact template-and-replicate
//! path must produce a byte-equivalent instance overlay to the historical
//! element-by-element expansion, because flat variable order and content are
//! derived from that overlay.

use crate::{InstantiateOptions, instantiate_model_with_options};
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

const CELL_ARRAY: &str = r"
    model Cell
        parameter Real R = 1.0;
        Real v;
        Real i;
    equation
        v = R * i;
    end Cell;
    model Stack
        Cell c[3];
    equation
        c[1].i = 1.0;
        c[2].i = 2.0;
        c[3].i = 3.0;
    end Stack;
";

pub(super) fn instantiate(source: &str, model: &str, compact: bool) -> ast::InstanceOverlay {
    let file_name = "<component_family_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let options = InstantiateOptions {
        compact_component_families: compact,
        ..InstantiateOptions::default()
    };
    instantiate_model_with_options(&tree, model, options).expect("instantiation should succeed")
}

fn component_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect()
}

/// Every instance in `InstanceId` order, as `(id, kind, path)`.
///
/// SPEC_0001: flat variable identity is `first_instance_index + instance_id`,
/// so the compact path must not merely allocate the same *set* of ids — it must
/// allocate them in the same *order*, and therefore hand each instance path the
/// same numeric id the scalar path would have.
fn instance_allocation_order(overlay: &ast::InstanceOverlay) -> Vec<(u32, &'static str, String)> {
    let mut entries: Vec<(u32, &'static str, String)> = overlay
        .components
        .values()
        .map(|data| {
            (
                data.instance_id.index(),
                "component",
                data.qualified_name.to_flat_string(),
            )
        })
        .chain(overlay.classes.values().map(|data| {
            (
                data.instance_id.index(),
                "class",
                data.qualified_name.to_flat_string(),
            )
        }))
        .collect();
    entries.sort_by_key(|(id, _, _)| *id);
    entries
}

#[test]
fn compact_component_families_is_on_by_default() {
    // Every other test in this module passes `compact` explicitly, so nothing
    // else would notice the whole feature being switched off. SPEC_0032 §1 has
    // the compact path as the shipped behaviour; the flag exists only so the
    // differential tests can run the element-by-element expansion.
    assert!(
        InstantiateOptions::default().compact_component_families,
        "compact component families must stay enabled by default"
    );
}

#[test]
fn homogeneous_component_array_records_compact_family() {
    let overlay = instantiate(CELL_ARRAY, "Stack", true);
    assert_eq!(
        overlay.component_families.len(),
        1,
        "expected exactly one compact family, got {:?}",
        overlay
            .component_families
            .iter()
            .map(|family| family.root.to_flat_string())
            .collect::<Vec<_>>()
    );
    let family = &overlay.component_families[0];
    assert_eq!(family.root.to_flat_string(), "c");
    assert_eq!(family.subscript_depth, 0);
    assert_eq!(family.domain.binders.len(), 1);
    assert_eq!(family.domain.binders[0].lower, 1);
    assert_eq!(family.domain.binders[0].upper, 3);
    assert_eq!(family.domain.binders[0].step, 1);
    assert!(!family.template_components.is_empty());
    assert!(!family.template_classes.is_empty());
    // The template ids must actually resolve inside the overlay.
    for id in &family.template_components {
        assert!(
            overlay.get_component(*id).is_some(),
            "missing template {id:?}"
        );
    }
    for id in &family.template_classes {
        assert!(overlay.get_class(*id).is_some(), "missing template {id:?}");
    }

    // Only one domain point went through `instantiate_component`, yet all three
    // appear in the overlay: the other two are pure reindexed replications.
    let members = component_paths(&overlay)
        .into_iter()
        .filter(|path| path.starts_with("c["))
        .count();
    assert_eq!(members, family.template_components.len() * 3);
    let member_classes = overlay
        .classes
        .values()
        .filter(|class| class.qualified_name.to_flat_string().starts_with("c["))
        .count();
    assert_eq!(member_classes, family.template_classes.len() * 3);
}

#[test]
fn family_replication_matches_scalar_expansion() {
    let compact = instantiate(CELL_ARRAY, "Stack", true);
    let scalar = instantiate(CELL_ARRAY, "Stack", false);
    assert!(!compact.component_families.is_empty());
    assert!(scalar.component_families.is_empty());
    assert_overlays_equivalent(&compact, &scalar);
}

#[test]
fn family_replication_allocates_instance_ids_in_scalar_order() {
    // SPEC_0001: occurrence keys use the raw `InstanceId`, so replication must
    // interleave component and class allocation exactly as
    // `instantiate_component` / `instantiate_class` do (component, then its own
    // class, then that class's components) rather than emitting all components
    // of a member before all of its classes. Allocation is one-based because
    // `InstanceId::UNSET` reserves zero.
    let compact = instantiate(CELL_ARRAY, "Stack", true);
    let expected: Vec<(u32, &'static str, String)> = [
        (1, "class", ""),
        (2, "component", "c[1]"),
        (3, "class", "c[1]"),
        (4, "component", "c[1].R"),
        (5, "component", "c[1].v"),
        (6, "component", "c[1].i"),
        (7, "component", "c[2]"),
        (8, "class", "c[2]"),
        (9, "component", "c[2].R"),
        (10, "component", "c[2].v"),
        (11, "component", "c[2].i"),
        (12, "component", "c[3]"),
        (13, "class", "c[3]"),
        (14, "component", "c[3].R"),
        (15, "component", "c[3].v"),
        (16, "component", "c[3].i"),
    ]
    .into_iter()
    .map(|(id, kind, path)| (id, kind, path.to_string()))
    .collect();
    assert_eq!(instance_allocation_order(&compact), expected);
    // The same literal order is what element-by-element expansion produces.
    assert_eq!(
        instance_allocation_order(&instantiate(CELL_ARRAY, "Stack", false)),
        expected
    );
}

#[test]
fn nested_component_array_replication_matches_scalar_expansion() {
    // Exercises reindexing at a non-zero subscript depth plus an inner array,
    // `each` bindings and connections inside the replicated element.
    const SOURCE: &str = r"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Branch
            Pin p;
            Pin n;
            parameter Real R = 2.0;
        equation
            connect(p, n);
        end Branch;
        model Bank
            Branch b[2](each R = 5.0);
            Pin pins[2];
        equation
            for k in 1:2 loop
                connect(pins[k], b[k].p);
            end for;
        end Bank;
        model Plant
            Bank banks[3];
        end Plant;
    ";
    let compact = instantiate(SOURCE, "Plant", true);
    let scalar = instantiate(SOURCE, "Plant", false);
    assert!(!compact.component_families.is_empty());
    assert_overlays_equivalent(&compact, &scalar);

    let roots: Vec<String> = compact
        .component_families
        .iter()
        .map(|family| family.root.to_flat_string())
        .collect();
    // The outer array owns a family, and each replicated element keeps its own
    // owner for the nested `Branch b[2]` / `Pin pins[2]` arrays.
    assert!(roots.contains(&"banks".to_string()), "roots {roots:?}");
    for index in 1..=3 {
        assert!(
            roots.contains(&format!("banks[{index}].b")),
            "missing nested family for banks[{index}].b in {roots:?}"
        );
    }
    for family in &compact.component_families {
        for id in &family.template_components {
            assert!(
                compact.get_component(*id).is_some(),
                "family {} references a missing template component",
                family.root.to_flat_string()
            );
        }
    }

    // The `for k in 1:2` connect inside `Bank` is stored as a compact
    // connection family, so every replicated `banks[n]` must carry the family
    // endpoints re-rooted at its own domain point. Without the endpoint rewrite
    // in `component_family::reindex_connection` all three banks would still
    // point at `banks[1]`.
    for index in 1..=3 {
        let path = format!("banks[{index}]");
        let class = compact
            .classes
            .values()
            .find(|class| class.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing class {path}"));
        let family = class.connections[0]
            .family
            .as_ref()
            .unwrap_or_else(|| panic!("{path} lost its compact connection family"));
        assert_eq!(
            family.a.parts[0].1[0],
            rumoca_core::AffineForm::constant(index, 1)
        );
        assert_eq!(
            family.b.parts[0].1[0],
            rumoca_core::AffineForm::constant(index, 1)
        );
        // The binder-carrying subscript of the inner arrays stays symbolic.
        assert_eq!(
            family.a.parts[1].1[0],
            rumoca_core::AffineForm::unit_binder(0, 1)
        );
        assert_eq!(
            family.b.parts[1].1[0],
            rumoca_core::AffineForm::unit_binder(0, 1)
        );
    }
}

#[test]
fn per_element_modifier_array_falls_back_to_scalar_expansion() {
    const SOURCE: &str = r"
        model Cell
            parameter Real R = 1.0;
            Real v;
        end Cell;
        model Stack
            Cell c[3](R = {1.0, 2.0, 3.0});
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert!(
        compact.component_families.is_empty(),
        "per-element modifiers must keep scalar expansion"
    );
    let scalar = instantiate(SOURCE, "Stack", false);
    assert_overlays_equivalent(&compact, &scalar);
    // The distributed per-element values must survive.
    for (index, expected) in ["1.0", "2.0", "3.0"].iter().enumerate() {
        let path = format!("c[{}].R", index + 1);
        let data = compact
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing {path}"));
        let rendered = format!("{:?}", data.binding);
        assert!(
            rendered.contains(expected),
            "expected {path} to bind {expected}, got {rendered}"
        );
    }
}

#[test]
fn each_modifier_array_stays_compact() {
    const SOURCE: &str = r"
        model Cell
            parameter Real R = 1.0;
            Real v;
        end Cell;
        model Stack
            Cell c[3](each R = 5.0);
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert_eq!(compact.component_families.len(), 1);
    for index in 1..=3 {
        let path = format!("c[{index}].R");
        let data = compact
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing {path}"));
        assert!(
            format!("{:?}", data.binding).contains("5.0"),
            "expected {path} to bind 5.0"
        );
    }
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "Stack", false));
}

#[test]
fn indexed_binding_array_falls_back_to_scalar_expansion() {
    const SOURCE: &str = r"
        record Complex
            Real re;
            Real im;
        end Complex;
        model Source
            Complex y[3];
        end Source;
        model Wrap
            Source s;
            Complex v[3] = s.y;
        end Wrap;
    ";
    let compact = instantiate(SOURCE, "Wrap", true);
    let scalar = instantiate(SOURCE, "Wrap", false);
    assert!(
        compact
            .component_families
            .iter()
            .all(|family| family.root.to_flat_string() != "v"),
        "an array with a per-element binding must not be compacted"
    );
    assert_overlays_equivalent(&compact, &scalar);
}

#[test]
fn inner_declaration_inside_array_element_falls_back() {
    // MLS §5.4 registration is path-dependent, so the replication guard must
    // refuse the template and every element must still be instantiated.
    const SOURCE: &str = r"
        model Root
            Real g;
        end Root;
        model Node
            inner Root root;
            Real x;
        end Node;
        model World
            Node n[3];
        end World;
    ";
    let compact = instantiate(SOURCE, "World", true);
    assert!(
        compact.component_families.is_empty(),
        "inner/outer registration must block replication"
    );
    let paths = component_paths(&compact);
    for index in 1..=3 {
        let path = format!("n[{index}].root.g");
        assert!(paths.contains(&path), "missing {path} in {paths:?}");
    }
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "World", false));
}

#[test]
fn zero_sized_array_records_no_family() {
    const SOURCE: &str = r"
        model Cell
            Real v;
        end Cell;
        model Stack
            Cell c[0];
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert!(compact.component_families.is_empty());
    assert_eq!(compact.array_parent_dims.get("c"), Some(&vec![0]));
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "Stack", false));
}

#[test]
fn zero_sized_primitive_arrays_retain_their_typed_instance_headers() {
    const SOURCE: &str = r"
        model EmptyInterface
            input Real u[0];
            parameter Real p[0];
        end EmptyInterface;
    ";
    let overlay = instantiate(SOURCE, "EmptyInterface", true);
    let u = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "u")
        .expect("empty input keeps one typed declaration header");
    assert_eq!(u.dims, [0]);
    assert!(u.is_primitive);
    assert!(matches!(u.causality, rumoca_core::Causality::Input(_)));

    let p = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "p")
        .expect("empty parameter keeps one typed declaration header");
    assert_eq!(p.dims, [0]);
    assert!(matches!(
        p.variability,
        rumoca_core::Variability::Parameter(_)
    ));
    assert_eq!(overlay.array_parent_dims.get("u"), Some(&vec![0]));
    assert_eq!(overlay.array_parent_dims.get("p"), Some(&vec![0]));
}

/// Assert that two overlays are equivalent, including `InstanceId` numbering.
fn assert_overlays_equivalent(compact: &ast::InstanceOverlay, scalar: &ast::InstanceOverlay) {
    assert_eq!(
        instance_allocation_order(compact),
        instance_allocation_order(scalar),
        "instance id allocation order differs"
    );
    assert_eq!(
        compact.components.len(),
        scalar.components.len(),
        "component count differs: {:?} vs {:?}",
        component_paths(compact),
        component_paths(scalar)
    );
    for (left, right) in compact.components.values().zip(scalar.components.values()) {
        assert_component_equivalent(left, right);
    }
    assert_eq!(compact.classes.len(), scalar.classes.len());
    for (left, right) in compact.classes.values().zip(scalar.classes.values()) {
        assert_class_equivalent(left, right);
    }
    assert_eq!(
        compact.array_parent_dims, scalar.array_parent_dims,
        "array_parent_dims differ"
    );
    assert_eq!(
        compact.disabled_components, scalar.disabled_components,
        "disabled_components differ"
    );
    assert_eq!(
        compact.each_modifier_bindings, scalar.each_modifier_bindings,
        "each_modifier_bindings differ"
    );
    assert_eq!(
        compact.outer_prefix_to_inner, scalar.outer_prefix_to_inner,
        "outer_prefix_to_inner differ"
    );
    assert_eq!(
        compact.inner_outer_to_parent_inner, scalar.inner_outer_to_parent_inner,
        "inner_outer_to_parent_inner differ"
    );
    assert_eq!(compact.synthesized_inners, scalar.synthesized_inners);
}

fn assert_component_equivalent(left: &ast::InstanceData, right: &ast::InstanceData) {
    let path = left.qualified_name.to_flat_string();
    assert_eq!(path, right.qualified_name.to_flat_string());
    assert_eq!(
        left.component_ref.as_ref().map(render_reference),
        right.component_ref.as_ref().map(render_reference),
        "component_ref differs for {path}"
    );
    assert_eq!(left.dims, right.dims, "dims differ for {path}");
    assert_eq!(
        left.type_name, right.type_name,
        "type_name differs for {path}"
    );
    assert_eq!(
        left.type_def_id, right.type_def_id,
        "type_def_id differs for {path}"
    );
    assert_eq!(
        left.variability, right.variability,
        "variability differs for {path}"
    );
    assert_eq!(
        left.causality, right.causality,
        "causality differs for {path}"
    );
    assert_eq!(left.flow, right.flow, "flow differs for {path}");
    assert_eq!(left.stream, right.stream, "stream differs for {path}");
    assert_eq!(
        left.is_primitive, right.is_primitive,
        "is_primitive differs for {path}"
    );
    assert_eq!(left.start, right.start, "start differs for {path}");
    assert_eq!(left.binding, right.binding, "binding differs for {path}");
    assert_eq!(
        left.binding_source, right.binding_source,
        "binding_source differs for {path}"
    );
    assert_eq!(
        left.binding_source_scope, right.binding_source_scope,
        "binding_source_scope differs for {path}"
    );
    assert_eq!(
        left.declaration_source_scope, right.declaration_source_scope,
        "declaration_source_scope differs for {path}"
    );
    assert_eq!(
        left.attribute_source_scopes, right.attribute_source_scopes,
        "attribute_source_scopes differ for {path}"
    );
    assert_eq!(
        left.oc_record_path, right.oc_record_path,
        "oc_record_path differs for {path}"
    );
    assert_eq!(
        left.is_overconstrained, right.is_overconstrained,
        "is_overconstrained differs for {path}"
    );
    assert_eq!(
        left.is_protected, right.is_protected,
        "is_protected differs for {path}"
    );
    assert_eq!(
        left.is_connector_type, right.is_connector_type,
        "is_connector_type differs for {path}"
    );
}

fn assert_class_equivalent(left: &ast::ClassInstanceData, right: &ast::ClassInstanceData) {
    let path = left.qualified_name.to_flat_string();
    assert_eq!(path, right.qualified_name.to_flat_string());
    assert_eq!(
        left.class_def_id, right.class_def_id,
        "class_def_id differs for {path}"
    );
    assert_eq!(
        left.source_scope, right.source_scope,
        "source_scope differs for {path}"
    );
    assert_eq!(
        left.source_scope_id, right.source_scope_id,
        "source_scope_id differs for {path}"
    );
    assert_eq!(
        equation_origins(&left.equations),
        equation_origins(&right.equations),
        "equation origins differ for {path}"
    );
    assert_eq!(
        equation_origins(&left.initial_equations),
        equation_origins(&right.initial_equations),
        "initial equation origins differ for {path}"
    );
    assert_eq!(
        connection_endpoints(left),
        connection_endpoints(right),
        "connections differ for {path}"
    );
    assert_eq!(
        left.resolved_imports, right.resolved_imports,
        "resolved_imports differ for {path}"
    );
}

fn equation_origins(equations: &[ast::InstanceEquation]) -> Vec<String> {
    equations
        .iter()
        .map(|equation| equation.origin.to_flat_string())
        .collect()
}

/// Render every field of a connection that replication rewrites, including the
/// compact family endpoints (`component_family::reindex_connection`).
fn connection_endpoints(class: &ast::ClassInstanceData) -> Vec<(String, String, String, String)> {
    class
        .connections
        .iter()
        .map(|connection| {
            (
                connection.a.to_flat_string(),
                connection.b.to_flat_string(),
                connection.scope.clone(),
                render_connection_family(connection.family.as_ref()),
            )
        })
        .collect()
}

fn render_connection_family(family: Option<&ast::InstanceConnectionFamily>) -> String {
    let Some(family) = family else {
        return "none".to_string();
    };
    format!(
        "{:?} | {} | {}",
        family.domain,
        render_endpoint(&family.a),
        render_endpoint(&family.b)
    )
}

fn render_endpoint(endpoint: &ast::InstanceConnectionEndpoint) -> String {
    endpoint
        .parts
        .iter()
        .map(|(name, subscripts)| format!("{name}{subscripts:?}"))
        .collect::<Vec<_>>()
        .join(".")
}

fn render_reference(reference: &rumoca_core::ComponentReference) -> String {
    rumoca_core::ComponentPath::from_component_reference(reference).to_flat_string()
}
