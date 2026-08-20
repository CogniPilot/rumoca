//! Conditional components whose condition reads an `outer` component's parameter.
//!
//! MLS §5.4 states that an `outer` element denotes the nearest enclosing `inner`
//! element of the same name, and MLS §4.4.5 requires the condition of a
//! conditional component to be evaluated at translation time so that a disabled
//! component contributes no variables, equations or connections. Before these
//! tests, a condition such as `not world.flag` inside a class that only declares
//! `outer World world` was unevaluable, so both arms of a mutually exclusive
//! pair were instantiated and the resulting system was structurally singular.

use crate::instantiate_model;
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

/// `inner` record carrying a Boolean parameter, plus a nested component that
/// declares two mutually exclusive conditional sub-components on `outer`
/// references to it.
const INNER_OUTER_CONDITION: &str = r"
    record Settings
        parameter Boolean use3D = false;
        parameter Boolean animate = true;
    end Settings;
    model Simple
        Real x;
    equation
        x = 1.0;
    end Simple;
    model Fancy
        Real y;
    equation
        y = 2.0;
    end Fancy;
    model Rotor
        outer Settings settings;
        Simple simple if not settings.use3D;
        Fancy fancy if settings.use3D;
    end Rotor;
    model Plant
        inner Settings settings(use3D = true);
        Rotor rotor;
    end Plant;
    model PlantDefault
        inner Settings settings;
        Rotor rotor;
    end PlantDefault;
";

fn instantiate(source: &str, model: &str) -> ast::InstanceOverlay {
    let file_name = "<conditional_outer_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.into_inner();
    instantiate_model(&tree, model).expect("instantiation should succeed")
}

fn component_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect()
}

fn disabled_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .disabled_components
        .iter()
        .map(rumoca_core::ComponentPath::to_flat_string)
        .collect()
}

#[test]
fn outer_parameter_modified_at_inner_disables_the_false_arm() {
    let overlay = instantiate(INNER_OUTER_CONDITION, "Plant");
    let paths = component_paths(&overlay);
    let disabled = disabled_paths(&overlay);

    assert!(
        disabled.contains(&"rotor.simple".to_string()),
        "settings.use3D = true must disable `simple`; disabled = {disabled:?}"
    );
    assert!(
        !disabled.contains(&"rotor.fancy".to_string()),
        "settings.use3D = true must keep `fancy`; disabled = {disabled:?}"
    );
    assert!(
        !paths.iter().any(|p| p.starts_with("rotor.simple")),
        "disabled component must contribute no variables; paths = {paths:?}"
    );
    assert!(
        paths.iter().any(|p| p == "rotor.fancy.y"),
        "enabled component must contribute its variables; paths = {paths:?}"
    );
}

#[test]
fn outer_parameter_declaration_default_disables_the_other_arm() {
    let overlay = instantiate(INNER_OUTER_CONDITION, "PlantDefault");
    let paths = component_paths(&overlay);
    let disabled = disabled_paths(&overlay);

    assert!(
        disabled.contains(&"rotor.fancy".to_string()),
        "default use3D = false must disable `fancy`; disabled = {disabled:?}"
    );
    assert!(
        !disabled.contains(&"rotor.simple".to_string()),
        "default use3D = false must keep `simple`; disabled = {disabled:?}"
    );
    assert!(
        !paths.iter().any(|p| p.starts_with("rotor.fancy")),
        "disabled component must contribute no variables; paths = {paths:?}"
    );
    assert!(
        paths.iter().any(|p| p == "rotor.simple.x"),
        "enabled component must contribute its variables; paths = {paths:?}"
    );
}

/// The `Modelica.Mechanics.MultiBody.Parts.Body` shape: a visualiser conditioned
/// on a Real parameter that is bound, through an `outer` reference, to a derived
/// parameter of the inner world (`sphereDiameter = world.defaultBodyDiameter`,
/// itself `nominalLength/9`). MLS §4.4.5 makes this a parameter expression, so it
/// must be decided at translation time.
const REAL_CONDITION_THROUGH_OUTER: &str = r"
    model World
        parameter Boolean enableAnimation = true;
        parameter Real nominalLength = 1;
        parameter Real defaultBodyDiameter = nominalLength/9;
    end World;
    model Shape
        Real s;
    equation
        s = 1.0;
    end Shape;
    model Body
        outer World world;
        parameter Boolean animation = true;
        parameter Real sphereDiameter = world.defaultBodyDiameter;
        Shape sphere if world.enableAnimation and animation and sphereDiameter > 0;
    end Body;
    model Plant
        inner World world;
        Body body;
    end Plant;
    model FlatWorld
        inner World world(nominalLength = 0);
        Body body;
    end FlatWorld;
";

#[test]
fn real_condition_through_outer_reference_keeps_the_component() {
    let overlay = instantiate(REAL_CONDITION_THROUGH_OUTER, "Plant");
    let disabled = disabled_paths(&overlay);
    assert!(
        disabled.is_empty(),
        "sphereDiameter = 1/9 > 0 must keep `sphere`; disabled = {disabled:?}"
    );
    assert!(
        component_paths(&overlay)
            .iter()
            .any(|p| p == "body.sphere.s"),
        "enabled component must contribute its variables"
    );
}

/// A modification on the inner instance must reach the declarations derived from
/// it (MLS §7.2): `nominalLength = 0` makes `defaultBodyDiameter` zero, which
/// makes `sphereDiameter > 0` false.
#[test]
fn inner_modification_propagates_to_derived_real_parameters() {
    let overlay = instantiate(REAL_CONDITION_THROUGH_OUTER, "FlatWorld");
    let disabled = disabled_paths(&overlay);
    let paths = component_paths(&overlay);
    assert!(
        disabled.contains(&"body.sphere".to_string()),
        "nominalLength = 0 must disable `sphere`; disabled = {disabled:?}"
    );
    assert!(
        !paths.iter().any(|p| p.starts_with("body.sphere.")),
        "disabled component must contribute no variables; paths = {paths:?}"
    );
}

/// The `outer` redirection must not leak into unrelated scopes: a sibling class
/// that has no `outer settings` still evaluates its own local parameter.
#[test]
fn local_parameter_still_wins_over_outer_resolution() {
    let source = r"
        record Settings
            parameter Boolean use3D = false;
        end Settings;
        model Leaf
            Real x;
        end Leaf;
        model Local
            parameter Boolean use3D = true;
            Leaf leaf if use3D;
        end Local;
        model Plant
            inner Settings settings(use3D = false);
            Local local1;
        end Plant;
    ";
    let overlay = instantiate(source, "Plant");
    let disabled = disabled_paths(&overlay);
    assert!(
        disabled.is_empty(),
        "local use3D = true must keep `leaf`; disabled = {disabled:?}"
    );
    assert!(
        component_paths(&overlay)
            .iter()
            .any(|p| p == "local1.leaf.x"),
        "local1.leaf.x must exist"
    );
}
