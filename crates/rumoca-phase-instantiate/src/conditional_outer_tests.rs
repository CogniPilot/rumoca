//! Conditional components whose condition reads an `outer` component's parameter.
//!
//! MLS §5.4 states that an `outer` element denotes the nearest enclosing `inner`
//! element of the same name, and MLS §4.4.5 requires the condition of a
//! conditional component to be evaluated at translation time so that a disabled
//! component contributes no variables, equations or connections. Before these
//! tests, a condition such as `not world.flag` inside a class that only declares
//! `outer World world` was unevaluable, so both arms of a mutually exclusive
//! pair were instantiated and the resulting system was structurally singular.

use crate::{InstantiateError, instantiate_model};
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

fn instantiate_result(source: &str, model: &str) -> crate::InstantiateResult<ast::InstanceOverlay> {
    let file_name = "<conditional_outer_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.into_inner();
    instantiate_model(&tree, model)
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

/// The `Modelica.Mechanics.MultiBody.Parts.Body` reproduction: a model that uses
/// `Body` without declaring `inner World world`. MLS §5.4 requires a default
/// inner to be synthesized from the outer's class; the `sphere` visualiser then
/// decides against the synthesized world's defaults (`enableAnimation = true`,
/// `defaultBodyDiameter = 1/9 > 0`) instead of raising EI006.
const BODY_WITHOUT_INNER_WORLD: &str = r#"
    model World
        parameter Boolean enableAnimation = true;
        parameter Real nominalLength = 1;
        parameter Real defaultBodyDiameter = nominalLength/9;
        annotation(
            defaultComponentName="world",
            defaultComponentPrefixes="inner",
            missingInnerMessage="A default world component with the default
gravity field will be used.");
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
    model Standalone
        Body body;
    end Standalone;
"#;

#[test]
fn unmatched_outer_synthesizes_default_inner_and_decides_the_conditional() {
    let overlay = instantiate(BODY_WITHOUT_INNER_WORLD, "Standalone");

    assert!(
        overlay.synthesized_inners.contains(&"world".to_string()),
        "MLS §5.4 must synthesize a default inner `world`; synthesized = {:?}",
        overlay.synthesized_inners
    );
    let disabled = disabled_paths(&overlay);
    assert!(
        !disabled.contains(&"body.sphere".to_string()),
        "the synthesized world's enableAnimation = true keeps `sphere`; disabled = {disabled:?}"
    );
    assert!(
        component_paths(&overlay)
            .iter()
            .any(|p| p == "body.sphere.s"),
        "the enabled visualiser must contribute its variables"
    );
}

#[test]
fn synthesized_inner_surfaces_missing_inner_message_annotation() {
    let overlay = instantiate(BODY_WITHOUT_INNER_WORLD, "Standalone");
    assert!(
        overlay
            .synthesized_inner_messages
            .iter()
            .any(|message| message
                == "A default world component with the default gravity field will be used."),
        "the class-authored missingInnerMessage must reach the overlay; messages = {:?}",
        overlay.synthesized_inner_messages
    );
}

/// A real matching inner in an enclosing scope still wins: no synthesis occurs.
#[test]
fn real_inner_wins_over_synthesis() {
    let overlay = instantiate(REAL_CONDITION_THROUGH_OUTER, "Plant");
    assert!(
        overlay.synthesized_inners.is_empty(),
        "a declared inner must avoid synthesis; synthesized = {:?}",
        overlay.synthesized_inners
    );
}

/// MLS §5.4: an outer whose class is partial cannot be synthesized. rumoca
/// already rejects a partial-class outer in the resolve phase (ER005) before
/// synthesis is reached, so the compiler never papers over it with a default
/// inner. `retry_with_synthetic_inners` keeps a defensive EI012 backstop should
/// such a declaration ever reach it.
#[test]
fn unmatched_outer_of_partial_class_is_rejected() {
    let source = r"
        partial model Env
            parameter Real g = 9.81;
        end Env;
        model Uses
            outer Env env;
            Real x;
        equation
            x = env.g;
        end Uses;
        model Root
            Uses uses;
        end Root;
    ";
    let file_name = "<conditional_outer_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let error = resolve(ast::ParsedTree::new(tree))
        .expect_err("a partial-class outer must be rejected, not synthesized");
    assert!(
        error.iter().any(|diag| diag.message.contains("partial")),
        "expected a partial-class rejection, got: {error:?}"
    );
}

/// MLS §5.4: two same-name unmatched outers naming different classes have no
/// unique class to synthesize, so the compiler must report the conflict.
#[test]
fn unmatched_outers_with_conflicting_classes_error() {
    let source = r"
        model WorldA
            parameter Real g = 1;
        end WorldA;
        model WorldB
            parameter Real g = 2;
        end WorldB;
        model UsesA
            outer WorldA world;
            Real x;
        equation
            x = world.g;
        end UsesA;
        model UsesB
            outer WorldB world;
            Real y;
        equation
            y = world.g;
        end UsesB;
        model Root
            UsesA a;
            UsesB b;
        end Root;
    ";
    let error =
        instantiate_result(source, "Root").expect_err("conflicting outer classes must error");
    assert!(
        matches!(
            *error,
            InstantiateError::AutomaticInnerConflictingClass { .. }
        ),
        "expected EI015 conflicting-class error, got: {error:?}"
    );
}
