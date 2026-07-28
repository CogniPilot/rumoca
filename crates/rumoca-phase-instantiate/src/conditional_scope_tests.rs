//! Conditional components whose condition is settled outside the declaring class.
//!
//! MLS §4.4.5 requires a conditional component's condition to be a Boolean
//! parameter expression decided at translation time. The value it reads is often
//! not written in the class that declares the component: it arrives as a
//! modification written by an enclosing class (MLS §7.2), it names a constant the
//! declaring class reached through an `import` (MLS §13.2/§5.3.2), or it names a
//! field of a record component whose default is declared by a base record
//! (MLS §7.1). Each of those has to be followed to a real declaration — an
//! undecided condition is an error, never a guessed `true` or `false`
//! (SPEC_0008).

use crate::instantiate_model;
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

fn instantiate(source: &str, model: &str) -> ast::InstanceOverlay {
    let file_name = "<conditional_scope_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.into_inner();
    instantiate_model(&tree, model).expect("instantiation should succeed")
}

fn disabled_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .disabled_components
        .iter()
        .map(rumoca_core::ComponentPath::to_flat_string)
        .collect()
}

fn component_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect()
}

/// A Boolean parameter bound to a literal decides its condition even when the
/// same class also declares Real parameters this phase cannot fold.
///
/// MLS §4.4.5 scopes the requirement to the condition expression itself, so a
/// neighbouring parameter that has no value here is irrelevant to it.
#[test]
fn literal_boolean_condition_survives_undecidable_real_neighbours() {
    let source = r"
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Converter
        parameter Boolean useExtra = false;
        parameter Real Lsigma;
        parameter Real effectiveTurns[3];
        Load extra if useExtra;
        Real z;
    equation
        z = 2.0;
    end Converter;
    model Plant
        Converter converter;
    end Plant;
    ";

    let overlay = instantiate(source, "Plant");

    assert_eq!(
        disabled_paths(&overlay),
        vec!["converter.extra".to_string()]
    );
}

/// MLS §13.2/§5.3.2: a condition may name a package constant the declaring class
/// imported, so deciding it requires resolving the short name through the class's
/// `import` aliases.
#[test]
fn condition_reading_an_imported_package_constant_is_decided() {
    let source = r"
    package Consts
        constant Real eps = 1.0e-15;
    end Consts;
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Winding
        import Consts.eps;
        parameter Real ratio = 1.0;
        Load stray if ratio > eps;
        Real z;
    equation
        z = 2.0;
    end Winding;
    model Plant
        Winding winding;
    end Plant;
    ";

    let overlay = instantiate(source, "Plant");

    assert!(disabled_paths(&overlay).is_empty());
    assert!(
        component_paths(&overlay)
            .iter()
            .any(|path| path == "winding.stray"),
        "enabled conditional component must be instantiated"
    );
}

/// The MSL `PolyphaseElectroMagneticConverter` shape: an enclosing class replaces
/// the Boolean parameter's literal default with a Real comparison written in its
/// own scope, naming a constant it imported (MLS §7.2 + §13.2).
#[test]
fn boolean_modifier_comparing_an_imported_constant_decides_nested_condition() {
    let source = r"
    package Consts
        constant Real eps = 1.0e-15;
    end Consts;
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Converter
        parameter Boolean useStray = false;
        parameter Real Lsigma = 0.0;
        Load stray if useStray;
        Real z;
    equation
        z = 2.0;
    end Converter;
    model Winding
        import Consts.eps;
        parameter Real ratio = 1.0;
        Converter converter(final useStray = ratio < (1.0 - eps));
    end Winding;
    model Plant
        Winding winding;
    end Plant;
    ";

    let overlay = instantiate(source, "Plant");

    assert_eq!(
        disabled_paths(&overlay),
        vec!["winding.converter.stray".to_string()]
    );
}

/// MLS §5.3.2: the same comparison written with the constant's qualified name
/// must fold too — the Real lookup has to reach a class-level constant, not only
/// components of the enclosing scope.
#[test]
fn condition_comparing_a_qualified_class_constant_is_decided() {
    let source = r"
    package Consts
        constant Real eps = 1.0e-15;
    end Consts;
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Converter
        parameter Boolean useStray = false;
        Load stray if useStray;
        Real z;
    equation
        z = 2.0;
    end Converter;
    model Winding
        parameter Real ratio = 1.0;
        Converter converter(final useStray = ratio < (1.0 - Consts.eps));
    end Winding;
    model Plant
        Winding winding;
    end Plant;
    ";

    let overlay = instantiate(source, "Plant");

    assert_eq!(
        disabled_paths(&overlay),
        vec!["winding.converter.stray".to_string()]
    );
}

/// MLS §7.1: a record's elements include the inherited ones, so `data.field`
/// takes the default declared by the base record when the derived record does not
/// redeclare it. The MSL machines route both the Boolean `useDamperCage` and the
/// Real `ratioCommonStatorLeakage` through such a record.
#[test]
fn record_field_default_inherited_from_a_base_record_decides_conditions() {
    let source = r"
    record BaseData
        parameter Boolean useCage = false;
        parameter Real ratio = 1.0;
    end BaseData;
    record MachineData
        extends BaseData;
        parameter Real extra = 2.0;
    end MachineData;
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Winding
        parameter Boolean useCage = true;
        Load cage if useCage;
        Real z;
    equation
        z = 2.0;
    end Winding;
    model Plant
        MachineData data;
        Load stray if data.ratio > 0.5;
        Winding winding(useCage = data.useCage);
    end Plant;
    ";

    let overlay = instantiate(source, "Plant");

    assert_eq!(
        disabled_paths(&overlay),
        vec!["winding.cage".to_string()],
        "`useCage` must take the inherited record default `false`"
    );
    assert!(
        component_paths(&overlay).iter().any(|path| path == "stray"),
        "`data.ratio` must fold to the inherited record default 1.0, enabling `stray`"
    );
}

/// An undecidable condition stays an error: nothing here may be answered from a
/// `start` attribute or any other substitute (SPEC_0008).
#[test]
fn condition_with_no_declared_value_is_rejected() {
    let source = r"
    model Load
        Real y;
    equation
        y = 1.0;
    end Load;
    model Winding
        parameter Boolean useCage(start = true);
        Load cage if useCage;
        Real z;
    equation
        z = 2.0;
    end Winding;
    model Plant
        Winding winding;
    end Plant;
    ";

    let file_name = "<conditional_scope_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.into_inner();

    let error = instantiate_model(&tree, "Plant").expect_err("undecidable condition must fail");

    assert!(
        error.to_string().contains("cage"),
        "error must name the undecided component: {error}"
    );
}
