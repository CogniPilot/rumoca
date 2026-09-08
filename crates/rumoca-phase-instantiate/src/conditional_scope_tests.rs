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

use crate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

fn instantiation_outcome(source: &str, model: &str) -> InstantiationOutcome {
    let file_name = "<conditional_scope_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    instantiate_model_with_outcome(&tree, model)
}

fn compile_error(source: &str, model: &str) -> String {
    let file_name = "<conditional_scope_error_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = match resolve(ast::ParsedTree::new(tree)) {
        Ok(resolved) => resolved,
        Err(diagnostics) => return format!("{diagnostics:?}"),
    };
    match instantiate_model_with_outcome(&resolved.inner().clone(), model) {
        InstantiationOutcome::Error(error) => error.to_string(),
        InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "structural error fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        InstantiationOutcome::Success(_) => {
            panic!("the structural connection guard must remain undecidable")
        }
    }
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

fn connection_paths(overlay: &ast::InstanceOverlay) -> Vec<(String, String)> {
    overlay
        .classes
        .values()
        .flat_map(|class| rumoca_eval_ast::connection::scalar_connection_view(&class.connections))
        .map(|connection| {
            let connection = connection.expect("test connection family must have a scalar view");
            (
                connection.a().to_flat_string(),
                connection.b().to_flat_string(),
            )
        })
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

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

    assert_eq!(
        disabled_paths(&overlay),
        vec!["converter.extra".to_string()]
    );
}

/// A record-field Boolean is registered under its qualified instance path
/// before the enclosing class selects structural connection branches. The
/// enclosing modifier, rather than the record declaration default, must decide
/// the branch.
#[test]
fn modified_nested_record_boolean_selects_one_connection_branch_end_to_end() {
    let source = r"
    connector Pin
        Real v;
        flow Real i;
    end Pin;
    record Settings
        parameter Boolean enabled = true;
    end Settings;
    model Plant
        parameter Settings settings(enabled = false);
        Pin a;
        Pin b;
        Pin c;
        Pin d;
    equation
        if settings.enabled then
            connect(a, b);
        else
            connect(c, d);
        end if;
    end Plant;
    ";

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

    assert_eq!(
        connection_paths(&overlay),
        vec![("c".to_string(), "d".to_string())]
    );
}

#[test]
fn shared_scalar_evaluator_selects_boolean_enum_real_and_constant_connection_guard() {
    let source = r"
    connector Pin
        Real v;
        flow Real i;
    end Pin;
    type Mode = enumeration(On, Off);
    record Settings
        constant Boolean enabled = true annotation(Evaluate=false);
    end Settings;
    model Plant
        constant Boolean compileEnabled = true;
        parameter Boolean enabled = true;
        parameter Real threshold = 0.5;
        parameter Mode mode = Mode.On;
        Settings settings;
        Pin a;
        Pin b;
        Pin c;
        Pin d;
    equation
        if compileEnabled and enabled == true and threshold > 0.0 and
           mode == Mode.On and settings.enabled then
            connect(a, b);
        else
            connect(c, d);
        end if;
    end Plant;
    ";

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

    assert_eq!(
        connection_paths(&overlay),
        vec![("a".to_string(), "b".to_string())]
    );
}

#[test]
fn nonevaluable_nested_parameter_occurrences_cannot_select_connection_topology() {
    for declaration in [
        "parameter Settings settings(fixed=false);",
        "parameter Settings settings annotation(Evaluate=false);",
        "parameter Settings settings(enabled(fixed=false)=true);",
    ] {
        let source = format!(
            r"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            record Settings
                parameter Boolean enabled = true;
            end Settings;
            model Plant
                {declaration}
                Pin a;
                Pin b;
                Pin c;
                Pin d;
            equation
                if settings.enabled then
                    connect(a, b);
                else
                    connect(c, d);
                end if;
            end Plant;
            "
        );

        let error = compile_error(&source, "Plant");
        assert!(
            error.contains("cannot decide a connection if-equation branch")
                || error.contains("ER083"),
            "`{declaration}` must fail closed at instantiation: {error}"
        );
    }
}

#[test]
fn enclosing_values_cannot_launder_same_named_blocked_child_parameters() {
    let cases = [
        (
            "parameter Boolean enabled = false;",
            "parameter Boolean enabled = true;",
            "enabled(fixed=false) = true",
            "if enabled then connect(a, b); end if;",
        ),
        (
            "parameter Integer n = 1;",
            "parameter Integer n = 1;",
            "n(fixed=false) = 1",
            "for i in 1:n loop connect(a, b); end for;",
        ),
        (
            "parameter Real threshold = -1.0;",
            "parameter Real threshold = 1.0;",
            "threshold(fixed=false) = 1.0",
            "if threshold > 0.0 then connect(a, b); end if;",
        ),
    ];

    for (parent_parameter, child_parameter, modifier, equation) in cases {
        let source = format!(
            r"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            model Child
                {child_parameter}
                Pin a;
                Pin b;
            equation
                {equation}
            end Child;
            model Plant
                {parent_parameter}
                Child child({modifier});
            end Plant;
            "
        );

        let error = compile_error(&source, "Plant");
        assert!(
            error.contains("cannot decide a connection if-equation branch")
                || error.contains("cannot evaluate connection for-equation range")
                || error.contains("ER083"),
            "a parent parameter must not supply a blocked child value for `{modifier}`: {error}"
        );
    }
}

#[test]
fn selected_record_field_must_itself_be_structurally_evaluable() {
    for field in [
        "Boolean enabled = true;",
        "parameter Boolean enabled(fixed=false) = true;",
        "parameter Boolean enabled = true annotation(Evaluate=false);",
        "parameter Boolean enabled;",
    ] {
        let source = format!(
            r"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            model Settings
                {field}
            end Settings;
            model Plant
                replaceable Settings settings constrainedby Settings;
                Pin a;
                Pin b;
            equation
                if settings.enabled then
                    connect(a, b);
                end if;
            end Plant;
            "
        );

        let file_name = "<deferred_selected_field_error_test>";
        let stored = parse_to_ast(&source, file_name).expect("parse should succeed");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, &source);
        let resolved = resolve(ast::ParsedTree::new(tree))
            .expect("Resolve must defer a tail reached through a replaceable component");
        let error = match instantiate_model_with_outcome(&resolved.inner().clone(), "Plant") {
            InstantiationOutcome::Error(error) => error.to_string(),
            InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
                "selected-field fixture unexpectedly needs inner declarations: {missing_inners:?}"
            ),
            InstantiationOutcome::Success(_) => {
                panic!("Instantiate must reject the selected non-evaluable field")
            }
        };
        assert!(
            error.contains("cannot decide a connection if-equation branch"),
            "non-evaluable selected field `{field}` must fail closed: {error}"
        );
    }
}

#[test]
fn deferred_parameter_and_constant_fields_can_select_connection_topology() {
    for field in [
        "parameter Boolean enabled = true;",
        "constant Boolean enabled = true annotation(Evaluate=false);",
    ] {
        let source = format!(
            r"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            model Settings
                {field}
            end Settings;
            model Plant
                replaceable Settings settings constrainedby Settings;
                Pin a;
                Pin b;
            equation
                if settings.enabled then
                    connect(a, b);
                end if;
            end Plant;
            "
        );

        let overlay = match instantiation_outcome(&source, "Plant") {
            InstantiationOutcome::Success(overlay) => overlay,
            InstantiationOutcome::NeedsInner { missing_inners, .. } => {
                panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
            }
            InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
        };
        assert_eq!(
            connection_paths(&overlay),
            vec![("a".to_string(), "b".to_string())],
            "evaluable selected field `{field}` must decide the branch"
        );
    }
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

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

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

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

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

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

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

    let overlay = match instantiation_outcome(source, "Plant") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };

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
    let tree = resolved.inner().clone();

    let error = match instantiate_model_with_outcome(&tree, "Plant") {
        InstantiationOutcome::Error(error) => error,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Success(_) => panic!("undecidable condition must fail"),
    };

    assert!(
        error.to_string().contains("cage"),
        "error must name the undecided component: {error}"
    );
}
