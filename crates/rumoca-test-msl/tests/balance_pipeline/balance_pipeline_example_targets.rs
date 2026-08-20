use super::*;
use rumoca_compile::parsing::ast;

// =============================================================================
// MSL example target discovery
// =============================================================================

pub(super) fn is_explicit_msl_example_model(model_name: &str) -> bool {
    model_name.starts_with("Modelica.") && model_name.contains(".Examples.")
}

/// Package segments that indicate support/helper classes within Examples trees.
pub(super) const EXAMPLE_SUPPORT_SEGMENTS: &[&str] =
    &["Utilities", "BaseClasses", "Internal", "Interfaces"];
pub(super) const ROOT_MSL_EXAMPLE_SELECTION_PATTERN: &str = "Modelica.*.Examples.*";

/// Root MSL examples by name:
/// - under `Modelica.*.Examples.*`
/// - not nested under support/helper package segments
pub(super) fn is_root_msl_example_model_name(model_name: &str) -> bool {
    if !is_explicit_msl_example_model(model_name) {
        return false;
    }
    let Some((_, suffix)) = model_name.split_once(".Examples.") else {
        return false;
    };
    let mut segments: Vec<&str> = rumoca_compile::compile::core::split_path_with_indices(suffix);
    if segments.len() <= 1 {
        return true;
    }
    // Exclude class name itself; only inspect package path under Examples.
    let _ = segments.pop();
    !segments
        .iter()
        .any(|seg| EXAMPLE_SUPPORT_SEGMENTS.contains(seg))
}

/// Root standalone MSL examples:
/// - root example by package name
/// - non-partial
/// - no top-level input scalars requiring external bindings
/// - no unbound fixed-parameter scalars (fixed=true by default for parameters)
pub(super) fn is_root_standalone_msl_example_model(
    model_name: &str,
    result: &rumoca_compile::compile::CompilationResult,
) -> bool {
    is_root_msl_example_model_name(model_name)
        && !result.flat.is_partial
        && !checked_dae_has_input_scalars(&result.dae)
        && !result.flat.has_unbound_fixed_parameters()
}

pub(super) fn is_root_standalone_msl_example_dae_model(
    model_name: &str,
    result: &rumoca_compile::compile::DaeCompilationResult,
) -> bool {
    is_root_msl_example_model_name(model_name)
        && !result.flat.is_partial
        && !checked_dae_has_input_scalars(result.dae.as_ref())
        && !result.has_unbound_fixed_parameters
}

struct ClassPathFrame<'a> {
    name: &'a str,
    class_type: &'a rumoca_compile::compile::core::ClassType,
}

pub(super) fn root_msl_example_model_names(
    tree: &ast::ClassTree,
    model_names: &[String],
) -> Vec<String> {
    let model_name_set: HashSet<&str> = model_names.iter().map(String::as_str).collect();
    let mut selected = Vec::new();
    let mut path = Vec::new();
    for (name, class) in &tree.definitions.classes {
        collect_root_example_model(name, class, "", &model_name_set, &mut path, &mut selected);
    }
    selected.sort();
    selected
}

fn collect_root_example_model<'a>(
    name: &'a str,
    class: &'a ast::ClassDef,
    prefix: &str,
    model_names: &HashSet<&str>,
    path: &mut Vec<ClassPathFrame<'a>>,
    selected: &mut Vec<String>,
) {
    let full_name = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    };
    path.push(ClassPathFrame {
        name,
        class_type: &class.class_type,
    });
    if model_names.contains(full_name.as_str()) && is_root_msl_example_path(path) {
        selected.push(full_name.clone());
    }
    for (child_name, child_class) in &class.classes {
        collect_root_example_model(
            child_name,
            child_class,
            &full_name,
            model_names,
            path,
            selected,
        );
    }
    let _ = path.pop();
}

fn is_root_msl_example_path(path: &[ClassPathFrame<'_>]) -> bool {
    let Some(examples_index) = path.iter().position(|frame| frame.name == "Examples") else {
        return false;
    };
    let Some(root) = path.first() else {
        return false;
    };
    if root.name != "Modelica" || examples_index + 1 >= path.len() {
        return false;
    }
    let package_path = &path[examples_index + 1..path.len() - 1];
    if package_path
        .iter()
        .any(|frame| EXAMPLE_SUPPORT_SEGMENTS.contains(&frame.name))
    {
        return false;
    }
    package_path.iter().all(|frame| {
        matches!(
            frame.class_type,
            rumoca_compile::compile::core::ClassType::Package
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::compile::Session;

    fn class(name: &str, class_type: rumoca_compile::compile::core::ClassType) -> ast::ClassDef {
        ast::ClassDef {
            name: rumoca_compile::compile::core::Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            class_type,
            ..Default::default()
        }
    }

    #[test]
    fn root_example_selection_excludes_models_nested_inside_models() {
        let mut tree = ast::ClassTree::new();
        let mut modelica = class(
            "Modelica",
            rumoca_compile::compile::core::ClassType::Package,
        );
        let mut mechanics = class(
            "Mechanics",
            rumoca_compile::compile::core::ClassType::Package,
        );
        let mut examples = class(
            "Examples",
            rumoca_compile::compile::core::ClassType::Package,
        );
        let mut elementary = class(
            "Elementary",
            rumoca_compile::compile::core::ClassType::Package,
        );
        let mut root_model = class(
            "PointGravityWithPointMasses2",
            rumoca_compile::compile::core::ClassType::Model,
        );
        root_model.classes.insert(
            "SystemWithStandardBodies".to_string(),
            class(
                "SystemWithStandardBodies",
                rumoca_compile::compile::core::ClassType::Model,
            ),
        );
        elementary
            .classes
            .insert("PointGravityWithPointMasses2".to_string(), root_model);
        examples
            .classes
            .insert("Elementary".to_string(), elementary);
        mechanics.classes.insert("Examples".to_string(), examples);
        modelica.classes.insert("Mechanics".to_string(), mechanics);
        tree.definitions
            .classes
            .insert("Modelica".to_string(), modelica);

        let names = vec![
            "Modelica.Mechanics.Examples.Elementary.PointGravityWithPointMasses2".to_string(),
            "Modelica.Mechanics.Examples.Elementary.PointGravityWithPointMasses2.SystemWithStandardBodies"
                .to_string(),
        ];

        assert_eq!(
            root_msl_example_model_names(&tree, &names),
            vec!["Modelica.Mechanics.Examples.Elementary.PointGravityWithPointMasses2"]
        );
    }

    /// Assert the checked DAE retained `name` under `role` carrying no scalars.
    fn assert_retained_zero_cardinality_dae_variable(
        dae: &Dae,
        name: &str,
        role: rumoca_ir_dae::VariableRole,
    ) {
        let retained = dae.inspect(|view| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .map(|(_, variable)| (variable.role(), variable.scalar_count()))
        });
        let Some((retained_role, scalar_count)) = retained else {
            panic!("the DAE must retain the zero-cardinality declaration `{name}`");
        };
        assert_eq!(retained_role, role);
        assert_eq!(
            scalar_count, 0,
            "a zero-cardinality `{name}` contributes no scalar unknown"
        );
    }

    /// MLS 3.6 §10.1: "Zero-valued dimensions are allowed, so: `C x[0];`
    /// declares an empty vector" (§10.7 calls these empty arrays). §4.7
    /// "Balanced Models" counts "the elements after expanding all records,
    /// operator record, and arrays to a set of scalars of primitive types", so
    /// an empty array contributes no unknown and no equation.
    ///
    /// The declaration is therefore retained (Flat keeps `u` with `dims == [0]`,
    /// the DAE keeps an `Input` role with `scalar_count() == 0`) and every
    /// scalar-weighted count stays zero. Retaining it is what lets a model with
    /// `input Real u[0]` still qualify as standalone: the standalone predicate
    /// asks whether any input *scalar* needs an external binding, not whether an
    /// input declaration exists.
    #[test]
    fn zero_sized_inputs_and_parameters_are_retained_and_stay_standalone() {
        let mut session = Session::default();
        session
            .add_document(
                "EmptyBindings.mo",
                r#"
                    model EmptyBindings
                      input Real u[0];
                      parameter Real p[0];
                      Real x(start = 1);
                    equation
                      der(x) = -x;
                    end EmptyBindings;
                "#,
            )
            .expect("parse zero-sized standalone model");
        let result = session
            .compile_model_dae_strict_reachable_uncached_with_recovery("EmptyBindings")
            .expect("compile zero-sized standalone model");

        let counts = checked_dae_counts(result.dae.as_ref());
        for name in ["u", "p"] {
            let declaration = result
                .flat
                .variables
                .get(&rumoca_compile::compile::core::VarName::new(name))
                .unwrap_or_else(|| {
                    panic!("Flat must retain the zero-cardinality declaration `{name}`")
                });
            assert_eq!(
                declaration.dims,
                [0],
                "`{name}` must be retained at its declared zero cardinality"
            );
        }
        for (name, role) in [
            ("u", rumoca_ir_dae::VariableRole::Input),
            ("p", rumoca_ir_dae::VariableRole::Parameter),
        ] {
            assert_retained_zero_cardinality_dae_variable(result.dae.as_ref(), name, role);
        }
        // `*_variables` counts declarations, `*_scalars` counts scalars; each
        // retained zero-sized declaration is one declaration carrying no scalars.
        assert_eq!(counts.input_variables, 1);
        assert_eq!(counts.input_scalars, 0);
        assert_eq!(counts.parameter_variables, 1);
        assert_eq!(counts.parameter_scalars, 0);
        // Balance accounting is scalar-weighted (MLS §4.7), so the retained
        // declaration cannot inflate it.
        assert!(!checked_dae_has_input_scalars(result.dae.as_ref()));
        assert!(!result.has_unbound_fixed_parameters);
        assert!(is_root_standalone_msl_example_dae_model(
            "Modelica.Test.Examples.EmptyBindings",
            &result
        ));
    }
}
