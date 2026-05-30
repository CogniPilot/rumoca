use super::*;

pub(super) const ER060_FUNCTION_INNER_OUTER_PREFIX: &str = "ER060";
pub(super) const ER061_FUNCTION_CLOCK_COMPONENT: &str = "ER061";
pub(super) const ER062_FUNCTION_INVALID_COMPONENT_TYPE: &str = "ER062";

/// FUNC-001: Public function components must have input or output prefix.
/// FUNC-002: Assignment to function input is forbidden.
/// FUNC-006: Functions cannot have equation sections.
/// FUNC-011: Functions cannot contain Clock-typed components.
/// FUNC-012: Functions cannot use inner/outer prefixes on components.
/// FUNC-015: Functions cannot contain model/block/operator/connector components.
pub(super) fn check_function_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if class.class_type != ClassType::Function {
        return;
    }

    check_function_public_io_prefixes(class, diags);
    check_function_equation_restrictions(class, diags);
    check_function_inner_outer_restrictions(class, diags);
    check_function_input_assignments(class, diags);
    check_function_component_type_restrictions(class, def, diags);
}

fn check_function_public_io_prefixes(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        if !comp.is_protected && matches!(comp.causality, Causality::Empty) {
            diags.push(semantic_error(
                ER013_FUNCTION_PUBLIC_MISSING_IO_PREFIX,
                format!(
                    "public component '{}' in function '{}' must have \
                     input or output prefix (MLS §12.2)",
                    name, class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "check_function_public_io_prefixes/missing_io_prefix",
                    format!("component '{}' is missing input/output prefix", name),
                ),
            ));
        }
    }
}

fn check_function_equation_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.equations.is_empty() && class.initial_equations.is_empty() {
        return;
    }

    let label = if let Some(eq) = class.equations.first() {
        label_from_equation(
            eq,
            "check_function_equation_restrictions/function_equations",
            "equation section is not allowed in functions",
        )
        .unwrap_or_else(|| {
            label_from_token(
                &class.name,
                "check_function_equation_restrictions/function_equations_fallback",
                "equation section is not allowed in functions",
            )
        })
    } else if let Some(eq) = class.initial_equations.first() {
        label_from_equation(
            eq,
            "check_function_equation_restrictions/function_initial_equations",
            "initial equation section is not allowed in functions",
        )
        .unwrap_or_else(|| {
            label_from_token(
                &class.name,
                "check_function_equation_restrictions/function_initial_equations_fallback",
                "initial equation section is not allowed in functions",
            )
        })
    } else {
        label_from_token(
            &class.name,
            "check_function_equation_restrictions/function_equation_section",
            "equation section is not allowed in functions",
        )
    };
    diags.push(semantic_error(
        ER038_FUNCTION_EQUATION_SECTION,
        format!(
            "function '{}' cannot have equation sections (MLS §12.2)",
            class.name.text
        ),
        label,
    ));
}

fn check_function_inner_outer_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        if comp.inner {
            diags.push(semantic_error(
                ER060_FUNCTION_INNER_OUTER_PREFIX,
                format!(
                    "function component '{}' cannot have 'inner' prefix (MLS §12.2)",
                    name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_function_inner_outer_restrictions/inner_prefix",
                    format!("component '{}' cannot be 'inner'", name),
                ),
            ));
        }
        if comp.outer {
            diags.push(semantic_error(
                ER060_FUNCTION_INNER_OUTER_PREFIX,
                format!(
                    "function component '{}' cannot have 'outer' prefix (MLS §12.2)",
                    name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_function_inner_outer_restrictions/outer_prefix",
                    format!("component '{}' cannot be 'outer'", name),
                ),
            ));
        }
    }
}

fn check_function_input_assignments(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let input_names: std::collections::HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Input(_)))
        .map(|(n, _)| n.clone())
        .collect();

    for alg in &class.algorithms {
        check_input_assignment(alg, &input_names, diags);
    }
}

fn check_function_component_type_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        match resolve_component_type_root(comp, def) {
            Some(ResolvedTypeRoot::Builtin("Clock")) => {
                diags.push(semantic_error(
                    ER061_FUNCTION_CLOCK_COMPONENT,
                    format!(
                        "function component '{}' cannot have type '{}' because it resolves to Clock (MLS §12.2)",
                        name, comp.type_name
                    ),
                    label_from_token(
                        &comp.name_token,
                        "check_function_component_type_restrictions/clock_component",
                        format!("Clock-typed function component '{}'", name),
                    ),
                ));
            }
            Some(ResolvedTypeRoot::Class(type_class))
                if matches!(
                    type_class.class_type,
                    ClassType::Model
                        | ClassType::Block
                        | ClassType::Connector
                        | ClassType::Operator
                ) =>
            {
                diags.push(semantic_error(
                    ER062_FUNCTION_INVALID_COMPONENT_TYPE,
                    format!(
                        "function component '{}' cannot have type '{}' because it resolves to a {} (MLS §12.2)",
                        name,
                        comp.type_name,
                        type_class.class_type.as_str()
                    ),
                    label_from_token(
                        &comp.name_token,
                        "check_function_component_type_restrictions/invalid_component_type",
                        format!("invalid function component '{}'", name),
                    ),
                ));
            }
            _ => {}
        }
    }
}

fn check_input_assignment(
    stmts: &[Statement],
    input_names: &std::collections::HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    struct InputAssignmentVisitor<'a> {
        input_names: &'a std::collections::HashSet<String>,
        diags: &'a mut Vec<Diagnostic>,
    }

    impl ast::Visitor for InputAssignmentVisitor<'_> {
        fn visit_assignment(
            &mut self,
            comp: &ComponentReference,
            _value: &Expression,
        ) -> std::ops::ControlFlow<()> {
            if let Some(first) = comp.parts.first()
                && self.input_names.contains(&*first.ident.text)
            {
                self.diags.push(semantic_error(
                    ER014_FUNCTION_INPUT_ASSIGNED,
                    format!(
                        "cannot assign to input parameter '{}' (MLS §12.2)",
                        first.ident.text
                    ),
                    label_from_token(
                        &first.ident,
                        "check_input_assignment/input_assignment",
                        format!("assignment to input '{}'", first.ident.text),
                    ),
                ));
            }
            std::ops::ControlFlow::Continue(())
        }
    }

    let mut visitor = InputAssignmentVisitor { input_names, diags };
    for stmt in stmts {
        let _ = visitor.visit_statement(stmt);
    }
}
