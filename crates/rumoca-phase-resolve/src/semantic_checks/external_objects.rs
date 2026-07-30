//! Resolved-identity checks for the MLS §12.9.7 ExternalObject lifecycle.

use std::ops::ControlFlow;

use rumoca_core::PhaseError;

use super::*;

const ER132_EXTERNAL_OBJECT_LIFECYCLE_SHAPE: &str = "ER132";
const ER133_EXTERNAL_OBJECT_LIFECYCLE_SIGNATURE: &str = "ER133";
const ER134_EXPLICIT_EXTERNAL_OBJECT_LIFECYCLE_CALL: &str = "ER134";

#[derive(Debug, Clone)]
enum ExternalObjectError {
    Shape {
        message: String,
        primary: PrimaryLabel,
    },
    Signature {
        message: String,
        primary: PrimaryLabel,
    },
    ExplicitLifecycleCall {
        message: String,
        primary: PrimaryLabel,
    },
}

impl ExternalObjectError {
    fn shape(message: impl Into<String>, primary: PrimaryLabel) -> Self {
        Self::Shape {
            message: message.into(),
            primary,
        }
    }

    fn signature(message: impl Into<String>, primary: PrimaryLabel) -> Self {
        Self::Signature {
            message: message.into(),
            primary,
        }
    }

    fn explicit_lifecycle_call(message: impl Into<String>, primary: PrimaryLabel) -> Self {
        Self::ExplicitLifecycleCall {
            message: message.into(),
            primary,
        }
    }

    fn emit(self, diagnostics: &mut Vec<Diagnostic>) {
        diagnostics.push(self.to_diagnostic());
    }
}

impl PhaseError for ExternalObjectError {
    fn to_diagnostic(&self) -> Diagnostic {
        let (code, message, primary) = match self {
            Self::Shape { message, primary } => {
                (ER132_EXTERNAL_OBJECT_LIFECYCLE_SHAPE, message, primary)
            }
            Self::Signature { message, primary } => {
                (ER133_EXTERNAL_OBJECT_LIFECYCLE_SIGNATURE, message, primary)
            }
            Self::ExplicitLifecycleCall { message, primary } => (
                ER134_EXPLICIT_EXTERNAL_OBJECT_LIFECYCLE_CALL,
                message,
                primary,
            ),
        };
        Diagnostic::error(code, message.clone(), primary.clone())
    }
}

pub(super) fn run_external_object_checks(tree: &ast::ClassTree) -> Vec<Diagnostic> {
    let index = ast::ClassDefIndex::from_tree(tree);
    let mut diagnostics = Vec::new();
    let mut lifecycle_roles = ast::AstIndexMap::default();
    let mut missing_builtin_reported = false;
    for class in tree.definitions.classes.values() {
        check_class_tree(
            class,
            &index,
            &mut lifecycle_roles,
            &mut missing_builtin_reported,
            &mut diagnostics,
        );
    }
    reject_explicit_lifecycle_calls(tree, &lifecycle_roles, &mut diagnostics);
    diagnostics
}

fn check_class_tree(
    class: &ClassDef,
    index: &ast::ClassDefIndex<'_>,
    lifecycle_roles: &mut ast::AstIndexMap<DefId, ast::ExternalObjectLifecycleRole>,
    missing_builtin_reported: &mut bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_external_object_owner(
        index,
        class,
        lifecycle_roles,
        missing_builtin_reported,
        diagnostics,
    );
    for nested in class.classes.values() {
        check_class_tree(
            nested,
            index,
            lifecycle_roles,
            missing_builtin_reported,
            diagnostics,
        );
    }
}

fn check_external_object_owner(
    index: &ast::ClassDefIndex<'_>,
    owner: &ClassDef,
    lifecycle_roles: &mut ast::AstIndexMap<DefId, ast::ExternalObjectLifecycleRole>,
    missing_builtin_reported: &mut bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(owner_def_id) = owner.def_id else {
        // This non-authoritative recognizer also sees partial planning trees.
        // Without a declaration identity the class cannot be classified as an
        // ExternalObject owner; registration diagnostics own that failure.
        return;
    };
    match index.external_object_lifecycle(owner_def_id) {
        Ok(Some(lifecycle)) => {
            lifecycle_roles.insert(
                lifecycle.constructor_def_id(),
                ast::ExternalObjectLifecycleRole::Constructor,
            );
            lifecycle_roles.insert(
                lifecycle.destructor_def_id(),
                ast::ExternalObjectLifecycleRole::Destructor,
            );
            check_lifecycle_shape(&lifecycle, diagnostics);
            check_lifecycle_signatures(&lifecycle, diagnostics);
        }
        Ok(None) => {}
        Err(error) => {
            if matches!(
                error,
                ast::ExternalObjectLifecycleError::MissingBuiltinIdentity { .. }
            ) {
                if *missing_builtin_reported {
                    return;
                }
                *missing_builtin_reported = true;
            }
            emit_lifecycle_query_error(owner, error, diagnostics);
        }
    }
}

fn emit_lifecycle_query_error(
    fallback_owner: &ClassDef,
    error: ast::ExternalObjectLifecycleError<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    use ast::ExternalObjectLifecycleError as Error;
    let error = match error {
        Error::IndirectExternalObjectExtension {
            owner,
            direct_base_def_id,
            ..
        } => indirect_extension_diagnostic(owner, direct_base_def_id),
        Error::MissingLifecycleFunction { owner, role, .. } => {
            missing_lifecycle_diagnostic(owner, role)
        }
        Error::LifecycleMemberIsNotFunction {
            owner,
            role,
            member,
            ..
        } => lifecycle_restriction_diagnostic(owner, role, member),
        Error::UnresolvedBaseIdentity { .. } => {
            // The same incomplete Resolve attempt already owns ER003/ER004.
            // Without a resolved base identity this read-only recognizer cannot
            // prove that the class is an ExternalObject owner.
            return;
        }
        Error::MissingBuiltinIdentity { owner_def_id } => ExternalObjectError::shape(
            format!(
                "cannot verify ExternalObject lifecycle for {owner_def_id:?}: \
                 predefined ExternalObject identity is missing"
            ),
            label_from_token(
                &fallback_owner.name,
                "external_object/missing_builtin_identity",
                "predefined ExternalObject identity is unavailable",
            ),
        ),
        Error::MissingOwner { owner_def_id } => ExternalObjectError::shape(
            format!("cannot verify ExternalObject lifecycle: owner {owner_def_id:?} is missing"),
            label_from_token(
                &fallback_owner.name,
                "external_object/missing_owner",
                "ExternalObject owner is absent from the declaration index",
            ),
        ),
        Error::MissingBase {
            owner, base_def_id, ..
        } => ExternalObjectError::shape(
            format!(
                "cannot verify ExternalObject lifecycle for '{}': resolved base \
                 {base_def_id:?} is missing",
                owner.name.text
            ),
            label_from_token(
                &owner.name,
                "external_object/missing_base",
                "ExternalObject base identity is incomplete",
            ),
        ),
        Error::MissingLifecycleIdentity {
            owner,
            role,
            member,
            ..
        } => ExternalObjectError::shape(
            format!(
                "cannot verify {} in ExternalObject class '{}': declaration identity is missing",
                role.source_name(),
                owner.name.text
            ),
            label_from_token(
                &member.name,
                "external_object/missing_lifecycle_identity",
                "lifecycle declaration identity is incomplete",
            ),
        ),
    };
    error.emit(diagnostics);
}

fn indirect_extension_diagnostic(
    owner: &ClassDef,
    direct_base_def_id: DefId,
) -> ExternalObjectError {
    let primary = owner
        .extends
        .iter()
        .find(|extend| extend.base_def_id == Some(direct_base_def_id))
        .and_then(|extend| {
            label_from_location(
                &extend.location,
                "external_object/indirect_extension",
                "ExternalObject must be the direct base",
            )
        })
        .unwrap_or_else(|| {
            label_from_token(
                &owner.name,
                "external_object/indirect_extension_owner",
                "ExternalObject must be the direct base",
            )
        });
    ExternalObjectError::shape(
        format!(
            "ExternalObject class '{}' must directly extend ExternalObject (MLS §12.9.7)",
            owner.name.text
        ),
        primary,
    )
}

fn missing_lifecycle_diagnostic(
    owner: &ClassDef,
    role: ast::ExternalObjectLifecycleRole,
) -> ExternalObjectError {
    if owner.end_name_token.is_none() {
        return ExternalObjectError::shape(
            format!(
                "ExternalObject '{}' must not use a short-class definition (MLS §12.9.7)",
                owner.name.text
            ),
            label_from_token(
                &owner.name,
                "external_object/short_class",
                "define the ExternalObject as a long-form class",
            ),
        );
    }
    ExternalObjectError::shape(
        format!(
            "ExternalObject class '{}' is missing {} function (MLS §12.9.7)",
            owner.name.text,
            role.source_name()
        ),
        label_from_token(
            &owner.name,
            "external_object/missing_lifecycle",
            format!("missing {}", role.source_name()),
        ),
    )
}

fn lifecycle_restriction_diagnostic(
    owner: &ClassDef,
    role: ast::ExternalObjectLifecycleRole,
    member: &ClassDef,
) -> ExternalObjectError {
    ExternalObjectError::shape(
        format!(
            "{} must be a function in ExternalObject class '{}' (MLS §12.9.7)",
            role.source_name(),
            owner.name.text
        ),
        label_from_token(
            &member.name,
            "external_object/lifecycle_restriction",
            format!("{} must be a function", role.source_name()),
        ),
    )
}

fn check_lifecycle_shape(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let owner = lifecycle.owner();
    if owner.class_type != ClassType::Class {
        ExternalObjectError::shape(
            format!(
                "ExternalObject owner '{}' must use the specialized class restriction (MLS §12.9.7)",
                owner.name.text
            ),
            label_from_token(
                &owner.class_type_token,
                "external_object/owner_restriction",
                "use `class` for an ExternalObject owner",
            ),
        )
        .emit(diagnostics);
    }
    if owner.extends.len() != 1 {
        let primary = owner.extends.get(1).map_or_else(
            || {
                label_from_token(
                    &owner.name,
                    "external_object/direct_base_count",
                    "exactly one direct ExternalObject base is required",
                )
            },
            |extend| {
                label_from_location(
                    &extend.location,
                    "external_object/extra_base",
                    "additional base is not allowed",
                )
                .unwrap_or_else(|| {
                    label_from_token(
                        &owner.name,
                        "external_object/extra_base_owner",
                        "additional base is not allowed",
                    )
                })
            },
        );
        ExternalObjectError::shape(
            format!(
                "ExternalObject class '{}' must have exactly one direct base (MLS §12.9.7)",
                owner.name.text
            ),
            primary,
        )
        .emit(diagnostics);
    }

    check_lifecycle_replaceability(lifecycle, diagnostics);
    check_other_owner_elements(lifecycle, diagnostics);
}

fn check_lifecycle_replaceability(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for function in [lifecycle.constructor(), lifecycle.destructor()] {
        if function.partial {
            ExternalObjectError::shape(
                format!(
                    "ExternalObject {} function in '{}' must not be partial (MLS §12.9.7)",
                    function.name.text,
                    lifecycle.owner().name.text
                ),
                label_from_token(
                    &function.name,
                    "external_object/partial_lifecycle",
                    "lifecycle functions must be complete",
                ),
            )
            .emit(diagnostics);
        }
        if function.is_replaceable {
            ExternalObjectError::shape(
                format!(
                    "ExternalObject {} function in '{}' must not be replaceable (MLS §12.9.7)",
                    function.name.text,
                    lifecycle.owner().name.text
                ),
                label_from_token(
                    &function.name,
                    "external_object/replaceable_lifecycle",
                    "lifecycle functions must not be replaceable",
                ),
            )
            .emit(diagnostics);
        }
    }
}

fn check_other_owner_elements(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let owner = lifecycle.owner();
    for nested in owner.classes.values() {
        let Some(nested_def_id) = nested.def_id else {
            ExternalObjectError::shape(
                format!(
                    "cannot verify nested class '{}' in ExternalObject '{}': declaration identity is missing",
                    nested.name.text, owner.name.text
                ),
                label_from_token(
                    &nested.name,
                    "external_object/missing_nested_identity",
                    "nested declaration identity is incomplete",
                ),
            )
            .emit(diagnostics);
            continue;
        };
        if nested_def_id != lifecycle.constructor_def_id()
            && nested_def_id != lifecycle.destructor_def_id()
        {
            push_other_element(
                owner,
                format!("class '{}'", nested.name.text),
                label_from_token(
                    &nested.name,
                    "external_object/extra_class",
                    "other elements are not allowed",
                ),
                diagnostics,
            );
        }
    }
    for (name, component) in &owner.components {
        push_other_element(
            owner,
            format!("component '{name}'"),
            label_from_token(
                &component.name_token,
                "external_object/extra_component",
                "other elements are not allowed",
            ),
            diagnostics,
        );
    }
    for import in &owner.imports {
        let primary = label_from_location(
            import.location(),
            "external_object/extra_import",
            "imports are not allowed",
        )
        .unwrap_or_else(|| {
            label_from_token(
                &owner.name,
                "external_object/extra_import_owner",
                "imports are not allowed",
            )
        });
        push_other_element(owner, "an import".to_string(), primary, diagnostics);
    }
    check_other_owner_sections(owner, diagnostics);
}

fn check_other_owner_sections(owner: &ClassDef, diagnostics: &mut Vec<Diagnostic>) {
    for equation in owner.equations.iter().chain(owner.initial_equations.iter()) {
        let primary = label_from_equation(
            equation,
            "external_object/extra_equation",
            "equations are not allowed",
        )
        .unwrap_or_else(|| {
            label_from_token(
                &owner.name,
                "external_object/extra_equation_owner",
                "equations are not allowed",
            )
        });
        push_other_element(owner, "an equation".to_string(), primary, diagnostics);
    }
    for statement in owner
        .algorithms
        .iter()
        .chain(owner.initial_algorithms.iter())
        .flatten()
    {
        let primary = label_from_statement(
            statement,
            "external_object/extra_algorithm",
            "algorithms are not allowed",
        )
        .unwrap_or_else(|| {
            label_from_token(
                &owner.name,
                "external_object/extra_algorithm_owner",
                "algorithms are not allowed",
            )
        });
        push_other_element(owner, "an algorithm".to_string(), primary, diagnostics);
    }
    for literal in &owner.enum_literals {
        push_other_element(
            owner,
            format!("enumeration literal '{}'", literal.ident.text),
            label_from_token(
                &literal.ident,
                "external_object/extra_enum_literal",
                "enumeration literals are not allowed",
            ),
            diagnostics,
        );
    }
}

fn push_other_element(
    owner: &ClassDef,
    element: String,
    primary: PrimaryLabel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    ExternalObjectError::shape(
        format!(
            "ExternalObject class '{}' cannot contain {element} (MLS §12.9.7)",
            owner.name.text
        ),
        primary,
    )
    .emit(diagnostics);
}

fn check_lifecycle_signatures(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_constructor_signature(lifecycle, diagnostics);
    check_destructor_signature(lifecycle, diagnostics);
}

fn check_constructor_signature(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let outputs = lifecycle
        .constructor()
        .components
        .values()
        .filter(|component| matches!(component.causality, Causality::Output(_)));
    let output = match exactly_one_component(outputs) {
        Ok(output) => output,
        Err(extra) => {
            let primary = extra.map_or_else(
                || {
                    label_from_token(
                        &lifecycle.constructor().name,
                        "external_object/constructor_output_count",
                        "exactly one output is required",
                    )
                },
                |component| {
                    label_from_token(
                        &component.name_token,
                        "external_object/constructor_extra_output",
                        "additional output is not allowed",
                    )
                },
            );
            ExternalObjectError::signature(
                format!(
                    "ExternalObject constructor in '{}' must declare exactly one output (MLS §12.9.7)",
                    lifecycle.owner().name.text
                ),
                primary,
            )
            .emit(diagnostics);
            return;
        }
    };
    check_role_component_type(lifecycle, output, "constructor output", diagnostics);
}

fn check_destructor_signature(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let inputs = lifecycle
        .destructor()
        .components
        .values()
        .filter(|component| matches!(component.causality, Causality::Input(_)));
    match exactly_one_component(inputs) {
        Ok(input) => {
            check_role_component_type(lifecycle, input, "destructor input", diagnostics);
        }
        Err(extra) => {
            let primary = extra.map_or_else(
                || {
                    label_from_token(
                        &lifecycle.destructor().name,
                        "external_object/destructor_input_count",
                        "exactly one input is required",
                    )
                },
                |component| {
                    label_from_token(
                        &component.name_token,
                        "external_object/destructor_extra_input",
                        "additional input is not allowed",
                    )
                },
            );
            ExternalObjectError::signature(
                format!(
                    "ExternalObject destructor in '{}' must declare exactly one input (MLS §12.9.7)",
                    lifecycle.owner().name.text
                ),
                primary,
            )
            .emit(diagnostics);
        }
    }

    for output in lifecycle
        .destructor()
        .components
        .values()
        .filter(|component| matches!(component.causality, Causality::Output(_)))
    {
        ExternalObjectError::signature(
            format!(
                "ExternalObject destructor in '{}' must not declare outputs (MLS §12.9.7)",
                lifecycle.owner().name.text
            ),
            label_from_token(
                &output.name_token,
                "external_object/destructor_output",
                "destructor output is not allowed",
            ),
        )
        .emit(diagnostics);
    }
}

fn exactly_one_component<'a>(
    mut components: impl Iterator<Item = &'a ast::Component>,
) -> Result<&'a ast::Component, Option<&'a ast::Component>> {
    let Some(first) = components.next() else {
        return Err(None);
    };
    match components.next() {
        Some(extra) => Err(Some(extra)),
        None => Ok(first),
    }
}

fn check_role_component_type(
    lifecycle: &ast::ExternalObjectLifecycle<'_>,
    component: &ast::Component,
    role: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(subscript) = component.shape_expr.first() {
        ExternalObjectError::signature(
            format!(
                "ExternalObject {role} in '{}' must be scalar (MLS §12.9.7)",
                lifecycle.owner().name.text
            ),
            lifecycle_dimension_label(component, subscript, role),
        )
        .emit(diagnostics);
        return;
    }
    if !component.shape.is_empty() {
        ExternalObjectError::signature(
            format!(
                "ExternalObject {role} in '{}' has a non-scalar semantic shape (MLS §12.9.7)",
                lifecycle.owner().name.text
            ),
            label_from_token(
                &component.name_token,
                "external_object/lifecycle_semantic_shape",
                format!("{role} must have scalar source and semantic shapes"),
            ),
        )
        .emit(diagnostics);
        return;
    }
    let Some(type_def_id) = component.type_def_id else {
        // This recognizer also runs on a partial Resolve planning tree.
        // Unresolved type identity is already owned by ER002 and cannot be
        // classified as a lifecycle signature mismatch yet.
        return;
    };
    if type_def_id == lifecycle.owner_def_id() {
        return;
    }
    ExternalObjectError::signature(
        format!(
            "ExternalObject {role} must have type '{}' (MLS §12.9.7)",
            lifecycle.owner().name.text
        ),
        lifecycle_type_label(
            component,
            "external_object/lifecycle_component_type",
            format!("{role} must have the owning ExternalObject type"),
        ),
    )
    .emit(diagnostics);
}

fn lifecycle_dimension_label(
    component: &ast::Component,
    subscript: &ast::Subscript,
    role: &str,
) -> PrimaryLabel {
    let message = format!("{role} cannot have array dimensions");
    match subscript {
        ast::Subscript::Expression(expression) => {
            PrimaryLabel::new(expression.span()).with_message(message)
        }
        ast::Subscript::Range { token } => {
            label_from_token(token, "external_object/lifecycle_range_dimension", message)
        }
        ast::Subscript::Empty => label_from_token(
            &component.name_token,
            "external_object/lifecycle_empty_dimension",
            message,
        ),
    }
}

fn lifecycle_type_label(
    component: &ast::Component,
    context: &str,
    message: impl Into<String>,
) -> PrimaryLabel {
    let Some(first) = component.type_name.name.first() else {
        return label_from_token(&component.name_token, context, message);
    };
    let last = component.type_name.name.last().unwrap_or(first);
    if first.location.source != last.location.source {
        return label_from_token(first, context, message);
    }
    PrimaryLabel::new(Span::from_offsets(
        first.location.source,
        first.location.start as usize,
        (last.location.end as usize).max(first.location.start as usize + 1),
    ))
    .with_message(message)
}

fn reject_explicit_lifecycle_calls(
    tree: &ast::ClassTree,
    lifecycle_roles: &ast::AstIndexMap<DefId, ast::ExternalObjectLifecycleRole>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut visitor = ExplicitLifecycleCallVisitor {
        lifecycle_roles,
        diagnostics,
    };
    let _ = visitor.visit_stored_definition(&tree.definitions);
}

struct ExplicitLifecycleCallVisitor<'a> {
    lifecycle_roles: &'a ast::AstIndexMap<DefId, ast::ExternalObjectLifecycleRole>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl Visitor for ExplicitLifecycleCallVisitor<'_> {
    fn enter_expr_function_call(
        &mut self,
        target: &ComponentReference,
        _args: &[Expression],
        _context: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        let Some((_, role)) = target
            .def_id
            .and_then(|def_id| self.lifecycle_roles.get_key_value(&def_id))
        else {
            return ControlFlow::Continue(());
        };
        ExternalObjectError::explicit_lifecycle_call(
            format!(
                "ExternalObject {} cannot be called explicitly (MLS §12.9.7)",
                role.source_name()
            ),
            PrimaryLabel::new(target.span).with_message(format!(
                "{} is invoked only by ExternalObject lifetime semantics",
                role.source_name()
            )),
        )
        .emit(self.diagnostics);
        ControlFlow::Continue(())
    }
}
