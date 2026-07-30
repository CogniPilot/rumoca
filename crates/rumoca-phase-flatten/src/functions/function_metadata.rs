use super::*;
use crate::source_spans::required_location_span;
use std::ops::ControlFlow;

#[derive(Clone, Copy)]
pub(super) struct FunctionExpressionContext<'types> {
    pub(super) predefined_intrinsics: ast_lower::PredefinedIntrinsicIds,
    pub(super) type_catalog: FunctionTypeCatalog<'types>,
}

#[derive(Clone, Copy)]
pub(crate) struct FunctionTypeCatalog<'types> {
    type_ids_by_def_id: &'types flat::TypeIdentityMap,
    type_roots: &'types ast::AstIndexMap<rumoca_core::TypeId, rumoca_core::TypeId>,
}

impl<'types> FunctionTypeCatalog<'types> {
    pub(crate) fn new(overlay: &'types ast::InstanceOverlay) -> Self {
        Self {
            type_ids_by_def_id: &overlay.type_ids_by_def_id,
            type_roots: &overlay.type_roots,
        }
    }

    fn effective_type(
        self,
        component: &ast::Component,
        type_def_id: Option<rumoca_core::DefId>,
        dimensions: Vec<i64>,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::EffectiveType, FlattenError> {
        let type_def_id = type_def_id.ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                &component.name,
                "function value type declaration identity",
                span,
            )
        })?;
        let nominal_type = self
            .type_ids_by_def_id
            .get(&type_def_id)
            .copied()
            .ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    &component.name,
                    "function value nominal TypeId",
                    span,
                )
            })?;
        let canonical_type = self.type_roots.get(&nominal_type).copied().ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                &component.name,
                "function value canonical TypeId",
                span,
            )
        })?;
        rumoca_core::EffectiveType::new(nominal_type, canonical_type, dimensions).map_err(|error| {
            FlattenError::missing_resolved_class_metadata(
                &component.name,
                format!("checked function value type: {error}"),
                span,
            )
        })
    }
}

pub(super) fn effective_function_param_class_type(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
) -> rumoca_core::ClassType {
    const MAX_ALIAS_DEPTH: usize = 32;
    let mut current = class_def;
    let mut visited = HashSet::new();

    for _ in 0..MAX_ALIAS_DEPTH {
        if current.class_type != rumoca_core::ClassType::Type {
            return current.class_type.clone();
        }
        if let Some(def_id) = current.def_id
            && !visited.insert(def_id)
        {
            break;
        }
        let Some(base) = current.extends.first() else {
            break;
        };
        let base_name = base.base_name.to_string();
        if rumoca_core::is_builtin_type(&base_name) {
            break;
        }
        let Some(base_class) = class_by_name_or_def_id(class_index, &base_name, base.base_def_id)
        else {
            break;
        };
        current = base_class;
    }

    class_def.class_type.clone()
}

/// The class a component's declared type names, together with that class's
/// declaration identity.
///
/// `class_def` is absent for the predefined types, which have a `DefId` but no
/// source class declaration.
#[derive(Clone, Copy)]
pub(super) struct ComponentTypeIdentity<'tree> {
    pub(super) def_id: Option<rumoca_core::DefId>,
    pub(super) class_def: Option<&'tree ast::ClassDef>,
}

/// Resolve the exact class identity a component's declared type names.
///
/// [`ast::Name::def_id`] is allowed to hold a *partial* resolution: for a
/// qualified type name whose first segment is a replaceable or aliased package,
/// name resolution records that first segment's class and leaves the trailing
/// segments for a later phase (MLS §7.3). Taking that partial identity as the
/// component's type silently names the enclosing package instead of the
/// declared class, so the trailing segments are completed here through the
/// declared members and inherited scopes of the partially resolved class.
pub(super) fn component_type_identity<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    component: &ast::Component,
) -> ComponentTypeIdentity<'tree> {
    if let Some(def_id) = component.type_def_id {
        return ComponentTypeIdentity {
            def_id: Some(def_id),
            class_def: class_index.get(def_id),
        };
    }
    let Some(def_id) = component.type_name.def_id else {
        let class_def = class_index.get_by_qualified_name(&component.type_name.to_string());
        return ComponentTypeIdentity {
            def_id: class_def.and_then(|class_def| class_def.def_id),
            class_def,
        };
    };
    let Some(class_def) = class_index.get(def_id) else {
        return ComponentTypeIdentity {
            def_id: Some(def_id),
            class_def: None,
        };
    };
    match complete_partial_type_resolution(class_index, component, def_id, class_def) {
        Some(completed) => ComponentTypeIdentity {
            def_id: completed.def_id,
            class_def: Some(completed),
        },
        None => ComponentTypeIdentity {
            def_id: Some(def_id),
            class_def: Some(class_def),
        },
    }
}

/// Complete a qualified type name whose recorded identity covers only its first
/// segment, returning `None` when the recorded identity is not such a prefix.
fn complete_partial_type_resolution<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    component: &ast::Component,
    def_id: rumoca_core::DefId,
    class_def: &'tree ast::ClassDef,
) -> Option<&'tree ast::ClassDef> {
    let segments = component
        .type_name
        .name
        .iter()
        .map(|token| token.text.as_ref())
        .collect::<Vec<&str>>();
    let (first, trailing) = segments.split_first()?;
    if trailing.is_empty() || class_index.local_name(def_id)? != *first {
        return None;
    }
    let written_name = component.type_name.to_string();
    if class_index.qualified_name(def_id)?.ends_with(&written_name) {
        // The recorded class already is the written path's leaf.
        return None;
    }
    trailing.iter().try_fold(class_def, |scope, segment| {
        nested_class_in_scope(class_index, scope, segment)
    })
}

/// Look up `name` as a class declared by `scope` or by one of the classes it
/// extends, including the short-class-definition alias chain that a package
/// alias such as `package Rotation = Quaternion` records as its base class.
fn nested_class_in_scope<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    scope: &'a ast::ClassDef,
    name: &str,
) -> Option<&'a ast::ClassDef> {
    let mut visited = HashSet::new();
    nested_class_in_scope_inner(class_index, scope, name, &mut visited)
}

fn nested_class_in_scope_inner<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    scope: &'a ast::ClassDef,
    name: &str,
    visited: &mut HashSet<usize>,
) -> Option<&'a ast::ClassDef> {
    if !visited.insert(scope as *const ast::ClassDef as usize) {
        return None;
    }
    if let Some(class_def) = scope.classes.get(name) {
        return Some(class_def);
    }
    scope.extends.iter().find_map(|ext| {
        let base_name = ext.base_name.to_string();
        let base = class_by_name_or_def_id(class_index, &base_name, ext.base_def_id)?;
        nested_class_in_scope_inner(class_index, base, name, visited)
    })
}

fn primitive_type_name(name: &str) -> Option<&'static str> {
    match name {
        "Real" => Some("Real"),
        "Integer" => Some("Integer"),
        "Boolean" => Some("Boolean"),
        "String" => Some("String"),
        _ => None,
    }
}

fn effective_function_param_primitive_type(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
    declared_name: &str,
) -> Option<&'static str> {
    if let Some(name) = primitive_type_name(declared_name) {
        return Some(name);
    }
    let mut current = component
        .type_def_id
        .or(component.type_name.def_id)
        .and_then(|def_id| class_index.get(def_id));
    let mut visited = HashSet::new();
    const MAX_ALIAS_DEPTH: usize = 32;
    for _ in 0..MAX_ALIAS_DEPTH {
        let class = current?;
        if let Some(def_id) = class.def_id
            && !visited.insert(def_id)
        {
            return None;
        }
        let base = class.extends.first()?;
        let base_name = base.base_name.to_string();
        if let Some(name) = primitive_type_name(&base_name) {
            return Some(name);
        }
        current = class_by_name_or_def_id(class_index, &base_name, base.base_def_id);
    }
    None
}

/// Convert an AST ExternalFunction to ExternalFunction.
pub(super) fn convert_external_function(
    ext: &rumoca_ir_ast::ExternalFunction,
    predefined_intrinsics: ast_lower::PredefinedIntrinsicIds,
) -> Result<rumoca_core::ExternalFunction, FlattenError> {
    Ok(rumoca_core::ExternalFunction {
        language: ext.language.clone().unwrap_or_else(|| "C".to_string()),
        function_name: ext.function_name.as_ref().map(|t| t.text.to_string()),
        output_name: ext.output.as_ref().map(|o| {
            o.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".")
        }),
        args: ext
            .args
            .iter()
            .map(|argument| {
                ast_lower::expression_from_ast_with_intrinsics(argument, predefined_intrinsics)
            })
            .collect::<Result<Vec<_>, _>>()?,
        annotations: ext
            .annotation
            .iter()
            .map(|annotation| convert_external_annotation(annotation, predefined_intrinsics))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn convert_external_annotation(
    annotation: &ast::Expression,
    predefined_intrinsics: ast_lower::PredefinedIntrinsicIds,
) -> Result<rumoca_core::ExternalFunctionAnnotation, FlattenError> {
    let ast::Expression::Modification {
        target,
        value,
        span,
    } = annotation
    else {
        return Err(unsupported_external_annotation(
            annotation,
            "expected a named annotation modification",
        ));
    };
    if target.parts.is_empty()
        || target
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return Err(unsupported_external_annotation(
            annotation,
            "annotation names must be non-indexed name paths",
        ));
    }
    reject_lossy_external_annotation_value(value)?;
    Ok(rumoca_core::ExternalFunctionAnnotation {
        name: target
            .parts
            .iter()
            .map(|part| part.ident.text.to_string())
            .collect(),
        value: ast_lower::expression_from_ast_with_intrinsics(value, predefined_intrinsics)?,
        span: *span,
    })
}

fn reject_lossy_external_annotation_value(value: &ast::Expression) -> Result<(), FlattenError> {
    struct LossySyntax {
        kind: Option<&'static str>,
    }

    impl ast::Visitor for LossySyntax {
        fn visit_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
            self.kind = match expr {
                ast::Expression::Empty { .. } => Some("empty modification"),
                ast::Expression::ClassModification { .. } => Some("class modification"),
                ast::Expression::NamedArgument { .. } => Some("named argument"),
                ast::Expression::Modification { .. } => Some("nested modification"),
                _ => None,
            };
            if self.kind.is_some() {
                return ControlFlow::Break(());
            }
            ast::walk_expression_default(self, expr)
        }
    }

    let mut visitor = LossySyntax { kind: None };
    let _ = <LossySyntax as ast::Visitor>::visit_expression(&mut visitor, value);
    visitor.kind.map_or(Ok(()), |kind| {
        Err(unsupported_external_annotation(
            value,
            &format!("cannot preserve {kind} in Flat metadata"),
        ))
    })
}

fn unsupported_external_annotation(expression: &ast::Expression, reason: &str) -> FlattenError {
    FlattenError::Internal(format!(
        "unsupported external-function annotation at {:?}: {reason}",
        expression.span()
    ))
}

/// Extract derivative annotations from function annotation expressions (MLS §12.7.1).
///
/// Looks for annotations like:
/// - `derivative = funcName`
/// - `derivative(order=2) = funcName`
/// - `derivative(zeroDerivative=x, zeroDerivative=y) = funcName`
/// - `derivative(noDerivative=u) = funcName`
pub(super) fn extract_derivative_annotations(
    annotations: &[ast::Expression],
) -> Vec<rumoca_core::DerivativeAnnotation> {
    let mut derivatives = Vec::new();

    for expr in annotations {
        if let Some(deriv) = extract_single_derivative(expr) {
            derivatives.push(deriv);
        }
    }

    derivatives
}

/// Extract a single derivative annotation from an expression.
pub(super) fn extract_single_derivative(
    expr: &ast::Expression,
) -> Option<rumoca_core::DerivativeAnnotation> {
    // Pattern 1: NamedArgument { name: "derivative", value: ... }
    // This handles: derivative = funcName
    if let ast::Expression::NamedArgument { name, value, .. } = expr
        && name.text.as_ref() == "derivative"
    {
        let func_name = extract_function_name(value)?;
        return Some(rumoca_core::DerivativeAnnotation {
            derivative_function: func_name,
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        });
    }

    // Pattern 2: Modification { target: derivative(...), value: funcName }
    // This handles: derivative(order=2) = funcName, derivative(zeroDerivative=x) = funcName
    if let ast::Expression::Modification { target, value, .. } = expr
        && let Some(annotation) = try_extract_modification_derivative(target, value)
    {
        return Some(annotation);
    }

    // Pattern 3: ClassModification { target: derivative, modifications: [...] }
    // This handles more complex cases where derivative has modifications
    if let ast::Expression::ClassModification {
        target,
        modifications,
        ..
    } = expr
        && let Some(annotation) = try_extract_class_mod_derivative(target, modifications)
    {
        return Some(annotation);
    }

    None
}

/// Try to extract a derivative annotation from a Modification expression.
pub(super) fn try_extract_modification_derivative(
    target: &rumoca_ir_ast::ComponentReference,
    value: &ast::Expression,
) -> Option<rumoca_core::DerivativeAnnotation> {
    // Check if target is "derivative"
    if target.parts.len() != 1 || target.parts[0].ident.text.as_ref() != "derivative" {
        return None;
    }

    let func_name = extract_function_name(value)?;
    let mut annotation = rumoca_core::DerivativeAnnotation {
        derivative_function: func_name,
        order: 1,
        zero_derivative: Vec::new(),
        no_derivative: Vec::new(),
    };

    // Extract modifiers from subscripts
    extract_modifiers_from_subscripts(&target.parts[0].subs, &mut annotation);
    Some(annotation)
}

/// Try to extract a derivative annotation from a ClassModification expression.
pub(super) fn try_extract_class_mod_derivative(
    target: &rumoca_ir_ast::ComponentReference,
    modifications: &[ast::Expression],
) -> Option<rumoca_core::DerivativeAnnotation> {
    // Check if target is "derivative"
    if target.parts.len() != 1 || target.parts[0].ident.text.as_ref() != "derivative" {
        return None;
    }

    let mut annotation = rumoca_core::DerivativeAnnotation {
        derivative_function: String::new(),
        order: 1,
        zero_derivative: Vec::new(),
        no_derivative: Vec::new(),
    };

    // Extract modifiers from the modifications list
    for mod_expr in modifications {
        extract_derivative_modifier(mod_expr, &mut annotation);
        // Check if this is the function name (ComponentReference without assignment)
        if let Some(name) = extract_function_name(mod_expr) {
            annotation.derivative_function = name;
        }
    }

    if annotation.derivative_function.is_empty() {
        None
    } else {
        Some(annotation)
    }
}

/// Extract modifiers from subscripts (used in derivative(order=2) style).
pub(super) fn extract_modifiers_from_subscripts(
    subs: &Option<Vec<rumoca_ir_ast::Subscript>>,
    annotation: &mut rumoca_core::DerivativeAnnotation,
) {
    let Some(subs) = subs else { return };
    for sub in subs {
        if let rumoca_ir_ast::Subscript::Expression(sub_expr) = sub {
            extract_derivative_modifier(sub_expr, annotation);
        }
    }
}

/// Extract derivative modifiers like order, zeroDerivative, noDerivative from an expression.
pub(super) fn extract_derivative_modifier(
    expr: &ast::Expression,
    annotation: &mut rumoca_core::DerivativeAnnotation,
) {
    // Handle NamedArgument { name: "order"|"zeroDerivative"|"noDerivative", value: ... }
    if let ast::Expression::NamedArgument { name, value, .. } = expr {
        apply_modifier(name.text.as_ref(), value, annotation);
    }

    // Handle Modification { target: "order"|..., value: ... }
    if let ast::Expression::Modification { target, value, .. } = expr
        && target.parts.len() == 1
    {
        apply_modifier(target.parts[0].ident.text.as_ref(), value, annotation);
    }
}

/// Apply a derivative modifier by name to the annotation.
pub(super) fn apply_modifier(
    name: &str,
    value: &ast::Expression,
    annotation: &mut rumoca_core::DerivativeAnnotation,
) {
    match name {
        "order" => {
            if let Some(order) = extract_integer_value(value) {
                annotation.order = order as u32;
            }
        }
        "zeroDerivative" => {
            if let Some(var_name) = extract_variable_name(value) {
                annotation.zero_derivative.push(var_name);
            }
        }
        "noDerivative" => {
            if let Some(var_name) = extract_variable_name(value) {
                annotation.no_derivative.push(var_name);
            }
        }
        _ => {}
    }
}

/// Extract a function name from an expression (ComponentReference).
pub(super) fn extract_function_name(expr: &ast::Expression) -> Option<String> {
    if let ast::Expression::ComponentReference(cr) = expr {
        Some(
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join("."),
        )
    } else {
        None
    }
}

/// Extract an integer value from an expression (Terminal with UnsignedInteger).
pub(super) fn extract_integer_value(expr: &ast::Expression) -> Option<i64> {
    if let ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token,
        ..
    } = expr
    {
        token.text.parse().ok()
    } else {
        None
    }
}

/// Extract a variable name from an expression (ComponentReference).
pub(super) fn extract_variable_name(expr: &ast::Expression) -> Option<String> {
    if let ast::Expression::ComponentReference(cr) = expr {
        Some(
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join("."),
        )
    } else {
        None
    }
}

/// Try to extract an integer value from a subscript expression.
pub(super) fn extract_integer_from_subscript(sub: &rumoca_ir_ast::Subscript) -> Option<i64> {
    if let rumoca_ir_ast::Subscript::Expression(rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token,
        ..
    }) = sub
    {
        token.text.parse().ok()
    } else {
        None
    }
}

pub(super) fn subscripts_to_param_dims(
    subscripts: &[rumoca_ir_ast::Subscript],
    context_name: &str,
    source_map: &rumoca_core::SourceMap,
) -> Result<Vec<i64>, FlattenError> {
    subscripts
        .iter()
        .map(|subscript| required_param_dim(subscript, context_name, source_map))
        .collect()
}

fn required_param_dim(
    subscript: &rumoca_ir_ast::Subscript,
    context_name: &str,
    source_map: &rumoca_core::SourceMap,
) -> Result<i64, FlattenError> {
    if let Some(dim) = extract_integer_from_subscript(subscript) {
        return Ok(dim);
    }
    let span = ast_subscript_span(subscript, source_map)?;
    Err(FlattenError::unresolved_component_dimension(
        context_name,
        format!("{subscript:?}"),
        span,
    ))
}

fn ast_subscript_span(
    subscript: &rumoca_ir_ast::Subscript,
    source_map: &rumoca_core::SourceMap,
) -> Result<rumoca_core::Span, FlattenError> {
    match subscript {
        rumoca_ir_ast::Subscript::Expression(expr) => Ok(expr.span()),
        rumoca_ir_ast::Subscript::Range { token } => required_location_span(
            source_map,
            &token.location,
            "function parameter range subscript",
        ),
        rumoca_ir_ast::Subscript::Empty => Err(FlattenError::missing_source_context(
            "empty function parameter subscript has no source token",
        )),
    }
}

fn apply_component_description(param: &mut rumoca_core::FunctionParam, component: &ast::Component) {
    if component.description.is_empty() {
        return;
    }
    param.description = Some(
        component
            .description
            .iter()
            .map(|token| token.text.as_ref())
            .collect::<Vec<_>>()
            .join(" "),
    );
}

/// Convert a component declaration to a function parameter.
pub(super) fn convert_component_to_param(
    class_index: &ast::ClassDefIndex<'_>,
    name: &str,
    component: &ast::Component,
    source_map: &rumoca_core::SourceMap,
    expressions: FunctionExpressionContext<'_>,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
) -> Result<rumoca_core::FunctionParam, FlattenError> {
    // Get the type name from type_name.name (Vec<Token>)
    let type_name = component
        .type_name
        .name
        .iter()
        .map(|t| t.text.to_string())
        .collect::<Vec<_>>()
        .join(".");

    let span = required_location_span(
        source_map,
        &component.location,
        "function parameter declaration",
    )?;
    // Get array dimensions from shape (resolved) or shape_expr (expressions).
    // For variable-size arrays (e.g., `Real x[:]`), use [0] as a sentinel
    // so that code generators know the parameter is an array even when
    // the exact size is unknown at compile time.
    let type_alias_dims = function_param_type_alias_dims(class_index, component, source_map)?;
    let mut param_dims = Vec::new();
    let shape_expr = if !component.shape_expr.is_empty() {
        let shape_expr = component
            .shape_expr
            .iter()
            .map(|sub| {
                lower_function_shape_subscript(sub, class_index, imports, locals, expressions, span)
            })
            .collect::<Result<Vec<_>, FlattenError>>()?;
        param_dims = shape_expr.iter().map(function_shape_dim).collect();
        Some(shape_expr)
    } else if !component.shape.is_empty() {
        param_dims = component.shape.iter().map(|&d| d as i64).collect();
        None
    } else {
        None
    };
    if !type_alias_dims.is_empty() {
        param_dims.extend(type_alias_dims);
    }
    // The declared type identity is resolved once, so the effective type, the
    // recorded `type_def_id`, and the class type all name the same class.
    let type_identity = component_type_identity(class_index, component);
    let effective_type = expressions.type_catalog.effective_type(
        component,
        type_identity.def_id,
        param_dims,
        span,
    )?;
    let mut param = rumoca_core::FunctionParam::new(name, type_name, effective_type, span);
    if let Some(shape_expr) = shape_expr {
        param = param.with_shape_expr(shape_expr);
    }
    finish_function_param(
        class_index,
        component,
        type_identity,
        expressions,
        imports,
        locals,
        param,
    )
}

fn finish_function_param(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
    type_identity: ComponentTypeIdentity<'_>,
    expressions: FunctionExpressionContext<'_>,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
    mut param: rumoca_core::FunctionParam,
) -> Result<rumoca_core::FunctionParam, FlattenError> {
    if let Some(def_id) = component.def_id {
        param = param.with_def_id(def_id);
    }
    if let Some(type_def_id) = type_identity.def_id {
        param = param.with_type_def_id(type_def_id);
    }
    if let Some(type_class) = type_identity
        .class_def
        .map(|class_def| effective_function_param_class_type(class_index, class_def))
    {
        param = param.with_type_class(type_class);
    }
    if let Some(type_name) =
        effective_function_param_primitive_type(class_index, component, &param.type_name)
    {
        param.type_name = type_name.to_string();
    }

    // Preserve declared scalar bounds on function parameters.  Besides being
    // part of the parameter contract, finite Integer bounds allow solve
    // lowering to turn a runtime-bounded Modelica loop into guarded straight-
    // line code suitable for differentiation and efficient repeated solves.
    let lower_bound = component
        .modifications
        .get("min")
        .map(|expr| {
            let qualified = qualify_function_expr(expr, imports, locals);
            ast_lower::expression_from_ast_with_intrinsics(
                &qualified,
                expressions.predefined_intrinsics,
            )
        })
        .transpose()?;
    let upper_bound = component
        .modifications
        .get("max")
        .map(|expr| {
            let qualified = qualify_function_expr(expr, imports, locals);
            ast_lower::expression_from_ast_with_intrinsics(
                &qualified,
                expressions.predefined_intrinsics,
            )
        })
        .transpose()?;
    param = param.with_bounds(lower_bound, upper_bound);

    // Use explicit declaration binding (`= expr`) for default function inputs.
    // Fall back to `start` when no declaration binding is available.
    if component.has_explicit_binding {
        if let Some(binding_expr) = component.binding.as_ref()
            && !matches!(binding_expr, ast::Expression::Empty { .. })
        {
            let qualified = qualify_function_expr(binding_expr, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_intrinsics(
                &qualified,
                expressions.predefined_intrinsics,
            )?);
        } else if !matches!(component.start, ast::Expression::Empty { .. }) {
            let qualified = qualify_function_expr(&component.start, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_intrinsics(
                &qualified,
                expressions.predefined_intrinsics,
            )?);
        }
    }

    apply_component_description(&mut param, component);

    Ok(param)
}

fn function_shape_dim(subscript: &rumoca_core::Subscript) -> i64 {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => *value,
        rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Expr { .. } => 0,
    }
}

pub(super) fn lower_function_shape_subscript(
    subscript: &ast::Subscript,
    class_index: &ast::ClassDefIndex<'_>,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
    expressions: FunctionExpressionContext<'_>,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Subscript, FlattenError> {
    match subscript {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(value) = resolve_compile_time_integer_expr(expr, class_index) {
                return Ok(rumoca_core::Subscript::index(value, span));
            }
            let qualified = qualify_function_expr(expr, imports, locals);
            Ok(rumoca_core::Subscript::expr(
                Box::new(ast_lower::expression_from_ast_with_intrinsics(
                    &qualified,
                    expressions.predefined_intrinsics,
                )?),
                span,
            ))
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => {
            Ok(rumoca_core::Subscript::try_generated_colon(
                owner_span,
                "flat function metadata subscript",
            )
            .map_err(|err| FlattenError::missing_source_context(err.to_string()))?)
        }
    }
}

pub(super) fn resolve_compile_time_integer_expr(
    expr: &ast::Expression,
    class_index: &ast::ClassDefIndex<'_>,
) -> Option<i64> {
    let mut visiting = FxHashSet::default();
    resolve_compile_time_integer_expr_inner(expr, class_index, &mut visiting)
}

pub(super) fn resolve_compile_time_integer_expr_inner(
    expr: &ast::Expression,
    class_index: &ast::ClassDefIndex<'_>,
    visiting: &mut FxHashSet<rumoca_core::DefId>,
) -> Option<i64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus,
            rhs,
            ..
        } => resolve_compile_time_integer_expr_inner(rhs, class_index, visiting),
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => resolve_compile_time_integer_expr_inner(rhs, class_index, visiting)
            .and_then(i64::checked_neg),
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = resolve_compile_time_integer_expr_inner(lhs, class_index, visiting)?;
            let rhs = resolve_compile_time_integer_expr_inner(rhs, class_index, visiting)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => lhs.checked_add(rhs),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => lhs.checked_sub(rhs),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => lhs.checked_mul(rhs),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem
                    if rhs != 0 && lhs % rhs == 0 =>
                {
                    Some(lhs / rhs)
                }
                _ => None,
            }
        }
        ast::Expression::ComponentReference(reference) => reference
            .target_def_id()
            .and_then(|def_id| resolve_component_constant_integer(def_id, class_index, visiting)),
        _ => None,
    }
}

pub(super) fn resolve_component_constant_integer(
    def_id: rumoca_core::DefId,
    class_index: &ast::ClassDefIndex<'_>,
    visiting: &mut FxHashSet<rumoca_core::DefId>,
) -> Option<i64> {
    if !visiting.insert(def_id) {
        return None;
    }
    let result = component_by_def_id(class_index, def_id)
        .filter(|component| {
            matches!(
                component.variability,
                rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
            )
        })
        .and_then(|component| component.binding.as_ref())
        .and_then(|binding| {
            resolve_compile_time_integer_expr_inner(binding, class_index, visiting)
        });
    visiting.remove(&def_id);
    result
}

pub(super) fn component_by_def_id<'a>(
    class_index: &'a ast::ClassDefIndex<'_>,
    def_id: rumoca_core::DefId,
) -> Option<&'a ast::Component> {
    let parent_def_id = class_index.parent_def_id(def_id)?;
    let local_name = class_index.local_name(def_id)?;
    let parent = class_index.get(parent_def_id)?;
    parent
        .components
        .get(local_name)
        .filter(|component| component.def_id == Some(def_id))
}

const FUNCTION_QUALIFY_OPTS: qualify::QualifyOptions = qualify::QualifyOptions { skip_local: true };

pub(super) fn qualify_function_expr(
    expr: &ast::Expression,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
) -> ast::Expression {
    qualify::qualify_expression_with_imports_and_locals(
        expr,
        &ast::QualifiedName::new(),
        FUNCTION_QUALIFY_OPTS,
        locals,
        imports,
    )
}

pub(crate) use crate::function_lowering::lower_record_function_params;

/// Specialize function-typed formal parameters when their call targets are
/// statically known.
///
/// The full specialization pass is intentionally conservative in this branch:
/// keeping canonical function names is always semantically valid, while
/// specialization is an optimization. This hook preserves the pipeline contract
/// and can be expanded without making function inlining mandatory.
pub(crate) fn specialize_static_function_params(_flat: &mut flat::Model) {}
