use super::*;
use crate::source_spans::required_location_span;

/// Convert an AST ExternalFunction to ExternalFunction.
pub(super) fn convert_external_function(
    ext: &rumoca_ir_ast::ExternalFunction,
    _default_name: &str,
) -> rumoca_core::ExternalFunction {
    let mut metadata = ExternalAnnotationMetadata::default();
    for annotation in &ext.annotation {
        metadata.raw.push(annotation.to_string());
        collect_external_annotation(annotation, &mut metadata);
    }

    rumoca_core::ExternalFunction {
        language: ext.language.clone().unwrap_or_else(|| "C".to_string()),
        function_name: ext.function_name.as_ref().map(|t| t.text.to_string()),
        output_name: ext.output.as_ref().map(|o| {
            o.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".")
        }),
        arg_names: ext
            .args
            .iter()
            .filter_map(|arg| {
                // Extract variable names from expressions
                if let ast::Expression::ComponentReference(cr) = arg {
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
            })
            .collect(),
        libraries: metadata.libraries,
        include_directories: metadata.include_directories,
        library_directories: metadata.library_directories,
        includes: metadata.includes,
        annotation: metadata.raw,
    }
}

#[derive(Debug, Default)]
struct ExternalAnnotationMetadata {
    raw: Vec<String>,
    libraries: Vec<String>,
    include_directories: Vec<String>,
    library_directories: Vec<String>,
    includes: Vec<String>,
}

fn collect_external_annotation(expr: &ast::Expression, metadata: &mut ExternalAnnotationMetadata) {
    match expr {
        ast::Expression::NamedArgument { name, value, .. } => {
            collect_external_annotation_value(name.text.as_ref(), value, metadata);
        }
        ast::Expression::Modification { target, value, .. } => {
            if let Some(name) = component_reference_simple_name(target) {
                collect_external_annotation_value(name, value, metadata);
            }
        }
        _ => {}
    }
}

fn collect_external_annotation_value(
    name: &str,
    value: &ast::Expression,
    metadata: &mut ExternalAnnotationMetadata,
) {
    let values = string_values(value);
    match name {
        "Library" => extend_unique(&mut metadata.libraries, values),
        "IncludeDirectory" => extend_unique(&mut metadata.include_directories, values),
        "LibraryDirectory" => extend_unique(&mut metadata.library_directories, values),
        "Include" => extend_unique(&mut metadata.includes, values),
        _ => {}
    }
}

fn component_reference_simple_name(reference: &ast::ComponentReference) -> Option<&str> {
    let [part] = reference.parts.as_slice() else {
        return None;
    };
    if part.subs.as_ref().is_some_and(|subs| !subs.is_empty()) {
        return None;
    }
    Some(part.ident.text.as_ref())
}

fn string_values(expr: &ast::Expression) -> Vec<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
            ..
        } => vec![token.text.trim_matches('"').to_string()],
        ast::Expression::Array { elements, .. } => {
            elements.iter().flat_map(string_values).collect()
        }
        _ => Vec::new(),
    }
}

fn extend_unique(target: &mut Vec<String>, values: Vec<String>) {
    for value in values {
        if !value.is_empty() && !target.contains(&value) {
            target.push(value);
        }
    }
}

#[cfg(test)]
mod external_annotation_tests {
    use super::*;
    use std::sync::Arc;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            ..Default::default()
        }
    }

    fn string_expr(value: &str) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token: token(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn named_arg(name: &str, value: ast::Expression) -> ast::Expression {
        ast::Expression::NamedArgument {
            name: token(name),
            value: Arc::new(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn convert_external_function_preserves_library_annotation_metadata() {
        let external = rumoca_ir_ast::ExternalFunction {
            language: Some("C".to_string()),
            function_name: Some(token("initialize_Modelica_EnergyPlus_9_6_0")),
            args: vec![],
            annotation: vec![
                named_arg(
                    "Library",
                    ast::Expression::Array {
                        elements: vec![
                            string_expr("ModelicaBuildingsEnergyPlus_9_6_0"),
                            string_expr("fmilib_shared"),
                        ],
                        is_matrix: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
                named_arg(
                    "IncludeDirectory",
                    string_expr("modelica://Buildings/Resources/C-Sources"),
                ),
            ],
            ..Default::default()
        };

        let converted = convert_external_function(&external, "initialize");

        assert_eq!(converted.language, "C");
        assert_eq!(
            converted.function_name.as_deref(),
            Some("initialize_Modelica_EnergyPlus_9_6_0")
        );
        assert_eq!(
            converted.libraries,
            vec!["ModelicaBuildingsEnergyPlus_9_6_0", "fmilib_shared"]
        );
        assert_eq!(
            converted.include_directories,
            vec!["modelica://Buildings/Resources/C-Sources"]
        );
        assert!(
            converted
                .annotation
                .iter()
                .any(|item| item.contains("Library"))
        );
    }
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

/// Convert a component declaration to a function parameter.
pub(super) fn convert_component_to_param(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    name: &str,
    component: &ast::Component,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
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
    let mut param = rumoca_core::FunctionParam::new(name, type_name, span);
    if let Some(def_id) = component.def_id {
        param = param.with_def_id(def_id);
    }
    if let Some(type_class) = function_param_type_class(class_index, component) {
        param = param.with_type_class(type_class);
    }

    // Get array dimensions from shape (resolved) or shape_expr (expressions).
    // For variable-size arrays (e.g., `Real x[:]`), use [0] as a sentinel
    // so that code generators know the parameter is an array even when
    // the exact size is unknown at compile time.
    let type_alias_dims = function_param_type_alias_dims(class_index, component, source_map)?;
    let mut param_dims = Vec::new();
    if !component.shape_expr.is_empty() {
        let shape_expr = component
            .shape_expr
            .iter()
            .map(|sub| {
                lower_function_shape_subscript(
                    sub,
                    tree,
                    class_index,
                    imports,
                    locals,
                    def_map,
                    span,
                )
            })
            .collect::<Result<Vec<_>, FlattenError>>()?;
        param_dims = shape_expr.iter().map(function_shape_dim).collect();
        param = param.with_shape_expr(shape_expr);
    } else if !component.shape.is_empty() {
        param_dims = component.shape.iter().map(|&d| d as i64).collect();
    }
    if !type_alias_dims.is_empty() {
        param_dims.extend(type_alias_dims);
    }
    if !param_dims.is_empty() {
        param = param.with_dims(param_dims);
    }

    // Use explicit declaration binding (`= expr`) for default function inputs.
    // Fall back to `start` when no declaration binding is available.
    if component.has_explicit_binding {
        if let Some(binding_expr) = component.binding.as_ref()
            && !matches!(binding_expr, ast::Expression::Empty { .. })
        {
            let qualified = qualify_function_expr(binding_expr, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_context(
                &qualified,
                ast_lower::LoweringContext {
                    def_map: Some(def_map),
                    class_tree: Some(tree),
                    instance_name: None,
                },
            )?);
        } else if !matches!(component.start, ast::Expression::Empty { .. }) {
            let qualified = qualify_function_expr(&component.start, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_context(
                &qualified,
                ast_lower::LoweringContext {
                    def_map: Some(def_map),
                    class_tree: Some(tree),
                    instance_name: None,
                },
            )?);
        }
    }

    // Get description
    if !component.description.is_empty() {
        let desc: Vec<_> = component
            .description
            .iter()
            .map(|t| t.text.to_string())
            .collect();
        param.description = Some(desc.join(" "));
    }

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
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
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
                Box::new(ast_lower::expression_from_ast_with_context(
                    &qualified,
                    ast_lower::LoweringContext {
                        def_map: Some(def_map),
                        class_tree: Some(tree),
                        instance_name: None,
                    },
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
            .def_id
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

pub(super) fn function_param_type_class(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
) -> Option<rumoca_core::ClassType> {
    component
        .type_name
        .def_id
        .and_then(|def_id| class_index.get(def_id))
        .map(|class_def| class_def.class_type.clone())
        .or_else(|| {
            let type_name = component.type_name.to_string();
            class_index
                .get_by_qualified_name(&type_name)
                .map(|class_def| class_def.class_type.clone())
        })
}

const FUNCTION_QUALIFY_OPTS: qualify::QualifyOptions = qualify::QualifyOptions {
    skip_local: true,
    preserve_def_id: true,
};

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
