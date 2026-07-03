//! The per-class component instantiation loop and its alias-set plumbing.

use super::*;
use rumoca_core::scoped_component_path_candidates;

/// Alias sets used while instantiating a class's components.
#[derive(Clone, Copy)]
pub(crate) struct ComponentImports<'a> {
    /// Aliases applied when qualifying component shape/binding expressions.
    pub(crate) qualification: &'a [(String, String)],
    /// Superset of `qualification` that also aliases enclosing-package
    /// constants (MLS §5.3.2); used only as an attribute-evaluation fallback
    /// so it cannot perturb regular component qualification.
    pub(crate) attributes: &'a [(String, String)],
}

impl ComponentImports<'_> {
    pub(super) const EMPTY: ComponentImports<'static> = ComponentImports {
        qualification: &[],
        attributes: &[],
    };
}

pub(super) fn instantiate_effective_components(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    type_overrides: &TypeOverrideMap,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    imports: ComponentImports<'_>,
) -> InstantiateResult<()> {
    let array_expansion_scope = ArrayExpansionScope {
        tree,
        effective_components,
        type_overrides,
        imports,
    };

    for (name, comp) in effective_components {
        if mark_disabled_component_if_needed(comp, name, ctx, effective_components, tree, overlay) {
            continue;
        }

        // MLS §7.3: Apply type override for replaceable type redeclarations.
        let comp_ref = apply_type_override(tree, comp, type_overrides, Some(ctx.mod_env()))?;
        let comp = comp_ref.as_ref();
        let type_name = comp.type_name.to_string();

        let qualified_shape_expr =
            qualify_shape_subscripts_imports(&comp.shape_expr, imports.qualification);
        let dims = evaluate_array_dimensions(
            &comp.shape,
            &qualified_shape_expr,
            ctx.mod_env(),
            effective_components,
            tree,
            resolve_effective_components_for_eval,
        );
        let dims = if !qualified_shape_expr.is_empty() && dims.as_ref().is_some_and(Vec::is_empty) {
            evaluate_array_dimensions_with_known_params(
                &comp.shape,
                &qualified_shape_expr,
                &ctx.known_int_params,
                &ctx.current_path(),
            )
            .or(dims)
        } else {
            dims
        };
        if let Some(dims) = dims.as_ref()
            && dims.contains(&0)
        {
            register_zero_sized_array_component(ctx, overlay, name, dims);
            continue;
        }

        let type_info = lookup_type_info(tree, comp, &type_name)?;
        let should_expand = !type_info.is_primitive && dims.as_ref().is_some_and(|d| !d.is_empty());

        if should_expand {
            expand_array_component(
                &array_expansion_scope,
                name,
                comp,
                dims.as_ref().unwrap(),
                ctx,
                overlay,
            )?;
            continue;
        }

        ctx.push_path(name);
        instantiate_component(
            tree,
            comp,
            ctx,
            overlay,
            effective_components,
            type_overrides,
            imports,
        )?;
        ctx.pop_path();
    }
    Ok(())
}

///
/// Note (MLS §4.8): Conditional components are handled in `instantiate_class`.
/// Components whose condition evaluates to false are skipped and recorded
/// in `overlay.disabled_components`. The flatten phase filters out connections
/// and equations involving disabled components.
///
/// MLS §10.1: Array components of structured types (connectors, models) are expanded
/// to indexed instances. For example, `Resistor r[3]` becomes `r[1]`, `r[2]`, `r[3]`.
pub(super) fn component_type_id(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
    is_primitive: bool,
) -> TypeId {
    if is_primitive {
        resolve_primitive_type_id(tree, type_name, class_def)
    } else {
        TypeId::UNKNOWN
    }
}

fn evaluate_array_dimensions_with_known_params(
    shape: &[usize],
    shape_expr: &[rumoca_ir_ast::Subscript],
    known_int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &rumoca_ir_ast::QualifiedName,
) -> Option<Vec<i64>> {
    if !shape_expr.is_empty() {
        let mut dims = Vec::with_capacity(shape_expr.len());
        for subscript in shape_expr {
            let rumoca_ir_ast::Subscript::Expression(expr) = subscript else {
                return None;
            };
            let dim = eval_known_integer_expr(expr, known_int_params, scope)?;
            if dim < 0 {
                return None;
            }
            dims.push(dim);
        }
        return Some(dims);
    }

    (!shape.is_empty()).then(|| shape.iter().map(|&dim| dim as i64).collect())
}

fn eval_known_integer_expr(
    expr: &rumoca_ir_ast::Expression,
    known_int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &rumoca_ir_ast::QualifiedName,
) -> Option<i64> {
    match expr {
        rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        rumoca_ir_ast::Expression::ComponentReference(component_ref)
            if !component_ref.parts.is_empty()
                && component_ref.parts.iter().all(|part| part.subs.is_none()) =>
        {
            let name = component_ref
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let name = rumoca_core::ComponentPath::from_flat_path(&name);
            let scope = scope.to_component_path();
            for candidate in scoped_component_path_candidates(&name, &scope) {
                if let Some(value) = known_int_params.get(candidate.as_str()) {
                    return Some(*value);
                }
            }
            None
        }
        rumoca_ir_ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_known_integer_expr(lhs, known_int_params, scope)?;
            let rhs = eval_known_integer_expr(rhs, known_int_params, scope)?;
            eval_known_integer_binary(op, lhs, rhs)
        }
        rumoca_ir_ast::Expression::Unary { op, rhs, .. } => {
            let value = eval_known_integer_expr(rhs, known_int_params, scope)?;
            match op {
                rumoca_core::OpUnary::Minus => value.checked_neg(),
                rumoca_core::OpUnary::Plus => Some(value),
                _ => None,
            }
        }
        rumoca_ir_ast::Expression::Parenthesized { inner, .. } => {
            eval_known_integer_expr(inner, known_int_params, scope)
        }
        rumoca_ir_ast::Expression::FunctionCall { comp, args, .. }
            if comp.parts.len() == 1
                && comp.parts[0].subs.is_none()
                && comp.parts[0].ident.text.as_ref() == "div"
                && args.len() == 2 =>
        {
            let lhs = eval_known_integer_expr(&args[0], known_int_params, scope)?;
            let rhs = eval_known_integer_expr(&args[1], known_int_params, scope)?;
            (rhs != 0).then_some(lhs / rhs)
        }
        _ => None,
    }
}

fn eval_known_integer_binary(op: &rumoca_core::OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    match op {
        rumoca_core::OpBinary::Add => lhs.checked_add(rhs),
        rumoca_core::OpBinary::Sub => lhs.checked_sub(rhs),
        rumoca_core::OpBinary::Mul => lhs.checked_mul(rhs),
        rumoca_core::OpBinary::Div if rhs != 0 && lhs % rhs == 0 => Some(lhs / rhs),
        _ => None,
    }
}

/// Flow/stream from the connection prefix (MLS §9.3), inheriting from the
/// parent for record fields (e.g. `flow Complex i` makes i.re/i.im flow).
pub(super) fn component_flow_stream(
    comp: &ast::Component,
    ctx: &InstantiateContext,
) -> (bool, bool) {
    match &comp.connection {
        rumoca_ir_ast::Connection::Flow(_) => (true, false),
        rumoca_ir_ast::Connection::Stream(_) => (false, true),
        rumoca_ir_ast::Connection::Empty => (ctx.inherited_flow(), ctx.inherited_stream()),
    }
}
