//! The per-class component instantiation loop and its alias-set plumbing.

use super::*;

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
            &resolve_effective_components_for_eval,
        );
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
