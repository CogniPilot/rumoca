//! The per-class component instantiation loop and its alias-set plumbing.

use super::*;

/// Rewrite vocabularies used while instantiating a class's components.
///
/// Import bindings are the lookup authority's effective imports; the constant
/// channels alias package members (MLS §5.3.2), which are not imports.
#[derive(Clone, Copy)]
pub(crate) struct ComponentImports<'a> {
    /// Root-issued declaration index shared by the entire instantiation.
    pub(crate) class_index: &'a ast::ClassDefIndex<'a>,
    /// Constant aliases of actively redeclared packages; they replace
    /// same-named aliases of the replaced package, so they precede imports.
    pub(crate) overriding_constants: &'a [(String, String)],
    /// Enclosing package-constant aliases (MLS §5.3.2); used only as an
    /// attribute-evaluation fallback after imports, so they cannot perturb
    /// regular component qualification.
    pub(crate) enclosing_constants: &'a [(String, String)],
    /// Import bindings of the instantiated class's own scope. Components with
    /// a recorded declaring scope use that scope's bindings instead.
    pub(crate) class_imports: Option<&'a ast::EffectiveImports>,
}

impl<'a> ComponentImports<'a> {
    /// The vocabulary for qualifying one component's shape and binding
    /// expressions: the declaring scope's import bindings when recorded,
    /// otherwise the class's own.
    pub(super) fn qualification<'b>(
        self,
        declaring: Option<&'b ast::EffectiveImports>,
    ) -> crate::dims::ImportRewrite<'b>
    where
        'a: 'b,
    {
        crate::dims::ImportRewrite {
            class_index: self.class_index,
            overriding_aliases: self.overriding_constants,
            effective: declaring.or(self.class_imports),
            fallback_aliases: &[],
        }
    }

    /// The vocabulary for attribute, condition, and modifier evaluation,
    /// which may also fall back to enclosing package constants.
    pub(super) fn attributes<'b>(
        self,
        declaring: Option<&'b ast::EffectiveImports>,
    ) -> crate::dims::ImportRewrite<'b>
    where
        'a: 'b,
    {
        crate::dims::ImportRewrite {
            class_index: self.class_index,
            overriding_aliases: self.overriding_constants,
            effective: declaring.or(self.class_imports),
            fallback_aliases: self.enclosing_constants,
        }
    }
}

/// True when this class both declares an `outer` component and has at least one
/// conditional component, i.e. when MLS §5.4 lookup can affect MLS §4.8 evaluation.
fn needs_outer_condition_values(effective_components: &IndexMap<String, ast::Component>) -> bool {
    effective_components
        .values()
        .any(|comp| comp.condition.is_some())
        && effective_components.values().any(|comp| comp.outer)
}

struct OuterConditionValues {
    integers: rustc_hash::FxHashMap<String, i64>,
    booleans: rustc_hash::FxHashMap<String, bool>,
    reals: rustc_hash::FxHashMap<String, f64>,
}

impl OuterConditionValues {
    fn empty() -> Self {
        Self {
            integers: rustc_hash::FxHashMap::default(),
            booleans: rustc_hash::FxHashMap::default(),
            reals: rustc_hash::FxHashMap::default(),
        }
    }

    fn resolve(
        ctx: &InstantiateContext,
        effective_components: &IndexMap<String, ast::Component>,
    ) -> Self {
        if !needs_outer_condition_values(effective_components) {
            return Self::empty();
        }
        Self {
            integers: ctx.outer_reference_int_values(effective_components),
            booleans: ctx.outer_reference_bool_values(effective_components),
            reals: ctx.outer_reference_real_values(effective_components),
        }
    }
}

#[derive(Clone, Copy)]
struct EffectiveComponentLoopPlan<'a> {
    type_overrides: &'a TypeOverrideMap,
    selected_component_types: &'a SelectedComponentTypes,
    owner_class_id: rumoca_core::InstanceId,
    imports: ComponentImports<'a>,
}

pub(super) struct EffectiveComponentConstruction<'a> {
    pub(super) imports: ComponentImports<'a>,
    pub(super) nested_selections: Option<crate::nested_scope::NestedComponentTypeSelections>,
}

pub(super) fn instantiate_effective_components(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    type_overrides: &TypeOverrideMap,
    owner_class_id: rumoca_core::InstanceId,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    construction: EffectiveComponentConstruction<'_>,
) -> InstantiateResult<std::sync::Arc<SelectedComponentTypes>> {
    let selected_component_types = std::sync::Arc::new(issue_selected_component_types(
        tree,
        effective_components,
        type_overrides,
        construction.nested_selections,
    )?);
    ctx.issue_selected_component_types(std::sync::Arc::clone(&selected_component_types))?;
    instantiate_effective_components_from_plan(
        tree,
        effective_components,
        ctx,
        overlay,
        EffectiveComponentLoopPlan {
            type_overrides,
            selected_component_types: &selected_component_types,
            owner_class_id,
            imports: construction.imports,
        },
    )?;
    Ok(selected_component_types)
}

fn instantiate_effective_components_from_plan(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    plan: EffectiveComponentLoopPlan<'_>,
) -> InstantiateResult<()> {
    let array_expansion_scope = ArrayExpansionScope {
        tree,
        effective_components,
        type_overrides: plan.type_overrides,
        selected_component_types: plan.selected_component_types,
        owner_class_id: plan.owner_class_id,
        imports: plan.imports,
    };

    // MLS §5.4: resolve `outer` references once per class so conditions such as
    // `not world.driveTrainMechanics3D` can read the matching `inner` instance.
    let outer_values = OuterConditionValues::resolve(ctx, effective_components);
    for (name, comp) in effective_components {
        // MLS §13.2: a component's shape and condition were written in its
        // declaring class, so that class's import bindings govern rewriting.
        let declaring_imports = component_effective_imports(tree, ctx, comp);
        let condition_scope = ConditionScope {
            tree,
            effective_components,
            outer_ints: &outer_values.integers,
            outer_bools: &outer_values.booleans,
            outer_reals: &outer_values.reals,
            imports: plan.imports.attributes(declaring_imports.as_ref()),
        };
        if mark_disabled_component_if_needed(comp, name, ctx, condition_scope, overlay)? {
            continue;
        }

        // Consume the class-occurrence owner's already-issued selection. This
        // projection performs no second lookup or compatibility decision.
        let comp_ref = component_with_issued_type_selection(comp, plan.selected_component_types)?;
        let comp = comp_ref.as_ref();
        let type_name = comp.type_name.to_string();

        let qualified_shape_expr = qualify_shape_subscripts_imports(
            tree,
            &comp.shape_expr,
            plan.imports.qualification(declaring_imports.as_ref()),
        )?;
        let declaration_occurrence = ctx.current_path();
        let qualified_shape_expr = resolve_dynamic_subscript_targets_at_occurrence(
            tree,
            plan.type_overrides,
            ctx.selected_component_type_catalog(),
            &declaration_occurrence,
            plan.selected_component_types,
            qualified_shape_expr,
        )?;
        let dims = evaluate_array_dimensions_with_index(
            &comp.shape,
            &qualified_shape_expr,
            ctx.mod_env(),
            effective_components,
            tree,
            plan.imports.class_index,
            resolve_effective_components_for_eval,
        );
        let type_info = lookup_type_info(tree, comp, &type_name)?;
        if let Some(dims) = dims.as_ref()
            && dims.contains(&0)
        {
            register_zero_sized_array_component(ctx, overlay, name, dims);
            if !type_info.is_primitive {
                continue;
            }
        }
        if !type_info.is_primitive
            && let Some(nonempty_dims) = dims.as_ref().filter(|dims| !dims.is_empty())
        {
            expand_array_component(
                &array_expansion_scope,
                name,
                comp,
                nonempty_dims,
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
            ComponentInstantiationScope {
                owner_class_id: Some(plan.owner_class_id),
                effective_components,
                type_overrides: plan.type_overrides,
                selected_component_types: plan.selected_component_types,
                imports: plan.imports,
            },
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
) -> InstantiateResult<TypeId> {
    if is_primitive {
        resolve_primitive_type_id(tree, type_name, class_def)
    } else {
        Ok(TypeId::UNKNOWN)
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
