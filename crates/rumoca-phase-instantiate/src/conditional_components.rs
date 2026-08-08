//! Conditional components (MLS §4.4.5) and the MLS §5.4 lookup they depend on.
//!
//! A conditional component's condition is a parameter expression that must be
//! decided at translation time; when it is false the component contributes no
//! variables, no equations and no connections. Conditions frequently read a
//! parameter of an `outer` component (`not world.driveTrainMechanics3D`), which
//! per MLS §5.4 denotes the nearest enclosing `inner` element of the same name.
//! This module resolves those references against the already-instantiated inner
//! instance and records the disabled component paths for the flatten phase.

use super::{
    IndexMap, InstantiateContext, InstantiateError, InstantiateEvalCtx, InstantiateResult,
    OuterValues, ast, evaluate_component_condition_with_outer_values,
    resolve_effective_components_for_eval,
};

impl InstantiateContext {
    /// Register boolean parameters discovered for a class scope (MLS §5.4 support).
    ///
    /// Keyed by full instance path so a nested `outer` reference can be redirected
    /// to the matching `inner` instance path when a conditional component's
    /// condition is evaluated.
    pub(crate) fn register_known_bool_params(
        &mut self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, bool>,
    ) {
        register_scoped_params(&mut self.known_bool_params, scope, local);
    }

    /// Register Real parameters discovered for a class scope (MLS §5.4 support).
    pub(crate) fn register_known_real_params(
        &mut self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, f64>,
    ) {
        register_scoped_params(&mut self.known_real_params, scope, local);
    }

    /// Boolean values reachable through the `outer` components of a class (MLS §5.4).
    pub(crate) fn outer_reference_bool_values(
        &self,
        effective_components: &IndexMap<String, ast::Component>,
    ) -> rustc_hash::FxHashMap<String, bool> {
        self.outer_reference_values(effective_components, &self.known_bool_params)
    }

    /// Real values reachable through the `outer` components of a class (MLS §5.4).
    pub(crate) fn outer_reference_real_values(
        &self,
        effective_components: &IndexMap<String, ast::Component>,
    ) -> rustc_hash::FxHashMap<String, f64> {
        self.outer_reference_values(effective_components, &self.known_real_params)
    }

    /// Re-key `known` under each `outer` component of this class (MLS §5.4).
    ///
    /// An `outer` element denotes the nearest enclosing `inner` element of the same
    /// name, so `world.driveTrainMechanics3D` inside a class that only declares
    /// `outer World world` must be read from the inner `world` instance. The returned
    /// map is keyed by the reference path as written in the class being instantiated.
    fn outer_reference_values<T: Copy>(
        &self,
        effective_components: &IndexMap<String, ast::Component>,
        known: &rustc_hash::FxHashMap<String, T>,
    ) -> rustc_hash::FxHashMap<String, T> {
        let mut values = rustc_hash::FxHashMap::default();
        if known.is_empty() {
            return values;
        }
        for (name, comp) in effective_components {
            if !comp.outer {
                continue;
            }
            // MLS §5.4: `inner outer` refers to the parent's inner, not itself.
            let inner_decl = if comp.inner {
                self.find_parent_inner(name)
            } else {
                self.find_inner(name)
            };
            let Some(inner_decl) = inner_decl else {
                continue;
            };
            let inner_prefix = inner_decl.qualified_name.to_flat_string();
            if inner_prefix.is_empty() {
                continue;
            }
            collect_under(known, &inner_prefix, name, &mut values);
        }
        values
    }
}

/// Record `local` under `scope`, so a nested `outer` reference can be redirected
/// to the matching `inner` instance path when a condition is evaluated.
fn register_scoped_params<T: Copy>(
    known: &mut rustc_hash::FxHashMap<String, T>,
    scope: &ast::QualifiedName,
    local: &rustc_hash::FxHashMap<String, T>,
) {
    let scope_prefix = scope.to_flat_string();
    for (k, v) in local {
        if scope_prefix.is_empty() {
            known.insert(k.clone(), *v);
        } else {
            known.insert(format!("{scope_prefix}.{k}"), *v);
        }
    }
}

/// Re-key every known value under `inner_prefix` as `<outer_name>.<field>`.
fn collect_under<T: Copy>(
    known: &rustc_hash::FxHashMap<String, T>,
    inner_prefix: &str,
    outer_name: &str,
    values: &mut rustc_hash::FxHashMap<String, T>,
) {
    for (path, value) in known {
        let field = path
            .strip_prefix(inner_prefix)
            .and_then(|rest| rest.strip_prefix('.'));
        if let Some(field) = field {
            values.insert(format!("{outer_name}.{field}"), *value);
        }
    }
}

/// Everything a conditional-component condition is evaluated against (MLS §4.4.5).
#[derive(Clone, Copy)]
pub(crate) struct ConditionScope<'a> {
    pub(crate) tree: &'a ast::ClassTree,
    pub(crate) effective_components: &'a IndexMap<String, ast::Component>,
    /// Boolean values reachable through this class's `outer` components (MLS §5.4).
    pub(crate) outer_bools: &'a rustc_hash::FxHashMap<String, bool>,
    /// Real values reachable through this class's `outer` components (MLS §5.4).
    pub(crate) outer_reals: &'a rustc_hash::FxHashMap<String, f64>,
    /// Import aliases visible in this class (MLS §13.2), so a condition may name
    /// an imported package constant by its short spelling.
    pub(crate) imports: &'a [(String, String)],
}

/// Record `name` as disabled when its condition evaluates to false (MLS §4.4.5).
///
/// MLS §4.4.5 requires the condition-attribute to be a Boolean *parameter
/// expression*: whether the component exists is settled at translation time,
/// before any variable has a value. There is therefore no third answer here.
/// Keeping a component whose condition could not be decided asserts `true`, and
/// dropping it asserts `false`; either way the compiler would be substituting an
/// answer the source did not give, which SPEC_0008 forbids ("Default values on
/// error are prohibited", "Missing semantic data MUST NOT be synthesized").
/// An undecidable condition is reported as [`InstantiateError::ConditionalError`]
/// against the condition's own span.
pub(crate) fn mark_disabled_component_if_needed(
    comp: &ast::Component,
    name: &str,
    ctx: &mut InstantiateContext,
    scope: ConditionScope<'_>,
    overlay: &mut ast::InstanceOverlay,
) -> InstantiateResult<bool> {
    let Some(cond) = comp.condition.as_ref() else {
        return Ok(false);
    };

    let eval_ctx = InstantiateEvalCtx {
        tree: scope.tree,
        mod_env: ctx.mod_env(),
        effective_components: scope.effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let Some(condition_value) = decide_condition(&eval_ctx, cond, scope) else {
        return Err(Box::new(InstantiateError::conditional_error(
            name,
            cond.span(),
        )));
    };

    if condition_value {
        return Ok(false);
    }

    // Disabled component paths are recorded in overlay.disabled_components so the
    // flatten phase drops the component's variables, equations and connections.
    ctx.push_path(name);
    overlay
        .disabled_components
        .insert(ctx.current_path().to_component_path());
    ctx.pop_path();
    Ok(true)
}

/// Decide a conditional component's condition (MLS §4.4.5).
///
/// The condition is first read exactly as written. Only when that leaves it
/// undecided is it retried with this class's `import` aliases applied
/// (MLS §13.2/§5.3.2): `ratioCommonLeakage > eps` names a package constant the
/// class imported under a short spelling, which is invisible to a lookup that
/// only searches this scope's components. Qualification resolves a *name*, so the
/// retry can only find a declaration the model wrote — it never supplies a value
/// of its own (SPEC_0008).
fn decide_condition(
    eval_ctx: &InstantiateEvalCtx<'_>,
    cond: &ast::Expression,
    scope: ConditionScope<'_>,
) -> Option<bool> {
    let outer_values = OuterValues::new(scope.outer_bools, scope.outer_reals);
    if let Some(value) =
        evaluate_component_condition_with_outer_values(eval_ctx, cond, outer_values)
    {
        return Some(value);
    }
    if !crate::dims::expr_mentions_import_alias(cond, scope.imports) {
        return None;
    }
    let qualified = crate::dims::qualify_shape_expr_imports(scope.tree, cond, scope.imports);
    evaluate_component_condition_with_outer_values(eval_ctx, &qualified, outer_values)
}
