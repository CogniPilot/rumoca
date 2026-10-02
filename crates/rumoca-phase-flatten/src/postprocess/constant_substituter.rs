//! Fold known constants into flat expressions, statements, and subscripts.
//!
//! The substituter decides whether an occurrence may be replaced at all: a
//! reference that names a live flat variable, a function local, or an already
//! expanded component keeps its symbolic form, and a declaration default is
//! never folded over a member the flat model owns (MLS §7.2.4). Values come
//! from [`super::constant_lookup`]; the expansion chain in
//! [`super::constant_expansion`] turns a self-referential binding into a
//! diagnostic instead of unbounded recursion.

use super::constant_expansion::{
    ConstantExpansion, ConstantExpansionId, ConstantSubstitutionEnv, SemanticConstantId,
};
use super::constant_lookup::{
    constant_expr_preserves_array_shape, generated_constant_candidate_exists,
    named_constructor_arg, reference_key_has_array_shape, resolve_constant_field_access,
    resolve_constant_value_expr, resolve_constant_value_expr_for_ref,
    resolve_indexed_constant_field_access, resolve_inline_indexed_constant,
    resolve_projected_constant_path, resolve_source_constant,
    resolve_varref_through_constant_aliases, scalar_parameter_literal,
};
use super::*;
use rumoca_core::{FallibleExpressionRewriter, FallibleStatementRewriter};

pub(crate) fn substitute_known_constants_expr(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<rumoca_core::Expression, FlattenError> {
    substitute_known_constants_expr_with_options(expr, ctx, live_vars, locals, scope, false)
}

pub(super) fn substitute_known_constants_expr_with_options(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
) -> Result<rumoca_core::Expression, FlattenError> {
    substitute_with_env(
        expr,
        ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
            expanding: None,
            exposures: &[],
        },
    )
}

/// Fold known constants in `expr` while keeping the caller's in-progress
/// constant-expansion chain, so a binding that expands into itself is reported
/// instead of recursing forever.
fn substitute_with_env(
    expr: rumoca_core::Expression,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<rumoca_core::Expression, FlattenError> {
    KnownConstantSubstituter { env }.rewrite_expression(&expr)
}

struct KnownConstantSubstituter<'a> {
    env: ConstantSubstitutionEnv<'a>,
}

impl FallibleExpressionRewriter for KnownConstantSubstituter<'_> {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.rewrite_var_ref(name, subscripts, *span),
            rumoca_core::Expression::FieldAccess {
                base,
                field,
                field_def_id,
                span,
            } => self.rewrite_field_access(base, field, *field_def_id, *span),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => self.rewrite_if(branches, else_branch, *span),
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => {
                let base = self.rewrite_expression(base)?;
                let subscripts = self.rewrite_subscripts(subscripts)?;
                if let Some(element) = select_constant_element(&base, &subscripts) {
                    return Ok(element.clone().with_span(*span));
                }
                Ok(rumoca_core::Expression::Index {
                    base: Box::new(base),
                    subscripts,
                    span: *span,
                })
            }
            other => self.walk_expression(other),
        }
    }

    fn rewrite_subscript(
        &mut self,
        subscript: &rumoca_core::Subscript,
    ) -> Result<rumoca_core::Subscript, Self::Error> {
        match subscript {
            rumoca_core::Subscript::Expr { expr, span } => Ok(rumoca_core::Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)?),
                span: *span,
            }),
            other => Ok(other.clone()),
        }
    }
}

impl KnownConstantSubstituter<'_> {
    /// Remove branches whose condition becomes constant after substitution.
    ///
    /// Loop expansion can settle a structural index only after the source
    /// conditional was lowered. MLS §11.2.3 evaluates at most one branch, so a
    /// dead branch must not retain references such as `a[i - 1]` when the
    /// specialized `i` selects an earlier branch. Unknown conditions remain in
    /// source order; a later proven-true condition becomes their exact else
    /// value because no following branch is reachable.
    fn rewrite_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        let evaluation = rumoca_eval_flat::constant::EvalContext::new();
        let mut retained = Vec::with_capacity(branches.len());
        for (condition, value) in branches {
            let condition = self.rewrite_expression(condition)?;
            let constant = rumoca_eval_flat::constant::eval_expr(&condition, &evaluation)
                .ok()
                .and_then(|value| value.as_bool());
            match constant {
                Some(false) => {}
                Some(true) if retained.is_empty() => return self.rewrite_expression(value),
                Some(true) => {
                    return Ok(rumoca_core::Expression::If {
                        branches: retained,
                        else_branch: Box::new(self.rewrite_expression(value)?),
                        span,
                    });
                }
                None => retained.push((condition, self.rewrite_expression(value)?)),
            }
        }
        let else_branch = self.rewrite_expression(else_branch)?;
        if retained.is_empty() {
            Ok(else_branch)
        } else {
            Ok(rumoca_core::Expression::If {
                branches: retained,
                else_branch: Box::new(else_branch),
                span,
            })
        }
    }

    fn rewrite_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        if subscripts.is_empty() {
            if let Some(replaced) = substitute_scalar_var_ref(name, span, self.env)? {
                return Ok(replaced);
            }
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![],
                span,
            });
        }

        let rewritten_subscripts = self.rewrite_subscripts(subscripts)?;
        if self.env.locals.contains(name.as_str()) {
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span,
            });
        }
        if let Some(replaced) =
            substitute_indexed_constant_var_ref(name, rewritten_subscripts.clone(), span, self.env)?
        {
            return Ok(replaced);
        }

        Ok(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: rewritten_subscripts,
            span,
        })
    }

    fn rewrite_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        field_def_id: rumoca_core::DefId,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        let rewritten_base = self.rewrite_expression(base)?;
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = &rewritten_base
        {
            if let Some(named_arg) = named_constructor_arg(args, field) {
                return Ok(named_arg.clone().with_span(span));
            }
            if args.is_empty()
                && let Some(resolved) =
                    resolve_constant_field_access(name.as_str(), field, span, self.env.ctx)
            {
                return Ok(resolved);
            }
        }
        if let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = &rewritten_base
            && let Some(resolved) = resolve_indexed_constant_field_access(
                base,
                subscripts,
                field,
                span,
                self.env.ctx,
                self.env.live_vars,
            )
        {
            return self.rewrite_expression(&resolved);
        }
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &rewritten_base
            && subscripts.is_empty()
            && !self.env.live_vars.contains(name.as_str())
            && !flat_member_is_live(name.as_str(), field, self.env.live_vars)
            && !reference_root_is_local(name, self.env.locals)
            && name.is_generated()
            && let Some(resolved) =
                resolve_constant_field_access(name.as_str(), field, span, self.env.ctx)
        {
            return Ok(resolved);
        }
        Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(rewritten_base),
            field: field.to_string(),
            field_def_id,
            span,
        })
    }
}

/// True when `<base>.<field>` is itself a variable of the instantiated flat model.
///
/// That variable already carries the component modification, so the declaration
/// default recorded for the whole record `<base>` must not be folded in its
/// place (MLS §7.2.4).
fn flat_member_is_live(base: &str, field: &str, live_vars: &rustc_hash::FxHashSet<String>) -> bool {
    let mut member = String::with_capacity(base.len() + field.len() + 1);
    member.push_str(base);
    member.push('.');
    member.push_str(field);
    live_vars.contains(&member)
}

impl FallibleStatementRewriter for KnownConstantSubstituter<'_> {}

fn substitute_indexed_constant_var_ref(
    name: &rumoca_core::Reference,
    subscripts: Vec<rumoca_core::Subscript>,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    if env.live_vars.contains(name.as_str()) {
        return Ok(None);
    }

    // MLS §7.2: an element of a constant an extends modification of the
    // exposing package binds (`extends Base(table = [...])`, read as `table[:, 1]`)
    // selects from that binding, exactly as an unsubscripted read does.
    let modified = (!name.is_generated())
        .then(|| modified_constant_value(name.as_str(), env))
        .flatten();
    let constant_expr = if let Some((key, value)) = modified {
        substitute_resolved_generated_constant(&key, value, span, env)?
    } else if name.is_generated() {
        let Some(value) = resolve_constant_value_expr(name.as_str(), env.ctx) else {
            return Ok(None);
        };
        substitute_resolved_generated_constant(name.as_str(), value, span, env)?
    } else if let Some((identity, value)) = resolve_exposed_source_constant(name, span, env)? {
        substitute_resolved_source_constant(name.as_str(), identity, value, span, env)?
    } else {
        if name.target_def_id().is_none()
            && generated_constant_candidate_exists(name.as_str(), env.ctx, env.scope)
        {
            return Err(FlattenError::missing_source_context(format!(
                "non-generated indexed constant reference `{}` has no resolved declaration identity",
                name.as_str()
            )));
        }
        return Ok(None);
    };
    if let Some(element) = select_constant_element(&constant_expr, &subscripts) {
        return Ok(Some(element.clone().with_span(span)));
    }
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(constant_expr),
        subscripts,
        span,
    }))
}

/// The element a compile-time subscript tuple selects from a constant `{...}`
/// array value (MLS §10.5): `{"a", "b"}[2]` is `"b"`. Only literal in-range
/// indices through nested `{...}` constructors select; anything else keeps the
/// indexed expression.
fn select_constant_element<'value>(
    value: &'value rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Option<&'value rumoca_core::Expression> {
    let Some((first, rest)) = subscripts.split_first() else {
        return Some(value);
    };
    let rumoca_core::Expression::Array {
        elements,
        kind: rumoca_core::ArrayConstructor::Array,
        ..
    } = value
    else {
        return None;
    };
    let index = match first {
        rumoca_core::Subscript::Index { value, .. } => *value,
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => *value,
            _ => return None,
        },
        rumoca_core::Subscript::Colon { .. } => return None,
    };
    let element = elements.get(usize::try_from(index).ok()?.checked_sub(1)?)?;
    select_constant_element(element, rest)
}

fn substitute_scalar_var_ref(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let key = name.as_str();
    if env.live_vars.contains(key) || reference_root_is_local(name, env.locals) {
        return Ok(None);
    }
    if env.ctx.expanded_component_keys.contains(key) {
        return Ok(None);
    }
    if inline_index_base_is_live_or_local(key, env.live_vars, env.locals) {
        return Ok(None);
    }
    if name.is_generated() {
        return substitute_generated_scalar_var_ref(name, span, env);
    }
    if name.target_def_id().is_some() {
        return substitute_source_scalar_var_ref(name, span, env);
    }
    if generated_constant_candidate_exists(key, env.ctx, env.scope) {
        return Err(FlattenError::missing_source_context(format!(
            "non-generated constant reference `{key}` has no resolved declaration identity"
        )));
    }
    Ok(None)
}

fn substitute_source_scalar_var_ref(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    // MLS §7.2: a constant an extends modification of the exposing package
    // binds (`extends Base(k = 1)`) takes that binding, which outranks the
    // declaration default and is keyed by the rendered exposing-package name.
    if let Some((key, value)) = modified_constant_value(name.as_str(), env) {
        return substitute_resolved_generated_constant(&key, value, span, env).map(Some);
    }
    let Some((identity, value)) = resolve_exposed_source_constant(name, span, env)? else {
        return Ok(None);
    };
    Ok(Some(substitute_resolved_source_constant(
        name.as_str(),
        identity,
        value,
        span,
        env,
    )?))
}

fn substitute_generated_scalar_var_ref(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let key = name.as_str();
    let has_array_shape = reference_key_has_array_shape(key, env.ctx, env.scope);
    if env.prefer_scoped_parameters
        && !env.scope.is_empty()
        && let Some(expr) = substitute_scoped_scalar_var_ref(key, span, env)?
    {
        return Ok(Some(expr));
    }
    if let Some(v) = resolve_constant_value_expr_for_ref(name, env.ctx) {
        if has_array_shape && !constant_expr_preserves_array_shape(v) {
            return Ok(None);
        }
        return Ok(Some(substitute_resolved_generated_constant(
            key, v, span, env,
        )?));
    }
    if !has_array_shape && let Some(literal) = scalar_parameter_literal(key, span, env.ctx) {
        return Ok(Some(literal));
    }
    if let Some(expr) = resolve_inline_indexed_constant(key, span, env.ctx)? {
        return Ok(Some(expr));
    }
    if let Some(expr) = resolve_projected_constant_path(key, span, env.ctx) {
        let identity = ConstantExpansionId::Generated(key);
        if env.is_expanding(identity) {
            return Err(FlattenError::cyclic_constant_binding(
                key,
                env.expansion_chain(key),
                span,
            ));
        }
        let frame = ConstantExpansion {
            identity,
            display: key,
            parent: env.expanding,
        };
        let inner = ConstantSubstitutionEnv {
            expanding: Some(&frame),
            ..env
        };
        return Ok(Some(substitute_with_env(expr, inner)?));
    }
    if !env.prefer_scoped_parameters
        && !env.scope.is_empty()
        && let Some(expr) = substitute_scoped_scalar_var_ref(key, span, env)?
    {
        return Ok(Some(expr));
    }

    substitute_alias_resolved_scalar_var_ref(key, span, env)
}

fn substitute_alias_resolved_scalar_var_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some(resolved_key) = resolve_varref_through_constant_aliases(key, env.ctx, env.scope)
    else {
        return Ok(None);
    };
    if resolved_key == key {
        return Ok(None);
    }
    if let Some(v) = resolve_constant_value_expr(&resolved_key, env.ctx) {
        if reference_key_has_array_shape(&resolved_key, env.ctx, env.scope)
            && !constant_expr_preserves_array_shape(v)
        {
            return Ok(None);
        }
        return Ok(Some(substitute_resolved_generated_constant(
            &resolved_key,
            v,
            span,
            env,
        )?));
    }
    if !reference_key_has_array_shape(&resolved_key, env.ctx, env.scope)
        && let Some(literal) = scalar_parameter_literal(&resolved_key, span, env.ctx)
    {
        return Ok(Some(literal));
    }
    if let Some(expr) = resolve_inline_indexed_constant(&resolved_key, span, env.ctx)? {
        return Ok(Some(expr));
    }
    Ok(Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::generated(resolved_key),
        subscripts: vec![],
        span,
    }))
}

fn substitute_resolved_generated_constant(
    key: &str,
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let identity = ConstantExpansionId::Generated(key);
    if env.is_expanding(identity) {
        let cycle_name = env.expanding_display(identity).unwrap_or(key);
        return Err(FlattenError::cyclic_constant_binding(
            cycle_name,
            env.expansion_chain(key),
            span,
        ));
    }
    let declaration_scope = parent_component_scope(key);
    let scope = if declaration_scope.is_empty() {
        env.scope
    } else {
        &declaration_scope
    };
    let frame = ConstantExpansion {
        identity,
        display: key,
        parent: env.expanding,
    };
    let inner = ConstantSubstitutionEnv {
        ctx: env.ctx,
        live_vars: env.live_vars,
        locals: env.locals,
        scope,
        prefer_scoped_parameters: env.prefer_scoped_parameters,
        expanding: Some(&frame),
        exposures: env.exposures,
    };
    substitute_with_env(expr.clone().with_span(span), inner)
}

/// The binding an extends modification of the exposing package gives the
/// constant `key` names, read in `env.scope` (MLS §7.2): the rendered
/// exposing-package key itself, or, for a reference written inside the
/// package (`tableDensity` in a sibling binding), its scoped candidates.
/// Only a simple name is looked up through the enclosing scopes (MLS 3.7
/// §5.3.1); a composite name such as `world.g` starts at the component its
/// first part names and never reaches a same-spelled constant of an
/// enclosing scope.
fn modified_constant_value<'e>(
    key: &str,
    env: ConstantSubstitutionEnv<'e>,
) -> Option<(String, &'e rumoca_core::Expression)> {
    let scoped = if !key.contains('.') {
        scoped_lookup_candidates_with_scope(key, env.scope)
    } else {
        Vec::new()
    };
    std::iter::once(key.to_string())
        .chain(scoped.into_iter().map(|(candidate, _)| candidate))
        .find(|candidate| env.ctx.modified_constant_keys.contains(candidate.as_str()))
        .and_then(|candidate| {
            let value = resolve_constant_value_expr(&candidate, env.ctx)?;
            Some((candidate, value))
        })
}

fn substitute_resolved_source_constant(
    display: &str,
    semantic_id: SemanticConstantId,
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let identity = ConstantExpansionId::Semantic(semantic_id);
    if env.is_expanding(identity) {
        let cycle_name = env.expanding_display(identity).unwrap_or(display);
        return Err(FlattenError::cyclic_constant_binding(
            cycle_name,
            env.expansion_chain(display),
            span,
        ));
    }
    // The binding reads the package the constant is exposed through, so a
    // sibling constant it names resolves there (MLS §5.3.2).
    let exposing_scope = parent_component_scope(display);
    let frame = ConstantExpansion {
        identity,
        display,
        parent: env.expanding,
    };
    let inner = ConstantSubstitutionEnv {
        expanding: Some(&frame),
        scope: if exposing_scope.is_empty() {
            env.scope
        } else {
            &exposing_scope
        },
        // A declaration read as an element of the package that exposes it is
        // evaluated in that package: the names its binding reads resolve
        // there (MLS §7.3, §5.3), not in the declaring class.
        exposures: match semantic_id {
            SemanticConstantId::Exposure { package, .. } => {
                env.ctx.package_names(package).unwrap_or(env.exposures)
            }
            SemanticConstantId::Occurrence(_) | SemanticConstantId::Declaration(_) => env.exposures,
        },
        ..env
    };
    substitute_with_env(expr.clone().with_span(span), inner)
}

fn substitute_scoped_scalar_var_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    for (candidate, candidate_scope) in scoped_lookup_candidates_with_scope(key, env.scope) {
        if candidate == key {
            continue;
        }
        let candidate_has_array_shape =
            reference_key_has_array_shape(&candidate, env.ctx, &candidate_scope);
        if !candidate_has_array_shape
            && let Some(literal) = scalar_parameter_literal(&candidate, span, env.ctx)
        {
            return Ok(Some(literal));
        }
        if let Some(v) = resolve_constant_value_expr(&candidate, env.ctx) {
            if candidate_has_array_shape && !constant_expr_preserves_array_shape(v) {
                continue;
            }
            let candidate_env = env.with_scope(&candidate_scope);
            return Ok(Some(substitute_resolved_generated_constant(
                &candidate,
                v,
                span,
                candidate_env,
            )?));
        }
        if let Some(expr) = resolve_inline_indexed_constant(&candidate, span, env.ctx)? {
            return Ok(Some(expr));
        }
    }
    Ok(None)
}

fn inline_index_base_is_live_or_local(
    name: &str,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> bool {
    let Some((base, _indices)) = split_inline_indexed_name(name) else {
        return false;
    };
    live_vars.contains(base) || locals.contains(base)
}

fn reference_root_is_local(name: &rumoca_core::Reference, locals: &HashSet<String>) -> bool {
    if let Some(component_ref) = name.component_ref()
        && let Some(root) = component_ref.parts().first()
    {
        return locals.contains(root.ident.as_str());
    }

    rumoca_core::first_path_segment_without_index(name.as_str())
        .is_some_and(|root| locals.contains(root))
}

pub(super) fn substitute_known_constants_statement(
    statement: &mut rumoca_core::Statement,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<(), FlattenError> {
    substitute_exposed_constants_statement(statement, ctx, live_vars, locals, scope, &[])
}

/// [`substitute_known_constants_statement`] inside a function body exposed
/// through `exposures` (see `function_exposures`).
pub(super) fn substitute_exposed_constants_statement(
    statement: &mut rumoca_core::Statement,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    exposures: &[String],
) -> Result<(), FlattenError> {
    *statement = KnownConstantSubstituter {
        env: ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters: false,
            expanding: None,
            exposures,
        },
    }
    .rewrite_statement(statement)?;
    Ok(())
}

/// Resolve a source constant reference, taking the value the packages that
/// expose the enclosing function give the declaration (MLS §7.3).
fn resolve_exposed_source_constant<'a>(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'a>,
) -> Result<Option<(SemanticConstantId, &'a rumoca_core::Expression)>, FlattenError> {
    if let Some(declaration) = name.target_def_id()
        && let Some(value) = super::function_exposures::exposed_constant_value(
            env.ctx,
            env.exposures,
            declaration,
            name.as_str(),
            span,
        )?
    {
        return Ok(Some((SemanticConstantId::Declaration(declaration), value)));
    }
    Ok(resolve_source_constant(name, env.ctx))
}

/// Fold known constants in one expression of a function exposed through
/// `exposures` (see `function_exposures`).
pub(super) fn substitute_exposed_constants_expr(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    exposures: &[String],
) -> Result<rumoca_core::Expression, FlattenError> {
    substitute_with_env(
        expr,
        ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters: false,
            expanding: None,
            exposures,
        },
    )
}
