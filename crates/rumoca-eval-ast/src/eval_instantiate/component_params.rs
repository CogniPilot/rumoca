use super::{
    ConditionEvalEnv, InstantiateEvalCtx, ast, eval_scoped_string_condition_with_depth,
    get_enum_value_with_depth, resolve_class_constant_binding, resolve_component_ref_expr,
    try_eval_bool_literal, try_eval_integer_expr_with_depth, try_eval_real_expr_with_known,
};
use crate::ast_scalar::{self, AstScalarContext};
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;

/// Canonical predefined scalar root of a resolved AST component type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AstScalarKind {
    Boolean,
    Integer,
    Real,
}

/// Follow a resolved scalar type-alias chain to its exact predefined root.
///
/// A user class named `Boolean` is not Boolean merely because its display name
/// matches. Conversely, a type alias whose resolved chain ends at predefined
/// Boolean remains Boolean. Cycles, arrays, enumerations, non-type classes, and
/// unresolved identities have no scalar-kind proof.
pub fn canonical_scalar_kind(
    tree: &ast::ClassTree,
    component: &ast::Component,
) -> Option<AstScalarKind> {
    let mut def_id = component.type_def_id;
    let mut visited = FxHashSet::default();
    loop {
        let current_def_id = def_id?;
        if !visited.insert(current_def_id) {
            return None;
        }
        if let Some(kind) = predefined_scalar_kind_by_def_id(tree, current_def_id) {
            return Some(kind);
        }
        let class = tree.get_class_by_def_id(current_def_id)?;
        if class.class_type != rumoca_core::ClassType::Type
            || !class.enum_literals.is_empty()
            || !class.array_subscripts.is_empty()
        {
            return None;
        }
        let [base] = class.extends.as_slice() else {
            return None;
        };
        def_id = base.base_def_id;
    }
}

fn predefined_scalar_kind_by_def_id(
    tree: &ast::ClassTree,
    def_id: rumoca_core::DefId,
) -> Option<AstScalarKind> {
    [
        ("Boolean", AstScalarKind::Boolean),
        ("Integer", AstScalarKind::Integer),
        ("Real", AstScalarKind::Real),
    ]
    .into_iter()
    .find_map(|(name, kind)| {
        (tree
            .scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            == Some(def_id))
        .then_some(kind)
    })
}

/// Try to extract a string from an expression.
pub fn expr_to_string(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
            ..
        } => {
            // Remove surrounding quotes if present
            let s = token.text.trim().trim_matches('"').trim();
            (!s.is_empty()).then(|| s.to_string())
        }
        ast::Expression::ComponentReference(comp_ref) => {
            let value = ast::QualifiedName::from_component_reference(comp_ref).to_flat_string();
            (!value.is_empty()).then_some(value)
        }
        ast::Expression::Parenthesized { inner, .. } => expr_to_string(inner),
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } => Some(format!("{}{}", expr_to_string(lhs)?, expr_to_string(rhs)?)),
        _ => None,
    }
}

/// Try to evaluate an expression to a `String` value (MLS §4.9).
///
/// Only a genuine `String` literal — directly, or reached through references
/// that resolve to one — is returned. An enumeration literal keeps its
/// enumeration identity and is not rewritten as text, and a reference this
/// phase cannot resolve stays unknown rather than becoming its own spelling.
pub fn try_eval_string_expr(ctx: &InstantiateEvalCtx, expr: &ast::Expression) -> Option<String> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        *resolve_class_components,
        None,
        0,
    )?
    .into_string_literal()
}

/// Evaluate an MLS predefined `StateSelect` attribute expression.
pub fn eval_state_select_expr(
    ctx: &InstantiateEvalCtx,
    expr: &ast::Expression,
) -> Option<rumoca_core::StateSelect> {
    eval_state_select_expr_with_source_scope(ctx, expr, None)
}

/// Evaluate an MLS predefined `StateSelect` attribute expression in the lexical
/// scope where a modifier was written.
pub fn eval_state_select_expr_with_source_scope(
    ctx: &InstantiateEvalCtx,
    expr: &ast::Expression,
    source_scope: Option<&ast::QualifiedName>,
) -> Option<rumoca_core::StateSelect> {
    let env = ConditionEvalEnv {
        mod_env: ctx.mod_env,
        effective_components: ctx.effective_components,
        tree: ctx.tree,
        resolve_class_components: ctx.resolve_class_components,
    };
    let scope_prefix = source_scope.map(ast::QualifiedName::to_flat_string);
    eval_state_select_expr_with_depth(expr, env, scope_prefix.as_deref(), 0)
}

fn eval_state_select_expr_with_depth(
    expr: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<rumoca_core::StateSelect> {
    if depth > super::MAX_CONDITION_DEPTH {
        return None;
    }
    if let Some(value) = parse_state_select(expr) {
        return Some(value);
    }

    match expr {
        ast::Expression::ComponentReference(comp_ref) => {
            let (resolved_expr, next_scope) = resolve_component_ref_expr(
                comp_ref,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                scope_prefix,
            )?;
            eval_state_select_expr_with_depth(&resolved_expr, env, next_scope.as_deref(), depth + 1)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            eval_state_select_expr_with_depth(inner, env, scope_prefix, depth + 1)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_if_state_select(branches, else_branch, env, scope_prefix, depth + 1),
        ast::Expression::Array { elements, .. } => {
            let first = elements.first()?;
            if !elements.iter().all(|element| element == first) {
                return None;
            }
            eval_state_select_expr_with_depth(first, env, scope_prefix, depth + 1)
        }
        _ => None,
    }
}

fn eval_if_state_select(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<rumoca_core::StateSelect> {
    for (cond, branch_expr) in branches {
        match eval_scoped_string_condition_with_depth(cond, env, scope_prefix, depth) {
            Some(true) => {
                return eval_state_select_expr_with_depth(branch_expr, env, scope_prefix, depth);
            }
            Some(false) => continue,
            None => return None,
        }
    }
    eval_state_select_expr_with_depth(else_branch, env, scope_prefix, depth)
}

/// Parse a rumoca_core::StateSelect value from an expression.
pub fn parse_state_select(expr: &ast::Expression) -> Option<rumoca_core::StateSelect> {
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && comp_ref.parts.len() == 2
        && comp_ref.parts.first()?.ident.text.as_ref() == "StateSelect"
    {
        return state_select_from_modelica_literal(&comp_ref.parts.last()?.ident.text);
    }
    None
}

/// Parse an MLS predefined `StateSelect` enumeration literal name.
fn state_select_from_modelica_literal(value: &str) -> Option<rumoca_core::StateSelect> {
    match value {
        "never" => Some(rumoca_core::StateSelect::Never),
        "avoid" => Some(rumoca_core::StateSelect::Avoid),
        "default" => Some(rumoca_core::StateSelect::Default),
        "prefer" => Some(rumoca_core::StateSelect::Prefer),
        "always" => Some(rumoca_core::StateSelect::Always),
        _ => None,
    }
}

/// Extract binding from declaration or modification.
///
/// MLS §7.2: Modifications from outer scopes override inner bindings.
/// A modification like `p(k = 5.0)` provides a binding value for component k.
pub fn extract_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> (Option<ast::Expression>, bool, Option<ast::QualifiedName>) {
    // Check mod_env for binding override (outer modification takes precedence)
    // The binding modification is stored under just the component name
    let binding_path = ast::QualifiedName::from_ident(&comp.name);
    if let Some(mod_value) = mod_env.get(&binding_path)
        && let Some(binding_value) = mod_value.value.component_modifier_binding_value()
    {
        return (
            Some(binding_value.clone()),
            true,
            mod_value.source_scope.clone(),
        );
    }

    // Check if the component has an explicit binding from declaration
    // Use the dedicated `binding` field which preserves the binding even when
    // there's a separate start= modifier (e.g., `Real v(start=V0) = p.v - n.v`)
    //
    // MLS §4.9: `start` is an initialization attribute, not a binding equation,
    // so there is deliberately no fallback to it here — a component without a
    // binding contributes no binding equation.
    if let Some(binding) = &comp.binding {
        return (Some(binding.clone()), false, None);
    }

    (None, false, None)
}

/// Extract boolean parameter values from components for conditional equation evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// if use_numberPort then connect(numberPort, showNumber); else ... end if;
/// ```
///
/// Returns a map of component names to their boolean values.
/// Takes the modification environment to check for parameter overrides.
pub fn extract_bool_params_with_mods(ctx: &InstantiateEvalCtx<'_>) -> FxHashMap<String, bool> {
    extract_params_with_mods(
        ctx.effective_components,
        ctx.mod_env,
        |name, comp, expr, _mod_env| {
            if !component_allows_structural_evaluation(name, comp, ctx) {
                return None;
            }
            if canonical_scalar_kind(ctx.tree, comp) != Some(AstScalarKind::Boolean) {
                return None;
            }
            try_eval_bool_literal(expr)
        },
    )
}

/// Whether one parameter/constant occurrence may contribute a value to a
/// translation-time structural decision (MLS §4.5).
///
/// A binding can be numerically foldable while the occurrence is explicitly
/// excluded from translation-time evaluation by `fixed=false` or
/// `annotation(Evaluate=false)`. Keeping that policy beside parameter
/// extraction prevents callers from laundering such a value through a literal
/// map before the structural evaluator sees it.
pub fn component_allows_structural_evaluation(
    name: &str,
    component: &ast::Component,
    ctx: &InstantiateEvalCtx<'_>,
) -> bool {
    StructuralAttributeEvaluator::new(ctx).component_allows(name, component)
}

/// Evaluate a Boolean expression with the structural constant authority used
/// for `fixed` and `Evaluate` attributes.
///
/// `None` means that the expression is not decidable from the complete
/// instantiation-time constant environment; callers that require a
/// source-present attribute value must turn that outcome into a phase error.
pub fn try_eval_structural_boolean(
    ctx: &InstantiateEvalCtx<'_>,
    expression: &ast::Expression,
) -> Option<bool> {
    StructuralAttributeEvaluator::new(ctx).evaluate_boolean(expression)
}

/// Whether occurrence attributes make a parameter non-evaluable.
///
/// Absence preserves the MLS `fixed=true` default. Once the source supplies a
/// `fixed` or `Evaluate` expression, however, translation-time use requires a
/// proof that the expression evaluates to `true`; `false` and undecidable
/// expressions both block. Constants remain translation-time values regardless
/// of these parameter-only controls (MLS §4.5, §18.6).
pub fn component_explicitly_disables_structural_evaluation(
    name: &str,
    component: &ast::Component,
    ctx: &InstantiateEvalCtx<'_>,
) -> bool {
    !StructuralAttributeEvaluator::new(ctx).parameter_attributes_allow(name, component)
}

/// Whether the occurrence requests compile-time evaluation through `final` or
/// an `Evaluate` annotation whose expression proves `true`.
pub fn component_has_evaluate_annotation(
    component: &ast::Component,
    ctx: &InstantiateEvalCtx<'_>,
) -> bool {
    component.is_final
        || evaluate_annotation_expression(component).is_some_and(|expression| {
            StructuralAttributeEvaluator::new(ctx).proves_true(expression)
        })
}

/// Whether an applied occurrence-level `fixed` expression fails to prove
/// `true`. This covers selected record fields that are not entries in the
/// current class's component map but do have an exact occurrence modifier.
pub fn modification_environment_disables_structural_evaluation(
    occurrence: &str,
    ctx: &InstantiateEvalCtx<'_>,
) -> bool {
    ctx.mod_env
        .get_attr(occurrence, "fixed")
        .is_some_and(|expression| !StructuralAttributeEvaluator::new(ctx).proves_true(expression))
}

struct StructuralAttributeEvaluator<'ctx, 'ast> {
    ctx: &'ctx InstantiateEvalCtx<'ast>,
    active_components: RefCell<FxHashSet<String>>,
    active_values: RefCell<FxHashSet<String>>,
}

impl<'ctx, 'ast> StructuralAttributeEvaluator<'ctx, 'ast> {
    fn new(ctx: &'ctx InstantiateEvalCtx<'ast>) -> Self {
        Self {
            ctx,
            active_components: RefCell::new(FxHashSet::default()),
            active_values: RefCell::new(FxHashSet::default()),
        }
    }

    fn component_allows(&self, occurrence: &str, component: &ast::Component) -> bool {
        match component.variability {
            rumoca_core::Variability::Constant(_) => return true,
            rumoca_core::Variability::Parameter(_) => {}
            _ => return false,
        }

        self.parameter_attributes_allow(occurrence, component)
    }

    fn parameter_attributes_allow(&self, occurrence: &str, component: &ast::Component) -> bool {
        if !self
            .active_components
            .borrow_mut()
            .insert(occurrence.to_string())
        {
            return false;
        }
        let allows = self.effective_fixed_allows(occurrence, component)
            && evaluate_annotation_expression(component)
                .is_none_or(|expression| self.proves_true(expression));
        self.active_components.borrow_mut().remove(occurrence);
        allows
    }

    fn proves_true(&self, expression: &ast::Expression) -> bool {
        self.evaluate_boolean(expression) == Some(true)
    }

    fn evaluate_boolean(&self, expression: &ast::Expression) -> Option<bool> {
        ast_scalar::eval_boolean(expression, self, "", 0)
    }

    fn effective_fixed_allows(&self, occurrence: &str, component: &ast::Component) -> bool {
        self.ctx
            .mod_env
            .get_attr(occurrence, "fixed")
            .or_else(|| component.modifications.get("fixed"))
            .is_none_or(|expression| self.proves_true(expression))
    }

    fn occurrence_allows_reference(
        &self,
        reference: &ast::ComponentReference,
        dotted: &str,
    ) -> bool {
        let mut occurrence = String::new();
        for (index, part) in reference.parts.iter().enumerate() {
            if index != 0 {
                occurrence.push('.');
            }
            occurrence.push_str(part.ident.text.as_ref());
            let Some(def_id) = part.def_id else {
                continue;
            };
            let Some(component) = find_component_by_def_id(self.ctx.tree, def_id) else {
                continue;
            };
            let is_target = index + 1 == reference.parts.len();
            if !is_target
                && !matches!(
                    component.variability,
                    rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
                )
            {
                continue;
            }
            if !self.component_allows(&occurrence, component) {
                return false;
            }
        }

        if let Some(component) = self.ctx.effective_components.get(dotted) {
            return self.component_allows(dotted, component);
        }
        let Some(root) = reference.parts.first().map(|part| part.ident.text.as_ref()) else {
            return false;
        };
        self.ctx
            .effective_components
            .get(root)
            .is_none_or(|component| {
                reference.parts.len() > 1
                    && !matches!(
                        component.variability,
                        rumoca_core::Variability::Parameter(_)
                            | rumoca_core::Variability::Constant(_)
                    )
                    || self.component_allows(root, component)
            })
    }

    fn reference_expression(&self, reference: &ast::ComponentReference) -> Option<ast::Expression> {
        let dotted = component_ref_to_dotted_no_subscripts(reference)?;
        if !self.occurrence_allows_reference(reference, &dotted) {
            return None;
        }
        let path = ast::QualifiedName::from_dotted(&dotted);
        if let Some(modification) = self.ctx.mod_env.get(&path)
            && let Some(binding) = modification.value.component_modifier_binding_value()
        {
            return Some(binding.clone());
        }
        if let Some(component) = self.ctx.effective_components.get(dotted.as_str()) {
            return component_expr_for_structural_eval(component).cloned();
        }
        if reference.parts.len() == 1
            && let Some(component) = self
                .ctx
                .effective_components
                .get(reference.parts[0].ident.text.as_ref())
        {
            return component_expr_for_structural_eval(component).cloned();
        }
        if let Some(target) = reference
            .target_def_id()
            .and_then(|def_id| find_component_by_def_id(self.ctx.tree, def_id))
        {
            return component_expr_for_structural_eval(target).cloned();
        }
        resolve_class_constant_binding(reference, self.ctx.tree, self.ctx.resolve_class_components)
    }

    fn evaluate_reference<T>(
        &self,
        expression: &ast::Expression,
        depth: usize,
        eval: impl FnOnce(&ast::Expression, &Self, usize) -> Option<T>,
    ) -> Option<T> {
        let ast::Expression::ComponentReference(reference) = expression else {
            return None;
        };
        let dotted = component_ref_to_dotted_no_subscripts(reference)?;
        if !self.active_values.borrow_mut().insert(dotted.clone()) {
            return None;
        }
        let result = self
            .reference_expression(reference)
            .and_then(|binding| eval(&binding, self, depth));
        self.active_values.borrow_mut().remove(&dotted);
        result
    }
}

impl AstScalarContext for StructuralAttributeEvaluator<'_, '_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(super::MAX_EXPR_EVAL_DEPTH)
    }

    fn lookup_integer(
        &self,
        expression: &ast::Expression,
        _scope: &str,
        depth: usize,
    ) -> Option<i64> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_integer(binding, ctx, "", depth)
        })
    }

    fn lookup_real(&self, expression: &ast::Expression, _scope: &str, depth: usize) -> Option<f64> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_real(binding, ctx, "", depth)
        })
    }

    fn lookup_boolean(
        &self,
        expression: &ast::Expression,
        _scope: &str,
        depth: usize,
    ) -> Option<bool> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_boolean(binding, ctx, "", depth)
        })
    }

    fn call_integer(
        &self,
        _function: &ast::ComponentReference,
        _args: &[ast::Expression],
        _scope: &str,
        _depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        None
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
    }
}

fn evaluate_annotation_expression(component: &ast::Component) -> Option<&ast::Expression> {
    component.annotation.iter().find_map(|entry| match entry {
        ast::Expression::Modification {
            target,
            value: Some(value),
            ..
        } if reference_is_single_name(target, "Evaluate") => Some(value.as_ref()),
        ast::Expression::NamedArgument { name, value, .. } if name.text.as_ref() == "Evaluate" => {
            Some(value.as_ref())
        }
        _ => None,
    })
}

fn reference_is_single_name(reference: &ast::ComponentReference, expected: &str) -> bool {
    reference.parts.len() == 1
        && reference.parts[0].subs.is_none()
        && reference.parts[0].ident.text.as_ref() == expected
}

fn find_component_by_def_id(
    tree: &ast::ClassTree,
    target: rumoca_core::DefId,
) -> Option<&ast::Component> {
    fn find_in_class(class: &ast::ClassDef, target: rumoca_core::DefId) -> Option<&ast::Component> {
        if let Some(component) = class
            .components
            .values()
            .find(|component| component.def_id == Some(target))
        {
            return Some(component);
        }
        class
            .classes
            .values()
            .find_map(|nested| find_in_class(nested, target))
    }

    tree.definitions
        .classes
        .values()
        .find_map(|class| find_in_class(class, target))
}

/// Extract scalar Real parameter values from a class's components (MLS §4.4.5).
///
/// Conditional-component conditions compare Real parameters — `Parts.Body` gates
/// its `sphere` visualiser on `sphereDiameter > 0` — so an `inner` class's Real
/// parameters must be known before a nested `outer` reference reads them.
///
/// Only scalar `parameter`/`constant` declarations whose value expression folds to
/// a finite Real are recorded; anything else stays absent rather than acquiring a
/// substitute value (SPEC_0008). Array declarations are excluded because a single
/// dotted key cannot name one of their elements.
///
/// `known` supplies values already settled for this class — the modifications an
/// enclosing scope wrote on the instance — so a declaration derived from a
/// modified parameter (`defaultBodyDiameter = nominalLength/9`) folds against the
/// modified value rather than the class default it replaced (MLS §7.2).
pub fn extract_real_params_with_mods(
    ctx: &InstantiateEvalCtx,
    known: &FxHashMap<String, f64>,
) -> FxHashMap<String, f64> {
    extract_params_with_mods(
        ctx.effective_components,
        ctx.mod_env,
        |name, comp, expr, _mod_env| {
            if !component_allows_structural_evaluation(name, comp, ctx)
                || !comp.shape.is_empty()
                || !comp.shape_expr.is_empty()
            {
                return None;
            }
            try_eval_real_expr_with_known(ctx, expr, known)
        },
    )
}

/// Extract integer parameter values from components for for-loop range evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// for k in 1:m loop connect(plug_p.pin[k], resistor[k].p); end for;
/// ```
///
/// Returns a map of component names to their integer values.
/// Takes the modification environment to check for parameter overrides.
pub fn extract_int_params_with_mods(ctx: &InstantiateEvalCtx) -> FxHashMap<String, i64> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    let mut int_params = extract_params_with_mods(
        effective_components,
        mod_env,
        |name, comp, expr, mod_env| {
            if !component_allows_structural_evaluation(name, comp, ctx) {
                return None;
            }
            try_eval_integer_expr_with_depth(
                expr,
                mod_env,
                effective_components,
                tree,
                *resolve_class_components,
                0,
            )
        },
    );

    // Also add dotted keys from multi-part modifications in mod_env.
    // This handles record field references like cellData.nRC used in for-loop ranges.
    for (qn, mod_value) in &mod_env.active {
        if qn.parts.len() > 1 {
            let dotted_key: String = qn
                .parts
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if int_params.contains_key(&dotted_key) {
                continue;
            }
            if let Some(value) = try_eval_integer_expr_with_depth(
                &mod_value.value,
                mod_env,
                effective_components,
                tree,
                *resolve_class_components,
                0,
            ) {
                int_params.insert(dotted_key, value);
            }
        }
    }

    // If a record parameter is rebound by reference (e.g., `cellData = cellData2`),
    // propagate integer fields from the target record (`cellData2.nRC`) to the
    // bound name (`cellData.nRC`) so for-loop ranges use the overridden values.
    propagate_record_alias_integer_params(&mut int_params, mod_env);

    int_params
}

fn extract_params_with_mods<T, F>(
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    mut eval: F,
) -> FxHashMap<String, T>
where
    F: FnMut(&str, &ast::Component, &ast::Expression, &ast::ModificationEnvironment) -> Option<T>,
{
    let mut params = FxHashMap::default();

    for (name, comp) in effective_components {
        let mod_path = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&mod_path)
            && let Some(binding) = mod_value.value.component_modifier_binding_value()
        {
            // MLS §7.2.3/§7.2.4: a present occurrence binding replaces
            // the declaration binding even when this phase cannot evaluate it.
            // Falling through here would launder an unknown override into the
            // declaration default and could choose the wrong structural branch.
            if let Some(value) = eval(name, comp, binding, mod_env) {
                params.insert(name.clone(), value);
            }
            continue;
        }

        if let Some(value_expr) = component_expr_for_structural_eval(comp)
            && let Some(value) = eval(name, comp, value_expr, mod_env)
        {
            params.insert(name.clone(), value);
        }
    }

    params
}

/// Return the declaration-side value expression for structural evaluation.
///
/// MLS §4.4.4 / §7.2: a component's value comes from its declaration binding
/// (`x = expr`) or from an applied modification. MLS §4.9 makes `start` an
/// initial *guess* for a simulation variable, never a value, and the parser
/// seeds `start` with the declared type's default (`0`, `0.0`, `false`) for
/// every component — so reading it here would answer "what is this parameter?"
/// with a number the model never wrote.
///
/// A component with no binding therefore has no compile-time value and this
/// returns `None` (SPEC_0008: recovery by substituting an invented value is
/// prohibited). Callers must treat `None` as undecidable.
pub(crate) fn component_expr_for_structural_eval(
    comp: &ast::Component,
) -> Option<&ast::Expression> {
    comp.binding.as_ref()
}

/// Generate enclosing-scope lookup paths for a dotted reference.
///
/// Example: `pipe2.flowModel.nFM` -> [`pipe2.nFM`, `nFM`]
/// This models lexical scope climbing for nested component members.
pub(crate) fn enclosing_scope_candidates(dotted: &str) -> Vec<String> {
    let mut parts = rumoca_core::ComponentPath::from_flat_path(dotted).into_parts();
    let mut candidates = Vec::new();
    while parts.len() > 1 {
        let remove_idx = parts.len() - 2;
        parts.remove(remove_idx);
        candidates.push(parts.join("."));
    }
    candidates
}

pub fn propagate_record_alias_integer_params(
    int_params: &mut FxHashMap<String, i64>,
    mod_env: &ast::ModificationEnvironment,
) {
    let aliases = collect_record_aliases(mod_env);
    if aliases.is_empty() {
        return;
    }

    // Resolve simple alias chains (a=b, b=c) without unbounded growth.
    const MAX_ALIAS_PROPAGATION_PASSES: usize = 8;
    for _ in 0..MAX_ALIAS_PROPAGATION_PASSES {
        let mut changed = false;

        for (alias_name, target_name) in &aliases {
            if alias_name == target_name {
                continue;
            }
            changed |= propagate_integer_alias(int_params, alias_name, target_name);
        }

        if !changed {
            break;
        }
    }
}

/// Propagate record-field integer values through modifier aliases using the
/// modifier's lexical source scope.
///
/// A nested instance can contain a textual modifier such as
/// `cellData = cellData`, where the left-hand name belongs to the nested
/// instance and the right-hand name belongs to an enclosing scope. The
/// `ModificationValue::source_scope` metadata distinguishes those paths.
pub fn propagate_scoped_record_alias_integer_params(
    int_params: &mut FxHashMap<String, i64>,
    mod_env: &ast::ModificationEnvironment,
    instance_scope: &ast::QualifiedName,
) {
    let aliases = collect_scoped_record_aliases(mod_env, instance_scope);
    if aliases.is_empty() {
        return;
    }

    const MAX_ALIAS_PROPAGATION_PASSES: usize = 8;
    for _ in 0..MAX_ALIAS_PROPAGATION_PASSES {
        let mut changed = false;
        for (alias_name, target_name) in &aliases {
            if alias_name == target_name {
                continue;
            }
            changed |= propagate_integer_alias(int_params, alias_name, target_name);
        }
        if !changed {
            break;
        }
    }
}

fn collect_scoped_record_aliases(
    mod_env: &ast::ModificationEnvironment,
    instance_scope: &ast::QualifiedName,
) -> Vec<(String, String)> {
    let mut aliases = Vec::new();
    for (target_qn, mod_value) in &mod_env.active {
        if target_qn.parts.len() != 1 {
            continue;
        }
        let source_expr = mod_value.source.as_ref().unwrap_or(&mod_value.value);
        let ast::Expression::ComponentReference(comp_ref) = source_expr else {
            continue;
        };
        let Some(target_relative) = component_ref_to_dotted_no_subscripts(comp_ref) else {
            continue;
        };
        let alias_name = instance_scope.join(target_qn).to_flat_string();
        let target_scope = mod_value.source_scope.as_ref().unwrap_or(instance_scope);
        let target_name = target_scope
            .join(&ast::QualifiedName::from_dotted(&target_relative))
            .to_flat_string();
        aliases.push((alias_name, target_name));
    }
    aliases
}

fn propagate_integer_alias(
    int_params: &mut FxHashMap<String, i64>,
    alias_name: &str,
    target_name: &str,
) -> bool {
    let mut changed = false;
    if let Some(value) = int_params.get(target_name).copied() {
        let previous = int_params.insert(alias_name.to_string(), value);
        changed |= previous != Some(value);
    }

    let target_prefix = format!("{target_name}.");
    let alias_prefix = format!("{alias_name}.");
    let propagated = int_params
        .iter()
        .filter_map(|(key, value)| {
            key.strip_prefix(&target_prefix)
                .map(|suffix| (format!("{alias_prefix}{suffix}"), *value))
        })
        .collect::<Vec<_>>();
    for (key, value) in propagated {
        let previous = int_params.insert(key, value);
        changed |= previous != Some(value);
    }
    changed
}

fn collect_record_aliases(mod_env: &ast::ModificationEnvironment) -> Vec<(String, String)> {
    let mut aliases = Vec::new();
    for (target_qn, mod_value) in &mod_env.active {
        if target_qn.parts.len() != 1 {
            continue;
        }
        let Some(alias_name) = target_qn.first_name() else {
            continue;
        };
        let ast::Expression::ComponentReference(comp_ref) = &mod_value.value else {
            continue;
        };
        let Some(target_name) = component_ref_to_dotted_no_subscripts(comp_ref) else {
            continue;
        };
        aliases.push((alias_name.to_string(), target_name));
    }
    aliases
}

pub(crate) fn component_ref_to_dotted_no_subscripts(
    comp_ref: &ast::ComponentReference,
) -> Option<String> {
    if comp_ref.parts.is_empty() || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(
        comp_ref
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join("."),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("component_params_test.mo"),
            1,
            2,
        )
    }

    fn bool_literal(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: rumoca_core::Token {
                text: Arc::from(if value { "true" } else { "false" }),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn integer_literal(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: Arc::from(value.to_string()),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn real_literal(value: &str) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token: rumoca_core::Token {
                text: Arc::from(value),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn scalar_param_with_binding(
        name: &str,
        type_name: &str,
        binding: ast::Expression,
    ) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            type_name: ast::Name::from_string(type_name),
            variability: rumoca_core::Variability::Parameter(Default::default()),
            binding: Some(binding),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        }
    }

    /// A component carrying `value` only in its `start` attribute — the shape
    /// the parser produces for `parameter Boolean b;` (MLS §4.9 default start)
    /// and for `parameter Boolean b(start = value);`.
    fn bool_param_with_start(name: &str, type_name: &str, value: bool) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            type_name: ast::Name::from_string(type_name),
            variability: rumoca_core::Variability::Parameter(Default::default()),
            start: bool_literal(value),
            ..ast::Component::empty_with_span(test_span())
        }
    }

    fn bool_param_with_binding(name: &str, type_name: &str, value: bool) -> ast::Component {
        scalar_param_with_binding(name, type_name, bool_literal(value))
    }

    fn component_ref_expr(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from(name),
                    ..Default::default()
                },
                subs: None,
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    }

    fn not(expression: ast::Expression) -> ast::Expression {
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Arc::new(expression),
            span: test_span(),
        }
    }

    fn parenthesized(expression: ast::Expression) -> ast::Expression {
        ast::Expression::Parenthesized {
            inner: Arc::new(expression),
            span: test_span(),
        }
    }

    fn evaluate_annotation(value: ast::Expression) -> ast::Expression {
        ast::Expression::Modification {
            target: match component_ref_expr("Evaluate") {
                ast::Expression::ComponentReference(reference) => reference,
                _ => unreachable!(),
            },
            value: Some(Arc::new(value)),
            span: test_span(),
        }
    }

    fn no_op_resolve_class_components(
        _tree: &ast::ClassTree,
        _class: &ast::ClassDef,
    ) -> IndexMap<String, ast::Component> {
        IndexMap::default()
    }

    fn test_predefined_scalar_id(offset: u32) -> rumoca_core::DefId {
        rumoca_core::DefId::new(80_000 + offset)
    }

    fn test_tree_with_predefined_scalars() -> ast::ClassTree {
        let mut tree = ast::ClassTree::new();
        for (offset, name) in [(1, "Boolean"), (2, "Integer"), (3, "Real")] {
            tree.scope_tree.add_predefined_member(
                rumoca_core::ComponentPath::from_flat_path(name),
                test_predefined_scalar_id(offset),
            );
        }
        tree
    }

    /// Simulate the resolver evidence consumed by `canonical_scalar_kind`.
    /// Display spellings are used only to build this test input; production
    /// classification sees the assigned identity and has no name fallback.
    fn resolved_test_scalar_components(
        components: &IndexMap<String, ast::Component>,
    ) -> IndexMap<String, ast::Component> {
        let mut resolved = components.clone();
        for component in resolved.values_mut() {
            if component.type_def_id.is_some() {
                continue;
            }
            let name = component.type_name.to_string();
            component.type_def_id = [(1, "Boolean"), (2, "Integer"), (3, "Real")]
                .into_iter()
                .find_map(|(offset, predefined)| {
                    rumoca_core::qualified_type_name_matches(&name, predefined)
                        .then(|| test_predefined_scalar_id(offset))
                });
        }
        resolved
    }

    fn extract_test_bool_params(
        components: &IndexMap<String, ast::Component>,
        mod_env: &ast::ModificationEnvironment,
    ) -> FxHashMap<String, bool> {
        let tree = test_tree_with_predefined_scalars();
        let components = resolved_test_scalar_components(components);
        extract_test_bool_params_with_tree(&tree, &components, mod_env)
    }

    fn extract_test_bool_params_with_tree(
        tree: &ast::ClassTree,
        components: &IndexMap<String, ast::Component>,
        mod_env: &ast::ModificationEnvironment,
    ) -> FxHashMap<String, bool> {
        extract_bool_params_with_mods(&InstantiateEvalCtx {
            tree,
            mod_env,
            effective_components: components,
            resolve_class_components: no_op_resolve_class_components,
        })
    }

    fn attribute_only_modification(name: &str) -> ast::Expression {
        ast::Expression::ClassModification {
            target: match component_ref_expr(name) {
                ast::Expression::ComponentReference(reference) => reference,
                _ => unreachable!(),
            },
            modifications: vec![ast::Expression::NamedArgument {
                name: rumoca_core::Token {
                    text: Arc::from("fixed"),
                    ..Default::default()
                },
                value: Arc::new(bool_literal(true)),
                span: test_span(),
            }],
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: test_span(),
        }
    }

    #[test]
    fn occurrence_binding_precedence_never_falls_back_to_declaration_defaults() {
        let components = resolved_test_scalar_components(&IndexMap::from_iter([
            (
                "b".to_string(),
                scalar_param_with_binding("b", "Boolean", bool_literal(true)),
            ),
            (
                "i".to_string(),
                scalar_param_with_binding("i", "Integer", integer_literal(1)),
            ),
            (
                "r".to_string(),
                scalar_param_with_binding("r", "Real", real_literal("1.0")),
            ),
        ]));
        let tree = test_tree_with_predefined_scalars();

        let mut known = ast::ModificationEnvironment::new();
        known.add(
            ast::QualifiedName::from_ident("b"),
            ast::ModificationValue::simple(bool_literal(false)),
        );
        known.add(
            ast::QualifiedName::from_ident("i"),
            ast::ModificationValue::simple(integer_literal(2)),
        );
        known.add(
            ast::QualifiedName::from_ident("r"),
            ast::ModificationValue::simple(real_literal("2.5")),
        );
        let known_ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &known,
            effective_components: &components,
            resolve_class_components: no_op_resolve_class_components,
        };
        assert!(!extract_bool_params_with_mods(&known_ctx)["b"]);
        assert_eq!(extract_int_params_with_mods(&known_ctx)["i"], 2);
        assert_eq!(
            extract_real_params_with_mods(&known_ctx, &FxHashMap::default())["r"],
            2.5
        );

        let mut unknown = ast::ModificationEnvironment::new();
        for name in ["b", "i", "r"] {
            unknown.add(
                ast::QualifiedName::from_ident(name),
                ast::ModificationValue::simple(component_ref_expr("missing")),
            );
        }
        let unknown_ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &unknown,
            effective_components: &components,
            resolve_class_components: no_op_resolve_class_components,
        };
        assert!(!extract_bool_params_with_mods(&unknown_ctx).contains_key("b"));
        assert!(!extract_int_params_with_mods(&unknown_ctx).contains_key("i"));
        assert!(
            !extract_real_params_with_mods(&unknown_ctx, &FxHashMap::default()).contains_key("r")
        );

        let mut attributes = ast::ModificationEnvironment::new();
        for name in ["b", "i", "r"] {
            attributes.add(
                ast::QualifiedName::from_ident(name),
                ast::ModificationValue::simple(attribute_only_modification(name)),
            );
        }
        let attribute_ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &attributes,
            effective_components: &components,
            resolve_class_components: no_op_resolve_class_components,
        };
        assert!(extract_bool_params_with_mods(&attribute_ctx)["b"]);
        assert_eq!(extract_int_params_with_mods(&attribute_ctx)["i"], 1);
        assert_eq!(
            extract_real_params_with_mods(&attribute_ctx, &FxHashMap::default())["r"],
            1.0
        );
    }

    #[test]
    fn bool_param_extraction_requires_boolean_type_segment() {
        let mut components = IndexMap::default();
        components.insert(
            "plain".to_string(),
            bool_param_with_binding("plain", "Boolean", true),
        );
        components.insert(
            "qualified".to_string(),
            bool_param_with_binding("qualified", "Modelica.Boolean", false),
        );
        components.insert(
            "nested".to_string(),
            bool_param_with_binding("nested", "Pkg.Types.Boolean", true),
        );
        components.insert(
            "prefix_lookalike".to_string(),
            bool_param_with_binding("prefix_lookalike", "MyBoolean", true),
        );
        components.insert(
            "suffix_lookalike".to_string(),
            bool_param_with_binding("suffix_lookalike", "Pkg.BooleanAlias", true),
        );

        let bool_params =
            extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bool_params.get("plain"), Some(&true));
        assert_eq!(bool_params.get("qualified"), Some(&false));
        assert_eq!(bool_params.get("nested"), Some(&true));
        assert!(!bool_params.contains_key("prefix_lookalike"));
        assert!(!bool_params.contains_key("suffix_lookalike"));
    }

    #[test]
    fn bool_extraction_uses_canonical_resolved_type_identity() {
        let builtin_boolean_id = rumoca_core::DefId::new(900);
        let boolean_alias_id = rumoca_core::DefId::new(901);
        let shadow_boolean_id = rumoca_core::DefId::new(902);
        let mut boolean_alias = ast::ClassDef {
            def_id: Some(boolean_alias_id),
            name: rumoca_core::Token {
                text: Arc::from("BooleanAlias"),
                ..Default::default()
            },
            class_type: rumoca_core::ClassType::Type,
            ..Default::default()
        };
        boolean_alias.extends.push(ast::Extend {
            base_name: ast::Name::from_string("Boolean"),
            base_def_id: Some(builtin_boolean_id),
            ..Default::default()
        });
        let mut shadow_boolean = ast::ClassDef {
            def_id: Some(shadow_boolean_id),
            name: rumoca_core::Token {
                text: Arc::from("Boolean"),
                ..Default::default()
            },
            class_type: rumoca_core::ClassType::Type,
            ..Default::default()
        };
        shadow_boolean.extends.push(ast::Extend {
            base_name: ast::Name::from_string("Real"),
            ..Default::default()
        });
        let mut tree = ast::ClassTree::new();
        tree.scope_tree.add_predefined_member(
            rumoca_core::ComponentPath::from_flat_path("Boolean"),
            builtin_boolean_id,
        );
        tree.definitions
            .classes
            .insert("BooleanAlias".to_string(), boolean_alias);
        tree.definitions
            .classes
            .insert("Boolean".to_string(), shadow_boolean);
        tree.def_map
            .insert(boolean_alias_id, "BooleanAlias".to_string());
        tree.def_map
            .insert(shadow_boolean_id, "Boolean".to_string());

        let mut direct = bool_param_with_binding("direct", "Boolean", true);
        direct.type_def_id = Some(builtin_boolean_id);
        let mut alias = bool_param_with_binding("alias", "BooleanAlias", true);
        alias.type_def_id = Some(boolean_alias_id);
        let mut shadow = bool_param_with_binding("shadow", "Boolean", true);
        shadow.type_def_id = Some(shadow_boolean_id);
        let unresolved = bool_param_with_binding("unresolved", "Boolean", true);
        let components = IndexMap::from_iter([
            ("direct".to_string(), direct),
            ("alias".to_string(), alias),
            ("shadow".to_string(), shadow),
            ("unresolved".to_string(), unresolved),
        ]);

        let values = extract_test_bool_params_with_tree(
            &tree,
            &components,
            &ast::ModificationEnvironment::new(),
        );

        assert_eq!(values.get("direct"), Some(&true));
        assert_eq!(values.get("alias"), Some(&true));
        assert!(
            !values.contains_key("shadow"),
            "a Real alias named Boolean must not enter the Boolean map"
        );
        assert!(
            !values.contains_key("unresolved"),
            "a Boolean spelling without resolved type identity is not predefined-type evidence"
        );
    }

    #[test]
    fn bool_constant_is_available_to_structural_conditions() {
        let mut components = IndexMap::default();
        let mut constant = bool_param_with_binding("enabled", "Boolean", true);
        constant.variability = rumoca_core::Variability::Constant(Default::default());
        constant.annotation.push(ast::Expression::Modification {
            target: match component_ref_expr("Evaluate") {
                ast::Expression::ComponentReference(reference) => reference,
                _ => unreachable!(),
            },
            value: Some(Arc::new(bool_literal(false))),
            span: test_span(),
        });
        components.insert("enabled".to_string(), constant);

        let bools = extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bools.get("enabled"), Some(&true));
    }

    #[test]
    fn explicitly_nonevaluable_boolean_is_absent_from_structural_values() {
        let mut fixed_false = bool_param_with_binding("fixedFalse", "Boolean", true);
        fixed_false
            .modifications
            .insert("fixed".to_string(), bool_literal(false));
        let mut evaluate_false = bool_param_with_binding("evaluateFalse", "Boolean", true);
        evaluate_false
            .annotation
            .push(ast::Expression::Modification {
                target: match component_ref_expr("Evaluate") {
                    ast::Expression::ComponentReference(reference) => reference,
                    _ => unreachable!(),
                },
                value: Some(Arc::new(bool_literal(false))),
                span: test_span(),
            });
        let mut components = IndexMap::from_iter([
            ("fixedFalse".to_string(), fixed_false),
            ("evaluateFalse".to_string(), evaluate_false),
        ]);
        components.insert(
            "modifiedFixedFalse".to_string(),
            bool_param_with_binding("modifiedFixedFalse", "Boolean", true),
        );
        let mut mod_env = ast::ModificationEnvironment::new();
        mod_env.add(
            ast::QualifiedName::from_ident("modifiedFixedFalse").child("fixed"),
            ast::ModificationValue::simple(bool_literal(false)),
        );

        let bools = extract_test_bool_params(&components, &mod_env);

        assert!(
            bools.is_empty(),
            "non-evaluable literals must not enter the structural environment"
        );
    }

    #[test]
    fn structural_attributes_require_a_semantic_true_proof() {
        let mut yes = bool_param_with_binding("yes", "Boolean", true);
        yes.variability = rumoca_core::Variability::Constant(Default::default());

        let mut fixed_expression = bool_param_with_binding("fixedExpression", "Boolean", true);
        fixed_expression
            .modifications
            .insert("fixed".to_string(), parenthesized(not(bool_literal(false))));
        let mut fixed_reference = bool_param_with_binding("fixedReference", "Boolean", true);
        fixed_reference
            .modifications
            .insert("fixed".to_string(), component_ref_expr("yes"));
        let mut evaluate_reference = bool_param_with_binding("evaluateReference", "Boolean", true);
        evaluate_reference
            .annotation
            .push(evaluate_annotation(component_ref_expr("yes")));
        let mut fixed_false = bool_param_with_binding("fixedFalseExpr", "Boolean", true);
        fixed_false
            .modifications
            .insert("fixed".to_string(), not(bool_literal(true)));
        let mut evaluate_unknown = bool_param_with_binding("evaluateUnknown", "Boolean", true);
        evaluate_unknown
            .annotation
            .push(evaluate_annotation(component_ref_expr("missing")));
        let mut occurrence_true = bool_param_with_binding("occurrenceTrue", "Boolean", true);
        occurrence_true
            .modifications
            .insert("fixed".to_string(), bool_literal(false));
        let occurrence_unknown = bool_param_with_binding("occurrenceUnknown", "Boolean", true);

        let components = IndexMap::from_iter([
            ("yes".to_string(), yes),
            ("fixedExpression".to_string(), fixed_expression),
            ("fixedReference".to_string(), fixed_reference),
            ("evaluateReference".to_string(), evaluate_reference),
            ("fixedFalseExpr".to_string(), fixed_false),
            ("evaluateUnknown".to_string(), evaluate_unknown),
            ("occurrenceTrue".to_string(), occurrence_true),
            ("occurrenceUnknown".to_string(), occurrence_unknown),
        ]);
        let mut mod_env = ast::ModificationEnvironment::new();
        mod_env.add(
            ast::QualifiedName::from_ident("occurrenceTrue").child("fixed"),
            ast::ModificationValue::simple(component_ref_expr("yes")),
        );
        mod_env.add(
            ast::QualifiedName::from_ident("occurrenceUnknown").child("fixed"),
            ast::ModificationValue::simple(component_ref_expr("missing")),
        );

        let values = extract_test_bool_params(&components, &mod_env);

        for name in [
            "yes",
            "fixedExpression",
            "fixedReference",
            "evaluateReference",
            "occurrenceTrue",
        ] {
            assert_eq!(values.get(name), Some(&true), "{name} must prove true");
        }
        for name in ["fixedFalseExpr", "evaluateUnknown", "occurrenceUnknown"] {
            assert!(
                !values.contains_key(name),
                "{name} must not enter structural values"
            );
        }
    }

    #[test]
    fn cyclic_structural_attribute_dependencies_fail_closed() {
        let mut left = bool_param_with_binding("left", "Boolean", true);
        left.modifications
            .insert("fixed".to_string(), component_ref_expr("right"));
        let mut right = bool_param_with_binding("right", "Boolean", true);
        right
            .modifications
            .insert("fixed".to_string(), component_ref_expr("left"));
        let components =
            IndexMap::from_iter([("left".to_string(), left), ("right".to_string(), right)]);

        let values = extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert!(values.is_empty());
    }

    /// MLS §4.9: `start` is an initial guess, not a value. The parser seeds
    /// every `Boolean` declaration with `start = false`, so reading it would
    /// invent `false` for a parameter the model never bound (SPEC_0008).
    #[test]
    fn bool_param_extraction_ignores_start_attribute_value() {
        let mut components = IndexMap::default();
        components.insert(
            "unbound".to_string(),
            bool_param_with_start("unbound", "Boolean", false),
        );

        let bool_params =
            extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert!(!bool_params.contains_key("unbound"));
    }

    /// A `start = expr` modifier alongside a binding must not shadow the
    /// binding (MLS §4.4.4: the binding equation supplies the value).
    #[test]
    fn bool_param_extraction_prefers_binding_over_start_modifier() {
        let mut components = IndexMap::default();
        let mut comp = bool_param_with_binding("useHeatPort", "Boolean", true);
        comp.start = bool_literal(false);
        comp.start_is_modification = true;
        components.insert("useHeatPort".to_string(), comp);

        let bool_params =
            extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bool_params.get("useHeatPort"), Some(&true));
    }

    #[test]
    fn bool_param_extraction_uses_declaration_binding() {
        let mut components = IndexMap::default();
        components.insert(
            "use_numberPort".to_string(),
            bool_param_with_binding("use_numberPort", "Boolean", true),
        );

        let bool_params =
            extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bool_params.get("use_numberPort"), Some(&true));
    }

    #[test]
    fn bool_param_extraction_ignores_start_attribute_modification() {
        let mut components = IndexMap::default();
        let mut comp = bool_param_with_start("useHeatPort", "Boolean", false);
        comp.start_is_modification = true;
        components.insert("useHeatPort".to_string(), comp);

        let bool_params =
            extract_test_bool_params(&components, &ast::ModificationEnvironment::new());

        assert!(!bool_params.contains_key("useHeatPort"));
    }

    #[test]
    fn scoped_record_alias_uses_modifier_source_scope() {
        let source = component_ref_expr("cellData");
        let mut mod_env = ast::ModificationEnvironment::new();
        mod_env.add(
            ast::QualifiedName::from_ident("cellData"),
            ast::ModificationValue::with_source_scope(
                source.clone(),
                Some(source),
                Some(ast::QualifiedName::new()),
            ),
        );
        let mut int_params = FxHashMap::from_iter([
            ("cellData.nRC".to_string(), 2),
            ("cell.cell.cellData.nRC".to_string(), 1),
        ]);

        propagate_scoped_record_alias_integer_params(
            &mut int_params,
            &mod_env,
            &ast::QualifiedName::from_dotted("cell.cell"),
        );

        assert_eq!(int_params.get("cell.cell.cellData.nRC"), Some(&2));
    }
}
