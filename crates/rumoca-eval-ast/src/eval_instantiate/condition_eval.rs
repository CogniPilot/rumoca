use super::*;

/// Look up a class by name in the class tree.
///
/// Uses O(1) lookup via the name_map (populated during resolve phase).
pub(super) fn find_class_in_tree<'a>(
    tree: &'a ast::ClassTree,
    name: &str,
) -> Option<&'a ast::ClassDef> {
    if let Some(&def_id) = tree.name_map.get(name) {
        return tree.get_class_by_def_id(def_id);
    }

    if let Some(class) = tree.definitions.classes.get(name) {
        return Some(class);
    }

    None
}

/// Convert a boolean literal expression to its value.
pub fn expr_to_bool(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match &*token.text {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Try to evaluate a boolean literal expression.
pub(super) fn try_eval_bool_literal(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Evaluate a conditional component's condition (MLS §4.8).
///
/// Tries to evaluate the condition expression to a boolean at instantiation time.
/// Returns:
/// - Some(true): condition is true, component should be instantiated
/// - Some(false): condition is false, component should be skipped
/// - None: condition cannot be evaluated from current parameter/modifier context,
///   and instantiation proceeds conservatively.
///
/// The condition is typically a simple boolean parameter reference like `useSupport`.
/// Disabled component paths are recorded in `overlay.disabled_components` so the
/// flatten phase can filter out connections and equations involving them.
pub fn evaluate_component_condition(
    ctx: &InstantiateEvalCtx,
    condition: &ast::Expression,
) -> Option<bool> {
    evaluate_component_condition_with_outer_values(ctx, condition, OuterValues::default())
}

/// Values reached through a class's `outer` references (MLS §5.4).
///
/// An `outer` element denotes the nearest enclosing `inner` element of the same
/// name, so a condition such as `world.enableAnimation and sphereDiameter > 0`
/// cannot be answered from the declaring class alone. The instantiate phase
/// resolves those references against the matching `inner` instance and passes the
/// values here, keyed by the dotted path as written in the condition.
#[derive(Clone, Copy, Default)]
pub struct OuterValues<'a> {
    pub integers: Option<&'a FxHashMap<String, i64>>,
    pub bools: Option<&'a FxHashMap<String, bool>>,
    pub reals: Option<&'a FxHashMap<String, f64>>,
}

impl<'a> OuterValues<'a> {
    /// Borrow Boolean and Real maps where Integer values have a separate owner.
    #[must_use]
    pub fn new(bools: &'a FxHashMap<String, bool>, reals: &'a FxHashMap<String, f64>) -> Self {
        Self {
            integers: None,
            bools: (!bools.is_empty()).then_some(bools),
            reals: (!reals.is_empty()).then_some(reals),
        }
    }

    /// Borrow every scalar map used by conditional-component evaluation.
    #[must_use]
    pub fn from_all(
        integers: &'a FxHashMap<String, i64>,
        bools: &'a FxHashMap<String, bool>,
        reals: &'a FxHashMap<String, f64>,
    ) -> Self {
        Self {
            integers: (!integers.is_empty()).then_some(integers),
            bools: (!bools.is_empty()).then_some(bools),
            reals: (!reals.is_empty()).then_some(reals),
        }
    }
}

/// Evaluate a conditional component's condition with pre-resolved reference values.
///
/// See [`OuterValues`] for what `outer_values` carries; everything else evaluates
/// exactly as in [`evaluate_component_condition`].
pub fn evaluate_component_condition_with_outer_values(
    ctx: &InstantiateEvalCtx,
    condition: &ast::Expression,
    outer_values: OuterValues<'_>,
) -> Option<bool> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    let adapter = InstantiateScalarAdapter {
        env: IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components: *resolve_class_components,
            work_budget: None,
        },
        local_ints: outer_values.integers,
        local_bools: outer_values.bools,
        local_reals: outer_values.reals,
    };
    ast_scalar::eval_boolean(condition, &adapter, "", 0)
}

pub(super) fn evaluate_component_condition_with_depth(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }
    let adapter = InstantiateScalarAdapter {
        env: IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            work_budget: None,
        },
        local_ints: None,
        local_bools: None,
        local_reals: None,
    };
    ast_scalar::eval_boolean(condition, &adapter, "", depth)
}

/// Evaluate a parameter reference in a condition.
pub(super) fn eval_param_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    // Build qualified path for multi-part references (e.g., smpmData.useDamperCage)
    let param_path = build_qualified_path(comp_ref);
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    if !local_reference_occurrence_allows(
        comp_ref,
        dotted.as_str(),
        IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            work_budget: None,
        },
    ) {
        return None;
    }

    if let Some(mod_value) = mod_env.get(&param_path) {
        if let Some(val) = expr_to_bool(&mod_value.value) {
            return Some(val);
        }
        if let Some(source_scope) = mod_value.source_scope.as_ref() {
            let scope_prefix = source_scope.to_flat_string();
            if let Some(val) = eval_scoped_string_condition_with_depth(
                &mod_value.value,
                ConditionEvalEnv {
                    mod_env,
                    effective_components,
                    tree,
                    resolve_class_components,
                },
                Some(scope_prefix.as_str()),
                depth + 1,
            ) {
                return Some(val);
            }
        }
        // Recursively evaluate only if mod_env value is a ast::ComponentReference
        // (another parameter ref like smpmData.useDamperCage → false)
        if matches!(&mod_value.value, ast::Expression::ComponentReference(_))
            && let Some(val) = evaluate_component_condition_with_depth(
                &mod_value.value,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
                depth + 1,
            )
        {
            return Some(val);
        }
        return None;
    }

    // Look up the parameter's declared value from effective components
    // (single-part only). MLS §4.9: a component without a binding has no value
    // here — its `start` attribute is an initial guess, not an answer — so an
    // undecidable condition stays `None`.
    if comp_ref.parts.len() == 1 {
        let param_name = comp_ref.parts[0].ident.text.as_ref();
        let sibling = effective_components.get(param_name)?;
        let eval_ctx = InstantiateEvalCtx {
            tree,
            mod_env,
            effective_components,
            resolve_class_components,
        };
        if !component_allows_structural_evaluation(param_name, sibling, &eval_ctx) {
            return None;
        }
        let value_expr = component_expr_for_structural_eval(sibling)?;
        // Try simple boolean extraction first
        if let Some(val) = expr_to_bool(value_expr) {
            return Some(val);
        }
        // Try recursive evaluation for expressions like controllerType == PI
        return evaluate_component_condition_with_depth(
            value_expr,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        );
    }

    // Qualified references keep the declaration scope of the value they find.
    // This matters for a record field such as `settings.connect3` whose binding
    // reads its sibling `layout`: evaluating that binding in the model scope
    // would silently lose the record-member lookup required by MLS §5.3/§7.2.
    let env = ConditionEvalEnv {
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
    };
    let (binding, binding_scope) = resolve_component_ref_expr(
        comp_ref,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        None,
    )?;
    eval_scoped_string_condition_with_depth(&binding, env, binding_scope.as_deref(), depth + 1)
}

/// Build a ast::QualifiedName from a ast::ComponentReference's parts.
fn build_qualified_path(comp_ref: &ast::ComponentReference) -> ast::QualifiedName {
    if comp_ref.parts.len() == 1 {
        ast::QualifiedName::from_ident(&comp_ref.parts[0].ident.text)
    } else {
        let dotted = comp_ref
            .parts
            .iter()
            .map(|p| p.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".");
        ast::QualifiedName::from_dotted(&dotted)
    }
}

/// Evaluate an enum equality comparison like `controllerType == SimpleController.PI`.
///
/// Returns Some(true) if values are equal, Some(false) if not equal, None if cannot evaluate.
pub(super) fn evaluate_enum_equality_with_depth(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    // Prevent deep recursion
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    // Get values from both sides (could be enum literals or parameter references).
    // If a side stays as an unresolved component reference, keep comparison unknown.
    let lhs_val = enum_value_for_comparison_with_depth(
        lhs,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        None,
        depth,
    );
    let rhs_val = enum_value_for_comparison_with_depth(
        rhs,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        None,
        depth,
    );

    match (lhs_val, rhs_val) {
        (Some(l), Some(r)) => {
            // Compare enum values, handling qualified name differences
            // "Modelica.Blocks.Types.SimpleController.PI" should match "SimpleController.PI"
            Some(enum_values_equal(&l, &r))
        }
        _ => None,
    }
}

pub(super) fn enum_value_for_comparison_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
    // MLS §4.8: a compile-time condition may only use values that are actually
    // known here. `get_enum_value_with_depth` already declines a reference it
    // could not resolve to a String literal or an enumeration literal, so no
    // further filtering of "looks unresolved" spellings is needed.
    get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        scope_prefix,
        depth,
    )
    .map(ResolvedValueText::into_text)
}

/// A value this phase resolved to a comparable spelling.
///
/// The two cases are kept apart because they are not interchangeable: an
/// enumeration literal is a value of its enumeration type (MLS §4.8.5.1) and
/// must never be rewritten into a `String` modifier, while a `String` literal
/// (MLS §4.9) may be. Collapsing them to one `String` is what let a rendered
/// reference be substituted where a string value was expected.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum ResolvedValueText {
    StringLiteral(String),
    EnumerationLiteral(String),
}

impl ResolvedValueText {
    fn into_text(self) -> String {
        match self {
            ResolvedValueText::StringLiteral(text)
            | ResolvedValueText::EnumerationLiteral(text) => text,
        }
    }

    pub(super) fn into_string_literal(self) -> Option<String> {
        match self {
            ResolvedValueText::StringLiteral(text) => Some(text),
            ResolvedValueText::EnumerationLiteral(_) => None,
        }
    }
}

/// Check if two enum values are equal, handling qualified enum spellings.
///
/// Enum values can be:
/// - Fully qualified: "Modelica.Blocks.Types.SimpleController.PI"
/// - Short form: "SimpleController.PI"
/// - Just the value: "PI" (rare but possible)
///
pub(super) fn enum_values_equal(a: &str, b: &str) -> bool {
    rumoca_core::enum_values_equal(a, b)
}

/// Get an enum or string value from an expression.
///
/// Handles:
/// - String literals
/// - Enumeration literals (e.g., `SimpleController.PI`), recognized against the
///   class tree rather than by their spelling (MLS §4.8.5.1)
/// - References that resolve to one of the above
///
/// A reference this phase cannot resolve is *unknown*: it is never answered
/// with its own rendered name (SPEC_0008 — no invented values).
pub(super) fn get_enum_value_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<ResolvedValueText> {
    // Prevent deep recursion
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    match expr {
        // String literal: "D", "Y", etc.
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
            ..
        } => {
            let s = token.text.trim_matches('"');
            Some(ResolvedValueText::StringLiteral(s.to_string()))
        }
        ast::Expression::ComponentReference(comp_ref) => resolve_component_ref_expr(
            comp_ref,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            scope_prefix,
        )
        .and_then(|(resolved_expr, next_scope)| {
            get_enum_value_with_depth(
                &resolved_expr,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
                next_scope.as_deref(),
                depth + 1,
            )
        })
        .or_else(|| {
            enum_literal::enumeration_literal_path(comp_ref, tree)
                .map(ResolvedValueText::EnumerationLiteral)
        }),
        ast::Expression::Parenthesized { inner, .. } => get_enum_value_with_depth(
            inner,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            scope_prefix,
            depth + 1,
        ),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let env = ConditionEvalEnv {
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
            };
            eval_if_enum_value(branches, else_branch, env, scope_prefix, depth + 1)
        }
        _ => None,
    }
}

fn parent_dotted_scope(path: &str) -> Option<String> {
    let enclosing = rumoca_core::ComponentPath::from_flat_path(path).parent()?;
    (!enclosing.is_root()).then(|| enclosing.to_flat_string())
}

fn eval_if_enum_value(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<ResolvedValueText> {
    for (cond, branch_expr) in branches {
        match eval_scoped_string_condition_with_depth(cond, env, scope_prefix, depth) {
            Some(true) => {
                return get_enum_value_with_depth(
                    branch_expr,
                    env.mod_env,
                    env.effective_components,
                    env.tree,
                    env.resolve_class_components,
                    scope_prefix,
                    depth,
                );
            }
            Some(false) => continue,
            None => return None,
        }
    }
    get_enum_value_with_depth(
        else_branch,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )
}

pub(super) fn resolve_component_ref_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
) -> Option<(ast::Expression, Option<String>)> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    let candidate_paths = candidate_paths_for_ref(comp_ref, dotted.as_str(), scope_prefix);

    lookup_exact_component_ref(candidate_paths.as_slice(), mod_env, effective_components)
        .or_else(|| {
            resolve_class_redeclare_field_expr(comp_ref, mod_env, tree, resolve_class_components)
                .map(|expr| (expr, None))
        })
        .or_else(|| {
            if comp_ref.parts.len() != 1 {
                return None;
            }
            let prefix = scope_prefix?;
            let scoped_expr = resolve_scoped_record_field_expr(
                prefix,
                dotted.as_str(),
                effective_components,
                tree,
            )?;
            Some((scoped_expr, Some(prefix.to_string())))
        })
        .or_else(|| {
            resolve_component_ref_from_record_defaults(
                comp_ref,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
            )
            .map(|expr| (expr, parent_dotted_scope(&dotted)))
        })
        .or_else(|| {
            resolve_class_constant_binding(comp_ref, tree, resolve_class_components)
                .map(|expr| (expr, None))
        })
}

pub(super) fn resolve_class_redeclare_field_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() != 2
        || comp_ref
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }

    let root_name = comp_ref.parts[0].ident.text.as_ref();
    let field_name = comp_ref.parts[1].ident.text.as_ref();
    let root_mod = mod_env.get(&ast::QualifiedName::from_ident(root_name))?;
    let target_cref = match &root_mod.value {
        ast::Expression::ClassModification { target, .. } => target,
        ast::Expression::ComponentReference(cref) => cref,
        _ => return None,
    };

    let forwarding_self_redeclare = target_cref.parts.len() == 1
        && target_cref.parts[0].subs.is_none()
        && target_cref.parts[0].ident.text.as_ref() == root_name;
    if forwarding_self_redeclare {
        return None;
    }

    let target_class = resolve_class_from_cref(tree, target_cref)?;
    let effective_components = resolve_class_components(tree, target_class);
    let field_component = effective_components.get(field_name)?;
    let occurrence = format!("{root_name}.{field_name}");
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components: &effective_components,
        resolve_class_components,
    };
    if !component_allows_structural_evaluation(&occurrence, field_component, &eval_ctx) {
        return None;
    }
    component_expr_for_structural_eval(field_component).cloned()
}

fn candidate_paths_for_ref(
    comp_ref: &ast::ComponentReference,
    dotted: &str,
    scope_prefix: Option<&str>,
) -> Vec<String> {
    let mut paths = Vec::with_capacity(2);
    if comp_ref.parts.len() == 1
        && let Some(prefix) = scope_prefix
    {
        paths.push(format!("{prefix}.{dotted}"));
    }
    paths.push(dotted.to_string());
    paths
}

fn lookup_exact_component_ref(
    candidate_paths: &[String],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> Option<(ast::Expression, Option<String>)> {
    for candidate in candidate_paths {
        if let Some(mod_value) = mod_env.get(&ast::QualifiedName::from_dotted(candidate))
            && !transparent_self_modifier(candidate, &mod_value.value)
        {
            return Some((
                mod_value.value.clone(),
                mod_value
                    .source_scope
                    .as_ref()
                    .map(ast::QualifiedName::to_flat_string)
                    .or_else(|| parent_dotted_scope(candidate)),
            ));
        }
        if let Some(comp) = effective_components.get(candidate.as_str()) {
            let expr = component_expr_for_structural_eval(comp)?;
            return Some((expr.clone(), parent_dotted_scope(candidate)));
        }
    }
    None
}

fn transparent_self_modifier(candidate: &str, value: &ast::Expression) -> bool {
    let ast::Expression::ComponentReference(comp_ref) = value else {
        return false;
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return false;
    }
    let Some(name) = rumoca_core::ComponentPath::from_flat_path(candidate)
        .into_parts()
        .last()
        .cloned()
    else {
        return false;
    };
    comp_ref.parts[0].ident.text.as_ref() == name
}

fn resolve_scoped_record_field_expr(
    scope_prefix: &str,
    field_name: &str,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let scope_comp = effective_components.get(scope_prefix)?;
    if let Some(expr) = scope_comp.modifications.get(field_name) {
        return Some(expr.clone());
    }
    let type_def_id = scope_comp.type_def_id?;
    let class = tree.get_class_by_def_id(type_def_id)?;
    let field = class.components.get(field_name)?;
    component_expr_for_structural_eval(field).cloned()
}

#[derive(Copy, Clone)]
pub(crate) struct IntegerEvalEnv<'a> {
    pub(crate) mod_env: &'a ast::ModificationEnvironment,
    pub(crate) effective_components: &'a IndexMap<String, ast::Component>,
    pub(crate) tree: &'a ast::ClassTree,
    pub(crate) resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> IndexMap<String, ast::Component>,
    pub(crate) work_budget: Option<&'a crate::function_budget::AstFunctionWorkBudget>,
}

impl<'a> IntegerEvalEnv<'a> {
    pub(crate) fn instantiate_ctx(self) -> InstantiateEvalCtx<'a> {
        InstantiateEvalCtx {
            tree: self.tree,
            mod_env: self.mod_env,
            effective_components: self.effective_components,
            resolve_class_components: self.resolve_class_components,
        }
    }

    pub(crate) fn with_work_budget<'b>(
        self,
        fallback: &'b crate::function_budget::AstFunctionWorkBudget,
    ) -> IntegerEvalEnv<'b>
    where
        'a: 'b,
    {
        IntegerEvalEnv {
            mod_env: self.mod_env,
            effective_components: self.effective_components,
            tree: self.tree,
            resolve_class_components: self.resolve_class_components,
            work_budget: Some(self.work_budget.unwrap_or(fallback)),
        }
    }
}

pub(super) struct InstantiateScalarAdapter<'a> {
    pub(super) env: IntegerEvalEnv<'a>,
    pub(super) local_ints: Option<&'a FxHashMap<String, i64>>,
    pub(super) local_bools: Option<&'a FxHashMap<String, bool>>,
    pub(super) local_reals: Option<&'a FxHashMap<String, f64>>,
}

/// Resolve a reference to the declaration-side expression that defines it.
///
/// MLS §4.4.4 / §7.2: a parameter's value is written either as an applied
/// modification or as the declaration binding, and MLS §5.3 makes an unqualified
/// name visible from the enclosing scopes as well. MLS §5.3.2 additionally makes a
/// qualified name denote a class-level constant (`Modelica.Constants.eps`). This
/// walks exactly those places and returns the expression found; a reference with
/// subscripts names one array element and is left unresolved rather than answered
/// with the whole array (SPEC_0008).
fn resolve_scalar_declaration_expr<'a>(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'a>,
) -> Option<Cow<'a, ast::Expression>> {
    if comp_ref
        .parts
        .iter()
        .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }

    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    if !local_reference_occurrence_allows(comp_ref, dotted.as_str(), env) {
        return None;
    }

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if let Some(mod_value) = env.mod_env.get(&param_path) {
        return Some(Cow::Borrowed(&mod_value.value));
    }

    if let Some(component) = env.effective_components.get(dotted.as_str()) {
        return component_expr_for_structural_eval(component).map(Cow::Borrowed);
    }

    for candidate in enclosing_scope_candidates(dotted.as_str()) {
        let qualified = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = env.mod_env.get(&qualified) {
            return Some(Cow::Borrowed(&mod_value.value));
        }
        if let Some(component) = env.effective_components.get(candidate.as_str()) {
            return component_expr_for_structural_eval(component).map(Cow::Borrowed);
        }
    }

    // MLS §5.3.2: a qualified name may denote a constant declared by a class or
    // package (`Modelica.Constants.eps`) rather than a component of this scope.
    // MLS §7.1/§7.2: it may equally name a field of a record component, whose
    // value comes from the record's modification or its declared default. The
    // Boolean and Integer paths already resolve both; a Real parameter
    // expression that compares against one needs the same reach.
    resolve_class_redeclare_field_expr(
        comp_ref,
        env.mod_env,
        env.tree,
        env.resolve_class_components,
    )
    .or_else(|| resolve_class_constant_binding(comp_ref, env.tree, env.resolve_class_components))
    .or_else(|| {
        resolve_component_ref_from_record_defaults(
            comp_ref,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
        )
    })
    .map(Cow::Owned)
}

pub(super) fn local_reference_occurrence_allows(
    comp_ref: &ast::ComponentReference,
    dotted: &str,
    env: IntegerEvalEnv<'_>,
) -> bool {
    let eval_ctx = env.instantiate_ctx();
    if modification_environment_disables_structural_evaluation(dotted, &eval_ctx) {
        return false;
    }
    if let Some(component) = env.effective_components.get(dotted) {
        return component_allows_structural_evaluation(dotted, component, &eval_ctx);
    }
    let Some(root) = comp_ref.parts.first().map(|part| part.ident.text.as_ref()) else {
        return false;
    };
    env.effective_components.get(root).is_none_or(|component| {
        if comp_ref.parts.len() > 1
            && !matches!(
                component.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            )
        {
            // An ordinary structured instance is a namespace container. Its
            // selected field retains its own variability, so the container's
            // continuous default must not poison a parameter field.
            true
        } else {
            component_allows_structural_evaluation(root, component, &eval_ctx)
        }
    })
}

pub(super) fn reference_root_is_outer(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
) -> bool {
    comp_ref
        .parts
        .first()
        .and_then(|part| env.effective_components.get(part.ident.text.as_ref()))
        .is_some_and(|component| component.outer)
}

impl AstScalarContext for InstantiateScalarAdapter<'_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(MAX_EXPR_EVAL_DEPTH)
    }

    fn lookup_integer(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<i64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        eval_integer_component_ref(reference, self.env, depth, self.local_ints)
    }

    /// Fold a Real-valued reference (MLS §4.4.5 parameter expression).
    ///
    /// A conditional component's condition may compare a Real parameter, as in
    /// `Parts.Body`'s `world.enableAnimation and animation and sphereDiameter > 0`.
    /// Values pre-resolved through an `outer` reference (MLS §5.4) win, otherwise
    /// the reference is followed to its declaration binding and folded there.
    fn lookup_real(&self, expr: &ast::Expression, scope: &str, depth: usize) -> Option<f64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        let dotted = component_ref_to_dotted_no_subscripts(reference)?;
        if !local_reference_occurrence_allows(reference, dotted.as_str(), self.env) {
            return None;
        }
        if let Some(values) = self.local_reals
            && let Some(value) = lookup_local_scalar(reference, values)
        {
            return Some(value);
        }
        // Integer-valued parameter declarations and pure function calls are
        // promoted when they occur in a Real expression (MLS §10.6.2).  Ask
        // the Integer evaluator first: it only returns a value when the full
        // declaration expression is exact, while `/` inside the surrounding
        // Real expression remains Real division.
        if let Some(value) = eval_integer_component_ref(reference, self.env, depth, self.local_ints)
        {
            return Some(value as f64);
        }
        if reference_root_is_outer(reference, self.env) {
            return None;
        }
        let declaration = resolve_scalar_declaration_expr(reference, self.env)?;
        ast_scalar::eval_real(declaration.as_ref(), self, scope, depth)
    }

    fn lookup_boolean(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<bool> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        let dotted = component_ref_to_dotted_no_subscripts(reference)?;
        if !local_reference_occurrence_allows(reference, dotted.as_str(), self.env) {
            return None;
        }
        self.local_bools
            .and_then(|values| lookup_local_scalar(reference, values))
            .or_else(|| {
                if reference_root_is_outer(reference, self.env) {
                    return None;
                }
                eval_param_ref(
                    reference,
                    self.env.mod_env,
                    self.env.effective_components,
                    self.env.tree,
                    self.env.resolve_class_components,
                    depth,
                )
            })
    }

    fn call_integer(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        if call_targets_predefined(function, "integer", self.env.tree)
            && let [argument] = args
        {
            let value = ast_scalar::eval_real(argument, self, scope, depth)?;
            let value = rumoca_core::modelica_integer_value(value);
            if value.is_finite() && value >= i64::MIN as f64 && value < -(i64::MIN as f64) {
                return Some(value as i64);
            }
            return None;
        }
        eval_integer_function_call(function, args, self.env, depth, self.local_ints)
    }

    fn call_boolean(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<bool> {
        eval_bool_function_call(
            function,
            args,
            self.env,
            depth,
            self.local_ints,
            self.local_bools,
        )
    }

    fn call_real(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<f64> {
        // A structurally evaluated Integer result is a valid Real operand by
        // MLS §10.6.2.  The Integer evaluator remains the certificate here: it
        // rejects functions whose result cannot be established exactly, so
        // this promotion cannot turn an undecidable Real call into a value.
        eval_integer_function_call(function, args, self.env, depth, self.local_ints)
            .map(|value| value as f64)
    }

    fn enum_equal(
        &self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
        _scope: &str,
        depth: usize,
    ) -> Option<bool> {
        evaluate_enum_equality_with_depth(
            lhs,
            rhs,
            self.env.mod_env,
            self.env.effective_components,
            self.env.tree,
            self.env.resolve_class_components,
            depth,
        )
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        eval_integer_binary(op, lhs, rhs)
    }
}
