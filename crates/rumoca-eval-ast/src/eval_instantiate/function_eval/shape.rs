use super::*;
use std::cell::Cell;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ShapeDimensionStability {
    TranslationConstant,
    ScopeDependent,
}

/// One evaluated dimension together with evaluator-issued stability evidence.
///
/// The fields are private so consumers cannot assert that a scope-dependent
/// value is a translation constant. Only the evaluator that selected every
/// referenced declaration may mint that fact.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EvaluatedShapeDimension {
    value: i64,
    stability: ShapeDimensionStability,
}

impl EvaluatedShapeDimension {
    pub fn value(self) -> i64 {
        self.value
    }

    pub fn is_translation_constant(self) -> bool {
        self.stability == ShapeDimensionStability::TranslationConstant
    }
}

/// Evaluate array dimensions from shape_expr subscripts.
/// Returns None if any dimension cannot be evaluated.
///
/// MLS §10.1: Array dimensions can depend on parameters that are overridden
/// by modifications. We try evaluating shape_expr with the current mod_env first,
/// which handles cases like `Plug starpoints(m=mSystems)` where `mSystems=1`
/// overrides the default `m=3`. A present symbolic shape is authoritative: if
/// it cannot be proved, a stale pre-computed shape must not replace it.
/// Evaluate array dimensions while reusing the caller's resolved-identity index.
pub fn evaluate_array_dimensions_with_index(
    shape: &[usize],
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
) -> Option<Vec<i64>> {
    if shape_expr.iter().any(|subscript| {
        rumoca_ir_ast::declaration_subscript_required_value_violation(subscript).is_some()
    }) {
        return None;
    }

    if !shape_expr.is_empty() {
        return eval_shape_expr(
            shape_expr,
            mod_env,
            effective_components,
            tree,
            class_index,
            resolve_class_components,
        );
    }

    if !shape.is_empty() {
        return Some(shape.iter().map(|&d| d as i64).collect());
    }

    Some(vec![]) // Scalar
}

/// Try to evaluate shape_expr subscripts to concrete dimensions.
fn eval_shape_expr(
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for sub in shape_expr {
        match sub {
            rumoca_ir_ast::Subscript::Expression(expr) => {
                // Shape expressions may include structural `if` branches that are
                // valid at translation time for parameter-based dimensions (MLS §10.1).
                // Keep this behavior scoped to shape evaluation so general integer
                // expression evaluation remains unchanged.
                let dim = try_eval_integer_shape_expr_with_index(
                    expr,
                    mod_env,
                    effective_components,
                    tree,
                    class_index,
                    resolve_class_components,
                )?;
                if dim < 0 {
                    return None;
                }
                dims.push(dim);
            }
            rumoca_ir_ast::Subscript::Range { .. } | rumoca_ir_ast::Subscript::Empty => {
                return None;
            }
        }
    }
    Some(dims)
}

/// Evaluate one shape expression while reusing the caller's identity index.
pub fn try_eval_integer_shape_expr_with_index(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
) -> Option<i64> {
    try_eval_integer_shape_expr_with_proof(
        expr,
        mod_env,
        effective_components,
        tree,
        class_index,
        resolve_class_components,
    )
    .map(EvaluatedShapeDimension::value)
}

/// Evaluate one dimension and mint stability evidence at the same lookup that
/// selects every referenced declaration.
pub fn try_eval_integer_shape_expr_with_proof(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
) -> Option<EvaluatedShapeDimension> {
    let stability = Cell::new(ShapeDimensionStability::TranslationConstant);
    let env = ShapeEvalEnv {
        mod_env,
        effective_components,
        tree,
        class_index,
        resolve_class_components,
        stability: &stability,
    };
    let value = try_eval_integer_shape_expr_with_depth(expr, env, 0)?;
    Some(EvaluatedShapeDimension {
        value,
        stability: stability.get(),
    })
}

fn mark_scope_dependent(stability: &Cell<ShapeDimensionStability>) {
    stability.set(ShapeDimensionStability::ScopeDependent);
}

#[derive(Clone, Copy)]
struct ShapeEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a ast::AstIndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> ast::AstIndexMap<String, ast::Component>,
    stability: &'a Cell<ShapeDimensionStability>,
}

fn try_eval_integer_shape_expr_with_depth(
    expr: &ast::Expression,
    env: ShapeEvalEnv<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let adapter = ShapeScalarAdapter { env };
    ast_scalar::eval_integer(expr, &adapter, "", depth)
}

struct ShapeScalarAdapter<'a> {
    env: ShapeEvalEnv<'a>,
}

impl AstScalarContext for ShapeScalarAdapter<'_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(MAX_EXPR_EVAL_DEPTH)
    }

    fn lookup_integer(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<i64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        eval_integer_shape_component_ref(reference, self.env, depth)
    }

    fn lookup_boolean(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<bool> {
        mark_scope_dependent(self.env.stability);
        evaluate_component_condition_with_depth(
            expr,
            self.env.mod_env,
            self.env.effective_components,
            self.env.tree,
            self.env.resolve_class_components,
            depth,
        )
    }

    fn call_integer(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        mark_scope_dependent(self.env.stability);
        eval_integer_function_call(
            function,
            args,
            IntegerEvalEnv {
                mod_env: self.env.mod_env,
                effective_components: self.env.effective_components,
                tree: self.env.tree,
                resolve_class_components: self.env.resolve_class_components,
                work_budget: None,
            },
            depth,
            None,
        )
    }

    fn enum_equal(
        &self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
        _scope: &str,
        depth: usize,
    ) -> Option<bool> {
        mark_scope_dependent(self.env.stability);
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

    fn boolean_expression_allowed(&self, expr: &ast::Expression) -> bool {
        shape_condition_uses_static_components(
            expr,
            self.env.mod_env,
            self.env.effective_components,
        )
    }
}

fn eval_integer_shape_component_ref(
    comp_ref: &ast::ComponentReference,
    env: ShapeEvalEnv<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |expr| try_eval_integer_shape_expr_with_depth(expr, env, depth + 1);

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    if let Some(mod_value) = env.mod_env.get(&param_path) {
        mark_scope_dependent(env.stability);
        return recurse(&mod_value.value);
    }

    match super::super::class_lookup::resolve_class_constant_route(comp_ref, env.class_index) {
        super::super::class_lookup::ClassConstantResolution::Bound { exposure, target } => {
            let owner_components = (env.resolve_class_components)(env.tree, exposure);
            let component = owner_components
                .values()
                .find(|component| component.def_id == Some(target))
                .filter(|component| {
                    matches!(component.variability, rumoca_core::Variability::Constant(_))
                })?;
            return try_eval_integer_shape_expr_with_depth(
                super::super::component_expr_for_structural_eval(component)?,
                ShapeEvalEnv {
                    effective_components: &owner_components,
                    ..env
                },
                depth + 1,
            );
        }
        super::super::class_lookup::ClassConstantResolution::Refused => return None,
        super::super::class_lookup::ClassConstantResolution::NotClassReference => {}
    }

    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        if let Some(component) = env.effective_components.get(name) {
            if !shape_component_is_exact_translation_constant(comp_ref, component) {
                mark_scope_dependent(env.stability);
            }
            return recurse(super::super::component_expr_for_structural_eval(component)?);
        }
    }

    if let Some(component) = env.effective_components.get(dotted.as_str()) {
        if !shape_component_is_exact_translation_constant(comp_ref, component) {
            mark_scope_dependent(env.stability);
        }
        return recurse(super::super::component_expr_for_structural_eval(component)?);
    }

    for candidate in super::super::enclosing_scope_candidates(dotted.as_str()) {
        let qn = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = env.mod_env.get(&qn) {
            mark_scope_dependent(env.stability);
            return recurse(&mod_value.value);
        }
        if let Some(component) = env.effective_components.get(candidate.as_str()) {
            if !shape_component_is_exact_translation_constant(comp_ref, component) {
                mark_scope_dependent(env.stability);
            }
            return recurse(super::super::component_expr_for_structural_eval(component)?);
        }
    }

    mark_scope_dependent(env.stability);
    let integer_env = super::IntegerEvalEnv {
        mod_env: env.mod_env,
        effective_components: env.effective_components,
        tree: env.tree,
        resolve_class_components: env.resolve_class_components,
        work_budget: None,
    };
    if let Some(value) =
        super::super::eval_integer_class_redeclare_field_ref(comp_ref, integer_env, depth, None)
    {
        return Some(value);
    }

    // Fall back to record-field resolution for paths like `data.n` where the
    // value lives in a record default/modification environment.
    super::super::eval_integer_record_field_ref(comp_ref, integer_env, depth)
}

fn shape_component_is_exact_translation_constant(
    comp_ref: &ast::ComponentReference,
    component: &ast::Component,
) -> bool {
    matches!(component.variability, rumoca_core::Variability::Constant(_))
        && matches!(
            (comp_ref.target_def_id(), component.def_id),
            (Some(reference), Some(declaration)) if reference == declaration
        )
}

fn shape_condition_uses_static_components(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
) -> bool {
    match expr {
        ast::Expression::Terminal { .. } => true,
        ast::Expression::ComponentReference(comp_ref) => {
            shape_component_ref_is_static(comp_ref, mod_env, effective_components)
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            shape_condition_uses_static_components(lhs, mod_env, effective_components)
                && shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Unary { rhs, .. } => {
            shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            shape_condition_uses_static_components(inner, mod_env, effective_components)
        }
        _ => false,
    }
}

fn shape_component_ref_is_static(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
) -> bool {
    if comp_ref.parts.is_empty() {
        return false;
    }

    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    let is_static_component = |component: &ast::Component| {
        component.is_structural
            || matches!(
                component.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            )
    };

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if mod_env.get(&param_path).is_some() {
        return true;
    }

    if let Some(component) = effective_components.get(dotted.as_str()) {
        return is_static_component(component);
    }

    if comp_ref.parts.len() == 1
        && let Some(component) = effective_components.get(comp_ref.parts[0].ident.text.as_ref())
    {
        return is_static_component(component);
    }

    // Unresolved refs in conditions are typically enum literals and are checked
    // by evaluate_component_condition_with_depth.
    true
}
