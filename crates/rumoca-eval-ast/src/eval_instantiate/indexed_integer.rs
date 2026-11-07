//! Element projection for structural Integer expressions (MLS §10.5).
//!
//! Select a binding element, then let the common scalar evaluator evaluate it.
//! This never substitutes a scalar default for an unresolved array selection.
use super::*;

pub(super) fn eval_reference(
    reference: &ast::ComponentReference,
    adapter: &InstantiateScalarAdapter<'_>,
    depth: usize,
) -> Option<i64> {
    let subscripts = reference.parts.last()?.subs.as_deref()?;
    let indices = eval_indices(subscripts, adapter, depth)?;
    let mut whole = reference.clone();
    whole.parts.last_mut()?.subs = None;
    project_reference(&whole, &indices, adapter, depth + 1)
}

pub(super) fn eval_array_index(
    base: &ast::Expression,
    subscripts: &[ast::Subscript],
    adapter: &InstantiateScalarAdapter<'_>,
    depth: usize,
) -> Option<i64> {
    project(
        base,
        &eval_indices(subscripts, adapter, depth)?,
        adapter,
        depth + 1,
    )
}

fn eval_indices(
    subscripts: &[ast::Subscript],
    adapter: &InstantiateScalarAdapter<'_>,
    depth: usize,
) -> Option<Vec<usize>> {
    subscripts
        .iter()
        .map(|subscript| {
            let ast::Subscript::Expression(expression) = subscript else {
                return None;
            };
            let index = ast_scalar::eval_integer(expression, adapter, "", depth + 1)?;
            usize::try_from(index.checked_sub(1)?).ok()
        })
        .collect()
}

fn project(
    expression: &ast::Expression,
    indices: &[usize],
    adapter: &InstantiateScalarAdapter<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let Some((index, rest)) = indices.split_first() else {
        return ast_scalar::eval_integer(expression, adapter, "", depth + 1);
    };
    match expression {
        ast::Expression::Array {
            elements,
            kind: rumoca_core::ArrayConstructor::Array,
            ..
        } => project(elements.get(*index)?, rest, adapter, depth + 1),
        ast::Expression::ComponentReference(reference) => {
            project_reference(reference, indices, adapter, depth + 1)
        }
        ast::Expression::Parenthesized { inner, .. } => project(inner, indices, adapter, depth + 1),
        _ => None,
    }
}

fn project_reference(
    reference: &ast::ComponentReference,
    indices: &[usize],
    adapter: &InstantiateScalarAdapter<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH || reference.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    // Instance modifications and local declarations have the same precedence as
    // scalar structural evaluation. Package expressions use their own scope.
    if reference.parts.len() == 1 {
        let binding = resolve_scalar_declaration_expr(reference, adapter.env)?;
        return project(&binding, indices, adapter, depth + 1);
    }
    if let Some(class) = selected_package(reference, adapter.env) {
        let components = (adapter.env.resolve_class_components)(adapter.env.tree, class);
        let field = components.get(reference.parts.last()?.ident.text.as_ref())?;
        if !matches!(field.variability, rumoca_core::Variability::Constant(_)) {
            return None;
        }
        let mod_env = ast::ModificationEnvironment::new();
        let scoped = InstantiateScalarAdapter {
            env: IntegerEvalEnv {
                effective_components: &components,
                mod_env: &mod_env,
                ..adapter.env
            },
            local_ints: None,
            local_bools: None,
            local_reals: None,
        };
        return project(
            component_expr_for_structural_eval(field)?,
            indices,
            &scoped,
            depth + 1,
        );
    }
    let binding = resolve_scalar_declaration_expr(reference, adapter.env)?;
    project(&binding, indices, adapter, depth + 1)
}

fn selected_package<'a>(
    reference: &ast::ComponentReference,
    env: IntegerEvalEnv<'a>,
) -> Option<&'a ast::ClassDef> {
    if reference.parts.len() == 2 {
        let root = reference.parts.first()?.ident.text.as_ref();
        if let Some(modification) = env.mod_env.get(&ast::QualifiedName::from_ident(root)) {
            let target = match &modification.value {
                ast::Expression::ClassModification { target, .. } => target,
                ast::Expression::ComponentReference(target) => target,
                _ => return None,
            };
            // A forwarding redeclare is unresolved until its enclosing instance.
            if target.parts.len() == 1 && target.parts[0].ident.text.as_ref() == root {
                return None;
            }
            return resolve_class_from_cref(env.tree, target);
        }
    }
    // Resolve attached the class identity to the part preceding its member.
    // In particular an imported alias D must not be looked up as a class named D.
    let owner = reference.parts.get(reference.parts.len().checked_sub(2)?)?;
    env.tree.get_class_by_def_id(owner.def_id?)
}
