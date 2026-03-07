use super::evaluator::{evaluate_array_dimensions, try_eval_integer_shape_expr};
use super::find_class_in_tree;
use indexmap::IndexMap;
use rumoca_core::{DefId, is_builtin_type};
use rumoca_ir_ast as ast;

/// Collect array subscripts from a type alias inheritance chain.
///
/// For aliases like:
/// - `type QuaternionBase = Real[4];`
/// - `type Orientation = QuaternionBase;`
///
/// this returns `[4]` for `Orientation`.
fn collect_type_alias_subscripts(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
) -> Vec<ast::Subscript> {
    const MAX_DEPTH: usize = 16;
    let mut subscripts = Vec::new();
    let mut current = class_def;
    let mut visited_defs = std::collections::HashSet::<DefId>::new();
    let mut visited_names = std::collections::HashSet::<String>::new();

    for _ in 0..MAX_DEPTH {
        let Some(class) = current else {
            break;
        };

        if let Some(def_id) = class.def_id {
            if !visited_defs.insert(def_id) {
                break;
            }
        } else if !visited_names.insert(class.name.text.to_string()) {
            break;
        }

        if !class.array_subscripts.is_empty() {
            subscripts.extend(class.array_subscripts.clone());
        }

        // Type aliases are expected to use a single base type.
        let Some(ext) = class.extends.first() else {
            break;
        };
        let base_name = ext.base_name.to_string();
        if is_builtin_type(&base_name) {
            break;
        }

        current = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| find_class_in_tree(tree, &base_name));
    }

    subscripts
}

/// Resolve array dimensions inherited from a type alias chain.
pub(super) fn resolve_type_alias_dimensions(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> Vec<i64> {
    let subscripts = collect_type_alias_subscripts(tree, class_def);
    if subscripts.is_empty() {
        return Vec::new();
    }

    evaluate_array_dimensions(&[], &subscripts, mod_env, effective_components, tree)
        .unwrap_or_default()
}

pub(super) fn resolve_component_dimensions(
    comp: &ast::Component,
    type_dims: &[i64],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> (Vec<i64>, Vec<ast::Subscript>) {
    let mut dims = Vec::new();
    let mut dims_expr = Vec::new();
    let mut shape_eval_succeeded = false;
    let needs_late_recompute =
        !comp.shape_expr.is_empty() && shape_expr_needs_late_recompute(&comp.shape_expr);

    if !comp.shape_expr.is_empty() {
        if let Some(eval_dims) =
            eval_shape_expr_dims(&comp.shape_expr, mod_env, effective_components, tree)
        {
            if needs_late_recompute {
                // Defer symbolic dimensions to later phases that have full local
                // scope/modifier context (MLS §10.1 structural dimensions).
                // Do not trust parser/early fallback dimensions here, because they
                // may have dropped colon positions (e.g. `[:, 2]` -> `[2]`) or
                // captured values from an outer scope before local parameters
                // converged.
                dims_expr = comp.shape_expr.clone();
            } else {
                dims = eval_dims;
                shape_eval_succeeded = true;
            }
        } else if needs_late_recompute {
            // Preserve symbolic/range expressions for late fixed-point passes.
            dims_expr = comp.shape_expr.clone();
        } else {
            // Keep a numeric fallback when available for non-symbolic shapes.
            dims = comp.shape.iter().map(|&d| d as i64).collect();
            let preserve_shape_expr_fallback =
                mod_env_has_package_alias_bindings(mod_env) || comp.shape.is_empty();
            if preserve_shape_expr_fallback {
                dims_expr = comp.shape_expr.clone();
            }
        }
    } else if !comp.shape.is_empty() {
        dims = comp.shape.iter().map(|&d| d as i64).collect();
    }

    // Append dimensions inherited from type aliases (e.g., Orientation -> Real[4]).
    // Keep legacy behavior for unresolved component shape expressions, but still
    // append alias dimensions when we already evaluated a concrete base shape.
    if (dims_expr.is_empty() || shape_eval_succeeded) && !type_dims.is_empty() {
        dims.extend_from_slice(type_dims);
    }

    (dims, dims_expr)
}

fn shape_expr_needs_late_recompute(shape_expr: &[ast::Subscript]) -> bool {
    shape_expr.iter().any(|sub| match sub {
        ast::Subscript::Expression(expr) => !matches!(
            expr,
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                ..
            }
        ),
        ast::Subscript::Range { .. } | ast::Subscript::Empty => true,
    })
}

fn mod_env_has_package_alias_bindings(mod_env: &ast::ModificationEnvironment) -> bool {
    mod_env.active.iter().any(|(key, mv)| {
        key.parts.len() == 1
            && key.parts[0]
                .0
                .chars()
                .next()
                .is_some_and(char::is_uppercase)
            && matches!(
                mv.value,
                ast::Expression::ClassModification { .. } | ast::Expression::ComponentReference(_)
            )
    })
}

fn eval_shape_expr_dims(
    shape_expr: &[ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for sub in shape_expr {
        let ast::Subscript::Expression(expr) = sub else {
            return None;
        };
        // MLS §10.1: structural dimension expressions may use compile-time `if`
        // branches over parameter/constant conditions.
        let dim = try_eval_integer_shape_expr(expr, mod_env, effective_components, tree)?;
        if dim < 0 {
            return None;
        }
        dims.push(dim);
    }
    Some(dims)
}

#[cfg(test)]
mod tests {
    use super::{resolve_component_dimensions, resolve_type_alias_dimensions};
    use indexmap::IndexMap;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(text),
            location: rumoca_ir_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_name(text: &str) -> ast::Name {
        ast::Name {
            name: vec![make_token(text)],
            def_id: None,
        }
    }

    fn make_dim_subscript(dim: i64) -> ast::Subscript {
        ast::Subscript::Expression(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&dim.to_string()),
        })
    }

    fn make_cref_subscript(path: &str) -> ast::Subscript {
        ast::Subscript::Expression(ast::Expression::ComponentReference(
            ast::ComponentReference {
                local: false,
                parts: path
                    .split('.')
                    .map(|part| ast::ComponentRefPart {
                        ident: make_token(part),
                        subs: None,
                    })
                    .collect(),
                def_id: None,
            },
        ))
    }

    #[test]
    fn test_resolve_component_dimensions_appends_type_alias_dims() {
        let comp = ast::Component {
            shape: vec![2],
            shape_expr: vec![make_dim_subscript(2)],
            ..Default::default()
        };
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[4],
            &ast::ModificationEnvironment::default(),
            &IndexMap::new(),
            &ast::ClassTree::default(),
        );
        assert_eq!(dims, vec![2, 4]);
        assert!(dims_expr.is_empty());
    }

    #[test]
    fn test_resolve_component_dimensions_evaluates_shape_expr_without_shape_fallback() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_dim_subscript(2)],
            ..Default::default()
        };
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::new(),
            &ast::ClassTree::default(),
        );
        assert_eq!(dims, vec![2]);
        assert!(dims_expr.is_empty());
    }

    #[test]
    fn test_resolve_component_dimensions_preserves_unresolved_shape_expr_without_shape_fallback() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_cref_subscript("Medium.nC")],
            ..Default::default()
        };
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::new(),
            &ast::ClassTree::default(),
        );
        assert!(dims.is_empty());
        assert_eq!(dims_expr.len(), 1);
    }

    #[test]
    fn test_resolve_component_dimensions_keeps_symbolic_shape_expr_after_early_eval() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_cref_subscript("nout")],
            ..Default::default()
        };
        let mut effective_components = IndexMap::new();
        effective_components.insert(
            "nout".to_string(),
            ast::Component {
                binding: Some(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: make_token("2"),
                }),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
        );
        assert!(dims.is_empty());
        assert_eq!(dims_expr.len(), 1);
    }

    #[test]
    fn test_resolve_component_dimensions_mixed_colon_shape_does_not_keep_partial_fallback_dims() {
        let comp = ast::Component {
            shape: vec![2],
            shape_expr: vec![
                ast::Subscript::Range {
                    token: make_token(":"),
                },
                make_dim_subscript(2),
            ],
            ..Default::default()
        };
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::new(),
            &ast::ClassTree::default(),
        );
        assert!(
            dims.is_empty(),
            "mixed colon dimensions must defer to late inference, not keep partial fallback dims"
        );
        assert_eq!(dims_expr.len(), 2);
    }

    #[test]
    fn test_resolve_type_alias_dimensions_through_extends_chain() {
        // Reproduces quaternion aliases used in MultiBody:
        // type QuaternionBase = Real[4];
        // type Orientation = QuaternionBase;
        let mut tree = ast::ClassTree::default();

        let quaternion_base = ast::ClassDef {
            name: make_token("QuaternionBase"),
            class_type: ast::ClassType::Type,
            extends: vec![ast::Extend {
                base_name: make_name("Real"),
                ..Default::default()
            }],
            array_subscripts: vec![make_dim_subscript(4)],
            ..Default::default()
        };

        let orientation = ast::ClassDef {
            name: make_token("Orientation"),
            class_type: ast::ClassType::Type,
            extends: vec![ast::Extend {
                base_name: make_name("QuaternionBase"),
                ..Default::default()
            }],
            ..Default::default()
        };

        tree.definitions
            .classes
            .insert("QuaternionBase".to_string(), quaternion_base);
        tree.definitions
            .classes
            .insert("Orientation".to_string(), orientation);

        let class_def = tree.definitions.classes.get("Orientation");
        let dims = resolve_type_alias_dimensions(
            &tree,
            class_def,
            &ast::ModificationEnvironment::default(),
            &IndexMap::new(),
        );
        assert_eq!(dims, vec![4]);
    }
}
