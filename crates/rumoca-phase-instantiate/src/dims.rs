use super::find_class_in_tree;
use super::inheritance::resolve_effective_components_for_eval;
use rumoca_core::is_builtin_type;
use rumoca_core::{DefId, split_path_with_indices};
use rumoca_eval_ast::eval_instantiate::{evaluate_array_dimensions, try_eval_integer_shape_expr};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

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

    evaluate_array_dimensions(
        &[],
        &subscripts,
        mod_env,
        effective_components,
        tree,
        resolve_effective_components_for_eval,
    )
    .unwrap_or_default()
}

pub(super) fn resolve_component_dimensions(
    comp: &ast::Component,
    type_dims: &[i64],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    imports: &[(String, String)],
) -> (Vec<i64>, Vec<ast::Subscript>) {
    let mut dims = Vec::new();
    let mut dims_expr = Vec::new();
    let mut shape_eval_succeeded = false;
    let qualified_shape_expr = qualify_shape_subscripts_imports(&comp.shape_expr, imports);
    let needs_late_recompute =
        !qualified_shape_expr.is_empty() && shape_expr_needs_late_recompute(&qualified_shape_expr);

    if !qualified_shape_expr.is_empty() {
        if let Some(eval_dims) = eval_shape_expr_dims(
            &qualified_shape_expr,
            mod_env,
            effective_components,
            tree,
            imports,
        ) {
            if needs_late_recompute {
                // Defer symbolic dimensions to later phases that have full local
                // scope/modifier context (MLS §10.1 structural dimensions).
                // Do not trust parser/early fallback dimensions here, because they
                // may have dropped colon positions (e.g. `[:, 2]` -> `[2]`) or
                // captured values from an outer scope before local parameters
                // converged.
                dims_expr = qualified_shape_expr.clone();
            } else {
                dims = eval_dims;
                shape_eval_succeeded = true;
            }
        } else if needs_late_recompute {
            // Preserve symbolic/range expressions for late fixed-point passes.
            dims_expr = qualified_shape_expr.clone();
        } else {
            // Keep a numeric fallback when available for non-symbolic shapes.
            dims = comp.shape.iter().map(|&d| d as i64).collect();
            let preserve_shape_expr_fallback =
                mod_env_has_package_alias_bindings(mod_env) || comp.shape.is_empty();
            if preserve_shape_expr_fallback {
                dims_expr = qualified_shape_expr.clone();
            }
        }
    } else if !comp.shape.is_empty() {
        dims = comp.shape.iter().map(|&d| d as i64).collect();
    }

    // Append dimensions inherited from type aliases (e.g., Orientation -> Real[4]).
    // If component dimensions remain symbolic, preserve them for later lowering;
    // otherwise extend the concrete base shape with alias dimensions.
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
    imports: &[(String, String)],
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for sub in shape_expr {
        let ast::Subscript::Expression(expr) = sub else {
            return None;
        };
        let expr = qualify_shape_expr_imports(expr, imports);
        // MLS §10.1: structural dimension expressions may use compile-time `if`
        // branches over parameter/constant conditions.
        let dim = try_eval_integer_shape_expr(
            &expr,
            mod_env,
            effective_components,
            tree,
            resolve_effective_components_for_eval,
        )?;
        if dim < 0 {
            return None;
        }
        dims.push(dim);
    }
    Some(dims)
}

pub(super) fn qualify_shape_subscripts_imports(
    shape_expr: &[ast::Subscript],
    imports: &[(String, String)],
) -> Vec<ast::Subscript> {
    shape_expr
        .iter()
        .map(|subscript| match subscript {
            ast::Subscript::Expression(expr) => {
                ast::Subscript::Expression(qualify_shape_expr_imports(expr, imports))
            }
            ast::Subscript::Range { token } => ast::Subscript::Range {
                token: token.clone(),
            },
            ast::Subscript::Empty => ast::Subscript::Empty,
        })
        .collect()
}

fn qualify_shape_expr_imports(
    expr: &ast::Expression,
    imports: &[(String, String)],
) -> ast::Expression {
    match expr {
        ast::Expression::ComponentReference(cref) => {
            ast::Expression::ComponentReference(qualify_component_ref_imports(cref, imports))
        }
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => ast::Expression::Range {
            start: Arc::new(qualify_shape_expr_imports(start, imports)),
            step: step
                .as_ref()
                .map(|expr| Arc::new(qualify_shape_expr_imports(expr, imports))),
            end: Arc::new(qualify_shape_expr_imports(end, imports)),
            span: *span,
        },
        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(qualify_shape_expr_imports(rhs, imports)),
            span: *span,
        },
        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(qualify_shape_expr_imports(lhs, imports)),
            rhs: Arc::new(qualify_shape_expr_imports(rhs, imports)),
            span: *span,
        },
        ast::Expression::If {
            branches,
            else_branch,
            span,
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, body)| {
                    (
                        qualify_shape_expr_imports(cond, imports),
                        qualify_shape_expr_imports(body, imports),
                    )
                })
                .collect(),
            else_branch: Arc::new(qualify_shape_expr_imports(else_branch, imports)),
            span: *span,
        },
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: Arc::new(qualify_shape_expr_imports(inner, imports)),
            span: *span,
        },
        ast::Expression::FunctionCall { comp, args, span } => ast::Expression::FunctionCall {
            comp: qualify_component_ref_imports(comp, imports),
            args: args
                .iter()
                .map(|arg| qualify_shape_expr_imports(arg, imports))
                .collect(),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn qualify_component_ref_imports(
    cref: &ast::ComponentReference,
    imports: &[(String, String)],
) -> ast::ComponentReference {
    let Some(first) = cref.parts.first() else {
        return cref.clone();
    };
    let alias = first.ident.text.as_ref();
    let Some((_, target)) = imports
        .iter()
        .rev()
        .find(|(candidate, _)| candidate == alias)
    else {
        return cref.clone();
    };

    let mut parts = split_path_with_indices(target)
        .into_iter()
        .map(|segment| ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(segment),
                ..rumoca_core::Token::default()
            },
            subs: None,
        })
        .collect::<Vec<_>>();
    if let Some(last) = parts.last_mut() {
        last.subs = first.subs.clone();
    }
    parts.extend(cref.parts.iter().skip(1).cloned());

    ast::ComponentReference {
        local: cref.local,
        parts,
        def_id: cref.def_id,
        span: cref.span,
    }
}

#[cfg(test)]
mod tests {
    use super::{resolve_component_dimensions, resolve_type_alias_dimensions};
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::AstIndexMap as IndexMap;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
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
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_cref_subscript(path: &str) -> ast::Subscript {
        ast::Subscript::Expression(ast::Expression::ComponentReference(
            ast::ComponentReference {
                local: false,
                parts: rumoca_core::split_path_with_indices(path)
                    .into_iter()
                    .map(|part| ast::ComponentRefPart {
                        ident: make_token(part),
                        subs: None,
                    })
                    .collect(),
                def_id: None,
                span: rumoca_core::Span::DUMMY,
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
            &IndexMap::default(),
            &ast::ClassTree::default(),
            &[],
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
            &IndexMap::default(),
            &ast::ClassTree::default(),
            &[],
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
            &IndexMap::default(),
            &ast::ClassTree::default(),
            &[],
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
        let mut effective_components = IndexMap::default();
        effective_components.insert(
            "nout".to_string(),
            ast::Component {
                binding: Some(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: make_token("2"),
                    span: rumoca_core::Span::DUMMY,
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
            &[],
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
            &IndexMap::default(),
            &ast::ClassTree::default(),
            &[],
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
            class_type: rumoca_core::ClassType::Type,
            extends: vec![ast::Extend {
                base_name: make_name("Real"),
                ..Default::default()
            }],
            array_subscripts: vec![make_dim_subscript(4)],
            ..Default::default()
        };

        let orientation = ast::ClassDef {
            name: make_token("Orientation"),
            class_type: rumoca_core::ClassType::Type,
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
            &IndexMap::default(),
        );
        assert_eq!(dims, vec![4]);
    }
}
