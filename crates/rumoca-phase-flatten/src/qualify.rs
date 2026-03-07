//! Shared expression and component reference qualification utilities.
//!
//! This module provides functions for qualifying variable names with instance prefixes,
//! used by both equation flattening and algorithm processing.

use rumoca_ir_ast as ast;
use rumoca_ir_ast::{
    ComponentRefPart, ComponentReference, Expression, ForIndex, QualifiedName, Subscript,
    TerminalType, Token,
};
use rustc_hash::FxHashMap;
use std::collections::HashSet;
use std::sync::Arc;

/// Resolved import map: short name → fully-qualified name.
///
/// Built from `ClassDef.imports` during instantiation. For example,
/// `import Modelica.Constants.pi;` produces `("pi", "Modelica.Constants.pi")`.
pub type ImportMap = FxHashMap<String, String>;

/// Add lexical package aliases visible from `class_name` into the import map.
///
/// Modelica class/package names are visible through lexical scope nesting.
/// This helper materializes those package-name aliases so short package refs
/// (for example `Common.*` inside `Modelica.Media.IdealGases.*`) can be
/// qualified to their fully qualified names without heuristics.
pub(crate) fn collect_lexical_package_aliases(
    tree: &ast::ClassTree,
    class_name: &str,
    imports: &mut ImportMap,
) {
    let mut ancestors = Vec::new();
    let mut scope = class_name;
    while let Some((parent, _)) = scope.rsplit_once('.') {
        ancestors.push(parent.to_string());
        scope = parent;
    }

    for ancestor in ancestors {
        let Some(ancestor_class) = tree.get_class_by_qualified_name(&ancestor) else {
            continue;
        };
        for (name, nested) in &ancestor_class.classes {
            if !matches!(nested.class_type, ast::ClassType::Package) {
                continue;
            }
            imports
                .entry(name.clone())
                .or_insert_with(|| format!("{ancestor}.{name}"));
        }
    }
}

/// Options for component reference qualification.
#[derive(Default, Clone, Copy)]
pub struct QualifyOptions {
    /// Whether to skip qualification of local references.
    pub skip_local: bool,
    /// Whether to preserve the original def_id (false = reset to None).
    pub preserve_def_id: bool,
}

// ── Public API (backward-compatible, no imports) ────────────────────────────

/// Qualify a component reference by prepending prefix parts.
pub fn qualify_component_ref(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
) -> ComponentReference {
    let locals = HashSet::new();
    let imports = ImportMap::default();
    qualify_cr_inner(cr, prefix, opts, &locals, &imports)
}

/// Qualify an expression by qualifying all component references within it.
pub fn qualify_expression(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
) -> Expression {
    let locals = HashSet::new();
    let imports = ImportMap::default();
    qualify_expr_inner(expr, prefix, opts, &locals, &imports)
}

// ── Public API (import-aware) ───────────────────────────────────────────────

/// Qualify a component reference, resolving imported short names via the import map.
pub fn qualify_component_ref_with_imports(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    imports: &ImportMap,
) -> ComponentReference {
    let locals = HashSet::new();
    qualify_cr_inner(cr, prefix, opts, &locals, imports)
}

/// Qualify a component reference with explicit local-scope identifiers.
///
/// This is used by algorithm `for`/comprehension handling where loop indices
/// must stay local inside nested expressions (MLS §11.2.2.2).
pub fn qualify_component_ref_with_imports_and_locals(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentReference {
    qualify_cr_inner(cr, prefix, opts, locals, imports)
}

/// Qualify an expression, resolving imported short names via the import map.
///
/// Single-part references that match an import entry are expanded to their
/// fully-qualified form instead of being prefixed with the component path.
pub fn qualify_expression_with_imports(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    imports: &ImportMap,
) -> Expression {
    let locals = HashSet::new();
    qualify_expr_inner(expr, prefix, opts, &locals, imports)
}

/// Qualify an expression with explicit local-scope identifiers.
///
/// This is used by algorithm `for`/comprehension handling where loop indices
/// must stay local inside nested expressions (MLS §11.2.2.2).
pub fn qualify_expression_with_imports_and_locals(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    qualify_expr_inner(expr, prefix, opts, locals, imports)
}

// ── Utility functions ───────────────────────────────────────────────────────

/// Convert integer subscripts to Subscript expressions.
pub fn subscripts_from_indices(indices: &[i64]) -> Option<Vec<Subscript>> {
    if indices.is_empty() {
        return None;
    }
    Some(
        indices
            .iter()
            .map(|&i| Subscript::Expression(int_expr(i)))
            .collect(),
    )
}

/// Create an integer literal expression.
pub fn int_expr(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: std::sync::Arc::from(value.to_string()),
            ..Default::default()
        },
    }
}

// ── Internal implementation ─────────────────────────────────────────────────

fn is_local_iterator_ref(cr: &ComponentReference, locals: &HashSet<String>) -> bool {
    cr.parts.len() == 1 && locals.contains(cr.parts[0].ident.text.as_ref())
}

/// Check if a component reference appears to be already fully-qualified.
///
/// A reference is considered fully-qualified if:
/// - The first part is a known package name like "Modelica"
///
/// This prevents instance prefixes from being added to global references like
/// `Modelica.Constants.eps` which would incorrectly become `instance.Modelica.Constants.eps`.
///
/// Note: We only check for known packages, not uppercase heuristics. References like
/// `ICP.di` (where ICP is a sub-component) must still get prefixed to become `L1.ICP.di`.
fn is_likely_fully_qualified(cr: &ComponentReference) -> bool {
    if cr.parts.is_empty() {
        return false;
    }

    let first_part = cr.parts[0].ident.text.as_ref();

    let known_packages = [
        "Modelica",
        "ModelicaTest",
        "ModelicaTestOverdetermined",
        "Complex",
        "ModelicaServices",
        "Modelica_DeviceDrivers",
        "Buildings",
        "OpenIPSL",
        "PowerSystems",
        "ThermoPower",
    ];

    known_packages.contains(&first_part)
}

/// Built-in enumeration literals are globally visible and must not be
/// qualified with instance prefixes (MLS §4.4.4.2, §8.3.7).
fn is_builtin_enum_literal_ref(cr: &ComponentReference) -> bool {
    if cr.parts.len() < 2 {
        return false;
    }
    matches!(
        cr.parts[0].ident.text.as_ref(),
        "StateSelect" | "AssertionLevel"
    )
}

/// Build a `ComponentReference` from a dotted fully-qualified name.
fn make_fqn_component_ref(fqn: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: fqn
            .split('.')
            .map(|seg| ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from(seg),
                    ..Default::default()
                },
                subs: None,
            })
            .collect(),
        def_id: None,
    }
}

fn qualify_component_part_subs(
    part: &ComponentRefPart,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentRefPart {
    ComponentRefPart {
        ident: part.ident.clone(),
        subs: part.subs.as_ref().map(|subs| {
            subs.iter()
                .map(|sub| qualify_sub_inner(sub, prefix, opts, locals, imports))
                .collect()
        }),
    }
}

fn resolve_import_alias_ref(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Option<ComponentReference> {
    let first_part = cr.parts.first()?;
    let alias = first_part.ident.text.as_ref();
    let fqn = imports.get(alias)?;

    let mut imported_parts = make_fqn_component_ref(fqn).parts;
    if cr.parts.len() == 1 {
        let non_empty_subs = first_part.subs.as_ref().filter(|subs| !subs.is_empty());
        if let Some(subs) = non_empty_subs {
            let last_part = imported_parts.last_mut()?;
            last_part.subs = Some(
                subs.iter()
                    .map(|sub| qualify_sub_inner(sub, prefix, opts, locals, imports))
                    .collect(),
            );
        }
    } else {
        imported_parts.extend(
            cr.parts
                .iter()
                .skip(1)
                .map(|part| qualify_component_part_subs(part, prefix, opts, locals, imports)),
        );
    }

    Some(ComponentReference {
        local: false,
        parts: imported_parts,
        def_id: if opts.preserve_def_id {
            cr.def_id
        } else {
            None
        },
    })
}

fn qualify_cr_inner(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentReference {
    let qualify_part_subs =
        |part: &ComponentRefPart| qualify_component_part_subs(part, prefix, opts, locals, imports);

    if is_local_iterator_ref(cr, locals) {
        let mut local = cr.clone();
        local.parts = local.parts.iter().map(qualify_part_subs).collect();
        return local;
    }

    // Skip if local and option is set
    if opts.skip_local && cr.local {
        return cr.clone();
    }

    // Skip qualification if reference appears to be already fully-qualified
    if is_likely_fully_qualified(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
        };
    }

    if is_builtin_enum_literal_ref(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
        };
    }

    // MLS §3.7.3: `time` is a built-in variable, never a component member.
    if cr.parts.len() == 1 && cr.parts[0].ident.text.as_ref() == "time" {
        return cr.clone();
    }

    // MLS §13.2: Resolve imported short names to their fully-qualified form.
    // E.g., `pi` from `import Modelica.Constants.pi` → `Modelica.Constants.pi`,
    // and `L.'1'` from `import ...Interfaces.Logic as L` →
    // `Modelica.Electrical.Digital.Interfaces.Logic.'1'`.
    //
    // This must happen BEFORE the empty-prefix early-return so top-level and
    // modification bindings still get import resolution.
    if let Some(imported_ref) = resolve_import_alias_ref(cr, prefix, opts, locals, imports) {
        return imported_ref;
    }

    // No prefix: nothing to prepend, return as-is (imports already resolved above)
    if prefix.is_empty() {
        return cr.clone();
    }

    let mut parts = Vec::with_capacity(prefix.parts.len() + cr.parts.len());

    // Add prefix parts
    for (name, subs) in &prefix.parts {
        parts.push(ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name.as_str()),
                ..Default::default()
            },
            subs: subscripts_from_indices(subs),
        });
    }

    // Add original parts, qualifying any subscript expressions within them.
    for part in &cr.parts {
        parts.push(qualify_part_subs(part));
    }

    ComponentReference {
        local: if opts.skip_local { false } else { cr.local },
        parts,
        def_id: if opts.preserve_def_id {
            cr.def_id
        } else {
            None
        },
    }
}

fn qualify_vec_inner(
    exprs: &[Expression],
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|e| qualify_expr_inner(e, prefix, opts, locals, imports))
        .collect()
}

fn qualify_opt_inner(
    expr: &Option<Arc<Expression>>,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Option<Arc<Expression>> {
    expr.as_ref().map(|e| {
        Arc::new(qualify_expr_inner(
            e.as_ref(),
            prefix,
            opts,
            locals,
            imports,
        ))
    })
}

fn locals_with_indices(locals: &HashSet<String>, indices: &[ForIndex]) -> HashSet<String> {
    let mut scoped = locals.clone();
    for index in indices {
        scoped.insert(index.ident.text.to_string());
    }
    scoped
}

fn qualify_if_inner(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, body)| {
                (
                    qualify_expr_inner(cond, prefix, opts, locals, imports),
                    qualify_expr_inner(body, prefix, opts, locals, imports),
                )
            })
            .collect(),
        else_branch: Arc::new(qualify_expr_inner(
            else_branch,
            prefix,
            opts,
            locals,
            imports,
        )),
    }
}

fn qualify_array_comprehension_inner(
    inner_expr: &Expression,
    indices: &[ForIndex],
    filter: &Option<Arc<Expression>>,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    let comprehension_locals = locals_with_indices(locals, indices);
    Expression::ArrayComprehension {
        expr: Arc::new(qualify_expr_inner(
            inner_expr,
            prefix,
            opts,
            &comprehension_locals,
            imports,
        )),
        indices: qualify_for_indices_inner(indices, prefix, opts, locals, imports),
        filter: qualify_opt_inner(filter, prefix, opts, &comprehension_locals, imports),
    }
}

fn qualify_expr_inner(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    match expr {
        Expression::ComponentReference(cr) => {
            Expression::ComponentReference(qualify_cr_inner(cr, prefix, opts, locals, imports))
        }
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(qualify_expr_inner(lhs, prefix, opts, locals, imports)),
            rhs: Arc::new(qualify_expr_inner(rhs, prefix, opts, locals, imports)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(qualify_expr_inner(rhs, prefix, opts, locals, imports)),
        },
        Expression::FunctionCall { comp, args } => Expression::FunctionCall {
            comp: comp.clone(),
            args: qualify_vec_inner(args, prefix, opts, locals, imports),
        },
        Expression::If {
            branches,
            else_branch,
        } => qualify_if_inner(branches, else_branch, prefix, opts, locals, imports),
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: qualify_vec_inner(elements, prefix, opts, locals, imports),
            is_matrix: *is_matrix,
        },
        Expression::Range { start, step, end } => Expression::Range {
            start: Arc::new(qualify_expr_inner(start, prefix, opts, locals, imports)),
            step: qualify_opt_inner(step, prefix, opts, locals, imports),
            end: Arc::new(qualify_expr_inner(end, prefix, opts, locals, imports)),
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: qualify_vec_inner(elements, prefix, opts, locals, imports),
        },
        Expression::Parenthesized { inner } => Expression::Parenthesized {
            inner: Arc::new(qualify_expr_inner(inner, prefix, opts, locals, imports)),
        },
        Expression::Terminal { .. } | Expression::Empty => expr.clone(),
        Expression::ClassModification {
            target,
            modifications,
        } => Expression::ClassModification {
            target: target.clone(),
            modifications: qualify_vec_inner(modifications, prefix, opts, locals, imports),
        },
        Expression::NamedArgument { name, value } => Expression::NamedArgument {
            name: name.clone(),
            value: Arc::new(qualify_expr_inner(value, prefix, opts, locals, imports)),
        },
        Expression::Modification { target, value } => Expression::Modification {
            target: target.clone(),
            value: Arc::new(qualify_expr_inner(value, prefix, opts, locals, imports)),
        },
        Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            filter,
        } => qualify_array_comprehension_inner(
            inner_expr, indices, filter, prefix, opts, locals, imports,
        ),
        Expression::ArrayIndex { base, subscripts } => Expression::ArrayIndex {
            base: Arc::new(qualify_expr_inner(base, prefix, opts, locals, imports)),
            subscripts: subscripts
                .iter()
                .map(|s| qualify_sub_inner(s, prefix, opts, locals, imports))
                .collect(),
        },
        Expression::FieldAccess { base, field } => Expression::FieldAccess {
            base: Arc::new(qualify_expr_inner(base, prefix, opts, locals, imports)),
            field: field.clone(),
        },
    }
}

fn qualify_for_indices_inner(
    indices: &[ForIndex],
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Vec<ForIndex> {
    let mut active_locals = locals.clone();
    let mut qualified_indices = Vec::with_capacity(indices.len());

    // MLS §10.4.1: range expressions are evaluated in lexical order where each
    // index introduces a new local name visible to subsequent ranges.
    for index in indices {
        let qualified_range =
            qualify_expr_inner(&index.range, prefix, opts, &active_locals, imports);
        qualified_indices.push(ForIndex {
            ident: index.ident.clone(),
            range: qualified_range,
        });
        active_locals.insert(index.ident.text.to_string());
    }

    qualified_indices
}

fn qualify_sub_inner(
    sub: &rumoca_ir_ast::Subscript,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> rumoca_ir_ast::Subscript {
    match sub {
        rumoca_ir_ast::Subscript::Expression(e) => rumoca_ir_ast::Subscript::Expression(
            qualify_expr_inner(e, prefix, opts, locals, imports),
        ),
        rumoca_ir_ast::Subscript::Range { token } => rumoca_ir_ast::Subscript::Range {
            token: token.clone(),
        },
        rumoca_ir_ast::Subscript::Empty => rumoca_ir_ast::Subscript::Empty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn make_comp_ref(names: &[&str]) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|n| ComponentRefPart {
                    ident: Token {
                        text: std::sync::Arc::from(*n),
                        ..Default::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn make_int(value: i64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(value.to_string()),
                ..Default::default()
            },
        }
    }

    fn make_for_index(name: &str, range: Expression) -> ForIndex {
        ForIndex {
            ident: Token {
                text: Arc::from(name.to_string()),
                ..Default::default()
            },
            range,
        }
    }

    #[test]
    fn test_qualify_component_ref_empty_prefix() {
        let cr = make_comp_ref(&["x"]);
        let prefix = QualifiedName::new();
        let result = qualify_component_ref(&cr, &prefix, QualifyOptions::default());
        assert_eq!(result.parts.len(), 1);
        assert_eq!(&*result.parts[0].ident.text, "x");
    }

    #[test]
    fn test_qualify_component_ref_with_prefix() {
        let cr = make_comp_ref(&["x"]);
        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let result = qualify_component_ref(&cr, &prefix, QualifyOptions::default());
        assert_eq!(result.parts.len(), 2);
        assert_eq!(&*result.parts[0].ident.text, "comp");
        assert_eq!(&*result.parts[1].ident.text, "x");
    }

    #[test]
    fn test_qualify_local_ref_skipped() {
        let mut cr = make_comp_ref(&["x"]);
        cr.local = true;

        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let opts = QualifyOptions {
            skip_local: true,
            preserve_def_id: false,
        };
        let result = qualify_component_ref(&cr, &prefix, opts);

        // Local ref should not be qualified
        assert_eq!(result.parts.len(), 1);
        assert_eq!(&*result.parts[0].ident.text, "x");
    }

    #[test]
    fn test_int_expr() {
        let expr = int_expr(42);
        if let Expression::Terminal {
            terminal_type,
            token,
        } = expr
        {
            assert_eq!(terminal_type, TerminalType::UnsignedInteger);
            assert_eq!(&*token.text, "42");
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_qualify_array_comprehension_keeps_iterator_local() {
        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let expr = Expression::ArrayComprehension {
            expr: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
            indices: vec![make_for_index(
                "i",
                Expression::Range {
                    start: Arc::new(make_int(1)),
                    step: None,
                    end: Arc::new(Expression::ComponentReference(make_comp_ref(&["n"]))),
                },
            )],
            filter: None,
        };

        let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
        let Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } = qualified
        else {
            panic!("Expected ArrayComprehension");
        };
        assert!(filter.is_none());

        let Expression::ComponentReference(body_ref) = expr.as_ref() else {
            panic!("Expected comprehension body ComponentReference");
        };
        assert_eq!(body_ref.parts.len(), 1);
        assert_eq!(body_ref.parts[0].ident.text.as_ref(), "i");

        let Expression::Range { end, .. } = &indices[0].range else {
            panic!("Expected range in comprehension index");
        };
        let Expression::ComponentReference(end_ref) = end.as_ref() else {
            panic!("Expected qualified range end component reference");
        };
        assert_eq!(end_ref.parts.len(), 2);
        assert_eq!(end_ref.parts[0].ident.text.as_ref(), "comp");
        assert_eq!(end_ref.parts[1].ident.text.as_ref(), "n");
    }

    #[test]
    fn test_qualify_array_comprehension_range_sees_prior_indices() {
        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let expr = Expression::ArrayComprehension {
            expr: Arc::new(Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Token::default()),
                lhs: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
                rhs: Arc::new(Expression::ComponentReference(make_comp_ref(&["j"]))),
            }),
            indices: vec![
                make_for_index(
                    "i",
                    Expression::Range {
                        start: Arc::new(make_int(1)),
                        step: None,
                        end: Arc::new(Expression::ComponentReference(make_comp_ref(&["n"]))),
                    },
                ),
                make_for_index(
                    "j",
                    Expression::Range {
                        start: Arc::new(make_int(1)),
                        step: None,
                        end: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
                    },
                ),
            ],
            filter: None,
        };

        let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
        let Expression::ArrayComprehension { indices, .. } = qualified else {
            panic!("Expected ArrayComprehension");
        };
        let Expression::Range { end, .. } = &indices[1].range else {
            panic!("Expected range in second comprehension index");
        };
        let Expression::ComponentReference(end_ref) = end.as_ref() else {
            panic!("Expected component reference in second range end");
        };
        assert_eq!(end_ref.parts.len(), 1);
        assert_eq!(end_ref.parts[0].ident.text.as_ref(), "i");
    }

    #[test]
    fn test_import_resolves_short_name_to_fqn() {
        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let mut imports = ImportMap::default();
        imports.insert("pi".to_string(), "Modelica.Constants.pi".to_string());

        let expr = Expression::ComponentReference(make_comp_ref(&["pi"]));
        let qualified =
            qualify_expression_with_imports(&expr, &prefix, QualifyOptions::default(), &imports);

        let Expression::ComponentReference(cr) = qualified else {
            panic!("Expected ComponentReference");
        };
        assert_eq!(cr.parts.len(), 3);
        assert_eq!(cr.parts[0].ident.text.as_ref(), "Modelica");
        assert_eq!(cr.parts[1].ident.text.as_ref(), "Constants");
        assert_eq!(cr.parts[2].ident.text.as_ref(), "pi");
    }

    #[test]
    fn test_time_builtin_not_qualified() {
        let mut prefix = QualifiedName::new();
        prefix.push("source".to_string(), vec![]);

        let expr = Expression::ComponentReference(make_comp_ref(&["time"]));
        let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());

        let Expression::ComponentReference(cr) = qualified else {
            panic!("Expected ComponentReference");
        };
        assert_eq!(cr.parts.len(), 1);
        assert_eq!(cr.parts[0].ident.text.as_ref(), "time");
    }

    #[test]
    fn test_builtin_enum_literals_not_qualified() {
        let mut prefix = QualifiedName::new();
        prefix.push("source".to_string(), vec![]);

        let state_select =
            Expression::ComponentReference(make_comp_ref(&["StateSelect", "default"]));
        let qualified_state = qualify_expression(&state_select, &prefix, QualifyOptions::default());
        let Expression::ComponentReference(state_cr) = qualified_state else {
            panic!("Expected ComponentReference for StateSelect literal");
        };
        assert_eq!(state_cr.parts.len(), 2);
        assert_eq!(state_cr.parts[0].ident.text.as_ref(), "StateSelect");
        assert_eq!(state_cr.parts[1].ident.text.as_ref(), "default");

        let assertion = Expression::ComponentReference(make_comp_ref(&["AssertionLevel", "error"]));
        let qualified_assertion =
            qualify_expression(&assertion, &prefix, QualifyOptions::default());
        let Expression::ComponentReference(assert_cr) = qualified_assertion else {
            panic!("Expected ComponentReference for AssertionLevel literal");
        };
        assert_eq!(assert_cr.parts.len(), 2);
        assert_eq!(assert_cr.parts[0].ident.text.as_ref(), "AssertionLevel");
        assert_eq!(assert_cr.parts[1].ident.text.as_ref(), "error");
    }

    #[test]
    fn test_non_imported_name_still_qualified() {
        let mut prefix = QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let mut imports = ImportMap::default();
        imports.insert("pi".to_string(), "Modelica.Constants.pi".to_string());

        // "f" is not in imports, so it should be qualified normally
        let expr = Expression::ComponentReference(make_comp_ref(&["f"]));
        let qualified =
            qualify_expression_with_imports(&expr, &prefix, QualifyOptions::default(), &imports);

        let Expression::ComponentReference(cr) = qualified else {
            panic!("Expected ComponentReference");
        };
        assert_eq!(cr.parts.len(), 2);
        assert_eq!(cr.parts[0].ident.text.as_ref(), "comp");
        assert_eq!(cr.parts[1].ident.text.as_ref(), "f");
    }

    #[test]
    fn test_import_alias_resolves_prefixed_component_reference() {
        let mut prefix = QualifiedName::new();
        prefix.push("inst".to_string(), vec![]);

        let mut imports = ImportMap::default();
        imports.insert(
            "L".to_string(),
            "Modelica.Electrical.Digital.Interfaces.Logic".to_string(),
        );

        let expr = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("L"),
                        ..Default::default()
                    },
                    subs: None,
                },
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("'1'"),
                        ..Default::default()
                    },
                    subs: None,
                },
            ],
            def_id: None,
        });

        let qualified =
            qualify_expression_with_imports(&expr, &prefix, QualifyOptions::default(), &imports);
        let Expression::ComponentReference(cr) = qualified else {
            panic!("Expected ComponentReference");
        };
        let parts: Vec<&str> = cr
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect();
        assert_eq!(
            parts,
            vec![
                "Modelica",
                "Electrical",
                "Digital",
                "Interfaces",
                "Logic",
                "'1'"
            ]
        );
    }

    #[test]
    fn test_fully_qualified_ref_qualifies_subscript_expressions() {
        let mut prefix = QualifiedName::new();
        prefix.push("MUX".to_string(), vec![]);
        prefix.push("And1".to_string(), vec![]);

        let ref_with_subscripts = ComponentReference {
            local: false,
            parts: vec![
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("Modelica"),
                        ..Default::default()
                    },
                    subs: None,
                },
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("Electrical"),
                        ..Default::default()
                    },
                    subs: None,
                },
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("Digital"),
                        ..Default::default()
                    },
                    subs: None,
                },
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("Tables"),
                        ..Default::default()
                    },
                    subs: None,
                },
                ComponentRefPart {
                    ident: Token {
                        text: Arc::from("AndTable"),
                        ..Default::default()
                    },
                    subs: Some(vec![
                        Subscript::Expression(Expression::ComponentReference(ComponentReference {
                            local: false,
                            parts: vec![ComponentRefPart {
                                ident: Token {
                                    text: Arc::from("auxiliary"),
                                    ..Default::default()
                                },
                                subs: Some(vec![Subscript::Expression(make_int(1))]),
                            }],
                            def_id: None,
                        })),
                        Subscript::Expression(Expression::ComponentReference(ComponentReference {
                            local: false,
                            parts: vec![ComponentRefPart {
                                ident: Token {
                                    text: Arc::from("x"),
                                    ..Default::default()
                                },
                                subs: Some(vec![Subscript::Expression(make_int(2))]),
                            }],
                            def_id: None,
                        })),
                    ]),
                },
            ],
            def_id: None,
        };

        let expr = Expression::ComponentReference(ref_with_subscripts);
        let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
        let Expression::ComponentReference(qualified_ref) = qualified else {
            panic!("Expected ComponentReference");
        };
        assert_eq!(qualified_ref.parts.len(), 5);
        assert_eq!(qualified_ref.parts[0].ident.text.as_ref(), "Modelica");
        let Some(subscripts) = qualified_ref.parts[4].subs.as_ref() else {
            panic!("expected AndTable subscripts");
        };
        assert_eq!(subscripts.len(), 2);

        let Subscript::Expression(Expression::ComponentReference(aux_ref)) = &subscripts[0] else {
            panic!("expected auxiliary ref in first subscript");
        };
        assert_eq!(aux_ref.parts.len(), 3);
        assert_eq!(aux_ref.parts[0].ident.text.as_ref(), "MUX");
        assert_eq!(aux_ref.parts[1].ident.text.as_ref(), "And1");
        assert_eq!(aux_ref.parts[2].ident.text.as_ref(), "auxiliary");

        let Subscript::Expression(Expression::ComponentReference(x_ref)) = &subscripts[1] else {
            panic!("expected x ref in second subscript");
        };
        assert_eq!(x_ref.parts.len(), 3);
        assert_eq!(x_ref.parts[0].ident.text.as_ref(), "MUX");
        assert_eq!(x_ref.parts[1].ident.text.as_ref(), "And1");
        assert_eq!(x_ref.parts[2].ident.text.as_ref(), "x");
    }
}
