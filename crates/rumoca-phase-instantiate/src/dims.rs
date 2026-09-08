use super::inheritance::location_to_span;
use super::inheritance::resolve_effective_components_for_eval;
use super::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_eval_ast::eval_instantiate::{
    evaluate_array_dimensions_with_index, try_eval_integer_shape_expr_with_proof,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

/// The rewrite vocabularies an unqualified name in a shape, condition, or
/// attribute expression can resolve through.
///
/// Import bindings come from the one lookup authority for the expression's
/// declaring scope and carry per-segment declaration identity; no import is
/// ever re-selected by clause order or re-derived from a rendered spelling.
/// Package-constant aliases remain rendered-name pairs because they alias
/// package members exposed by redeclared or enclosing packages (MLS §5.3.2),
/// which are not imports.
#[derive(Clone, Copy)]
pub(crate) struct ImportRewrite<'a> {
    /// Root-issued index of resolved declaration identities. Every production
    /// rewrite and shape evaluation in one instantiation reuses this index.
    pub(crate) class_index: &'a ast::ClassDefIndex<'a>,
    /// Constant aliases of actively redeclared packages. These deliberately
    /// take precedence over import bindings: a redeclared package's constants
    /// replace same-named aliases of the replaced package.
    pub(crate) overriding_aliases: &'a [(String, String)],
    /// Import bindings of the expression's declaring scope (MLS §13.2).
    pub(crate) effective: Option<&'a ast::EffectiveImports>,
    /// Enclosing package-constant aliases, consulted only after imports.
    pub(crate) fallback_aliases: &'a [(String, String)],
}

impl<'a> ImportRewrite<'a> {
    pub(crate) fn without_imports(class_index: &'a ast::ClassDefIndex<'a>) -> Self {
        Self {
            class_index,
            overriding_aliases: &[],
            effective: None,
            fallback_aliases: &[],
        }
    }

    fn names_alias(&self, alias: &str) -> bool {
        self.overriding_aliases
            .iter()
            .any(|(candidate, _)| candidate == alias)
            || self.effective.is_some_and(|effective| {
                effective.mentions(&rumoca_core::ComponentPath::from_flat_path(alias))
            })
            || self
                .fallback_aliases
                .iter()
                .any(|(candidate, _)| candidate == alias)
    }
}

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
) -> InstantiateResult<Vec<ast::Subscript>> {
    let mut subscripts = Vec::new();
    let mut current = class_def;
    let mut visited_defs = std::collections::HashSet::<DefId>::new();

    while let Some(class) = current {
        let def_id = class.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("type alias `{}`", class.name.text),
                class.location.span(),
            ))
        })?;
        if !visited_defs.insert(def_id) {
            return Err(Box::new(InstantiateError::instantiation_cycle(
                format!("type alias `{}`", class.name.text),
                class.location.span(),
            )));
        }

        if !class.array_subscripts.is_empty() {
            subscripts.extend(class.array_subscripts.clone());
        }

        // Only a short class definition can carry alias array subscripts, and
        // a short definition has exactly one extends edge (MLS §4.6). A class
        // with zero extends clauses, or a long definition with several (MLS
        // §7.1 permits multiple inheritance), contributes no further alias
        // dimensions, so the walk terminates here with the subscripts already
        // collected.
        let [ext] = class.extends.as_slice() else {
            current = None;
            continue;
        };
        let base_name = ext.base_name.to_string();
        if super::inheritance::predefined_extend_name(tree, ext)?.is_some() {
            current = None;
            continue;
        }

        let base_def_id = ext.base_def_id.or(ext.base_name.def_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("type-alias extends edge `{base_name}`"),
                ext.location.span(),
            ))
        })?;
        current = Some(tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("type-alias extends edge `{base_name}` ({base_def_id:?})"),
                ext.location.span(),
            ))
        })?);
    }

    Ok(subscripts)
}

/// Resolve array dimensions inherited from a type alias chain.
pub(super) fn resolve_type_alias_dimensions(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    class_index: &ast::ClassDefIndex<'_>,
) -> InstantiateResult<Vec<i64>> {
    let subscripts = collect_type_alias_subscripts(tree, class_def)?;
    if subscripts.is_empty() {
        return Ok(Vec::new());
    }

    let Some(dims) = evaluate_array_dimensions_with_index(
        &[],
        &subscripts,
        mod_env,
        effective_components,
        tree,
        class_index,
        resolve_effective_components_for_eval,
    ) else {
        let name = class_def
            .map(|class| class.name.text.to_string())
            .unwrap_or_else(|| "<anonymous type alias>".to_string());
        let Some(class) = class_def else {
            return Err(Box::new(InstantiateError::missing_source_context(
                "type-alias array dimensions are missing source class provenance",
            )));
        };
        let span = location_to_span(&class.location, &tree.source_map, "type-alias class")?;
        return Err(Box::new(InstantiateError::structural_param_error(
            name,
            "cannot evaluate type-alias array dimensions",
            span,
        )));
    };
    Ok(dims)
}

pub(super) fn resolve_component_dimensions(
    comp: &ast::Component,
    type_dims: &[i64],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    imports: ImportRewrite<'_>,
) -> InstantiateResult<(Vec<i64>, Vec<ast::Subscript>)> {
    let mut dims = Vec::new();
    let mut dims_expr = Vec::new();
    let mut shape_eval_succeeded = false;
    let qualified_shape_expr = qualify_shape_subscripts_imports(tree, &comp.shape_expr, imports)?;
    let needs_late_recompute =
        !qualified_shape_expr.is_empty() && shape_expr_needs_late_recompute(&qualified_shape_expr);

    if !qualified_shape_expr.is_empty() {
        if let Some(evaluation) = eval_shape_expr_dims(
            &qualified_shape_expr,
            mod_env,
            effective_components,
            tree,
            imports.class_index,
        )? {
            if needs_late_recompute && !evaluation.translation_constant {
                // Defer symbolic dimensions to later phases that have full local
                // scope/modifier context (MLS §10.1 structural dimensions).
                // Do not trust parser/early fallback dimensions here, because they
                // may have dropped colon positions (e.g. `[:, 2]` -> `[2]`) or
                // captured values from an outer scope before local parameters
                // converged.
                dims_expr = qualified_shape_expr.clone();
            } else {
                dims = evaluation.dims;
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

    Ok((dims, dims_expr))
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

struct EvaluatedShape {
    dims: Vec<i64>,
    translation_constant: bool,
}

fn eval_shape_expr_dims(
    shape_expr: &[ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> InstantiateResult<Option<EvaluatedShape>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    let mut translation_constant = true;
    for sub in shape_expr {
        let ast::Subscript::Expression(expr) = sub else {
            return Ok(None);
        };
        // MLS §10.1: structural dimension expressions may use compile-time `if`
        // branches over parameter/constant conditions.
        let Some(evaluation) = try_eval_integer_shape_expr_with_proof(
            expr,
            mod_env,
            effective_components,
            tree,
            class_index,
            resolve_effective_components_for_eval,
        ) else {
            return Ok(None);
        };
        let dim = evaluation.value();
        if dim < 0 {
            return Ok(None);
        }
        translation_constant &= evaluation.is_translation_constant();
        dims.push(dim);
    }
    Ok(Some(EvaluatedShape {
        dims,
        translation_constant,
    }))
}

pub(super) fn qualify_shape_subscripts_imports(
    tree: &ast::ClassTree,
    shape_expr: &[ast::Subscript],
    imports: ImportRewrite<'_>,
) -> InstantiateResult<Vec<ast::Subscript>> {
    shape_expr
        .iter()
        .map(|subscript| {
            Ok(match subscript {
                ast::Subscript::Expression(expr) => {
                    ast::Subscript::Expression(qualify_shape_expr_imports_with_index(
                        tree,
                        imports.class_index,
                        expr,
                        imports,
                    )?)
                }
                ast::Subscript::Range { token } => ast::Subscript::Range {
                    token: token.clone(),
                },
                ast::Subscript::Empty => ast::Subscript::Empty,
            })
        })
        .collect()
}

/// True when [`qualify_shape_expr_imports`] would rewrite something in `expr`.
///
/// Rewriting builds a fresh expression tree, so callers that only qualify as a
/// retry ask this first and skip the retry entirely when no name in the
/// expression is an import alias.
pub(super) fn expr_mentions_import_alias(
    expr: &ast::Expression,
    imports: ImportRewrite<'_>,
) -> bool {
    let names_alias = |cref: &ast::ComponentReference| {
        cref.parts
            .first()
            .is_some_and(|first| imports.names_alias(first.ident.text.as_ref()))
    };
    let recurse = |expr| expr_mentions_import_alias(expr, imports);
    match expr {
        ast::Expression::ComponentReference(cref) => names_alias(cref),
        ast::Expression::Range {
            start, step, end, ..
        } => recurse(start) || step.as_ref().is_some_and(|expr| recurse(expr)) || recurse(end),
        ast::Expression::Unary { rhs, .. } => recurse(rhs),
        ast::Expression::Binary { lhs, rhs, .. } => recurse(lhs) || recurse(rhs),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .any(|(cond, body)| recurse(cond) || recurse(body))
                || recurse(else_branch)
        }
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::FunctionCall { comp, args, .. } => {
            names_alias(comp) || args.iter().any(recurse)
        }
        ast::Expression::DerivativeCall { args, .. } => args.iter().any(recurse),
        _ => false,
    }
}

pub(super) fn qualify_shape_expr_imports(
    tree: &ast::ClassTree,
    expr: &ast::Expression,
    imports: ImportRewrite<'_>,
) -> InstantiateResult<ast::Expression> {
    qualify_shape_expr_imports_with_index(tree, imports.class_index, expr, imports)
}

fn qualify_shape_expr_imports_with_index(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    expr: &ast::Expression,
    imports: ImportRewrite<'_>,
) -> InstantiateResult<ast::Expression> {
    ShapeImportQualifier {
        tree,
        class_index,
        imports,
    }
    .qualify(expr)
}

#[derive(Clone, Copy)]
struct ShapeImportQualifier<'a> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    imports: ImportRewrite<'a>,
}

impl ShapeImportQualifier<'_> {
    fn qualify(self, expr: &ast::Expression) -> InstantiateResult<ast::Expression> {
        Ok(match expr {
            ast::Expression::ComponentReference(cref) => ast::Expression::ComponentReference(
                qualify_component_ref_imports(self.tree, self.class_index, cref, self.imports)?,
            ),
            ast::Expression::Range {
                start,
                step,
                end,
                span,
            } => ast::Expression::Range {
                start: Arc::new(self.qualify(start)?),
                step: step
                    .as_ref()
                    .map(|expr| -> InstantiateResult<_> { Ok(Arc::new(self.qualify(expr)?)) })
                    .transpose()?,
                end: Arc::new(self.qualify(end)?),
                span: *span,
            },
            ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
                op: op.clone(),
                rhs: Arc::new(self.qualify(rhs)?),
                span: *span,
            },
            ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
                op: op.clone(),
                lhs: Arc::new(self.qualify(lhs)?),
                rhs: Arc::new(self.qualify(rhs)?),
                span: *span,
            },
            ast::Expression::If {
                branches,
                else_branch,
                span,
            } => ast::Expression::If {
                branches: branches
                    .iter()
                    .map(|(cond, body)| Ok((self.qualify(cond)?, self.qualify(body)?)))
                    .collect::<InstantiateResult<Vec<_>>>()?,
                else_branch: Arc::new(self.qualify(else_branch)?),
                span: *span,
            },
            ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
                inner: Arc::new(self.qualify(inner)?),
                span: *span,
            },
            ast::Expression::FunctionCall {
                comp,
                args,
                is_partial_application,
                span,
            } => ast::Expression::FunctionCall {
                comp: qualify_component_ref_imports(
                    self.tree,
                    self.class_index,
                    comp,
                    self.imports,
                )?,
                args: args
                    .iter()
                    .map(|arg| self.qualify(arg))
                    .collect::<InstantiateResult<Vec<_>>>()?,
                is_partial_application: *is_partial_application,
                span: *span,
            },
            ast::Expression::DerivativeCall { args, span } => ast::Expression::DerivativeCall {
                args: args
                    .iter()
                    .map(|arg| self.qualify(arg))
                    .collect::<InstantiateResult<Vec<_>>>()?,
                span: *span,
            },
            _ => expr.clone(),
        })
    }
}

/// Expand an alias root into the segments of its target path.
///
/// Import aliases are decided by the lookup authority's effective bindings
/// for the expression's declaring scope: the binding carries an identity for
/// every segment, so the rewritten reference is rebuilt from identities and
/// never re-derives one from a rendered spelling (SPEC_0036). A name the
/// authority refused as ambiguous is a typed error, never a silent pass.
/// Package-constant aliases keep their rendered-pair mechanism because they
/// alias package members, not imports.
fn qualify_component_ref_imports(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    cref: &ast::ComponentReference,
    imports: ImportRewrite<'_>,
) -> InstantiateResult<ast::ComponentReference> {
    let Some(first) = cref.parts.first() else {
        return Ok(cref.clone());
    };
    let alias = first.ident.text.as_ref();

    if let Some((_, target)) = imports
        .overriding_aliases
        .iter()
        .find(|(candidate, _)| candidate == alias)
    {
        return Ok(qualify_reference_with_alias_target(
            tree, cref, first, target,
        ));
    }

    if let Some(effective) = imports.effective {
        match effective.get(&rumoca_core::ComponentPath::from_flat_path(alias)) {
            Some(ast::EffectiveImport::Bound(binding)) => {
                return qualify_reference_with_import_binding(class_index, cref, first, binding);
            }
            Some(ast::EffectiveImport::Refused(refusal)) => {
                let reason = match refusal {
                    ast::ImportRefusal::AmbiguousUnqualifiedImport => {
                        "found in more than one package through unqualified imports (MLS §5.3.1)"
                    }
                    ast::ImportRefusal::AmbiguousInherited => {
                        "ambiguous among inherited declarations (MLS §5.3.1)"
                    }
                };
                return Err(Box::new(InstantiateError::ambiguous_imported_name(
                    alias.to_string(),
                    reason.to_string(),
                    cref.span,
                )));
            }
            None => {}
        }
    }

    if let Some((_, target)) = imports
        .fallback_aliases
        .iter()
        .find(|(candidate, _)| candidate == alias)
    {
        return Ok(qualify_reference_with_alias_target(
            tree, cref, first, target,
        ));
    }

    Ok(cref.clone())
}

/// Rebuild a reference from an import binding's per-segment identities.
///
/// Every segment's spelling is read structurally from that declaration's own
/// name (a class's name token, or a member declaration inside the preceding
/// package segment); the identity itself travels unchanged and no segment is
/// ever recovered from a rendered path.
fn qualify_reference_with_import_binding(
    class_index: &ast::ClassDefIndex<'_>,
    cref: &ast::ComponentReference,
    first: &ast::ComponentRefPart,
    binding: &ast::ImportBinding,
) -> InstantiateResult<ast::ComponentReference> {
    let mut parts = Vec::new();
    for def_id in binding.segments() {
        // `def_id` is the exact effective target selected by Resolve. In
        // particular, an inherited component belongs to the base declaration,
        // not to the package named by the preceding prefix segment. Read the
        // declaration spelling from that identity; do not retry lookup against
        // the derived package or linearize its extends graph here.
        let segment = class_index.local_name(def_id);
        let Some(segment) = segment else {
            return Err(Box::new(InstantiateError::missing_resolved_identity(
                format!("import binding segment {def_id:?}"),
                cref.span,
            )));
        };
        parts.push(ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(segment),
                ..rumoca_core::Token::default()
            },
            subs: None,
            def_id: Some(def_id),
        });
    }
    let Some(last) = parts.last_mut() else {
        return Err(Box::new(InstantiateError::missing_resolved_identity(
            "import binding without a target segment".to_string(),
            cref.span,
        )));
    };
    last.subs = first.subs.clone();
    parts.extend(cref.parts.iter().skip(1).cloned());

    Ok(ast::ComponentReference {
        local: cref.local,
        parts,
        span: cref.span,
        qualified_display_name: cref.qualified_display_name.clone(),
    })
}

/// Rebuild a reference from a package-constant alias target.
///
/// The pair's target spelling was rendered from the aliased declaration's
/// identity when the alias set was built, so looking each prefix back up
/// round-trips the same identity.
fn qualify_reference_with_alias_target(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    first: &ast::ComponentRefPart,
    target: &str,
) -> ast::ComponentReference {
    let mut qualified_prefix = String::new();
    let mut parts = rumoca_core::ComponentPath::from_flat_path(target)
        .into_parts()
        .into_iter()
        .map(|segment| {
            if !qualified_prefix.is_empty() {
                qualified_prefix.push('.');
            }
            qualified_prefix.push_str(segment.as_str());
            ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from(segment.as_str()),
                    ..rumoca_core::Token::default()
                },
                subs: None,
                def_id: tree.get_def_id_by_name(&qualified_prefix),
            }
        })
        .collect::<Vec<_>>();
    if let Some(last) = parts.last_mut() {
        last.subs = first.subs.clone();
        last.def_id = first.def_id.or(last.def_id);
    }
    parts.extend(cref.parts.iter().skip(1).cloned());

    ast::ComponentReference {
        local: cref.local,
        parts,
        span: cref.span,
        qualified_display_name: cref.qualified_display_name.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ImportRewrite, qualify_shape_subscripts_imports, resolve_component_dimensions,
        resolve_effective_components_for_eval, resolve_type_alias_dimensions,
    };
    use rumoca_eval_ast::eval_instantiate::try_eval_integer_shape_expr_with_index;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::AstIndexMap as IndexMap;
    use rumoca_phase_parse::parse_to_ast;
    use rumoca_phase_resolve::resolve;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("dims_test.mo"),
            1,
            2,
        )
    }

    fn make_name(text: &str) -> ast::Name {
        ast::Name {
            name: vec![make_token(text)],
            def_id: None,
        }
    }

    fn make_resolved_name(text: &str, def_id: rumoca_core::DefId) -> ast::Name {
        let mut name = make_name(text);
        name.def_id = Some(def_id);
        name
    }

    fn insert_class(
        tree: &mut ast::ClassTree,
        name: &str,
        def_id: rumoca_core::DefId,
        mut class: ast::ClassDef,
    ) {
        class.def_id = Some(def_id);
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
        tree.definitions.classes.insert(name.to_string(), class);
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
                parts: rumoca_core::ComponentPath::from_flat_path(path)
                    .into_parts()
                    .into_iter()
                    .map(|part| ast::ComponentRefPart {
                        ident: make_token(&part),
                        subs: None,
                        def_id: None,
                    })
                    .collect(),
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
        ))
    }

    fn assert_imported_dimension(
        tree: &ast::ClassTree,
        model_name: &str,
        exposure_name: &str,
        base_target: rumoca_core::DefId,
        expected: i64,
    ) {
        let class_index = ast::ClassDefIndex::from_tree(tree);
        let model = tree
            .get_class_by_qualified_name(model_name)
            .expect("model exists");
        let effective = tree.effective_imports(model.scope_id.expect("model scope is resolved"));
        let binding = match effective.get(&rumoca_core::ComponentPath::from_flat_path("N")) {
            Some(ast::EffectiveImport::Bound(binding)) => binding,
            other => panic!("expected bound inherited import, got {other:?}"),
        };
        assert_eq!(binding.target(), base_target);

        let component = model.components.get("x").expect("x exists");
        let imports = ImportRewrite {
            class_index: &class_index,
            overriding_aliases: &[],
            effective: Some(&effective),
            fallback_aliases: &[],
        };
        let qualified = qualify_shape_subscripts_imports(tree, &component.shape_expr, imports)
            .expect("qualification consumes the issued binding");
        let ast::Subscript::Expression(ast::Expression::ComponentReference(reference)) =
            &qualified[0]
        else {
            panic!("expected qualified component reference: {qualified:?}");
        };
        assert_eq!(
            reference
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>(),
            ["P", exposure_name, "'n.x'"],
            "quoted declaration spelling must come from the structural DefId index"
        );
        assert_eq!(
            reference.parts.last().and_then(|part| part.def_id),
            Some(base_target),
            "Instantiate must retain the inherited declaration identity"
        );
        assert!(
            component.shape.is_empty(),
            "the fixture must not carry a numeric parser fallback"
        );

        let effective_components = resolve_effective_components_for_eval(tree, model);
        let ast::Subscript::Expression(qualified_dimension) = &qualified[0] else {
            panic!("expected expression dimension: {qualified:?}");
        };
        assert_eq!(
            try_eval_integer_shape_expr_with_index(
                qualified_dimension,
                &ast::ModificationEnvironment::default(),
                &effective_components,
                tree,
                &class_index,
                resolve_effective_components_for_eval,
            ),
            Some(expected),
            "dimension evaluation must consume the effective imported occurrence"
        );
        let (dimensions, deferred) = resolve_component_dimensions(
            component,
            &[],
            &ast::ModificationEnvironment::default(),
            &effective_components,
            tree,
            imports,
        )
        .expect("quoted inherited import dimension resolves");
        assert_eq!(dimensions, vec![expected]);
        assert!(
            deferred.is_empty(),
            "a constructor-proved translation constant must not be rechecked downstream"
        );
    }

    #[test]
    fn renamed_inherited_import_preserves_quoted_name_identity_and_dimension() {
        let source = r#"
            package P
              package Base
                constant Integer m = 2;
                constant Integer 'n.x' = m + 1;
              end Base;
              package DerivedSibling extends Base(m = 4); end DerivedSibling;
              package DerivedDirect extends Base('n.x' = 7); end DerivedDirect;
              model Sibling
                import N = P.DerivedSibling.'n.x';
                Real x[N];
              end Sibling;
              model Direct
                import N = P.DerivedDirect.'n.x';
                Real x[N];
              end Direct;
            end P;
        "#;
        let stored = parse_to_ast(source, "<inherited_import_identity>").expect("fixture parses");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add("<inherited_import_identity>", source);
        let tree = resolve(ast::ParsedTree::new(tree))
            .expect("fixture resolves")
            .inner()
            .clone();
        let base_target = tree
            .get_class_by_qualified_name("P.Base")
            .and_then(|class| class.components.get("'n.x'"))
            .and_then(|component| component.def_id)
            .expect("base member has resolved identity");
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let target_declaration = class_index
            .component(base_target)
            .expect("target identity indexes its declaration");
        assert!(matches!(
            target_declaration.variability,
            rumoca_core::Variability::Constant(_)
        ));
        assert!(
            target_declaration.binding.is_some(),
            "the target constant must retain its declaration binding"
        );
        assert_eq!(
            class_index
                .parent_def_id(base_target)
                .and_then(|owner| class_index.get(owner))
                .map(|owner| owner.name.text.as_ref()),
            Some("Base")
        );
        assert_imported_dimension(&tree, "P.Sibling", "DerivedSibling", base_target, 5);
        assert_imported_dimension(&tree, "P.Direct", "DerivedDirect", base_target, 7);
    }

    #[test]
    fn test_resolve_component_dimensions_appends_type_alias_dims() {
        let comp = ast::Component {
            shape: vec![2],
            shape_expr: vec![make_dim_subscript(2)],
            ..ast::Component::empty_with_span(test_span())
        };
        let tree = ast::ClassTree::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[4],
            &ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &tree,
            super::ImportRewrite::without_imports(&class_index),
        )
        .expect("dimensions resolve");
        assert_eq!(dims, vec![2, 4]);
        assert!(dims_expr.is_empty());
    }

    #[test]
    fn test_resolve_component_dimensions_evaluates_shape_expr_without_shape_fallback() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_dim_subscript(2)],
            ..ast::Component::empty_with_span(test_span())
        };
        let tree = ast::ClassTree::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &tree,
            super::ImportRewrite::without_imports(&class_index),
        )
        .expect("dimensions resolve");
        assert_eq!(dims, vec![2]);
        assert!(dims_expr.is_empty());
    }

    #[test]
    fn test_resolve_component_dimensions_preserves_unresolved_shape_expr_without_shape_fallback() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_cref_subscript("Medium.nC")],
            ..ast::Component::empty_with_span(test_span())
        };
        let tree = ast::ClassTree::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &tree,
            super::ImportRewrite::without_imports(&class_index),
        )
        .expect("dimensions resolve");
        assert!(dims.is_empty());
        assert_eq!(dims_expr.len(), 1);
    }

    #[test]
    fn test_resolve_component_dimensions_keeps_symbolic_shape_expr_after_early_eval() {
        let comp = ast::Component {
            shape: vec![],
            shape_expr: vec![make_cref_subscript("nout")],
            ..ast::Component::empty_with_span(test_span())
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
                ..ast::Component::empty_with_span(test_span())
            },
        );
        let tree = ast::ClassTree::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &tree,
            super::ImportRewrite::without_imports(&class_index),
        )
        .expect("dimensions resolve");
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
            ..ast::Component::empty_with_span(test_span())
        };
        let tree = ast::ClassTree::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let (dims, dims_expr) = resolve_component_dimensions(
            &comp,
            &[],
            &ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &tree,
            super::ImportRewrite::without_imports(&class_index),
        )
        .expect("dimensions resolve");
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
        let real_id = rumoca_core::DefId::new(90);
        let quaternion_id = rumoca_core::DefId::new(91);
        let orientation_id = rumoca_core::DefId::new(92);
        tree.scope_tree
            .add_predefined_member(rumoca_core::ComponentPath::from_flat_path("Real"), real_id);

        let quaternion_base = ast::ClassDef {
            name: make_token("QuaternionBase"),
            class_type: rumoca_core::ClassType::Type,
            extends: vec![ast::Extend {
                base_name: make_resolved_name("Real", real_id),
                base_def_id: Some(real_id),
                ..Default::default()
            }],
            array_subscripts: vec![make_dim_subscript(4)],
            ..Default::default()
        };

        let orientation = ast::ClassDef {
            name: make_token("Orientation"),
            class_type: rumoca_core::ClassType::Type,
            extends: vec![ast::Extend {
                base_name: make_resolved_name("QuaternionBase", quaternion_id),
                base_def_id: Some(quaternion_id),
                ..Default::default()
            }],
            ..Default::default()
        };

        insert_class(&mut tree, "QuaternionBase", quaternion_id, quaternion_base);
        insert_class(&mut tree, "Orientation", orientation_id, orientation);

        let class_def = tree.definitions.classes.get("Orientation");
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let dims = resolve_type_alias_dimensions(
            &tree,
            class_def,
            &ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &class_index,
        )
        .expect("type alias dimensions should resolve");
        assert_eq!(dims, vec![4]);
    }

    #[test]
    fn type_alias_dimensions_follow_more_than_the_old_limit() {
        let mut tree = ast::ClassTree::default();
        let real_id = rumoca_core::DefId::new(100);
        tree.scope_tree
            .add_predefined_member(rumoca_core::ComponentPath::from_flat_path("Real"), real_id);
        for index in 0..24_u32 {
            let def_id = rumoca_core::DefId::new(101 + index);
            let (base_name, base_def_id) = if index == 0 {
                ("Real".to_string(), real_id)
            } else {
                (
                    format!("Alias{}", index - 1),
                    rumoca_core::DefId::new(100 + index),
                )
            };
            let class = ast::ClassDef {
                name: make_token(&format!("Alias{index}")),
                class_type: rumoca_core::ClassType::Type,
                extends: vec![ast::Extend {
                    base_name: make_resolved_name(&base_name, base_def_id),
                    base_def_id: Some(base_def_id),
                    ..Default::default()
                }],
                array_subscripts: (index == 0)
                    .then(|| make_dim_subscript(4))
                    .into_iter()
                    .collect(),
                ..Default::default()
            };
            insert_class(&mut tree, &format!("Alias{index}"), def_id, class);
        }
        let root = tree.get_class_by_def_id(rumoca_core::DefId::new(124));
        let dimensions = super::collect_type_alias_subscripts(&tree, root)
            .expect("long acyclic alias graph is valid");
        assert_eq!(dimensions, vec![make_dim_subscript(4)]);
    }

    #[test]
    fn type_alias_dimensions_reject_cycles_and_missing_edges() {
        let mut tree = ast::ClassTree::default();
        let a_id = rumoca_core::DefId::new(201);
        let b_id = rumoca_core::DefId::new(202);
        for (name, def_id, base_name, base_id) in [("A", a_id, "B", b_id), ("B", b_id, "A", a_id)] {
            insert_class(
                &mut tree,
                name,
                def_id,
                ast::ClassDef {
                    name: make_token(name),
                    extends: vec![ast::Extend {
                        base_name: make_resolved_name(base_name, base_id),
                        base_def_id: Some(base_id),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
            );
        }
        let a = tree.get_class_by_def_id(a_id);
        assert!(super::collect_type_alias_subscripts(&tree, a).is_err());

        let missing_id = rumoca_core::DefId::new(299);
        let broken_id = rumoca_core::DefId::new(203);
        insert_class(
            &mut tree,
            "Broken",
            broken_id,
            ast::ClassDef {
                name: make_token("Broken"),
                extends: vec![ast::Extend {
                    base_name: make_resolved_name("Missing", missing_id),
                    base_def_id: Some(missing_id),
                    ..Default::default()
                }],
                ..Default::default()
            },
        );
        let broken = tree.get_class_by_def_id(broken_id);
        assert!(super::collect_type_alias_subscripts(&tree, broken).is_err());
    }

    #[test]
    fn builtin_spelling_does_not_override_resolved_user_identity() {
        let mut tree = ast::ClassTree::default();
        let predefined_real = rumoca_core::DefId::new(300);
        let user_real = rumoca_core::DefId::new(301);
        let alias = rumoca_core::DefId::new(302);
        let package = rumoca_core::DefId::new(303);
        tree.scope_tree.add_predefined_member(
            rumoca_core::ComponentPath::from_flat_path("Real"),
            predefined_real,
        );
        let user_real_class = ast::ClassDef {
            def_id: Some(user_real),
            name: make_token("Real"),
            extends: vec![ast::Extend {
                base_name: make_resolved_name("Real", predefined_real),
                base_def_id: Some(predefined_real),
                ..Default::default()
            }],
            array_subscripts: vec![make_dim_subscript(7)],
            ..Default::default()
        };
        let mut package_class = ast::ClassDef {
            name: make_token("P"),
            ..Default::default()
        };
        package_class
            .classes
            .insert("Real".to_string(), user_real_class);
        insert_class(&mut tree, "P", package, package_class);
        tree.name_map.insert("P.Real".to_string(), user_real);
        tree.def_map.insert(user_real, "P.Real".to_string());
        insert_class(
            &mut tree,
            "Alias",
            alias,
            ast::ClassDef {
                name: make_token("Alias"),
                extends: vec![ast::Extend {
                    base_name: make_resolved_name("Real", user_real),
                    base_def_id: Some(user_real),
                    ..Default::default()
                }],
                ..Default::default()
            },
        );
        let alias = tree.get_class_by_def_id(alias);
        let dimensions = super::collect_type_alias_subscripts(&tree, alias)
            .expect("resolved user identity is followed despite builtin spelling");
        assert_eq!(dimensions, vec![make_dim_subscript(7)]);
    }
}
