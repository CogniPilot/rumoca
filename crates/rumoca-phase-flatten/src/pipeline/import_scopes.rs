use super::*;

/// Names the lookup authority refused to bind through imports (MLS §5.3.1),
/// keyed by rendered short name.
pub(crate) type ImportRefusalMap = rustc_hash::FxHashMap<String, rumoca_ir_ast::ImportRefusal>;

pub(crate) struct ImportScope<'a> {
    pub(crate) source_scope: Option<&'a QualifiedName>,
    pub(crate) source_scope_id: Option<rumoca_core::ScopeId>,
    pub(crate) span: rumoca_core::Span,
}

pub(crate) fn set_class_instance_imports_for_statement_block(
    ctx: &mut Context,
    class_data: &ClassInstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    statements: &[InstanceStatement],
    override_packages: &[String],
    override_aliases: &[(String, String)],
) -> Result<(), FlattenError> {
    let Some(statement) = statements.first() else {
        return Ok(());
    };
    set_class_instance_imports_for_scope(
        ctx,
        class_data,
        tree,
        class_index,
        ImportScope {
            source_scope: statement.source_scope.as_ref(),
            source_scope_id: statement.source_scope_id,
            span: statement.span,
        },
        override_packages,
        override_aliases,
    )
}

pub(crate) fn set_class_instance_imports_for_scope(
    ctx: &mut Context,
    class_data: &ClassInstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    import_scope: ImportScope<'_>,
    override_packages: &[String],
    override_aliases: &[(String, String)],
) -> Result<(), FlattenError> {
    let source_scope = import_scope.source_scope.ok_or_else(|| {
        missing_class_instance_source_scope_error(class_data, import_scope.span, "imports")
    })?;
    let source_scope_id = import_scope.source_scope_id.ok_or_else(|| {
        missing_class_instance_source_scope_error(class_data, import_scope.span, "imports")
    })?;
    if tree.scope_tree.get(source_scope_id).is_none() {
        return Err(missing_class_instance_source_scope_error(
            class_data,
            import_scope.span,
            "imports",
        ));
    }
    // MLS §13.2: the import view of the equation's own recorded scope, as
    // decided by the one lookup authority. Imports are never inherited, so
    // no extends-chain union of import clauses exists here.
    let mut imports = crate::qualify::ImportMap::default();
    let mut refusals = ImportRefusalMap::default();
    seed_effective_imports(tree, source_scope_id, &mut imports, &mut refusals)?;
    add_package_override_aliases(class_index, override_aliases, &mut imports);
    crate::qualify::collect_lexical_package_aliases(
        tree,
        class_index,
        &source_scope.to_flat_string(),
        &mut imports,
    );
    crate::qualify::collect_lexical_constant_aliases_for_source_scope_with_packages(
        tree,
        class_index,
        source_scope,
        override_packages,
        &mut imports,
    );
    // A name a later channel deliberately binds (a redeclared package alias
    // or an enclosing-scope constant alias) is a declaration-backed binding,
    // not an import; it takes precedence over an import-tier refusal.
    refusals.retain(|name, _| !imports.contains_key(name));
    ctx.current_imports = imports;
    ctx.current_import_refusals = refusals;
    Ok(())
}

/// Seed `imports` and `refusals` from the lookup authority's effective import
/// view of `scope_id`.
///
/// Each bound name renders its target from the definition map, keyed by the
/// binding's declaration identity; a refused name is recorded so its use can
/// be rejected with a typed error instead of silently falling through.
pub(crate) fn seed_effective_imports(
    tree: &ClassTree,
    scope_id: rumoca_core::ScopeId,
    imports: &mut crate::qualify::ImportMap,
    refusals: &mut ImportRefusalMap,
) -> Result<(), FlattenError> {
    for (name, verdict) in tree.effective_imports(scope_id).iter() {
        let name = name.to_flat_string();
        match verdict {
            rumoca_ir_ast::EffectiveImport::Bound(binding) => {
                let target = binding.target();
                let rendered = tree.def_map.get(&target).ok_or_else(|| {
                    FlattenError::internal(format!(
                        "import binding target {target} is missing from the definition map"
                    ))
                })?;
                imports.insert(name, rendered.clone());
            }
            rumoca_ir_ast::EffectiveImport::Refused(refusal) => {
                refusals.insert(name, *refusal);
            }
        }
    }
    Ok(())
}

/// Reject any use of a name the lookup authority refused to bind (MLS §5.3.1).
///
/// A component-reference root that names a refused import, and is not bound
/// by an expression-local binder (for-index, comprehension index, statement
/// local), is a typed error: ambiguity is refused at the use site, never
/// silently dropped or rebound.
pub(crate) fn refuse_ambiguous_import_uses(
    expr: &ast::Expression,
    refusals: &ImportRefusalMap,
    locals: Option<&std::collections::HashSet<String>>,
) -> Result<(), FlattenError> {
    if refusals.is_empty() {
        return Ok(());
    }
    let mut binders: Vec<String> = locals
        .map(|locals| locals.iter().cloned().collect())
        .unwrap_or_default();
    check_expression_for_refused_imports(expr, refusals, &mut binders)
}

/// Reject any use of a refused name inside algorithm statements
/// (MLS §5.3.1), tracking for-index binders the way the expression walker
/// tracks comprehension indices: iteration ranges are evaluated outside the
/// binder scope, the loop body inside it.
pub(crate) fn refuse_ambiguous_import_uses_in_statements(
    statements: &[ast::Statement],
    refusals: &ImportRefusalMap,
    locals: &std::collections::HashSet<String>,
) -> Result<(), FlattenError> {
    if refusals.is_empty() {
        return Ok(());
    }
    let mut binders: Vec<String> = locals.iter().cloned().collect();
    for statement in statements {
        check_statement_for_refused_imports(statement, refusals, &mut binders)?;
    }
    Ok(())
}

fn check_statement_for_refused_imports(
    statement: &ast::Statement,
    refusals: &ImportRefusalMap,
    binders: &mut Vec<String>,
) -> Result<(), FlattenError> {
    match statement {
        ast::Statement::Empty | ast::Statement::Return { .. } | ast::Statement::Break { .. } => {
            Ok(())
        }
        ast::Statement::Assignment { comp, value } => {
            check_component_ref_for_refused_imports(comp, refusals, binders)?;
            check_expression_for_refused_imports(value, refusals, binders)
        }
        ast::Statement::For { indices, equations } => {
            for index in indices {
                check_expression_for_refused_imports(&index.range, refusals, binders)?;
            }
            let binder_base = binders.len();
            binders.extend(indices.iter().map(|index| index.ident.text.to_string()));
            for inner in equations {
                check_statement_for_refused_imports(inner, refusals, binders)?;
            }
            binders.truncate(binder_base);
            Ok(())
        }
        ast::Statement::While(block) => {
            check_statement_block_for_refused_imports(block, refusals, binders)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_statement_block_for_refused_imports(block, refusals, binders)?;
            }
            if let Some(else_block) = else_block {
                for inner in else_block {
                    check_statement_for_refused_imports(inner, refusals, binders)?;
                }
            }
            Ok(())
        }
        ast::Statement::When(blocks) => {
            for block in blocks {
                check_statement_block_for_refused_imports(block, refusals, binders)?;
            }
            Ok(())
        }
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            check_component_ref_for_refused_imports(comp, refusals, binders)?;
            for arg in args {
                check_expression_for_refused_imports(arg, refusals, binders)?;
            }
            for output in outputs {
                check_expression_for_refused_imports(output, refusals, binders)?;
            }
            Ok(())
        }
        ast::Statement::Reinit { variable, value } => {
            check_component_ref_for_refused_imports(variable, refusals, binders)?;
            check_expression_for_refused_imports(value, refusals, binders)
        }
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => {
            check_expression_for_refused_imports(condition, refusals, binders)?;
            check_expression_for_refused_imports(message, refusals, binders)?;
            if let Some(level) = level {
                check_expression_for_refused_imports(level, refusals, binders)?;
            }
            Ok(())
        }
    }
}

fn check_statement_block_for_refused_imports(
    block: &ast::StatementBlock,
    refusals: &ImportRefusalMap,
    binders: &mut Vec<String>,
) -> Result<(), FlattenError> {
    check_expression_for_refused_imports(&block.cond, refusals, binders)?;
    for statement in &block.stmts {
        check_statement_for_refused_imports(statement, refusals, binders)?;
    }
    Ok(())
}

fn import_refusal_reason(refusal: rumoca_ir_ast::ImportRefusal) -> &'static str {
    match refusal {
        rumoca_ir_ast::ImportRefusal::AmbiguousUnqualifiedImport => {
            "found in more than one package through unqualified imports (MLS §5.3.1)"
        }
        rumoca_ir_ast::ImportRefusal::AmbiguousInherited => {
            "ambiguous among inherited declarations (MLS §5.3.1)"
        }
    }
}

fn check_expression_for_refused_imports(
    expr: &ast::Expression,
    refusals: &ImportRefusalMap,
    binders: &mut Vec<String>,
) -> Result<(), FlattenError> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            check_component_ref_for_refused_imports(cr, refusals, binders)
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            check_expression_for_refused_imports(lhs, refusals, binders)?;
            check_expression_for_refused_imports(rhs, refusals, binders)
        }
        ast::Expression::Unary { rhs, .. } | ast::Expression::Parenthesized { inner: rhs, .. } => {
            check_expression_for_refused_imports(rhs, refusals, binders)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            check_component_ref_for_refused_imports(comp, refusals, binders)?;
            for arg in args {
                check_expression_for_refused_imports(arg, refusals, binders)?;
            }
            Ok(())
        }
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            check_component_ref_for_refused_imports(target, refusals, binders)?;
            for modification in modifications {
                check_expression_for_refused_imports(modification, refusals, binders)?;
            }
            Ok(())
        }
        ast::Expression::NamedArgument { value, .. } => {
            check_expression_for_refused_imports(value, refusals, binders)
        }
        ast::Expression::Modification { target, value, .. } => {
            check_component_ref_for_refused_imports(target, refusals, binders)?;
            match value {
                Some(value) => check_expression_for_refused_imports(value, refusals, binders),
                None => Ok(()),
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                check_expression_for_refused_imports(condition, refusals, binders)?;
                check_expression_for_refused_imports(value, refusals, binders)?;
            }
            check_expression_for_refused_imports(else_branch, refusals, binders)
        }
        ast::Expression::DerivativeCall { args: elements, .. }
        | ast::Expression::Array { elements, .. }
        | ast::Expression::Tuple { elements, .. } => {
            for element in elements {
                check_expression_for_refused_imports(element, refusals, binders)?;
            }
            Ok(())
        }
        ast::Expression::Range {
            start, step, end, ..
        } => {
            check_expression_for_refused_imports(start, refusals, binders)?;
            if let Some(step) = step {
                check_expression_for_refused_imports(step, refusals, binders)?;
            }
            check_expression_for_refused_imports(end, refusals, binders)
        }
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            // Iteration ranges are evaluated outside the binder scope; the
            // body and filter see the comprehension indices as locals.
            for index in indices {
                check_expression_for_refused_imports(&index.range, refusals, binders)?;
            }
            let binder_base = binders.len();
            binders.extend(indices.iter().map(|index| index.ident.text.to_string()));
            check_expression_for_refused_imports(expr, refusals, binders)?;
            if let Some(filter) = filter {
                check_expression_for_refused_imports(filter, refusals, binders)?;
            }
            binders.truncate(binder_base);
            Ok(())
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            check_expression_for_refused_imports(base, refusals, binders)?;
            for subscript in subscripts {
                check_subscript_for_refused_imports(subscript, refusals, binders)?;
            }
            Ok(())
        }
        ast::Expression::FieldAccess { base, .. } => {
            check_expression_for_refused_imports(base, refusals, binders)
        }
        ast::Expression::Terminal { .. } | ast::Expression::Empty { .. } => Ok(()),
    }
}

fn check_component_ref_for_refused_imports(
    cr: &ast::ComponentReference,
    refusals: &ImportRefusalMap,
    binders: &mut Vec<String>,
) -> Result<(), FlattenError> {
    for part in &cr.parts {
        if let Some(subscripts) = &part.subs {
            for subscript in subscripts {
                check_subscript_for_refused_imports(subscript, refusals, binders)?;
            }
        }
    }
    if cr.local {
        return Ok(());
    }
    let Some(first) = cr.parts.first() else {
        return Ok(());
    };
    let root = first.ident.text.as_ref();
    if binders.iter().any(|binder| binder == root) {
        return Ok(());
    }
    if let Some(refusal) = refusals.get(root) {
        return Err(FlattenError::ambiguous_imported_name(
            root,
            import_refusal_reason(*refusal),
            cr.span,
        ));
    }
    Ok(())
}

fn check_subscript_for_refused_imports(
    subscript: &ast::Subscript,
    refusals: &ImportRefusalMap,
    binders: &mut Vec<String>,
) -> Result<(), FlattenError> {
    if let ast::Subscript::Expression(expr) = subscript {
        check_expression_for_refused_imports(expr, refusals, binders)?;
    }
    Ok(())
}

fn missing_class_instance_source_scope_error(
    class_data: &ClassInstanceData,
    span: rumoca_core::Span,
    context: &str,
) -> FlattenError {
    FlattenError::missing_source_scope(class_data.qualified_name.to_flat_string(), context, span)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reference(name: &str, subs: Option<Vec<ast::Subscript>>) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: std::sync::Arc::from(name),
                    ..rumoca_core::Token::default()
                },
                subs,
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn name_expr(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(reference(name, None))
    }

    fn refusals_for(name: &str) -> ImportRefusalMap {
        let mut refusals = ImportRefusalMap::default();
        refusals.insert(
            name.to_string(),
            rumoca_ir_ast::ImportRefusal::AmbiguousUnqualifiedImport,
        );
        refusals
    }

    #[test]
    fn refused_root_use_is_a_typed_error() {
        let error = refuse_ambiguous_import_uses(&name_expr("X"), &refusals_for("X"), None)
            .expect_err("a refused import name in use position must error");
        assert!(
            matches!(error, FlattenError::AmbiguousImportedName { ref name, .. } if name == "X"),
            "expected AmbiguousImportedName for X, got {error:?}"
        );
    }

    #[test]
    fn refused_name_inside_a_subscript_is_a_typed_error() {
        let expr = ast::Expression::ComponentReference(reference(
            "a",
            Some(vec![ast::Subscript::Expression(name_expr("X"))]),
        ));
        let error = refuse_ambiguous_import_uses(&expr, &refusals_for("X"), None)
            .expect_err("a refused import name used as a subscript must error");
        assert!(
            matches!(error, FlattenError::AmbiguousImportedName { ref name, .. } if name == "X")
        );
    }

    #[test]
    fn expression_local_binder_shadows_the_refusal() {
        let locals = std::collections::HashSet::from(["X".to_string()]);
        refuse_ambiguous_import_uses(&name_expr("X"), &refusals_for("X"), Some(&locals))
            .expect("an expression-local binder shadows the ambiguous import (MLS §5.3.1)");
    }

    #[test]
    fn comprehension_binder_shadows_only_the_body() {
        let comprehension = ast::Expression::ArrayComprehension {
            expr: std::sync::Arc::new(name_expr("X")),
            indices: vec![ast::ForIndex {
                ident: rumoca_core::Token {
                    text: std::sync::Arc::from("X"),
                    ..rumoca_core::Token::default()
                },
                range: name_expr("n"),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };
        refuse_ambiguous_import_uses(&comprehension, &refusals_for("X"), None)
            .expect("the comprehension binder shadows the refused name in the body");
        let range_use = ast::Expression::ArrayComprehension {
            expr: std::sync::Arc::new(name_expr("v")),
            indices: vec![ast::ForIndex {
                ident: rumoca_core::Token {
                    text: std::sync::Arc::from("i"),
                    ..rumoca_core::Token::default()
                },
                range: name_expr("X"),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };
        refuse_ambiguous_import_uses(&range_use, &refusals_for("X"), None)
            .expect_err("an iteration range is outside the binder scope, so the use is refused");
    }
}
