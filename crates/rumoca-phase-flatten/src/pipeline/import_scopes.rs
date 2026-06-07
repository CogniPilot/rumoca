use super::*;

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
    let mut imports: crate::qualify::ImportMap =
        class_data.resolved_imports.iter().cloned().collect();
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
    collect_imports_for_scope_id(tree, source_scope_id, &mut imports)?;
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
    ctx.current_imports = imports;
    Ok(())
}

fn collect_imports_for_scope_id(
    tree: &ClassTree,
    source_scope_id: rumoca_core::ScopeId,
    imports: &mut crate::qualify::ImportMap,
) -> Result<(), FlattenError> {
    let mut scope_ids = Vec::new();
    let mut current = Some(source_scope_id);
    while let Some(scope_id) = current {
        let Some(scope) = tree.scope_tree.get(scope_id) else {
            return Err(FlattenError::internal(format!(
                "source scope id {scope_id} is not present in the scope tree"
            )));
        };
        scope_ids.push(scope_id);
        current = if scope.is_encapsulated() && !scope_id.is_global() {
            Some(rumoca_core::ScopeId::GLOBAL)
        } else {
            scope.parent
        };
    }

    for scope_id in scope_ids.into_iter().rev() {
        let scope = tree.scope_tree.get(scope_id).ok_or_else(|| {
            FlattenError::internal(format!(
                "source scope id {scope_id} is not present in the scope tree"
            ))
        })?;
        for import in &scope.imports {
            insert_scope_import(tree, import, imports)?;
        }
    }
    Ok(())
}

fn insert_scope_import(
    tree: &ClassTree,
    import: &rumoca_ir_ast::scope::Import,
    imports: &mut crate::qualify::ImportMap,
) -> Result<(), FlattenError> {
    match import {
        rumoca_ir_ast::scope::Import::Qualified { path, .. } => {
            let Some(alias) = path.last() else {
                return Ok(());
            };
            imports.insert(alias.clone(), path.join("."));
        }
        rumoca_ir_ast::scope::Import::Renamed { alias, path, .. } => {
            imports.insert(alias.to_flat_string(), path.join("."));
        }
        rumoca_ir_ast::scope::Import::Unqualified { names, .. } => {
            for (name, def_id) in names {
                let target = tree.def_map.get(def_id).ok_or_else(|| {
                    FlattenError::internal(format!(
                        "scope import target {def_id} is missing from the definition map"
                    ))
                })?;
                imports.insert(name.to_flat_string(), target.clone());
            }
        }
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
