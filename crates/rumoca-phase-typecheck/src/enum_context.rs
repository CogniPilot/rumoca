use rumoca_core::ScopeId;
use rumoca_ir_ast::{ClassDefIndex, ClassTree, ScopeImport};

use crate::{TypeCheckError, TypeCheckResult, TypeChecker};

impl TypeChecker {
    /// Collect enumeration type sizes from the class tree (MLS §10.5).
    ///
    /// Scans the class tree for enumeration type definitions and populates
    /// the eval context's `enum_sizes` map with type name -> literal count mappings.
    /// Also resolves import aliases so that `import L = ...Logic` makes `L` usable
    /// as a dimension.
    pub(crate) fn collect_enum_sizes(&mut self, tree: &ClassTree) -> TypeCheckResult<()> {
        let class_index = ClassDefIndex::from_tree(tree);
        let mut class_entries = Vec::new();
        for def_id in class_index.def_ids() {
            let Some(class_def) = class_index.get(def_id) else {
                return Err(Box::new(TypeCheckError::missing_source_context(format!(
                    "class index DefId {def_id:?} did not resolve to a class definition"
                ))));
            };
            let Some(name) = class_index.qualified_name(def_id) else {
                return Err(Box::new(TypeCheckError::phase_diagnostic(
                    "ET001",
                    format!("missing resolved class name metadata for DefId {def_id:?}"),
                    "name resolution must preserve class DefId name metadata for enum dimensions",
                    self.location_span(&class_def.name.location)?,
                )));
            };
            class_entries.push((name.to_string(), def_id));
        }
        class_entries.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));

        for (name, def_id) in class_entries {
            let Some(class_def) = class_index.get(def_id) else {
                return Err(Box::new(TypeCheckError::missing_source_context(format!(
                    "qualified class entry {name} with DefId {def_id:?} did not resolve to a class definition"
                ))));
            };
            let size = class_def.enum_literals.len();
            if size == 0 {
                continue;
            }
            self.eval_ctx.enum_sizes.insert(name.clone(), size);
            let Some(short) = class_index.local_name(def_id) else {
                return Err(Box::new(TypeCheckError::phase_diagnostic(
                    "ET002",
                    format!("missing local class name metadata for DefId {def_id:?}"),
                    "name resolution must preserve class DefId local-name metadata for enum dimensions",
                    self.location_span(&class_def.name.location)?,
                )));
            };
            self.eval_ctx
                .enum_sizes
                .entry(short.to_string())
                .or_insert(size);
            Self::collect_enum_ordinals(&class_index, def_id, &name, &mut self.eval_ctx);
        }

        for idx in 0..tree.scope_tree.len() {
            let scope_id = ScopeId::new(idx as u32);
            let Some(scope) = tree.scope_tree.get(scope_id) else {
                continue;
            };
            for import in &scope.imports {
                Self::collect_enum_from_import(&class_index, import, &mut self.eval_ctx);
            }
        }
        Ok(())
    }

    fn enum_literal_count(
        class_index: &ClassDefIndex<'_>,
        def_id: rumoca_core::DefId,
    ) -> Option<usize> {
        class_index
            .get(def_id)
            .map(|class| class.enum_literals.len())
    }

    fn collect_enum_ordinals(
        class_index: &ClassDefIndex<'_>,
        def_id: rumoca_core::DefId,
        type_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let class = class_index
            .get(def_id)
            .expect("enum ordinal collection requires an indexed enum class definition");
        for (i, literal) in class.enum_literals.iter().enumerate() {
            let ordinal = (i + 1) as i64;
            let lit_name = &*literal.ident.text;
            let full = format!("{}.{}", type_name, lit_name);
            ctx.enum_ordinals.insert(full, ordinal);
            ctx.enum_ordinals
                .entry(lit_name.to_string())
                .or_insert(ordinal);
        }
    }

    fn collect_enum_from_import(
        class_index: &ClassDefIndex<'_>,
        import: &ScopeImport,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let pairs: Vec<(String, rumoca_core::DefId)> = match import {
            ScopeImport::Renamed { .. } | ScopeImport::Qualified { .. } => {
                Self::import_constant_prefixes(import)
            }
            ScopeImport::Unqualified { names, .. } => names
                .iter()
                .map(|(name, &def_id)| (name.as_str().to_string(), def_id))
                .collect(),
        };
        for (name, def_id) in pairs {
            if let Some(size) = Self::enum_literal_count(class_index, def_id)
                && size > 0
            {
                ctx.enum_sizes.entry(name.clone()).or_insert(size);
                Self::collect_enum_ordinals(class_index, def_id, &name, ctx);
            }
        }
    }
}
