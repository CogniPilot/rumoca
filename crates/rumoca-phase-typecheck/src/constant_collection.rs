//! Constant collection for compile-time dimension evaluation.
//!
//! Populates the [`TypeChecker`] evaluation context with constants reachable
//! through imports, extends chains, and redeclare overrides so dimension
//! expressions can be evaluated (MLS §10.1, §12.4).

use super::*;

impl TypeChecker {
    /// Collect integer/real/boolean constants from classes referenced by import aliases.
    ///
    /// When a class has `import generator = Modelica.Math.Random.Generators.Xorshift128plus`,
    /// this adds `generator.nState = 4` to the eval context so that dimension expressions
    /// like `Integer state[generator.nState]` can be evaluated.
    pub(crate) fn collect_import_constants(
        tree: &ClassTree,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for idx in 0..tree.scope_tree.len() {
            let scope_id = ScopeId::new(idx as u32);
            let Some(scope) = tree.scope_tree.get(scope_id) else {
                continue;
            };
            for import in &scope.imports {
                Self::collect_constants_from_import(tree, import, ctx);
            }
        }
    }

    /// Collect constants from direct model-level `extends(... redeclare ...)` overrides.
    ///
    /// MLS §7.3: redeclare modifiers in an extends clause define the effective
    /// replacement class/package in the derived class scope.
    pub(crate) fn collect_model_extends_redeclare_constants(
        tree: &ClassTree,
        model_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let model_class = tree
            .get_class_by_qualified_name(model_name)
            .or_else(|| tree.get_class_by_qualified_name(top_level_last_segment(model_name)));
        let Some(model_class) = model_class else {
            return;
        };

        let override_roots = Self::collect_redeclare_override_roots(tree, model_name, model_class);
        if override_roots.is_empty() {
            return;
        }

        // MLS §5.3 + §7.3: model-level redeclare package bindings are in the
        // local class scope and must take precedence over unrelated global
        // import aliases (e.g. other `import Medium = ...` entries in MSL).
        for (alias, _) in &override_roots {
            Self::clear_alias_scope_values(ctx, alias);
        }

        const MAX_PASSES: usize = 5;
        for _ in 0..MAX_PASSES {
            let prev =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            for (alias, def_id) in &override_roots {
                Self::extract_override_class_constants(tree, alias, *def_id, ctx);
            }
            let new =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            if new == prev {
                break;
            }
        }
    }

    pub(crate) fn clear_alias_scope_values(
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
        alias: &str,
    ) {
        let prefix = format!("{alias}.");
        ctx.integers.retain(|k, _| !k.starts_with(&prefix));
        ctx.reals.retain(|k, _| !k.starts_with(&prefix));
        ctx.booleans.retain(|k, _| !k.starts_with(&prefix));
        ctx.enums.retain(|k, _| !k.starts_with(&prefix));
        ctx.dimensions.retain(|k, _| !k.starts_with(&prefix));
        ctx.enum_sizes.retain(|k, _| !k.starts_with(&prefix));
        ctx.enum_ordinals.retain(|k, _| !k.starts_with(&prefix));
    }

    /// Collect `(alias, def_id)` pairs from direct model extends redeclare modifiers.
    fn collect_redeclare_override_roots(
        tree: &ClassTree,
        model_name: &str,
        model_class: &ClassDef,
    ) -> Vec<(String, DefId)> {
        let mut roots = Vec::new();
        let mut seen = std::collections::HashSet::<(String, DefId)>::new();

        for ext_mod in model_class
            .extends
            .iter()
            .flat_map(|ext| ext.modifications.iter())
        {
            let Some((alias, def_id)) =
                Self::extract_redeclare_override_root(tree, model_name, ext_mod)
            else {
                continue;
            };
            if seen.insert((alias.clone(), def_id)) {
                roots.push((alias, def_id));
            }
        }

        roots
    }

    /// Resolve one extends redeclare modifier to `(target alias, replacement def_id)`.
    fn extract_redeclare_override_root(
        tree: &ClassTree,
        model_name: &str,
        ext_mod: &rumoca_ir_ast::ExtendModification,
    ) -> Option<(String, DefId)> {
        if !ext_mod.redeclare {
            return None;
        }

        let Expression::Modification { target, value, .. } = &ext_mod.expr else {
            return None;
        };
        let alias = target.parts.first()?.ident.text.to_string();
        let def_id = Self::resolve_redeclare_target_def_id(tree, value, model_name)?;
        Some((alias, def_id))
    }

    /// Resolve the replacement class/package def id from a redeclare value expression.
    fn resolve_redeclare_target_def_id(
        tree: &ClassTree,
        value: &Expression,
        resolve_context: &str,
    ) -> Option<DefId> {
        let cref = match value {
            Expression::ComponentReference(cref) => cref,
            Expression::ClassModification { target, .. } => target,
            _ => return None,
        };

        let target_name = cref.to_string();
        // MLS §7.3: redeclare values may be multi-part class references
        // (e.g. `Modelica.Media.Incompressible.Examples.Essotherm650`).
        // Parser metadata can attach def_id to the first segment only, so
        // resolve the full path before falling back to cref.def_id.
        if let Some(def_id) = tree.name_map.get(&target_name).copied() {
            return Some(def_id);
        }
        if let Some(class) = tree.get_class_by_qualified_name(&target_name)
            && let Some(def_id) = class.def_id
        {
            return Some(def_id);
        }
        if let Some(def_id) = cref.def_id {
            return Some(def_id);
        }

        let (class, resolved_qname) =
            Self::resolve_class_name_with_qname(tree, &target_name, resolve_context);
        class.and_then(|c| {
            c.def_id
                .or_else(|| resolved_qname.and_then(|q| tree.name_map.get(&q).copied()))
        })
    }

    /// Extract constants for one resolved redeclare override root under its alias.
    pub(crate) fn extract_override_class_constants(
        tree: &ClassTree,
        alias: &str,
        def_id: DefId,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            return;
        };

        Self::extract_class_constants(alias, class, ctx);
        Self::extract_nested_class_constants_for_import(tree, alias, class, ctx);

        let resolve_context = tree
            .def_map
            .get(&def_id)
            .map(String::as_str)
            .unwrap_or(alias);
        for ext in &class.extends {
            Self::extract_extends_modification_constants(alias, ext, ctx);
            Self::extract_class_constants_from_extends(
                tree,
                alias,
                &ext.base_name.to_string(),
                resolve_context,
                ctx,
            );
        }
    }

    /// Extract constant values from a single import and add them to the eval context.
    ///
    /// Recursively extracts from nested classes and extends chains so that
    /// deeply nested subpackage constants are available for dimension evaluation.
    fn collect_constants_from_import(
        tree: &ClassTree,
        import: &ScopeImport,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let pairs: Vec<(String, rumoca_core::DefId)> = match import {
            ScopeImport::Renamed { .. } | ScopeImport::Qualified { .. } => {
                Self::import_constant_prefixes(import)
            }
            ScopeImport::Unqualified { .. } => return, // Too broad, skip
        };
        for (alias, def_id) in pairs {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };
            Self::extract_class_constants(&alias, class, ctx);
            // Also extract from nested classes (subpackages) with qualified prefixes
            Self::extract_nested_class_constants_for_import(tree, &alias, class, ctx);
            // Follow extends chains to get inherited constants
            for ext in &class.extends {
                Self::extract_extends_modification_constants(&alias, ext, ctx);
                Self::extract_import_extends_constants(
                    tree,
                    &alias,
                    &ext.base_name.to_string(),
                    ctx,
                );
            }
        }
    }

    /// Return lookup prefixes for imported classes/packages used in constant extraction.
    ///
    /// Includes both short import names and full qualified paths so structural
    /// dimension expressions can resolve either spelling without heuristic fallback.
    pub(crate) fn import_constant_prefixes(
        import: &ScopeImport,
    ) -> Vec<(String, rumoca_core::DefId)> {
        let mut out: Vec<(String, rumoca_core::DefId)> = Vec::new();
        let mut seen: std::collections::HashSet<(String, rumoca_core::DefId)> =
            std::collections::HashSet::new();
        let mut push_unique = |name: String, def_id: rumoca_core::DefId| {
            if name.is_empty() {
                return;
            }
            if seen.insert((name.clone(), def_id)) {
                out.push((name, def_id));
            }
        };

        match import {
            ScopeImport::Renamed {
                alias,
                path,
                def_id,
                ..
            } => {
                push_unique(alias.as_str().to_string(), *def_id);
                push_unique(path.join("."), *def_id);
                if let Some(last) = path.last() {
                    push_unique(last.clone(), *def_id);
                }
            }
            ScopeImport::Qualified { path, def_id } => {
                if let Some(last) = path.last() {
                    push_unique(last.clone(), *def_id);
                }
                push_unique(path.join("."), *def_id);
            }
            ScopeImport::Unqualified { .. } => {}
        }

        out
    }

    /// Recursively extract constants from nested classes of an imported class.
    fn extract_nested_class_constants_for_import(
        tree: &ClassTree,
        prefix: &str,
        class: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for (nested_name, nested_class) in &class.classes {
            let qualified = format!("{}.{}", prefix, nested_name);
            Self::extract_class_constants(&qualified, nested_class, ctx);
            // Follow extends chains of nested classes
            for ext in &nested_class.extends {
                Self::extract_extends_modification_constants(&qualified, ext, ctx);
                Self::extract_import_extends_constants(
                    tree,
                    &qualified,
                    &ext.base_name.to_string(),
                    ctx,
                );
            }
        }
    }

    /// Extract constants from an extends chain for imported classes.
    fn extract_import_extends_constants(
        tree: &ClassTree,
        alias: &str,
        base_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let Some(base_class) = tree.get_class_by_qualified_name(base_name) else {
            return;
        };
        Self::extract_class_constants(alias, base_class, ctx);
        for ext in &base_class.extends {
            Self::extract_extends_modification_constants(alias, ext, ctx);
            Self::extract_import_extends_constants(tree, alias, &ext.base_name.to_string(), ctx);
        }
    }

    /// Extract constant-affecting extends modifiers into the eval context.
    ///
    /// MLS §7.2/§7.3: extends modifiers override inherited constants and must be
    /// visible before evaluating dependent constants (e.g., nS/nX/nXi).
    fn extract_extends_modification_constants(
        alias: &str,
        ext: &rumoca_ir_ast::Extend,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for ext_mod in &ext.modifications {
            if ext_mod.redeclare {
                continue;
            }
            Self::extract_extends_modification_expr(alias, &ext_mod.expr, ctx);
        }
    }

    /// Re-evaluate component-scoped extends modifiers inside the dimension fixpoint.
    ///
    /// MLS §7.2/§7.3 modifications are part of instantiation. When an extends
    /// modifier depends on a sibling array dimension, such as
    /// `extends SIMO(final nout=size(columns, 1))`, it cannot be evaluated until
    /// colon dimensions for `columns[:]` have been inferred.
    pub(crate) fn reevaluate_component_scoped_extends_modification_constants(
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) -> bool {
        let mut progress = false;
        for instance_data in overlay.components.values() {
            let comp_scope = instance_data.qualified_name.to_component_path();
            if comp_scope.is_root() {
                continue;
            }
            let type_name = instance_data.type_name.as_str();
            if type_name.is_empty() {
                continue;
            }

            for ancestor in Self::collect_ancestor_classes(tree, type_name) {
                progress |= Self::reevaluate_class_extends_modification_constants_for_scope(
                    &comp_scope,
                    ancestor,
                    ctx,
                );
            }
        }
        progress
    }

    fn reevaluate_class_extends_modification_constants_for_scope(
        comp_scope: &ComponentPath,
        class_def: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) -> bool {
        let mut progress = false;
        for ext in &class_def.extends {
            for ext_mod in ext
                .modifications
                .iter()
                .filter(|ext_mod| !ext_mod.redeclare)
            {
                progress |=
                    Self::reevaluate_extends_modification_expr(comp_scope, &ext_mod.expr, ctx);
            }
        }
        progress
    }

    fn reevaluate_extends_modification_expr(
        comp_scope: &ComponentPath,
        expr: &Expression,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) -> bool {
        let Expression::Modification { target, value, .. } = expr else {
            return false;
        };
        let Some(target_path) = Self::component_reference_path(target) else {
            return false;
        };
        let full_path = comp_scope.join(&target_path).to_flat_string();
        let scope_name = comp_scope.to_flat_string();

        let mut progress = false;
        if let Some(val) = rumoca_eval_ast::eval::eval_integer_with_scope(value, ctx, &scope_name)
            && ctx.integers.get(full_path.as_str()).copied() != Some(val)
        {
            ctx.integers.insert(full_path.clone(), val);
            progress = true;
        }
        if let Some(val) = rumoca_eval_ast::eval::eval_boolean_with_scope(value, ctx, &scope_name)
            && ctx.booleans.get(full_path.as_str()).copied() != Some(val)
        {
            ctx.booleans.insert(full_path.clone(), val);
            progress = true;
        }
        if let Some(val) =
            rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(value, ctx, &scope_name)
            && ctx.dimensions.get(full_path.as_str()) != Some(&val)
        {
            ctx.dimensions.insert(full_path, val);
            progress = true;
        }
        progress
    }

    fn component_reference_path(
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> Option<ComponentPath> {
        if reference.parts.is_empty() {
            return None;
        }
        let mut parts = Vec::with_capacity(reference.parts.len());
        for part in &reference.parts {
            if part.subs.as_ref().is_some_and(|subs| !subs.is_empty()) {
                return None;
            }
            parts.push(part.ident.text.to_string());
        }
        Some(ComponentPath::from_parts(parts))
    }

    /// Walk an extends-modification expression and record scalar/dimension overrides.
    fn extract_extends_modification_expr(
        alias: &str,
        expr: &Expression,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if let Expression::Modification { target, value, .. } = expr {
            let target_name = target.to_string();
            let full_name = if alias.is_empty() {
                target_name
            } else {
                format!("{alias}.{}", target)
            };

            if let Some(val) = rumoca_eval_ast::eval::eval_integer_with_scope(value, ctx, alias) {
                ctx.integers.insert(full_name.clone(), val);
            }
            if let Some(val) = rumoca_eval_ast::eval::eval_boolean_with_scope(value, ctx, alias) {
                ctx.booleans.insert(full_name.clone(), val);
            }
            if let Some(dims) =
                rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(value, ctx, alias)
            {
                ctx.dimensions.insert(full_name, dims);
            }
        }
    }

    /// Apply direct extends-modifier constant overrides from one ancestor class.
    pub(crate) fn extract_ancestor_extends_modification_constants(
        ancestor: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for ext in &ancestor.extends {
            Self::extract_extends_modification_constants("", ext, ctx);
        }
    }

    /// Extract constant integer/real/boolean values and array dimensions from a class definition.
    /// MLS §4.5: Constants have values determined at compile time.
    pub(crate) fn extract_class_constants(
        prefix: &str,
        class: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for (name, comp) in &class.components {
            if !matches!(comp.variability, rumoca_core::Variability::Constant(_)) {
                continue;
            }
            let full_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            let type_name = comp.type_name.to_string();
            let binding =
                comp.binding
                    .as_ref()
                    .or((!matches!(comp.start, Expression::Empty { .. })).then_some(&comp.start));
            let Some(expr) = binding else { continue };
            Self::insert_constant_value(&full_name, &type_name, expr, prefix, ctx);
            // Also extract array dimensions from bindings (e.g., substanceNames = {mediumName})
            Self::insert_constant_dimensions(&full_name, &comp.shape, expr, prefix, ctx);
        }
    }

    /// Extract and insert array dimensions for a constant component.
    ///
    /// For array constants, dimensions come from either:
    /// - The component's explicit shape (e.g., `String[2] names`)
    /// - The binding expression (e.g., `String[:] names = {"air"}` → dims = [1])
    fn insert_constant_dimensions(
        full_name: &str,
        shape: &[usize],
        binding: &Expression,
        scope: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if ctx.dimensions.contains_key(full_name) {
            return;
        }
        // Use explicit shape if available and non-empty
        if !shape.is_empty() {
            ctx.dimensions.insert(full_name.to_string(), shape.to_vec());
            return;
        }
        // Try to infer dimensions from the binding expression
        if let Some(dims) =
            rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(binding, ctx, scope)
        {
            ctx.dimensions.insert(full_name.to_string(), dims);
        }
    }

    /// Evaluate and insert a single constant value into the eval context.
    fn insert_constant_value(
        full_name: &str,
        type_name: &str,
        expr: &Expression,
        scope: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        match type_name {
            "Integer" => {
                if let Some(val) = rumoca_eval_ast::eval::eval_integer_with_scope(expr, ctx, scope)
                {
                    ctx.integers.entry(full_name.to_string()).or_insert(val);
                }
            }
            "Real" => {
                if let Some(val) = rumoca_eval_ast::eval::eval_real_with_scope(expr, ctx, scope) {
                    ctx.reals.entry(full_name.to_string()).or_insert(val);
                }
            }
            "Boolean" => {
                if let Some(val) = rumoca_eval_ast::eval::eval_boolean_with_scope(expr, ctx, scope)
                {
                    ctx.booleans.entry(full_name.to_string()).or_insert(val);
                }
            }
            _ => {}
        }
    }

    /// Collect constants from nested classes in the model being compiled (MLS §7.3).
    ///
    /// When a model has `replaceable package Medium = SomeMedium`, the
    /// package-level constants (nX, nXi, nC, nS) are not instantiated as
    /// overlay components. This function finds the model's ClassDef and all
    /// its ancestors, then extracts constants from their nested class
    /// declarations (following extends chains to concrete types).
    ///
    /// Uses multi-pass extraction to resolve cascading dependencies like:
    /// `substanceNames[1]` → `nS = size(substanceNames,1)` → `nX = nS`
    pub(crate) fn collect_nested_class_constants(
        tree: &ClassTree,
        model_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        // Get all ancestors of the model (including itself) via extends chains
        let ancestors = Self::collect_ancestor_classes(tree, model_name);
        if ancestors.is_empty() {
            return;
        }
        const MAX_PASSES: usize = 5;
        for _pass in 0..MAX_PASSES {
            let prev_count = ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len();
            for ancestor in &ancestors {
                Self::extract_nested_class_constants_from(tree, ancestor, model_name, ctx);
            }
            let new_count = ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len();
            if new_count == prev_count {
                break;
            }
        }
    }

    /// Extract constants from nested classes of a given class definition.
    fn extract_nested_class_constants_from(
        tree: &ClassTree,
        class_def: &ClassDef,
        model_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        // Walk nested class declarations (e.g., `package Medium = SomeMedium`)
        for (nested_name, nested_class) in &class_def.classes {
            // MLS §5.3: local nested class/package names shadow imported aliases.
            // Clear stale alias-prefixed values before seeding this class scope.
            Self::clear_alias_scope_values(ctx, nested_name);
            // Extract constants directly from the nested class
            Self::extract_class_constants(nested_name, nested_class, ctx);
            // Follow extends chains to concrete types and extract their constants
            for ext in &nested_class.extends {
                Self::extract_extends_modification_constants(nested_name, ext, ctx);
                Self::extract_nested_extends_redeclare_constants(
                    tree,
                    nested_name,
                    model_name,
                    ext,
                    ctx,
                );
                Self::extract_class_constants_from_extends(
                    tree,
                    nested_name,
                    &ext.base_name.to_string(),
                    model_name,
                    ctx,
                );
            }
        }
    }

    /// Extract nested/redeclare constants for each instantiated component type
    /// into the component instance scope.
    pub(crate) fn collect_component_type_nested_constants(
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        const MAX_PASSES: usize = 4;
        for _ in 0..MAX_PASSES {
            let prev =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            for instance_data in overlay.components.values() {
                Self::collect_component_instance_type_nested_constants(tree, instance_data, ctx);
            }
            let new =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            if new == prev {
                break;
            }
        }
    }

    fn collect_component_instance_type_nested_constants(
        tree: &ClassTree,
        instance_data: &rumoca_ir_ast::InstanceData,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let comp_scope = instance_data.qualified_name.to_flat_string();
        if comp_scope.is_empty() {
            return;
        }
        let type_name = instance_data.type_name.as_str();
        if type_name.is_empty() {
            return;
        }

        let ancestors = Self::collect_ancestor_classes(tree, type_name);
        for ancestor in ancestors {
            Self::extract_class_extends_constants_for_scope(
                tree,
                ancestor,
                &comp_scope,
                type_name,
                ctx,
            );
            Self::extract_nested_class_constants_from_scoped(
                tree,
                ancestor,
                &comp_scope,
                type_name,
                ctx,
            );
        }
    }

    /// Materialize class-level extends redeclare constants under an instance scope.
    ///
    /// Example:
    /// `comp_scope = voltage.term`, class has
    /// `extends Interfaces.TerminalDC(redeclare package PhaseSystem = ...);`
    /// -> populate `voltage.term.PhaseSystem.*`.
    fn extract_class_extends_constants_for_scope(
        tree: &ClassTree,
        class_def: &ClassDef,
        comp_scope: &str,
        resolve_context: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for ext in &class_def.extends {
            Self::extract_extends_modification_constants(comp_scope, ext, ctx);
            Self::extract_nested_extends_redeclare_constants(
                tree,
                comp_scope,
                resolve_context,
                ext,
                ctx,
            );
        }
    }

    /// Scoped variant of nested class constant extraction.
    ///
    /// Emits constants under `<base_scope>.<nested_name>.*` so dimension
    /// expressions in nested members resolve lexically in instance scope.
    fn extract_nested_class_constants_from_scoped(
        tree: &ClassTree,
        class_def: &ClassDef,
        base_scope: &str,
        resolve_context: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for (nested_name, nested_class) in &class_def.classes {
            let nested_alias = format!("{base_scope}.{nested_name}");
            Self::extract_class_constants(&nested_alias, nested_class, ctx);
            for ext in &nested_class.extends {
                Self::extract_extends_modification_constants(&nested_alias, ext, ctx);
                Self::extract_nested_extends_redeclare_constants(
                    tree,
                    &nested_alias,
                    resolve_context,
                    ext,
                    ctx,
                );
                Self::extract_class_constants_from_extends(
                    tree,
                    &nested_alias,
                    &ext.base_name.to_string(),
                    resolve_context,
                    ctx,
                );
            }
        }
    }

    /// Materialize nested-class `extends(... redeclare package/class ...)` constants.
    ///
    /// Example: inside nested class `TwoPin`,
    /// `extends Interfaces.TerminalDC(redeclare package PhaseSystem = PhaseSystems.TwoConductor)`
    /// must populate `TwoPin.PhaseSystem.*` so expressions like `PhaseSystem.n`
    /// in inherited dimension declarations become evaluable.
    fn extract_nested_extends_redeclare_constants(
        tree: &ClassTree,
        nested_alias: &str,
        resolve_context: &str,
        ext: &rumoca_ir_ast::Extend,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for ext_mod in &ext.modifications {
            let Expression::Modification { target, value, .. } = &ext_mod.expr else {
                continue;
            };
            let looks_like_class_or_package_rebind = matches!(
                value.as_ref(),
                Expression::ComponentReference(_) | Expression::ClassModification { .. }
            );
            if !ext_mod.redeclare && !looks_like_class_or_package_rebind {
                continue;
            }
            let Some(def_id) = Self::resolve_redeclare_target_def_id(tree, value, resolve_context)
            else {
                continue;
            };
            let alias_scope = format!("{nested_alias}.{target}");
            Self::extract_override_class_constants(tree, &alias_scope, def_id, ctx);
        }
    }

    /// Extract constants from a class reached via an extends chain.
    /// Recursively follows extends to extract from all ancestor classes.
    /// Uses scope-based resolution for relative extends names.
    fn extract_class_constants_from_extends(
        tree: &ClassTree,
        alias: &str,
        base_name: &str,
        resolve_context: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let (base_class, resolved_qname) =
            Self::resolve_class_name_with_qname(tree, base_name, resolve_context);
        let Some(base_class) = base_class else {
            return;
        };
        let qname = resolved_qname.unwrap_or_else(|| base_name.to_string());
        Self::extract_class_constants(alias, base_class, ctx);
        // Recursively follow extends using resolved name as context
        for ext in &base_class.extends {
            Self::extract_extends_modification_constants(alias, ext, ctx);
            Self::extract_class_constants_from_extends(
                tree,
                alias,
                &ext.base_name.to_string(),
                &qname,
                ctx,
            );
        }
    }

    /// Collect constants from the enclosing class of the model being compiled (MLS §5.3).
    ///
    /// For `Modelica.Media.IdealGases.Common.SingleGasNasa.BaseProperties`,
    /// the enclosing class is `Modelica.Media.IdealGases.Common.SingleGasNasa`.
    /// Constants like `nX`, `nXi`, `nS` defined in the enclosing package are
    /// needed for dimension expressions in the model (e.g., `Xi[nXi]`).
    /// Uses multi-pass extraction to resolve cascading dependencies.
    pub(crate) fn collect_enclosing_class_constants(
        tree: &ClassTree,
        model_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let Some(pos) = find_last_top_level_dot(model_name) else {
            return;
        };
        let enclosing_name = &model_name[..pos];
        // Collect all ancestor classes (enclosing + full extends chain)
        let ancestors = Self::collect_ancestor_classes(tree, enclosing_name);
        if ancestors.is_empty() {
            return;
        }
        Self::extract_enclosing_constants_multi_pass(&ancestors, ctx);
    }
}
