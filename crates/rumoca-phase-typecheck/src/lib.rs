//! Type checking phase for the Rumoca compiler.
//!
//! This phase walks the resolved Class Tree and:
//! 1. Resolves type specifiers to TypeIds
//! 2. Populates the type_id fields on components
//! 3. **Evaluates dimension expressions** (MLS §10.1)
//! 4. **Marks structural parameters** (MLS §18.3)
//! 5. **Infers array dimensions from bindings**
//! 6. Performs type checking on expressions
//! 7. Validates type constraints (variability, causality, etc.)
//!
//! The input is a `ResolvedTree` and the output is a `TypedTree`.
//! Both wrap the same underlying `ClassTree`, but the newtype wrappers
//! provide compile-time guarantees about which phase has been completed.
//!
//! ## Dimension Evaluation (MLS §10.1)
//!
//! Dimensions are evaluated during typing, not flattening. This ensures:
//! - Concrete dimensions are available before instantiation
//! - Structural parameters are identified early
//! - Array sizes for for-loops can be computed at compile time

mod modifier_targets;
mod typechecker;

use miette::{Diagnostic, SourceSpan};
use rumoca_core::{
    DefId, Diagnostic as CommonDiagnostic, Diagnostics, PrimaryLabel, ScopeId, SourceMap, Span,
    TypeId, find_last_top_level_dot, has_top_level_dot, parent_scope, top_level_last_segment,
};
use rumoca_ir_ast::{
    ClassDef, ClassKind, ClassTree, Component, EnumerationType, Expression, InstanceOverlay,
    ResolvedTree, ScopeImport, StoredDefinition, Type, TypeAlias, TypeClassType, TypeTable,
    TypedTree,
};
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use typechecker::traversal_adapter::walk_equations;

pub use typechecker::api::{typecheck, typecheck_instanced};

/// Type alias for typecheck results with boxed errors.
///
/// Boxing the error type avoids clippy::result_large_err warnings while
/// preserving rich diagnostic information. The error path is cold (errors
/// are exceptional), so the allocation overhead is negligible.
pub type TypeCheckResult<T> = Result<T, Box<TypeCheckError>>;

/// Errors that can occur during type checking.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum TypeCheckError {
    /// A type was referenced but not found.
    #[error("undefined type: `{name}` not found")]
    #[diagnostic(
        code(rumoca::typecheck::ET001),
        help("check that the type name is spelled correctly")
    )]
    UndefinedType {
        name: String,
        #[label("type not found")]
        span: SourceSpan,
    },

    /// A type mismatch in an expression or equation.
    #[error("type mismatch: expected `{expected}`, found `{found}`")]
    #[diagnostic(
        code(rumoca::typecheck::ET002),
        help("MLS §4: types must be compatible for this operation")
    )]
    TypeMismatch {
        expected: String,
        found: String,
        #[label("type mismatch here")]
        span: SourceSpan,
    },

    /// Invalid variability constraint.
    #[error("variability error: {message}")]
    #[diagnostic(
        code(rumoca::typecheck::ET003),
        help("MLS §4.5: variability must be respected in assignments")
    )]
    VariabilityError {
        message: String,
        #[label("variability error here")]
        span: SourceSpan,
    },

    /// Array dimensions could not be evaluated.
    #[error("unevaluable array dimensions for '{name}': {reason}")]
    #[diagnostic(
        code(rumoca::typecheck::ET004),
        help(
            "MLS §10.1: array dimensions must be parameter expressions evaluable at translation time"
        )
    )]
    UnevaluableDimensions { name: String, reason: String },
}

impl TypeCheckError {
    /// Create an UndefinedType error.
    pub fn undefined_type(name: impl Into<String>, span: Span) -> Self {
        Self::UndefinedType {
            name: name.into(),
            span: span.to_source_span(),
        }
    }

    /// Create a TypeMismatch error.
    pub fn type_mismatch(
        expected: impl Into<String>,
        found: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::TypeMismatch {
            expected: expected.into(),
            found: found.into(),
            span: span.to_source_span(),
        }
    }

    /// Create a VariabilityError.
    pub fn variability_error(message: impl Into<String>, span: Span) -> Self {
        Self::VariabilityError {
            message: message.into(),
            span: span.to_source_span(),
        }
    }

    /// Create an UnevaluableDimensions error.
    pub fn unevaluable_dimensions(name: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::UnevaluableDimensions {
            name: name.into(),
            reason: reason.into(),
        }
    }
}

/// Type checking context.
pub struct TypeChecker {
    /// Collected diagnostics.
    diagnostics: Diagnostics,
    /// Evaluation context for current class (built from constants/parameters).
    eval_ctx: rumoca_eval_ast::eval::TypeCheckEvalContext,
    /// Source map for file name → SourceId resolution in diagnostics.
    source_map: SourceMap,
    /// DefId → fully-qualified class name map for anchor-aware dotted type lookup.
    def_qualified_names: HashMap<DefId, String>,
    /// Resolved TypeId map for user-defined type DefIds.
    type_ids_by_def_id: HashMap<DefId, TypeId>,
    /// Unique dotted-suffix index for type-name fallback lookup.
    ///
    /// Key examples:
    /// - `Modelica.Units.SI.Reluctance`
    /// - `SI.Reluctance`
    /// - `Reluctance`
    ///
    /// Value is `Some(TypeId)` when unique, `None` when ambiguous.
    type_suffix_index: HashMap<String, Option<TypeId>>,
    /// Canonical type roots used for compatibility checks.
    ///
    /// This unwraps aliases and trivial class wrappers (e.g. operator-record
    /// unit wrappers) so assignment checks compare semantic roots.
    type_roots: HashMap<TypeId, TypeId>,
    /// Component type ids visible in the current class scope.
    current_component_types: HashMap<String, TypeId>,
    /// Allowed first-segment modifier targets per class DefId.
    ///
    /// Includes direct and inherited members (components and nested classes),
    /// with `break` names removed per extends-clause selection rules.
    component_modifier_targets: HashMap<DefId, HashSet<String>>,
    /// Component member types available for modifier-path validation.
    ///
    /// Keys are class DefIds; values map component member names to their TypeIds
    /// (including inherited members, with extends `break` names removed).
    component_modifier_member_types: HashMap<DefId, HashMap<String, TypeId>>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        Self {
            diagnostics: Diagnostics::new(),
            eval_ctx: rumoca_eval_ast::eval::TypeCheckEvalContext::new(),
            source_map: SourceMap::default(),
            def_qualified_names: HashMap::new(),
            type_ids_by_def_id: HashMap::new(),
            type_suffix_index: HashMap::new(),
            type_roots: HashMap::new(),
            current_component_types: HashMap::new(),
            component_modifier_targets: HashMap::new(),
            component_modifier_member_types: HashMap::new(),
        }
    }

    /// Type check a ClassTree.
    pub fn check(&mut self, tree: &mut ClassTree) {
        self.source_map = tree.source_map.clone();
        self.def_qualified_names = tree
            .def_map
            .iter()
            .map(|(def_id, name)| (*def_id, name.clone()))
            .collect();
        let (type_table, type_ids_by_def_id) = Self::build_type_context(tree);
        tree.type_table = type_table;
        self.type_ids_by_def_id = type_ids_by_def_id;
        self.type_suffix_index = Self::build_type_suffix_index(&tree.type_table);
        self.rebuild_type_roots(tree, &tree.type_table);
        self.component_modifier_targets = modifier_targets::build_component_modifier_targets(tree);
        self.component_modifier_member_types =
            modifier_targets::build_component_modifier_member_types(
                tree,
                &tree.type_table,
                &self.type_ids_by_def_id,
                &self.type_suffix_index,
            );
        self.check_stored_definition(&mut tree.definitions, &mut tree.type_table);
    }

    /// Type check an instanced model using the overlay for modification context.
    ///
    /// This builds an evaluation context from the overlay's parameter values
    /// and evaluates dimensions for components that have unevaluated dimensions.
    pub fn check_instanced(
        &mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        model_name: &str,
    ) {
        self.source_map = tree.source_map.clone();
        self.def_qualified_names = tree
            .def_map
            .iter()
            .map(|(def_id, name)| (*def_id, name.clone()))
            .collect();
        // Build evaluation context from overlay's parameter values
        self.eval_ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::new();
        let (type_table, type_ids_by_def_id) = Self::build_type_context(tree);
        self.type_ids_by_def_id = type_ids_by_def_id;
        self.type_suffix_index = Self::build_type_suffix_index(&type_table);
        self.rebuild_type_roots(tree, &type_table);
        self.component_modifier_targets = modifier_targets::build_component_modifier_targets(tree);
        self.component_modifier_member_types =
            modifier_targets::build_component_modifier_member_types(
                tree,
                &type_table,
                &self.type_ids_by_def_id,
                &self.type_suffix_index,
            );
        self.populate_overlay_type_roots(tree, overlay, &type_table);
        self.resolve_overlay_component_types(overlay, &type_table);

        // Extract parameter values from the overlay (integers, booleans, reals)
        // Uses scope-aware evaluation so `nout = max(size(deltaq,1))` in component
        // `kinematicPTP` can resolve `deltaq` as `kinematicPTP.deltaq`.
        for (_def_id, instance_data) in &overlay.components {
            let name = instance_data.qualified_name.to_flat_string();
            let scope = parent_scope(&name).unwrap_or("");

            // Try integer first (binding then start)
            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_integer_with_scope(binding, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_integer(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_integer_with_scope(start, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_integer(&name, value);
            }

            // Try boolean (binding then start)
            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_boolean_with_scope(binding, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_boolean(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_boolean_with_scope(start, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_boolean(&name, value);
            }

            // Try real (binding then start)
            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_real_with_scope(binding, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_real(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_real_with_scope(start, &self.eval_ctx, scope)
            {
                self.eval_ctx.add_real(&name, value);
            }

            // Try enumeration (binding then start)
            // Enum values are ComponentReferences with qualified paths (>= 2 parts)
            if let Some(ref binding) = instance_data.binding
                && let Some(value) = rumoca_eval_ast::eval::extract_enum_value(binding)
            {
                self.eval_ctx.add_enum(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) = rumoca_eval_ast::eval::extract_enum_value(start)
            {
                self.eval_ctx.add_enum(&name, value);
            }

            // Pre-populate dimensions from overlay if already known
            if !instance_data.dims.is_empty() {
                self.eval_ctx.add_dimensions(
                    &name,
                    instance_data.dims.iter().map(|&d| d as usize).collect(),
                );
            }
        }

        // Collect enumeration type sizes from the class tree (MLS §10.5).
        // When an enumeration type is used as a dimension (e.g., `Real x[Logic]`),
        // the size of that dimension is the number of enumeration literals.
        Self::collect_enum_sizes(tree, &mut self.eval_ctx);
        // MLS §7.3: later constant extraction performs many suffix lookups on
        // real libraries; seed the index before walking import/extends chains.
        self.eval_ctx.build_suffix_index();

        // Collect integer/real constants from imported classes (MLS §13.2).
        // This handles patterns like `import generator = ...Xorshift128plus;`
        // where `generator.nState` is used as a dimension expression.
        Self::collect_import_constants(tree, &mut self.eval_ctx);

        // Collect constants from direct model-level extends redeclare overrides
        // (e.g., `extends Base(redeclare package Medium = ...);`).
        // MLS §7.3: redeclare overrides are active in the derived class scope.
        Self::collect_model_extends_redeclare_constants(tree, model_name, &mut self.eval_ctx);

        // Collect constants from nested package classes (MLS §7.3).
        // When a model has `replaceable package Medium = PartialMedium`, the
        // package-level constants (nX, nXi, etc.) need to be available for
        // dimension evaluation. Extract them from the ClassTree.
        Self::collect_nested_class_constants(tree, model_name, &mut self.eval_ctx);

        // Re-apply direct model-level extends redeclare overrides after nested
        // class extraction so redeclared package/type constants take precedence
        // over inherited replaceable defaults (MLS §7.3).
        Self::collect_model_extends_redeclare_constants(tree, model_name, &mut self.eval_ctx);

        // Collect constants from the enclosing class (MLS §5.3).
        // When compiling `Package.BaseProperties`, constants like nX, nXi
        // defined in `Package` need to be available for dimension evaluation.
        Self::collect_enclosing_class_constants(tree, model_name, &mut self.eval_ctx);

        // Collect function definitions for compile-time evaluation (MLS §12.4).
        // When a dimension depends on a user-defined function call (e.g.,
        // `Integer ind[:] = indexPositiveSequence(m)`), the function definition
        // must be available so the output dimensions can be inferred.
        Self::collect_function_defs(tree, &mut self.eval_ctx);

        // MLS §7.3: apply instance-specific class/package redeclare overrides
        // from instantiation (e.g., `a(redeclare package Medium=...)`) so
        // `a.Medium.*` and `b.Medium.*` constants are scoped per instance.
        Self::collect_instance_class_override_constants(tree, overlay, &mut self.eval_ctx);
        let record_aliases = Self::collect_record_aliases(overlay);

        // Multi-pass dimension evaluation (MLS §10.1)
        //
        // Both explicit dimensions (e.g., `x[size(a,1)-1]`) and colon dimensions
        // (e.g., `a[:]={1,2,3}`) are evaluated in the same loop because they can
        // depend on each other. For example:
        //   parameter Real a[:] = {1};           // colon → inferred as [1]
        //   Real x[size(a, 1) - 1];              // explicit → depends on a's dims
        //   parameter Integer nx = size(a, 1);    // integer → depends on a's dims
        //   Real x_scaled[size(x, 1)];            // explicit → depends on x's dims
        self.evaluate_all_dimensions_multi_pass(overlay, &record_aliases);

        // Validate that all array dimensions have been evaluated
        // This is MLS §10.1 compliance - dimensions must be evaluable at translation time
        self.validate_dimensions(overlay);

        // Validate component modifiers in the target model scope.
        // This catches misspelled builtin modifiers like `startd=...`.
        self.check_instanced_component_modifiers(tree, model_name, &type_table);

        // Type-check model equations with instanced type identities from the overlay.
        self.check_instanced_equations(tree, overlay, model_name, &type_table);
    }

    fn check_instanced_component_modifiers(
        &mut self,
        tree: &ClassTree,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let model_class = tree
            .get_class_by_qualified_name(model_name)
            .or_else(|| tree.get_class_by_qualified_name(top_level_last_segment(model_name)));
        let Some(model_class) = model_class else {
            return;
        };
        self.check_instanced_component_modifiers_in_class(model_class, type_table);
    }

    fn check_instanced_component_modifiers_in_class(
        &mut self,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        for (comp_name, comp) in &class.components {
            let type_name = comp.type_name.to_string();
            let type_id = self.resolve_type_name(&type_name, comp.type_def_id, type_table);
            self.validate_component_modifier_names(comp_name, comp, type_table, type_id);
        }
        for nested in class.classes.values() {
            self.check_instanced_component_modifiers_in_class(nested, type_table);
        }
    }

    /// Check equation compatibility for a specific instanced model.
    fn check_instanced_equations(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let model_class = tree
            .get_class_by_qualified_name(model_name)
            .or_else(|| tree.get_class_by_qualified_name(top_level_last_segment(model_name)));
        let Some(model_class) = model_class else {
            return;
        };

        let prev_scope_types = std::mem::take(&mut self.current_component_types);
        self.current_component_types =
            Self::build_instanced_component_type_scope(overlay, model_name);

        // Validate builtin modifier value types with instanced component scope.
        self.check_component_modifier_types_in_class(model_class, type_table);

        walk_equations(self, &model_class.equations, type_table);
        walk_equations(self, &model_class.initial_equations, type_table);

        self.current_component_types = prev_scope_types;
    }

    /// Collect constants from instance-level class/package redeclare overrides.
    ///
    /// Example:
    /// - `a(redeclare package Medium = MediumA)`
    /// - `b(redeclare package Medium = MediumB)`
    ///
    /// This populates `a.Medium.*` and `b.Medium.*` from each instance's
    /// `class_overrides`, so dotted references resolve lexically without
    /// depending on global suffix matching.
    fn collect_instance_class_override_constants(
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let component_index: HashMap<String, &rumoca_ir_ast::InstanceData> = overlay
            .components
            .values()
            .map(|data| (data.qualified_name.to_flat_string(), data))
            .collect();

        const MAX_PASSES: usize = 5;
        for _ in 0..MAX_PASSES {
            let prev =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();

            for data in overlay.components.values() {
                Self::apply_instance_class_overrides(tree, &component_index, data, ctx);
            }

            let new =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            if new == prev {
                break;
            }
        }
    }

    fn apply_instance_class_overrides(
        tree: &ClassTree,
        component_index: &HashMap<String, &rumoca_ir_ast::InstanceData>,
        data: &rumoca_ir_ast::InstanceData,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if data.class_overrides.is_empty() {
            return;
        }
        let comp_scope = data.qualified_name.to_flat_string();
        if comp_scope.is_empty() {
            return;
        }

        let active_alias = Self::component_active_alias(data);
        for (alias, def_id) in &data.class_overrides {
            Self::apply_class_override_alias(
                tree,
                component_index,
                &comp_scope,
                active_alias.as_deref(),
                alias,
                *def_id,
                ctx,
            );
        }
    }

    fn apply_class_override_alias(
        tree: &ClassTree,
        component_index: &HashMap<String, &rumoca_ir_ast::InstanceData>,
        comp_scope: &str,
        active_alias: Option<&str>,
        alias: &str,
        def_id: DefId,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if Self::try_apply_forwarded_parent_alias_constants(
            tree,
            component_index,
            comp_scope,
            active_alias,
            alias,
            def_id,
            ctx,
        ) {
            return;
        }

        let alias_scope = format!("{comp_scope}.{alias}");
        Self::extract_override_class_constants(tree, &alias_scope, def_id, ctx);

        // For declarations like `Medium.BaseProperties medium`, expose
        // unqualified constants (`medium.nX`) from the active alias only.
        if active_alias == Some(alias) {
            Self::extract_override_class_constants(tree, comp_scope, def_id, ctx);
        }
    }

    fn try_apply_forwarded_parent_alias_constants(
        tree: &ClassTree,
        component_index: &HashMap<String, &rumoca_ir_ast::InstanceData>,
        comp_scope: &str,
        active_alias: Option<&str>,
        alias: &str,
        def_id: DefId,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) -> bool {
        let Some(def_qname) = tree.def_map.get(&def_id) else {
            return false;
        };
        if top_level_last_segment(def_qname) != alias {
            return false;
        }

        let parent_scope = Self::parent_scope(comp_scope);
        if parent_scope.is_empty() {
            return false;
        }
        let Some(parent_data) = component_index.get(parent_scope) else {
            return false;
        };
        if !parent_data.class_overrides.contains_key(alias) {
            return false;
        }

        let source_alias = format!("{comp_scope}.{alias}");
        let target_alias = format!("{parent_scope}.{alias}");
        let alias_pair = [(source_alias, target_alias.clone())];
        Self::propagate_alias_values_in_ctx(&alias_pair, ctx);

        if active_alias == Some(alias) {
            let root_pair = [(comp_scope.to_string(), target_alias)];
            Self::propagate_alias_values_in_ctx(&root_pair, ctx);
        }

        true
    }

    fn propagate_alias_values_in_ctx(
        alias_pairs: &[(String, String)],
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        Self::propagate_alias_map(alias_pairs, &mut ctx.integers);
        Self::propagate_alias_map(alias_pairs, &mut ctx.reals);
        Self::propagate_alias_map(alias_pairs, &mut ctx.booleans);
        Self::propagate_alias_map(alias_pairs, &mut ctx.enums);
        Self::propagate_alias_map(alias_pairs, &mut ctx.dimensions);
        Self::propagate_alias_map(alias_pairs, &mut ctx.enum_sizes);
        Self::propagate_alias_map(alias_pairs, &mut ctx.enum_ordinals);
    }

    fn component_active_alias(data: &rumoca_ir_ast::InstanceData) -> Option<String> {
        if let Some((head, _tail)) = data.type_name.split_once('.')
            && data.class_overrides.contains_key(head)
        {
            return Some(head.to_string());
        }

        // If this instance has exactly one class/package override, that alias is
        // the active lexical source for unqualified constants in nested members.
        if data.class_overrides.len() == 1 {
            return data.class_overrides.keys().next().cloned();
        }

        None
    }

    /// Build lookup keys for top-level model components from instanced overlay data.
    fn build_instanced_component_type_scope(
        overlay: &InstanceOverlay,
        model_name: &str,
    ) -> HashMap<String, TypeId> {
        let mut out = HashMap::new();
        let full_prefix = format!("{model_name}.");
        let short_model = top_level_last_segment(model_name);
        let short_prefix = format!("{short_model}.");

        for data in overlay.components.values() {
            let qn = data.qualified_name.to_flat_string();
            let canonical_type = overlay
                .type_roots
                .get(&data.type_id)
                .copied()
                .unwrap_or(data.type_id);
            if let Some(rest) = qn.strip_prefix(&full_prefix) {
                out.insert(qn.clone(), canonical_type);
                Self::insert_instanced_aliases(&mut out, rest, canonical_type, Some(short_model));
                continue;
            }
            if let Some(rest) = qn.strip_prefix(&short_prefix) {
                out.insert(qn.clone(), canonical_type);
                Self::insert_instanced_aliases(&mut out, rest, canonical_type, None);
                continue;
            }
            if !has_top_level_dot(&qn) {
                out.insert(qn, canonical_type);
            }
        }

        out
    }

    fn insert_instanced_aliases(
        out: &mut HashMap<String, TypeId>,
        rest: &str,
        type_id: TypeId,
        short_model: Option<&str>,
    ) {
        if has_top_level_dot(rest) {
            return;
        }
        out.insert(rest.to_string(), type_id);
        if let Some(short_model) = short_model {
            out.insert(format!("{short_model}.{rest}"), type_id);
        }
    }

    /// Build a type context that includes user-defined classes, enums, and aliases.
    fn build_type_context(tree: &ClassTree) -> (TypeTable, HashMap<DefId, TypeId>) {
        let mut type_table = tree.type_table.clone();
        let mut type_ids_by_def_id = HashMap::new();

        // Register classes and enumerations first.
        for (qualified_name, &def_id) in &tree.name_map {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };

            if !class.enum_literals.is_empty() {
                let id = Self::register_enumeration_type(&mut type_table, qualified_name, class);
                type_ids_by_def_id.insert(def_id, id);
                continue;
            }

            if matches!(class.class_type, rumoca_ir_ast::ClassType::Type) {
                continue;
            }

            let id = Self::register_class_type(
                &mut type_table,
                qualified_name,
                def_id,
                &class.class_type,
            );
            type_ids_by_def_id.insert(def_id, id);
        }

        // Register aliases with placeholder targets so alias chains are representable.
        for (qualified_name, &def_id) in &tree.name_map {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };
            if !matches!(class.class_type, rumoca_ir_ast::ClassType::Type)
                || !class.enum_literals.is_empty()
            {
                continue;
            }

            let id = if let Some(existing) = type_table.lookup(qualified_name) {
                existing
            } else {
                type_table.add_type(Type::Alias(TypeAlias {
                    name: qualified_name.clone(),
                    aliased: TypeId::UNKNOWN,
                }))
            };
            type_ids_by_def_id.insert(def_id, id);
        }

        // Resolve alias targets once all alias ids exist.
        for (_qualified_name, &def_id) in &tree.name_map {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };
            if !matches!(class.class_type, rumoca_ir_ast::ClassType::Type)
                || !class.enum_literals.is_empty()
            {
                continue;
            }

            let Some(&alias_id) = type_ids_by_def_id.get(&def_id) else {
                continue;
            };
            let aliased =
                Self::resolve_alias_target_type_id(class, &type_table, &type_ids_by_def_id);
            if let Some(Type::Alias(alias)) = type_table.get_mut(alias_id) {
                alias.aliased = aliased;
            }
        }

        (type_table, type_ids_by_def_id)
    }

    fn build_type_suffix_index(type_table: &TypeTable) -> HashMap<String, Option<TypeId>> {
        let mut index = HashMap::new();
        for idx in 0..type_table.len() {
            let type_id = TypeId::new(idx as u32);
            let Some(type_name) = type_table.get(type_id).and_then(|ty| ty.name()) else {
                continue;
            };
            Self::insert_type_suffixes(&mut index, type_name, type_id);
        }
        index
    }

    fn insert_type_suffixes(
        index: &mut HashMap<String, Option<TypeId>>,
        type_name: &str,
        type_id: TypeId,
    ) {
        Self::insert_type_suffix(index, type_name, type_id);
        let mut offset = 0usize;
        while let Some(dot_rel) = type_name[offset..].find('.') {
            offset += dot_rel + 1;
            let suffix = &type_name[offset..];
            if suffix.is_empty() {
                break;
            }
            Self::insert_type_suffix(index, suffix, type_id);
        }
    }

    fn insert_type_suffix(
        index: &mut HashMap<String, Option<TypeId>>,
        suffix: &str,
        type_id: TypeId,
    ) {
        use std::collections::hash_map::Entry;
        match index.entry(suffix.to_string()) {
            Entry::Vacant(entry) => {
                entry.insert(Some(type_id));
            }
            Entry::Occupied(mut entry) => {
                if entry.get().is_some_and(|existing| existing != type_id) {
                    entry.insert(None);
                }
            }
        }
    }

    fn register_enumeration_type(
        type_table: &mut TypeTable,
        qualified_name: &str,
        class: &ClassDef,
    ) -> TypeId {
        if let Some(existing) = type_table.lookup(qualified_name) {
            return existing;
        }
        let literals = class
            .enum_literals
            .iter()
            .map(|lit| lit.ident.text.to_string())
            .collect();
        type_table.add_type(Type::Enumeration(EnumerationType {
            name: qualified_name.to_string(),
            literals,
        }))
    }

    fn register_class_type(
        type_table: &mut TypeTable,
        qualified_name: &str,
        def_id: DefId,
        class_type: &rumoca_ir_ast::ClassType,
    ) -> TypeId {
        if let Some(existing) = type_table.lookup(qualified_name) {
            return existing;
        }

        let kind = match class_type {
            rumoca_ir_ast::ClassType::Class => ClassKind::Class,
            rumoca_ir_ast::ClassType::Model => ClassKind::Model,
            rumoca_ir_ast::ClassType::Block => ClassKind::Block,
            rumoca_ir_ast::ClassType::Record => ClassKind::Record,
            rumoca_ir_ast::ClassType::Connector => ClassKind::Connector,
            rumoca_ir_ast::ClassType::Type => ClassKind::Type,
            rumoca_ir_ast::ClassType::Package => ClassKind::Package,
            rumoca_ir_ast::ClassType::Function => ClassKind::Function,
            rumoca_ir_ast::ClassType::Operator => ClassKind::Operator,
        };

        type_table.add_type(Type::Class(TypeClassType {
            name: qualified_name.to_string(),
            def_id,
            kind,
        }))
    }

    fn resolve_alias_target_type_id(
        class: &ClassDef,
        type_table: &TypeTable,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> TypeId {
        let Some(ext) = class.extends.first() else {
            return TypeId::UNKNOWN;
        };

        if let Some(base_def_id) = ext.base_def_id
            && let Some(&target) = type_ids_by_def_id.get(&base_def_id)
        {
            return target;
        }

        let base_name = ext.base_name.to_string();
        type_table
            .lookup(&base_name)
            .or_else(|| type_table.lookup(top_level_last_segment(&base_name)))
            .unwrap_or(TypeId::UNKNOWN)
    }

    /// Resolve and populate component type ids in the instance overlay.
    ///
    /// This is used for the instanced pipeline where flatten consumes overlay type_ids.
    fn resolve_overlay_component_types(
        &mut self,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
    ) {
        for (_instance_id, data) in overlay.components.iter_mut() {
            let resolved = self.resolve_type_name(&data.type_name, data.type_def_id, type_table);
            if resolved.is_unknown() {
                let instance_name = data.qualified_name.to_flat_string();
                let span = self.source_map.location_to_span(
                    &data.source_location.file_name,
                    data.source_location.start as usize,
                    data.source_location.end as usize,
                );
                self.diagnostics.emit(CommonDiagnostic::error(
                    "ET001",
                    format!(
                        "undefined type '{}' for instance '{}'",
                        data.type_name, instance_name
                    ),
                    PrimaryLabel::new(span).with_message("type declaration here"),
                ));
            }
            data.type_id = resolved;
        }
    }

    /// Populate overlay-level canonical type roots for downstream flatten checks.
    fn populate_overlay_type_roots(
        &self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
    ) {
        overlay.type_roots.clear();
        for idx in 0..type_table.len() {
            let ty = TypeId::new(idx as u32);
            overlay
                .type_roots
                .insert(ty, self.resolve_overlay_type_root(tree, type_table, ty));
        }
    }

    fn rebuild_type_roots(&mut self, tree: &ClassTree, type_table: &TypeTable) {
        self.type_roots.clear();
        for idx in 0..type_table.len() {
            let ty = TypeId::new(idx as u32);
            self.type_roots
                .insert(ty, self.resolve_overlay_type_root(tree, type_table, ty));
        }
    }

    fn resolve_overlay_type_root(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        mut ty: TypeId,
    ) -> TypeId {
        const MAX_DEPTH: usize = 16;
        for _ in 0..MAX_DEPTH {
            let Some(next) =
                self.next_overlay_type_root_step(tree, type_table, ty, &self.type_ids_by_def_id)
            else {
                return ty;
            };
            if next.is_unknown() || next == ty {
                return ty;
            }
            ty = next;
        }
        ty
    }

    fn next_overlay_type_root_step(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        ty: TypeId,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> Option<TypeId> {
        match type_table.get(ty) {
            Some(Type::Alias(alias)) => Some(alias.aliased),
            Some(Type::Class(class_ty))
                if class_ty.kind == ClassKind::Connector
                    || class_ty.kind == ClassKind::Type
                    || class_ty.kind == ClassKind::Operator
                    || class_ty.kind == ClassKind::Record =>
            {
                let class = tree.get_class_by_def_id(class_ty.def_id)?;
                let is_wrapper = if class_ty.kind == ClassKind::Connector {
                    Self::is_connector_alias_wrapper(class)
                } else {
                    Self::is_class_alias_wrapper(class)
                };
                if !is_wrapper {
                    return None;
                }
                Some(Self::resolve_alias_target_type_id(
                    class,
                    type_table,
                    type_ids_by_def_id,
                ))
            }
            _ => None,
        }
    }

    fn is_connector_alias_wrapper(class: &ClassDef) -> bool {
        matches!(class.class_type, rumoca_ir_ast::ClassType::Connector)
            && class.extends.len() == 1
            && class.classes.is_empty()
            && class.components.is_empty()
            && class.equations.is_empty()
            && class.initial_equations.is_empty()
            && class.algorithms.is_empty()
            && class.initial_algorithms.is_empty()
            && class.enum_literals.is_empty()
    }

    fn is_class_alias_wrapper(class: &ClassDef) -> bool {
        class.extends.len() == 1
            && class.classes.is_empty()
            && class.components.is_empty()
            && class.equations.is_empty()
            && class.initial_equations.is_empty()
            && class.algorithms.is_empty()
            && class.initial_algorithms.is_empty()
    }

    /// Collect enumeration type sizes from the class tree (MLS §10.5).
    ///
    /// Scans the class tree for enumeration type definitions and populates
    /// the eval context's `enum_sizes` map with type name → literal count mappings.
    /// Also resolves import aliases so that `import L = ...Logic` makes `L` usable
    /// as a dimension.
    fn collect_enum_sizes(tree: &ClassTree, ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext) {
        // Scan all classes in the name_map for enumeration types
        for (name, &def_id) in &tree.name_map {
            let size = Self::enum_literal_count(tree, def_id);
            if size == 0 {
                continue;
            }
            ctx.add_enum_size(name.clone(), size);
            // Also add short name (last segment after dot)
            let short = top_level_last_segment(name);
            ctx.add_enum_size_if_absent(short, size);
            // Populate enum ordinals (MLS §4.9.5: ordinal is 1-based position)
            Self::collect_enum_ordinals(tree, def_id, name, ctx);
        }

        // Scan all scope imports for aliases that resolve to enum types
        for idx in 0..tree.scope_tree.len() {
            let scope_id = ScopeId::new(idx as u32);
            let Some(scope) = tree.scope_tree.get(scope_id) else {
                continue;
            };
            for import in &scope.imports {
                Self::collect_enum_from_import(tree, import, ctx);
            }
        }
    }

    /// Return the number of enum literals for a class, or 0 if not an enum type.
    fn enum_literal_count(tree: &ClassTree, def_id: rumoca_core::DefId) -> usize {
        tree.get_class_by_def_id(def_id)
            .map(|c| c.enum_literals.len())
            .unwrap_or(0)
    }

    /// Populate enum ordinals for all literals of an enumeration type.
    ///
    /// For `type Logic = enumeration('U', 'X', ...)`, this adds:
    ///
    /// - `"Modelica.Electrical.Digital.Interfaces.Logic.'U'"` → 1
    /// - `"Modelica.Electrical.Digital.Interfaces.Logic.'X'"` → 2
    ///
    /// Also adds short forms without the full prefix.
    fn collect_enum_ordinals(
        tree: &ClassTree,
        def_id: rumoca_core::DefId,
        type_name: &str,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            return;
        };
        for (i, literal) in class.enum_literals.iter().enumerate() {
            let ordinal = (i + 1) as i64; // 1-based per MLS §4.9.5
            let lit_name = &*literal.ident.text;
            // Full qualified: "TypeName.LiteralName"
            let full = format!("{}.{}", type_name, lit_name);
            ctx.add_enum_ordinal(full, ordinal);
            // Short form: just the literal name (for unqualified references)
            // Only insert if not already present (avoid conflicts)
            ctx.add_enum_ordinal_if_absent(lit_name, ordinal);
        }
    }

    /// Check if an import resolves to an enumeration type and add it to enum_sizes.
    fn collect_enum_from_import(
        tree: &ClassTree,
        import: &ScopeImport,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        // Extract (alias_name, def_id) pairs from the import.
        // For renamed/qualified imports, include both import alias and full path
        // so strict structural lookups can resolve either spelling.
        let pairs: Vec<(String, rumoca_core::DefId)> = match import {
            ScopeImport::Renamed { .. } | ScopeImport::Qualified { .. } => {
                Self::import_constant_prefixes(import)
            }
            ScopeImport::Unqualified { names, .. } => {
                names.iter().map(|(n, &d)| (n.clone(), d)).collect()
            }
        };
        for (name, def_id) in pairs {
            let size = Self::enum_literal_count(tree, def_id);
            if size > 0 {
                ctx.add_enum_size_if_absent(name.clone(), size);
                // Also populate ordinals for import aliases
                Self::collect_enum_ordinals(tree, def_id, &name, ctx);
            }
        }
    }

    /// Collect integer/real/boolean constants from classes referenced by import aliases.
    ///
    /// When a class has `import generator = Modelica.Math.Random.Generators.Xorshift128plus`,
    /// this adds `generator.nState = 4` to the eval context so that dimension expressions
    /// like `Integer state[generator.nState]` can be evaluated.
    fn collect_import_constants(
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
    fn collect_model_extends_redeclare_constants(
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

    fn clear_alias_scope_values(
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

        let Expression::Modification { target, value } = &ext_mod.expr else {
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
    fn extract_override_class_constants(
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
    fn import_constant_prefixes(import: &ScopeImport) -> Vec<(String, rumoca_core::DefId)> {
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
                push_unique(alias.clone(), *def_id);
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

    /// Walk an extends-modification expression and record scalar/dimension overrides.
    fn extract_extends_modification_expr(
        alias: &str,
        expr: &Expression,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if let Expression::Modification { target, value } = expr {
            let target_name = target.to_string();
            let full_name = if alias.is_empty() {
                target_name
            } else {
                format!("{alias}.{}", target)
            };

            if let Some(val) = rumoca_eval_ast::eval::eval_integer_with_scope(value, ctx, alias) {
                ctx.add_integer(&full_name, val);
            }
            if let Some(val) = rumoca_eval_ast::eval::eval_boolean_with_scope(value, ctx, alias) {
                ctx.add_boolean(&full_name, val);
            }
            if let Some(dims) =
                rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(value, ctx, alias)
            {
                ctx.add_dimensions(full_name, dims);
            }
        }
    }

    /// Apply direct extends-modifier constant overrides from one ancestor class.
    fn extract_ancestor_extends_modification_constants(
        ancestor: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for ext in &ancestor.extends {
            Self::extract_extends_modification_constants("", ext, ctx);
        }
    }

    /// Extract constant integer/real/boolean values and array dimensions from a class definition.
    /// MLS §4.5: Constants have values determined at compile time.
    fn extract_class_constants(
        prefix: &str,
        class: &ClassDef,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        for (name, comp) in &class.components {
            if !matches!(comp.variability, rumoca_ir_core::Variability::Constant(_)) {
                continue;
            }
            let full_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            let type_name = comp.type_name.to_string();
            let binding = comp
                .binding
                .as_ref()
                .or((!matches!(comp.start, Expression::Empty)).then_some(&comp.start));
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
            ctx.add_dimensions(full_name, shape.to_vec());
            return;
        }
        // Try to infer dimensions from the binding expression
        if let Some(dims) =
            rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(binding, ctx, scope)
        {
            ctx.add_dimensions(full_name, dims);
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
                    ctx.add_integer_if_absent(full_name, val);
                }
            }
            "Real" => {
                if let Some(val) = rumoca_eval_ast::eval::eval_real_with_scope(expr, ctx, scope) {
                    ctx.add_real_if_absent(full_name, val);
                }
            }
            "Boolean" => {
                if let Some(val) = rumoca_eval_ast::eval::eval_boolean_with_scope(expr, ctx, scope)
                {
                    ctx.add_boolean_if_absent(full_name, val);
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
    fn collect_nested_class_constants(
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
    fn collect_enclosing_class_constants(
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

    /// Collect function definitions from the class tree for compile-time evaluation.
    ///
    /// Populates `ctx.functions` with function ClassDefs keyed by qualified name.
    /// Used by `eval_integer_func_with_scope` to interpret user-defined pure
    /// functions whose return values appear in dimension expressions (MLS §12.4).
    fn collect_function_defs(
        tree: &ClassTree,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        ctx.functions = std::sync::Arc::clone(tree.function_defs_for_eval());
    }

    /// Collect the enclosing class and all its ancestors via extends chains.
    ///
    /// Tracks resolution context so that relative extends names are resolved
    /// relative to the package where the parent class was found.
    fn collect_ancestor_classes<'a>(tree: &'a ClassTree, class_name: &str) -> Vec<&'a ClassDef> {
        let mut result = Vec::new();
        // Queue entries: (extends_name, resolution_context)
        let mut queue: Vec<(String, String)> =
            vec![(class_name.to_string(), class_name.to_string())];
        let mut visited = std::collections::HashSet::new();
        while let Some((name, context)) = queue.pop() {
            if !visited.insert(name.clone()) {
                continue;
            }
            let (class_def, resolved_qname) =
                Self::resolve_class_name_with_qname(tree, &name, &context);
            let Some(class_def) = class_def else { continue };
            let qname = resolved_qname.unwrap_or_else(|| name.clone());
            for ext in &class_def.extends {
                queue.push((ext.base_name.to_string(), qname.clone()));
            }
            result.push(class_def);
        }
        result
    }

    /// Resolve a potentially relative class name using scope-based lookup.
    /// Returns the class definition and the resolved qualified name.
    fn resolve_class_name_with_qname<'a>(
        tree: &'a ClassTree,
        name: &str,
        context: &str,
    ) -> (Option<&'a ClassDef>, Option<String>) {
        // Try fully qualified first
        if let Some(cls) = tree.get_class_by_qualified_name(name) {
            return (Some(cls), Some(name.to_string()));
        }
        // Try prepending scope prefixes from context
        let mut scope = context;
        while let Some(parent_scope) = parent_scope(scope) {
            scope = parent_scope;
            let qualified = format!("{}.{}", scope, name);
            if let Some(cls) = tree.get_class_by_qualified_name(&qualified) {
                return (Some(cls), Some(qualified));
            }
        }
        (None, None)
    }

    /// Multi-pass extraction of constants from ancestor classes (MLS §4.5, §7.1).
    fn extract_enclosing_constants_multi_pass(
        ancestors: &[&ClassDef],
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        const MAX_PASSES: usize = 5;
        for _pass in 0..MAX_PASSES {
            let prev = ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len();
            for ancestor in ancestors {
                Self::extract_ancestor_extends_modification_constants(ancestor, ctx);
                Self::extract_class_constants("", ancestor, ctx);
            }
            let new = ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len();
            if new == prev {
                break;
            }
        }
    }

    /// Multi-pass dimension evaluation for all dimension types (MLS §10.1).
    ///
    /// Iterates until no progress is made, handling dependencies between:
    /// - Colon dimensions inferred from bindings (e.g., `a[:] = {1,2,3}`)
    /// - Explicit dimensions evaluated from expressions (e.g., `x[size(a,1)-1]`)
    /// - Integer parameters computed from array sizes (e.g., `n = size(a,1)`)
    /// - Boolean/real parameters enabling if-expression evaluation
    ///
    /// Each pass:
    /// 1. Evaluates explicit (non-colon) dimension expressions
    /// 2. Infers colon dimensions from bindings (array literals, function calls)
    /// 3. Re-evaluates integer parameters that may now be computable
    /// 4. Re-evaluates boolean and real parameters
    fn evaluate_all_dimensions_multi_pass(
        &mut self,
        overlay: &mut InstanceOverlay,
        record_aliases: &[(String, String)],
    ) {
        const MAX_INFERENCE_PASSES: usize = 10;
        // Build once up front, then rebuild only after a pass makes progress.
        self.eval_ctx.build_suffix_index();

        for _pass in 0..MAX_INFERENCE_PASSES {
            let alias_progress = self.propagate_record_alias_values(record_aliases);

            // Pass 1: Try to evaluate explicit (non-colon) dimension expressions
            let explicit_progress = self.evaluate_explicit_dimensions_pass(overlay);

            // Pass 2: Infer colon dimensions from bindings
            let colon_progress = self.infer_colon_dimensions_single_pass(overlay);

            // Pass 3: Re-evaluate integer parameters that may now be computable
            // This handles cases like `n = size(table, 1)` after table dims are known
            let int_progress = self.reevaluate_integer_parameters(overlay);

            // Pass 4: Re-evaluate boolean and real parameters that may now be computable
            // This enables if-expression evaluation for dimension inference
            let bool_real_progress = self.reevaluate_boolean_and_real_parameters(overlay);

            let made_progress = alias_progress
                || explicit_progress
                || colon_progress
                || int_progress
                || bool_real_progress;
            if !made_progress {
                break;
            }

            self.eval_ctx.build_suffix_index();
        }
    }

    /// Collect record-parameter aliases from overlay bindings (MLS §7.2.3).
    ///
    /// Example: for `chain(cellData = cellData2)`, collect
    /// `chain.cellData -> cellData2` so field references like
    /// `chain.cellData.nRC` can resolve through the modifier binding.
    fn collect_record_aliases(overlay: &InstanceOverlay) -> Vec<(String, String)> {
        let mut aliases: HashMap<String, String> = HashMap::new();
        let (known_paths, known_prefixes) = Self::collect_known_component_paths(overlay);
        for instance_data in overlay.components.values() {
            let Some(binding) = &instance_data.binding else {
                continue;
            };
            let Some(target_unqualified) = Self::extract_simple_path(binding) else {
                continue;
            };
            let source = instance_data.qualified_name.to_flat_string();
            let target = Self::resolve_alias_target(
                &source,
                &target_unqualified,
                instance_data.binding_from_modification,
                &known_paths,
                &known_prefixes,
            );
            if source != target {
                aliases.insert(source, target);
            }
        }
        Self::collapse_alias_chains(&mut aliases);
        let mut out: Vec<(String, String)> = aliases.into_iter().collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    /// Extract a simple dotted path from an expression if it is a plain reference.
    fn extract_simple_path(expr: &Expression) -> Option<String> {
        match expr {
            Expression::ComponentReference(cr) => (!cr.parts.is_empty()).then(|| cr.to_string()),
            Expression::FieldAccess { base, field } => {
                let base_path = Self::extract_simple_path(base)?;
                Some(format!("{base_path}.{field}"))
            }
            _ => None,
        }
    }

    /// Collect full component paths and all dotted prefixes from the overlay.
    fn collect_known_component_paths(
        overlay: &InstanceOverlay,
    ) -> (HashSet<String>, HashSet<String>) {
        let mut known_paths: HashSet<String> = HashSet::new();
        let mut known_prefixes: HashSet<String> = HashSet::new();
        for instance_data in overlay.components.values() {
            let full = instance_data.qualified_name.to_flat_string();
            known_paths.insert(full.clone());
            let mut scope = full.as_str();
            while let Some((prefix, _)) = scope.rsplit_once('.') {
                known_prefixes.insert(prefix.to_string());
                scope = prefix;
            }
        }
        (known_paths, known_prefixes)
    }

    /// Resolve modifier/declaration alias targets into absolute instance scope.
    ///
    /// Applies lexical scope traversal and picks the first candidate that maps
    /// to an existing overlay path or a known parent prefix.
    fn resolve_alias_target(
        source: &str,
        target_unqualified: &str,
        from_modification: bool,
        known_paths: &HashSet<String>,
        known_prefixes: &HashSet<String>,
    ) -> String {
        let mut scope = if from_modification {
            Self::grandparent_scope(source)
        } else {
            Self::parent_scope(source)
        };
        loop {
            let candidate = if scope.is_empty() {
                target_unqualified.to_string()
            } else {
                format!("{scope}.{target_unqualified}")
            };
            if Self::is_known_component_or_prefix(&candidate, known_paths, known_prefixes) {
                return candidate;
            }
            if scope.is_empty() {
                break;
            }
            scope = Self::parent_scope(scope);
        }
        target_unqualified.to_string()
    }

    fn is_known_component_or_prefix(
        candidate: &str,
        known_paths: &HashSet<String>,
        known_prefixes: &HashSet<String>,
    ) -> bool {
        known_paths.contains(candidate) || known_prefixes.contains(candidate)
    }

    fn parent_scope(path: &str) -> &str {
        path.rsplit_once('.').map_or("", |(prefix, _)| prefix)
    }

    fn grandparent_scope(path: &str) -> &str {
        Self::parent_scope(Self::parent_scope(path))
    }

    /// Collapse `A->B->C` alias chains to direct `A->C` mappings.
    fn collapse_alias_chains(aliases: &mut HashMap<String, String>) {
        const MAX_PASSES: usize = 20;
        for _ in 0..MAX_PASSES {
            let mut changed = false;
            let keys: Vec<String> = aliases.keys().cloned().collect();
            for key in keys {
                changed |= Self::collapse_alias_for_key(aliases, &key);
            }
            if !changed {
                break;
            }
        }
    }

    fn collapse_alias_for_key(aliases: &mut HashMap<String, String>, key: &str) -> bool {
        let Some(target) = Self::resolve_alias_terminal(aliases, key) else {
            return false;
        };
        if aliases.get(key) == Some(&target) {
            return false;
        }
        aliases.insert(key.to_string(), target);
        true
    }

    fn resolve_alias_terminal(aliases: &HashMap<String, String>, key: &str) -> Option<String> {
        let mut target = aliases.get(key)?.clone();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(target.clone()) {
            let Some(next) = aliases.get(&target).cloned() else {
                break;
            };
            target = next;
        }
        Some(target)
    }

    /// Propagate evaluated scalar/dimension values through record aliases.
    ///
    /// If `A -> B`, then `A.field` should mirror `B.field` for compile-time
    /// evaluation of dimension expressions and range bounds.
    fn propagate_record_alias_values(&mut self, record_aliases: &[(String, String)]) -> bool {
        Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.integers)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.reals)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.booleans)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.enums)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.dimensions)
    }

    fn propagate_alias_map<T: Clone + PartialEq>(
        record_aliases: &[(String, String)],
        values: &mut rustc_hash::FxHashMap<String, T>,
    ) -> bool {
        if record_aliases.is_empty() || values.is_empty() {
            return false;
        }
        let mut sorted_keys: Vec<String> = values.keys().cloned().collect();
        sorted_keys.sort_unstable();

        let mut updates: rustc_hash::FxHashMap<String, T> = rustc_hash::FxHashMap::default();
        for (alias_source, alias_target) in record_aliases {
            Self::queue_alias_root_update(alias_source, alias_target, values, &mut updates);
            let target_prefix = format!("{alias_target}.");
            for field_name in Self::alias_field_key_range(&sorted_keys, &target_prefix) {
                Self::queue_alias_field_update(
                    alias_source,
                    &target_prefix,
                    field_name,
                    values,
                    &mut updates,
                );
            }
        }

        let mut progress = false;
        for (name, value) in updates {
            if values.get(&name) != Some(&value) {
                values.insert(name, value);
                progress = true;
            }
        }
        progress
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
