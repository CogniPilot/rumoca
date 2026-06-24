//! Type checking phase for the Rumoca compiler.
//!
//! SPEC_0021 file-size exception: this facade still owns late type inference,
//! modifier validation, and scoped instance assembly. split plan: move those
//! concerns behind focused modules as follow-up compiler hardening work.
//!
//! This phase has two entry points. `typecheck` walks a resolved `ClassTree`
//! and returns a `TypedTree`. The production compiled-model pipeline uses
//! `typecheck_instanced` after instantiation so modifier-dependent dimensions
//! and structural parameters are available in the instance overlay.
//!
//! Type checking:
//! 1. Resolves type specifiers to TypeIds
//! 2. Populates the type_id fields on components
//! 3. **Evaluates dimension expressions** (MLS §10.1)
//! 4. **Marks structural parameters** (MLS §18.3)
//! 5. **Infers array dimensions from bindings**
//! 6. Performs type checking on expressions
//! 7. Validates type constraints (variability, causality, etc.)
//!
//! The standalone API input is a `ResolvedTree` and the output is a
//! `TypedTree`. The production API input is a resolved `ClassTree` plus an
//! `InstanceOverlay`, and it annotates the overlay in place before flattening.
//!
//! ## Dimension Evaluation (MLS §10.1)
//!
//! Production model dimensions are evaluated after instantiation, not during
//! flattening. This ensures:
//! - Modifier-dependent dimensions use final instantiated values
//! - Structural parameters are identified before Flat is produced
//! - Array sizes for for-loops can be computed at compile time

mod constant_collection;
mod enum_context;
mod instanced;
mod modifier_targets;
mod path_utils;
mod typechecker;
pub mod unit_syntax;

use rumoca_core::{ComponentPath, DefId, ScopeId, Span, TypeId};
use rumoca_core::{
    Diagnostic as CommonDiagnostic, Diagnostics, PhaseError, PrimaryLabel, SourceMap,
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
#[derive(Debug, Clone, Error)]
pub enum TypeCheckError {
    /// A type was referenced but not found.
    #[error("undefined type: `{name}` not found")]
    UndefinedType { name: String, span: Span },

    /// A type mismatch in an expression or equation.
    #[error("type mismatch: expected `{expected}`, found `{found}`")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    /// Invalid variability constraint.
    #[error("variability error: {message}")]
    VariabilityError { message: String, span: Span },

    /// Array dimensions could not be evaluated.
    #[error("unevaluable array dimensions for '{name}': {reason}")]
    UnevaluableDimensions { name: String, reason: String },

    /// Required source provenance was missing from type-check metadata.
    #[error("missing source context: {reason}")]
    MissingSourceContext { reason: String },

    /// Phase-local diagnostic emitted during recoverable type checking.
    #[error("{message}")]
    PhaseDiagnostic {
        code: String,
        message: String,
        label: String,
        span: Span,
        note: Option<String>,
    },
}

impl TypeCheckError {
    /// Create an UndefinedType error.
    pub fn undefined_type(name: impl Into<String>, span: Span) -> Self {
        Self::UndefinedType {
            name: name.into(),
            span,
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
            span,
        }
    }

    /// Create a VariabilityError.
    pub fn variability_error(message: impl Into<String>, span: Span) -> Self {
        Self::VariabilityError {
            message: message.into(),
            span,
        }
    }

    /// Create an UnevaluableDimensions error.
    pub fn unevaluable_dimensions(name: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::UnevaluableDimensions {
            name: name.into(),
            reason: reason.into(),
        }
    }

    pub fn missing_source_context(reason: impl Into<String>) -> Self {
        Self::MissingSourceContext {
            reason: reason.into(),
        }
    }

    pub fn phase_diagnostic(
        code: impl Into<String>,
        message: impl Into<String>,
        label: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::PhaseDiagnostic {
            code: code.into(),
            message: message.into(),
            label: label.into(),
            span,
            note: None,
        }
    }

    pub fn with_note(self, note: impl Into<String>) -> Self {
        match self {
            Self::PhaseDiagnostic {
                code,
                message,
                label,
                span,
                ..
            } => Self::PhaseDiagnostic {
                code,
                message,
                label,
                span,
                note: Some(note.into()),
            },
            other => other,
        }
    }
}

impl PhaseError for TypeCheckError {
    fn to_diagnostic(&self) -> CommonDiagnostic {
        match self {
            Self::UndefinedType { name, span } => CommonDiagnostic::error(
                "ET001",
                format!("undefined type: `{name}` not found"),
                PrimaryLabel::new(*span).with_message("type not found"),
            )
            .with_note("check that the type name is spelled correctly"),
            Self::TypeMismatch {
                expected,
                found,
                span,
            } => CommonDiagnostic::error(
                "ET002",
                format!("type mismatch: expected `{expected}`, found `{found}`"),
                PrimaryLabel::new(*span).with_message("type mismatch here"),
            )
            .with_note("MLS §4: types must be compatible for this operation"),
            Self::VariabilityError { message, span } => CommonDiagnostic::error(
                "ET003",
                format!("variability error: {message}"),
                PrimaryLabel::new(*span).with_message("variability error here"),
            )
            .with_note("MLS §4.5: variability must be respected in assignments"),
            Self::UnevaluableDimensions { name, reason } => CommonDiagnostic::global_error(
                "ET004",
                format!("unevaluable array dimensions for '{name}': {reason}"),
            )
            .with_note(
                "MLS §10.1: array dimensions must be parameter expressions evaluable at translation time",
            ),
            Self::MissingSourceContext { reason } => CommonDiagnostic::global_error(
                "ET000",
                format!("missing source context: {reason}"),
            )
            .with_note(
                "internal type-check metadata must preserve source provenance for diagnostics",
            ),
            Self::PhaseDiagnostic {
                code,
                message,
                label,
                span,
                note,
            } => {
                let diagnostic = CommonDiagnostic::error(
                    code.clone(),
                    message.clone(),
                    PrimaryLabel::new(*span).with_message(label.clone()),
                );
                if let Some(note) = note {
                    diagnostic.with_note(note.clone())
                } else {
                    diagnostic
                }
            }
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
    /// Component shapes visible in the current class scope.
    ///
    /// `Some(shape)` is a known shape (empty = scalar); `None` means the
    /// component declares dimensions that are not evaluated here. Per
    /// SPEC_0008, an absent or `None` entry must be treated as unknown,
    /// never as scalar.
    current_component_shapes: HashMap<String, Option<Vec<usize>>>,
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
    /// DefIds of connector classes declared with `expandable` (MLS §9.1.3).
    ///
    /// Expandable connector members are inferred from connect-equations and
    /// component references, so missing members are not ordinary static
    /// member errors.
    expandable_connector_defs: HashSet<DefId>,
    /// Expandable connector member references visible in the current class.
    ///
    /// This is collected before validating connect equations so equation order
    /// does not decide whether a dynamic bus member is considered synthesized.
    expandable_member_surfaces: HashSet<String>,
    /// Type aliases whose targets could not be resolved during type-table
    /// construction (e.g. an MSL alias into a library that is not loaded).
    ///
    /// The error is deferred and surfaced only when the alias is actually
    /// used by the model being checked, so unrelated broken library classes
    /// cannot fail every compile in the session (strict-reachable semantics).
    deferred_alias_errors: HashMap<TypeId, (String, Span)>,
}

struct ClassOverrideAliasContext<'a> {
    tree: &'a ClassTree,
    component_index: &'a HashMap<String, &'a rumoca_ir_ast::InstanceData>,
    comp_scope: &'a str,
    active_alias: Option<&'a str>,
    eval_ctx: &'a mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    cleared_alias_scopes: &'a mut HashSet<String>,
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
            current_component_shapes: HashMap::new(),
            component_modifier_targets: HashMap::new(),
            component_modifier_member_types: HashMap::new(),
            expandable_connector_defs: HashSet::new(),
            expandable_member_surfaces: HashSet::new(),
            deferred_alias_errors: HashMap::new(),
        }
    }

    pub(crate) fn emit_typecheck_error(&mut self, error: TypeCheckError) {
        self.diagnostics.emit(error.to_diagnostic());
    }

    pub(crate) fn diagnostic_location_span(
        &mut self,
        location: &rumoca_core::Location,
        context: &str,
    ) -> Option<Span> {
        match self.source_map.try_location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        ) {
            Some(span) => Some(span),
            None => {
                self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                    "source file `{}` for {context} was not found",
                    location.file_name
                )));
                None
            }
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
        self.expandable_connector_defs = Self::collect_expandable_connector_defs(tree);
        let (type_table, type_ids_by_def_id) = match self.build_type_context(tree) {
            Ok(context) => context,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return;
            }
        };
        tree.type_table = type_table;
        self.type_ids_by_def_id = type_ids_by_def_id;
        self.type_suffix_index = Self::build_type_suffix_index(&tree.type_table);
        self.rebuild_type_roots(tree, &tree.type_table);
        self.component_modifier_targets = modifier_targets::build_component_modifier_targets(tree);
        self.component_modifier_member_types =
            match modifier_targets::build_component_modifier_member_types(
                tree,
                &tree.type_table,
                &self.type_ids_by_def_id,
                &self.type_suffix_index,
                &self.source_map,
            ) {
                Ok(member_types) => member_types,
                Err(error) => {
                    self.emit_typecheck_error(*error);
                    return;
                }
            };
        self.check_stored_definition(&mut tree.definitions, &mut tree.type_table);
        self.flush_eval_warnings();
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
        let mut cleared_alias_scopes = HashSet::<String>::new();
        for _ in 0..MAX_PASSES {
            let prev =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();

            for data in overlay.components.values() {
                Self::apply_instance_class_overrides(
                    tree,
                    &component_index,
                    data,
                    ctx,
                    &mut cleared_alias_scopes,
                );
            }

            let new =
                ctx.integers.len() + ctx.dimensions.len() + ctx.reals.len() + ctx.booleans.len();
            if new == prev {
                break;
            }
        }
    }

    fn flush_eval_warnings(&mut self) {
        for diagnostic in self.eval_ctx.take_warnings() {
            self.diagnostics.emit(diagnostic);
        }
    }

    fn collect_expandable_connector_defs(tree: &ClassTree) -> HashSet<DefId> {
        tree.name_map
            .values()
            .filter_map(|def_id| tree.get_class_by_def_id(*def_id))
            .filter(|class| {
                class.expandable && matches!(class.class_type, rumoca_core::ClassType::Connector)
            })
            .filter_map(|class| class.def_id)
            .collect()
    }

    fn apply_instance_class_overrides(
        tree: &ClassTree,
        component_index: &HashMap<String, &rumoca_ir_ast::InstanceData>,
        data: &rumoca_ir_ast::InstanceData,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
        cleared_alias_scopes: &mut HashSet<String>,
    ) {
        if data.class_overrides.is_empty() {
            return;
        }
        let comp_scope = data.qualified_name.to_flat_string();
        if comp_scope.is_empty() {
            return;
        }

        let active_alias = Self::component_active_alias(data);
        for class_override in data.class_overrides.values() {
            let mut alias_ctx = ClassOverrideAliasContext {
                tree,
                component_index,
                comp_scope: &comp_scope,
                active_alias: active_alias.as_deref(),
                eval_ctx: ctx,
                cleared_alias_scopes,
            };
            Self::apply_class_override_alias(
                &mut alias_ctx,
                &class_override.alias,
                class_override.target_def_id,
            );
        }
    }

    fn apply_class_override_alias(
        alias_ctx: &mut ClassOverrideAliasContext<'_>,
        alias: &str,
        def_id: DefId,
    ) {
        if Self::try_apply_forwarded_parent_alias_constants(
            alias_ctx.tree,
            alias_ctx.component_index,
            alias_ctx.comp_scope,
            alias_ctx.active_alias,
            alias,
            def_id,
            alias_ctx.eval_ctx,
        ) {
            return;
        }

        let is_active_alias = alias_ctx.active_alias == Some(alias);

        let alias_scope = format!("{}.{alias}", alias_ctx.comp_scope);
        // MLS §7.3: instance-level redeclare overrides must replace inherited/default
        // package constants in the local alias scope.
        if alias_ctx.cleared_alias_scopes.insert(alias_scope.clone()) {
            Self::clear_alias_scope_values(alias_ctx.eval_ctx, &alias_scope);
        }
        Self::extract_override_class_constants(
            alias_ctx.tree,
            &alias_scope,
            def_id,
            alias_ctx.eval_ctx,
        );

        // For declarations like `Medium.BaseProperties medium`, expose
        // unqualified constants (`medium.nX`) from the active alias only.
        if is_active_alias {
            Self::extract_override_class_constants(
                alias_ctx.tree,
                alias_ctx.comp_scope,
                def_id,
                alias_ctx.eval_ctx,
            );
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
        if path_utils::class_name_leaf(def_qname) != alias {
            return false;
        }

        let enclosing = Self::enclosing_scope_or_root(comp_scope);
        if enclosing.is_empty() {
            return false;
        }
        let Some(parent_data) = component_index.get(enclosing) else {
            return false;
        };
        if Self::class_override_by_alias(parent_data, alias).is_none() {
            return false;
        }

        let source_alias = format!("{comp_scope}.{alias}");
        let target_alias = format!("{enclosing}.{alias}");
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
        if let Some(type_def_id) = data.type_def_id
            && let Some(class_override) = data.class_overrides.get(&type_def_id)
        {
            return Some(class_override.alias.clone());
        }

        if let Some((head, _tail)) = path_utils::class_root_split(&data.type_name)
            && Self::class_override_by_alias(data, head).is_some()
        {
            return Some(head.to_string());
        }

        if data.class_overrides.len() == 1 {
            return data
                .class_overrides
                .values()
                .next()
                .map(|class_override| class_override.alias.clone());
        }

        None
    }

    fn class_override_by_alias<'a>(
        data: &'a rumoca_ir_ast::InstanceData,
        alias: &str,
    ) -> Option<&'a rumoca_ir_ast::ClassOverride> {
        data.class_overrides
            .values()
            .find(|class_override| class_override.alias == alias)
    }

    /// Build lookup keys for top-level model components from instanced overlay data.
    fn build_instanced_component_type_scope(
        overlay: &InstanceOverlay,
        full_prefix: &str,
        short_model: &str,
    ) -> HashMap<String, TypeId> {
        let mut out = HashMap::new();
        let short_prefix = format!("{short_model}.");

        for data in overlay.components.values() {
            let qn = data.qualified_name.to_flat_string();
            let canonical_type = overlay
                .type_roots
                .get(&data.type_id)
                .copied()
                .unwrap_or(data.type_id);
            out.insert(qn.clone(), canonical_type);
            if let Some(rest) = qn.strip_prefix(full_prefix) {
                Self::insert_instanced_aliases(&mut out, rest, canonical_type, Some(short_model));
                continue;
            }
            if let Some(rest) = qn.strip_prefix(&short_prefix) {
                Self::insert_instanced_aliases(&mut out, rest, canonical_type, None);
                continue;
            }
            if !path_utils::is_qualified_class_name(&qn) {
                out.insert(qn, canonical_type);
            }
        }

        out
    }

    /// Shape map keyed like `build_instanced_component_type_scope`. The model
    /// prefixes are passed in so the textual-path handling stays in one place.
    fn build_instanced_component_shape_scope(
        overlay: &InstanceOverlay,
        full_prefix: &str,
        short_model: &str,
    ) -> HashMap<String, Option<Vec<usize>>> {
        let mut out = HashMap::new();
        let short_prefix = format!("{short_model}.");
        for data in overlay.components.values() {
            let shape = if !data.dims.is_empty() {
                Some(data.dims.iter().map(|&d| d as usize).collect::<Vec<_>>())
            } else if data.dims_expr.is_empty() {
                Some(Vec::new())
            } else {
                None
            };
            let qn = data.qualified_name.to_flat_string();
            if let Some(rest) = qn.strip_prefix(full_prefix) {
                out.insert(rest.to_string(), shape.clone());
            } else if let Some(rest) = qn.strip_prefix(&short_prefix) {
                out.insert(rest.to_string(), shape.clone());
            }
            out.insert(qn, shape);
        }
        out
    }

    /// Model-name prefix and short model name shared by the instanced scope
    /// builders, so the textual-path handling happens exactly once.
    fn instanced_scope_prefixes(model_name: &str) -> (String, String) {
        let full_prefix = format!("{model_name}.");
        let short_model = path_utils::class_name_leaf(model_name).to_string();
        (full_prefix, short_model)
    }

    fn insert_instanced_aliases(
        out: &mut HashMap<String, TypeId>,
        rest: &str,
        type_id: TypeId,
        short_model: Option<&str>,
    ) {
        if path_utils::is_qualified_class_name(rest) {
            return;
        }
        out.insert(rest.to_string(), type_id);
        if let Some(short_model) = short_model {
            out.insert(format!("{short_model}.{rest}"), type_id);
        }
    }

    /// Build a type context that includes user-defined classes, enums, and aliases.
    fn build_type_context(
        &mut self,
        tree: &ClassTree,
    ) -> TypeCheckResult<(TypeTable, HashMap<DefId, TypeId>)> {
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

            if matches!(class.class_type, rumoca_core::ClassType::Type) {
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
            if !matches!(class.class_type, rumoca_core::ClassType::Type)
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
            if !matches!(class.class_type, rumoca_core::ClassType::Type)
                || !class.enum_literals.is_empty()
            {
                continue;
            }

            let Some(&alias_id) = type_ids_by_def_id.get(&def_id) else {
                continue;
            };
            let Some(aliased) = self.resolve_alias_target_or_defer(
                alias_id,
                class,
                &type_table,
                &type_ids_by_def_id,
            )?
            else {
                continue;
            };
            if let Some(Type::Alias(alias)) = type_table.get_mut(alias_id) {
                alias.aliased = aliased;
            }
        }

        Ok((type_table, type_ids_by_def_id))
    }

    /// Resolve an alias target, deferring `UndefinedType` failures.
    ///
    /// An unresolvable alias target anywhere in the tree (an MSL alias into a
    /// library that is not loaded) must not fail every model in the session.
    /// The error is recorded per alias `TypeId` and surfaced when a model's
    /// overlay actually resolves a component to the alias; `Ok(None)` means
    /// the alias keeps its `UNKNOWN` target. Other errors still propagate.
    fn resolve_alias_target_or_defer(
        &mut self,
        alias_id: TypeId,
        class: &ClassDef,
        type_table: &TypeTable,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> TypeCheckResult<Option<TypeId>> {
        match self.resolve_alias_target_type_id(class, type_table, type_ids_by_def_id) {
            Ok(aliased) => Ok(Some(aliased)),
            Err(error) => match error.as_ref() {
                TypeCheckError::UndefinedType { name, span } => {
                    self.deferred_alias_errors
                        .insert(alias_id, (name.clone(), *span));
                    Ok(None)
                }
                _ => Err(error),
            },
        }
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
        class_type: &rumoca_core::ClassType,
    ) -> TypeId {
        if let Some(existing) = type_table.lookup(qualified_name) {
            return existing;
        }

        let kind = match class_type {
            rumoca_core::ClassType::Class => ClassKind::Class,
            rumoca_core::ClassType::Model => ClassKind::Model,
            rumoca_core::ClassType::Block => ClassKind::Block,
            rumoca_core::ClassType::Record => ClassKind::Record,
            rumoca_core::ClassType::Connector => ClassKind::Connector,
            rumoca_core::ClassType::Type => ClassKind::Type,
            rumoca_core::ClassType::Package => ClassKind::Package,
            rumoca_core::ClassType::Function => ClassKind::Function,
            rumoca_core::ClassType::Operator => ClassKind::Operator,
        };

        type_table.add_type(Type::Class(TypeClassType {
            name: qualified_name.to_string(),
            def_id,
            kind,
        }))
    }

    fn resolve_alias_target_type_id(
        &self,
        class: &ClassDef,
        type_table: &TypeTable,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> TypeCheckResult<TypeId> {
        let Some(ext) = class.extends.first() else {
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET001",
                format!(
                    "type alias `{}` does not extend a base type",
                    class.name.text
                ),
                "type alias declaration here",
                self.location_span(&class.location)?,
            )));
        };

        if let Some(base_def_id) = ext.base_def_id
            && let Some(&target) = type_ids_by_def_id.get(&base_def_id)
        {
            return Ok(target);
        }

        let base_name = ext.base_name.to_string();
        let base_span = self.name_span(&ext.base_name)?;
        Self::try_resolve_alias_target_type_id(class, type_table, type_ids_by_def_id)
            .ok_or_else(|| Box::new(TypeCheckError::undefined_type(base_name, base_span)))
    }

    fn try_resolve_alias_target_type_id(
        class: &ClassDef,
        type_table: &TypeTable,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> Option<TypeId> {
        let ext = class.extends.first()?;
        if let Some(base_def_id) = ext.base_def_id
            && let Some(&target) = type_ids_by_def_id.get(&base_def_id)
        {
            return Some(target);
        }

        let base_name = ext.base_name.to_string();
        type_table
            .lookup(&base_name)
            .or_else(|| type_table.lookup(path_utils::class_name_leaf(&base_name)))
    }

    fn name_span(&self, name: &rumoca_ir_ast::Name) -> TypeCheckResult<Span> {
        let Some(first) = name.name.first() else {
            return Err(Box::new(TypeCheckError::missing_source_context(
                "type alias target name has no source path segments",
            )));
        };
        let last = name.name.last().unwrap_or(first);
        let file_name = if !first.location.file_name.is_empty() {
            first.location.file_name.as_str()
        } else {
            last.location.file_name.as_str()
        };
        self.source_map
            .try_location_to_span(
                file_name,
                first.location.start as usize,
                last.location.end as usize,
            )
            .ok_or_else(|| {
                Box::new(TypeCheckError::missing_source_context(format!(
                    "source file `{file_name}` for type alias target name was not found"
                )))
            })
    }

    fn location_span(&self, location: &rumoca_core::Location) -> TypeCheckResult<Span> {
        self.source_map
            .try_location_to_span(
                &location.file_name,
                location.start as usize,
                location.end as usize,
            )
            .ok_or_else(|| {
                Box::new(TypeCheckError::missing_source_context(format!(
                    "source file `{}` for typecheck location was not found",
                    location.file_name
                )))
            })
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
            if let Some((missing, span)) = self.deferred_alias_errors.get(&resolved) {
                let error = TypeCheckError::undefined_type(missing.clone(), *span);
                self.emit_typecheck_error(error);
            }
            if !resolved.is_unknown() {
                data.type_id = resolved;
                continue;
            }

            let instance_name = data.qualified_name.to_flat_string();
            let span = match self
                .diagnostic_location_span(&data.source_location, "overlay component type")
            {
                Some(span) => span,
                None => {
                    data.type_id = resolved;
                    continue;
                }
            };
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET001",
                format!(
                    "undefined type '{}' for instance '{}'",
                    data.type_name, instance_name
                ),
                "type declaration here",
                span,
            ));
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
                Self::try_resolve_alias_target_type_id(class, type_table, type_ids_by_def_id)
            }
            _ => None,
        }
    }

    fn is_connector_alias_wrapper(class: &ClassDef) -> bool {
        matches!(class.class_type, rumoca_core::ClassType::Connector)
            && !class.expandable
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

    /// Collect function definitions from the class tree for compile-time evaluation.
    ///
    /// Populates `ctx.functions` with function ClassDefs keyed by qualified name.
    /// Used by `eval_integer_func_with_scope` to interpret user-defined pure
    /// functions whose return values appear in dimension expressions (MLS §12.4).
    fn collect_function_defs(
        tree: &ClassTree,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        ctx.functions = build_function_defs_for_eval(tree);
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
        // Try prepending the context class and its enclosing classes,
        // walked through the scope tree.
        for scope in std::iter::once(context).chain(tree.enclosing_class_names_of(context)) {
            let qualified = format!("{scope}.{name}");
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
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        record_aliases: &[(String, String)],
    ) {
        const MAX_INFERENCE_PASSES: usize = 10;
        for _pass in 0..MAX_INFERENCE_PASSES {
            let alias_progress = self.propagate_record_alias_values(record_aliases);

            // Pass 1: Try to evaluate explicit (non-colon) dimension expressions
            let explicit_progress = self.evaluate_explicit_dimensions_pass(overlay);

            // Pass 2: Infer colon dimensions from bindings
            let colon_progress = self.infer_colon_dimensions_single_pass(overlay);

            // Pass 3: Re-evaluate inherited extends modifier parameters that
            // may depend on dimensions inferred earlier in this pass.
            let extends_progress = Self::reevaluate_component_scoped_extends_modification_constants(
                tree,
                overlay,
                &mut self.eval_ctx,
            );

            // Pass 4: Re-evaluate integer parameters that may now be computable
            // This handles cases like `n = size(table, 1)` after table dims are known
            let int_progress = self.reevaluate_integer_parameters(overlay);

            // Pass 5: Re-evaluate boolean, real, and enum parameters that may now be computable
            // This enables if-expression evaluation for dimension inference
            let value_progress = self.reevaluate_boolean_real_and_enum_parameters(overlay);

            let made_progress = alias_progress
                || explicit_progress
                || colon_progress
                || extends_progress
                || int_progress
                || value_progress;
            if !made_progress {
                break;
            }
        }
    }
}

fn build_function_defs_for_eval(
    tree: &ClassTree,
) -> std::sync::Arc<rustc_hash::FxHashMap<String, ClassDef>> {
    let mut functions = rustc_hash::FxHashMap::default();
    for (name, &def_id) in &tree.name_map {
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            continue;
        };
        insert_function_def(&mut functions, name, class);
    }
    insert_import_function_aliases(tree, &mut functions);
    std::sync::Arc::new(functions)
}

fn insert_import_function_aliases(
    tree: &ClassTree,
    functions: &mut rustc_hash::FxHashMap<String, ClassDef>,
) {
    for idx in 0..tree.scope_tree.len() {
        let scope_id = ScopeId::new(idx as u32);
        let Some(scope) = tree.scope_tree.get(scope_id) else {
            continue;
        };
        for import in &scope.imports {
            insert_import_function_alias(tree, import, functions);
        }
    }
}

fn insert_import_function_alias(
    tree: &ClassTree,
    import: &ScopeImport,
    functions: &mut rustc_hash::FxHashMap<String, ClassDef>,
) {
    match import {
        ScopeImport::Renamed { .. } | ScopeImport::Qualified { .. } => {
            for (alias, def_id) in TypeChecker::import_constant_prefixes(import) {
                let Some(class) = tree.get_class_by_def_id(def_id) else {
                    continue;
                };
                insert_function_alias_tree(functions, &alias, class);
            }
        }
        ScopeImport::Unqualified { names, .. } => {
            for (alias, &def_id) in names {
                let Some(class) = tree.get_class_by_def_id(def_id) else {
                    continue;
                };
                insert_function_alias_tree(functions, alias.as_str(), class);
            }
        }
    }
}

fn insert_function_alias_tree(
    functions: &mut rustc_hash::FxHashMap<String, ClassDef>,
    prefix: &str,
    class: &ClassDef,
) {
    insert_function_def(functions, prefix, class);
    for (name, nested) in &class.classes {
        let nested_prefix = format!("{prefix}.{name}");
        insert_function_alias_tree(functions, &nested_prefix, nested);
    }
}

fn insert_function_def(
    functions: &mut rustc_hash::FxHashMap<String, ClassDef>,
    name: &str,
    class: &ClassDef,
) {
    if class.class_type == rumoca_core::ClassType::Function && !class.algorithms.is_empty() {
        functions
            .entry(name.to_string())
            .or_insert_with(|| class.clone());
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
