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
mod function_signatures;
mod instanced;
mod modifier_targets;
mod path_utils;
mod semantic_scope;
mod type_roots;
mod typechecker;
pub mod unit_syntax;

use rumoca_core::{ComponentPath, DefId, InstanceId, ScopeId, SourceId, Span, TypeId};
use rumoca_core::{
    Diagnostic as CommonDiagnostic, Diagnostics, PhaseError, PrimaryLabel, SourceMap,
};

/// Placeholder used when a `SourceId` has no registered name in the source map.
pub(crate) const UNKNOWN_SOURCE_DISPLAY_NAME: &str = "<unknown source>";
use rumoca_ir_ast::{
    ClassDef, ClassKind, ClassTree, Component, EnumerationType, Expression, ExpressionContext,
    InstanceOverlay, ScopeImport, StoredDefinition, Type, TypeAlias, TypeClassType, TypeTable,
    TypedTree, Visitor,
};
use rumoca_phase_resolve::ResolvedTree;
use semantic_scope::{
    ComponentSemantics, InstanceSemanticScope, SemanticLookup, invalid_subscript_owner,
    subscripts_required_value_violation,
};
use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
use thiserror::Error;
use typechecker::traversal_adapter::{
    walk_equation, walk_equations, walk_expression, walk_statement, walk_statements,
};

#[cfg(test)]
use typechecker::api::typecheck_instanced_test_projection;
pub use typechecker::api::{typecheck, typecheck_instanced};

/// Type alias for typecheck results with boxed errors.
///
/// Boxing the error type avoids clippy::result_large_err warnings while
/// preserving rich diagnostic information. The error path is cold (errors
/// are exceptional), so the allocation overhead is negligible.
pub type TypeCheckResult<T> = Result<T, Box<TypeCheckError>>;

/// Canonical test-only projection for fixtures that deliberately mutate a raw
/// ClassTree and therefore cannot carry Resolve's production success brand.
#[cfg(test)]
pub(crate) fn semantic_catalog_projection_for_test(
    tree: &ClassTree,
) -> Result<rumoca_ir_ast::SemanticCatalogProjection, String> {
    let predefined = |role: rumoca_core::ConnectionGraphOperatorRole| {
        let path = match role {
            rumoca_core::ConnectionGraphOperatorRole::Branch => role.predefined_path(),
            rumoca_core::ConnectionGraphOperatorRole::Root => role.predefined_path(),
            rumoca_core::ConnectionGraphOperatorRole::PotentialRoot => role.predefined_path(),
            rumoca_core::ConnectionGraphOperatorRole::IsRoot => role.predefined_path(),
            rumoca_core::ConnectionGraphOperatorRole::Rooted => role.predefined_path(),
        };
        tree.scope_tree
            .predefined_member(&ComponentPath::from_parts(path))
            .ok_or_else(|| format!("fixture is missing predefined {}.{}", path[0], path[1]))
    };
    let branch = predefined(rumoca_core::ConnectionGraphOperatorRole::Branch)?;
    let root = predefined(rumoca_core::ConnectionGraphOperatorRole::Root)?;
    let potential_root = predefined(rumoca_core::ConnectionGraphOperatorRole::PotentialRoot)?;
    let is_root = predefined(rumoca_core::ConnectionGraphOperatorRole::IsRoot)?;
    let rooted = predefined(rumoca_core::ConnectionGraphOperatorRole::Rooted)?;
    let connections =
        rumoca_ir_ast::ConnectionOperatorCatalog::from_resolve_registration(|role| match role {
            rumoca_core::ConnectionGraphOperatorRole::Branch => branch,
            rumoca_core::ConnectionGraphOperatorRole::Root => root,
            rumoca_core::ConnectionGraphOperatorRole::PotentialRoot => potential_root,
            rumoca_core::ConnectionGraphOperatorRole::IsRoot => is_root,
            rumoca_core::ConnectionGraphOperatorRole::Rooted => rooted,
        });
    let index = rumoca_ir_ast::ClassDefIndex::from_tree(tree);
    let mut external_objects = rumoca_ir_ast::ExternalObjectLifecycleCatalog::begin_resolve_check();
    for owner in index.def_ids() {
        let Some(lifecycle) = index
            .external_object_lifecycle(owner)
            .map_err(|error| format!("{error:?}"))?
        else {
            continue;
        };
        external_objects
            .insert_from_resolve_check(
                rumoca_ir_ast::ExternalObjectLifecycleIdentity::from_resolve_check(
                    lifecycle.owner_def_id(),
                    lifecycle.constructor_def_id(),
                    lifecycle.destructor_def_id(),
                ),
            )
            .map_err(|duplicate| {
                format!("fixture repeats ExternalObject owner identity {duplicate:?}")
            })?;
    }
    Ok(
        rumoca_ir_ast::SemanticCatalogProjection::from_resolve_issued(
            connections,
            external_objects,
        ),
    )
}

struct ResolvedTypeRootCatalog {
    roots: HashMap<TypeId, TypeId>,
    failures: HashMap<TypeId, type_roots::TypeRootResolutionError>,
    enumeration_roots: HashSet<TypeId>,
    declarations: Vec<(DefId, TypeId)>,
}

#[derive(Default)]
struct UsedFunctionCollector {
    declarations: HashMap<DefId, Span>,
    missing_target: Option<Span>,
}

impl Visitor for UsedFunctionCollector {
    fn visit_expression_ctx(
        &mut self,
        expression: &Expression,
        context: ExpressionContext,
    ) -> ControlFlow<()> {
        if matches!(
            context,
            ExpressionContext::ComponentAnnotation
                | ExpressionContext::ClassAnnotation
                | ExpressionContext::ExtendAnnotation
                | ExpressionContext::ExternalAnnotation
        ) {
            return ControlFlow::Continue(());
        }
        self.visit_expression(expression)
    }

    fn visit_expr_function_call(
        &mut self,
        call: &rumoca_ir_ast::ComponentReference,
        arguments: &[Expression],
    ) -> ControlFlow<()> {
        if let Some(declaration) = call.target_def_id() {
            self.declarations.entry(declaration).or_insert(call.span);
        } else if self.missing_target.is_none() {
            self.missing_target = Some(call.span);
        }
        self.visit_each(arguments, Self::visit_expression)
    }
}

fn format_type_root_failure(failure: Option<&type_roots::TypeRootResolutionError>) -> String {
    match failure {
        Some(type_roots::TypeRootResolutionError::UnknownTarget { source }) => {
            format!("the alias edge from {source:?} has no exact target")
        }
        Some(type_roots::TypeRootResolutionError::Cycle { repeated }) => {
            format!("the alias graph repeats {repeated:?}")
        }
        None => "the type identity is absent from the issued type table".to_string(),
    }
}

fn collect_overlay_function_declarations(
    overlay: &InstanceOverlay,
) -> Result<Vec<(DefId, Span)>, Span> {
    let mut collector = UsedFunctionCollector::default();
    for data in overlay.components.values() {
        for expression in [
            data.start.as_ref(),
            data.min.as_ref(),
            data.max.as_ref(),
            data.nominal.as_ref(),
            data.binding.as_ref(),
            data.binding_source.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            let _visit_outcome = collector.visit_expression(expression);
        }
        for subscript in &data.dims_expr {
            let _visit_outcome = collector.visit_subscript(subscript);
        }
    }
    for class in overlay.classes.values() {
        for equation in class.equations.iter().chain(&class.initial_equations) {
            let _visit_outcome = collector.visit_equation(&equation.equation);
        }
        for statement in class
            .algorithms
            .iter()
            .chain(&class.initial_algorithms)
            .flatten()
        {
            let _visit_outcome = collector.visit_statement(&statement.statement);
        }
    }
    if let Some(span) = collector.missing_target {
        return Err(span);
    }
    let mut declarations = collector.declarations.into_iter().collect::<Vec<_>>();
    declarations.sort_unstable_by_key(|(declaration, _)| declaration.index());
    Ok(declarations)
}

fn collect_class_function_declarations(class: &ClassDef) -> Result<Vec<(DefId, Span)>, Span> {
    let mut collector = UsedFunctionCollector::default();
    for equation in class.equations.iter().chain(&class.initial_equations) {
        let _visit_outcome = collector.visit_equation(equation);
    }
    for statement in class
        .algorithms
        .iter()
        .chain(&class.initial_algorithms)
        .flatten()
    {
        let _visit_outcome = collector.visit_statement(statement);
    }
    for component in class.components.values() {
        let _visit_outcome = collector.visit_component(component);
    }
    if let Some(span) = collector.missing_target {
        return Err(span);
    }
    Ok(collector.declarations.into_iter().collect())
}

fn is_predefined_function_declaration(
    tree: &ClassTree,
    semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
    declaration: DefId,
) -> bool {
    let flat_builtin = rumoca_core::BUILTIN_FUNCTIONS.iter().any(|name| {
        tree.scope_tree
            .predefined_member(&ComponentPath::from_flat_path(name))
            == Some(declaration)
    });
    flat_builtin || semantic_catalogs.connections().role(declaration).is_some()
}

fn missing_checked_call_target(declaration: DefId, span: Span) -> Box<TypeCheckError> {
    Box::new(TypeCheckError::phase_diagnostic(
        "ET012",
        format!("used call target {declaration:?} is absent from the checked class graph"),
        "call requires an exact checked declaration",
        span,
    ))
}

fn missing_checked_call_identity(span: Span) -> Box<TypeCheckError> {
    Box::new(TypeCheckError::phase_diagnostic(
        "ET012",
        "a used call has no exact resolved callable declaration identity",
        "call requires an exact checked declaration",
        span,
    ))
}

fn invalid_checked_call_target(declaration: DefId, span: Span) -> Box<TypeCheckError> {
    Box::new(TypeCheckError::phase_diagnostic(
        "ET012",
        format!("used call target {declaration:?} is not a function or record constructor"),
        "call requires an exact checked declaration",
        span,
    ))
}

fn missing_checked_function_signature(declaration: DefId, span: Span) -> Box<TypeCheckError> {
    Box::new(TypeCheckError::phase_diagnostic(
        "ET012",
        format!("used function {declaration:?} has no checked signature"),
        "call requires a complete checked function signature",
        span,
    ))
}

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

    /// Invalid required-value syntax reached an array selector.
    #[error("invalid subscript on component reference `{reference}`: {reason}")]
    InvalidAstSubscript {
        reference: String,
        reason: String,
        span: Span,
    },

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
            Self::InvalidAstSubscript {
                reference,
                reason,
                span,
            } => CommonDiagnostic::error(
                "ET005",
                format!("invalid subscript on component reference `{reference}`: {reason}"),
                PrimaryLabel::new(*span).with_message("invalid subscript value here"),
            )
            .with_note(
                "invalid required-value syntax cannot be used for semantic name or instance lookup",
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
    /// Exact Resolve identities of synchronous predefined intrinsics.
    predefined_intrinsics: HashMap<DefId, rumoca_core::BuiltinFunction>,
    /// DefId → fully-qualified class name map for anchor-aware dotted type lookup.
    def_qualified_names: HashMap<DefId, String>,
    /// Resolved TypeId map for user-defined type DefIds.
    type_ids_by_def_id: HashMap<DefId, TypeId>,
    /// Direct class inheritance edges used for nominal subtype compatibility.
    class_base_def_ids: HashMap<DefId, Vec<DefId>>,
    /// Operator-record identities and whether they declare their own `'0'` operator.
    operator_record_zero_capabilities: HashMap<DefId, bool>,
    /// Canonical type roots used for compatibility checks.
    ///
    /// This unwraps aliases and trivial class wrappers (e.g. operator-record
    /// unit wrappers) so assignment checks compare semantic roots.
    type_roots: HashMap<TypeId, TypeId>,
    /// Exact refusal retained for every type identity omitted from `type_roots`.
    ///
    /// Unused unresolved aliases are intentionally absent from downstream
    /// catalogs, but their construction failure is not erased: a later use can
    /// surface the original unknown edge or cycle.
    type_root_failures: HashMap<TypeId, type_roots::TypeRootResolutionError>,
    /// Source-declaration component metadata for standalone resolved-tree checks.
    current_declaration_semantics: HashMap<DefId, ComponentSemantics>,
    /// Concrete component metadata keyed by `InstanceId`.
    current_instance_semantics: InstanceSemanticScope,
    /// Concrete class instance whose body is currently being checked.
    current_class_instance_id: Option<InstanceId>,
    /// Concrete instance scope used for lexical lookup in instanced bodies and
    /// bindings.
    current_instance_scope: Option<ComponentPath>,
    /// Lexically active `for` iterators. These are Integer locals, not
    /// component references, and may shadow a component with the same name.
    current_integer_iterators: Vec<String>,
    /// Array domain contributed by the current structured class instance.
    /// Declaration-body equations are scalar over this implicit outer domain.
    current_instance_domain_shape: Vec<usize>,
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
    /// Complete function signatures after inherited inputs and outputs are
    /// merged in declaration order.
    function_signatures: HashMap<DefId, function_signatures::FunctionSignature>,
    /// Instance-context type specialization for calls through package aliases.
    ///
    /// Replaceable package functions can be inherited from a base package
    /// while their unqualified input/output record types are redeclared by the
    /// selected package. The outer key is the source alias (`Medium`); the
    /// inner map preserves the declaration slot from base type DefId to the
    /// effective instance type DefId.
    current_call_type_overrides: function_signatures::CallTypeOverrides,
    /// Type aliases whose targets could not be resolved during type-table
    /// construction (e.g. an MSL alias into a library that is not loaded).
    ///
    /// The error is deferred and surfaced only when the alias is actually
    /// used by the model being checked, so unrelated broken library classes
    /// cannot fail every compile in the session (strict-reachable semantics).
    deferred_alias_errors: HashMap<TypeId, (String, Span)>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        Self {
            diagnostics: Diagnostics::new(),
            eval_ctx: rumoca_eval_ast::eval::TypeCheckEvalContext::new(),
            source_map: SourceMap::default(),
            predefined_intrinsics: HashMap::new(),
            def_qualified_names: HashMap::new(),
            type_ids_by_def_id: HashMap::new(),
            class_base_def_ids: HashMap::new(),
            operator_record_zero_capabilities: HashMap::new(),
            type_roots: HashMap::new(),
            type_root_failures: HashMap::new(),
            current_declaration_semantics: HashMap::new(),
            current_instance_semantics: InstanceSemanticScope::default(),
            current_class_instance_id: None,
            current_instance_scope: None,
            current_integer_iterators: Vec::new(),
            current_instance_domain_shape: Vec::new(),
            component_modifier_targets: HashMap::new(),
            component_modifier_member_types: HashMap::new(),
            function_signatures: HashMap::new(),
            current_call_type_overrides: function_signatures::CallTypeOverrides::default(),
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
        match self.source_map.try_span(
            location.source,
            location.start as usize,
            location.end as usize,
        ) {
            Some(span) => Some(span),
            None => {
                let name = self
                    .source_map
                    .name(location.source)
                    .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME)
                    .to_string();
                self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                    "source file `{name}` for {context} was not found"
                )));
                None
            }
        }
    }

    /// Type check a ClassTree.
    pub fn check(&mut self, tree: &mut ClassTree) {
        self.source_map = tree.source_map.clone();
        register_predefined_eval_functions(tree, &mut self.eval_ctx);
        self.predefined_intrinsics = rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED
            .iter()
            .filter_map(|intrinsic| {
                tree.scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path(
                        intrinsic.name(),
                    ))
                    .map(|identity| (identity, *intrinsic))
            })
            .collect();
        self.def_qualified_names = tree
            .def_map
            .iter()
            .map(|(def_id, name)| (*def_id, name.clone()))
            .collect();
        self.populate_nominal_class_context(tree);
        self.populate_operator_record_capabilities(tree);
        self.function_signatures = function_signatures::build_function_signatures(tree);
        let (type_table, type_ids_by_def_id) = match self.build_type_context(tree) {
            Ok(context) => context,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return;
            }
        };
        tree.type_table = type_table;
        self.type_ids_by_def_id = type_ids_by_def_id;
        self.rebuild_type_roots(tree, &tree.type_table);
        self.component_modifier_targets = modifier_targets::build_component_modifier_targets(tree);
        self.component_modifier_member_types =
            match modifier_targets::build_component_modifier_member_types(
                tree,
                &tree.type_table,
                &self.type_ids_by_def_id,
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

    fn populate_nominal_class_context(&mut self, tree: &ClassTree) {
        self.class_base_def_ids = tree
            .name_map
            .values()
            .filter_map(|def_id| {
                let class = tree.get_class_by_def_id(*def_id)?;
                let bases = class
                    .extends
                    .iter()
                    .filter_map(|extends| extends.base_def_id)
                    .chain(class.redeclare_target_def_id)
                    .collect::<Vec<_>>();
                Some((*def_id, bases))
            })
            .collect();
    }

    fn populate_operator_record_capabilities(&mut self, tree: &ClassTree) {
        self.operator_record_zero_capabilities = tree
            .def_map
            .keys()
            .filter_map(|def_id| {
                let class = tree.get_class_by_def_id(*def_id)?;
                class
                    .operator_record
                    .then_some((*def_id, class.classes.contains_key("'0'")))
            })
            .collect();
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
        let mut component_index =
            HashMap::<ComponentPath, Vec<&rumoca_ir_ast::InstanceData>>::new();
        for data in overlay.components.values() {
            component_index
                .entry(data.qualified_name.to_component_path())
                .or_default()
                .push(data);
        }

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

    fn flush_eval_warnings(&mut self) {
        for diagnostic in self.eval_ctx.take_warnings() {
            self.diagnostics.emit(diagnostic);
        }
    }

    fn apply_instance_class_overrides(
        tree: &ClassTree,
        component_index: &HashMap<ComponentPath, Vec<&rumoca_ir_ast::InstanceData>>,
        data: &rumoca_ir_ast::InstanceData,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if data.class_overrides.is_empty() {
            return;
        }
        let comp_path = data.qualified_name.to_component_path();
        if comp_path.is_root() {
            return;
        }

        let active_alias = Self::component_active_alias(data);
        for class_override in data.class_overrides.values() {
            Self::apply_class_override_alias(
                tree,
                component_index,
                &comp_path,
                active_alias.as_deref(),
                &class_override.alias,
                class_override.target_def_id,
                ctx,
            );
        }
    }

    fn apply_class_override_alias(
        tree: &ClassTree,
        component_index: &HashMap<ComponentPath, Vec<&rumoca_ir_ast::InstanceData>>,
        comp_path: &ComponentPath,
        active_alias: Option<&str>,
        alias: &str,
        def_id: DefId,
        ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) {
        if Self::try_apply_forwarded_parent_alias_constants(
            tree,
            component_index,
            comp_path,
            active_alias,
            alias,
            def_id,
            ctx,
        ) {
            return;
        }

        let is_active_alias = active_alias == Some(alias);

        let comp_scope = comp_path.to_flat_string();
        let alias_scope = format!("{comp_scope}.{alias}");
        // MLS §7.3: instance-level redeclare overrides must replace inherited/default
        // package constants in the local alias scope.
        Self::clear_alias_scope_values(ctx, &alias_scope);
        Self::extract_override_class_constants(tree, &alias_scope, def_id, ctx);

        // For declarations like `Medium.BaseProperties medium`, expose
        // unqualified constants (`medium.nX`) from the active alias only.
        if is_active_alias {
            Self::extract_override_class_constants(tree, &comp_scope, def_id, ctx);
        }
    }

    fn try_apply_forwarded_parent_alias_constants(
        tree: &ClassTree,
        component_index: &HashMap<ComponentPath, Vec<&rumoca_ir_ast::InstanceData>>,
        comp_path: &ComponentPath,
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

        let Some(enclosing) = comp_path.parent() else {
            return false;
        };
        let Some(parent_candidates) = component_index.get(&enclosing) else {
            return false;
        };
        let [parent_data] = parent_candidates.as_slice() else {
            return false;
        };
        if Self::class_override_by_alias(parent_data, alias).is_none() {
            return false;
        }

        let comp_scope = comp_path.to_flat_string();
        let source_alias = format!("{comp_scope}.{alias}");
        let target_alias = format!("{}.{alias}", enclosing.to_flat_string());
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

    /// Build a type context that includes user-defined classes, enums, and aliases.
    fn build_type_context(
        &mut self,
        tree: &ClassTree,
    ) -> TypeCheckResult<(TypeTable, HashMap<DefId, TypeId>)> {
        let mut type_table = tree.type_table.clone();
        let mut type_ids_by_def_id = HashMap::new();
        for (name, type_id) in [
            ("Real", type_table.real()),
            ("Integer", type_table.integer()),
            ("Boolean", type_table.boolean()),
            ("String", type_table.string()),
            ("Clock", type_table.clock()),
        ] {
            if let Some(def_id) = tree
                .scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            {
                type_ids_by_def_id.insert(def_id, type_id);
            }
        }

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
        let source = if first.location.source != SourceId::DUMMY {
            first.location.source
        } else {
            last.location.source
        };
        self.source_map
            .try_span(
                source,
                first.location.start as usize,
                last.location.end as usize,
            )
            .ok_or_else(|| {
                let file_name = self
                    .source_map
                    .name(source)
                    .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME);
                Box::new(TypeCheckError::missing_source_context(format!(
                    "source file `{file_name}` for type alias target name was not found"
                )))
            })
    }

    fn location_span(&self, location: &rumoca_core::Location) -> TypeCheckResult<Span> {
        self.source_map
            .try_span(
                location.source,
                location.start as usize,
                location.end as usize,
            )
            .ok_or_else(|| {
                let file_name = self
                    .source_map
                    .name(location.source)
                    .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME);
                Box::new(TypeCheckError::missing_source_context(format!(
                    "source file `{file_name}` for typecheck location was not found"
                )))
            })
    }

    /// Resolve and populate component type ids in the instance overlay.
    ///
    /// This is used for the instanced pipeline where flatten consumes overlay type_ids.
    fn resolve_overlay_component_types(
        &mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
    ) {
        let specializations = instanced::overlay_component_type_specializations(tree, overlay);
        for (_instance_id, data) in overlay.components.iter_mut() {
            let type_def_id = instanced::specialized_instance_type_def_id(data, &specializations)
                .or(data.type_def_id);
            let resolved = self.resolve_type_name(&data.type_name, type_def_id, type_table);
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
    #[cfg(test)]
    fn populate_overlay_type_roots(
        &self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
    ) -> TypeCheckResult<()> {
        let used_functions = collect_overlay_function_declarations(overlay)
            .map_err(missing_checked_call_identity)?;
        let semantic_catalogs = semantic_catalog_projection_for_test(tree).map_err(|error| {
            Box::new(TypeCheckError::missing_source_context(format!(
                "cannot construct checked semantic catalogs: {error}",
            )))
        })?;
        self.populate_overlay_type_roots_with_semantics(
            tree,
            overlay,
            type_table,
            used_functions,
            &semantic_catalogs,
        )
    }

    fn populate_overlay_type_roots_with_semantics(
        &self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
        used_functions: Vec<(DefId, Span)>,
        semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
    ) -> TypeCheckResult<()> {
        let catalog = self.construct_type_root_catalog(tree, type_table);
        self.require_overlay_component_type_roots(
            tree,
            overlay,
            type_table,
            &catalog,
            used_functions,
            semantic_catalogs,
        )?;

        let mut roots = catalog.roots.into_iter().collect::<Vec<_>>();
        roots.sort_unstable_by_key(|(type_id, _)| type_id.index());
        let mut enumeration_roots = catalog.enumeration_roots.into_iter().collect::<Vec<_>>();
        enumeration_roots.sort_unstable_by_key(|type_id| type_id.index());
        overlay.type_roots = roots.into_iter().collect();
        overlay.enumeration_type_roots = enumeration_roots.into_iter().collect();
        overlay.type_ids_by_def_id = catalog.declarations.into_iter().collect();
        Ok(())
    }

    fn construct_type_root_catalog(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
    ) -> ResolvedTypeRootCatalog {
        let mut roots = HashMap::new();
        let mut failures = HashMap::new();
        let mut enumeration_roots = HashSet::new();
        for idx in 0..type_table.len() {
            let ty = TypeId::new(idx as u32);
            match self.resolve_overlay_type_root(tree, type_table, ty) {
                Ok(root) => {
                    roots.insert(ty, root);
                    if matches!(type_table.get(root), Some(Type::Enumeration(_))) {
                        enumeration_roots.insert(root);
                    }
                }
                Err(error) => {
                    failures.insert(ty, error);
                }
            }
        }
        let mut declarations = self
            .type_ids_by_def_id
            .iter()
            .filter_map(|(&declaration, &type_id)| {
                roots
                    .contains_key(&type_id)
                    .then_some((declaration, type_id))
            })
            .collect::<Vec<_>>();
        declarations.sort_unstable_by_key(|(declaration, _)| declaration.index());
        ResolvedTypeRootCatalog {
            roots,
            failures,
            enumeration_roots,
            declarations,
        }
    }

    fn rebuild_type_roots(&mut self, tree: &ClassTree, type_table: &TypeTable) {
        let catalog = self.construct_type_root_catalog(tree, type_table);
        self.type_roots = catalog.roots;
        self.type_root_failures = catalog.failures;
    }

    fn require_overlay_component_type_roots(
        &self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        type_table: &TypeTable,
        catalog: &ResolvedTypeRootCatalog,
        used_functions: Vec<(DefId, Span)>,
        semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
    ) -> TypeCheckResult<()> {
        let specializations = instanced::overlay_component_type_specializations(tree, overlay);
        for data in overlay.components.values() {
            let type_def_id = instanced::specialized_instance_type_def_id(data, &specializations)
                .or(data.type_def_id);
            let type_id = self.resolve_type_name(&data.type_name, type_def_id, type_table);
            if let Some((missing, span)) = self.deferred_alias_errors.get(&type_id) {
                return Err(Box::new(TypeCheckError::undefined_type(
                    missing.clone(),
                    *span,
                )));
            }
            if catalog.roots.contains_key(&type_id) {
                continue;
            }
            let span = self.location_span(&data.source_location)?;
            let failure = catalog.failures.get(&type_id);
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!(
                    "cannot issue the canonical type root for component `{}` (type {:?}, declaration {:?}): {}",
                    data.qualified_name.to_flat_string(),
                    type_id,
                    type_def_id,
                    format_type_root_failure(failure),
                ),
                "component uses an incomplete type identity",
                span,
            )));
        }
        self.require_used_function_type_roots(
            tree,
            type_table,
            catalog,
            used_functions,
            semantic_catalogs,
        )
    }

    fn require_used_function_type_roots(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        catalog: &ResolvedTypeRootCatalog,
        mut used: Vec<(DefId, Span)>,
        semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
    ) -> TypeCheckResult<()> {
        let mut checked_functions = HashSet::new();
        let mut checked_records = HashSet::new();
        while let Some((function, use_span)) = used.pop() {
            if !checked_functions.insert(function) {
                continue;
            }
            if is_predefined_function_declaration(tree, semantic_catalogs, function) {
                continue;
            }
            let Some(class) = tree.get_class_by_def_id(function) else {
                return Err(missing_checked_call_target(function, use_span));
            };
            if matches!(class.class_type, rumoca_core::ClassType::Record) {
                self.require_record_field_type_roots(
                    tree,
                    function,
                    type_table,
                    catalog,
                    &mut checked_records,
                    use_span,
                )?;
                continue;
            }
            if !matches!(class.class_type, rumoca_core::ClassType::Function) {
                if let Some(lifecycle) = semantic_catalogs.external_object(function) {
                    used.push((lifecycle.constructor(), use_span));
                    used.push((lifecycle.destructor(), use_span));
                    continue;
                }
                return Err(invalid_checked_call_target(function, use_span));
            }
            let signature = self
                .function_signatures
                .get(&function)
                .ok_or_else(|| missing_checked_function_signature(function, use_span))?;
            for (_, component) in signature.inputs.iter().chain(&signature.outputs) {
                self.require_declaration_component_type_root(
                    tree,
                    component,
                    type_table,
                    catalog,
                    &mut checked_records,
                )?;
            }
            used.extend(
                collect_class_function_declarations(class)
                    .map_err(missing_checked_call_identity)?,
            );
            for component in class.components.values() {
                self.require_declaration_component_type_root(
                    tree,
                    component,
                    type_table,
                    catalog,
                    &mut checked_records,
                )?;
            }
        }
        Ok(())
    }

    fn require_declaration_component_type_root(
        &self,
        tree: &ClassTree,
        component: &Component,
        type_table: &TypeTable,
        catalog: &ResolvedTypeRootCatalog,
        checked_records: &mut HashSet<DefId>,
    ) -> TypeCheckResult<()> {
        let type_id = self.resolve_type_name(
            &component.type_name.to_string(),
            component.type_def_id,
            type_table,
        );
        if let Some((missing, span)) = self.deferred_alias_errors.get(&type_id) {
            return Err(Box::new(TypeCheckError::undefined_type(
                missing.clone(),
                *span,
            )));
        }
        let Some(&canonical) = catalog.roots.get(&type_id) else {
            let span = self.location_span(&component.location)?;
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!(
                    "cannot issue the canonical type root for declaration {:?} (type {:?}): {}",
                    component.def_id,
                    type_id,
                    format_type_root_failure(catalog.failures.get(&type_id)),
                ),
                "function or record declaration uses an incomplete type identity",
                span,
            )));
        };
        let Some(record) =
            self.record_declaration_for_canonical_root(component, canonical, type_table, catalog)?
        else {
            return Ok(());
        };
        let span = self.location_span(&component.location)?;
        self.require_record_field_type_roots(
            tree,
            record,
            type_table,
            catalog,
            checked_records,
            span,
        )
    }

    fn record_declaration_for_canonical_root(
        &self,
        component: &Component,
        canonical: TypeId,
        type_table: &TypeTable,
        catalog: &ResolvedTypeRootCatalog,
    ) -> TypeCheckResult<Option<DefId>> {
        let Some(Type::Class(class_type)) = type_table.get(canonical) else {
            return Ok(None);
        };
        if class_type.kind != ClassKind::Record {
            return Ok(None);
        }
        let exact_nominal = self.type_ids_by_def_id.get(&class_type.def_id).copied();
        if exact_nominal != Some(canonical)
            || catalog.roots.get(&canonical).copied() != Some(canonical)
        {
            let span = self.location_span(&component.location)?;
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!(
                    "record declaration {:?} contradicts canonical type identity {canonical:?}",
                    class_type.def_id,
                ),
                "function or record declaration requires an exact nominal record root",
                span,
            )));
        }
        Ok(Some(class_type.def_id))
    }

    fn require_record_field_type_roots(
        &self,
        tree: &ClassTree,
        record: DefId,
        type_table: &TypeTable,
        catalog: &ResolvedTypeRootCatalog,
        checked_records: &mut HashSet<DefId>,
        use_span: Span,
    ) -> TypeCheckResult<()> {
        let Some(class) = tree.get_class_by_def_id(record) else {
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!("required record declaration {record:?} is absent from the class tree"),
                "record type is required by this checked declaration",
                use_span,
            )));
        };
        if !matches!(class.class_type, rumoca_core::ClassType::Record) {
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!("required record declaration {record:?} is not a record class"),
                "record type is required by this checked declaration",
                use_span,
            )));
        }
        if !checked_records.insert(record) {
            return Ok(());
        }
        for field in class.components.values() {
            self.require_declaration_component_type_root(
                tree,
                field,
                type_table,
                catalog,
                checked_records,
            )?;
        }
        for extends in &class.extends {
            let base = extends.base_def_id.ok_or_else(|| {
                Box::new(TypeCheckError::phase_diagnostic(
                    "ET012",
                    format!("required record declaration {record:?} has an unresolved base edge"),
                    "record base requires an exact resolved declaration identity",
                    use_span,
                ))
            })?;
            let Some(base_class) = tree.get_class_by_def_id(base) else {
                return Err(Box::new(TypeCheckError::phase_diagnostic(
                    "ET000",
                    format!("record base declaration {base:?} is absent from the class tree"),
                    "record type is required by this checked declaration",
                    use_span,
                )));
            };
            if !matches!(base_class.class_type, rumoca_core::ClassType::Record) {
                return Err(Box::new(TypeCheckError::phase_diagnostic(
                    "ET000",
                    format!("record base declaration {base:?} is not a record class"),
                    "record inheritance requires an exact record declaration",
                    use_span,
                )));
            }
            self.require_record_field_type_roots(
                tree,
                base,
                type_table,
                catalog,
                checked_records,
                use_span,
            )?;
        }
        Ok(())
    }

    fn resolve_overlay_type_root(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        ty: TypeId,
    ) -> Result<TypeId, type_roots::TypeRootResolutionError> {
        type_roots::resolve_type_root(ty, |current| {
            self.next_overlay_type_root_step(tree, type_table, current, &self.type_ids_by_def_id)
        })
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
                let Some(class) = tree.get_class_by_def_id(class_ty.def_id) else {
                    return Some(TypeId::UNKNOWN);
                };
                let is_wrapper = if class_ty.kind == ClassKind::Connector {
                    Self::is_connector_alias_wrapper(class)
                } else {
                    Self::is_class_alias_wrapper(class)
                };
                if !is_wrapper {
                    return None;
                }
                Some(
                    Self::try_resolve_alias_target_type_id(class, type_table, type_ids_by_def_id)
                        .unwrap_or(TypeId::UNKNOWN),
                )
            }
            Some(Type::Unknown) | None => Some(TypeId::UNKNOWN),
            _ => None,
        }
    }

    fn is_connector_alias_wrapper(class: &ClassDef) -> bool {
        matches!(class.class_type, rumoca_core::ClassType::Connector)
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
            let explicit_progress = self.evaluate_explicit_dimensions_pass(tree, overlay);

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

fn register_predefined_eval_functions(
    tree: &ClassTree,
    ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
) {
    ctx.set_predefined_functions(rumoca_core::BUILTIN_FUNCTIONS.iter().filter_map(|name| {
        tree.scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            .map(|identity| ((*name).to_string(), identity))
    }));
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
