//! Type checking phase for the Rumoca compiler.
//!
//! SPEC_0021 file-size exception: this facade still owns late type inference,
//! modifier validation, and scoped instance assembly. split plan: move those
//! concerns behind focused modules as follow-up compiler hardening work.
//!
//! This phase has two entry points. `typecheck` is a standalone diagnostics
//! query over a resolved `ClassTree`; it mints no proof. The production
//! compiled-model pipeline uses `typecheck_instanced_tree` after
//! instantiation, so modifier-dependent dimensions and structural parameters
//! are available in the instance overlay, and it is the sole mint of the
//! opaque [`TypedInstancedTree`] proof that flattening consumes by value.
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
//! The standalone API input is a `ResolvedTree` and the output is the checked
//! `ClassTree` data plus diagnostics. The production API consumes the
//! instantiation overlay by value and publishes one immutable
//! `TypedInstancedTree`; no mutable predecessor alias crosses the boundary.
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
mod expression_type;
mod function_signatures;
mod instanced;
mod modifier_targets;
mod path_utils;
mod semantic_scope;
mod type_roots;
mod typechecker;
mod typed_instanced;
pub mod unit_syntax;

use expression_type::{
    ExpressionType, IncompatibilityKind, MultiValueForm, ReportedComposition, ReportedTypeError,
    TypeErrorReason, ValueCompositionContext,
};
use rumoca_core::{ComponentPath, DefId, InstanceId, ScopeId, SourceId, Span, TypeId};
use rumoca_core::{
    Diagnostic as CommonDiagnostic, Diagnostics, PhaseError, PrimaryLabel, SourceMap,
};

/// Placeholder used when a `SourceId` has no registered name in the source map.
pub(crate) const UNKNOWN_SOURCE_DISPLAY_NAME: &str = "<unknown source>";
use rumoca_ir_ast::{
    ClassDef, ClassKind, ClassTree, Component, EnumerationType, Expression, ExpressionContext,
    InstanceOverlay, StoredDefinition, Type, TypeAlias, TypeClassType, TypeTable,
    TypeTableAppendError, Visitor,
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
pub use typechecker::api::{typecheck, typecheck_instanced_tree};
pub use typed_instanced::{TypedInstancedTree, TypedOverlayProjection};

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

enum PlannedType<'tree> {
    Enumeration {
        name: String,
        declaration: DefId,
        class: &'tree ClassDef,
    },
    Class {
        name: String,
        declaration: DefId,
        class: &'tree ClassDef,
    },
    Alias {
        name: String,
        declaration: DefId,
        class: &'tree ClassDef,
    },
}

impl<'tree> PlannedType<'tree> {
    fn name(&self) -> &str {
        match self {
            Self::Enumeration { name, .. }
            | Self::Class { name, .. }
            | Self::Alias { name, .. } => name,
        }
    }

    fn declaration(&self) -> DefId {
        match self {
            Self::Enumeration { declaration, .. }
            | Self::Class { declaration, .. }
            | Self::Alias { declaration, .. } => *declaration,
        }
    }

    fn class(&self) -> &'tree ClassDef {
        match self {
            Self::Enumeration { class, .. }
            | Self::Class { class, .. }
            | Self::Alias { class, .. } => class,
        }
    }
}

#[derive(Clone)]
struct ResolvedTypeRoots {
    dense: Box<[(TypeId, TypeId)]>,
}

impl ResolvedTypeRoots {
    fn empty() -> Self {
        Self {
            dense: Box::new([]),
        }
    }

    fn canonical(&self, type_id: TypeId) -> TypeId {
        if type_id.is_unknown() {
            return TypeId::UNKNOWN;
        }
        self.dense[type_id.index() as usize].1
    }

    fn get(&self, type_id: TypeId) -> Option<TypeId> {
        if type_id.is_unknown() {
            return None;
        }
        self.dense
            .get(type_id.index() as usize)
            .map(|(_, canonical)| *canonical)
    }

    fn into_pairs(self) -> impl ExactSizeIterator<Item = (TypeId, TypeId)> {
        self.dense.into_vec().into_iter()
    }
}

struct ResolvedTypeRootCatalog {
    roots: ResolvedTypeRoots,
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

fn format_type_root_failure(failure: type_roots::TypeRootResolutionError) -> String {
    match failure {
        type_roots::TypeRootResolutionError::UnknownTarget { .. } => {
            "has no exact target".to_string()
        }
        type_roots::TypeRootResolutionError::Cycle { .. } => "closes an alias cycle".to_string(),
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

    /// Canonical type-root construction encountered an impossible edge.
    #[error("canonical type-root invariant violated: {reason}")]
    CanonicalTypeRootInvariant {
        root_name: String,
        root: Option<TypeId>,
        edge_source: Option<TypeId>,
        edge_target: Option<TypeId>,
        reason: String,
        span: Option<Span>,
    },

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

    fn canonical_type_root_diagnostic(
        root_name: &str,
        root: Option<TypeId>,
        edge_source: Option<TypeId>,
        edge_target: Option<TypeId>,
        reason: &str,
        span: Option<Span>,
    ) -> CommonDiagnostic {
        let root = root.map_or_else(|| "<unallocated>".to_string(), |root| format!("{root:?}"));
        let (message, label) = if let Some(edge_source) = edge_source {
            let edge_target =
                edge_target.map_or_else(|| "<missing>".to_string(), |target| format!("{target:?}"));
            (
                format!(
                    "cannot construct the canonical type root for `{root_name}` ({root}): edge {edge_source:?} -> {edge_target} {reason}"
                ),
                format!("failing canonical edge {edge_source:?} -> {edge_target}"),
            )
        } else {
            (
                format!(
                    "cannot construct the canonical alias identity for `{root_name}` ({root}): {reason}"
                ),
                "affected canonical alias edge here".to_string(),
            )
        };
        let diagnostic = if let Some(span) = span {
            CommonDiagnostic::error(
                "ET014",
                message,
                PrimaryLabel::new(span).with_message(label),
            )
        } else {
            CommonDiagnostic::global_error("ET014", message)
        };
        diagnostic.with_note(
            "Resolve-issued type identities must form a finite, exact canonical-root graph",
        )
    }
}

impl PhaseError for TypeCheckError {
    fn to_diagnostic(&self) -> CommonDiagnostic {
        match self {
            Self::UndefinedType { name, span } => CommonDiagnostic::error(
                "ET001",
                format!("undefined type '{name}' not found"),
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
            Self::CanonicalTypeRootInvariant {
                root_name,
                root,
                edge_source,
                edge_target,
                reason,
                span,
            } => Self::canonical_type_root_diagnostic(
                root_name,
                *root,
                *edge_source,
                *edge_target,
                reason,
                *span,
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

/// One-shot type checking context.
///
/// The context stays crate-private and every complete phase entry consumes it,
/// so tree-local identities and diagnostics cannot be reused with another
/// compiler root.
struct TypeChecker {
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
    type_roots: ResolvedTypeRoots,
    /// Source-declaration component metadata for standalone resolved-tree checks.
    current_declaration_semantics: HashMap<DefId, ComponentSemantics>,
    /// Concrete component metadata keyed by `InstanceId`.
    current_instance_semantics: InstanceSemanticScope,
    /// Concrete class instance whose body is currently being checked.
    current_class_instance_id: Option<InstanceId>,
    /// Concrete instance scope used for lexical lookup in instanced bodies and
    /// bindings.
    current_instance_scope: Option<ComponentPath>,
    /// Lexically active `for` iterator binders, paired with the value type
    /// issued from the iterator's checked range domain (MLS §11.2.2). These are
    /// locals, not component references, and may shadow a component with the
    /// same name, so the binder is consulted before instance lookup.
    ///
    /// The binder type is derived, never asserted: an iterator over
    /// `1:0.5:2` binds Real, one over an enumeration binds that enumeration,
    /// and one over a range that cannot be inferred stays unknown rather than
    /// defaulting to Integer.
    current_iterator_binders: Vec<(String, ExpressionType)>,
    /// Array domain contributed by the current structured class instance.
    /// Declaration-body equations are scalar over this implicit outer domain.
    current_instance_domain_shape: Vec<usize>,
    /// Resolve-issued direct/inherited member outcomes used by the sole
    /// modifier-target validator.
    class_members: modifier_targets::ModifierMemberCatalog,
    /// Class declarations whose source modifier targets have been checked.
    ///
    /// The instanced checker may visit one declaration through many concrete
    /// occurrences; member existence belongs to the source declaration and is
    /// issued exactly once.
    validated_modifier_classes: HashSet<DefId>,
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
}

impl TypeChecker {
    /// Create a new type checker.
    fn new() -> Self {
        Self {
            diagnostics: Diagnostics::new(),
            eval_ctx: rumoca_eval_ast::eval::TypeCheckEvalContext::for_resolved_identities(),
            source_map: SourceMap::default(),
            predefined_intrinsics: HashMap::new(),
            def_qualified_names: HashMap::new(),
            type_ids_by_def_id: HashMap::new(),
            class_base_def_ids: HashMap::new(),
            operator_record_zero_capabilities: HashMap::new(),
            type_roots: ResolvedTypeRoots::empty(),
            current_declaration_semantics: HashMap::new(),
            current_instance_semantics: InstanceSemanticScope::default(),
            current_class_instance_id: None,
            current_instance_scope: None,
            current_iterator_binders: Vec::new(),
            current_instance_domain_shape: Vec::new(),
            class_members: HashMap::new(),
            validated_modifier_classes: HashSet::new(),
            function_signatures: HashMap::new(),
            current_call_type_overrides: function_signatures::CallTypeOverrides::default(),
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
    fn check(self, tree: &mut ClassTree) -> Diagnostics {
        let mut candidate = tree.clone();
        let diagnostics = self.check_detached(&mut candidate);
        if !diagnostics.has_errors() {
            *tree = candidate;
        }
        diagnostics
    }

    fn check_detached(mut self, tree: &mut ClassTree) -> Diagnostics {
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
                return self.diagnostics;
            }
        };
        self.type_ids_by_def_id = type_ids_by_def_id;
        let type_root_catalog = match self.construct_type_root_catalog(tree, &type_table) {
            Ok(catalog) => catalog,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return self.diagnostics;
            }
        };
        if let Err(error) = self.require_standalone_external_type_ids(tree, &type_table) {
            self.emit_typecheck_error(*error);
            return self.diagnostics;
        }
        self.type_roots = type_root_catalog.roots;
        tree.type_table = type_table;
        self.class_members =
            modifier_targets::build_modifier_member_catalog(tree, &self.type_ids_by_def_id);
        self.validate_all_modifier_targets(tree, &tree.type_table);
        self.check_stored_definition(&mut tree.definitions, &mut tree.type_table);
        self.flush_eval_warnings();
        self.diagnostics
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

    fn require_standalone_external_type_ids(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
    ) -> TypeCheckResult<()> {
        let mut visited = HashSet::new();
        let components = tree
            .name_map
            .values()
            .copied()
            .filter(|def_id| visited.insert(*def_id))
            .filter_map(|def_id| tree.get_class_by_def_id(def_id))
            .flat_map(|class| class.components.values());
        for component in components {
            self.require_standalone_external_type_id(component, type_table)?;
        }
        Ok(())
    }

    fn require_standalone_external_type_id(
        &self,
        component: &Component,
        type_table: &TypeTable,
    ) -> TypeCheckResult<()> {
        let Some(type_id) = component.type_id.filter(|type_id| !type_id.is_unknown()) else {
            return Ok(());
        };
        if type_table.get(type_id).is_some() {
            return Ok(());
        }
        Err(self.type_identity_invariant_error(
            &component.type_name.to_string(),
            Some(type_id),
            Some(type_id),
            None,
            format!(
                "component `{}` carries a non-issued external type identity",
                component.name
            ),
            self.location_span(&component.location).ok(),
        ))
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
        let inventory = tree.type_declaration_inventory().map_err(|error| {
            let class = tree.get_class_by_qualified_name(error.repeated_name());
            self.type_identity_invariant_error(
                error.repeated_name(),
                None,
                None,
                None,
                error.to_string(),
                class.and_then(|class| self.class_type_edge_span(class)),
            )
        })?;
        let mut type_ids_by_def_id = inventory.predefined().collect::<HashMap<_, _>>();
        let declarations = self.planned_type_declarations(tree, &inventory)?;
        let first_name = declarations
            .first()
            .map_or("resolved type inventory", PlannedType::name)
            .to_string();
        let first_class = declarations.first().map(PlannedType::class);
        let append_plan = type_table
            .plan_declaration_append(declarations, inventory)
            .map_err(|error| {
                self.type_identity_invariant_error(
                    &first_name,
                    None,
                    None,
                    None,
                    error.to_string(),
                    first_class.and_then(|class| self.class_type_edge_span(class)),
                )
            })?;

        for (type_id, declaration) in append_plan.entries() {
            let def_id = declaration.declaration();
            if let Some(previous) = type_ids_by_def_id.insert(def_id, type_id) {
                return Err(self.type_identity_invariant_error(
                    declaration.name(),
                    Some(type_id),
                    Some(type_id),
                    Some(previous),
                    format!("Resolve declaration {def_id:?} was claimed by two type payloads"),
                    self.class_type_edge_span(declaration.class()),
                ));
            }
        }
        let commit_result =
            append_plan.commit_declared(|type_id, declaration| -> TypeCheckResult<(DefId, Type)> {
                let def_id = declaration.declaration();
                self.construct_planned_type(type_id, declaration, &type_ids_by_def_id)
                    .map(|ty| (def_id, ty))
            });
        self.finish_type_append(tree, &type_table, commit_result)?;
        Ok((type_table, type_ids_by_def_id))
    }

    fn planned_type_declarations<'tree>(
        &self,
        tree: &'tree ClassTree,
        inventory: &rumoca_ir_ast::TypeDeclarationInventory,
    ) -> TypeCheckResult<Vec<PlannedType<'tree>>> {
        inventory
            .declarations()
            .map(|(declaration, name)| {
                let class = tree.get_class_by_def_id(declaration).ok_or_else(|| {
                    self.type_identity_invariant_error(
                        name,
                        None,
                        None,
                        None,
                        "tree-issued type declaration has no structural payload".to_string(),
                        None,
                    )
                })?;
                let name = name.to_string();
                if !class.enum_literals.is_empty() {
                    return Ok(PlannedType::Enumeration {
                        name,
                        declaration,
                        class,
                    });
                }
                if matches!(class.class_type, rumoca_core::ClassType::Type) {
                    return Ok(PlannedType::Alias {
                        name,
                        declaration,
                        class,
                    });
                }
                Ok(PlannedType::Class {
                    name,
                    declaration,
                    class,
                })
            })
            .collect()
    }

    fn construct_planned_type(
        &self,
        type_id: TypeId,
        declaration: PlannedType<'_>,
        identities: &HashMap<DefId, TypeId>,
    ) -> TypeCheckResult<Type> {
        if identities.get(&declaration.declaration()) != Some(&type_id) {
            return Err(self.type_identity_invariant_error(
                declaration.name(),
                Some(type_id),
                Some(type_id),
                None,
                "planned declaration identity is not paired with its construction payload"
                    .to_string(),
                self.class_type_edge_span(declaration.class()),
            ));
        }
        match declaration {
            PlannedType::Enumeration { name, class, .. } => {
                Ok(Type::Enumeration(EnumerationType {
                    name,
                    literals: class
                        .enum_literals
                        .iter()
                        .map(|literal| literal.ident.text.to_string())
                        .collect(),
                }))
            }
            PlannedType::Class {
                name,
                declaration,
                class,
            } => Ok(Type::Class(TypeClassType {
                name,
                def_id: declaration,
                kind: Self::ast_class_kind(&class.class_type),
            })),
            PlannedType::Alias { name, class, .. } => {
                let aliased =
                    self.resolve_alias_target_type_id(name.as_str(), type_id, class, identities)?;
                Ok(Type::Alias(TypeAlias { name, aliased }))
            }
        }
    }

    fn finish_type_append(
        &self,
        tree: &ClassTree,
        detached: &TypeTable,
        result: Result<(), TypeTableAppendError<Box<TypeCheckError>>>,
    ) -> TypeCheckResult<()> {
        match result {
            Ok(()) => Ok(()),
            Err(TypeTableAppendError::Construction(error)) => Err(error),
            Err(TypeTableAppendError::MissingDeclarationInventory) => {
                Err(self.type_identity_invariant_error(
                    "resolved type inventory",
                    None,
                    None,
                    None,
                    "the checked append did not consume its ClassTree declaration authority"
                        .to_string(),
                    None,
                ))
            }
            Err(TypeTableAppendError::ExistingUnclaimedPayload) => {
                Err(self.type_identity_invariant_error(
                    "resolved type inventory",
                    None,
                    None,
                    None,
                    "the input TypeTable contains a payload not issued by this ClassTree declaration inventory"
                        .to_string(),
                    None,
                ))
            }
            Err(TypeTableAppendError::DeclarationCount { expected, actual }) => {
                Err(self.type_identity_invariant_error(
                    "resolved type inventory",
                    None,
                    None,
                    None,
                    format!(
                        "the ClassTree owns {expected} type declarations but the checked append received {actual} payloads"
                    ),
                    None,
                ))
            }
            Err(TypeTableAppendError::DuplicateDeclaration {
                type_id,
                declaration,
                previous,
            }) => {
                let class = tree.get_class_by_def_id(declaration);
                Err(self.type_identity_invariant_error(
                    tree.def_map
                        .get(&declaration)
                        .map_or("resolved type declaration", String::as_str),
                    Some(type_id),
                    Some(type_id),
                    Some(previous),
                    format!(
                        "Resolve declaration {declaration:?} was claimed by two type payloads"
                    ),
                    class.and_then(|class| self.class_type_edge_span(class)),
                ))
            }
            Err(TypeTableAppendError::DeclarationPayloadMismatch {
                type_id,
                declaration,
                expected,
                expected_name,
            }) => {
                let class = tree.get_class_by_def_id(expected);
                Err(self.type_identity_invariant_error(
                    &expected_name,
                    Some(type_id),
                    Some(type_id),
                    None,
                    format!(
                        "type payload claims {declaration:?}, but the ClassTree inventory issued {expected:?}"
                    ),
                    class.and_then(|class| self.class_type_edge_span(class)),
                ))
            }
            Err(TypeTableAppendError::DuplicateTypeName { type_id, name }) => {
                let class = tree.get_class_by_qualified_name(&name);
                Err(self.type_identity_invariant_error(
                    &name,
                    Some(type_id),
                    Some(type_id),
                    detached.lookup(&name),
                    "would overwrite or reuse an existing type-name binding".to_string(),
                    class.and_then(|class| self.class_type_edge_span(class)),
                ))
            }
            Err(TypeTableAppendError::InvalidTypePayload {
                type_id,
                referenced,
            }) => Err(self.invalid_type_payload_append_error(type_id, referenced)),
            Err(TypeTableAppendError::AliasCycle {
                type_id,
                referenced,
                name,
            }) => Err(self.alias_cycle_append_error(tree, type_id, referenced, name)),
        }
    }

    fn invalid_type_payload_append_error(
        &self,
        type_id: TypeId,
        referenced: Option<TypeId>,
    ) -> Box<TypeCheckError> {
        self.type_identity_invariant_error(
            &format!("{type_id:?}"),
            Some(type_id),
            Some(type_id),
            referenced,
            "planned type payload is incomplete or references an unissued identity".to_string(),
            None,
        )
    }

    fn alias_cycle_append_error(
        &self,
        tree: &ClassTree,
        type_id: TypeId,
        referenced: TypeId,
        name: String,
    ) -> Box<TypeCheckError> {
        let class = tree.get_class_by_qualified_name(&name);
        self.type_identity_invariant_error(
            &name,
            Some(type_id),
            Some(type_id),
            Some(referenced),
            "closes an alias cycle".to_string(),
            class.and_then(|class| self.class_type_edge_span(class)),
        )
    }

    fn type_identity_invariant_error(
        &self,
        name: &str,
        type_id: Option<TypeId>,
        edge_source: Option<TypeId>,
        edge_target: Option<TypeId>,
        reason: String,
        span: Option<Span>,
    ) -> Box<TypeCheckError> {
        Box::new(TypeCheckError::CanonicalTypeRootInvariant {
            root_name: name.to_string(),
            root: type_id,
            edge_source,
            edge_target,
            reason,
            span,
        })
    }

    fn class_type_edge_span(&self, class: &ClassDef) -> Option<Span> {
        class
            .extends
            .first()
            .and_then(|extend| self.name_span(&extend.base_name).ok())
            .or_else(|| self.location_span(&class.location).ok())
    }

    fn resolve_alias_target_type_id(
        &self,
        name: &str,
        source: TypeId,
        class: &ClassDef,
        type_ids_by_def_id: &HashMap<DefId, TypeId>,
    ) -> TypeCheckResult<TypeId> {
        let [extend] = class.extends.as_slice() else {
            let span = class
                .extends
                .get(1)
                .and_then(|extra| self.name_span(&extra.base_name).ok())
                .or_else(|| self.location_span(&class.location).ok());
            return Err(self.type_identity_invariant_error(
                name,
                Some(source),
                Some(source),
                None,
                format!(
                    "Resolve-branded alias must own exactly one base edge, found {}",
                    class.extends.len()
                ),
                span,
            ));
        };
        let edge_span = self.name_span(&extend.base_name).ok();
        let Some(base_def_id) = extend.base_def_id else {
            return Err(self.type_identity_invariant_error(
                name,
                Some(source),
                Some(source),
                None,
                format!(
                    "Resolve-branded alias base edge `{}` has no declaration identity",
                    extend.base_name
                ),
                edge_span,
            ));
        };
        let Some(target) = type_ids_by_def_id.get(&base_def_id).copied() else {
            return Err(self.type_identity_invariant_error(
                name,
                Some(source),
                Some(source),
                None,
                format!("Resolve-branded alias target {base_def_id:?} has no issued type payload"),
                edge_span,
            ));
        };
        Ok(target)
    }

    fn ast_class_kind(class_type: &rumoca_core::ClassType) -> ClassKind {
        match class_type {
            rumoca_core::ClassType::Class => ClassKind::Class,
            rumoca_core::ClassType::Model => ClassKind::Model,
            rumoca_core::ClassType::Block => ClassKind::Block,
            rumoca_core::ClassType::Record => ClassKind::Record,
            rumoca_core::ClassType::Connector => ClassKind::Connector,
            rumoca_core::ClassType::Type => ClassKind::Type,
            rumoca_core::ClassType::Package => ClassKind::Package,
            rumoca_core::ClassType::Function => ClassKind::Function,
            rumoca_core::ClassType::Operator => ClassKind::Operator,
        }
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

    #[cfg(test)]
    fn populate_overlay_type_roots_with_semantics(
        &self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
        used_functions: Vec<(DefId, Span)>,
        semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
    ) -> TypeCheckResult<()> {
        let catalog = self.construct_type_root_catalog(tree, type_table)?;
        self.populate_overlay_type_roots_from_catalog(
            tree,
            overlay,
            type_table,
            used_functions,
            semantic_catalogs,
            catalog,
        )
    }

    fn populate_overlay_type_roots_from_catalog(
        &self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        type_table: &TypeTable,
        used_functions: Vec<(DefId, Span)>,
        semantic_catalogs: &rumoca_ir_ast::SemanticCatalogProjection,
        catalog: ResolvedTypeRootCatalog,
    ) -> TypeCheckResult<()> {
        self.require_overlay_component_type_roots(
            tree,
            overlay,
            type_table,
            &catalog,
            used_functions,
            semantic_catalogs,
        )?;

        let roots = catalog.roots.into_pairs();
        let mut enumeration_roots = catalog.enumeration_roots.into_iter().collect::<Vec<_>>();
        enumeration_roots.sort_unstable_by_key(|type_id| type_id.index());
        overlay.type_roots = roots.collect();
        overlay.enumeration_type_roots = enumeration_roots.into_iter().collect();
        overlay.type_ids_by_def_id = catalog.declarations.into_iter().collect();
        Ok(())
    }

    fn construct_type_root_catalog(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
    ) -> TypeCheckResult<ResolvedTypeRootCatalog> {
        let mut pairs = Vec::with_capacity(type_table.len());
        let mut enumeration_roots = HashSet::new();
        for (ty, _) in type_table.entries() {
            let root = self
                .resolve_overlay_type_root(tree, type_table, ty)
                .map_err(|failure| {
                    self.type_root_construction_error(tree, type_table, ty, failure)
                })?;
            pairs.push((ty, root));
            enumeration_roots
                .extend(matches!(type_table.get(root), Some(Type::Enumeration(_))).then_some(root));
        }
        let mut declarations = self
            .type_ids_by_def_id
            .iter()
            .map(|(&declaration, &type_id)| (declaration, type_id))
            .collect::<Vec<_>>();
        declarations.sort_unstable_by_key(|(declaration, _)| declaration.index());
        Ok(ResolvedTypeRootCatalog {
            roots: ResolvedTypeRoots {
                dense: pairs.into_boxed_slice(),
            },
            enumeration_roots,
            declarations,
        })
    }

    fn type_root_construction_error(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        type_id: TypeId,
        failure: type_roots::TypeRootResolutionError,
    ) -> Box<TypeCheckError> {
        let type_name = TypeChecker::format_type_name(type_table, type_id);
        let edge_source = failure.source();
        let edge_target = failure.target();
        let span = self
            .type_ids_by_def_id
            .iter()
            .filter(|(_, candidate)| **candidate == edge_source)
            .filter_map(|(declaration, _)| {
                tree.get_class_by_def_id(*declaration)
                    .map(|class| (*declaration, class))
            })
            .min_by_key(|(declaration, _)| declaration.index())
            .and_then(|(_, class)| class.extends.first())
            .and_then(|extend| self.name_span(&extend.base_name).ok());
        Box::new(TypeCheckError::CanonicalTypeRootInvariant {
            root_name: type_name,
            root: Some(type_id),
            edge_source: Some(edge_source),
            edge_target,
            reason: format_type_root_failure(failure),
            span,
        })
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
            if !data.type_id.is_unknown() && type_table.get(data.type_id).is_none() {
                let span = self.location_span(&data.source_location)?;
                return Err(self.type_identity_invariant_error(
                    &data.type_name,
                    Some(data.type_id),
                    Some(data.type_id),
                    None,
                    format!(
                        "component `{}` carries a non-issued external type identity",
                        data.qualified_name.to_flat_string()
                    ),
                    Some(span),
                ));
            }
            let type_def_id = instanced::specialized_instance_type_def_id(data, &specializations)
                .or(data.type_def_id);
            let type_id = self.resolve_type_name(&data.type_name, type_def_id, type_table);
            if type_id.is_unknown() {
                let span = self.location_span(&data.source_location)?;
                return Err(Box::new(TypeCheckError::undefined_type(
                    data.type_name.clone(),
                    span,
                )));
            }
            if catalog.roots.get(type_id).is_some() {
                continue;
            }
            let span = self.location_span(&data.source_location)?;
            return Err(self.type_identity_invariant_error(
                &data.type_name,
                Some(type_id),
                Some(type_id),
                None,
                format!(
                    "component `{}` resolves to an identity absent from the issued type table (declaration {type_def_id:?})",
                    data.qualified_name.to_flat_string(),
                ),
                Some(span),
            ));
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
                let lifecycle = semantic_catalogs
                    .external_object(function)
                    .ok_or_else(|| invalid_checked_call_target(function, use_span))?;
                used.push((lifecycle.constructor(), use_span));
                used.push((lifecycle.destructor(), use_span));
                continue;
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
        let Some(canonical) = catalog.roots.get(type_id) else {
            let span = self.location_span(&component.location)?;
            return Err(Box::new(TypeCheckError::phase_diagnostic(
                "ET000",
                format!(
                    "cannot issue the canonical type root for declaration {:?} (type {:?}): the type identity is absent from the issued type table",
                    component.def_id, type_id,
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
        if exact_nominal != Some(canonical) || catalog.roots.get(canonical) != Some(canonical) {
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
    ) -> Result<Option<TypeId>, type_roots::TypeRootResolutionError> {
        match type_table.get(ty) {
            Some(Type::Alias(alias)) => Ok(Some(alias.aliased)),
            Some(Type::Class(class_ty))
                if class_ty.kind == ClassKind::Connector
                    || class_ty.kind == ClassKind::Type
                    || class_ty.kind == ClassKind::Operator
                    || class_ty.kind == ClassKind::Record =>
            {
                let Some(class) = tree.get_class_by_def_id(class_ty.def_id) else {
                    return Err(type_roots::TypeRootResolutionError::UnknownTarget { source: ty });
                };
                let is_wrapper = if class_ty.kind == ClassKind::Connector {
                    Self::is_connector_alias_wrapper(class)
                } else {
                    Self::is_class_alias_wrapper(class)
                };
                if !is_wrapper {
                    return Ok(None);
                }
                Self::try_resolve_alias_target_type_id(class, type_table, type_ids_by_def_id)
                    .map(Some)
                    .ok_or(type_roots::TypeRootResolutionError::UnknownTarget { source: ty })
            }
            Some(Type::Unknown) | None => {
                Err(type_roots::TypeRootResolutionError::UnknownTarget { source: ty })
            }
            _ => Ok(None),
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

/// Build the compile-time function catalog keyed by canonical qualified name.
///
/// Every algorithmic function declaration Resolve registered is published under
/// exactly one key: its canonical qualified name from `name_map`. No import
/// alias, terminal short name, or other spelling is published: call selection
/// reads the call's Resolve-issued target `DefId`
/// (`selected_user_function_name`), so an import-visible call (qualified,
/// renamed, selective, or wildcard; MLS §13.2) selects its exact declaration by
/// identity while two imports sharing one local alias cannot collide in this
/// shared map.
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

#[cfg(test)]
mod tests;
