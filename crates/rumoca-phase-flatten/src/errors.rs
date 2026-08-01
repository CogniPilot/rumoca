//! Phase-local error types for flattening.
//!
//! Error codes: EF0xx for flatten phase (per SPEC_0008).
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{
    BoxedResult, Diagnostic as CommonDiagnostic, PhaseError, Span, error_constructor,
    miette_phase_error_to_diagnostic,
};
use thiserror::Error;

/// Type alias for flatten results with boxed errors.
pub type FlattenResult<T> = BoxedResult<T, FlattenError>;

/// Errors that can occur during flattening.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum FlattenError {
    /// A variable was referenced but not found.
    #[error("undefined variable: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF001),
        help("check that the variable is declared in scope")
    )]
    UndefinedVariable {
        name: String,
        #[label("referenced here")]
        span: Span,
    },

    /// A connection involves incompatible connector types.
    #[error("incompatible connector types in connection: {a} and {b}")]
    #[diagnostic(
        code(rumoca::flatten::EF002),
        help("MLS §9.1: connected components must have compatible connector types")
    )]
    IncompatibleConnectors {
        a: String,
        b: String,
        #[label("connection here")]
        span: Span,
    },

    /// A flow variable is missing in a connector.
    #[error("flow variable not found in connector: {connector}.{flow_var}")]
    #[diagnostic(
        code(rumoca::flatten::EF003),
        help("MLS §9.2: flow variables must be declared in connectors")
    )]
    MissingFlowVariable { connector: String, flow_var: String },

    /// Unsupported equation form.
    #[error("unsupported equation form: {description}")]
    #[diagnostic(code(rumoca::flatten::EF004))]
    UnsupportedEquation {
        description: String,
        #[label("unsupported equation")]
        span: Span,
    },

    /// Internal error during flattening.
    #[error("internal flatten error: {0}")]
    #[diagnostic(code(rumoca::flatten::EF005))]
    Internal(String),

    /// A function call binds its argument slots incorrectly (MLS §12.4.1).
    #[error("invalid call of function `{function}`: {reason}")]
    #[diagnostic(
        code(rumoca::flatten::EF016),
        help(
            "MLS §12.4.1: each input slot is filled by exactly one positional or named argument; slots without defaults must be filled"
        )
    )]
    InvalidFunctionCallArgs {
        function: String,
        reason: String,
        #[label("function call here")]
        span: Span,
    },

    // EF018 is reserved for the deferred MLS §12.6.1 record-cast check.
    // Note: EF006 was EventTriggerOutsideWhen, removed per MLS Appendix B which
    // allows edge()/change() in discrete equations. Code reserved for future use.
    // Note: EF007 (UnevaluableDimensions) removed - typecheck phase (ET004) now handles this
    // per SPEC_0007, which assigns dimension evaluation to typecheck.
    /// Source-scope metadata required for Modelica name lookup was missing.
    #[error("missing source scope for {context}: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF008),
        help(
            "instantiate must preserve the lexical source scope used for flatten-time name lookup"
        )
    )]
    MissingSourceScope {
        name: String,
        context: String,
        #[label("instance created here")]
        span: Span,
    },

    /// Flat IR contains a callable definition that is not executable.
    #[error("invalid flat IR function binding: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF009),
        help(
            "flatten must resolve replaceable package/function bindings to executable concrete functions before producing Flat IR"
        )
    )]
    FunctionWithoutBody {
        name: String,
        #[label("non-executable function reached the flat IR boundary")]
        span: Span,
    },

    /// A primitive component reached Flat IR with a symbolic dimension that could not be resolved.
    #[error("unresolved component dimension for {name}: {expression}")]
    #[diagnostic(
        code(rumoca::flatten::EF010),
        help("flatten must resolve primitive component array dimensions before emitting Flat IR")
    )]
    UnresolvedComponentDimension {
        name: String,
        expression: String,
        #[label("dimension declared here")]
        span: Span,
    },

    /// A numeric token accepted by the parser could not be converted to a number.
    #[error("malformed numeric literal: {text}")]
    #[diagnostic(
        code(rumoca::flatten::EF011),
        help("the lexer produced a numeric token that flatten could not parse")
    )]
    MalformedNumericLiteral {
        text: String,
        #[label("malformed numeric literal")]
        span: Span,
    },

    /// Function-override rewriting did not converge within the configured fixed-point cap.
    #[error(
        "function override rewriting did not converge after {iterations} iterations ({function_count} functions collected)"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF012),
        help("flatten must reach a stable rewritten function table before emitting Flat IR")
    )]
    FunctionRewriteNoConverge {
        iterations: usize,
        function_count: usize,
    },

    /// A function output can be returned without an assignment.
    #[error("function output '{output}' is not definitely assigned in function '{function}'")]
    #[diagnostic(
        code(rumoca::flatten::EF013),
        help("MLS §12.4.4 requires every function output variable to be assigned before return")
    )]
    FunctionOutputUnassigned {
        function: String,
        output: String,
        #[label("function output may be unassigned here")]
        span: Span,
    },

    /// A variable reached flattening without a resolvable type name.
    #[error("unresolved variable type for `{name}`")]
    #[diagnostic(
        code(rumoca::flatten::EF014),
        help(
            "instantiate/typecheck must preserve a structured type id or type name before flattening variables"
        )
    )]
    UnresolvedVariableType {
        name: String,
        #[label("variable declared here")]
        span: Span,
    },

    /// A resolved class reached flattening without the DefId metadata required
    /// for scope-based lookup.
    #[error("missing resolved class metadata for `{name}` ({context})")]
    #[diagnostic(
        code(rumoca::flatten::EF015),
        help("name resolution must assign and preserve DefId metadata before flattening")
    )]
    MissingResolvedClassMetadata {
        name: String,
        context: String,
        #[label("class used here")]
        span: Span,
    },

    /// Source location metadata could not be mapped to the source text needed
    /// for a diagnostic span.
    #[error("missing source context: {reason}")]
    #[diagnostic(
        code(rumoca::flatten::EF017),
        help("earlier phases must preserve source-map entries and non-empty source locations")
    )]
    MissingSourceContext { reason: String },

    /// A function reference's display name disagreed with its structured path.
    #[error(
        "inconsistent resolved function reference: rendered `{rendered}`, structured `{structured}`"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF019),
        help("name resolution must preserve one authoritative function exposure identity")
    )]
    InconsistentFunctionReference {
        rendered: String,
        structured: String,
        #[label("conflicting function reference")]
        span: Span,
    },

    /// Function modifier materialization lacked exact exposure ownership.
    #[error("missing exact function-selection identity for `{function}`: {reason}")]
    #[diagnostic(
        code(rumoca::flatten::EF025),
        help(
            "resolve and instantiate must preserve both the exposed function declaration and selected implementation before modifier materialization"
        )
    )]
    MissingFunctionSelectionIdentity {
        function: String,
        reason: String,
        #[label("function selection is incomplete here")]
        span: Span,
    },

    /// Connecting expandable connectors would require MLS §9.1.3 member-union
    /// augmentation, which must not be approximated by connecting only the
    /// members that already exist on both sides.
    #[error(
        "expandable connector connection between `{a}` and `{b}` requires unsupported member augmentation"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF020),
        help(
            "MLS §9.1.3 requires every expandable connector in the connection set to be augmented with the union of its members"
        )
    )]
    UnsupportedExpandableConnectorAugmentation {
        a: String,
        b: String,
        #[label("this connection requires expandable-connector augmentation")]
        span: Span,
    },

    /// A constant/parameter binding expands into itself, so constant folding
    /// would never terminate.
    #[error("cyclic constant binding for `{name}`: {cycle}")]
    #[diagnostic(
        code(rumoca::flatten::EF021),
        help(
            "MLS §4.4.5: the declaration equation of a constant or parameter must not depend on itself"
        )
    )]
    CyclicConstantBinding {
        name: String,
        cycle: String,
        #[label("this reference expands into itself")]
        span: Span,
    },

    /// Required spanning-tree edges failed an MLS §9.4 construction invariant.
    #[error("invalid virtual connection graph: {detail}")]
    #[diagnostic(
        code(rumoca::flatten::EF022),
        help(
            "MLS §9.4: Connections.branch() edges form a forest and each required-edge tree contains at most one Connections.root()"
        )
    )]
    InvalidConnectionGraph {
        detail: String,
        #[label("this required edge makes the graph invalid")]
        span: Span,
    },

    /// A non-generated value reference reached the Flat boundary without
    /// either a resolved component path or a matching Flat declaration.
    #[error("unresolved flat reference: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF023),
        help(
            "resolve and instantiate must preserve structured reference identity; flatten does not infer semantic identity from rendered names"
        )
    )]
    UnresolvedFlatReference {
        name: String,
        #[label("unresolved reference reached the Flat IR boundary")]
        span: Span,
    },

    /// A variable declaration reached the Flat boundary without the resolved
    /// component path that identifies the instantiated declaration.
    #[error("flat variable is missing structured identity: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF024),
        help(
            "instantiate must preserve the resolved component path on every variable declaration"
        )
    )]
    MissingFlatVariableIdentity {
        name: String,
        #[label("this declaration has no structured Flat identity")]
        span: Span,
    },

    /// A connect endpoint subscripts a component whose declaration carries no
    /// dimensions, so the subscript selects along a dimension that does not
    /// exist.
    #[error(
        "connect endpoint `{endpoint}` subscripts `{component}`, which is declared without dimensions"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF026),
        help(
            "MLS §10.5: a subscript selects along a declared dimension, so a connect argument (MLS §9.1) may only subscript a component that declares at least that many dimensions"
        )
    )]
    SubscriptedDimensionlessConnector {
        endpoint: String,
        component: String,
        #[label("this connect endpoint is subscripted")]
        span: Span,
        #[label("declared here without dimensions")]
        declaration_span: Span,
    },

    /// A `connect` matched a `stream` primitive member against a member that is
    /// not declared `stream`.
    #[error(
        "connect matches stream variable `{stream_member}` with non-stream variable `{plain_member}`"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF027),
        help(
            "MLS §9.3: `stream variables only to other stream variables`. MLS §15.1 gives a stream variable mixing semantics instead of a connection equation, so a stream/non-stream pair has no defined equation at all"
        )
    )]
    StreamMemberPairedWithNonStream {
        stream_member: String,
        plain_member: String,
        #[label("this connection member is declared `stream`")]
        stream_span: Span,
        #[label("this connection member is not declared `stream`")]
        plain_span: Span,
    },

    /// A `connect` matched a `parameter`/`constant` primitive member against a
    /// member of higher variability.
    #[error(
        "connect matches {structural_variability} variable `{structural_member}` with non-structural variable `{variable_member}`"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF028),
        help(
            "MLS §9.3: `the primitive components may only connect parameter variables to parameter variables and constant variables to constant variables`. The same section generates an equality assertion rather than a connection equation for such a pair, so the non-structural side would be left with no equation at all"
        )
    )]
    StructuralMemberPairedWithVariable {
        structural_member: String,
        structural_variability: &'static str,
        variable_member: String,
        #[label("this connection member is a parameter or constant")]
        structural_span: Span,
        #[label("this connection member is neither a parameter nor a constant")]
        variable_span: Span,
    },
}

impl FlattenError {
    // Constructor methods using the error_constructor! macro
    error_constructor!(undefined_variable, UndefinedVariable { name: String });
    error_constructor!(
        incompatible_connectors,
        IncompatibleConnectors {
            a: String,
            b: String
        }
    );
    error_constructor!(
        unsupported_equation,
        UnsupportedEquation {
            description: String
        }
    );
    error_constructor!(
        unsupported_expandable_connector_augmentation,
        UnsupportedExpandableConnectorAugmentation {
            a: String,
            b: String
        }
    );

    /// Create a CyclicConstantBinding error.
    pub fn cyclic_constant_binding(
        name: impl Into<String>,
        cycle: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::CyclicConstantBinding {
            name: name.into(),
            cycle: cycle.into(),
            span,
        }
    }

    /// Create a SubscriptedDimensionlessConnector error.
    pub fn subscripted_dimensionless_connector(
        endpoint: impl Into<String>,
        component: impl Into<String>,
        span: rumoca_core::Span,
        declaration_span: rumoca_core::Span,
    ) -> Self {
        Self::SubscriptedDimensionlessConnector {
            endpoint: endpoint.into(),
            component: component.into(),
            span,
            declaration_span,
        }
    }

    /// Create a MissingFlowVariable error (no span).
    pub fn missing_flow_variable(
        connector: impl Into<String>,
        flow_var: impl Into<String>,
    ) -> Self {
        Self::MissingFlowVariable {
            connector: connector.into(),
            flow_var: flow_var.into(),
        }
    }

    /// Create an Internal error (no span).
    pub fn internal(message: impl Into<String>) -> Self {
        Self::Internal(message.into())
    }

    /// Create a MissingSourceScope error.
    pub fn missing_source_scope(
        name: impl Into<String>,
        context: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::MissingSourceScope {
            name: name.into(),
            context: context.into(),
            span,
        }
    }

    pub fn inconsistent_function_reference(
        rendered: impl Into<String>,
        structured: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::InconsistentFunctionReference {
            rendered: rendered.into(),
            structured: structured.into(),
            span,
        }
    }

    pub fn missing_function_selection_identity(
        function: impl Into<String>,
        reason: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::MissingFunctionSelectionIdentity {
            function: function.into(),
            reason: reason.into(),
            span,
        }
    }

    /// Create an InvalidFunctionCallArgs error.
    pub fn invalid_function_call_args(
        function: impl Into<String>,
        reason: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::InvalidFunctionCallArgs {
            function: function.into(),
            reason: reason.into(),
            span,
        }
    }

    /// Create a FunctionWithoutBody error.
    pub fn function_without_body(name: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::FunctionWithoutBody {
            name: name.into(),
            span,
        }
    }

    /// Create an UnresolvedComponentDimension error.
    pub fn unresolved_component_dimension(
        name: impl Into<String>,
        expression: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::UnresolvedComponentDimension {
            name: name.into(),
            expression: expression.into(),
            span,
        }
    }

    /// Create a MalformedNumericLiteral error.
    pub fn malformed_numeric_literal(text: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::MalformedNumericLiteral {
            text: text.into(),
            span,
        }
    }

    /// Create a FunctionRewriteNoConverge error.
    pub fn function_rewrite_no_converge(iterations: usize, function_count: usize) -> Self {
        Self::FunctionRewriteNoConverge {
            iterations,
            function_count,
        }
    }

    /// Create a FunctionOutputUnassigned error.
    pub fn function_output_unassigned(
        function: impl Into<String>,
        output: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::FunctionOutputUnassigned {
            function: function.into(),
            output: output.into(),
            span,
        }
    }

    /// Create an UnresolvedVariableType error.
    pub fn unresolved_variable_type(name: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::UnresolvedVariableType {
            name: name.into(),
            span,
        }
    }

    /// Create a MissingResolvedClassMetadata error.
    pub fn missing_resolved_class_metadata(
        name: impl Into<String>,
        context: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::MissingResolvedClassMetadata {
            name: name.into(),
            context: context.into(),
            span,
        }
    }

    /// Create a MissingSourceContext error.
    pub fn missing_source_context(reason: impl Into<String>) -> Self {
        Self::MissingSourceContext {
            reason: reason.into(),
        }
    }

    pub fn invalid_connection_graph(detail: impl Into<String>, span: Span) -> Self {
        Self::InvalidConnectionGraph {
            detail: detail.into(),
            span,
        }
    }

    pub fn unresolved_flat_reference(name: impl Into<String>, span: Span) -> Self {
        Self::UnresolvedFlatReference {
            name: name.into(),
            span,
        }
    }

    pub fn missing_flat_variable_identity(name: impl Into<String>, span: Span) -> Self {
        Self::MissingFlatVariableIdentity {
            name: name.into(),
            span,
        }
    }

    /// Create a StreamMemberPairedWithNonStream error (MLS §9.3, §15.1).
    pub fn stream_member_paired_with_non_stream(
        stream_member: impl Into<String>,
        stream_span: Span,
        plain_member: impl Into<String>,
        plain_span: Span,
    ) -> Self {
        Self::StreamMemberPairedWithNonStream {
            stream_member: stream_member.into(),
            plain_member: plain_member.into(),
            stream_span,
            plain_span,
        }
    }

    /// Create a StructuralMemberPairedWithVariable error (MLS §9.3).
    pub fn structural_member_paired_with_variable(
        structural_member: impl Into<String>,
        structural_variability: &'static str,
        structural_span: Span,
        variable_member: impl Into<String>,
        variable_span: Span,
    ) -> Self {
        Self::StructuralMemberPairedWithVariable {
            structural_member: structural_member.into(),
            structural_variability,
            variable_member: variable_member.into(),
            structural_span,
            variable_span,
        }
    }
}

impl PhaseError for FlattenError {
    fn to_diagnostic(&self) -> CommonDiagnostic {
        // Holds the multi-label span list alive for the borrow below; the
        // bridge maps `source_spans[i]` onto the i-th `#[label]` field.
        let endpoint_and_declaration;
        let member_pair;
        let source_spans: &[Span] = match self {
            Self::SubscriptedDimensionlessConnector {
                span,
                declaration_span,
                ..
            } => {
                endpoint_and_declaration = [*span, *declaration_span];
                &endpoint_and_declaration
            }
            Self::StreamMemberPairedWithNonStream {
                stream_span,
                plain_span,
                ..
            } => {
                member_pair = [*stream_span, *plain_span];
                &member_pair
            }
            Self::StructuralMemberPairedWithVariable {
                structural_span,
                variable_span,
                ..
            } => {
                member_pair = [*structural_span, *variable_span];
                &member_pair
            }
            Self::UndefinedVariable { span, .. }
            | Self::IncompatibleConnectors { span, .. }
            | Self::UnsupportedEquation { span, .. }
            | Self::InvalidFunctionCallArgs { span, .. }
            | Self::MissingSourceScope { span, .. }
            | Self::FunctionWithoutBody { span, .. }
            | Self::UnresolvedComponentDimension { span, .. }
            | Self::MalformedNumericLiteral { span, .. }
            | Self::FunctionOutputUnassigned { span, .. }
            | Self::UnresolvedVariableType { span, .. }
            | Self::MissingResolvedClassMetadata { span, .. }
            | Self::InconsistentFunctionReference { span, .. }
            | Self::MissingFunctionSelectionIdentity { span, .. }
            | Self::UnsupportedExpandableConnectorAugmentation { span, .. }
            | Self::CyclicConstantBinding { span, .. }
            | Self::InvalidConnectionGraph { span, .. }
            | Self::UnresolvedFlatReference { span, .. }
            | Self::MissingFlatVariableIdentity { span, .. } => std::slice::from_ref(span),
            Self::MissingFlowVariable { .. }
            | Self::Internal(_)
            | Self::FunctionRewriteNoConverge { .. }
            | Self::MissingSourceContext { .. } => &[],
        };
        miette_phase_error_to_diagnostic(self, source_spans)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn test_undefined_variable_error() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_flatten_errors_source_0.mo"),
            10,
            20,
        );
        let err = FlattenError::undefined_variable("x", span);
        assert_eq!(format!("{err}"), "undefined variable: x");

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::flatten::EF001".to_string()));
    }

    #[test]
    fn test_incompatible_connectors_with_help() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_flatten_errors_source_0.mo"),
            0,
            10,
        );
        let err = FlattenError::incompatible_connectors("A", "B", span);

        // Check that help text is present
        use miette::Diagnostic;
        let help = err.help().map(|h| h.to_string());
        assert!(help.is_some());
        assert!(help.unwrap().contains("MLS §9.1"));
    }

    #[test]
    fn phase_error_preserves_source_identity_and_help() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_flatten_phase_error.mo"),
            2,
            8,
        );
        let error = FlattenError::incompatible_connectors("left", "right", span);
        let diagnostic = error.to_diagnostic();

        assert_eq!(diagnostic.code.as_deref(), Some("EF002"));
        assert_eq!(diagnostic.labels[0].span, span);
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|note| note.contains("MLS §9.1"))
        );
    }

    fn member_span(start: usize) -> Span {
        Span::from_offsets(
            SourceId::from_source_name("phase_flatten_member_pairing.mo"),
            start,
            start + 4,
        )
    }

    /// Both member declarations reach the rendered diagnostic, in the order the
    /// `#[label]` fields declare them. SPEC_0008: a pairing rejection points at
    /// the two declarations that disagree, not only at one of them.
    #[test]
    fn stream_pairing_error_labels_both_member_declarations() {
        let error = FlattenError::stream_member_paired_with_non_stream(
            "a.h_outflow",
            member_span(10),
            "b.h_outflow",
            member_span(40),
        );
        let diagnostic = error.to_diagnostic();

        assert_eq!(diagnostic.code.as_deref(), Some("EF027"));
        assert_eq!(diagnostic.labels.len(), 2);
        assert_eq!(diagnostic.labels[0].span, member_span(10));
        assert_eq!(diagnostic.labels[1].span, member_span(40));
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|note| note.contains("MLS §9.3"))
        );
    }

    #[test]
    fn variability_pairing_error_labels_both_member_declarations() {
        let error = FlattenError::structural_member_paired_with_variable(
            "a.m",
            "parameter",
            member_span(10),
            "b.m",
            member_span(40),
        );
        let diagnostic = error.to_diagnostic();

        assert_eq!(diagnostic.code.as_deref(), Some("EF028"));
        assert_eq!(diagnostic.labels.len(), 2);
        assert_eq!(diagnostic.labels[0].span, member_span(10));
        assert_eq!(diagnostic.labels[1].span, member_span(40));
        // The pairing clause this rejection quotes lives in MLS 3.6 §9.3, the
        // same section SPEC_0022 files CONN-028 under — not §9.1, which only
        // forbids declaring a *connector component* parameter/constant (ER027).
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|note| note.contains("MLS §9.3"))
        );
    }
}
