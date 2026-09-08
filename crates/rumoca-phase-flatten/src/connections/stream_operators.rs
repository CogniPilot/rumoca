//! MLS chapter 15 stream-operator elimination.
//!
//! This runs while Flat IR still has semantic connection sets. Downstream IRs
//! therefore never need to guess connector membership or treat stream
//! operators as runtime pass-through functions.

mod projection;

use rumoca_core::{
    BuiltinFunction, ComponentPath, ComponentRefPart, ComponentReference, DefId, Expression,
    ExpressionVisitor, FallibleExpressionRewriter, FallibleStatementRewriter, Literal, OpBinary,
    OpUnary, Reference, Span, Subscript, VarName,
};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rustc_hash::{FxHashMap, FxHashSet};

use super::FlattenError;
pub(super) use projection::StreamRewriteProjection;
use projection::rewrite_assertion_expressions;

/// The MLS recommended regularization scales epsilon from flow nominal values.
/// This value is above Rumoca's algebraic-Newton residual tolerance while small
/// relative to the default flow nominal of one.
const STREAM_RELATIVE_TOLERANCE: f64 = 1.0e-7;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum StreamOperatorRole {
    InStream,
    ActualStream,
}

#[derive(Clone, Copy)]
pub(crate) struct StreamOperatorIdentities {
    in_stream: DefId,
    actual_stream: DefId,
}

impl StreamOperatorIdentities {
    pub(crate) fn from_tree(tree: &ast::ClassTree, span: Span) -> Result<Self, FlattenError> {
        let lookup = |name: &str| {
            tree.scope_tree
                .predefined_member(&ComponentPath::from_flat_path(name))
                .ok_or_else(|| {
                    FlattenError::invalid_connection_evidence(
                        format!("Resolve did not issue the predefined `{name}` operator identity"),
                        span,
                    )
                })
        };
        let identities = Self {
            in_stream: lookup("inStream")?,
            actual_stream: lookup("actualStream")?,
        };
        if identities.in_stream == identities.actual_stream {
            return Err(FlattenError::invalid_connection_evidence(
                "Resolve issued one DefId for both predefined stream-operator roles",
                span,
            ));
        }
        Ok(identities)
    }

    #[cfg(test)]
    pub(super) fn fixture() -> Self {
        Self {
            in_stream: DefId::new(0x00fe_2001),
            actual_stream: DefId::new(0x00fe_2002),
        }
    }

    fn classify(
        self,
        name: &Reference,
        span: Span,
    ) -> Result<Option<StreamOperatorRole>, FlattenError> {
        let spelling = name.last_segment();
        let apparent = matches!(spelling, "inStream" | "actualStream");
        match name.target_def_id() {
            Some(target) if target == self.in_stream && spelling == "inStream" => {
                Ok(Some(StreamOperatorRole::InStream))
            }
            Some(target) if target == self.actual_stream && spelling == "actualStream" => {
                Ok(Some(StreamOperatorRole::ActualStream))
            }
            Some(target) if target == self.in_stream || target == self.actual_stream => {
                Err(FlattenError::invalid_connection_evidence(
                    "a stream-operator reference contradicts its Resolve-issued operator role",
                    span,
                ))
            }
            Some(_) => Ok(None),
            None if apparent => Err(FlattenError::invalid_connection_evidence(
                format!("apparent `{spelling}` call lacks exact Resolve target identity"),
                span,
            )),
            None => Ok(None),
        }
    }

    pub(super) fn reference(self, role: StreamOperatorRole, span: Span) -> Reference {
        let (spelling, def_id) = match role {
            StreamOperatorRole::InStream => ("inStream", self.in_stream),
            StreamOperatorRole::ActualStream => ("actualStream", self.actual_stream),
        };
        let component_ref = ComponentReference::construct(
            true,
            span,
            vec![ComponentRefPart {
                ident: spelling.to_string(),
                span,
                subs: Vec::new(),
                def_id,
            }],
        )
        .expect("Resolve-issued stream operator identity constructs one exact reference");
        Reference::generated_component_reference(component_ref)
    }

    pub(super) fn call(
        self,
        role: StreamOperatorRole,
        args: Vec<Expression>,
        span: Span,
    ) -> Expression {
        Expression::FunctionCall {
            name: self.reference(role, span),
            args,
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span,
        }
    }
}

#[derive(Clone)]
struct FlowCandidate {
    name: VarName,
    parent: ComponentPath,
    normalized_parent: ComponentPath,
    leaf: String,
    nominal: f64,
}

/// MLS §9.1.2 role of a connector inside one connection set.
///
/// The role fixes the flow sign convention used by MLS §9.2 and §15.2: a
/// positive `m_flow` on an inside connector leaves the connection set (it flows
/// into the component that owns the connector), while a positive `m_flow` on an
/// outside connector enters the connection set from the enclosing model.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ConnectorRole {
    Inside,
    Outside,
}

#[derive(Clone)]
struct StreamPeer {
    stream: VarName,
    flow: VarName,
    role: ConnectorRole,
}

#[derive(Clone)]
struct StreamEndpoint {
    flow: FlowCandidate,
    peers: Vec<StreamPeer>,
    epsilon: f64,
}

/// Per-connector view of the MLS §15.2 stream connection sets.
///
/// A connector can occur in two sets: as the inside connector of the set
/// declared by the enclosing model, and as the outside connector of the set
/// declared inside its own model. The two roles answer different questions, so
/// they are kept apart:
///
/// * `inside` answers `inStream(c)` — the mixing enthalpy the *environment* of
///   the component pushes into `c`, i.e. the mix of the set in which `c` is an
///   inside connector, excluding `c` itself.
/// * `outside` answers the MLS §15.2 outside-connector equation
///   `c.h_outflow = <mix of the set declared inside c's model, excluding c>`.
pub(super) struct StreamConnectionEndpoints {
    inside: StreamEndpointMap,
    outside: StreamEndpointMap,
}

struct StreamAccess {
    name: VarName,
    subscripts: Vec<Subscript>,
    indexed: bool,
    original: Expression,
    field_base: Option<(Box<Expression>, rumoca_core::DefId)>,
    span: Span,
}

type StreamEndpointMap = FxHashMap<VarName, StreamEndpoint>;

pub(super) fn plan_stream_operator_rewrite(
    model: &flat::Model,
    stream_sets: &[super::StreamConnectionSet],
    endpoints: &StreamConnectionEndpoints,
    operator_identities: StreamOperatorIdentities,
    projected_equations: &mut [super::transaction::PlannedConnectionEquation],
    projected_assertions: &mut [flat::AssertEquation],
) -> Result<StreamRewriteProjection, FlattenError> {
    let stream_variables = collect_stream_variables(model, stream_sets);
    let mut rewriter = StreamOperatorRewriter {
        endpoints: endpoints.inside.clone(),
        flow_candidates: collect_flow_candidates(model),
        declared_paths: collect_declared_paths(model),
        stream_connectors: collect_stream_connectors(&stream_variables),
        stream_variables,
        expanding: Vec::new(),
        operator_identities,
    };
    for planned in projected_equations {
        if contains_stream_operator(&planned.equation.residual, operator_identities)? {
            planned.equation.residual = rewriter.rewrite_expression(&planned.equation.residual)?;
        }
        if let Some(template) = planned
            .family
            .as_mut()
            .and_then(|family| family.template.as_mut())
            && expressions_contain_stream_operator(&template.body, operator_identities)?
        {
            template.body = rewriter.rewrite_expressions(&template.body)?;
        }
    }
    for assertion in projected_assertions {
        rewrite_assertion_expressions(assertion, &mut rewriter)?;
    }
    StreamRewriteProjection::plan(model, &mut rewriter)
}

/// Split the per-scope stream connection sets into the two per-connector views
/// described on [`StreamConnectionEndpoints`].
pub(super) fn build_stream_connection_endpoints(
    model: &flat::Model,
    stream_sets: &[super::StreamConnectionSet],
    outside_roles: &super::InterfaceStreamEndpointsByScope,
) -> Result<StreamConnectionEndpoints, FlattenError> {
    let flow_candidates = collect_flow_candidates(model);
    let mut result = StreamConnectionEndpoints {
        inside: FxHashMap::default(),
        outside: FxHashMap::default(),
    };
    for stream_set in stream_sets {
        let flows = stream_set
            .variables
            .iter()
            .map(|stream| associated_flow(model, &flow_candidates, stream, stream_set.span))
            .collect::<Result<Vec<_>, _>>()?;
        let epsilon = stream_epsilon(&flows);
        let roles = stream_set
            .variables
            .iter()
            .map(|stream| {
                if super::is_outside_stream_var_for_scope(stream, &stream_set.scope, outside_roles)
                {
                    ConnectorRole::Outside
                } else {
                    ConnectorRole::Inside
                }
            })
            .collect::<Vec<_>>();
        for (index, stream) in stream_set.variables.iter().enumerate() {
            let peers = stream_set
                .variables
                .iter()
                .enumerate()
                .filter(|(peer_index, _)| *peer_index != index)
                .map(|(peer_index, peer)| StreamPeer {
                    stream: peer.clone(),
                    flow: flows[peer_index].name.clone(),
                    role: roles[peer_index],
                })
                .collect();
            let endpoint = StreamEndpoint {
                flow: flows[index].clone(),
                peers,
                epsilon,
            };
            let target = match roles[index] {
                ConnectorRole::Inside => &mut result.inside,
                ConnectorRole::Outside => &mut result.outside,
            };
            target.insert(stream.clone(), endpoint);
        }
    }
    Ok(result)
}

impl StreamConnectionEndpoints {
    /// Right-hand side of the MLS §15.2 connection equation generated for the
    /// outside connector `stream`: the mixing enthalpy of the set declared
    /// inside the connector's own model, with `stream` itself excluded.
    ///
    /// Returns `None` when `stream` is not an outside member of any stream
    /// connection set, which is the MLS §15.2 "not connected" case where the
    /// equation degenerates to `c.h_outflow = inStream(c.h_outflow)`.
    pub(super) fn outside_equation_rhs(
        &self,
        stream: &VarName,
        operator_identities: StreamOperatorIdentities,
        span: Span,
    ) -> Option<Expression> {
        let endpoint = self.outside.get(stream)?;
        let access = StreamAccess {
            name: stream.clone(),
            subscripts: Vec::new(),
            indexed: false,
            original: variable_reference(stream, &[], span),
            field_base: None,
            span,
        };
        Some(in_stream_expression(
            &access,
            endpoint,
            operator_identities,
            span,
        ))
    }
}

fn collect_flow_candidates(model: &flat::Model) -> Vec<FlowCandidate> {
    model
        .variables
        .values()
        .filter(|variable| variable.flow)
        .filter_map(|variable| {
            let path = ComponentPath::from_flat_path(variable.name.as_str());
            let parent = path.parent()?;
            let leaf = path.parts().last()?.clone();
            Some(FlowCandidate {
                name: variable.name.clone(),
                normalized_parent: normalized_path(&parent),
                parent,
                leaf,
                nominal: numeric_nominal(variable.nominal.as_ref()),
            })
        })
        .collect()
}

fn collect_stream_variables(
    model: &flat::Model,
    stream_sets: &[super::StreamConnectionSet],
) -> FxHashSet<VarName> {
    model
        .variables
        .values()
        .filter(|variable| variable.stream)
        .map(|variable| variable.name.clone())
        .chain(
            stream_sets
                .iter()
                .flat_map(|stream_set| stream_set.variables.iter().cloned()),
        )
        .collect()
}

/// Normalized paths of every declared Flat variable.
///
/// A stream member that expands to zero scalar variables (MLS §15.2 permits a
/// zero-sized array-valued stream member, e.g. `stream Real Xi_outflow[nXi]`
/// with `nXi = 0`) is absent here, which is what distinguishes it from an
/// ordinary non-stream reference.
fn collect_declared_paths(model: &flat::Model) -> FxHashSet<ComponentPath> {
    model
        .variables
        .keys()
        .map(|name| normalized_path(&ComponentPath::from_flat_path(name.as_str())))
        .collect()
}

/// Normalized connector paths that own at least one declared stream member.
fn collect_stream_connectors(stream_variables: &FxHashSet<VarName>) -> FxHashSet<ComponentPath> {
    stream_variables
        .iter()
        .filter_map(|name| normalized_path(&ComponentPath::from_flat_path(name.as_str())).parent())
        .collect()
}

fn associated_flow(
    model: &flat::Model,
    candidates: &[FlowCandidate],
    stream: &VarName,
    span: Span,
) -> Result<FlowCandidate, FlattenError> {
    let stream_declaration = stream_variable(model, stream, span)?;
    let stream_path = ComponentPath::from_flat_path(stream.as_str());
    let Some(parent) = stream_path.parent() else {
        return Err(stream_flow_error(stream, stream_declaration));
    };
    let exact = candidates
        .iter()
        .filter(|candidate| candidate.parent == parent)
        .collect::<Vec<_>>();
    if let [candidate] = exact.as_slice() {
        return Ok((*candidate).clone());
    }
    if !exact.is_empty() {
        return Err(stream_flow_error(stream, stream_declaration));
    }

    let normalized_parent = normalized_path(&parent);
    let normalized = candidates
        .iter()
        .filter(|candidate| candidate.normalized_parent == normalized_parent)
        .collect::<Vec<_>>();
    let [candidate] = normalized.as_slice() else {
        return Err(stream_flow_error(stream, stream_declaration));
    };
    let flow_path = parent.join(&ComponentPath::from_parts([candidate.leaf.clone()]));
    let mut candidate = (*candidate).clone();
    candidate.name = VarName::new(flow_path.to_flat_string());
    Ok(candidate)
}

fn normalized_path(path: &ComponentPath) -> ComponentPath {
    ComponentPath::from_parts(
        path.parts()
            .iter()
            .map(|part| rumoca_core::strip_array_index(part).to_string()),
    )
}

fn stream_flow_error(stream: &VarName, variable: &flat::Variable) -> FlattenError {
    FlattenError::unsupported_equation(
        format!(
            "stream variable `{}` does not have exactly one scalar flow variable at the same connector level",
            stream.as_str()
        ),
        variable.source_span,
    )
}

fn stream_variable<'a>(
    model: &'a flat::Model,
    stream: &VarName,
    span: Span,
) -> Result<&'a flat::Variable, FlattenError> {
    let evidence = super::require_connection_declaration(model, stream, span)?;
    Ok(evidence.declaration())
}

fn numeric_nominal(nominal: Option<&Expression>) -> f64 {
    let value = match nominal {
        Some(Expression::Literal {
            value: Literal::Real(value),
            ..
        }) => *value,
        Some(Expression::Literal {
            value: Literal::Integer(value),
            ..
        }) => *value as f64,
        _ => 1.0,
    };
    if value.is_finite() && value > 0.0 {
        value
    } else {
        1.0
    }
}

fn stream_epsilon(flows: &[FlowCandidate]) -> f64 {
    let minimum_nominal = flows
        .iter()
        .map(|flow| flow.nominal)
        .reduce(f64::min)
        .unwrap_or(1.0);
    STREAM_RELATIVE_TOLERANCE * minimum_nominal
}

struct StreamOperatorRewriter {
    endpoints: StreamEndpointMap,
    flow_candidates: Vec<FlowCandidate>,
    stream_variables: FxHashSet<VarName>,
    declared_paths: FxHashSet<ComponentPath>,
    stream_connectors: FxHashSet<ComponentPath>,
    /// Stream variables whose operator expansion is currently in progress.
    ///
    /// A mix can name `inStream()` of an outside peer, which is resolved one
    /// hierarchy level up. The stack bounds that walk and turns an ill-formed
    /// self-referential set into a diagnostic instead of unbounded recursion.
    expanding: Vec<VarName>,
    operator_identities: StreamOperatorIdentities,
}

impl StreamOperatorRewriter {
    fn rewrite_stream_call(
        &mut self,
        operator: StreamOperatorRole,
        args: &[Expression],
        span: Span,
    ) -> Result<Expression, FlattenError> {
        let operator_name = match operator {
            StreamOperatorRole::InStream => "inStream",
            StreamOperatorRole::ActualStream => "actualStream",
        };
        let [argument] = args else {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "{operator_name}() requires one stream-variable reference, got {} arguments",
                    args.len()
                ),
                span,
            ));
        };
        let access = stream_access(argument).ok_or_else(|| {
            FlattenError::unsupported_equation(
                format!("{operator_name}() requires one stream-variable reference"),
                span,
            )
        })?;
        if !stream_variable_is_valid(&access.name, &self.stream_variables) {
            if self.is_empty_stream_member(&access.name) {
                return Ok(empty_stream_result(access.span));
            }
            return Err(FlattenError::unsupported_equation(
                format!(
                    "{operator_name}() argument `{}` is not a stream variable",
                    access.name.as_str()
                ),
                access.span,
            ));
        }

        let indexed_matches = self.indexed_endpoint_matches(&access.name);
        let expanded = if indexed_matches.len() > 1 {
            indexed_endpoint_expression(
                operator,
                &access,
                indexed_matches,
                self.operator_identities,
                span,
            )?
        } else {
            let endpoint = self.endpoint_for(&access.name, span)?;
            if operator == StreamOperatorRole::ActualStream {
                actual_stream_expression(&access, &endpoint, self.operator_identities, span)
            } else {
                in_stream_expression(&access, &endpoint, self.operator_identities, span)
            }
        };
        self.resolve_nested_stream_operators(&access.name, expanded, span)
    }

    /// Expand the `inStream()` of outside peers left inside a lowered mix.
    ///
    /// Each nested operator belongs to a connection set one level up the
    /// hierarchy, so the walk terminates; a set that names itself is rejected
    /// rather than expanded forever.
    fn resolve_nested_stream_operators(
        &mut self,
        stream: &VarName,
        expression: Expression,
        span: Span,
    ) -> Result<Expression, FlattenError> {
        if !contains_stream_operator(&expression, self.operator_identities)? {
            return Ok(expression);
        }
        if self.expanding.iter().any(|active| active == stream) {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "stream connection sets containing `{}` are mutually recursive: its mixing enthalpy depends on itself",
                    stream.as_str()
                ),
                span,
            ));
        }
        self.expanding.push(stream.clone());
        let resolved = self.rewrite_expression(&expression);
        self.expanding.pop();
        resolved
    }

    /// True when `stream` names a member of a stream connector that expanded to
    /// no scalar Flat variables at all.
    ///
    /// MLS §15.2 defines `inStream`/`actualStream` elementwise over an
    /// array-valued stream member, so a zero-sized member such as
    /// `stream Medium.MassFraction Xi_outflow[Medium.nXi]` with `nXi = 0` is
    /// legal and simply yields a zero-sized result. Such a member leaves no
    /// scalar variable behind after array expansion, so it can only be told
    /// apart from a genuine non-stream reference by the absence of any declared
    /// variable at that path combined with the presence of other stream members
    /// on the same connector.
    fn is_empty_stream_member(&self, stream: &VarName) -> bool {
        let normalized = normalized_path(&ComponentPath::from_flat_path(stream.as_str()));
        if self.declared_paths.contains(&normalized) {
            return false;
        }
        normalized
            .parent()
            .is_some_and(|parent| self.stream_connectors.contains(&parent))
    }

    fn indexed_endpoint_matches(&self, stream: &VarName) -> Vec<(VarName, StreamEndpoint)> {
        let normalized = normalized_path(&ComponentPath::from_flat_path(stream.as_str()));
        let mut matching = self
            .endpoints
            .iter()
            .filter(|(candidate, _)| {
                normalized_path(&ComponentPath::from_flat_path(candidate.as_str())) == normalized
            })
            .map(|(candidate, endpoint)| (candidate.clone(), endpoint.clone()))
            .collect::<Vec<_>>();
        matching.sort_by(|(left, _), (right, _)| left.as_str().cmp(right.as_str()));
        matching
    }

    fn endpoint_for(&self, stream: &VarName, span: Span) -> Result<StreamEndpoint, FlattenError> {
        if let Some(endpoint) = self.endpoints.get(stream) {
            return Ok(endpoint.clone());
        }
        let normalized = normalized_path(&ComponentPath::from_flat_path(stream.as_str()));
        let matching = self
            .endpoints
            .iter()
            .filter(|(candidate, _)| {
                normalized_path(&ComponentPath::from_flat_path(candidate.as_str())) == normalized
            })
            .map(|(_, endpoint)| endpoint)
            .collect::<Vec<_>>();
        if let [endpoint] = matching.as_slice() {
            return Ok((*endpoint).clone());
        }
        if !matching.is_empty() {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "stream variable `{}` ambiguously matches multiple connection-set endpoints",
                    stream.as_str()
                ),
                span,
            ));
        }
        let flow = associated_flow_from_candidates(&self.flow_candidates, stream).ok_or_else(|| {
            FlattenError::unsupported_equation(
                format!(
                    "stream variable `{}` does not have exactly one scalar flow variable at the same connector level",
                    stream.as_str()
                ),
                span,
            )
        })?;
        Ok(StreamEndpoint {
            epsilon: STREAM_RELATIVE_TOLERANCE * flow.nominal,
            flow,
            peers: Vec::new(),
        })
    }
}

fn indexed_endpoint_expression(
    operator: StreamOperatorRole,
    access: &StreamAccess,
    matches: Vec<(VarName, StreamEndpoint)>,
    operator_identities: StreamOperatorIdentities,
    span: Span,
) -> Result<Expression, FlattenError> {
    let operator_name = match operator {
        StreamOperatorRole::InStream => "inStream",
        StreamOperatorRole::ActualStream => "actualStream",
    };
    let access_indices = connector_access_indices(access);
    let mut choices = Vec::with_capacity(matches.len());
    for (stream, endpoint) in matches {
        let candidate_indices = embedded_parent_indices(&stream).ok_or_else(|| {
            indexed_stream_error(
                operator_name,
                access,
                "connection member has a non-integer index",
            )
        })?;
        let condition = index_match_condition(&access_indices, &candidate_indices, span)
            .ok_or_else(|| {
                indexed_stream_error(
                    operator_name,
                    access,
                    "connector access rank does not match connection members",
                )
            })?;
        let concrete = concrete_stream_access(access, stream, span);
        let expression = if operator == StreamOperatorRole::ActualStream {
            actual_stream_expression(&concrete, &endpoint, operator_identities, span)
        } else {
            in_stream_expression(&concrete, &endpoint, operator_identities, span)
        };
        choices.push((condition, expression));
    }
    let Some((_, else_branch)) = choices.pop() else {
        return Err(indexed_stream_error(
            operator_name,
            access,
            "connection set is empty",
        ));
    };
    Ok(Expression::If {
        branches: choices,
        else_branch: Box::new(else_branch),
        span,
    })
}

fn indexed_stream_error(operator: &str, access: &StreamAccess, description: &str) -> FlattenError {
    FlattenError::unsupported_equation(
        format!(
            "{operator}() cannot resolve indexed stream reference `{}`: {description}",
            access.name.as_str()
        ),
        access.span,
    )
}

fn connector_access_indices(access: &StreamAccess) -> Vec<Subscript> {
    let mut result = Vec::new();
    if let Some((base, _)) = &access.field_base {
        collect_reference_subscripts(base, &mut result);
    }
    result
}

fn collect_reference_subscripts(expression: &Expression, result: &mut Vec<Subscript>) {
    match expression {
        Expression::VarRef { subscripts, .. } => result.extend(subscripts.iter().cloned()),
        Expression::Index {
            base, subscripts, ..
        } => {
            collect_reference_subscripts(base, result);
            result.extend(subscripts.iter().cloned());
        }
        Expression::FieldAccess { base, .. } => collect_reference_subscripts(base, result),
        _ => {}
    }
}

fn embedded_parent_indices(stream: &VarName) -> Option<Vec<i64>> {
    let parent = ComponentPath::from_flat_path(stream.as_str()).parent()?;
    let mut result = Vec::new();
    for part in parent.parts() {
        let mut base = part.as_str();
        let mut groups = Vec::new();
        while let Some((next, raw)) = rumoca_core::split_trailing_subscript_suffix(base) {
            groups.push(raw);
            base = next;
        }
        for group in groups.into_iter().rev() {
            result.extend(
                group
                    .split(',')
                    .map(str::trim)
                    .map(str::parse::<i64>)
                    .collect::<Result<Vec<_>, _>>()
                    .ok()?,
            );
        }
    }
    Some(result)
}

fn index_match_condition(
    access: &[Subscript],
    candidate: &[i64],
    span: Span,
) -> Option<Expression> {
    if access.is_empty() || access.len() != candidate.len() {
        return None;
    }
    access
        .iter()
        .zip(candidate)
        .map(|(subscript, value)| {
            let lhs = match subscript {
                Subscript::Index { value, .. } => integer_literal(*value, span),
                Subscript::Expr { expr, .. } => expr.as_ref().clone(),
                Subscript::Colon { .. } => return None,
            };
            Some(binary(
                OpBinary::Eq,
                lhs,
                integer_literal(*value, span),
                span,
            ))
        })
        .collect::<Option<Vec<_>>>()?
        .into_iter()
        .reduce(|lhs, rhs| binary(OpBinary::And, lhs, rhs, span))
}

fn concrete_stream_access(access: &StreamAccess, stream: VarName, span: Span) -> StreamAccess {
    StreamAccess {
        name: stream.clone(),
        subscripts: access.subscripts.clone(),
        indexed: false,
        original: variable_reference(&stream, &access.subscripts, span),
        field_base: None,
        span: access.span,
    }
}

impl FallibleExpressionRewriter for StreamOperatorRewriter {
    type Error = FlattenError;

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, Self::Error> {
        if let Expression::FunctionCall {
            name, args, span, ..
        } = expr
            && let Some(operator) = self.operator_identities.classify(name, *span)?
        {
            return self.rewrite_stream_call(operator, args, *span);
        }
        self.walk_expression(expr)
    }
}

impl FallibleStatementRewriter for StreamOperatorRewriter {}

fn stream_access(expression: &Expression) -> Option<StreamAccess> {
    match expression {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => Some(StreamAccess {
            name: name.var_name().clone(),
            subscripts: subscripts.clone(),
            indexed: false,
            original: expression.clone(),
            field_base: None,
            span: *span,
        }),
        Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let mut access = stream_access(base)?;
            access.subscripts.extend(subscripts.iter().cloned());
            access.indexed = true;
            access.original = expression.clone();
            access.span = *span;
            Some(access)
        }
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => {
            let base_name = reference_name_without_subscripts(base)?;
            let path = ComponentPath::from_flat_path(base_name.as_str())
                .join(&ComponentPath::from_parts([field.clone()]));
            Some(StreamAccess {
                name: VarName::new(path.to_flat_string()),
                subscripts: Vec::new(),
                indexed: false,
                original: expression.clone(),
                field_base: Some((base.clone(), *field_def_id)),
                span: *span,
            })
        }
        _ => None,
    }
}

fn reference_name_without_subscripts(expression: &Expression) -> Option<VarName> {
    match expression {
        Expression::VarRef { name, .. } => Some(name.var_name().clone()),
        Expression::Index { base, .. } => reference_name_without_subscripts(base),
        Expression::FieldAccess { base, field, .. } => {
            let base_name = reference_name_without_subscripts(base)?;
            let path = ComponentPath::from_flat_path(base_name.as_str())
                .join(&ComponentPath::from_parts([field.clone()]));
            Some(VarName::new(path.to_flat_string()))
        }
        _ => None,
    }
}

fn stream_variable_is_valid(stream: &VarName, stream_variables: &FxHashSet<VarName>) -> bool {
    if stream_variables.contains(stream) {
        return true;
    }
    let normalized = normalized_path(&ComponentPath::from_flat_path(stream.as_str()));
    stream_variables.iter().any(|candidate| {
        normalized_path(&ComponentPath::from_flat_path(candidate.as_str())) == normalized
    })
}

fn associated_flow_from_candidates(
    candidates: &[FlowCandidate],
    stream: &VarName,
) -> Option<FlowCandidate> {
    let parent = ComponentPath::from_flat_path(stream.as_str()).parent()?;
    let exact = candidates
        .iter()
        .filter(|candidate| candidate.parent == parent)
        .collect::<Vec<_>>();
    if let [candidate] = exact.as_slice() {
        return Some((*candidate).clone());
    }
    if !exact.is_empty() {
        return None;
    }
    let normalized_parent = normalized_path(&parent);
    let normalized = candidates
        .iter()
        .filter(|candidate| candidate.normalized_parent == normalized_parent)
        .collect::<Vec<_>>();
    let [candidate] = normalized.as_slice() else {
        return None;
    };
    let flow_path = parent.join(&ComponentPath::from_parts([candidate.leaf.clone()]));
    let mut result = (*candidate).clone();
    result.name = VarName::new(flow_path.to_flat_string());
    Some(result)
}

/// MLS §15.2 mixing enthalpy of `endpoint`'s connection set with the endpoint
/// itself excluded.
///
/// With a single remaining peer the weight cancels out of numerator and
/// denominator, so the mix is that peer's contributed value verbatim.
fn in_stream_expression(
    access: &StreamAccess,
    endpoint: &StreamEndpoint,
    operator_identities: StreamOperatorIdentities,
    span: Span,
) -> Expression {
    match endpoint.peers.as_slice() {
        [] => stream_reference(&access.name, access, span),
        [peer] => peer_stream_value(peer, access, operator_identities, span),
        peers => weighted_stream_mean(peers, access, endpoint, operator_identities, span),
    }
}

/// Value a peer contributes to the mixing enthalpy (MLS §15.2).
///
/// An inside connector contributes the value its component pushes out,
/// `m_j.h_outflow`. An outside connector contributes what the *enclosing* model
/// pushes into this set, `inStream(c_k.h_outflow)`; that nested operator is
/// resolved against the set in which `c_k` is an inside connector, one level up
/// the hierarchy.
fn peer_stream_value(
    peer: &StreamPeer,
    access: &StreamAccess,
    operator_identities: StreamOperatorIdentities,
    span: Span,
) -> Expression {
    let reference = stream_reference(&peer.stream, access, span);
    match peer.role {
        ConnectorRole::Inside => reference,
        ConnectorRole::Outside => {
            operator_identities.call(StreamOperatorRole::InStream, vec![reference], span)
        }
    }
}

fn actual_stream_expression(
    access: &StreamAccess,
    endpoint: &StreamEndpoint,
    operator_identities: StreamOperatorIdentities,
    span: Span,
) -> Expression {
    let condition = binary(
        OpBinary::Gt,
        stream_member_reference(&endpoint.flow.name, access, span),
        real_literal(0.0, span),
        span,
    );
    Expression::If {
        branches: vec![(
            condition,
            in_stream_expression(access, endpoint, operator_identities, span),
        )],
        else_branch: Box::new(stream_reference(&access.name, access, span)),
        span,
    }
}

fn weighted_stream_mean(
    peers: &[StreamPeer],
    access: &StreamAccess,
    endpoint: &StreamEndpoint,
    operator_identities: StreamOperatorIdentities,
    span: Span,
) -> Expression {
    let weighted_values = peers
        .iter()
        .map(|peer| {
            let weight = positive_flow_weight(peer, endpoint.epsilon, span);
            binary(
                OpBinary::Mul,
                weight,
                peer_stream_value(peer, access, operator_identities, span),
                span,
            )
        })
        .collect::<Vec<_>>();
    let weights = peers
        .iter()
        .map(|peer| positive_flow_weight(peer, endpoint.epsilon, span))
        .collect::<Vec<_>>();
    binary(
        OpBinary::Div,
        sum_expressions(weighted_values, span),
        sum_expressions(weights, span),
        span,
    )
}

/// Regularized MLS §15.2 inflow weight of a peer.
///
/// The flow into the connection set is `-m_j.m_flow` for an inside connector and
/// `+c_k.m_flow` for an outside connector (MLS §9.2 sign convention). `epsilon`
/// replaces the specified `max(..., 0)` so the mixing denominator can never
/// vanish (STRM-009).
fn positive_flow_weight(peer: &StreamPeer, epsilon: f64, span: Span) -> Expression {
    let flow = variable_reference(&peer.flow, &[], span);
    let inflow = match peer.role {
        ConnectorRole::Inside => Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(flow),
            span,
        },
        ConnectorRole::Outside => flow,
    };
    Expression::BuiltinCall {
        function: BuiltinFunction::Max,
        args: vec![inflow, real_literal(epsilon, span)],
        span,
    }
}

fn sum_expressions(expressions: Vec<Expression>, span: Span) -> Expression {
    expressions
        .into_iter()
        .reduce(|lhs, rhs| binary(OpBinary::Add, lhs, rhs, span))
        .unwrap_or_else(|| real_literal(0.0, span))
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn variable_reference(name: &VarName, subscripts: &[Subscript], span: Span) -> Expression {
    Expression::VarRef {
        name: name.clone().into(),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn stream_reference(name: &VarName, access: &StreamAccess, span: Span) -> Expression {
    if name == &access.name {
        return access.original.clone();
    }
    stream_member_reference(name, access, span)
}

fn stream_member_reference(name: &VarName, access: &StreamAccess, span: Span) -> Expression {
    if let Some((base, field_def_id)) = &access.field_base {
        let parent = ComponentPath::from_flat_path(name.as_str()).parent();
        let field = ComponentPath::from_flat_path(name.as_str())
            .parts()
            .last()
            .cloned()
            .unwrap_or_else(|| name.as_str().to_string());
        let Some(parent) = parent else {
            return variable_reference(name, &access.subscripts, span);
        };
        let reference = Expression::FieldAccess {
            base: Box::new(retarget_reference_base(
                base,
                &VarName::new(parent.to_flat_string()),
                span,
            )),
            field,
            field_def_id: *field_def_id,
            span,
        };
        if access.subscripts.is_empty() {
            return reference;
        }
        return Expression::Index {
            base: Box::new(reference),
            subscripts: access.subscripts.clone(),
            span,
        };
    }
    let reference = variable_reference(name, &[], span);
    if access.indexed {
        Expression::Index {
            base: Box::new(reference),
            subscripts: access.subscripts.clone(),
            span,
        }
    } else {
        variable_reference(name, &access.subscripts, span)
    }
}

fn retarget_reference_base(expression: &Expression, target: &VarName, span: Span) -> Expression {
    match expression {
        Expression::VarRef { subscripts, .. } => variable_reference(target, subscripts, span),
        Expression::Index {
            base, subscripts, ..
        } => Expression::Index {
            base: Box::new(retarget_reference_base(base, target, span)),
            subscripts: subscripts.clone(),
            span,
        },
        _ => variable_reference(target, &[], span),
    }
}

/// Result of a stream operator applied to a zero-sized stream member (MLS §15.2).
fn empty_stream_result(span: Span) -> Expression {
    Expression::Array {
        elements: Vec::new(),
        is_matrix: false,
        span,
    }
}

fn real_literal(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn integer_literal(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

struct StreamOperatorDetector {
    identities: StreamOperatorIdentities,
    found: bool,
    error: Option<FlattenError>,
}

impl ExpressionVisitor for StreamOperatorDetector {
    fn visit_expression(&mut self, expression: &Expression) {
        if self.found || self.error.is_some() {
            return;
        }
        if let Expression::FunctionCall { name, span, .. } = expression {
            match self.identities.classify(name, *span) {
                Ok(Some(_)) => {
                    self.found = true;
                    return;
                }
                Ok(None) => {}
                Err(error) => {
                    self.error = Some(error);
                    return;
                }
            }
        }
        self.walk_expression(expression);
    }
}

impl flat::StatementVisitor for StreamOperatorDetector {}

fn contains_stream_operator(
    expression: &Expression,
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    let mut detector = StreamOperatorDetector {
        identities,
        found: false,
        error: None,
    };
    ExpressionVisitor::visit_expression(&mut detector, expression);
    match detector.error {
        Some(error) => Err(error),
        None => Ok(detector.found),
    }
}

fn expressions_contain_stream_operator(
    expressions: &[Expression],
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    for expression in expressions {
        if contains_stream_operator(expression, identities)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn statements_contain_stream_operator(
    statements: &[rumoca_core::Statement],
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    let mut detector = StreamOperatorDetector {
        identities,
        found: false,
        error: None,
    };
    for statement in statements {
        flat::StatementVisitor::visit_statement(&mut detector, statement);
        if detector.found || detector.error.is_some() {
            break;
        }
    }
    match detector.error {
        Some(error) => Err(error),
        None => Ok(detector.found),
    }
}

fn when_chain_contains_stream_operator(
    chain: &flat::WhenChain,
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    for branch in chain.branches() {
        if contains_stream_operator(&branch.condition, identities)?
            || when_equations_contain_stream_operator(&branch.equations, identities)?
        {
            return Ok(true);
        }
    }
    Ok(false)
}

fn when_equations_contain_stream_operator(
    equations: &[flat::WhenEquation],
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    for equation in equations {
        let found = match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                contains_stream_operator(value, identities)?
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                contains_stream_operator(condition, identities)?
                    || contains_stream_operator(message, identities)?
                    || match level.as_deref() {
                        Some(level) => contains_stream_operator(level, identities)?,
                        None => false,
                    }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                contains_stream_operator(message, identities)?
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => conditional_when_equations_contain_stream_operator(
                branches,
                else_branch.as_deref(),
                identities,
            )?,
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                contains_stream_operator(function, identities)?
            }
        };
        if found {
            return Ok(true);
        }
    }
    Ok(false)
}

fn conditional_when_equations_contain_stream_operator(
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: Option<&[flat::WhenEquation]>,
    identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    for (condition, branch) in branches {
        if contains_stream_operator(condition, identities)?
            || when_equations_contain_stream_operator(branch, identities)?
        {
            return Ok(true);
        }
    }
    match else_branch {
        Some(branch) => when_equations_contain_stream_operator(branch, identities),
        None => Ok(false),
    }
}

fn rewrite_when_chain_slice(
    chains: &mut [flat::WhenChain],
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for chain in chains {
        for branch in chain.branches_mut() {
            branch.condition = rewriter.rewrite_expression(&branch.condition)?;
            rewrite_when_equations(&mut branch.equations, rewriter)?;
        }
    }
    Ok(())
}

fn rewrite_when_equations(
    equations: &mut [flat::WhenEquation],
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                *value = rewriter.rewrite_expression(value)?;
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                *condition = rewriter.rewrite_expression(condition)?;
                *message = rewriter.rewrite_expression(message)?;
                if let Some(level) = level.as_deref_mut() {
                    *level = rewriter.rewrite_expression(level)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                *message = rewriter.rewrite_expression(message)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch) in branches {
                    *condition = rewriter.rewrite_expression(condition)?;
                    rewrite_when_equations(branch, rewriter)?;
                }
                if let Some(else_branch) = else_branch {
                    rewrite_when_equations(else_branch, rewriter)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function)?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod selection_tests {
    use super::*;

    #[test]
    fn malformed_selected_stream_never_reaches_stream_semantics() {
        let mut model = flat::Model::new();
        model.add_variable(
            VarName::new("port.h_outflow"),
            flat::Variable {
                name: VarName::new("port.h_outflow"),
                dims: vec![2],
                stream: true,
                ..flat::Variable::empty_with_span(Span::DUMMY)
            },
        );

        let error = stream_variable(
            &model,
            &VarName::new("port.h_outflow[not_an_integer]"),
            Span::DUMMY,
        )
        .expect_err("malformed selection evidence cannot authorize stream semantics");

        assert!(error.to_string().contains("concrete integer indices"));
    }
}
