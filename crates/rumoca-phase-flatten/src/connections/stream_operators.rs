//! MLS chapter 15 stream-operator elimination.
//!
//! This runs while Flat IR still has semantic connection sets. Downstream IRs
//! therefore never need to guess connector membership or treat stream
//! operators as runtime pass-through functions.

use rumoca_core::{
    BuiltinFunction, ComponentPath, Expression, FallibleExpressionRewriter,
    FallibleStatementRewriter, Literal, OpBinary, OpUnary, Span, Subscript, VarName,
};
use rumoca_ir_flat as flat;
use rustc_hash::{FxHashMap, FxHashSet};

use super::FlattenError;

/// The MLS recommended regularization scales epsilon from flow nominal values.
/// This value is above Rumoca's algebraic-Newton residual tolerance while small
/// relative to the default flow nominal of one.
const STREAM_RELATIVE_TOLERANCE: f64 = 1.0e-7;

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
    field_base: Option<Box<Expression>>,
    span: Span,
}

type StreamEndpointMap = FxHashMap<VarName, StreamEndpoint>;

pub(super) fn rewrite_stream_operators(
    model: &mut flat::Model,
    stream_sets: &[super::StreamConnectionSet],
    endpoints: &StreamConnectionEndpoints,
) -> Result<(), FlattenError> {
    let stream_variables = collect_stream_variables(model, stream_sets);
    let mut rewriter = StreamOperatorRewriter {
        endpoints: endpoints.inside.clone(),
        flow_candidates: collect_flow_candidates(model),
        declared_paths: collect_declared_paths(model),
        stream_connectors: collect_stream_connectors(&stream_variables),
        stream_variables,
        expanding: Vec::new(),
    };
    rewrite_model_expressions(model, &mut rewriter)
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
            .map(|stream| associated_flow(model, &flow_candidates, stream))
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
    pub(super) fn outside_equation_rhs(&self, stream: &VarName, span: Span) -> Option<Expression> {
        let endpoint = self.outside.get(stream)?;
        let access = StreamAccess {
            name: stream.clone(),
            subscripts: Vec::new(),
            indexed: false,
            original: variable_reference(stream, &[], span),
            field_base: None,
            span,
        };
        Some(in_stream_expression(&access, endpoint, span))
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
) -> Result<FlowCandidate, FlattenError> {
    let stream_path = ComponentPath::from_flat_path(stream.as_str());
    let Some(parent) = stream_path.parent() else {
        return Err(stream_flow_error(model, stream));
    };
    let exact = candidates
        .iter()
        .filter(|candidate| candidate.parent == parent)
        .collect::<Vec<_>>();
    if let [candidate] = exact.as_slice() {
        return Ok((*candidate).clone());
    }
    if !exact.is_empty() {
        return Err(stream_flow_error(model, stream));
    }

    let normalized_parent = normalized_path(&parent);
    let normalized = candidates
        .iter()
        .filter(|candidate| candidate.normalized_parent == normalized_parent)
        .collect::<Vec<_>>();
    let [candidate] = normalized.as_slice() else {
        return Err(stream_flow_error(model, stream));
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

fn stream_flow_error(model: &flat::Model, stream: &VarName) -> FlattenError {
    let Some(variable) = stream_variable(model, stream) else {
        return FlattenError::MissingSourceContext {
            reason: format!(
                "stream-set endpoint `{}` has no declared Flat IR variable",
                stream.as_str()
            ),
        };
    };
    FlattenError::unsupported_equation(
        format!(
            "stream variable `{}` does not have exactly one scalar flow variable at the same connector level",
            stream.as_str()
        ),
        variable.source_span,
    )
}

fn stream_variable<'a>(model: &'a flat::Model, stream: &VarName) -> Option<&'a flat::Variable> {
    model
        .variables
        .get(stream)
        .or_else(|| {
            super::subscripted_base_var(stream, model).and_then(|base| model.variables.get(&base))
        })
        .or_else(|| {
            super::strip_embedded_array_indices(stream.as_str())
                .and_then(|base| model.variables.get(&VarName::new(base)))
        })
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
}

impl StreamOperatorRewriter {
    fn rewrite_stream_call(
        &mut self,
        operator: &str,
        args: &[Expression],
        span: Span,
    ) -> Result<Expression, FlattenError> {
        let [argument] = args else {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "{operator}() requires one stream-variable reference, got {} arguments",
                    args.len()
                ),
                span,
            ));
        };
        let access = stream_access(argument).ok_or_else(|| {
            FlattenError::unsupported_equation(
                format!("{operator}() requires one stream-variable reference"),
                span,
            )
        })?;
        if !stream_variable_is_valid(&access.name, &self.stream_variables) {
            if self.is_empty_stream_member(&access.name) {
                return Ok(empty_stream_result(access.span));
            }
            return Err(FlattenError::unsupported_equation(
                format!(
                    "{operator}() argument `{}` is not a stream variable",
                    access.name.as_str()
                ),
                access.span,
            ));
        }

        let indexed_matches = self.indexed_endpoint_matches(&access.name);
        let expanded = if indexed_matches.len() > 1 {
            indexed_endpoint_expression(operator, &access, indexed_matches, span)?
        } else {
            let endpoint = self.endpoint_for(&access.name, span)?;
            if operator == "actualStream" {
                actual_stream_expression(&access, &endpoint, span)
            } else {
                in_stream_expression(&access, &endpoint, span)
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
        if !contains_stream_operator(&expression) {
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
    operator: &str,
    access: &StreamAccess,
    matches: Vec<(VarName, StreamEndpoint)>,
    span: Span,
) -> Result<Expression, FlattenError> {
    let access_indices = connector_access_indices(access);
    let mut choices = Vec::with_capacity(matches.len());
    for (stream, endpoint) in matches {
        let candidate_indices = embedded_parent_indices(&stream).ok_or_else(|| {
            indexed_stream_error(
                operator,
                access,
                "connection member has a non-integer index",
            )
        })?;
        let condition = index_match_condition(&access_indices, &candidate_indices, span)
            .ok_or_else(|| {
                indexed_stream_error(
                    operator,
                    access,
                    "connector access rank does not match connection members",
                )
            })?;
        let concrete = concrete_stream_access(access, stream, span);
        let expression = if operator == "actualStream" {
            actual_stream_expression(&concrete, &endpoint, span)
        } else {
            in_stream_expression(&concrete, &endpoint, span)
        };
        choices.push((condition, expression));
    }
    let Some((_, else_branch)) = choices.pop() else {
        return Err(indexed_stream_error(
            operator,
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

fn contains_stream_operator(expression: &Expression) -> bool {
    expression.contains_subexpression(|candidate| {
        matches!(
            candidate,
            Expression::FunctionCall { name, .. }
                if matches!(name.var_name().last_segment(), "inStream" | "actualStream")
        )
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
    if let Some(base) = &access.field_base {
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
        {
            let operator = name.var_name().last_segment();
            if matches!(operator, "inStream" | "actualStream") {
                return self.rewrite_stream_call(operator, args, *span);
            }
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
            base, field, span, ..
        } => {
            let base_name = reference_name_without_subscripts(base)?;
            let path = ComponentPath::from_flat_path(base_name.as_str())
                .join(&ComponentPath::from_parts([field.clone()]));
            Some(StreamAccess {
                name: VarName::new(path.to_flat_string()),
                subscripts: Vec::new(),
                indexed: false,
                original: expression.clone(),
                field_base: Some(base.clone()),
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
    span: Span,
) -> Expression {
    match endpoint.peers.as_slice() {
        [] => stream_reference(&access.name, access, span),
        [peer] => peer_stream_value(peer, access, span),
        peers => weighted_stream_mean(peers, access, endpoint, span),
    }
}

/// Value a peer contributes to the mixing enthalpy (MLS §15.2).
///
/// An inside connector contributes the value its component pushes out,
/// `m_j.h_outflow`. An outside connector contributes what the *enclosing* model
/// pushes into this set, `inStream(c_k.h_outflow)`; that nested operator is
/// resolved against the set in which `c_k` is an inside connector, one level up
/// the hierarchy.
fn peer_stream_value(peer: &StreamPeer, access: &StreamAccess, span: Span) -> Expression {
    let reference = stream_reference(&peer.stream, access, span);
    match peer.role {
        ConnectorRole::Inside => reference,
        ConnectorRole::Outside => Expression::FunctionCall {
            name: rumoca_core::Reference::generated("inStream"),
            args: vec![reference],
            is_constructor: false,
            span,
        },
    }
}

fn actual_stream_expression(
    access: &StreamAccess,
    endpoint: &StreamEndpoint,
    span: Span,
) -> Expression {
    let condition = binary(
        OpBinary::Gt,
        stream_member_reference(&endpoint.flow.name, access, span),
        real_literal(0.0, span),
        span,
    );
    Expression::If {
        branches: vec![(condition, in_stream_expression(access, endpoint, span))],
        else_branch: Box::new(stream_reference(&access.name, access, span)),
        span,
    }
}

fn weighted_stream_mean(
    peers: &[StreamPeer],
    access: &StreamAccess,
    endpoint: &StreamEndpoint,
    span: Span,
) -> Expression {
    let weighted_values = peers
        .iter()
        .map(|peer| {
            let weight = positive_flow_weight(peer, endpoint.epsilon, span);
            binary(
                OpBinary::Mul,
                weight,
                peer_stream_value(peer, access, span),
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
    if let Some(base) = &access.field_base {
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

fn rewrite_model_expressions(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    rewrite_equation_partitions(model, rewriter)?;
    rewrite_variable_expressions(model, rewriter)?;
    rewrite_assertions(model, rewriter)?;
    rewrite_algorithms(model, rewriter)?;
    rewrite_when_clauses(model, rewriter)?;
    Ok(())
}

fn rewrite_equation_partitions(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for equation in model
        .equations
        .iter_mut()
        .chain(model.initial_equations.iter_mut())
    {
        equation.residual = rewriter.rewrite_expression(&equation.residual)?;
    }
    for family in model
        .structured_equations
        .iter_mut()
        .chain(model.initial_structured_equations.iter_mut())
    {
        if let Some(template) = &mut family.template {
            template.body = rewriter.rewrite_expressions(&template.body)?;
        }
    }
    Ok(())
}

fn rewrite_variable_expressions(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for variable in model.variables.values_mut() {
        rewrite_optional_expression(&mut variable.binding, rewriter)?;
        rewrite_optional_expression(&mut variable.start, rewriter)?;
        rewrite_optional_expression(&mut variable.min, rewriter)?;
        rewrite_optional_expression(&mut variable.max, rewriter)?;
        rewrite_optional_expression(&mut variable.nominal, rewriter)?;
    }
    Ok(())
}

fn rewrite_optional_expression(
    expression: &mut Option<Expression>,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    if let Some(value) = expression.take() {
        *expression = Some(rewriter.rewrite_expression(&value)?);
    }
    Ok(())
}

fn rewrite_assertions(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for assertion in model
        .assert_equations
        .iter_mut()
        .chain(model.initial_assert_equations.iter_mut())
    {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition)?;
        assertion.message = rewriter.rewrite_expression(&assertion.message)?;
        rewrite_optional_expression(&mut assertion.level, rewriter)?;
    }
    Ok(())
}

fn rewrite_algorithms(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for algorithm in model
        .algorithms
        .iter_mut()
        .chain(model.initial_algorithms.iter_mut())
    {
        algorithm.statements = rewriter.rewrite_statements(&algorithm.statements)?;
    }
    Ok(())
}

fn rewrite_when_clauses(
    model: &mut flat::Model,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    for clause in &mut model.when_clauses {
        clause.condition = rewriter.rewrite_expression(&clause.condition)?;
        rewrite_when_equations(&mut clause.equations, rewriter)?;
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
                condition, message, ..
            } => {
                *condition = rewriter.rewrite_expression(condition)?;
                *message = rewriter.rewrite_expression(message)?;
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
                rewrite_when_equations(else_branch, rewriter)?;
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function)?;
            }
        }
    }
    Ok(())
}
