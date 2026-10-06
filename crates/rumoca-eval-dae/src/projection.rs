mod array_update;
mod call_arguments;
mod dependencies;
mod domain_context;
mod fold_graph;
mod indexed_write_fold;
mod integer;
mod literal_bindings;
mod literal_update_sweeps;
#[cfg(test)]
mod merge_profile_tests;
mod parameter_fragments;
mod query;
mod record_fields;
mod scalar_selection;
#[cfg(test)]
mod tests;
mod visited;
mod zero_coefficients;
pub use literal_bindings::{LiteralBinding, literal_bindings};
pub use zero_coefficients::ZeroCoefficients;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::Arc;

use rumoca_core::{Span, flatten_coordinates, row_major_coordinates};
use rumoca_ir_dae as dae;

/// Failure to derive one exact scalar view from a checked DAE expression.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ProjectionError {
    #[error("scalar projection {index} is outside an expression containing {count} scalars")]
    ScalarOutOfBounds {
        index: usize,
        count: usize,
        span: Span,
    },
    #[error("array subscript is not compile-time computable")]
    DynamicSubscript { span: Span },
    #[error("Modelica index {index} is outside an axis of extent {extent}")]
    IndexOutOfBounds { index: i64, extent: u32, span: Span },
    #[error("integer evaluation overflowed during scalar projection")]
    IntegerOverflow { span: Span },
    #[error("function scalar projection exceeded the checked recursion limit")]
    FunctionRecursion { span: Span },
    #[error(
        "record field `{target}` cannot be read one field at a time: its value comes from {operation}, which this compiler does not project per record field"
    )]
    UnsupportedRecordOperation {
        target: String,
        operation: &'static str,
        span: Span,
    },
    #[error(
        "external {language} function `{name}` calls `{symbol}`, which this runtime cannot execute"
    )]
    ExternalFunction {
        name: String,
        language: &'static str,
        symbol: String,
        span: Span,
    },
}

/// Report an MLS §12.9 external body that projection cannot look through.
///
/// Projection resolves a call by continuing into the callee's result
/// definition. An external body has none, so the exact interface is reported
/// instead of a silently empty incidence set.
fn external_projection_error(
    definition: dae::FunctionView<'_>,
    external: dae::ExternalFunctionView<'_>,
    span: Span,
) -> ProjectionError {
    ProjectionError::ExternalFunction {
        name: definition.name().to_string(),
        language: external.language().as_str(),
        symbol: external.symbol().to_string(),
        span,
    }
}

/// Report a record value whose definition projection cannot split per field.
///
/// The rejected value is named the way the source names it, `Record.field`
/// where the DAE retained both names, so a reader can find the declaration
/// without reading IR. The definition form is named too, because that is what
/// decides whether the source has to change or the compiler does.
fn unsupported_record_operation(node: dae::ExpressionView<'_>, field: usize) -> ProjectionError {
    let value_type = node.value_type();
    let target = match (
        value_type.record_name(),
        value_type.record_field_name(field),
    ) {
        (Some(record), Some(name)) => format!("{record}.{name}"),
        (Some(record), None) => format!("{record} field {field}"),
        (None, Some(name)) => name.to_string(),
        (None, None) => format!("field {field}"),
    };
    ProjectionError::UnsupportedRecordOperation {
        target,
        operation: record_definition_form(node.operation()),
        span: node.provenance().span(),
    }
}

/// Name the definition form a record field projection could not look through.
const fn record_definition_form(operation: dae::ExpressionOperation<'_>) -> &'static str {
    match operation {
        dae::ExpressionOperation::Field { .. } => "a field of an enclosing record",
        dae::ExpressionOperation::Coordinate(_) => "a coordinate of the model",
        dae::ExpressionOperation::Builtin { .. } => "a built-in operator result",
        dae::ExpressionOperation::Binary { .. } | dae::ExpressionOperation::Unary { .. } => {
            "an arithmetic result"
        }
        dae::ExpressionOperation::ClockTransfer { .. } => "a clocked transfer",
        _ => "a definition form record projection does not cover",
    }
}

/// Visit every coordinate on which one scalar result depends.
///
/// `scalar_index` is row-major within `root`. For a structured equation body,
/// pass its domain and the one-based coordinates of the domain point being
/// projected. The callback receives the typed coordinate and its row-major
/// scalar index. Runtime array selection reports the exact union of every
/// potentially selected base scalar plus its subscript dependencies. This is a
/// conservative incidence proof, not a guessed runtime value.
pub fn for_each_scalar_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    visit: impl FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    let mut cache = ScalarCoordinateProjectionCache::default();
    for_each_scalar_coordinate_cached(view, root, scalar_index, domain_point, &mut cache, visit)
}

/// Reusable cache for dependency projection over one finalized DAE.
///
/// Structural incidence owns one cache for its complete row walk so repeated
/// scalar views of the same compact function transition reuse its exact
/// parameter-scalar summary, then substitute the actual call arguments.
#[derive(Default)]
pub struct ScalarCoordinateProjectionCache<'dae> {
    function_results: HashMap<FunctionSummaryKey, FunctionSummaryEntry>,
    completed_folds: HashMap<fold_graph::FoldNode<'dae>, Arc<[FunctionParameterDependency]>>,
    parameter_fragments: parameter_fragments::reuse::Cache,
    query_validation: HashMap<u32, Box<ScalarCoordinateProjectionCache<'dae>>>,
    zero_coefficients: zero_coefficients::ZeroCoefficients<'dae>,
    marker: std::marker::PhantomData<&'dae ()>,
    #[cfg(test)]
    uncached_fold_reference: bool,
    #[cfg(test)]
    uncached_indexed_write_folds: bool,
    #[cfg(test)]
    indexed_write_folds: usize,
    #[cfg(test)]
    uncached_parameter_fragments: bool,
    #[cfg(test)]
    uncached_literal_update_sweeps: bool,
    #[cfg(test)]
    fragment_hits: u64,
    #[cfg(test)]
    validation_memo_hits: u64,
    #[cfg(test)]
    uncached_validation_memo: bool,
    #[cfg(test)]
    uncached_guard_memo: bool,
    #[cfg(test)]
    guard_memo_hits: u64,
    #[cfg(test)]
    imported_fragment_hits: u64,
    #[cfg(test)]
    sweep_hits: u64,
    #[cfg(test)]
    fold_edges: HashMap<fold_graph::FoldNode<'dae>, Vec<fold_graph::FoldNode<'dae>>>,
    #[cfg(test)]
    fold_walks: usize,
    #[cfg(test)]
    fold_reuses: usize,
    #[cfg(test)]
    fold_suppressions: usize,
    #[cfg(test)]
    fold_graph_repeated_edges: usize,
}

impl ScalarCoordinateProjectionCache<'_> {
    fn cache_function_summary(&mut self, key: FunctionSummaryKey, entry: &FunctionSummaryEntry) {
        if !matches!(entry, FunctionSummaryEntry::Direct) {
            self.function_results.insert(key, entry.clone());
        }
    }
}

#[track_caller]
pub fn for_each_scalar_coordinate_cached<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    mut visit: impl FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    project_coordinates(
        view,
        root,
        scalar_index,
        domain_point,
        cache,
        None,
        &mut visit,
    )
}

/// Visit only coordinates selected by a declaration-level query.
///
/// The predicate must return true when any scalar of a declaration is relevant.
/// Plain query-free call arguments permit validation without irrelevant formal
/// dependency capture. Address specialization, recursion and projection errors
/// still use the complete original walker in an isolated cache. Argument
/// selectors, conditionals and calls retain the unfiltered dependency path.
#[track_caller]
pub fn for_each_scalar_coordinate_filtered_cached<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    relevant: impl Fn(dae::CoordinateView<'dae>) -> bool,
    mut visit: impl FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    let mut filtered = |coordinate, scalar| {
        if relevant(coordinate) {
            visit(coordinate, scalar);
        }
    };
    project_coordinates(
        view,
        root,
        scalar_index,
        domain_point,
        cache,
        Some(&relevant),
        &mut filtered,
    )
}

fn project_coordinates<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    relevant: Option<&dyn Fn(dae::CoordinateView<'dae>) -> bool>,
    visit: &mut dyn FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    let mut projection = Projection {
        activation: Activation::Guaranteed,
        validating_actuals: false,
        view,
        domain_contexts: domain_context::DomainContexts::new(match domain_point {
            Some((domain, point)) => vec![(domain, point.to_vec())],
            None => Vec::new(),
        }),
        integer_stack: vec![false; view.expression_count()],
        function_frames: Vec::new(),
        function_call_active: HashSet::default(),
        function_fold_active: HashSet::default(),
        function_summary_captures: Vec::new(),
        model_visited: visited::Visited::default(),
        frame_memos: Vec::new(),
        cache,
        visit,
        relevant,
        validation: None,
        validation_memo: None,
        guard_memo: None,
    };
    projection.expression(root, scalar_index)
}

/// Whether a source read must execute if the enclosing expression executes.
/// This is independent of lexical binder identity and recursion membership.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
enum Activation {
    #[default]
    Guaranteed,
    Conditional,
}

impl Activation {
    const fn within(self, child: Self) -> Self {
        match (self, child) {
            (Self::Guaranteed, Self::Guaranteed) => Self::Guaranteed,
            _ => Self::Conditional,
        }
    }

    const fn index(self) -> usize {
        match self {
            Self::Guaranteed => 0,
            Self::Conditional => 1,
        }
    }
}

/// The walk dispatches the caller's visitor dynamically, so every caller shares
/// one compiled projection instead of one copy per visitor type.
struct Projection<'visit, 'dae> {
    activation: Activation,
    validating_actuals: bool,
    view: dae::DaeView<'dae>,
    domain_contexts: domain_context::DomainContexts<'dae>,
    integer_stack: Vec<bool>,
    function_frames: Vec<FunctionFrame<'dae>>,
    function_call_active: HashSet<FunctionResultDependency>,
    function_fold_active: HashSet<FunctionFoldDependency>,
    function_summary_captures: Vec<FunctionSummaryCapture<'dae>>,
    model_visited: visited::Visited,
    /// One memo per entry of `function_frames`, with the same invocation lifetime.
    frame_memos: Vec<FrameMemo>,
    cache: &'visit mut ScalarCoordinateProjectionCache<'dae>,
    visit: &'visit mut dyn FnMut(dae::CoordinateView<'dae>, usize),
    relevant: Option<&'visit dyn Fn(dae::CoordinateView<'dae>) -> bool>,
    validation: Option<u32>,
    validation_memo: Option<query::validation_memo::ValidationMemo<'dae>>,
    guard_memo: Option<query::guard_memo::GuardMemo<'dae>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionFoldDependency {
    function: u32,
    fold: u32,
    carried: u32,
    field: Option<usize>,
    scalar: usize,
}

/// What one call frame has already projected.
///
/// Within one frame every revisit of a scalar dependency, or of a completed
/// fold dependency under the same enclosing fold points, reaches the same
/// coordinates through the same frames. Remembering both keeps a body whose
/// definitions share subexpressions, and a fold whose carried values read
/// each other, linear in the body instead of exponential in its paths.
#[derive(Debug, Default)]
struct FrameMemo {
    visited: HashSet<ScalarExpressionDependency>,
    folds: HashSet<FoldVisit>,
}

/// One completed fold dependency under the enclosing fold points.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FoldVisit {
    activation: Activation,
    dependency: FunctionFoldDependency,
    context: Vec<(u32, Vec<i64>)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionResultDependency {
    function: u32,
    output: u32,
    field: Option<usize>,
    scalar: usize,
}

#[derive(Debug, Clone)]
enum FunctionFrame<'dae> {
    Actual {
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
    },
    /// A call-site independent summary of one function result, specialized
    /// to the proven values of the integer parameter scalars it reads.
    Summary {
        function: dae::FunctionId<'dae>,
        integers: Vec<IntegerBinding>,
    },
}

impl<'dae> FunctionFrame<'dae> {
    const fn function(&self) -> dae::FunctionId<'dae> {
        match self {
            Self::Actual { function, .. } | Self::Summary { function, .. } => *function,
        }
    }
}

/// The proven value of one integer parameter scalar a summary depends on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct IntegerBinding {
    parameter: u32,
    scalar: usize,
    value: i64,
}

/// A function result summary under one set of integer parameter values.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionSummaryKey {
    activation: Activation,
    dependency: FunctionResultDependency,
    integers: Vec<IntegerBinding>,
}

/// What a function result depends on under one [`FunctionSummaryKey`].
#[derive(Debug, Clone, PartialEq, Eq)]
enum FunctionSummaryEntry {
    /// Conservative parameter incidence, retaining each read's activation.
    Complete(Vec<FunctionParameterDependency>),
    /// The result also reads the values of these integer parameter scalars
    /// (an index or a size), which the key does not bind yet.
    NeedsIntegers(Vec<(u32, usize)>),
    /// A local capture requires its original actual-call walk. Never cached.
    Direct,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FunctionParameterDependency {
    Scalar {
        activation: Activation,
        parameter: u32,
        scalar: usize,
    },
    RecordField {
        activation: Activation,
        parameter: u32,
        field: usize,
        scalar: usize,
    },
}

#[derive(Debug)]
struct FunctionSummaryCapture<'dae> {
    function: u32,
    dependencies: dependencies::OrderedDependencies,
    needed_integers: Vec<(u32, usize)>,
    cacheable: bool,
    visited: visited::Visited,
    folds: fold_graph::FoldGraph<'dae>,
    fragments: parameter_fragments::ParameterFragments<'dae>,
    sweeps: literal_update_sweeps::LiteralUpdateSweeps<'dae>,
}

impl FunctionSummaryCapture<'_> {
    fn visit_once(&mut self, dependency: ScalarExpressionDependency) -> bool {
        let inserted = self.visited.insert(dependency);
        if !inserted {
            self.fragments.suppress();
        }
        inserted
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ScalarExpressionDependency {
    activation: Activation,
    expression: u32,
    field: Option<usize>,
    scalar: usize,
    domain_context: domain_context::DomainContextId,
}

impl<'dae> Projection<'_, 'dae> {
    fn expression_uncached(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let Some(node) = self.expression_to_project(expression, scalar_index)? else {
            return Ok(());
        };
        match node.operation() {
            dae::ExpressionOperation::Literal(_) => Ok(()),
            dae::ExpressionOperation::Range(range) => self.range_dependencies(range),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
                    return self.function_parameter(
                        parameter,
                        scalar_index,
                        node.provenance().span(),
                    );
                }
                if !matches!(coordinate, dae::CoordinateView::Binder(_)) {
                    self.emit_coordinate(coordinate, scalar_index);
                }
                Ok(())
            }
            dae::ExpressionOperation::Unary { operand, .. } => {
                self.expression(operand, scalar_index)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs, scalar_index)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditional(operands, scalar_index)
            }
            dae::ExpressionOperation::Array(elements) => self.array(elements, scalar_index),
            dae::ExpressionOperation::Record(fields) => {
                for field in fields.iter() {
                    self.all_scalars(field)?;
                }
                Ok(())
            }
            dae::ExpressionOperation::Field { base, field } => {
                self.record_field(base, field as usize, scalar_index)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body, scalar_index)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.expression(definition.rhs(), scalar_index)
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
            | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
                self.function_fold_dependency(fold, carried, None, scalar_index)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                match self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                ) {
                    Ok(base_index) => self.expression(base, base_index),
                    Err(error) if self.conservative_subscript(&error) => {
                        self.all_scalars(base)?;
                        self.subscripts(subscripts)
                    }
                    Err(error) => Err(error),
                }
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update_scalar(base, value, subscripts, scalar_index),
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(node, builtin, arguments, scalar_index)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.function_call(
                function,
                output,
                arguments,
                scalar_index,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::StringConversion { value, format, .. } => {
                self.string_conversion_dependencies(value, format)
            }
            dae::ExpressionOperation::ClockTransfer { source, .. } => {
                self.expression(source, scalar_index)
            }
        }
    }

    fn expression_to_project(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<Option<dae::ExpressionView<'dae>>, ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        let is_fold_boundary = matches!(
            node.operation(),
            dae::ExpressionOperation::FunctionFoldParameter { .. }
                | dae::ExpressionOperation::FunctionFoldOutput { .. }
        );
        Ok(
            (is_fold_boundary || self.visit_expression_once(expression, None, scalar_index))
                .then_some(node),
        )
    }

    /// Visit the coordinates one carried loop value depends on.
    ///
    /// `field` selects a record field ordinal of the carried value; `None`
    /// projects the carried value itself. A record-typed carry has no scalar
    /// view of its own, so its readers reach it through this field-projected
    /// entry instead.
    fn function_fold_dependency(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        if self.fold_summary_capture(fold.function()).is_some() {
            return self.enqueue_fold(fold, carried, field, scalar);
        }
        self.function_fold_dependency_direct(fold, carried, field, scalar)
    }

    fn function_fold_dependency_direct(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        let dependency = FunctionFoldDependency {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried,
            field,
            scalar,
        };
        let fold_view = self
            .view
            .function_fold(fold)
            .expect("checked function fold identity resolves");
        let fold_domain = fold_view.domain();
        // The dependency spans every point of the fold, so it is independent
        // of any point of the fold's own domain (or a domain nested in it)
        // that is current where the carried value is read: a read from inside
        // the fold's own update reaches the same coordinates as a read after
        // it. Projecting it under those points would key every iteration's
        // visits by a point that does not change them, and re-walk the whole
        // fold once per enclosing point.
        let enclosing = self
            .domain_contexts
            .points
            .iter()
            .filter(|(domain, _)| !self.domain_is_within(*domain, fold_domain))
            .cloned()
            .collect::<Vec<_>>();
        // Within one call frame and one enclosing fold point, a completed fold
        // dependency has already reached every coordinate it can reach; a
        // second read of the same carried scalar adds nothing.
        let completed = FoldVisit {
            activation: self.activation,
            dependency: dependency.clone(),
            context: enclosing
                .iter()
                .map(|(domain, point)| (domain.index(), point.clone()))
                .collect(),
        };
        if self.direct_fold_memo_enabled()
            && self
                .frame_memos
                .last()
                .is_some_and(|memo| memo.folds.contains(&completed))
        {
            return Ok(());
        }
        if !self.function_fold_active.insert(dependency.clone()) {
            #[cfg(test)]
            {
                self.cache.fold_suppressions += 1;
            }
            return Ok(());
        }
        #[cfg(test)]
        {
            self.cache.fold_walks += 1;
        }
        let current = self.domain_contexts.replace(enclosing);
        let carried = carried as usize;
        let projected = (|| {
            let initial = fold_view
                .initial_values()
                .rhs(carried)
                .expect("checked fold carried ordinal has an initial value");
            self.projected_value(initial, field, scalar)?;
            let domain = self
                .view
                .domain(fold_domain)
                .expect("checked function fold domain resolves");
            let update = fold_view
                .update_values()
                .rhs(carried)
                .expect("checked fold carried ordinal has an update value");
            let points = domain
                .structured()
                .index_tuples()
                .expect("checked fold domain remains representable");
            for point in points {
                self.domain_contexts.push(fold_domain, point);
                let result = self.projected_value(update, field, scalar);
                self.domain_contexts.pop();
                result?;
            }
            Ok(())
        })();
        self.domain_contexts.replace(current);
        self.function_fold_active.remove(&dependency);
        if self.direct_fold_memo_enabled()
            && projected.is_ok()
            && let Some(memo) = self.frame_memos.last_mut()
        {
            memo.folds.insert(completed);
        }
        projected
    }

    fn direct_fold_memo_enabled(&self) -> bool {
        if self.validating_actuals {
            return false;
        }
        #[cfg(test)]
        if self.cache.uncached_fold_reference {
            return false;
        }
        true
    }

    fn fold_summary_capture(
        &mut self,
        function: dae::FunctionId<'dae>,
    ) -> Option<&mut FunctionSummaryCapture<'dae>> {
        if self.validating_actuals {
            return None;
        }
        #[cfg(test)]
        if self.cache.uncached_fold_reference {
            return None;
        }
        if !matches!(self.function_frames.last(), Some(FunctionFrame::Summary { function: active, .. }) if *active == function)
        {
            return None;
        }
        self.function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function.index())
    }

    fn drain_fold_graph(&mut self, function: dae::FunctionId<'dae>) -> Result<(), ProjectionError> {
        while let Some(node) = self
            .fold_summary_capture(function)
            .and_then(|capture| capture.folds.begin_next())
        {
            let parent = node
                .parent
                .iter()
                .map(|(domain, point)| {
                    (
                        self.view
                            .domain_id(*domain as usize)
                            .expect("checked lexical domain resolves"),
                        point.clone(),
                    )
                })
                .collect();
            let previous = self.domain_contexts.replace(parent);
            self.fold_summary_capture(function).unwrap().visited.clear();
            #[cfg(test)]
            {
                self.cache.fold_walks += 1;
            }
            let projected = self.with_activation(node.activation, |projection| {
                projection.project_fold_node(&node)
            });
            self.domain_contexts.replace(previous);
            let capture = self.fold_summary_capture(function).unwrap();
            projected?;
            capture.folds.finish_node();
        }
        Ok(())
    }

    fn project_fold_node(
        &mut self,
        node: &fold_graph::FoldNode<'dae>,
    ) -> Result<(), ProjectionError> {
        self.projected_value(node.initial, node.field, node.scalar)?;
        if self.project_indexed_write_fold(node)? {
            return Ok(());
        }
        if self.project_literal_update_sweep(node)? {
            return Ok(());
        }
        let fold = self.view.function_fold(node.fold).unwrap();
        let points = self
            .view
            .domain(fold.domain())
            .unwrap()
            .structured()
            .index_tuples()
            .expect("checked fold domain remains representable");
        for point in points {
            self.domain_contexts.push(fold.domain(), point);
            let projected = self.projected_value(node.update, node.field, node.scalar);
            self.domain_contexts.pop();
            projected?;
        }
        Ok(())
    }

    /// Project either one scalar of a value or one scalar of a record field.
    fn domain_is_within(&self, domain: dae::DomainId<'dae>, ancestor: dae::DomainId<'dae>) -> bool {
        let mut current = Some(domain);
        while let Some(candidate) = current {
            if candidate == ancestor {
                return true;
            }
            current = self
                .view
                .domain(candidate)
                .expect("checked domain resolves")
                .parent();
        }
        false
    }

    fn projected_value(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        match field {
            Some(field) => self.record_field(expression, field, scalar),
            None => self.expression(expression, scalar),
        }
    }

    fn emit_coordinate(&mut self, coordinate: dae::CoordinateView<'dae>, scalar: usize) {
        if !self.validating_actuals {
            (self.visit)(coordinate, scalar);
        }
    }

    fn string_conversion_dependencies(
        &mut self,
        value: dae::ExprId<'dae>,
        format: dae::StringConversionFormatView<'dae>,
    ) -> Result<(), ProjectionError> {
        self.all_scalars(value)?;
        match format {
            dae::StringConversionFormatView::Options {
                minimum_length,
                left_justified,
                significant_digits,
            } => {
                for option in [minimum_length, left_justified, significant_digits]
                    .into_iter()
                    .flatten()
                {
                    self.all_scalars(option)?;
                }
            }
            dae::StringConversionFormatView::Format { value } => self.all_scalars(value)?,
        }
        Ok(())
    }

    fn range_dependencies(&mut self, range: dae::RangeView<'dae>) -> Result<(), ProjectionError> {
        self.expression(range.start().expression(), 0)?;
        if let Some(step) = range.explicit_step() {
            self.expression(step.expression(), 0)?;
        }
        self.expression(range.stop().expression(), 0)
    }

    fn subscripts(&mut self, subscripts: dae::SubscriptsView<'dae>) -> Result<(), ProjectionError> {
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. }
                | dae::SubscriptView::Slice { expression, .. } => {
                    self.all_scalars(expression)?;
                }
                dae::SubscriptView::Whole { .. } => {}
            }
        }
        Ok(())
    }

    fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let Some(frame) = self.function_frames.last() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        if frame.function() != parameter.function() {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        match frame {
            FunctionFrame::Actual { arguments, .. } => {
                let argument = arguments
                    .get(parameter.ordinal() as usize)
                    .copied()
                    .ok_or(ProjectionError::FunctionRecursion { span })?;
                self.in_caller_context(|projection| projection.expression(argument, scalar_index))
            }
            FunctionFrame::Summary { function, .. } => {
                let function = *function;
                self.capture_function_parameter(
                    function,
                    FunctionParameterDependency::Scalar {
                        activation: self.activation,
                        parameter: parameter.ordinal(),
                        scalar: scalar_index,
                    },
                    span,
                )
            }
        }
    }

    fn push_frame(&mut self, frame: FunctionFrame<'dae>) {
        self.function_frames.push(frame);
        self.frame_memos.push(FrameMemo::default());
    }

    fn pop_frame(&mut self) -> Option<(FunctionFrame<'dae>, FrameMemo)> {
        let frame = self.function_frames.pop()?;
        let memo = self
            .frame_memos
            .pop()
            .expect("every function frame owns one memo");
        Some((frame, memo))
    }

    /// Run `walk` with the innermost call frame removed.
    ///
    /// An actual argument is an expression of the caller, so its parameters,
    /// fold points, and integer values resolve against the caller's frames.
    fn in_caller_context<T>(
        &mut self,
        walk: impl FnOnce(&mut Self) -> Result<T, ProjectionError>,
    ) -> Result<T, ProjectionError> {
        let callee = self.pop_frame();
        let result = walk(self);
        if let Some((frame, memo)) = callee {
            self.function_frames.push(frame);
            self.frame_memos.push(memo);
        }
        result
    }

    fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        if self.function_frames.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let arguments = arguments.iter().collect::<Vec<_>>();
        self.validate_call_arguments(&arguments)?;
        if self.is_native_table_call(function) {
            // A native table interpolation (MLS §12.9) is a solver primitive:
            // its result depends on its argument incidence, and the opaque
            // external body is never entered.
            for argument in &arguments {
                self.expression(*argument, 0)?;
            }
            return Ok(());
        }
        if self.has_native_body(function) || self.is_active_function(function) {
            return self.native_body_arguments(&arguments);
        }
        if !self.validating_actuals && self.query_free_arguments(&arguments) {
            return self.validate_query_free_call(
                function,
                output,
                None,
                scalar_index,
                arguments,
                span,
            );
        }
        let dependency = FunctionResultDependency {
            function: function.index(),
            output,
            field: None,
            scalar: scalar_index,
        };
        self.project_function_result(dependency, function, arguments, span)
    }

    /// Whether `function` is already being projected on this walk, so the call
    /// is recursive (MLS §12.2). A pure result is determined by its arguments,
    /// so the recursive call reads at most every argument scalar; that bound
    /// is sound without unrolling a recursion whose depth is a runtime value.
    fn is_active_function(&self, function: dae::FunctionId<'dae>) -> bool {
        self.function_frames
            .iter()
            .any(|frame| frame.function() == function)
    }

    /// A SPEC_0040 DAE-C30 native body, or a recursive call, reads its
    /// inputs, which are closed over the call's arguments; every scalar of
    /// every argument is read.
    fn native_body_arguments(
        &mut self,
        arguments: &[dae::ExprId<'dae>],
    ) -> Result<(), ProjectionError> {
        for argument in arguments {
            (0..self.scalar_count(*argument))
                .try_for_each(|scalar| self.expression(*argument, scalar))?;
        }
        Ok(())
    }

    fn has_native_body(&self, function: dae::FunctionId<'dae>) -> bool {
        self.view
            .function(function)
            .and_then(|definition| definition.external())
            .is_some_and(|external| external.native_body().is_some())
    }

    fn is_native_table_call(&self, function: dae::FunctionId<'dae>) -> bool {
        self.view
            .function(function)
            .and_then(|definition| definition.external())
            .is_some_and(|external| {
                dae::NativeTableOperator::from_symbol(external.symbol().as_str()).is_some()
            })
    }

    fn function_call_record_field(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        if self.function_frames.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let arguments = arguments.iter().collect::<Vec<_>>();
        self.validate_call_arguments(&arguments)?;
        if self.is_active_function(function) {
            return self.native_body_arguments(&arguments);
        }
        if !self.validating_actuals && self.query_free_arguments(&arguments) {
            return self.validate_query_free_call(
                function,
                output,
                Some(field),
                scalar,
                arguments,
                span,
            );
        }
        let dependency = FunctionResultDependency {
            function: function.index(),
            output,
            field: Some(field),
            scalar,
        };
        self.project_function_result(dependency, function, arguments, span)
    }

    /// Project one function result scalar at a call site.
    ///
    /// The summary of a result is the set of parameter scalars it reads,
    /// which is a property of the function alone once every integer value it
    /// reads (an index, a size, a loop bound) is known. A summary that reads
    /// integer parameter scalars is keyed by their values: the call site
    /// proves them from its own arguments, and every later call with the same
    /// values reuses the summary. Only a call whose integer arguments are not
    /// translation-time values walks the body directly.
    fn project_function_result(
        &mut self,
        dependency: FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<(), ProjectionError> {
        if self.validating_actuals {
            return self.project_function_result_direct(&dependency, function, arguments, span);
        }
        self.guard_memo_note_call(function);
        let mut integers: Vec<IntegerBinding> = Vec::new();
        loop {
            let key = FunctionSummaryKey {
                activation: self.activation,
                dependency: dependency.clone(),
                integers: integers.clone(),
            };
            let entry = match self.cache.function_results.get(&key).cloned() {
                Some(entry) => entry,
                None => {
                    let entry =
                        self.derive_function_summary(&dependency, function, &integers, span)?;
                    self.cache.cache_function_summary(key, &entry);
                    entry
                }
            };
            let needed = match entry {
                FunctionSummaryEntry::Complete(summary) => {
                    return self.apply_function_summary(&summary, &arguments, span);
                }
                FunctionSummaryEntry::NeedsIntegers(needed) => needed,
                FunctionSummaryEntry::Direct => {
                    return self.project_function_result_direct(
                        &dependency,
                        function,
                        arguments,
                        span,
                    );
                }
            };
            if !self.bind_integer_arguments(&needed, &arguments, &mut integers, span)? {
                return self.project_function_result_direct(&dependency, function, arguments, span);
            }
        }
    }

    fn bind_integer_arguments(
        &mut self,
        needed: &[(u32, usize)],
        arguments: &[dae::ExprId<'dae>],
        integers: &mut Vec<IntegerBinding>,
        span: Span,
    ) -> Result<bool, ProjectionError> {
        let mut bound_more = false;
        for &(parameter, scalar) in needed {
            let bound = integers
                .iter()
                .any(|binding| binding.parameter == parameter && binding.scalar == scalar);
            if bound {
                continue;
            }
            let argument = arguments
                .get(parameter as usize)
                .copied()
                .ok_or(ProjectionError::FunctionRecursion { span })?;
            let value = match self.integer(argument, scalar) {
                Ok(value) => value,
                Err(ProjectionError::DynamicSubscript { .. }) => return Ok(false),
                Err(error) => return Err(error),
            };
            integers.push(IntegerBinding {
                parameter,
                scalar,
                value,
            });
            bound_more = true;
        }
        integers.sort_unstable();
        Ok(bound_more)
    }

    fn apply_function_summary(
        &mut self,
        summary: &[FunctionParameterDependency],
        arguments: &[dae::ExprId<'dae>],
        span: Span,
    ) -> Result<(), ProjectionError> {
        for parameter in summary {
            self.apply_function_parameter(parameter, arguments, span)?;
        }
        Ok(())
    }

    fn apply_function_parameter(
        &mut self,
        parameter: &FunctionParameterDependency,
        arguments: &[dae::ExprId<'dae>],
        span: Span,
    ) -> Result<(), ProjectionError> {
        match *parameter {
            FunctionParameterDependency::Scalar {
                activation,
                parameter,
                scalar,
            } => {
                let argument = arguments
                    .get(parameter as usize)
                    .copied()
                    .ok_or(ProjectionError::FunctionRecursion { span })?;
                self.with_activation(activation, |projection| {
                    projection.expression(argument, scalar)
                })?;
            }
            FunctionParameterDependency::RecordField {
                activation,
                parameter,
                field,
                scalar,
            } => {
                let argument = arguments
                    .get(parameter as usize)
                    .copied()
                    .ok_or(ProjectionError::FunctionRecursion { span })?;
                self.with_activation(activation, |projection| {
                    projection.record_field(argument, field, scalar)
                })?;
            }
        }
        Ok(())
    }

    fn derive_function_summary(
        &mut self,
        dependency: &FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        integers: &[IntegerBinding],
        span: Span,
    ) -> Result<FunctionSummaryEntry, ProjectionError> {
        // Preserve the local error boundary before opening a frame or graph.
        let result = self.function_result(function, dependency.output, span)?;
        if !self.function_call_active.insert(dependency.clone()) {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        // These local caches do not bind Integer profiles. A specialized
        // invocation cannot import generic facts or publish into their cache.
        let isolated = (!integers.is_empty()).then(|| {
            (
                std::mem::take(&mut self.cache.completed_folds),
                std::mem::take(&mut self.cache.parameter_fragments),
            )
        });
        self.function_summary_captures.push(FunctionSummaryCapture {
            function: function.index(),
            dependencies: dependencies::OrderedDependencies::default(),
            needed_integers: Vec::new(),
            cacheable: true,
            visited: visited::Visited::default(),
            folds: fold_graph::FoldGraph::default(),
            fragments: parameter_fragments::ParameterFragments::default(),
            sweeps: literal_update_sweeps::LiteralUpdateSweeps::default(),
        });
        self.push_frame(FunctionFrame::Summary {
            function,
            integers: integers.to_vec(),
        });
        let projected = match dependency.field {
            Some(field) => self.record_field(result, field, dependency.scalar),
            None => self.expression(result, dependency.scalar),
        }
        .and_then(|()| self.drain_fold_graph(function));
        self.pop_frame();
        let capture = self
            .function_summary_captures
            .pop()
            .expect("function summary capture was just pushed");
        self.function_call_active.remove(dependency);
        if let Some((folds, fragments)) = isolated {
            self.cache.completed_folds = folds;
            self.cache.parameter_fragments = fragments;
        }
        projected?;
        #[cfg(test)]
        {
            self.cache.fold_graph_repeated_edges += capture.folds.repeated_edges;
            self.cache.fragment_hits += capture.fragments.hits;
            self.cache.sweep_hits += capture.sweeps.hits;
            self.cache.fold_edges.extend(capture.folds.ordered_edges());
        }
        if !capture.needed_integers.is_empty() {
            return Ok(FunctionSummaryEntry::NeedsIntegers(capture.needed_integers));
        }
        if !capture.cacheable {
            return Ok(FunctionSummaryEntry::Direct);
        }
        if integers.is_empty() {
            for (key, values) in capture.fragments.into_reusable() {
                self.cache.parameter_fragments.insert(key, values);
            }
            self.cache.completed_folds.extend(capture.folds.completed());
        }
        Ok(FunctionSummaryEntry::Complete(
            capture.dependencies.into_values(),
        ))
    }

    fn project_function_result_direct(
        &mut self,
        dependency: &FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let result = self.function_result(function, dependency.output, span)?;
        self.push_frame(FunctionFrame::Actual {
            function,
            arguments,
        });
        let projected = match dependency.field {
            Some(field) => self.record_field(result, field, dependency.scalar),
            None => self.expression(result, dependency.scalar),
        };
        self.pop_frame();
        projected
    }

    fn capture_function_parameter(
        &mut self,
        function: dae::FunctionId<'dae>,
        dependency: FunctionParameterDependency,
        span: Span,
    ) -> Result<(), ProjectionError> {
        if self.validating_actuals {
            return Ok(());
        }
        self.record_guard_parameter(function, &dependency, span);
        let capture = self
            .function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function.index())
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        if self.validation == Some(function.index()) {
            return Ok(());
        }
        capture.dependencies.insert(&dependency);
        capture.folds.capture(&dependency);
        capture.fragments.capture(&dependency);
        capture.sweeps.capture(&dependency);
        Ok(())
    }

    fn visit_expression_once(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> bool {
        if self.validating_actuals
            || self.validation_memo_forces_walk()
            || self.guard_memo_forces_walk()
        {
            return true;
        }
        let dependency = ScalarExpressionDependency {
            activation: self.activation,
            expression: expression.index(),
            field,
            scalar,
            domain_context: self.expression_domain_context(expression),
        };
        match self.function_frames.last() {
            None => self.model_visited.insert(dependency),
            Some(FunctionFrame::Summary { function, .. }) => {
                let function = function.index();
                self.function_summary_captures
                    .last_mut()
                    .filter(|capture| capture.function == function)
                    .is_none_or(|capture| capture.visit_once(dependency))
            }
            Some(FunctionFrame::Actual { .. }) => self
                .frame_memos
                .last_mut()
                .is_none_or(|memo| memo.visited.insert(dependency)),
        }
    }

    fn expression_domain_context(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> domain_context::DomainContextId {
        let domain = self.node(expression).binder_domain();
        self.domain_contexts.for_domain(self.view, domain)
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        self.conditional_value(operands, None, scalar_index)
    }

    fn conditional_value(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        self.walk_conditional(operands, |projection, value| {
            projection.projected_value(value, field, scalar)
        })
    }

    /// One ordered activation inventory for incidence and actual validation.
    fn walk_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        mut walk: impl FnMut(&mut Self, dae::ExprId<'dae>) -> Result<(), ProjectionError>,
    ) -> Result<(), ProjectionError> {
        let mut remaining = self.activation;
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let guard = operands.get(ordinal).expect("checked conditional guard");
            self.with_activation(remaining, |projection| projection.project_guard(guard))?;
            let literal = match self.node(guard).operation() {
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(value)) => Some(*value),
                _ => None,
            };
            if literal == Some(false) {
                continue;
            }
            let value = operands
                .get(ordinal + 1)
                .expect("checked conditional value");
            let selected = if literal == Some(true) {
                remaining
            } else {
                Activation::Conditional
            };
            self.with_activation(selected, |projection| walk(projection, value))?;
            if literal == Some(true) {
                return Ok(());
            }
            remaining = Activation::Conditional;
        }
        let fallback = operands
            .get(operands.len() - 1)
            .expect("checked conditional fallback");
        self.with_activation(remaining, |projection| walk(projection, fallback))
    }

    /// Restore the enclosing activation on success and on every ordinary error.
    fn with_activation<T>(
        &mut self,
        activation: Activation,
        walk: impl FnOnce(&mut Self) -> Result<T, ProjectionError>,
    ) -> Result<T, ProjectionError> {
        let previous = self.activation;
        self.activation = previous.within(activation);
        let result = walk(self);
        self.activation = previous;
        result
    }

    fn conservative_subscript(&self, error: &ProjectionError) -> bool {
        matches!(error, ProjectionError::DynamicSubscript { .. })
            || (self.activation == Activation::Conditional
                && matches!(error, ProjectionError::IndexOutOfBounds { .. }))
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        if operator == dae::BinaryOperator::Multiply {
            return self.multiplication(lhs, rhs, scalar_index);
        }
        let lhs_index = if self.scalar_count(lhs) == 1 {
            0
        } else {
            scalar_index
        };
        let rhs_index = if self.scalar_count(rhs) == 1 {
            0
        } else {
            scalar_index
        };
        self.expression(lhs, lhs_index)?;
        self.expression(rhs, rhs_index)
    }

    fn multiplication(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let lhs_dimensions = self.node(lhs).value_type().dimensions();
        let rhs_dimensions = self.node(rhs).value_type().dimensions();
        let pairs = multiplication_scalar_pairs(lhs_dimensions, rhs_dimensions, scalar_index);
        for (lhs_index, rhs_index) in pairs {
            if !self
                .cache
                .zero_coefficients
                .omits_coordinate(self.view, lhs, rhs, rhs_index)
            {
                self.expression(lhs, lhs_index)?;
            }
            if !self
                .cache
                .zero_coefficients
                .omits_coordinate(self.view, rhs, lhs, lhs_index)
            {
                self.expression(rhs, rhs_index)?;
            }
        }
        Ok(())
    }

    fn array(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let (element, scalar) = scalar_selection::array_scalar(self.view, elements, scalar_index);
        self.expression(element, scalar)
    }

    fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let body_count = self.scalar_count(body);
        let point_index = scalar_index / body_count;
        let body_index = scalar_index % body_count;
        let point = domain_view
            .structured()
            .index_tuple_at(point_index)
            .expect("checked comprehension domain remains valid")
            .expect("checked comprehension scalar index selects its domain");
        self.domain_contexts.push(domain, point);
        let result = self.expression(body, body_index);
        self.domain_contexts.pop();
        result
    }

    fn builtin(
        &mut self,
        node: dae::ExpressionView<'dae>,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        match builtin {
            dae::PureBuiltin::Abs
            | dae::PureBuiltin::Sign
            | dae::PureBuiltin::Sqrt
            | dae::PureBuiltin::Floor
            | dae::PureBuiltin::Ceil
            | dae::PureBuiltin::Integer
            | dae::PureBuiltin::Sin
            | dae::PureBuiltin::Cos
            | dae::PureBuiltin::Tan
            | dae::PureBuiltin::Asin
            | dae::PureBuiltin::Acos
            | dae::PureBuiltin::Atan
            | dae::PureBuiltin::Sinh
            | dae::PureBuiltin::Cosh
            | dae::PureBuiltin::Tanh
            | dae::PureBuiltin::Exp
            | dae::PureBuiltin::Log
            | dae::PureBuiltin::Log10
            | dae::PureBuiltin::NoEvent
            | dae::PureBuiltin::Vector => self.expression(
                arguments
                    .get(0)
                    .expect("checked unary builtin has one argument"),
                scalar_index,
            ),
            dae::PureBuiltin::Transpose => {
                self.transpose(arguments, node.value_type().dimensions(), scalar_index)
            }
            dae::PureBuiltin::LinearSolve => self.coupled_arguments(arguments),
            dae::PureBuiltin::Diagonal
            | dae::PureBuiltin::OuterProduct
            | dae::PureBuiltin::Skew => self.matrix_product(builtin, arguments, node, scalar_index),
            dae::PureBuiltin::Atan2
            | dae::PureBuiltin::Div
            | dae::PureBuiltin::Mod
            | dae::PureBuiltin::Rem
            | dae::PureBuiltin::Homotopy => {
                for argument in arguments.iter() {
                    self.expression(argument, scalar_index)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Smooth => self.expression(
                arguments.get(1).expect("checked smooth value argument"),
                scalar_index,
            ),
            dae::PureBuiltin::Sum | dae::PureBuiltin::Product => self.all_scalars(
                arguments
                    .get(0)
                    .expect("checked reduction has one argument"),
            ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max if arguments.len() == 1 => self
                .all_scalars(
                    arguments
                        .get(0)
                        .expect("checked reduction has one argument"),
                ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                for argument in arguments.iter() {
                    self.expression(argument, scalar_index)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Size => {
                if let Some(dimension) = arguments.get(1) {
                    self.expression(dimension, 0)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Zeros
            | dae::PureBuiltin::Ones
            | dae::PureBuiltin::Fill
            | dae::PureBuiltin::Linspace => self.scalar_arguments(arguments),
            dae::PureBuiltin::Cross => {
                let (first, second) = [(1, 2), (2, 0), (0, 1)][scalar_index];
                for argument in arguments.iter() {
                    self.expression(argument, first)?;
                    self.expression(argument, second)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Identity => self.expression(
                arguments
                    .get(0)
                    .expect("checked identity has one extent argument"),
                0,
            ),
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2 => {
                let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
                self.promoted_concatenation(
                    arguments,
                    axis,
                    node.value_type().dimensions(),
                    scalar_index,
                )
            }
        }
    }

    fn coupled_arguments(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<(), ProjectionError> {
        // A dense implicit solve may couple every result to every coefficient
        // and RHS entry; diagonal structure is not proved here.
        for argument in arguments.iter() {
            self.all_scalars(argument)?;
        }
        Ok(())
    }

    fn transpose(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let operand = arguments.get(0).expect("checked transpose has one operand");
        let mut coordinates = row_major_coordinates(result_dimensions, scalar_index)
            .expect("checked transpose scalar belongs to its result shape");
        coordinates.swap(0, 1);
        let operand_scalar =
            flatten_coordinates(self.node(operand).value_type().dimensions(), &coordinates)
                .expect("transposed coordinate belongs to its checked operand shape");
        self.expression(operand, operand_scalar)
    }

    fn diagonal(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let [_, columns] = result_dimensions else {
            unreachable!("checked diagonal result has rank two")
        };
        let row = scalar_index / *columns as usize;
        let column = scalar_index % *columns as usize;
        if row != column {
            return Ok(());
        }
        self.expression(
            arguments.get(0).expect("checked diagonal has one operand"),
            row,
        )
    }

    fn matrix_product(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        node: dae::ExpressionView<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let result_dimensions = node.value_type().dimensions();
        match builtin {
            dae::PureBuiltin::Diagonal => self.diagonal(arguments, result_dimensions, scalar_index),
            dae::PureBuiltin::OuterProduct => {
                self.outer_product(arguments, result_dimensions, scalar_index)
            }
            dae::PureBuiltin::Skew => self.skew(arguments, scalar_index),
            _ => unreachable!("only compact matrix products use this projection"),
        }
    }

    fn outer_product(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let [_, columns] = result_dimensions else {
            unreachable!("checked outerProduct result has rank two")
        };
        self.expression(
            arguments
                .get(0)
                .expect("checked outerProduct has a left operand"),
            scalar_index / *columns as usize,
        )?;
        self.expression(
            arguments
                .get(1)
                .expect("checked outerProduct has a right operand"),
            scalar_index % *columns as usize,
        )
    }

    fn skew(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let operand_scalar = match scalar_index {
            0 | 4 | 8 => return Ok(()),
            1 | 3 => 2,
            2 | 6 => 1,
            5 | 7 => 0,
            _ => unreachable!("checked skew scalar belongs to its 3x3 result"),
        };
        self.expression(
            arguments.get(0).expect("checked skew has one operand"),
            operand_scalar,
        )
    }

    fn promoted_concatenation(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        axis: usize,
        result_dimensions: &[u32],
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let (argument, scalar) = scalar_selection::concatenation_scalar(
            self.view,
            arguments,
            axis,
            result_dimensions,
            scalar_index,
        );
        self.expression(argument, scalar)
    }

    fn scalar_arguments(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<(), ProjectionError> {
        for argument in arguments.iter() {
            self.expression(argument, 0)?;
        }
        Ok(())
    }

    fn all_scalars(&mut self, expression: dae::ExprId<'dae>) -> Result<(), ProjectionError> {
        for index in 0..self.scalar_count(expression) {
            self.expression(expression, index)?;
        }
        Ok(())
    }

    fn indexed_base_scalar(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_index: usize,
    ) -> Result<usize, ProjectionError> {
        let base_node = self.node(base);
        let result_coordinates = row_major_coordinates(result_dimensions, result_index)
            .expect("checked indexed result scalar is within its shape");
        let mut result_axis = 0usize;
        let mut base_coordinates = Vec::with_capacity(base_node.value_type().dimensions().len());
        for (axis, &extent) in base_node.value_type().dimensions().iter().enumerate() {
            match subscripts.get(axis) {
                Some(dae::SubscriptView::Index {
                    expression,
                    provenance,
                }) => {
                    let index = self.integer(expression, 0)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                }
                Some(dae::SubscriptView::Whole { .. }) | None => {
                    base_coordinates.push(result_coordinates[result_axis]);
                    result_axis += 1;
                }
                Some(dae::SubscriptView::Slice {
                    expression,
                    provenance,
                }) => {
                    let rank = self.node(expression).value_type().dimensions().len();
                    let slice_coordinates =
                        &result_coordinates[result_axis..result_axis.saturating_add(rank)];
                    let slice_index = flatten_coordinates(
                        self.node(expression).value_type().dimensions(),
                        slice_coordinates,
                    )
                    .expect("checked result projection selects a slice element");
                    let index = self.integer(expression, slice_index)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                    result_axis += rank;
                }
            }
        }
        Ok(
            flatten_coordinates(base_node.value_type().dimensions(), &base_coordinates)
                .expect("checked index projection maps into its base"),
        )
    }

    /// Resolve the checked result definition a call continues into.
    fn function_result(
        &self,
        function: dae::FunctionId<'dae>,
        output: u32,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, ProjectionError> {
        let definition = self
            .view
            .function(function)
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        if let Some(external) = definition.external() {
            return Err(external_projection_error(definition, external, span));
        }
        definition
            .result_values()
            .rhs(output as usize)
            .ok_or(ProjectionError::FunctionRecursion { span })
    }

    fn node(&self, expression: dae::ExprId<'dae>) -> dae::ExpressionView<'dae> {
        self.view
            .expression(expression)
            .expect("branded expression resolves in its owning DAE")
    }

    fn scalar_count(&self, expression: dae::ExprId<'dae>) -> usize {
        self.node(expression)
            .value_type()
            .scalar_count()
            .expect("checked expression shape has a representable scalar count")
    }

    fn expect_scalar_index(
        &self,
        node: dae::ExpressionView<'dae>,
        index: usize,
    ) -> Result<(), ProjectionError> {
        let count = node
            .value_type()
            .scalar_count()
            .expect("checked expression shape has a representable scalar count");
        if index < count {
            return Ok(());
        }
        Err(ProjectionError::ScalarOutOfBounds {
            index,
            count,
            span: node.provenance().span(),
        })
    }
}

fn checked_index(index: i64, extent: u32, span: Span) -> Result<u32, ProjectionError> {
    if index < 1 || index > i64::from(extent) {
        return Err(ProjectionError::IndexOutOfBounds {
            index,
            extent,
            span,
        });
    }
    Ok(u32::try_from(index - 1).expect("positive in-range u32 index"))
}

/// The `(lhs, rhs)` scalar factor pairs whose products sum to row-major scalar
/// `scalar` of a checked `Multiply` with these operand dimensions.
pub fn multiplication_scalar_pairs(lhs: &[u32], rhs: &[u32], scalar: usize) -> Vec<(usize, usize)> {
    match (lhs, rhs) {
        ([], _) => vec![(0, scalar)],
        (_, []) => vec![(scalar, 0)],
        ([inner], [rhs_inner]) if inner == rhs_inner => {
            (0..*inner as usize).map(|term| (term, term)).collect()
        }
        ([_, inner], [rhs_inner]) if inner == rhs_inner => {
            let start = scalar * *inner as usize;
            (0..*inner as usize)
                .map(|term| (start + term, term))
                .collect()
        }
        ([inner], [rhs_inner, columns]) if inner == rhs_inner => (0..*inner as usize)
            .map(|term| (term, term * *columns as usize + scalar))
            .collect(),
        ([_, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            let columns = *columns as usize;
            let row = scalar / columns;
            let column = scalar % columns;
            let lhs_start = row * *inner as usize;
            (0..*inner as usize)
                .map(|term| (lhs_start + term, term * columns + column))
                .collect()
        }
        _ => unreachable!("checked multiplication has a valid algebraic shape"),
    }
}
