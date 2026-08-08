#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use rumoca_core::Span;
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
    #[error("record field projection has no checked aggregate definition")]
    UnsupportedRecordOperation { span: Span },
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
    function_results: HashMap<FunctionResultDependency, Vec<FunctionParameterDependency>>,
    marker: std::marker::PhantomData<&'dae ()>,
}

pub fn for_each_scalar_coordinate_cached<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    mut visit: impl FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    let mut projection = Projection {
        view,
        domain_points: match domain_point {
            Some((domain, point)) => vec![(domain, point.to_vec())],
            None => Vec::new(),
        },
        integer_stack: vec![false; view.expression_count()],
        function_frames: Vec::new(),
        function_call_active: HashSet::new(),
        function_fold_active: HashSet::new(),
        function_summary_captures: Vec::new(),
        cache,
        visit: &mut visit,
    };
    projection.expression(root, scalar_index)
}

struct Projection<'visit, 'dae, F> {
    view: dae::DaeView<'dae>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    integer_stack: Vec<bool>,
    function_frames: Vec<FunctionFrame<'dae>>,
    function_call_active: HashSet<FunctionResultDependency>,
    function_fold_active: HashSet<FunctionFoldDependency>,
    function_summary_captures: Vec<FunctionSummaryCapture>,
    cache: &'visit mut ScalarCoordinateProjectionCache<'dae>,
    visit: &'visit mut F,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionFoldDependency {
    function: u32,
    fold: u32,
    carried: u32,
    scalar: usize,
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
    Summary(dae::FunctionId<'dae>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FunctionParameterDependency {
    Scalar {
        parameter: u32,
        scalar: usize,
    },
    RecordField {
        parameter: u32,
        field: usize,
        scalar: usize,
    },
}

#[derive(Debug)]
struct FunctionSummaryCapture {
    function: u32,
    dependencies: Vec<FunctionParameterDependency>,
    cacheable: bool,
    visited: HashSet<FunctionExpressionDependency>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionExpressionDependency {
    expression: u32,
    field: Option<usize>,
    scalar: usize,
    domain_context: Vec<(u32, Vec<i64>)>,
}

impl<'dae, F> Projection<'_, 'dae, F>
where
    F: FnMut(dae::CoordinateView<'dae>, usize),
{
    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        if !matches!(
            node.operation(),
            dae::ExpressionOperation::FunctionFoldParameter { .. }
                | dae::ExpressionOperation::FunctionFoldOutput { .. }
        ) && !self.visit_function_expression_once(expression, None, scalar_index)
        {
            return Ok(());
        }
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
                self.function_fold_dependency(fold, carried, scalar_index)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                match self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                ) {
                    Ok(base_index) => self.expression(base, base_index),
                    Err(ProjectionError::DynamicSubscript { .. }) => {
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
            } => {
                self.expression(base, scalar_index)?;
                self.all_scalars(value)?;
                self.subscripts(subscripts)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(node, builtin, arguments, scalar_index)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
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

    fn function_fold_dependency(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        let dependency = FunctionFoldDependency {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried,
            scalar,
        };
        if !self.function_fold_active.insert(dependency.clone()) {
            return Ok(());
        }
        let fold_view = self
            .view
            .function_fold(fold)
            .expect("checked function fold identity resolves");
        let carried = carried as usize;
        let initial = fold_view
            .initial_values()
            .rhs(carried)
            .expect("checked fold carried ordinal has an initial value");
        let projected = (|| {
            self.expression(initial, scalar)?;
            let domain = self
                .view
                .domain(fold_view.domain())
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
                self.domain_points.push((fold_view.domain(), point));
                self.expression(update, scalar)?;
                self.domain_points.pop();
            }
            Ok(())
        })();
        self.function_fold_active.remove(&dependency);
        projected
    }

    fn emit_coordinate(&mut self, coordinate: dae::CoordinateView<'dae>, scalar: usize) {
        (self.visit)(coordinate, scalar);
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
        let Some(frame) = self.function_frames.last().cloned() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        match frame {
            FunctionFrame::Actual {
                function,
                arguments,
            } if function == parameter.function() => arguments
                .get(parameter.ordinal() as usize)
                .copied()
                .ok_or(ProjectionError::FunctionRecursion { span })
                .and_then(|argument| self.expression(argument, scalar_index)),
            FunctionFrame::Summary(function) if function == parameter.function() => self
                .capture_function_parameter(
                    function,
                    FunctionParameterDependency::Scalar {
                        parameter: parameter.ordinal(),
                        scalar: scalar_index,
                    },
                    span,
                ),
            _ => Err(ProjectionError::FunctionRecursion { span }),
        }
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
        let dependency = FunctionResultDependency {
            function: function.index(),
            output,
            field: None,
            scalar: scalar_index,
        };
        self.project_function_result(dependency, function, arguments, span)
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
        let dependency = FunctionResultDependency {
            function: function.index(),
            output,
            field: Some(field),
            scalar,
        };
        self.project_function_result(dependency, function, arguments, span)
    }

    fn project_function_result(
        &mut self,
        dependency: FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let summary = match self.cache.function_results.get(&dependency).cloned() {
            Some(summary) => Some(summary),
            None => self.derive_function_summary(&dependency, function, span)?,
        };
        let Some(summary) = summary else {
            return self.project_function_result_direct(&dependency, function, arguments, span);
        };
        for parameter in summary {
            match parameter {
                FunctionParameterDependency::Scalar { parameter, scalar } => {
                    let argument = arguments
                        .get(parameter as usize)
                        .copied()
                        .ok_or(ProjectionError::FunctionRecursion { span })?;
                    self.expression(argument, scalar)?;
                }
                FunctionParameterDependency::RecordField {
                    parameter,
                    field,
                    scalar,
                } => {
                    let argument = arguments
                        .get(parameter as usize)
                        .copied()
                        .ok_or(ProjectionError::FunctionRecursion { span })?;
                    self.record_field(argument, field, scalar)?;
                }
            }
        }
        Ok(())
    }

    fn derive_function_summary(
        &mut self,
        dependency: &FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        span: Span,
    ) -> Result<Option<Vec<FunctionParameterDependency>>, ProjectionError> {
        if !self.function_call_active.insert(dependency.clone()) {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        self.function_summary_captures.push(FunctionSummaryCapture {
            function: function.index(),
            dependencies: Vec::new(),
            cacheable: true,
            visited: HashSet::new(),
        });
        self.function_frames.push(FunctionFrame::Summary(function));
        let result = self.function_result(function, dependency.output, span)?;
        let projected = match dependency.field {
            Some(field) => self.record_field(result, field, dependency.scalar),
            None => self.expression(result, dependency.scalar),
        };
        self.function_frames.pop();
        let capture = self
            .function_summary_captures
            .pop()
            .expect("function summary capture was just pushed");
        self.function_call_active.remove(dependency);
        projected?;
        if !capture.cacheable {
            return Ok(None);
        }
        self.cache
            .function_results
            .insert(dependency.clone(), capture.dependencies.clone());
        Ok(Some(capture.dependencies))
    }

    fn project_function_result_direct(
        &mut self,
        dependency: &FunctionResultDependency,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let result = self.function_result(function, dependency.output, span)?;
        self.function_frames.push(FunctionFrame::Actual {
            function,
            arguments,
        });
        let projected = match dependency.field {
            Some(field) => self.record_field(result, field, dependency.scalar),
            None => self.expression(result, dependency.scalar),
        };
        self.function_frames.pop();
        projected
    }

    fn capture_function_parameter(
        &mut self,
        function: dae::FunctionId<'dae>,
        dependency: FunctionParameterDependency,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let capture = self
            .function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function.index())
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        if !capture.dependencies.contains(&dependency) {
            capture.dependencies.push(dependency);
        }
        Ok(())
    }

    fn visit_function_expression_once(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> bool {
        let Some(FunctionFrame::Summary(function)) = self.function_frames.last() else {
            return true;
        };
        let function = function.index();
        let dependency = FunctionExpressionDependency {
            expression: expression.index(),
            field,
            scalar,
            domain_context: self.function_expression_domain_context(expression),
        };
        self.function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function)
            .is_none_or(|capture| capture.visited.insert(dependency))
    }

    fn function_expression_domain_context(
        &self,
        expression: dae::ExprId<'dae>,
    ) -> Vec<(u32, Vec<i64>)> {
        let Some(mut domain) = self.node(expression).binder_domain() else {
            return Vec::new();
        };
        let mut lexical_domains = Vec::new();
        loop {
            lexical_domains.push(domain.index());
            let Some(parent) = self
                .view
                .domain(domain)
                .expect("checked expression binder domain resolves")
                .parent()
            else {
                break;
            };
            domain = parent;
        }
        self.domain_points
            .iter()
            .filter(|(domain, _)| lexical_domains.contains(&domain.index()))
            .map(|(domain, point)| (domain.index(), point.clone()))
            .collect()
    }

    fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        if !self.visit_function_expression_once(expression, Some(field), scalar_index) {
            return Ok(());
        }
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .expect("checked record field ordinal is in range"),
                scalar_index,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.function_call_record_field(
                function,
                output,
                arguments,
                field,
                scalar_index,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar_index)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.function_parameter_field(
                parameter,
                field,
                scalar_index,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Conditional(operands) => {
                let fallback = operands
                    .get(operands.len() - 1)
                    .expect("checked conditional has a fallback");
                for ordinal in (0..operands.len() - 1).step_by(2) {
                    self.expression(
                        operands
                            .get(ordinal)
                            .expect("checked conditional condition ordinal"),
                        0,
                    )?;
                    self.record_field(
                        operands
                            .get(ordinal + 1)
                            .expect("checked conditional value ordinal"),
                        field,
                        scalar_index,
                    )?;
                }
                self.record_field(fallback, field, scalar_index)
            }
            dae::ExpressionOperation::Array(elements) => {
                self.record_array_field(elements, field, scalar_index)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.record_comprehension_field(domain, body, field, scalar_index)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.indexed_record_field(expression, base, subscripts, field, scalar_index)
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                self.record_field(base, field, scalar_index)?;
                self.all_record_field_scalars(value, field)?;
                self.subscripts(subscripts)
            }
            _ => Err(ProjectionError::UnsupportedRecordOperation {
                span: node.provenance().span(),
            }),
        }
    }

    fn function_parameter_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let Some(frame) = self.function_frames.last().cloned() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        match frame {
            FunctionFrame::Actual {
                function,
                arguments,
            } if function == parameter.function() => arguments
                .get(parameter.ordinal() as usize)
                .copied()
                .ok_or(ProjectionError::FunctionRecursion { span })
                .and_then(|argument| self.record_field(argument, field, scalar_index)),
            FunctionFrame::Summary(function) if function == parameter.function() => self
                .capture_function_parameter(
                    function,
                    FunctionParameterDependency::RecordField {
                        parameter: parameter.ordinal(),
                        field,
                        scalar: scalar_index,
                    },
                    span,
                ),
            _ => Err(ProjectionError::FunctionRecursion { span }),
        }
    }

    fn record_array_field(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let first = elements.get(0).expect("checked record array is nonempty");
        let element_count = self.record_field_scalar_count(first, field);
        let element = elements
            .get(scalar_index / element_count)
            .expect("checked record field scalar selects an array element");
        self.record_field(element, field, scalar_index % element_count)
    }

    fn record_comprehension_field(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let body_count = self.record_field_scalar_count(body, field);
        let point_index = scalar_index / body_count;
        let point = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves")
            .structured()
            .index_tuple_at(point_index)
            .expect("checked comprehension domain remains valid")
            .expect("checked record field scalar selects its domain");
        self.domain_points.push((domain, point));
        let result = self.record_field(body, field, scalar_index % body_count);
        self.domain_points.pop();
        result
    }

    fn indexed_record_field(
        &mut self,
        indexed: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let field_width = self.record_field_width(indexed, field);
        let record_index = scalar_index / field_width;
        let field_index = scalar_index % field_width;
        let base_record = self.indexed_base_scalar(
            base,
            subscripts,
            self.node(indexed).value_type().dimensions(),
            record_index,
        )?;
        self.record_field(base, field, base_record * field_width + field_index)
    }

    fn all_record_field_scalars(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
    ) -> Result<(), ProjectionError> {
        for scalar in 0..self.record_field_scalar_count(expression, field) {
            self.record_field(expression, field, scalar)?;
        }
        Ok(())
    }

    fn record_field_scalar_count(&self, expression: dae::ExprId<'dae>, field: usize) -> usize {
        let layout = self.record_layout(expression, field);
        layout.outer_count() * layout.field_width()
    }

    fn record_field_width(&self, expression: dae::ExprId<'dae>, field: usize) -> usize {
        self.record_layout(expression, field).field_width()
    }

    fn record_layout(&self, expression: dae::ExprId<'dae>, field: usize) -> dae::RecordFieldLayout {
        let node = self.node(expression);
        self.view
            .record_field_layout(node.value_type_id(), field)
            .expect("checked record projection has a finite field layout")
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let fallback = operands
            .get(operands.len() - 1)
            .expect("checked conditional has a fallback");
        for ordinal in (0..operands.len() - 1).step_by(2) {
            self.expression(
                operands
                    .get(ordinal)
                    .expect("checked conditional condition ordinal"),
                0,
            )?;
            self.expression(
                operands
                    .get(ordinal + 1)
                    .expect("checked conditional value ordinal"),
                scalar_index,
            )?;
        }
        self.expression(fallback, scalar_index)
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
            self.expression(lhs, lhs_index)?;
            self.expression(rhs, rhs_index)?;
        }
        Ok(())
    }

    fn array(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let first = elements.get(0).expect("checked array is nonempty");
        let element_count = self.scalar_count(first);
        let element_ordinal = scalar_index / element_count;
        let element_index = scalar_index % element_count;
        self.expression(
            elements
                .get(element_ordinal)
                .expect("checked array scalar index selects an element"),
            element_index,
        )
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
        self.domain_points.push((domain, point));
        let result = self.expression(body, body_index);
        self.domain_points.pop();
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
            | dae::PureBuiltin::Vector => self.expression(
                arguments
                    .get(0)
                    .expect("checked unary builtin has one argument"),
                scalar_index,
            ),
            dae::PureBuiltin::Transpose => {
                self.transpose(arguments, node.value_type().dimensions(), scalar_index)
            }
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
            dae::PureBuiltin::NoEvent => self.expression(
                arguments.get(0).expect("checked noEvent value argument"),
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
        let mut coordinates = row_major_coordinates(result_dimensions, scalar_index)
            .expect("checked concatenation scalar belongs to its result shape");
        let selected = coordinates[axis];
        let mut offset = 0_u32;
        for argument in arguments.iter() {
            let dimensions = self.node(argument).value_type().dimensions();
            let extent = dimensions.get(axis).copied().unwrap_or(1);
            let end = offset
                .checked_add(extent)
                .expect("checked concatenation extent remains in the u32 domain");
            if selected < end {
                coordinates[axis] = selected - offset;
                let operand_scalar =
                    flatten_coordinates(dimensions, &coordinates[..dimensions.len()])
                        .expect("checked promoted coordinate belongs to its operand shape");
                return self.expression(argument, operand_scalar);
            }
            offset = end;
        }
        unreachable!("checked concatenation operands cover the result")
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

    fn integer(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let raw = expression.index() as usize;
        if self.integer_stack[raw] {
            return Err(ProjectionError::DynamicSubscript {
                span: self.node(expression).provenance().span(),
            });
        }
        self.integer_stack[raw] = true;
        let result = self.integer_inner(expression, scalar_index);
        self.integer_stack[raw] = false;
        result
    }

    fn integer_inner(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value),
            dae::ExpressionOperation::Range(range) => {
                let offset = i64::try_from(scalar_index)
                    .map_err(|_| ProjectionError::IntegerOverflow { span })?;
                range
                    .start()
                    .value()
                    .checked_add(
                        range
                            .effective_step()
                            .checked_mul(offset)
                            .ok_or(ProjectionError::IntegerOverflow { span })?,
                    )
                    .ok_or(ProjectionError::IntegerOverflow { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let Some((_, point)) = self
                    .domain_points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                else {
                    return Err(ProjectionError::DynamicSubscript { span });
                };
                point
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or(ProjectionError::DynamicSubscript { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.integer_parameter(parameter, scalar_index, span),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable
                    .binding()
                    .ok_or(ProjectionError::DynamicSubscript { span })?;
                self.integer(binding, scalar_index)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.integer(operand, scalar_index)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => value
                        .checked_neg()
                        .ok_or(ProjectionError::IntegerOverflow { span }),
                    dae::UnaryOperator::Not => Err(ProjectionError::DynamicSubscript { span }),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.integer(lhs, scalar_index)?;
                let rhs = self.integer(rhs, scalar_index)?;
                integer_binary(operator, lhs, rhs, span)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.integer_call(function, output, arguments, scalar_index, span),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let element_count = self.scalar_count(first);
                self.integer(
                    elements
                        .get(scalar_index / element_count)
                        .expect("checked integer array projection selects an element"),
                    scalar_index % element_count,
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base_index = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                )?;
                self.integer(base, base_index)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.integer(definition.rhs(), scalar_index)
            }
            _ => Err(ProjectionError::DynamicSubscript { span }),
        }
    }

    fn integer_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        let Some(frame) = self.function_frames.last().cloned() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        match frame {
            FunctionFrame::Actual {
                function,
                arguments,
            } if function == parameter.function() => arguments
                .get(parameter.ordinal() as usize)
                .copied()
                .ok_or(ProjectionError::FunctionRecursion { span })
                .and_then(|argument| self.integer(argument, scalar_index)),
            FunctionFrame::Summary(function) if function == parameter.function() => {
                if let Some(capture) = self
                    .function_summary_captures
                    .last_mut()
                    .filter(|capture| capture.function == function.index())
                {
                    capture.cacheable = false;
                }
                Err(ProjectionError::DynamicSubscript { span })
            }
            _ => Err(ProjectionError::FunctionRecursion { span }),
        }
    }

    fn integer_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        if self.function_frames.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let result = self.function_result(function, output, span)?;
        self.function_frames.push(FunctionFrame::Actual {
            function,
            arguments: arguments.iter().collect(),
        });
        let value = self.integer(result, scalar_index);
        self.function_frames.pop();
        value
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

fn integer_binary(
    operator: dae::BinaryOperator,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, ProjectionError> {
    let overflow = || ProjectionError::IntegerOverflow { span };
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
            lhs.checked_add(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
            lhs.checked_sub(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
            lhs.checked_mul(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0 => {
            lhs.checked_div(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower if rhs >= 0 => lhs
            .checked_pow(u32::try_from(rhs).map_err(|_| overflow())?)
            .ok_or_else(overflow),
        _ => Err(ProjectionError::DynamicSubscript { span }),
    }
}

fn multiplication_scalar_pairs(lhs: &[u32], rhs: &[u32], scalar: usize) -> Vec<(usize, usize)> {
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

fn row_major_coordinates(extents: &[u32], index: usize) -> Option<Vec<u32>> {
    let scalar_count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    if index >= scalar_count {
        return None;
    }
    let mut remainder = index;
    let mut coordinates = Vec::with_capacity(extents.len());
    for extent in extents.iter().rev() {
        if *extent == 0 {
            return None;
        }
        coordinates.push(u32::try_from(remainder % *extent as usize).ok()?);
        remainder /= *extent as usize;
    }
    coordinates.reverse();
    Some(coordinates)
}

fn flatten_coordinates(extents: &[u32], coordinates: &[u32]) -> Option<usize> {
    if extents.len() != coordinates.len() {
        return None;
    }
    extents
        .iter()
        .zip(coordinates)
        .try_fold(0usize, |flat, (extent, coordinate)| {
            if coordinate >= extent {
                return None;
            }
            flat.checked_mul(*extent as usize)?
                .checked_add(*coordinate as usize)
        })
}
