//! Target-neutral serialized views over checked Algorithm Code.
//!
//! These adapters expose semantic facts and construction-issued presentation
//! facts. Templates own lexical spelling only; identifiers and package-member
//! relations come from sealed presentation/layout values.

use std::collections::BTreeSet;

use rumoca_ir_galec::TracedAlgorithmCodeProduct;
use rumoca_ir_galec::ast;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use serde::Serialize;

use super::algorithm_code_artifact_layout::{
    AlgorithmCodeArtifactLayout, AlgorithmCodeRepresentationFile,
};
use super::solve_algorithm_production::{
    PreparedSolveAlgorithmProduction, ProductionArtifactRepresentationFile,
    ProductionDeclarationPresentation, ProductionMethodPresentation, ProductionPresentationPlan,
};

#[derive(Debug, Clone, Serialize)]
pub(crate) struct AlgorithmCodeView<'a> {
    package: PackageRoot<'a>,
    presentation: AlgorithmCodeManifestPresentation<'a>,
    artifact_layout: AlgorithmCodeArtifactLayoutView<'a>,
    variables: Vec<VariableView<'a>>,
    methods: MethodsView,
    /// The file-level trace legend: every Modelica file this block's statement
    /// anchors name, plus the source root their paths are relative to. A target
    /// emits it so the generated artifact documents its own trace universe
    /// instead of leaving a reviewer to guess which files the `path:line`
    /// anchors refer to, and so no build-machine path reaches the artifact
    /// (SPEC_0034 GAL-032).
    traces: super::source_trace::TraceLegend,
}

/// Source-only Algorithm Code projection. This view contains no manifest
/// presentation or package-member relation and is the sole context available
/// to the generic renderer.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct AlgorithmCodeSourceView<'a> {
    package: PackageRoot<'a>,
    methods: MethodsView,
    traces: super::source_trace::TraceLegend,
}

#[derive(Debug, Clone, Copy, Serialize)]
struct AlgorithmCodeArtifactLayoutView<'a> {
    algorithm_code_source: &'a AlgorithmCodeRepresentationFile,
}

#[derive(Debug, Clone, Serialize)]
struct PackageRoot<'a> {
    /// The immutable syntax retained by the checked package.  Algorithm Code
    /// source rendering is a spelling of this exact tree; it never builds a
    /// target-side typed program, storage plan, or execution strategy.
    block: &'a ast::Block,
}

#[derive(Debug, Clone, Serialize)]
struct AlgorithmCodeManifestPresentation<'a> {
    manifest_name: &'a str,
    algorithm_code_file: AlgorithmCodeManifestIdentifier,
    clock: AlgorithmCodeManifestIdentifier,
    clock_variable: AlgorithmCodeManifestIdentifier,
    startup_method: AlgorithmCodeManifestIdentifier,
    recalibrate_method: AlgorithmCodeManifestIdentifier,
    do_step_method: AlgorithmCodeManifestIdentifier,
    error_signal_status: AlgorithmCodeManifestIdentifier,
}

#[derive(Debug, Clone, Serialize)]
#[serde(transparent)]
struct AlgorithmCodeManifestIdentifier(String);

impl AlgorithmCodeManifestIdentifier {
    fn issue(value: String, definitions: &mut BTreeSet<String>) -> Result<Self, String> {
        if value.is_empty() {
            return Err("Algorithm Code manifest identifier is empty".to_owned());
        }
        let first = value.as_bytes()[0];
        if !(first.is_ascii_alphabetic() || first == b'_')
            || !value.as_bytes()[1..].iter().all(|character| {
                character.is_ascii_alphanumeric() || matches!(character, b'_' | b'.' | b'-')
            })
        {
            return Err(format!(
                "Algorithm Code manifest identifier `{value}` is not an XML identifier"
            ));
        }
        if !definitions.insert(value.clone()) {
            return Err(format!(
                "Algorithm Code manifest identifier `{value}` is not unique"
            ));
        }
        Ok(Self(value))
    }
}

impl<'a> AlgorithmCodeManifestPresentation<'a> {
    fn construct(
        manifest_name: &'a str,
        variable_count: usize,
        clock_variable_ordinal: usize,
    ) -> Result<(Self, Vec<AlgorithmCodeManifestIdentifier>), String> {
        let mut definitions = BTreeSet::new();
        let algorithm_code_file = AlgorithmCodeManifestIdentifier::issue(
            "F_ALGORITHM_CODE".to_owned(),
            &mut definitions,
        )?;
        let clock = AlgorithmCodeManifestIdentifier::issue("CLK".to_owned(), &mut definitions)?;
        let startup_method =
            AlgorithmCodeManifestIdentifier::issue("BM_STARTUP".to_owned(), &mut definitions)?;
        let recalibrate_method =
            AlgorithmCodeManifestIdentifier::issue("BM_RECALIBRATE".to_owned(), &mut definitions)?;
        let do_step_method =
            AlgorithmCodeManifestIdentifier::issue("BM_DOSTEP".to_owned(), &mut definitions)?;
        let error_signal_status =
            AlgorithmCodeManifestIdentifier::issue("ESS".to_owned(), &mut definitions)?;
        let variable_identifiers = (1..=variable_count)
            .map(|ordinal| {
                AlgorithmCodeManifestIdentifier::issue(format!("V{ordinal}"), &mut definitions)
            })
            .collect::<Result<Vec<_>, _>>()?;
        // AlgorithmCodePackage construction proves that the retained
        // one-based clock ordinal names exactly one declaration. This direct
        // projection deliberately relies on that proof instead of rechecking
        // the same invariant in presentation code.
        let clock_variable = variable_identifiers[clock_variable_ordinal - 1].clone();
        Ok((
            Self {
                manifest_name,
                algorithm_code_file,
                clock,
                clock_variable,
                startup_method,
                recalibrate_method,
                do_step_method,
                error_signal_status,
            },
            variable_identifiers,
        ))
    }
}

impl<'a> AlgorithmCodeView<'a> {
    /// Project a packaged Algorithm Code block for rendering.
    ///
    pub(crate) fn new(
        product: &'a TracedAlgorithmCodeProduct<'_>,
        artifact_layout: &'a AlgorithmCodeArtifactLayout,
    ) -> Result<Self, String> {
        let package = product.package();
        let block = package.block();
        let block_name = name_of(&block.name);
        let (presentation, variable_identifiers) = AlgorithmCodeManifestPresentation::construct(
            block_name,
            package.variable_nominals().len(),
            package.clock_variable_ordinal(),
        )?;
        let declarations = block
            .interface
            .iter()
            .map(|variable| {
                (
                    &variable.decl,
                    variable.start.as_ref(),
                    match variable.kind {
                        ast::InterfaceKind::Input => "input",
                        ast::InterfaceKind::Output => "output",
                        ast::InterfaceKind::TunableParameter => "tunable_parameter",
                    },
                )
            })
            .chain(block.protected.iter().map(|variable| {
                (
                    &variable.decl,
                    variable.start.as_ref(),
                    match variable.kind {
                        ast::ProtectedKind::DependentParameter => "dependent_parameter",
                        ast::ProtectedKind::Constant => "constant",
                        ast::ProtectedKind::State => "state",
                    },
                )
            }));
        let variable_nominals = package.variable_nominals();
        let variables = declarations
            .enumerate()
            .map(|(index, (declaration, start, causality))| {
                VariableView::new(
                    variable_identifiers[index].clone(),
                    declaration,
                    start,
                    causality,
                    variable_nominals[index],
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            package: PackageRoot { block },
            presentation,
            artifact_layout: AlgorithmCodeArtifactLayoutView {
                algorithm_code_source: artifact_layout.algorithm_code_source_file(),
            },
            variables,
            methods: MethodsView::new(block),
            traces: trace_legend(product),
        })
    }
}

impl<'a> AlgorithmCodeSourceView<'a> {
    pub(crate) fn new(product: &'a TracedAlgorithmCodeProduct<'_>) -> Self {
        let package = product.package();
        let block = package.block();
        Self {
            package: PackageRoot { block },
            methods: MethodsView::new(block),
            traces: trace_legend(product),
        }
    }
}

/// Correlated Algorithm Code manifest projection for one prepared eFMI
/// product. Variable and method presentation identities are joined here once;
/// XML templates never align lists, calculate ordinals, or spell cross-file
/// identifiers.
#[derive(Debug, Serialize)]
pub(crate) struct CorrelatedAlgorithmCodeView<'a> {
    package: PackageRoot<'a>,
    variables: Vec<CorrelatedVariableView<'a>>,
    methods: MethodsView,
    manifest_methods: CorrelatedMethodsView<'a>,
    traces: super::source_trace::TraceLegend,
    presentation: &'a ProductionPresentationPlan,
    artifact_layout: CorrelatedArtifactLayoutView<'a>,
}

#[derive(Debug, Serialize)]
struct CorrelatedArtifactLayoutView<'a> {
    algorithm_code_manifest: &'a ProductionArtifactRepresentationFile,
    algorithm_code_source: &'a ProductionArtifactRepresentationFile,
    production_manifest: &'a ProductionArtifactRepresentationFile,
}

#[derive(Debug, Serialize)]
struct CorrelatedVariableView<'a> {
    #[serde(flatten)]
    semantic: VariableSemanticView<'a>,
    presentation: &'a ProductionDeclarationPresentation,
}

#[derive(Debug, Serialize)]
struct CorrelatedMethodsView<'a> {
    startup: CorrelatedMethodView<'a>,
    recalibrate: CorrelatedMethodView<'a>,
    do_step: CorrelatedMethodView<'a>,
}

#[derive(Debug, Serialize)]
struct CorrelatedMethodView<'a> {
    presentation: &'a ProductionMethodPresentation,
}

impl<'a> CorrelatedAlgorithmCodeView<'a> {
    pub(crate) fn new(
        production: &'a PreparedSolveAlgorithmProduction<'_>,
    ) -> Result<Self, String> {
        let package = production.algorithm_code();
        let block = package.block();
        let declarations = block
            .interface
            .iter()
            .map(|variable| {
                (
                    &variable.decl,
                    variable.start.as_ref(),
                    match variable.kind {
                        ast::InterfaceKind::Input => "input",
                        ast::InterfaceKind::Output => "output",
                        ast::InterfaceKind::TunableParameter => "tunable_parameter",
                    },
                )
            })
            .chain(block.protected.iter().map(|variable| {
                (
                    &variable.decl,
                    variable.start.as_ref(),
                    match variable.kind {
                        ast::ProtectedKind::DependentParameter => "dependent_parameter",
                        ast::ProtectedKind::Constant => "constant",
                        ast::ProtectedKind::State => "state",
                    },
                )
            }));
        let variable_nominals = package.variable_nominals();
        let variables = declarations
            .enumerate()
            .map(|(index, (declaration, start, causality))| {
                VariableSemanticView::new(declaration, start, causality, variable_nominals[index])
            })
            .collect::<Result<Vec<_>, _>>()?;
        let source = AlgorithmCodeSourceView::new(production.traced_algorithm_code());
        let presentation = production.presentation();
        let artifact_layout = CorrelatedArtifactLayoutView {
            algorithm_code_manifest: presentation
                .artifact_layout()
                .algorithm_code_manifest_file(),
            algorithm_code_source: presentation.artifact_layout().algorithm_code_source_file(),
            production_manifest: presentation.artifact_layout().production_manifest_file(),
        };
        // SolveAlgorithmProduct construction proves the package declarations
        // and Solve declarations are one ordered correlated family. Indexing
        // uses that retained proof; no second cardinality check or truncating
        // zip is introduced at presentation time.
        let variables = variables
            .into_iter()
            .enumerate()
            .map(|(index, semantic)| CorrelatedVariableView {
                semantic,
                presentation: &presentation.declarations()[index],
            })
            .collect();
        let manifest_methods = CorrelatedMethodsView {
            startup: CorrelatedMethodView {
                presentation: presentation
                    .method(rumoca_ir_solve::SolveAlgorithmMethodKind::Startup),
            },
            recalibrate: CorrelatedMethodView {
                presentation: presentation
                    .method(rumoca_ir_solve::SolveAlgorithmMethodKind::Recalibrate),
            },
            do_step: CorrelatedMethodView {
                presentation: presentation
                    .method(rumoca_ir_solve::SolveAlgorithmMethodKind::DoStep),
            },
        };
        Ok(Self {
            package: source.package,
            variables,
            methods: source.methods,
            manifest_methods,
            traces: source.traces,
            presentation,
            artifact_layout,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
struct VariableView<'a> {
    #[serde(flatten)]
    semantic: VariableSemanticView<'a>,
    manifest: AlgorithmCodeVariablePresentation,
}

#[derive(Debug, Clone, Serialize)]
struct VariableSemanticView<'a> {
    kind: &'static str,
    name: &'a str,
    causality: &'static str,
    dimensions: Vec<DimensionView>,
    start: StartView,
    real_min: Option<f64>,
    real_max: Option<f64>,
    real_nominal: Option<f64>,
    integer_min: Option<i64>,
    integer_max: Option<i64>,
}

#[derive(Debug, Clone, Serialize)]
struct AlgorithmCodeVariablePresentation {
    algorithm_code_identity: AlgorithmCodeManifestIdentifier,
}

#[derive(Debug, Clone, Copy, Serialize)]
struct DimensionView {
    number: usize,
    size: u64,
}

impl<'a> VariableView<'a> {
    fn new(
        algorithm_code_identity: AlgorithmCodeManifestIdentifier,
        declaration: &'a ast::VariableDeclaration,
        start: Option<&ast::Expression>,
        causality: &'static str,
        nominal: Option<f64>,
    ) -> Result<Self, String> {
        Ok(Self {
            semantic: VariableSemanticView::new(declaration, start, causality, nominal)?,
            manifest: AlgorithmCodeVariablePresentation {
                algorithm_code_identity,
            },
        })
    }
}

impl<'a> VariableSemanticView<'a> {
    fn new(
        declaration: &'a ast::VariableDeclaration,
        start: Option<&ast::Expression>,
        causality: &'static str,
        nominal: Option<f64>,
    ) -> Result<Self, String> {
        let ast::TypeRef::Primitive(scalar) = declaration.ty else {
            return Err(format!(
                "container views do not support compartment root `{}`",
                declaration.name.lexeme()
            ));
        };
        let dimensions = literal_dimensions(declaration)?
            .into_iter()
            .enumerate()
            .map(|(index, size)| {
                let number = index.checked_add(1).ok_or_else(|| {
                    format!(
                        "Algorithm Code dimension number overflow for `{}`",
                        declaration.name.lexeme()
                    )
                })?;
                Ok(DimensionView { number, size })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let start = start.ok_or_else(|| {
            format!(
                "checked projection omitted start semantics for `{}`",
                declaration.name.lexeme()
            )
        })?;
        let (real_min, real_max, integer_min, integer_max) =
            range_values(scalar, &declaration.range)?;
        Ok(Self {
            kind: scalar_kind(scalar),
            name: declaration.name.lexeme(),
            causality,
            dimensions,
            start: StartView::new(scalar, start, declaration.dimensions.is_empty())?,
            real_min,
            real_max,
            real_nominal: (scalar == ast::ScalarType::Real)
                .then_some(nominal)
                .flatten(),
            integer_min,
            integer_max,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum StartView {
    Real { start: StartPayload<f64> },
    Integer { start: StartPayload<i64> },
    Boolean { start: StartPayload<bool> },
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "form", rename_all = "snake_case")]
enum StartPayload<T> {
    Scalar { value: T },
    Tensor { values: Vec<T> },
}

impl StartView {
    fn new(
        scalar_type: ast::ScalarType,
        expression: &ast::Expression,
        scalar: bool,
    ) -> Result<Self, String> {
        match scalar_type {
            ast::ScalarType::Real if scalar => match expression {
                ast::Expression::Real(value) => Ok(Self::Real {
                    start: StartPayload::Scalar { value: *value },
                }),
                _ => Err("checked Real scalar start is not one Real literal".to_owned()),
            },
            ast::ScalarType::Real => {
                let mut values = Vec::new();
                flatten_real(expression, &mut values)?;
                Ok(Self::Real {
                    start: StartPayload::Tensor { values },
                })
            }
            ast::ScalarType::Integer if scalar => match expression {
                ast::Expression::Integer(value) => Ok(Self::Integer {
                    start: StartPayload::Scalar { value: *value },
                }),
                _ => Err("checked Integer scalar start is not one Integer literal".to_owned()),
            },
            ast::ScalarType::Integer => {
                let mut values = Vec::new();
                flatten_integer(expression, &mut values)?;
                Ok(Self::Integer {
                    start: StartPayload::Tensor { values },
                })
            }
            ast::ScalarType::Boolean if scalar => match expression {
                ast::Expression::Bool(value) => Ok(Self::Boolean {
                    start: StartPayload::Scalar { value: *value },
                }),
                _ => Err("checked Boolean scalar start is not one Boolean literal".to_owned()),
            },
            ast::ScalarType::Boolean => {
                let mut values = Vec::new();
                flatten_boolean(expression, &mut values)?;
                Ok(Self::Boolean {
                    start: StartPayload::Tensor { values },
                })
            }
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct MethodsView {
    startup_signals: Vec<&'static str>,
    recalibrate_signals: Vec<&'static str>,
    do_step_signals: Vec<&'static str>,
}

impl MethodsView {
    fn new(block: &ast::Block) -> Self {
        Self {
            startup_signals: signal_names(&block.startup.signals),
            recalibrate_signals: signal_names(&block.recalibrate.signals),
            do_step_signals: signal_names(&block.do_step.signals),
        }
    }
}

fn signal_names(signals: &[ast::PredefinedSignal]) -> Vec<&'static str> {
    signals.iter().map(|signal| signal.name()).collect()
}

/// Target-neutral view for a validated standalone `.alg` block.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct CheckedAlgorithmBlockView<'a> {
    package: CheckedBlockRoot<'a>,
    block_name: &'a str,
    variables: Vec<CheckedBlockVariable<'a>>,
    methods: MethodsView,
    /// See [`AlgorithmCodeView::traces`]; the two render paths carry the
    /// same trace legend so a target template reads one name.
    traces: super::source_trace::TraceLegend,
}

#[derive(Debug, Clone, Serialize)]
struct CheckedBlockRoot<'a> {
    block: &'a ast::Block,
}

/// A checked-block variable as the standalone (package-free) render path sees
/// it.
///
/// The declared range fields are NOT decoration: SPEC_0042 T3 makes a declared
/// `min`/`max` a saturation the target must apply at every method boundary, so
/// a view that dropped them would let this path emit silently unclamped code
/// while [`VariableView`] (the packaged path) clamps. The two views therefore
/// carry the same range facts under the same names, and `range_values` rejects
/// a non-literal bound on both.
#[derive(Debug, Clone, Serialize)]
struct CheckedBlockVariable<'a> {
    kind: &'static str,
    ordinal: usize,
    name: &'a str,
    causality: &'static str,
    dimensions: Vec<u64>,
    real_min: Option<f64>,
    real_max: Option<f64>,
    integer_min: Option<i64>,
    integer_max: Option<i64>,
}

impl<'a> CheckedAlgorithmBlockView<'a> {
    /// Project a standalone checked block. This editor-only path has no DAE
    /// origin authority, so it emits no file legend instead of accepting a
    /// caller-selected source map.
    pub(crate) fn new(checked: &'a CheckedAlgorithmBlock) -> Result<Self, String> {
        let block = checked.block();
        let block_name = name_of(&block.name);
        let variables = block
            .interface
            .iter()
            .map(|variable| {
                (
                    &variable.decl,
                    match variable.kind {
                        ast::InterfaceKind::Input => "input",
                        ast::InterfaceKind::Output => "output",
                        ast::InterfaceKind::TunableParameter => "tunable_parameter",
                    },
                )
            })
            .chain(block.protected.iter().map(|variable| {
                (
                    &variable.decl,
                    match variable.kind {
                        ast::ProtectedKind::DependentParameter => "dependent_parameter",
                        ast::ProtectedKind::Constant => "constant",
                        ast::ProtectedKind::State => "state",
                    },
                )
            }))
            .enumerate()
            .map(|(index, (declaration, causality))| {
                let ast::TypeRef::Primitive(scalar) = declaration.ty else {
                    return Err(format!(
                        "standalone target does not support compartment root `{}`",
                        declaration.name.lexeme()
                    ));
                };
                let (real_min, real_max, integer_min, integer_max) =
                    range_values(scalar, &declaration.range)?;
                Ok(CheckedBlockVariable {
                    kind: scalar_kind(scalar),
                    ordinal: index + 1,
                    name: declaration.name.lexeme(),
                    causality,
                    dimensions: literal_dimensions(declaration)?,
                    real_min,
                    real_max,
                    integer_min,
                    integer_max,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(Self {
            package: CheckedBlockRoot { block },
            block_name,
            variables,
            methods: MethodsView::new(block),
            traces: super::source_trace::TraceLegend::unavailable(),
        })
    }
}

/// Build only the file legend that accompanies the checked syntax.
///
/// This walk follows existing provenance spans and performs no name lookup,
/// type/shape inference, call analysis, or statement rewriting.  The raw
/// checked block remains the sole semantic input to the `.alg` template.
fn trace_legend(product: &TracedAlgorithmCodeProduct<'_>) -> super::source_trace::TraceLegend {
    let block = product.package().block();
    let resolver = super::source_trace::SourceTraceResolver::new(product.sources());
    for function in block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
    {
        let _ = resolver.trace(&function.span);
        record_statement_spans(&resolver, &function.statements);
    }
    for method in [&block.startup, &block.recalibrate, &block.do_step] {
        record_statement_spans(&resolver, &method.statements);
    }
    resolver.legend()
}

fn record_statement_spans(
    resolver: &super::source_trace::SourceTraceResolver<'_>,
    statements: &[ast::Spanned<ast::Statement>],
) {
    for statement in statements {
        let _ = resolver.trace(&statement.span);
        match &statement.node {
            ast::Statement::If(conditional) => {
                for branch in &conditional.branches {
                    record_statement_spans(resolver, &branch.body);
                }
                if let Some(body) = &conditional.else_body {
                    record_statement_spans(resolver, body);
                }
            }
            ast::Statement::For(loop_statement) => {
                record_statement_spans(resolver, &loop_statement.body);
            }
            ast::Statement::Assignment { .. }
            | ast::Statement::MultiAssignment { .. }
            | ast::Statement::Call(_)
            | ast::Statement::Limit(_)
            | ast::Statement::Signal(_) => {}
        }
    }
}

/// The block's name as a target renders it. A quoted GALEC name renders as the
/// text between the quotes, exactly as an identifier one does: no target has
/// ever needed to tell the two spellings apart, because every target that
/// prints the name into an artifact re-escapes it for its own language.
fn name_of(name: &ast::Name) -> &str {
    match name {
        ast::Name::Ident(identifier, _) => identifier.as_str(),
        ast::Name::Quoted(value, _) => value.as_str(),
    }
}

const fn scalar_kind(scalar: ast::ScalarType) -> &'static str {
    match scalar {
        ast::ScalarType::Real => "real",
        ast::ScalarType::Integer => "integer",
        ast::ScalarType::Boolean => "boolean",
    }
}

fn literal_dimensions(declaration: &ast::VariableDeclaration) -> Result<Vec<u64>, String> {
    declaration
        .dimensions
        .iter()
        .map(|dimension| match dimension {
            ast::Dimension::Expr(ast::Expression::Integer(size)) if *size > 0 => {
                u64::try_from(*size).map_err(|_| "dimension exceeds semantic range".to_owned())
            }
            _ => Err(format!(
                "block dimension on `{}` is not a positive literal",
                declaration.name.lexeme()
            )),
        })
        .collect()
}

type RangeValues = (Option<f64>, Option<f64>, Option<i64>, Option<i64>);

fn range_values(
    scalar: ast::ScalarType,
    range: &ast::RangeAttributes,
) -> Result<RangeValues, String> {
    match scalar {
        ast::ScalarType::Real => Ok((
            optional_real(range.min.as_ref())?,
            optional_real(range.max.as_ref())?,
            None,
            None,
        )),
        ast::ScalarType::Integer => Ok((
            None,
            None,
            optional_integer(range.min.as_ref())?,
            optional_integer(range.max.as_ref())?,
        )),
        ast::ScalarType::Boolean if range.is_empty() => Ok((None, None, None, None)),
        ast::ScalarType::Boolean => Err("Boolean declaration has a numeric range".to_owned()),
    }
}

fn optional_real(value: Option<&ast::Expression>) -> Result<Option<f64>, String> {
    value
        .map(|value| match value {
            ast::Expression::Real(value) => Ok(*value),
            _ => Err("Real range is not a literal".to_owned()),
        })
        .transpose()
}

fn optional_integer(value: Option<&ast::Expression>) -> Result<Option<i64>, String> {
    value
        .map(|value| match value {
            ast::Expression::Integer(value) => Ok(*value),
            _ => Err("Integer range is not a literal".to_owned()),
        })
        .transpose()
}

fn flatten_real(expression: &ast::Expression, out: &mut Vec<f64>) -> Result<(), String> {
    match expression {
        ast::Expression::Real(value) => out.push(*value),
        ast::Expression::Array(values) => {
            for value in values {
                flatten_real(value, out)?;
            }
        }
        _ => return Err("Real start is not a literal constructor".to_owned()),
    }
    Ok(())
}

fn flatten_integer(expression: &ast::Expression, out: &mut Vec<i64>) -> Result<(), String> {
    match expression {
        ast::Expression::Integer(value) => out.push(*value),
        ast::Expression::Array(values) => {
            for value in values {
                flatten_integer(value, out)?;
            }
        }
        _ => return Err("Integer start is not a literal constructor".to_owned()),
    }
    Ok(())
}

fn flatten_boolean(expression: &ast::Expression, out: &mut Vec<bool>) -> Result<(), String> {
    match expression {
        ast::Expression::Bool(value) => out.push(*value),
        ast::Expression::Array(values) => {
            for value in values {
                flatten_boolean(value, out)?;
            }
        }
        _ => return Err("Boolean start is not a literal constructor".to_owned()),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn algorithm_code_dimensions_are_prepared_one_based_for_the_xsd() {
        let mut declaration =
            ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident("matrix"));
        declaration.dimensions = [2, 3]
            .into_iter()
            .map(|size| ast::Dimension::Expr(ast::Expression::Integer(size)))
            .collect();
        let view = VariableView::new(
            AlgorithmCodeManifestIdentifier::issue("V1".to_owned(), &mut BTreeSet::new())
                .expect("the fixture identifier is valid and unique"),
            &declaration,
            Some(&ast::Expression::Real(0.0)),
            "input",
            None,
        )
        .expect("literal tensor dimensions are a checked manifest projection");
        assert_eq!(
            view.semantic
                .dimensions
                .iter()
                .map(|dimension| (dimension.number, dimension.size))
                .collect::<Vec<_>>(),
            [(1, 2), (2, 3)]
        );
    }

    #[test]
    fn scalar_start_payload_projects_the_checked_literal_form_directly() {
        assert!(matches!(
            StartView::new(
                ast::ScalarType::Integer,
                &ast::Expression::Integer(-3),
                true
            ),
            Ok(StartView::Integer {
                start: StartPayload::Scalar { value: -3 }
            })
        ));
        assert!(
            StartView::new(
                ast::ScalarType::Integer,
                &ast::Expression::Array(vec![ast::Expression::Integer(1)]),
                true,
            )
            .is_err(),
            "a scalar projection never flattens and rechecks a collection"
        );
        assert!(matches!(
            StartView::new(
                ast::ScalarType::Integer,
                &ast::Expression::Array(vec![
                    ast::Expression::Integer(1),
                    ast::Expression::Integer(2),
                ]),
                false,
            ),
            Ok(StartView::Integer {
                start: StartPayload::Tensor { values }
            }) if values == [1, 2]
        ));
    }

    #[test]
    fn standalone_manifest_presentation_issues_one_unique_identifier_family() {
        let (presentation, variables) =
            AlgorithmCodeManifestPresentation::construct("CheckedBlock", 3, 2)
                .expect("the standalone presentation family is valid");
        let identifiers = [
            presentation.algorithm_code_file.0.clone(),
            presentation.clock.0.clone(),
            presentation.startup_method.0.clone(),
            presentation.recalibrate_method.0.clone(),
            presentation.do_step_method.0.clone(),
            presentation.error_signal_status.0.clone(),
            variables[0].0.clone(),
            variables[1].0.clone(),
            variables[2].0.clone(),
        ];
        assert_eq!(
            identifiers.iter().collect::<BTreeSet<_>>().len(),
            identifiers.len()
        );
        assert_eq!(presentation.clock_variable.0, variables[1].0);
    }
}
