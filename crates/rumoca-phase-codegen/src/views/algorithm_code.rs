//! Target-neutral serialized views over checked Algorithm Code.
//!
//! These adapters expose semantic facts only. Templates own every emitted
//! identifier, keyword, filename, schema spelling, and target type.

use rumoca_ir_galec::ast as ast;
use rumoca_ir_galec::package::{AlgorithmCodePackage, CheckedAlgorithmBlock};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct AlgorithmCodeView<'a> {
    package: PackageRoot<'a>,
    block_name: &'a str,
    block_name_quoted: bool,
    variables: Vec<VariableView<'a>>,
    methods: MethodsView,
}

#[derive(Debug, Clone, Copy, Serialize)]
struct PackageRoot<'a> {
    block: &'a ast::Block,
    clock_variable_ordinal: usize,
}

impl<'a> AlgorithmCodeView<'a> {
    pub fn new(package: &'a AlgorithmCodePackage) -> Result<Self, String> {
        let block = package.block();
        let (block_name, block_name_quoted) = name_parts(&block.name);
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
        let variables = declarations
            .zip(package.variable_nominals())
            .enumerate()
            .map(|(index, ((declaration, start, causality), nominal))| {
                VariableView::new(index + 1, declaration, start, causality, *nominal)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            package: PackageRoot {
                block,
                clock_variable_ordinal: package.clock_variable_ordinal(),
            },
            block_name,
            block_name_quoted,
            variables,
            methods: MethodsView::new(block),
        })
    }
}

#[derive(Debug, Clone, Serialize)]
struct VariableView<'a> {
    kind: &'static str,
    ordinal: usize,
    name: &'a str,
    causality: &'static str,
    dimensions: Vec<u64>,
    start: StartView,
    real_min: Option<f64>,
    real_max: Option<f64>,
    real_nominal: Option<f64>,
    integer_min: Option<i64>,
    integer_max: Option<i64>,
}

impl<'a> VariableView<'a> {
    fn new(
        ordinal: usize,
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
        let dimensions = literal_dimensions(declaration)?;
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
            ordinal,
            name: declaration.name.lexeme(),
            causality,
            dimensions,
            start: StartView::new(scalar, start, declaration.dimensions.is_empty())?,
            real_min,
            real_max,
            real_nominal: (scalar == ast::ScalarType::Real).then_some(nominal).flatten(),
            integer_min,
            integer_max,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum StartView {
    Real { scalar: bool, values: Vec<f64> },
    Integer { scalar: bool, values: Vec<i64> },
    Boolean { scalar: bool, values: Vec<bool> },
}

impl StartView {
    fn new(
        scalar_type: ast::ScalarType,
        expression: &ast::Expression,
        scalar: bool,
    ) -> Result<Self, String> {
        match scalar_type {
            ast::ScalarType::Real => {
                let mut values = Vec::new();
                flatten_real(expression, &mut values)?;
                Ok(Self::Real { scalar, values })
            }
            ast::ScalarType::Integer => {
                let mut values = Vec::new();
                flatten_integer(expression, &mut values)?;
                Ok(Self::Integer { scalar, values })
            }
            ast::ScalarType::Boolean => {
                let mut values = Vec::new();
                flatten_boolean(expression, &mut values)?;
                Ok(Self::Boolean { scalar, values })
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
pub struct CheckedAlgorithmBlockView<'a> {
    package: CheckedBlockRoot<'a>,
    block_name: &'a str,
    block_name_quoted: bool,
    variables: Vec<CheckedBlockVariable<'a>>,
    methods: MethodsView,
}

#[derive(Debug, Clone, Copy, Serialize)]
struct CheckedBlockRoot<'a> {
    block: &'a ast::Block,
}

#[derive(Debug, Clone, Serialize)]
struct CheckedBlockVariable<'a> {
    kind: &'static str,
    ordinal: usize,
    name: &'a str,
    causality: &'static str,
    dimensions: Vec<u64>,
}

impl<'a> CheckedAlgorithmBlockView<'a> {
    pub fn new(checked: &'a CheckedAlgorithmBlock) -> Result<Self, String> {
        let block = checked.block();
        let (block_name, block_name_quoted) = name_parts(&block.name);
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
                Ok(CheckedBlockVariable {
                    kind: scalar_kind(scalar),
                    ordinal: index + 1,
                    name: declaration.name.lexeme(),
                    causality,
                    dimensions: literal_dimensions(declaration)?,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(Self {
            package: CheckedBlockRoot { block },
            block_name,
            block_name_quoted,
            variables,
            methods: MethodsView::new(block),
        })
    }
}

fn name_parts(name: &ast::Name) -> (&str, bool) {
    match name {
        ast::Name::Ident(identifier, _) => (identifier.as_str(), false),
        ast::Name::Quoted(value, _) => (value.as_str(), true),
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
