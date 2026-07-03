//! Typed model of the Algorithm Code manifest
//! (`AlgorithmCode/efmiAlgorithmCodeManifest.xsd`, 0.14.0).
//!
//! Illegal states are unrepresentable where cheap:
//!
//! - the fixed attributes (`xsdVersion="0.14.0"`, `kind="AlgorithmCode"`,
//!   `efmiVersion="1.0.0"`) are serializer constants, not fields (GAL-022);
//! - the exactly-three block methods are three named fields
//!   ([`BlockMethods`]), not a `Vec`;
//! - `Dimension` `number` attributes are derived from position (always
//!   consecutive from 1);
//! - the `needsChecksum`/`checksum` coupling ("checksum iff needed") is one
//!   enum, [`FileChecksum`](crate::manifest_common::FileChecksum).
//!
//! Everything else is validated eagerly by [`AlgorithmCodeManifest::new`]
//! (SPEC_0008: fail early, no silent defaults).

use std::collections::{BTreeMap, BTreeSet};

use crate::diagnostic::EfmiError;
use crate::ids::{IdRegistry, Identifier, NormalizedText};
use crate::manifest_common::{Annotation, File, FileRole, ManifestAttributes, Unit};

/// `Clock` element: references the block variable defining the fixed sample
/// period (§3.1.2). The referenced variable must be a Real `constant`; when
/// it carries a `unitRefId` whose unit has a `BaseUnit` decomposition, that
/// decomposition must be exactly seconds (SPEC_0034 D6). A referenced unit
/// without a `BaseUnit` carries no machine-checkable SI information and is
/// accepted (name heuristics are banned, GAL-016).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clock {
    /// Id of the sample period (`id`, required).
    pub id: Identifier,
    /// Reference to the sample-period constant in `Variables`
    /// (`variableRefId`, required).
    pub variable_ref_id: Identifier,
}

/// Error signal values exposable by a block method (§3.2.5).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorSignal {
    /// `INVALID_ARGUMENT`
    InvalidArgument,
    /// `OVERFLOW`
    Overflow,
    /// `NAN`
    Nan,
    /// `SOLVE_LINEAR_EQUATIONS_FAILED`
    SolveLinearEquationsFailed,
    /// `NO_SOLUTION_FOUND`
    NoSolutionFound,
    /// `UNSPECIFIED_ERROR`
    UnspecifiedError,
}

impl ErrorSignal {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::InvalidArgument => "INVALID_ARGUMENT",
            Self::Overflow => "OVERFLOW",
            Self::Nan => "NAN",
            Self::SolveLinearEquationsFailed => "SOLVE_LINEAR_EQUATIONS_FAILED",
            Self::NoSolutionFound => "NO_SOLUTION_FOUND",
            Self::UnspecifiedError => "UNSPECIFIED_ERROR",
        }
    }
}

/// One `BlockMethod` element; its `kind` is implied by which
/// [`BlockMethods`] field it occupies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockMethod {
    /// Method id (`id`, required).
    pub id: Identifier,
    /// Exposed error signals (`Signals`, optional child); empty means the
    /// `Signals` element is omitted.
    pub signals: Vec<ErrorSignal>,
}

/// The mandatory three block methods (§3.1.3): exactly `Startup`,
/// `Recalibrate`, and `DoStep`, each exactly once — enforced by shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockMethods {
    /// The `Startup` method.
    pub startup: BlockMethod,
    /// The `Recalibrate` method.
    pub recalibrate: BlockMethod,
    /// The `DoStep` method.
    pub do_step: BlockMethod,
}

/// `ErrorSignalStatus` element: unique anchor used by derived
/// representations to mark error-signal status variables (§3.1.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorSignalStatus {
    /// Anchor id (`id`, required).
    pub id: Identifier,
}

/// `blockCausality` enumeration (`AlgorithmCode/efmiVariable.xsd`).
///
/// Note: the XSD enumeration value is `dependentParameter`; the Beta-1 prose
/// erroneously calls it `calculatedParameter`. The XSD is ground truth.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BlockCausality {
    /// Provided from the environment at the start of the sampling period.
    Input,
    /// Provided to the environment at the end of the sampling period.
    Output,
    /// Independent parameter, calibratable between steps.
    TunableParameter,
    /// Computed from other parameters during initialization/recalibration.
    DependentParameter,
    /// Value fixed by `start`, never changes.
    Constant,
    /// Protected state passed between method calls.
    State,
}

impl BlockCausality {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Input => "input",
            Self::Output => "output",
            Self::TunableParameter => "tunableParameter",
            Self::DependentParameter => "dependentParameter",
            Self::Constant => "constant",
            Self::State => "state",
        }
    }
}

/// `start` attribute value: a scalar (broadcast over all elements of an
/// array variable) or a full row-major element list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StartValue<T> {
    /// One value; for array variables it applies to every element.
    Scalar(T),
    /// Row-major full value (elements of the last index in sequence). Its
    /// length must equal the product of the dimension sizes.
    Array(Vec<T>),
}

impl<T> StartValue<T> {
    /// Iterate over all encoded values.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        match self {
            Self::Scalar(value) => std::slice::from_ref(value).iter(),
            Self::Array(values) => values.iter(),
        }
    }
}

/// Attributes and children shared by all variable kinds (`efmiVariableBase`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableCommon {
    /// Variable id (`id`, required), unique across the whole manifest.
    pub id: Identifier,
    /// Full unique name, e.g. `a.b.c` or `previous(I.x)` (`name`, required).
    pub name: NormalizedText,
    /// Free-text description (`description`, optional; `xs:string` in the
    /// XSD, held to normalized-attribute discipline — see
    /// [`crate::manifest_common`] module docs).
    pub description: Option<NormalizedText>,
    /// Block causality (`blockCausality`, required).
    pub block_causality: BlockCausality,
    /// Dimension sizes in order; empty means scalar. Each size must be a
    /// literal integer >= 1 (`Dimensions/Dimension/@size`); the `number`
    /// attribute is derived from position.
    pub dimensions: Vec<u64>,
    /// Vendor annotations (`Annotations`, optional child).
    pub annotations: Vec<Annotation>,
}

/// `RealVariable` (`efmiRealVariable`).
#[derive(Debug, Clone, PartialEq)]
pub struct RealVariable {
    /// Shared attributes/children.
    pub common: VariableCommon,
    /// Initial value (`start`, required).
    pub start: StartValue<f64>,
    /// Unit reference into the `Units` list (`unitRefId`, optional).
    pub unit_ref_id: Option<Identifier>,
    /// Ignore the base-unit offset in conversions (`relativeQuantity`,
    /// XSD default `false`; emitted only when `true`).
    pub relative_quantity: bool,
    /// Lower bound (`min`, optional; scalar even for arrays).
    pub min: Option<f64>,
    /// Upper bound (`max`, optional; `max >= min` required).
    pub max: Option<f64>,
    /// Nominal value (`nominal`, optional; `> 0.0` required).
    pub nominal: Option<f64>,
}

/// `IntegerVariable` (`efmiIntegerVariable`; bounds are `xs:int`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerVariable {
    /// Shared attributes/children.
    pub common: VariableCommon,
    /// Initial value (`start`, required).
    pub start: StartValue<i32>,
    /// Lower bound (`min`, optional).
    pub min: Option<i32>,
    /// Upper bound (`max`, optional; `max >= min` required).
    pub max: Option<i32>,
}

/// `BooleanVariable` (`efmiBooleanVariable`; no extra attributes).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanVariable {
    /// Shared attributes/children.
    pub common: VariableCommon,
    /// Initial value (`start`, required).
    pub start: StartValue<bool>,
}

/// One entry of the ordered `Variables` list (first element has index 1).
/// There is no String variable kind in eFMI.
#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    /// A `RealVariable` element.
    Real(RealVariable),
    /// An `IntegerVariable` element.
    Integer(IntegerVariable),
    /// A `BooleanVariable` element.
    Boolean(BooleanVariable),
}

impl Variable {
    /// The shared attribute/children set of any variable kind.
    pub fn common(&self) -> &VariableCommon {
        match self {
            Self::Real(v) => &v.common,
            Self::Integer(v) => &v.common,
            Self::Boolean(v) => &v.common,
        }
    }
}

/// Unvalidated field set for [`AlgorithmCodeManifest`]. Assemble freely, then
/// validate via [`AlgorithmCodeManifest::new`]. Fields are in XSD child order.
#[derive(Debug, Clone, PartialEq)]
pub struct AlgorithmCodeManifestParts {
    /// Shared top-level attributes (`efmiManifestAttributesBase`).
    pub attributes: ManifestAttributes,
    /// Reference to the single GALEC `.alg` code file implementing the block
    /// (`fileRefId`, required root attribute; schema 0.14.0 moved it here
    /// from `BlockMethods`).
    pub file_ref_id: Identifier,
    /// `Files` list (exactly one, at least the `.alg` file).
    pub files: Vec<File>,
    /// `Clock` element (exactly one).
    pub clock: Clock,
    /// `BlockMethods` element (exactly one, exactly three methods).
    pub block_methods: BlockMethods,
    /// `ErrorSignalStatus` element (exactly one).
    pub error_signal_status: ErrorSignalStatus,
    /// `Units` list (element always emitted, may be empty).
    pub units: Vec<Unit>,
    /// Ordered `Variables` list; must be non-empty (the Clock's sample-period
    /// constant lives here).
    pub variables: Vec<Variable>,
    /// Manifest-level vendor annotations (`Annotations`, 0..1).
    pub annotations: Vec<Annotation>,
}

/// Validated Algorithm Code manifest model. Construction enforces
/// (fail-early, GAL-018/020/021):
///
/// - all id values unique across the whole manifest (principle 6);
/// - at most one `File` with `role="FMU"` (eFMI ch. 2.3.6: the optional FMU
///   of a representation is exactly one file);
/// - non-empty `Variables` with unique names, dimension sizes >= 1, start
///   shapes matching dimensions (row-major, scalar broadcast), finite Reals,
///   and min/max/nominal/start range consistency;
/// - unique unit names, finite `BaseUnit` factor/offset, and per-list unique
///   annotation types;
/// - no duplicate signals per block method;
/// - `fileRefId`, `Clock/@variableRefId`, and every `unitRefId` resolve, the
///   `fileRefId` target is the `role="Code"` GALEC program file (§3.1.1 —
///   which also guarantees at least one code file), the clock variable is a
///   Real `constant`, and its unit's `BaseUnit` (when present) is exactly
///   seconds (§3.1.2, D6).
#[derive(Debug, Clone, PartialEq)]
pub struct AlgorithmCodeManifest(AlgorithmCodeManifestParts);

impl AlgorithmCodeManifest {
    /// Validate the parts and construct the model.
    pub fn new(parts: AlgorithmCodeManifestParts) -> Result<Self, EfmiError> {
        validate_unique_ids(&parts)?;
        validate_files(&parts.files)?;
        validate_variables(&parts.variables)?;
        validate_units(&parts.units)?;
        validate_annotations("Manifest", &parts.annotations)?;
        validate_signals(&parts.block_methods)?;
        validate_references(&parts)?;
        Ok(Self(parts))
    }

    /// The validated field set.
    pub fn parts(&self) -> &AlgorithmCodeManifestParts {
        &self.0
    }
}

fn validate_unique_ids(parts: &AlgorithmCodeManifestParts) -> Result<(), EfmiError> {
    let mut registry = IdRegistry::new();
    registry.register(&parts.attributes.id.to_string())?;
    for file in &parts.files {
        registry.register(file.id.as_str())?;
    }
    registry.register(parts.clock.id.as_str())?;
    let methods = &parts.block_methods;
    for method in [&methods.startup, &methods.recalibrate, &methods.do_step] {
        registry.register(method.id.as_str())?;
    }
    registry.register(parts.error_signal_status.id.as_str())?;
    for unit in &parts.units {
        registry.register(unit.id.as_str())?;
    }
    for variable in &parts.variables {
        registry.register(variable.common().id.as_str())?;
    }
    Ok(())
}

/// eFMI ch. 2.3.6: the optional FMU of a representation is "exactly one
/// file"; the XSD cannot express this cardinality, so the typed model is the
/// enforcement point.
fn validate_files(files: &[File]) -> Result<(), EfmiError> {
    let fmu_count = files.iter().filter(|f| f.role == FileRole::Fmu).count();
    if fmu_count > 1 {
        return Err(EfmiError::MultipleFmuFiles { count: fmu_count });
    }
    Ok(())
}

fn validate_variables(variables: &[Variable]) -> Result<(), EfmiError> {
    if variables.is_empty() {
        return Err(EfmiError::EmptyVariables);
    }
    let mut names: BTreeSet<&str> = BTreeSet::new();
    for variable in variables {
        let common = variable.common();
        if !names.insert(common.name.as_str()) {
            return Err(EfmiError::DuplicateVariableName {
                name: common.name.as_str().to_owned(),
            });
        }
        validate_dimensions(common)?;
        validate_annotations(common.name.as_str(), &common.annotations)?;
        match variable {
            Variable::Real(v) => {
                validate_start_shape(common, v.start.len_if_array())?;
                validate_real_ranges(v)?;
            }
            Variable::Integer(v) => {
                validate_start_shape(common, v.start.len_if_array())?;
                validate_integer_ranges(v)?;
            }
            Variable::Boolean(v) => {
                validate_start_shape(common, v.start.len_if_array())?;
            }
        }
    }
    Ok(())
}

impl<T> StartValue<T> {
    fn len_if_array(&self) -> Option<usize> {
        match self {
            Self::Scalar(_) => None,
            Self::Array(values) => Some(values.len()),
        }
    }
}

fn validate_dimensions(common: &VariableCommon) -> Result<(), EfmiError> {
    for (index, size) in common.dimensions.iter().enumerate() {
        if *size < 1 {
            return Err(EfmiError::InvalidDimension {
                variable: common.name.as_str().to_owned(),
                dimension_number: index + 1,
                size: *size,
            });
        }
    }
    Ok(())
}

fn element_count(common: &VariableCommon) -> Result<u64, EfmiError> {
    let mut count: u64 = 1;
    for size in &common.dimensions {
        count = count
            .checked_mul(*size)
            .ok_or_else(|| EfmiError::DimensionProductOverflow {
                variable: common.name.as_str().to_owned(),
            })?;
    }
    Ok(count)
}

fn validate_start_shape(
    common: &VariableCommon,
    array_len: Option<usize>,
) -> Result<(), EfmiError> {
    let Some(actual) = array_len else {
        return Ok(()); // scalar start: valid for scalars and as broadcast
    };
    if common.dimensions.is_empty() {
        return Err(EfmiError::ArrayStartOnScalar {
            variable: common.name.as_str().to_owned(),
        });
    }
    let expected = element_count(common)?;
    if actual as u64 != expected {
        return Err(EfmiError::StartLengthMismatch {
            variable: common.name.as_str().to_owned(),
            expected,
            actual,
        });
    }
    Ok(())
}

fn validate_real_ranges(variable: &RealVariable) -> Result<(), EfmiError> {
    let name = variable.common.name.as_str();
    let non_finite = |attribute: &str| EfmiError::NonFiniteReal {
        variable: name.to_owned(),
        attribute: attribute.to_owned(),
    };
    let range = |reason: String| EfmiError::InvalidVariableRange {
        variable: name.to_owned(),
        reason,
    };
    for (attribute, value) in [
        ("min", variable.min),
        ("max", variable.max),
        ("nominal", variable.nominal),
    ] {
        if let Some(value) = value
            && !value.is_finite()
        {
            return Err(non_finite(attribute));
        }
    }
    if variable.start.iter().any(|value| !value.is_finite()) {
        return Err(non_finite("start"));
    }
    if let (Some(min), Some(max)) = (variable.min, variable.max)
        && min > max
    {
        return Err(range(format!("min {min} > max {max}")));
    }
    if let Some(nominal) = variable.nominal
        && nominal <= 0.0
    {
        return Err(range(format!("nominal {nominal} must be > 0.0")));
    }
    for value in variable.start.iter() {
        if variable.min.is_some_and(|min| *value < min)
            || variable.max.is_some_and(|max| *value > max)
        {
            return Err(range(format!("start value {value} outside [min, max]")));
        }
    }
    Ok(())
}

fn validate_integer_ranges(variable: &IntegerVariable) -> Result<(), EfmiError> {
    let name = variable.common.name.as_str();
    let range = |reason: String| EfmiError::InvalidVariableRange {
        variable: name.to_owned(),
        reason,
    };
    if let (Some(min), Some(max)) = (variable.min, variable.max)
        && min > max
    {
        return Err(range(format!("min {min} > max {max}")));
    }
    for value in variable.start.iter() {
        if variable.min.is_some_and(|min| *value < min)
            || variable.max.is_some_and(|max| *value > max)
        {
            return Err(range(format!("start value {value} outside [min, max]")));
        }
    }
    Ok(())
}

fn validate_units(units: &[Unit]) -> Result<(), EfmiError> {
    let mut names: BTreeSet<&str> = BTreeSet::new();
    for unit in units {
        if !names.insert(unit.name.as_str()) {
            return Err(EfmiError::DuplicateUnitName {
                name: unit.name.as_str().to_owned(),
            });
        }
        validate_base_unit_finite(unit)?;
    }
    Ok(())
}

/// Non-finite `BaseUnit` factor/offset would serialize to bytes xmllint
/// rejects (`xs:double` has no lexical form for Rust's `inf.0`/`NaN.0`), so
/// they are rejected here, like every Real variable attribute.
fn validate_base_unit_finite(unit: &Unit) -> Result<(), EfmiError> {
    let Some(base_unit) = &unit.base_unit else {
        return Ok(());
    };
    for (attribute, value) in [("factor", base_unit.factor), ("offset", base_unit.offset)] {
        if !value.is_finite() {
            return Err(EfmiError::NonFiniteUnitAttribute {
                unit: unit.name.as_str().to_owned(),
                attribute: attribute.to_owned(),
            });
        }
    }
    Ok(())
}

fn validate_annotations(owner: &str, annotations: &[Annotation]) -> Result<(), EfmiError> {
    let mut types: BTreeSet<&str> = BTreeSet::new();
    for annotation in annotations {
        if !types.insert(annotation.annotation_type.as_str()) {
            return Err(EfmiError::DuplicateAnnotationType {
                annotation_type: annotation.annotation_type.as_str().to_owned(),
                owner: owner.to_owned(),
            });
        }
    }
    Ok(())
}

fn validate_signals(methods: &BlockMethods) -> Result<(), EfmiError> {
    for (name, method) in [
        ("Startup", &methods.startup),
        ("Recalibrate", &methods.recalibrate),
        ("DoStep", &methods.do_step),
    ] {
        let mut seen: BTreeSet<&str> = BTreeSet::new();
        for signal in &method.signals {
            if !seen.insert(signal.as_str()) {
                return Err(EfmiError::DuplicateSignal {
                    method: name.to_owned(),
                    signal: signal.as_str().to_owned(),
                });
            }
        }
    }
    Ok(())
}

fn validate_references(parts: &AlgorithmCodeManifestParts) -> Result<(), EfmiError> {
    let files: BTreeMap<&str, &File> = parts.files.iter().map(|f| (f.id.as_str(), f)).collect();
    let units: BTreeMap<&str, &Unit> = parts.units.iter().map(|u| (u.id.as_str(), u)).collect();
    let variables: BTreeMap<&str, &Variable> = parts
        .variables
        .iter()
        .map(|v| (v.common().id.as_str(), v))
        .collect();
    let unresolved = |attribute: &str, id: &Identifier| EfmiError::UnresolvedReference {
        attribute: attribute.to_owned(),
        id: id.as_str().to_owned(),
    };
    let Some(referenced) = files.get(parts.file_ref_id.as_str()) else {
        return Err(unresolved("Manifest/@fileRefId", &parts.file_ref_id));
    };
    // §3.1.1: fileRefId is the reference to the GALEC program implementing
    // the block — the representation's code file. Resolving to any other
    // role (the manifest itself, reference data, ...) would be a
    // schema-valid but nonconforming Algorithm Code container.
    if referenced.role != FileRole::Code {
        return Err(EfmiError::FileRefRoleNotCode {
            id: parts.file_ref_id.as_str().to_owned(),
            role: referenced.role.as_str().to_owned(),
        });
    }
    for variable in &parts.variables {
        if let Variable::Real(real) = variable
            && let Some(unit_ref) = &real.unit_ref_id
            && !units.contains_key(unit_ref.as_str())
        {
            return Err(unresolved("RealVariable/@unitRefId", unit_ref));
        }
    }
    let clock_ref = &parts.clock.variable_ref_id;
    let clock_variable = variables
        .get(clock_ref.as_str())
        .ok_or_else(|| unresolved("Clock/@variableRefId", clock_ref))?;
    validate_clock_variable(clock_variable, &units)
}

fn validate_clock_variable(
    variable: &Variable,
    units: &BTreeMap<&str, &Unit>,
) -> Result<(), EfmiError> {
    let invalid = |reason: String| EfmiError::InvalidClockVariable {
        id: variable.common().id.as_str().to_owned(),
        reason,
    };
    let Variable::Real(real) = variable else {
        return Err(invalid("sample period must be a Real variable".to_owned()));
    };
    if real.common.block_causality != BlockCausality::Constant {
        return Err(invalid(
            "sample period must have blockCausality `constant` \
             (only fixed sample periods are permitted)"
                .to_owned(),
        ));
    }
    // §3.1.2 / SPEC_0034 D6: if the sample period has a unit, it must be
    // seconds. A `BaseUnit` decomposition makes that machine-checkable; a
    // unit without one carries no SI information and is accepted (name
    // heuristics are banned, GAL-016). Unit resolution is checked by the
    // caller for all Real variables, so a missing entry here means only that
    // the unresolved-reference error will be reported instead.
    if let Some(unit_ref) = &real.unit_ref_id
        && let Some(unit) = units.get(unit_ref.as_str())
        && let Some(base_unit) = &unit.base_unit
        && !base_unit.is_seconds()
    {
        return Err(invalid(format!(
            "sample-period unit `{}` is not seconds (its BaseUnit must be s=1, \
             all other exponents 0, factor 1.0, offset 0.0)",
            unit.name.as_str()
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checksum::Sha1Hex;
    use crate::ids::{FilePath, ManifestId, NameWithoutSlashes, UtcTimestamp};
    use crate::manifest_common::{BaseUnit, FileChecksum};

    fn attributes() -> ManifestAttributes {
        ManifestAttributes {
            id: ManifestId::parse("{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}").unwrap(),
            name: NormalizedText::new("TestBlock").unwrap(),
            description: None,
            version: None,
            generation_date_and_time: UtcTimestamp::parse("2026-07-02T12:00:00Z").unwrap(),
            generation_tool: None,
            copyright: None,
            license: None,
        }
    }

    fn ident(value: &str) -> Identifier {
        Identifier::new(value).unwrap()
    }

    fn real_variable(id: &str, name: &str, causality: BlockCausality) -> Variable {
        Variable::Real(RealVariable {
            common: VariableCommon {
                id: ident(id),
                name: NormalizedText::new(name).unwrap(),
                description: None,
                block_causality: causality,
                dimensions: vec![],
                annotations: vec![],
            },
            start: StartValue::Scalar(0.0),
            unit_ref_id: None,
            relative_quantity: false,
            min: None,
            max: None,
            nominal: None,
        })
    }

    fn base_parts() -> AlgorithmCodeManifestParts {
        AlgorithmCodeManifestParts {
            attributes: attributes(),
            file_ref_id: ident("F_ALG"),
            files: vec![File {
                id: ident("F_ALG"),
                name: NameWithoutSlashes::new("TestBlock.alg").unwrap(),
                path: FilePath::root(),
                checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(b"abc")),
                role: FileRole::Code,
                description: None,
            }],
            clock: Clock {
                id: ident("CLK"),
                variable_ref_id: ident("V_T"),
            },
            block_methods: BlockMethods {
                startup: BlockMethod {
                    id: ident("BM_STARTUP"),
                    signals: vec![],
                },
                recalibrate: BlockMethod {
                    id: ident("BM_RECALIBRATE"),
                    signals: vec![],
                },
                do_step: BlockMethod {
                    id: ident("BM_DOSTEP"),
                    signals: vec![],
                },
            },
            error_signal_status: ErrorSignalStatus { id: ident("ESS") },
            units: vec![],
            variables: vec![real_variable("V_T", "T", BlockCausality::Constant)],
            annotations: vec![],
        }
    }

    #[test]
    fn minimal_manifest_validates() {
        assert!(AlgorithmCodeManifest::new(base_parts()).is_ok());
    }

    #[test]
    fn duplicate_id_across_sections_rejected() {
        let mut parts = base_parts();
        // Variable id colliding with a file id: uniqueness is per manifest,
        // not per element type.
        parts
            .variables
            .push(real_variable("F_ALG", "x", BlockCausality::State));
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err, EfmiError::DuplicateId { id: "F_ALG".into() });
    }

    #[test]
    fn empty_variables_rejected() {
        let mut parts = base_parts();
        parts.variables.clear();
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err, EfmiError::EmptyVariables);
    }

    #[test]
    fn zero_dimension_rejected() {
        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.common.dimensions = vec![2, 0];
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::InvalidDimension {
                variable: "T".into(),
                dimension_number: 2,
                size: 0
            }
        );
    }

    #[test]
    fn start_shape_mismatches_rejected() {
        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.common.dimensions = vec![2, 2];
        v.start = StartValue::Array(vec![1.0, 2.0, 3.0]);
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::StartLengthMismatch {
                variable: "T".into(),
                expected: 4,
                actual: 3
            }
        );

        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.start = StartValue::Array(vec![1.0]);
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::ArrayStartOnScalar {
                variable: "T".into()
            }
        );
    }

    #[test]
    fn clock_reference_must_be_real_constant() {
        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.common.block_causality = BlockCausality::TunableParameter;
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM020");

        let mut parts = base_parts();
        parts.clock.variable_ref_id = ident("MISSING");
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "Clock/@variableRefId".into(),
                id: "MISSING".into()
            }
        );
    }

    /// §3.1.1: `fileRefId` must reference the GALEC program — the
    /// representation's `role="Code"` file. Resolving to a file of any other
    /// role (here the manifest itself) is a nonconforming container.
    #[test]
    fn file_ref_must_resolve_to_code_role() {
        let mut parts = base_parts();
        parts.files.push(File {
            id: ident("F_MANIFEST"),
            name: NameWithoutSlashes::new("manifest.xml").unwrap(),
            path: FilePath::root(),
            checksum: FileChecksum::NotNeeded,
            role: FileRole::Manifest,
            description: None,
        });
        parts.file_ref_id = ident("F_MANIFEST");
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::FileRefRoleNotCode {
                id: "F_MANIFEST".into(),
                role: "Manifest".into(),
            }
        );
    }

    #[test]
    fn unresolved_file_and_unit_references_rejected() {
        let mut parts = base_parts();
        parts.file_ref_id = ident("NOPE");
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM019");

        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.unit_ref_id = Some(ident("U_MISSING"));
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM019");
    }

    #[test]
    fn range_violations_rejected() {
        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.min = Some(1.0);
        v.max = Some(-1.0);
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM021");

        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.start = StartValue::Scalar(f64::NAN);
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM022");

        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.nominal = Some(0.0);
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err.code(), "EFM021");
    }

    fn seconds_unit(id: &str, name: &str) -> Unit {
        Unit {
            id: ident(id),
            name: NormalizedText::new(name).unwrap(),
            base_unit: Some(BaseUnit {
                s: 1,
                ..BaseUnit::default()
            }),
        }
    }

    fn clock_with_unit(unit: Unit) -> AlgorithmCodeManifestParts {
        let mut parts = base_parts();
        let Variable::Real(ref mut v) = parts.variables[0] else {
            unreachable!()
        };
        v.unit_ref_id = Some(unit.id.clone());
        parts.units = vec![unit];
        parts
    }

    #[test]
    fn clock_unit_must_be_seconds_when_decomposed() {
        // Milliseconds: correct SI exponents but a non-identity factor.
        let mut unit = seconds_unit("U_MS", "ms");
        unit.base_unit.as_mut().unwrap().factor = 0.001;
        let err = AlgorithmCodeManifest::new(clock_with_unit(unit)).unwrap_err();
        assert_eq!(err.code(), "EFM020");

        // Kelvin: wrong SI exponents entirely.
        let unit = Unit {
            id: ident("U_K"),
            name: NormalizedText::new("K").unwrap(),
            base_unit: Some(BaseUnit {
                kelvin: 1,
                ..BaseUnit::default()
            }),
        };
        let err = AlgorithmCodeManifest::new(clock_with_unit(unit)).unwrap_err();
        assert_eq!(err.code(), "EFM020");

        // Exact seconds decomposition is accepted.
        assert!(AlgorithmCodeManifest::new(clock_with_unit(seconds_unit("U_S", "s"))).is_ok());

        // A unit without BaseUnit carries no SI information: accepted.
        let unit = Unit {
            id: ident("U_OPAQUE"),
            name: NormalizedText::new("s").unwrap(),
            base_unit: None,
        };
        assert!(AlgorithmCodeManifest::new(clock_with_unit(unit)).is_ok());
    }

    #[test]
    fn non_finite_unit_factor_and_offset_rejected() {
        for (factor, offset) in [(f64::INFINITY, 0.0), (1.0, f64::NAN)] {
            let mut parts = base_parts();
            parts.units = vec![Unit {
                id: ident("U_BAD"),
                name: NormalizedText::new("bad").unwrap(),
                base_unit: Some(BaseUnit {
                    factor,
                    offset,
                    ..BaseUnit::default()
                }),
            }];
            let err = AlgorithmCodeManifest::new(parts).unwrap_err();
            assert_eq!(err.code(), "EFM033");
        }
    }

    #[test]
    fn multiple_fmu_files_rejected() {
        let mut parts = base_parts();
        for (id, name) in [("F_FMU1", "a.fmu"), ("F_FMU2", "b.fmu")] {
            parts.files.push(File {
                id: ident(id),
                name: NameWithoutSlashes::new(name).unwrap(),
                path: FilePath::root(),
                checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(name.as_bytes())),
                role: FileRole::Fmu,
                description: None,
            });
        }
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(err, EfmiError::MultipleFmuFiles { count: 2 });
    }

    #[test]
    fn duplicate_signals_rejected() {
        let mut parts = base_parts();
        parts.block_methods.do_step.signals = vec![ErrorSignal::Nan, ErrorSignal::Nan];
        let err = AlgorithmCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::DuplicateSignal {
                method: "DoStep".into(),
                signal: "NAN".into()
            }
        );
    }
}
