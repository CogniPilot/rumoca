//! Checked external function bodies (MLS §12.9).
//!
//! An external function has no Modelica algorithm section: its outputs are
//! produced by a foreign body identified by a language, a symbol, an ordered
//! ABI argument list, and its link facts. This module owns the one construction
//! operation that turns those facts into an aggregate-owned function body, so
//! `Dae` never stores an external interface it did not check.
use super::*;

/// MLS §12.3 declared purity of a function.
///
/// Purity is a typed body fact rather than a name convention: a pure external
/// function is an ordinary callable, while an impure one may only be executed
/// as one ordered effectful action inside its MLS-legal contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FunctionPurity {
    Pure,
    Impure,
}

impl FunctionPurity {
    pub const fn is_pure(self) -> bool {
        matches!(self, Self::Pure)
    }
}

/// MLS §12.9 external language of an external function body.
///
/// The language is the exact set MLS §12.9 defines. An unrecognized language
/// string never becomes a plausible default; producers must fail before
/// construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExternalLanguage {
    C,
    Fortran77,
    Builtin,
}

impl ExternalLanguage {
    /// The exact MLS §12.9 language specification text.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::C => "C",
            Self::Fortran77 => "FORTRAN 77",
            Self::Builtin => "builtin",
        }
    }

    /// Classify a declared language specification exactly.
    pub fn from_declared(text: &str) -> Option<Self> {
        match text {
            "C" => Some(Self::C),
            "FORTRAN 77" => Some(Self::Fortran77),
            "builtin" => Some(Self::Builtin),
            _ => None,
        }
    }
}

/// MLS §12.9.4 external link facts.
///
/// These are the exact declared annotation values. They carry no identity and
/// no cross-arena reference, so they are stored and replayed verbatim.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ExternalLinkage {
    libraries: Box<[String]>,
    include: Option<String>,
    include_directory: Option<String>,
    library_directory: Option<String>,
}

impl ExternalLinkage {
    pub fn new(
        libraries: impl IntoIterator<Item = String>,
        include: Option<String>,
        include_directory: Option<String>,
        library_directory: Option<String>,
    ) -> Self {
        Self {
            libraries: libraries.into_iter().collect(),
            include,
            include_directory,
            library_directory,
        }
    }

    pub fn libraries(&self) -> &[String] {
        &self.libraries
    }

    pub fn include(&self) -> Option<&str> {
        self.include.as_deref()
    }

    pub fn include_directory(&self) -> Option<&str> {
        self.include_directory.as_deref()
    }

    pub fn library_directory(&self) -> Option<&str> {
        self.library_directory.as_deref()
    }

    /// Whether the declaration carried no link facts at all.
    pub fn is_empty(&self) -> bool {
        self.libraries.is_empty()
            && self.include.is_none()
            && self.include_directory.is_none()
            && self.library_directory.is_none()
    }

    fn validate(&self, at: DaeProvenance) -> Result<(), DaeConstructionError> {
        for fact in self
            .libraries
            .iter()
            .map(String::as_str)
            .chain(self.include.as_deref())
            .chain(self.include_directory.as_deref())
            .chain(self.library_directory.as_deref())
        {
            if fact.trim().is_empty() {
                return Err(DaeConstructionError::InvalidExternalLinkage { span: at.span() });
            }
        }
        Ok(())
    }
}

/// One ordered ABI argument position of an external function body.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternalArgument<'dae> {
    /// A checked value expression the external body reads.
    Input(ExprId<'dae>),
    /// A declared output the external body writes through this position.
    Output(FunctionValueId<'dae>),
}

/// The complete external body supplied to one checked construction operation.
pub struct ExternalFunctionBody<'dae> {
    purity: FunctionPurity,
    language: ExternalLanguage,
    symbol: VarName,
    arguments: Vec<ExternalArgument<'dae>>,
    /// Output bound by the MLS §12.9 `output = symbol(...)` return form.
    result: Option<FunctionValueId<'dae>>,
    linkage: ExternalLinkage,
}

impl<'dae> ExternalFunctionBody<'dae> {
    pub fn new(
        purity: FunctionPurity,
        language: ExternalLanguage,
        symbol: VarName,
        arguments: impl IntoIterator<Item = ExternalArgument<'dae>>,
        result: Option<FunctionValueId<'dae>>,
        linkage: ExternalLinkage,
    ) -> Self {
        Self {
            purity,
            language,
            symbol,
            arguments: arguments.into_iter().collect(),
            result,
            linkage,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ExternalBodyEntry {
    pub(crate) purity: FunctionPurity,
    pub(crate) language: ExternalLanguage,
    pub(crate) symbol: VarName,
    pub(crate) arguments: Vec<ExternalArgumentEntry>,
    pub(crate) result: Option<u32>,
    pub(crate) linkage: ExternalLinkage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum ExternalArgumentEntry {
    Input(u32),
    Output(u32),
}

/// Build the checked external body entry for one reserved function.
///
/// Every declared output is proven to be produced exactly once, every input
/// expression is proven to be closed over this function's own parameters, and
/// every call reached from an input obeys the same dependency order acyclic
/// Modelica bodies obey.
pub(super) fn build_external_body<'dae>(
    storage: &mut Storage,
    function: FunctionId<'dae>,
    construction: FunctionConstruction,
    external: ExternalFunctionBody<'dae>,
    provenance: DaeProvenance,
) -> Result<ExternalBodyEntry, DaeConstructionError> {
    let ExternalFunctionBody {
        purity,
        language,
        symbol,
        arguments,
        result,
        linkage,
    } = external;
    expect_declared_header(storage, function, provenance)?;
    expect_external_symbol(&symbol, provenance)?;
    linkage.validate(provenance)?;

    let output_count = storage.functions[function.index() as usize]
        .output_values
        .len();
    let mut produced = vec![None; output_count];
    let mut entries = Vec::with_capacity(arguments.len());
    for (position, argument) in arguments.iter().enumerate() {
        match *argument {
            ExternalArgument::Input(expression) => {
                expect_external_input(storage, function, expression, provenance)?;
                entries.push(ExternalArgumentEntry::Input(expression.index()));
            }
            ExternalArgument::Output(value) => {
                let ordinal = expect_external_output(storage, function, value, provenance)?;
                record_produced_output(&mut produced, ordinal, position, provenance)?;
                entries.push(ExternalArgumentEntry::Output(value.ordinal()));
            }
        }
    }
    let result = match result {
        Some(value) => {
            let ordinal = expect_external_output(storage, function, value, provenance)?;
            record_produced_output(&mut produced, ordinal, arguments.len(), provenance)?;
            Some(value.ordinal())
        }
        None => None,
    };
    if let Some(missing) = produced.iter().position(Option::is_none) {
        let value = storage.functions[function.index() as usize].output_values[missing];
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "external function output",
            index: value,
            span: provenance.span(),
        });
    }
    validate_external_dependencies(storage, function, construction, &entries, provenance)?;
    Ok(ExternalBodyEntry {
        purity,
        language,
        symbol,
        arguments: entries,
        result,
        linkage,
    })
}

fn expect_declared_header(
    storage: &Storage,
    function: FunctionId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let entry = storage
        .functions
        .get(function.index() as usize)
        .ok_or_else(|| unknown("function", function.index(), provenance))?;
    if entry.parameter_values.len() != entry.parameters.len() {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "function parameter declaration",
            index: checked_u32(
                entry.parameter_values.len(),
                "function parameter ordinal",
                provenance,
            )?,
            span: provenance.span(),
        });
    }
    if entry.output_values.len() != entry.results.len() {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "function output declaration",
            index: checked_u32(
                entry.output_values.len(),
                "function output ordinal",
                provenance,
            )?,
            span: provenance.span(),
        });
    }
    if entry.definition.is_some() || entry.build.is_some() {
        return Err(duplicate("function", function.index(), provenance));
    }
    if !entry.definitions.is_empty() || !entry.folds.is_empty() {
        return Err(duplicate("function", function.index(), provenance));
    }
    Ok(())
}

/// MLS §12.9 external symbols name a foreign entry point exactly.
///
/// A rendered display name is not an entry point: the symbol must be a legal
/// C/Fortran identifier or construction fails with its declaration span.
fn expect_external_symbol(
    symbol: &VarName,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let text = symbol.as_str();
    let mut characters = text.chars();
    let legal = match characters.next() {
        Some(first) if first == '_' || first.is_ascii_alphabetic() => {
            characters.all(|character| character == '_' || character.is_ascii_alphanumeric())
        }
        _ => false,
    };
    if legal {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidExternalSymbol {
        symbol: symbol.clone(),
        span: provenance.span(),
    })
}

fn expect_external_input(
    storage: &Storage,
    function: FunctionId<'_>,
    expression: ExprId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if let Some(found_domain) = storage.expr_binder_domain(expression, provenance)? {
        return Err(DaeConstructionError::InvalidBinderScope {
            expected_domain: None,
            found_domain,
            span: provenance.span(),
        });
    }
    match storage.expr_function_scope(expression, provenance)? {
        None => {}
        Some(found) if found == function.index() => {}
        Some(found_function) => {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(function.index()),
                found_function,
                span: provenance.span(),
            });
        }
    }
    if let Some(raw) = storage
        .expressions
        .function_illegal_coordinates
        .get(expression.index() as usize)
        .copied()
        .ok_or_else(|| unknown("expression", expression.index(), provenance))?
    {
        let node = storage
            .expressions
            .nodes
            .get(raw as usize)
            .ok_or_else(|| unknown("expression", raw, provenance))?;
        let coordinate_provenance = storage
            .expressions
            .provenance
            .get(raw as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", raw, provenance))?;
        reject_model_coordinate(node, coordinate_provenance)?;
    }
    // An external body has no Modelica statement that could have defined a
    // function value, so an argument can never read one.
    let reads = storage
        .expressions
        .function_read_sets
        .get(expression.index() as usize)
        .copied()
        .ok_or_else(|| unknown("expression", expression.index(), provenance))?;
    storage.function_read_sets.try_for_each(reads, &mut |fact| {
        let span = storage
            .expressions
            .provenance
            .get(fact.witness as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", fact.witness, provenance))?
            .span();
        Err(DaeConstructionError::InvalidFunctionValueRead {
            value: fact.value,
            expected_definition: None,
            found_definition: fact.definition,
            span,
        })
    })
}

fn expect_external_output<'dae>(
    storage: &Storage,
    function: FunctionId<'dae>,
    value: FunctionValueId<'dae>,
    provenance: DaeProvenance,
) -> Result<usize, DaeConstructionError> {
    check_function_value_owner(function, value, provenance)?;
    let entry = function_value_entry(storage, value, provenance)?;
    if entry.role != FunctionValueRole::Output {
        return Err(DaeConstructionError::InvalidVariableRole {
            name: entry.name.clone(),
            span: provenance.span(),
        });
    }
    storage.functions[function.index() as usize]
        .output_values
        .iter()
        .position(|output| *output == value.ordinal())
        .ok_or_else(|| unknown("external function output", value.ordinal(), provenance))
}

fn record_produced_output(
    produced: &mut [Option<usize>],
    ordinal: usize,
    position: usize,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let output_count = produced.len();
    let slot = produced
        .get_mut(ordinal)
        .ok_or_else(|| invalid_arity(output_count, ordinal, provenance))?;
    if slot.is_some() {
        return Err(DaeConstructionError::DuplicateDefinition {
            kind: "external function output",
            index: checked_u32(ordinal, "external function output", provenance)?,
            span: provenance.span(),
        });
    }
    *slot = Some(position);
    Ok(())
}

/// External argument expressions obey the same call-order rule bodies obey.
fn validate_external_dependencies(
    storage: &Storage,
    function: FunctionId<'_>,
    construction: FunctionConstruction,
    arguments: &[ExternalArgumentEntry],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let limit = match construction {
        FunctionConstruction::Acyclic => function.index(),
        FunctionConstruction::Recursive { start, end } => {
            if !(start..end).contains(&function.index()) {
                return Err(DaeConstructionError::InvalidRecursiveFunctionGroup {
                    span: provenance.span(),
                });
            }
            end
        }
    };
    for argument in arguments {
        let ExternalArgumentEntry::Input(expression) = argument else {
            continue;
        };
        let latest_call = storage
            .expressions
            .function_latest_calls
            .get(*expression as usize)
            .copied()
            .ok_or_else(|| unknown("expression", *expression, provenance))?;
        let Some(call) = latest_call else {
            continue;
        };
        if call.target < limit {
            continue;
        }
        let span = storage
            .expressions
            .provenance
            .get(call.witness as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", call.witness, provenance))?
            .span();
        return Err(DaeConstructionError::InvalidFunctionDependency {
            function: function.index(),
            target: call.target,
            span,
        });
    }
    Ok(())
}
