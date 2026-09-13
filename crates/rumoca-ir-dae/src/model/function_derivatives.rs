use super::*;
use rumoca_core::FunctionDerivativeInput;

#[derive(Clone, Debug, PartialEq)]
pub(super) struct FunctionDerivativeEntry {
    pub(super) target: u32,
    pub(super) inputs: Vec<FunctionDerivativeInput>,
    pub(super) provenance: DaeProvenance,
    pub(super) previous: Option<(u32, u32)>,
    pub(super) priority: u32,
    order: u32,
    tangent_start: usize,
}

/// A checked differential, with original arguments followed by tangents.
///
/// This is an executable ABI contract, not an unchecked source annotation.
/// Higher-order links require a predecessor issued by checked construction.
#[derive(Clone, Copy)]
pub struct FunctionDerivativeView<'dae> {
    pub(super) source: FunctionId<'dae>,
    pub(super) ordinal: u32,
    pub(super) types: &'dae [ValueType],
    pub(super) parameters: &'dae [u32],
    pub(super) results: &'dae [u32],
    pub(super) entry: &'dae FunctionDerivativeEntry,
}

impl<'dae> FunctionDerivativeView<'dae> {
    pub fn id(self) -> FunctionDerivativeId<'dae> {
        FunctionDerivativeId::from_raw(self.source.index(), self.ordinal)
    }

    pub fn previous(self) -> Option<FunctionDerivativeId<'dae>> {
        self.entry
            .previous
            .map(|(function, ordinal)| FunctionDerivativeId::from_raw(function, ordinal))
    }

    pub const fn order(self) -> u32 {
        self.entry.order
    }

    pub const fn priority(self) -> u32 {
        self.entry.priority
    }

    pub const fn source(self) -> FunctionId<'dae> {
        self.source
    }

    pub fn target(self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry.target)
    }

    pub fn inputs(self) -> &'dae [FunctionDerivativeInput] {
        &self.entry.inputs
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }

    /// Source input ordinals whose tangents follow the original arguments.
    pub fn tangent_inputs(self) -> impl Iterator<Item = usize> + 'dae {
        self.parameters
            .iter()
            .enumerate()
            .skip(self.entry.tangent_start)
            .filter_map(move |(ordinal, ty)| {
                (self.entry.inputs[ordinal] == FunctionDerivativeInput::Differentiate
                    && contains_real(self.types, *ty))
                .then_some(ordinal)
            })
    }

    /// Derivative output ordinal for a Real-containing source result.
    pub fn result(self, source_result: usize) -> Option<usize> {
        self.results
            .iter()
            .enumerate()
            .filter(|(_, ty)| contains_real(self.types, **ty))
            .position(|(ordinal, _)| ordinal == source_result)
    }
}

impl<'dae> Functions<'_, 'dae> {
    /// Append an MLS §12.7.1 first derivative in source priority order.
    ///
    /// Both functions must already be complete. The constructor proves their
    /// common input prefix, compact tangent types, and filtered output ABI.
    /// Call-site applicability of `zeroDerivative` remains a differentiation
    /// proof obligation; `noDerivative` retains the source domain assumption.
    pub fn first_derivative(
        &mut self,
        source: FunctionId<'dae>,
        target: FunctionId<'dae>,
        inputs: impl IntoIterator<Item = FunctionDerivativeInput>,
        priority: u32,
        provenance: DaeProvenance,
    ) -> Result<FunctionDerivativeId<'dae>, DaeConstructionError> {
        self.insert_derivative(
            source,
            FunctionDerivativeEntry {
                target: target.index(),
                inputs: inputs.into_iter().collect(),
                provenance,
                previous: None,
                priority,
                order: 1,
                tangent_start: 0,
            },
        )
    }

    /// Extend only a constructor-issued differentiation chain (MLS §12.7.1).
    pub fn next_derivative(
        &mut self,
        source: FunctionId<'dae>,
        previous: FunctionDerivativeId<'dae>,
        target: FunctionId<'dae>,
        inputs: impl IntoIterator<Item = FunctionDerivativeInput>,
        priority: u32,
        provenance: DaeProvenance,
    ) -> Result<FunctionDerivativeId<'dae>, DaeConstructionError> {
        let parent = self
            .storage
            .functions
            .get(previous.function().index() as usize)
            .ok_or_else(|| {
                unknown(
                    "derivative predecessor function",
                    previous.function().index(),
                    provenance,
                )
            })?;
        let preceding = parent
            .derivatives
            .get(previous.ordinal() as usize)
            .ok_or_else(|| unknown("derivative predecessor", previous.ordinal(), provenance))?;
        if preceding.target != source.index() {
            return Err(invalid_derivative(
                "predecessor does not derive this function",
                provenance,
            ));
        }
        let inputs = inputs.into_iter().collect::<Vec<_>>();
        if preceding
            .inputs
            .iter()
            .zip(&inputs)
            .any(|(old, new)| *old == FunctionDerivativeInput::ZeroDerivative && old != new)
        {
            return Err(invalid_derivative(
                "inconsistent zeroDerivative across the derivative chain",
                provenance,
            ));
        }
        let order = preceding
            .order
            .checked_add(1)
            .ok_or_else(|| invalid_derivative("derivative order exceeds its domain", provenance))?;
        let tangent_start = parent.parameters.len();
        self.insert_derivative(
            source,
            FunctionDerivativeEntry {
                target: target.index(),
                inputs,
                provenance,
                previous: Some((previous.function().index(), previous.ordinal())),
                priority,
                order,
                tangent_start,
            },
        )
    }

    fn insert_derivative(
        &mut self,
        source: FunctionId<'dae>,
        entry: FunctionDerivativeEntry,
    ) -> Result<FunctionDerivativeId<'dae>, DaeConstructionError> {
        let provenance = entry.provenance;
        let target = FunctionId::from_raw(entry.target);
        check_provenance(self.source_map, provenance)?;
        function_checks::expect_complete_function(self.storage, source, provenance)?;
        function_checks::expect_complete_function(self.storage, target, provenance)?;
        check_derivative(self.storage, source, &entry)?;
        let links = &mut self.storage.functions[source.index() as usize].derivatives;
        if links
            .iter()
            .any(|old| old.priority == entry.priority && old.previous == entry.previous)
        {
            return Err(invalid_derivative(
                "duplicate priority in one differentiation context",
                provenance,
            ));
        }
        let ordinal = checked_u32(links.len(), "function derivatives", provenance)?;
        links.push(entry);
        Ok(FunctionDerivativeId::from_raw(source.index(), ordinal))
    }
}

fn invalid_derivative(reason: &'static str, provenance: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::InvalidFunctionDerivative {
        reason,
        span: provenance.span(),
    }
}

fn check_derivative(
    storage: &Storage,
    source: FunctionId<'_>,
    entry: &FunctionDerivativeEntry,
) -> Result<(), DaeConstructionError> {
    let provenance = entry.provenance;
    let inputs = &entry.inputs;
    let source = &storage.functions[source.index() as usize];
    let target = &storage.functions[entry.target as usize];
    if [source, target].into_iter().any(|function| {
        function
            .definition
            .as_ref()
            .and_then(FunctionBodyEntry::external)
            .is_some_and(|body| !body.purity.is_pure())
    }) {
        return Err(invalid_derivative(
            "differentiation requires pure functions",
            provenance,
        ));
    }
    let prefix = source.parameters.len();
    if !source
        .parameters
        .iter()
        .any(|ty| contains_real(&storage.value_types, *ty))
        || target.results.is_empty()
    {
        return Err(invalid_derivative(
            "a derivative requires Real-containing inputs and nonempty results",
            provenance,
        ));
    }
    if inputs.len() != prefix {
        return Err(invalid_derivative(
            "input restrictions do not cover the source signature",
            provenance,
        ));
    }
    if !target.parameters.starts_with(&source.parameters)
        || source
            .parameter_values
            .iter()
            .zip(&target.parameter_values)
            .any(|(original, derivative)| original.name != derivative.name)
    {
        return Err(invalid_derivative(
            "original input prefix differs in type, shape, or declaration order",
            provenance,
        ));
    }
    let tangents = source
        .parameters
        .iter()
        .zip(inputs)
        .skip(entry.tangent_start)
        .filter(|(ty, role)| {
            **role == FunctionDerivativeInput::Differentiate
                && contains_real(&storage.value_types, **ty)
        })
        .map(|(ty, _)| *ty);
    check_tangent_list(
        &storage.value_types,
        tangents,
        &target.parameters[prefix..],
        provenance,
    )?;
    let results = source
        .results
        .iter()
        .copied()
        .filter(|ty| contains_real(&storage.value_types, *ty));
    check_tangent_list(&storage.value_types, results, &target.results, provenance)
}

fn check_tangent_list(
    types: &[ValueType],
    originals: impl Iterator<Item = u32>,
    tangents: &[u32],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut tangents = tangents.iter();
    for original in originals {
        let Some(tangent) = tangents.next() else {
            return Err(invalid_derivative(
                "missing tangent argument or result",
                provenance,
            ));
        };
        if !is_tangent_type(types, original, *tangent) {
            return Err(invalid_derivative(
                "tangent type or shape differs from its original",
                provenance,
            ));
        }
    }
    if tangents.next().is_some() {
        return Err(invalid_derivative(
            "extra tangent argument or result",
            provenance,
        ));
    }
    Ok(())
}

fn contains_real(types: &[ValueType], ty: u32) -> bool {
    let ty = &types[ty as usize];
    ty.scalar_type() == ScalarType::Real
        || ty
            .record_fields()
            .any(|(_, field)| contains_real(types, field))
}

fn is_tangent_type(types: &[ValueType], original: u32, tangent: u32) -> bool {
    let original = &types[original as usize];
    let tangent = &types[tangent as usize];
    if original.dimensions() != tangent.dimensions() {
        return false;
    }
    match (original.scalar_type(), tangent.scalar_type()) {
        (ScalarType::Real, ScalarType::Real) => true,
        (ScalarType::Record, ScalarType::Record) => record_is_tangent(types, original, tangent),
        _ => false,
    }
}

fn record_is_tangent(types: &[ValueType], original: &ValueType, tangent: &ValueType) -> bool {
    let mut targets = tangent.record_fields();
    for (name, field) in original.record_fields() {
        if !contains_real(types, field) {
            continue;
        }
        let Some((target_name, target)) = targets.next() else {
            return false;
        };
        if name != target_name || !is_tangent_type(types, field, target) {
            return false;
        }
    }
    targets.next().is_none()
}
