//! GAL-020 variable classification over the canonical DAE.
//!
//! `Variable.causality` is authoritative — *not* the partition a variable
//! is stored in: a `discrete output Real` lives in the `z` partition but
//! keeps `causality = output` and classifies as a block output. The
//! partition, `is_tunable`, `origin`, and membership in the dedicated
//! `constants` map complete the decision:
//!
//! | DAE evidence | GALEC class |
//! |---|---|
//! | causality `input` | `Input` |
//! | causality `output` | `Output` |
//! | `constants` map | `Constant` |
//! | causality `parameter`, `is_tunable` | `TunableParameter` |
//! | causality `parameter`, not tunable (structural) | `Constant` |
//! | causality `calculatedParameter`, not tunable, origin `source` | `DependentParameter` (defining expression preserved symbolically in `Variable.start`) |
//! | generated `__pre__.` slot (origin `generated` + prefix) | `State`, named `'previous(x)'` (trap T2) |
//! | causality `local` in `z`/`m` | `State` |
//! | anything else | `ET010` diagnostic (fail early, no default class) |
//!
//! The generated condition vector `c` (the `f_c` targets) and its
//! `__pre__.c` slot are **projection-internal**: they encode the when-edge
//! guards the lowering slice unwraps back into DoStep control flow. They are
//! classified (as `State`) but flagged [`ClassifiedVariable::
//! projection_internal`] and never listed in the manifest: lowering either
//! inlines a `c[i]` reference to its defining `f_c` expression or rejects
//! the reference (`unsupported-feature:condition-memory-outside-guard`) —
//! condition machinery never survives into emitted code.
//!
//! Scalar types are resolved from structure only (never start values, S8):
//! caller-supplied [`ScalarTypeMap`](crate::input::ScalarTypeMap) provenance
//! first, then the partition contract (`x`/`y`/`u`/`w`/`z` are Real), then
//! condition-vector targets (Boolean by MLS B.1d), then pre-slot base
//! inheritance; otherwise `ET011`.

use std::collections::{HashMap, HashSet};

use rumoca_galec::ast::{Name, ScalarType};
use rumoca_ir_dae::{
    Dae, DaeVariablePartition, Variable, VariableCausality, VariableOrigin, component_base_name,
};

use crate::admissibility::for_each_variable;
use crate::diagnostic::GalecTargetError;
use crate::input::GalecInput;
use crate::mangle;

/// GALEC variable class per the GAL-020 classification table, mirroring the
/// manifest `blockCausality` enumeration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableClass {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    State,
}

impl VariableClass {
    /// The manifest `blockCausality` this class maps to.
    #[must_use]
    pub const fn block_causality(self) -> rumoca_efmi::algorithm_code_manifest::BlockCausality {
        use rumoca_efmi::algorithm_code_manifest::BlockCausality;
        match self {
            Self::Input => BlockCausality::Input,
            Self::Output => BlockCausality::Output,
            Self::TunableParameter => BlockCausality::TunableParameter,
            Self::DependentParameter => BlockCausality::DependentParameter,
            Self::Constant => BlockCausality::Constant,
            Self::State => BlockCausality::State,
        }
    }
}

/// One classified DAE variable.
#[derive(Debug, Clone)]
pub struct ClassifiedVariable<'a> {
    /// The untouched DAE variable.
    pub variable: &'a Variable,
    /// The DAE partition the variable was found in.
    pub partition: DaeVariablePartition,
    /// GAL-020 class.
    pub class: VariableClass,
    /// Structurally resolved scalar type (module docs).
    pub scalar_type: ScalarType,
    /// GALEC name per the GAL-015 mangling scheme (`'previous(x)'` for
    /// pre-slots).
    pub galec_name: Name,
    /// Base variable name when this is a generated `__pre__.` slot.
    pub pre_base: Option<String>,
    /// Generated when-edge machinery (`c` / `__pre__.c`), never listed in
    /// the manifest: lowering inlines or rejects references to it (module
    /// docs).
    pub projection_internal: bool,
}

/// Classification of every DAE variable, in canonical partition order.
///
/// Constructed via [`Classification::new`], which indexes the variables by
/// DAE name once so [`Classification::find`] stays O(1) per lookup (it is
/// called for every variable reference during lowering). `variables` is a
/// read surface; it is not mutated after construction.
#[derive(Debug, Clone, Default)]
pub struct Classification<'a> {
    pub variables: Vec<ClassifiedVariable<'a>>,
    /// DAE name → position in `variables` (built once at construction).
    by_name: HashMap<String, usize>,
}

impl<'a> Classification<'a> {
    /// Classification over pre-built variables, indexed by DAE name.
    #[must_use]
    pub fn new(variables: Vec<ClassifiedVariable<'a>>) -> Self {
        let by_name = variables
            .iter()
            .enumerate()
            .map(|(position, classified)| (classified.variable.name.as_str().to_owned(), position))
            .collect();
        Self { variables, by_name }
    }

    /// Find a classified variable by its DAE name.
    #[must_use]
    pub fn find(&self, name: &str) -> Option<&ClassifiedVariable<'a>> {
        self.by_name
            .get(name)
            .map(|&position| &self.variables[position])
    }
}

/// Classify every variable of the untouched DAE, collecting all failures.
pub fn classify_variables<'a>(
    input: &GalecInput<'a>,
) -> Result<Classification<'a>, Vec<GalecTargetError>> {
    let condition_targets = condition_targets(input.dae);
    let mut variables = Vec::new();
    let mut errors = Vec::new();
    for_each_variable(input.dae, |partition, variable| {
        match classify_one(input, partition, variable, &condition_targets) {
            Ok(classified) => variables.push(classified),
            Err(error) => errors.push(error),
        }
    });
    if errors.is_empty() {
        Ok(Classification::new(variables))
    } else {
        Err(errors)
    }
}

/// Base names of the generated condition vector: the `f_c` equation targets
/// (subscripts stripped, so `c[1]`..`c[4]` collapse to `c`).
fn condition_targets(dae: &Dae) -> HashSet<String> {
    dae.conditions
        .equations
        .iter()
        .filter_map(|equation| equation.lhs.as_ref())
        .filter_map(|lhs| component_base_name(lhs.as_str()))
        .collect()
}

fn classify_one<'a>(
    input: &GalecInput<'a>,
    partition: DaeVariablePartition,
    variable: &'a Variable,
    condition_targets: &HashSet<String>,
) -> Result<ClassifiedVariable<'a>, GalecTargetError> {
    let pre_base = pre_slot_base(variable);
    let class = variable_class(partition, variable, pre_base.is_some())?;
    let scalar_type =
        resolve_scalar_type(input, partition, variable, &pre_base, condition_targets)?;
    let galec_name = match &pre_base {
        Some(base) => mangle::pre_state_name(base)?,
        None => mangle::galec_variable_name(variable.name.as_str())?,
    };
    let own_base = component_base_name(variable.name.as_str());
    let projection_internal = own_base.is_some_and(|base| condition_targets.contains(&base))
        || pre_base
            .as_deref()
            .and_then(component_base_name)
            .is_some_and(|base| condition_targets.contains(&base));
    Ok(ClassifiedVariable {
        variable,
        partition,
        class,
        scalar_type,
        galec_name,
        pre_base,
        projection_internal,
    })
}

/// Base name of a generated `__pre__.` slot, when this variable is one.
///
/// The naming convention is owned by [`rumoca_core::pre_slot_name`];
/// recognition additionally requires `origin == generated`, so an
/// (illegally-named) source variable can never be mistaken for a pre-slot.
fn pre_slot_base(variable: &Variable) -> Option<String> {
    if variable.origin != VariableOrigin::Generated {
        return None;
    }
    rumoca_core::pre_slot_base(variable.name.as_str()).map(str::to_owned)
}

/// The GAL-020 decision table (module docs). Fails early on evidence
/// combinations the table has no row for.
fn variable_class(
    partition: DaeVariablePartition,
    variable: &Variable,
    is_pre_slot: bool,
) -> Result<VariableClass, GalecTargetError> {
    use DaeVariablePartition as P;
    use VariableCausality as C;
    if is_pre_slot {
        return Ok(VariableClass::State);
    }
    if partition == P::Constant {
        return Ok(VariableClass::Constant);
    }
    let class = match (variable.causality, partition) {
        (C::Input, _) => Some(VariableClass::Input),
        (C::Output, _) => Some(VariableClass::Output),
        (C::Parameter, _) if variable.is_tunable => Some(VariableClass::TunableParameter),
        // Structural (evaluate=true) parameters are legitimately fixed
        // post-compile; they surface as manifest constants. Tunable
        // parameters are NEVER folded to constants (GAL-020).
        (C::Parameter, _) => Some(VariableClass::Constant),
        (C::CalculatedParameter, _)
            if !variable.is_tunable && variable.origin == VariableOrigin::Source =>
        {
            Some(VariableClass::DependentParameter)
        }
        (C::Local, P::DiscreteReal | P::DiscreteValued) => Some(VariableClass::State),
        _ => None,
    };
    class.ok_or_else(|| GalecTargetError::UnclassifiableVariable {
        variable: variable.name.as_str().to_owned(),
        causality: causality_name(variable.causality),
        partition: partition_name(partition),
        origin: origin_name(variable.origin),
        span: variable.source_span,
    })
}

/// Structural scalar-type resolution (module docs). Never consults start
/// values; never defaults.
fn resolve_scalar_type(
    input: &GalecInput<'_>,
    partition: DaeVariablePartition,
    variable: &Variable,
    pre_base: &Option<String>,
    condition_targets: &HashSet<String>,
) -> Result<ScalarType, GalecTargetError> {
    if let Some(map) = input.scalar_types
        && let Some(scalar_type) = map.get(&variable.name)
    {
        return Ok(*scalar_type);
    }
    if let Some(base) = pre_base {
        return resolve_pre_base_type(input, variable, base, condition_targets);
    }
    let own_base = component_base_name(variable.name.as_str());
    if own_base.is_some_and(|base| condition_targets.contains(&base)) {
        // MLS B.1d: conditions are Boolean by construction.
        return Ok(ScalarType::Boolean);
    }
    partition_contract_type(partition).ok_or_else(|| GalecTargetError::UnresolvedScalarType {
        variable: variable.name.as_str().to_owned(),
        partition: partition_name(partition),
        span: variable.source_span,
    })
}

/// A pre-slot inherits the scalar type of its base variable.
fn resolve_pre_base_type(
    input: &GalecInput<'_>,
    pre_variable: &Variable,
    base: &str,
    condition_targets: &HashSet<String>,
) -> Result<ScalarType, GalecTargetError> {
    if let Some((partition, base_variable)) = find_variable(input.dae, base) {
        // Base variables are never themselves pre-slots (pre() of a pre-slot
        // does not exist), so this recursion terminates after one step.
        return resolve_scalar_type(input, partition, base_variable, &None, condition_targets);
    }
    if condition_targets.contains(&component_base_name(base).unwrap_or_else(|| base.to_owned())) {
        // `__pre__.c` for a condition vector that has no variable entry of
        // its own still types as Boolean (MLS B.1d).
        return Ok(ScalarType::Boolean);
    }
    Err(GalecTargetError::UnresolvedScalarType {
        variable: pre_variable.name.as_str().to_owned(),
        partition: partition_name(DaeVariablePartition::Parameter),
        span: pre_variable.source_span,
    })
}

/// Partitions whose DAE contract fixes the scalar type: `x`/`y` continuous
/// and `u`/`w` are Real (discrete-valued inputs are carried in `m`, per
/// `DaeMetadata::discrete_input_names`); `z` is discrete *Real* by
/// definition. `p`/`constants`/`m` admit multiple Modelica types and need
/// provenance.
fn partition_contract_type(partition: DaeVariablePartition) -> Option<ScalarType> {
    use DaeVariablePartition as P;
    match partition {
        P::State | P::Algebraic | P::Input | P::Output | P::DiscreteReal => Some(ScalarType::Real),
        P::Parameter | P::Constant | P::DiscreteValued => None,
    }
}

fn find_variable<'a>(dae: &'a Dae, name: &str) -> Option<(DaeVariablePartition, &'a Variable)> {
    let mut found = None;
    for_each_variable(dae, |partition, variable| {
        if found.is_none() && variable.name.as_str() == name {
            found = Some((partition, variable));
        }
    });
    found
}

pub(crate) fn partition_name(partition: DaeVariablePartition) -> &'static str {
    use DaeVariablePartition as P;
    match partition {
        P::State => "state",
        P::Algebraic => "algebraic",
        P::Input => "input",
        P::Output => "output",
        P::Parameter => "parameter",
        P::Constant => "constant",
        P::DiscreteReal => "discrete Real",
        P::DiscreteValued => "discrete-valued",
    }
}

fn causality_name(causality: VariableCausality) -> &'static str {
    use VariableCausality as C;
    match causality {
        C::Input => "input",
        C::Output => "output",
        C::Local => "local",
        C::Parameter => "parameter",
        C::CalculatedParameter => "calculatedParameter",
        C::Independent => "independent",
    }
}

fn origin_name(origin: VariableOrigin) -> &'static str {
    match origin {
        VariableOrigin::Source => "source",
        VariableOrigin::Generated => "generated",
    }
}
