//! Evaluation context: parameter/enum/function tables and scoped name lookup.

use indexmap::IndexMap;
use rumoca_core::{
    ComponentPath, DefId, EvalLookup, FunctionInstanceId, InstanceId, Reference, Span,
    scoped_component_path_candidates,
};
use rustc_hash::FxBuildHasher;
use std::hash::BuildHasher;

use super::errors::DeferredParameterSource;
use super::value::{ResolvedEnumValue, Value};
use super::{EvalIndexMap, Function};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ResolvedOccurrenceKey {
    pub instance_id: InstanceId,
    pub root_def_id: DefId,
}

#[derive(Clone, Debug)]
pub struct ResolvedValueBinding {
    pub identity: ResolvedOccurrenceKey,
    pub value: Value,
}

#[derive(Clone, Debug)]
pub struct ResolvedShapeBinding {
    pub identity: ResolvedOccurrenceKey,
    pub dimensions: Vec<i64>,
}

#[derive(Clone, Debug)]
pub struct ResolvedEnumDeclaration {
    pub declaration: DefId,
    pub type_name: String,
    pub literals: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct ResolvedEnumCatalog {
    literals: IndexMap<(DefId, String), ResolvedEnumValue, FxBuildHasher>,
}

impl ResolvedEnumCatalog {
    pub fn try_from_declarations(
        declarations: Vec<ResolvedEnumDeclaration>,
    ) -> Result<Self, super::EvalError> {
        let literal_capacity = declarations
            .iter()
            .map(|declaration| declaration.literals.len())
            .sum();
        let mut literals = IndexMap::with_capacity_and_hasher(literal_capacity, FxBuildHasher);
        let mut seen_declarations = rustc_hash::FxHashSet::with_hasher(rustc_hash::FxBuildHasher);
        for declaration in declarations {
            insert_enum_declaration(&mut literals, &mut seen_declarations, declaration)?;
        }
        Ok(Self { literals })
    }

    pub fn empty() -> Self {
        Self {
            literals: IndexMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn get_reference(&self, reference: &Reference) -> Option<&ResolvedEnumValue> {
        let component = reference.component_ref()?;
        let literal = component.parts().last()?.ident.as_str();
        let mut matches = component
            .parts()
            .iter()
            .rev()
            .skip(1)
            .filter_map(|part| self.literals.get(&(part.def_id, literal.to_string())));
        let selected = matches.next()?;
        matches.next().is_none().then_some(selected)
    }

    pub fn get(&self, declaration: DefId, literal: &str) -> Option<&ResolvedEnumValue> {
        self.literals.get(&(declaration, literal.to_string()))
    }

    pub fn contains_reference(&self, reference: &Reference) -> bool {
        self.get_reference(reference).is_some()
    }
}

fn insert_enum_declaration(
    catalog: &mut IndexMap<(DefId, String), ResolvedEnumValue, FxBuildHasher>,
    seen: &mut rustc_hash::FxHashSet<DefId>,
    declaration: ResolvedEnumDeclaration,
) -> Result<(), super::EvalError> {
    if declaration.type_name.is_empty()
        || declaration.literals.is_empty()
        || !seen.insert(declaration.declaration)
    {
        return Err(super::EvalError::Internal {
            message: "resolved enumeration catalog contains an empty or duplicate declaration"
                .to_string(),
        });
    }
    let mut declaration_literals = rustc_hash::FxHashSet::with_hasher(rustc_hash::FxBuildHasher);
    for (index, literal) in declaration.literals.into_iter().enumerate() {
        insert_enum_literal(
            catalog,
            &mut declaration_literals,
            declaration.declaration,
            &declaration.type_name,
            index,
            literal,
        )?;
    }
    Ok(())
}

fn insert_enum_literal(
    catalog: &mut IndexMap<(DefId, String), ResolvedEnumValue, FxBuildHasher>,
    declaration_literals: &mut rustc_hash::FxHashSet<String>,
    declaration: DefId,
    type_name: &str,
    index: usize,
    literal: String,
) -> Result<(), super::EvalError> {
    if literal.is_empty() || !declaration_literals.insert(literal.clone()) {
        return Err(super::EvalError::Internal {
            message: "resolved enumeration declaration contains an empty or duplicate literal"
                .to_string(),
        });
    }
    let ordinal = i64::try_from(index)
        .map_err(|_| enum_ordinal_domain_error())?
        .checked_add(1)
        .ok_or_else(enum_ordinal_domain_error)?;
    let key = (declaration, literal.clone());
    let value = ResolvedEnumValue::issued(declaration, type_name.to_string(), literal, ordinal);
    if catalog.insert(key, value).is_some() {
        return Err(super::EvalError::Internal {
            message: "resolved enumeration catalog contains a duplicate literal identity"
                .to_string(),
        });
    }
    Ok(())
}

fn enum_ordinal_domain_error() -> super::EvalError {
    super::EvalError::Internal {
        message: "resolved enumeration declaration exceeds the ordinal domain".to_string(),
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedIdentityInventory {
    values: IndexMap<ResolvedOccurrenceKey, Value, FxBuildHasher>,
    shapes: IndexMap<ResolvedOccurrenceKey, Vec<i64>, FxBuildHasher>,
}

impl ResolvedIdentityInventory {
    pub fn try_from_bindings(
        values: Vec<ResolvedValueBinding>,
        shapes: Vec<ResolvedShapeBinding>,
    ) -> Result<Self, super::EvalError> {
        let mut value_map = IndexMap::with_capacity_and_hasher(values.len(), FxBuildHasher);
        for binding in values {
            if contains_structural_enum(&binding.value) {
                return Err(super::EvalError::Internal {
                    message: "resolved value inventory contains an enumeration without declaration identity"
                        .to_string(),
                });
            }
            if value_map.insert(binding.identity, binding.value).is_some() {
                return Err(super::EvalError::Internal {
                    message: "resolved value inventory contains a duplicate occurrence identity"
                        .to_string(),
                });
            }
        }
        let mut shape_map = IndexMap::with_capacity_and_hasher(shapes.len(), FxBuildHasher);
        for binding in shapes {
            if shape_map
                .insert(binding.identity, binding.dimensions)
                .is_some()
            {
                return Err(super::EvalError::Internal {
                    message: "resolved shape inventory contains a duplicate occurrence identity"
                        .to_string(),
                });
            }
        }
        Ok(Self {
            values: value_map,
            shapes: shape_map,
        })
    }
}

fn contains_structural_enum(value: &Value) -> bool {
    match value {
        Value::Enum(_, _) => true,
        Value::Array(elements) => elements.iter().any(contains_structural_enum),
        Value::Record(fields) => fields.values().any(contains_structural_enum),
        _ => false,
    }
}

#[derive(Clone, Debug)]
enum ReferenceAuthority {
    StructuralPreIdentity,
    Resolved {
        values: IndexMap<ResolvedOccurrenceKey, Value, FxBuildHasher>,
        shapes: IndexMap<ResolvedOccurrenceKey, Vec<i64>, FxBuildHasher>,
    },
}

#[derive(Clone, Debug)]
enum CallableAuthority {
    NoUserCallables {
        capacity: usize,
    },
    PendingFlatIdentity {
        functions: Vec<Function>,
    },
    TransitionalFlatIdentity {
        pending: Vec<Function>,
        finalized: IndexMap<FunctionInstanceId, Function, FxBuildHasher>,
    },
    FinalizedFlatInstances {
        functions: IndexMap<FunctionInstanceId, Function, FxBuildHasher>,
    },
}

fn insert_finalized_function(
    functions: &mut IndexMap<FunctionInstanceId, Function, FxBuildHasher>,
    instance_id: FunctionInstanceId,
    function: Function,
) -> Result<(), super::EvalError> {
    if let Some(existing) = functions.get(&instance_id) {
        return Err(super::EvalError::Internal {
            message: format!(
                "function instance {} is issued as both `{}` and `{}`",
                instance_id.index(),
                existing.name,
                function.name
            ),
        });
    }
    functions.insert(instance_id, function);
    Ok(())
}

/// Evaluation context providing variable/parameter values.
#[derive(Clone, Debug)]
pub struct EvalContext {
    /// Parameter values by name (e.g., "component.subcomponent.param" -> value)
    pub parameters: EvalIndexMap<Value>,

    /// Exact enumeration products issued from declaration order.
    resolved_enums: ResolvedEnumCatalog,

    /// Closed authority over user-defined callables available to this frame.
    ///
    /// Pre-Flat collection retains pending semantic objects, but only the
    /// finalized arm can authorize evaluation by `FunctionInstanceId`.
    callable_authority: CallableAuthority,

    /// Known array dimensions when the array's element values are unavailable.
    ///
    /// This lets the shared evaluator handle `size`/`ndims` without allocating
    /// placeholder arrays or dropping multidimensional shape metadata.
    array_dimensions: EvalIndexMap<Vec<i64>>,

    /// Function-formal values keyed by their declaration identity. This table
    /// is separate from rendered parameter names, so a foreign same-spelling
    /// reference cannot select a formal binding.
    values_by_def: IndexMap<DefId, Value, FxBuildHasher>,

    /// Function-formal shapes keyed by declaration identity.
    array_dimensions_by_def: IndexMap<DefId, Vec<i64>, FxBuildHasher>,

    /// Declared `fixed = false` parameters the initialization system settles.
    ///
    /// These names are *declared* and resolve fine; what they lack is a
    /// translation-time number. Keeping them apart from the value table lets a
    /// failed lookup report the construct (MLS §8.6 deferred parameter) instead
    /// of reporting the name as unknown.
    deferred_parameters: EvalIndexMap<DeferredParameterSource>,

    /// Lexical instance scope used for modification-binding lookup.
    lookup_scope: Option<ComponentPath>,

    reference_authority: ReferenceAuthority,
}

impl EvalContext {
    pub fn structural_preidentity() -> Self {
        Self::structural_preidentity_with_capacity(0, 0)
    }

    pub fn structural_preidentity_with_capacity(parameters: usize, functions: usize) -> Self {
        Self::structural_preidentity_with_catalog(
            parameters,
            functions,
            ResolvedEnumCatalog::empty(),
        )
    }

    pub fn structural_preidentity_with_catalog(
        parameters: usize,
        functions: usize,
        enum_catalog: ResolvedEnumCatalog,
    ) -> Self {
        Self::allocate(
            parameters,
            enum_catalog,
            functions,
            ReferenceAuthority::StructuralPreIdentity,
        )
    }

    pub fn resolved(
        parameters: usize,
        functions: usize,
        inventory: ResolvedIdentityInventory,
        enum_catalog: ResolvedEnumCatalog,
    ) -> Self {
        Self::allocate(
            parameters,
            enum_catalog,
            functions,
            ReferenceAuthority::Resolved {
                values: inventory.values,
                shapes: inventory.shapes,
            },
        )
    }

    pub fn resolved_empty() -> Self {
        Self::resolved_empty_with_capacity(0, 0)
    }

    pub fn resolved_empty_with_capacity(parameters: usize, functions: usize) -> Self {
        Self::allocate(
            parameters,
            ResolvedEnumCatalog::empty(),
            functions,
            ReferenceAuthority::Resolved {
                values: IndexMap::with_hasher(FxBuildHasher),
                shapes: IndexMap::with_hasher(FxBuildHasher),
            },
        )
    }

    fn allocate(
        parameters: usize,
        enum_catalog: ResolvedEnumCatalog,
        functions: usize,
        reference_authority: ReferenceAuthority,
    ) -> Self {
        Self {
            parameters: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            resolved_enums: enum_catalog,
            callable_authority: CallableAuthority::NoUserCallables {
                capacity: functions,
            },
            array_dimensions: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            values_by_def: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            array_dimensions_by_def: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            deferred_parameters: IndexMap::with_capacity_and_hasher(0, FxBuildHasher),
            lookup_scope: None,
            reference_authority,
        }
    }

    /// Issue one canonical function definition into the evaluator catalog.
    pub fn try_add_function(&mut self, func: Function) -> Result<(), super::EvalError> {
        let name = func.name.to_string();
        if name.is_empty()
            || func.exposure_def_id.index() == 0
            || func.def_id.is_some_and(|def_id| def_id.index() == 0)
        {
            return Err(super::EvalError::Internal {
                message: format!("function `{name}` has invalid semantic identity"),
            });
        }
        let Some(instance_id) = func.instance_id else {
            return self.try_add_pending_function(func);
        };
        if func.def_id.is_none() {
            return Err(super::EvalError::Internal {
                message: format!("function `{name}` has invalid semantic identity"),
            });
        }
        match &mut self.callable_authority {
            CallableAuthority::NoUserCallables { capacity } => {
                let mut functions = IndexMap::with_capacity_and_hasher(*capacity, FxBuildHasher);
                functions.insert(instance_id, func);
                self.callable_authority = CallableAuthority::FinalizedFlatInstances { functions };
                Ok(())
            }
            CallableAuthority::PendingFlatIdentity { functions } => {
                let pending = std::mem::take(functions);
                let mut finalized = IndexMap::with_capacity_and_hasher(1, FxBuildHasher);
                finalized.insert(instance_id, func);
                self.callable_authority =
                    CallableAuthority::TransitionalFlatIdentity { pending, finalized };
                Ok(())
            }
            CallableAuthority::TransitionalFlatIdentity { finalized, .. } => {
                insert_finalized_function(finalized, instance_id, func)
            }
            CallableAuthority::FinalizedFlatInstances { functions } => {
                insert_finalized_function(functions, instance_id, func)
            }
        }
    }

    fn try_add_pending_function(&mut self, func: Function) -> Result<(), super::EvalError> {
        match &mut self.callable_authority {
            CallableAuthority::NoUserCallables { capacity } => {
                let mut functions = Vec::with_capacity(*capacity);
                functions.push(func);
                self.callable_authority = CallableAuthority::PendingFlatIdentity { functions };
                Ok(())
            }
            CallableAuthority::PendingFlatIdentity { functions } => {
                if !functions.contains(&func) {
                    functions.push(func);
                }
                Ok(())
            }
            CallableAuthority::TransitionalFlatIdentity { pending, .. } => {
                if !pending.contains(&func) {
                    pending.push(func);
                }
                Ok(())
            }
            CallableAuthority::FinalizedFlatInstances { functions } => {
                let finalized_capacity = functions.capacity();
                let finalized = std::mem::replace(
                    functions,
                    IndexMap::with_capacity_and_hasher(finalized_capacity, FxBuildHasher),
                );
                self.callable_authority = CallableAuthority::TransitionalFlatIdentity {
                    pending: vec![func],
                    finalized,
                };
                Ok(())
            }
        }
    }

    pub(crate) fn refuse_pending_callable(
        &self,
        occurrence: &Reference,
        span: Span,
    ) -> Result<(), super::EvalError> {
        if occurrence.resolved_function().is_none()
            && matches!(
                self.callable_authority,
                CallableAuthority::PendingFlatIdentity { .. }
                    | CallableAuthority::TransitionalFlatIdentity { .. }
            )
        {
            return Err(super::EvalError::PendingCallableIdentity {
                name: occurrence.as_str().to_string(),
                span,
            });
        }
        Ok(())
    }

    pub(crate) fn finalized_function_by_instance(
        &self,
        instance_id: FunctionInstanceId,
    ) -> Option<&Function> {
        match &self.callable_authority {
            CallableAuthority::FinalizedFlatInstances { functions } => functions.get(&instance_id),
            CallableAuthority::TransitionalFlatIdentity { finalized, .. } => {
                finalized.get(&instance_id)
            }
            CallableAuthority::NoUserCallables { .. }
            | CallableAuthority::PendingFlatIdentity { .. } => None,
        }
    }

    /// Fixture-only direct catalog insertion for interpreter unit tests that
    /// exercise pre-identity function bodies in isolation.
    #[cfg(test)]
    pub(crate) fn insert_direct_function_fixture(&mut self, mut func: Function) {
        if func.def_id.is_none() {
            func.def_id = Some(func.exposure_def_id);
        }
        if func.instance_id.is_none() {
            func.instance_id = Some(FunctionInstanceId::new(func.exposure_def_id.index()));
        }
        self.try_add_function(func)
            .expect("fixture function identity is exact and unique");
    }

    #[cfg(test)]
    pub(crate) fn finalized_function_count(&self) -> usize {
        match &self.callable_authority {
            CallableAuthority::FinalizedFlatInstances { functions } => functions.len(),
            CallableAuthority::TransitionalFlatIdentity { finalized, .. } => finalized.len(),
            CallableAuthority::NoUserCallables { .. }
            | CallableAuthority::PendingFlatIdentity { .. } => 0,
        }
    }

    #[cfg(test)]
    pub(crate) fn pending_function_count(&self) -> Option<usize> {
        match &self.callable_authority {
            CallableAuthority::PendingFlatIdentity { functions } => Some(functions.len()),
            CallableAuthority::TransitionalFlatIdentity { pending, .. } => Some(pending.len()),
            CallableAuthority::NoUserCallables { .. }
            | CallableAuthority::FinalizedFlatInstances { .. } => None,
        }
    }

    /// Add a parameter value.
    pub fn add_parameter(&mut self, name: impl Into<String>, value: Value) {
        self.parameters.insert(name.into(), value);
    }

    pub(crate) fn add_def_parameter(&mut self, def_id: DefId, value: Value) {
        self.values_by_def.insert(def_id, value);
    }

    /// Extend a resolved evaluation frame with one declaration-local value.
    ///
    /// Function specialization happens after occurrence resolution, but its
    /// formal and local values are not model-coordinate occurrences. They are
    /// therefore keyed by their issued declaration identity in this separate
    /// frame. The operation is fallible and insertion is last so an invalid or
    /// duplicate binding cannot partially mutate the frame.
    pub fn try_bind_resolved_declaration_value(
        &mut self,
        def_id: DefId,
        value: Value,
    ) -> Result<(), super::EvalError> {
        if !matches!(
            self.reference_authority,
            ReferenceAuthority::Resolved { .. }
        ) {
            return Err(super::EvalError::Internal {
                message: "resolved declaration values require a resolved evaluation frame"
                    .to_string(),
            });
        }
        if def_id.index() == 0 || contains_structural_enum(&value) {
            return Err(super::EvalError::Internal {
                message: "resolved declaration value lacks exact semantic identity".to_string(),
            });
        }
        match self.values_by_def.entry(def_id) {
            indexmap::map::Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
            indexmap::map::Entry::Occupied(_) => Err(super::EvalError::Internal {
                message: format!("resolved declaration value {def_id} is bound more than once"),
            }),
        }
    }

    pub(crate) fn permits_structural_name_lookup(&self) -> bool {
        matches!(
            self.reference_authority,
            ReferenceAuthority::StructuralPreIdentity
        )
    }

    /// Whether a value is bound to the declaration `def_id` itself.
    pub(crate) fn has_def_value(&self, def_id: DefId) -> bool {
        self.values_by_def.contains_key(&def_id)
    }

    /// Whether an array shape is bound to the declaration `def_id` itself.
    pub(crate) fn has_def_shape(&self, def_id: DefId) -> bool {
        self.array_dimensions_by_def.contains_key(&def_id)
    }

    pub(crate) fn get_reference(&self, reference: &Reference) -> Option<&Value> {
        if let Some(def_id) = reference.root_def_id()
            && let Some(value) = self.values_by_def.get(&def_id)
        {
            return Some(value);
        }
        match &self.reference_authority {
            ReferenceAuthority::StructuralPreIdentity => self.get(reference.as_str()),
            ReferenceAuthority::Resolved { values, .. } => {
                let identity = resolved_occurrence_key(reference)?;
                values.get(&identity)
            }
        }
    }

    /// A reference read under resolved authority must carry its exact
    /// occurrence identity, except one whose root declaration this context
    /// bound directly: a call frame binds a function formal under its
    /// declaration identity alone, by value for a scalar and by checked shape
    /// for an array (`size(x, k)` reads that shape, and no extent expression
    /// consumes the array value), so either binding vouches for the read.
    pub(crate) fn validate_resolved_reference(
        &self,
        reference: &Reference,
        span: Span,
    ) -> Result<(), super::EvalError> {
        if matches!(
            self.reference_authority,
            ReferenceAuthority::StructuralPreIdentity
        ) || self.resolved_enums.get_reference(reference).is_some()
            || reference.root_def_id().is_some_and(|def_id| {
                self.values_by_def.contains_key(&def_id)
                    || self.array_dimensions_by_def.contains_key(&def_id)
            })
        {
            return Ok(());
        }
        if resolved_occurrence_key(reference).is_none() {
            return Err(super::EvalError::InvalidSemanticIr {
                reason: format!(
                    "post-Resolve reference `{}` lacks its exact occurrence InstanceId/root DefId pair",
                    reference.as_str()
                ),
                span,
            });
        }
        Ok(())
    }

    /// Drop the value bound to `name`, if any.
    ///
    /// A caller that rebinds a name to a scope where the value is *not* known —
    /// a function formal shadowing an enclosing model coordinate of the same
    /// flat name — must remove the inherited value rather than leave it
    /// readable, or the inner scope would fold an outer coordinate's value.
    pub fn remove_parameter(&mut self, name: &str) {
        self.parameters.shift_remove(name);
    }

    /// Record that `name` is a declared parameter the initialization system
    /// determines, so a failed value lookup can name that construct.
    pub fn add_deferred_parameter(
        &mut self,
        name: impl Into<String>,
        source: DeferredParameterSource,
    ) {
        self.deferred_parameters.insert(name.into(), source);
    }

    /// What settles `name`, when it is a declared but translation-time
    /// valueless parameter.
    pub fn deferred_parameter(&self, name: &str) -> Option<DeferredParameterSource> {
        self.lookup_value(&self.deferred_parameters, name).copied()
    }

    /// Add shape metadata without materializing placeholder element values.
    pub fn add_array_dimensions(&mut self, name: impl Into<String>, dims: Vec<i64>) {
        self.array_dimensions.insert(name.into(), dims);
    }

    pub(crate) fn add_def_array_dimensions(&mut self, def_id: DefId, dims: Vec<i64>) {
        self.array_dimensions_by_def.insert(def_id, dims);
    }

    pub(crate) fn get_array_dimensions_reference(&self, reference: &Reference) -> Option<&[i64]> {
        if let Some(def_id) = reference.root_def_id()
            && let Some(shape) = self.array_dimensions_by_def.get(&def_id)
        {
            return Some(shape);
        }
        match &self.reference_authority {
            ReferenceAuthority::StructuralPreIdentity => {
                self.get_array_dimensions(reference.as_str())
            }
            ReferenceAuthority::Resolved { shapes, .. } => {
                let identity = resolved_occurrence_key(reference)?;
                shapes.get(&identity).map(Vec::as_slice)
            }
        }
    }

    /// Value of one declaration occurrence, keyed on its complete identity.
    pub fn occurrence_value(&self, identity: ResolvedOccurrenceKey) -> Option<&Value> {
        let ReferenceAuthority::Resolved { values, .. } = &self.reference_authority else {
            return None;
        };
        values.get(&identity)
    }

    /// Set the lexical scope used for unqualified parameter references.
    pub fn set_lookup_scope(&mut self, scope: Option<ComponentPath>) {
        self.lookup_scope = scope;
    }

    /// Look up a variable/parameter by name.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.lookup_value(&self.parameters, name)
    }

    /// Look up an enum literal by resolved identity when structure is present.
    /// A structured miss never falls back to spelling: doing so would let a
    /// same-named declaration impersonate the referenced enum.
    pub fn get_enum_reference(&self, reference: &Reference) -> Option<&ResolvedEnumValue> {
        self.resolved_enums.get_reference(reference)
    }

    pub(super) fn get_array_dimensions(&self, name: &str) -> Option<&[i64]> {
        self.lookup_value(&self.array_dimensions, name)
            .map(Vec::as_slice)
    }

    fn lookup_value<'a, T>(&'a self, values: &'a EvalIndexMap<T>, name: &str) -> Option<&'a T> {
        let name_path = ComponentPath::from_flat_path(name);
        if name_path.len() == 1
            && let Some(scope) = self.lookup_scope.as_ref()
            && let Some(value) = lookup_scoped(values, name, &scope.to_flat_string())
        {
            return Some(value);
        }
        if let Some(value) = values.get(name) {
            return Some(value);
        }
        if let Some(scope) = self.lookup_scope.as_ref()
            && let Some(value) = lookup_scoped(values, name, &scope.to_flat_string())
        {
            return Some(value);
        }
        None
    }
}

fn lookup_scoped<'a, T, S>(
    map: &'a IndexMap<String, T, S>,
    name: &str,
    scope: &str,
) -> Option<&'a T>
where
    S: BuildHasher,
{
    let name_path = ComponentPath::from_flat_path(name);
    let scope_path = ComponentPath::from_flat_path(scope);
    for candidate in scoped_component_path_candidates(&name_path, &scope_path) {
        if let Some(value) = map.get(&candidate) {
            return Some(value);
        }
    }
    None
}

impl EvalLookup for EvalContext {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::as_integer)
    }

    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::to_real)
    }

    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::as_bool)
    }
}

fn resolved_occurrence_key(reference: &Reference) -> Option<ResolvedOccurrenceKey> {
    Some(ResolvedOccurrenceKey {
        instance_id: reference.instance_id()?,
        root_def_id: reference.root_def_id()?,
    })
}
