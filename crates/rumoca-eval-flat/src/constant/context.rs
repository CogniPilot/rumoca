//! Evaluation context: parameter/enum/function tables and scoped name lookup.

use indexmap::IndexMap;
use rumoca_core::{ComponentPath, EvalLookup, InstanceId, scoped_component_path_candidates};
use rustc_hash::FxBuildHasher;
use std::borrow::Cow;
use std::hash::BuildHasher;

use super::value::Value;
use super::{EvalIndexMap, Function, VarName};

/// Evaluation context providing variable/parameter values.
#[derive(Clone, Debug)]
pub struct EvalContext {
    /// Parameter values by name (e.g., "component.subcomponent.param" -> value)
    pub parameters: EvalIndexMap<Value>,

    /// Enum values: "TypeName.LiteralName" -> (TypeName, LiteralName)
    pub enum_literals: EvalIndexMap<(String, String)>,

    /// User-defined function definitions for constant evaluation (MLS §12).
    pub functions: EvalIndexMap<Function>,

    /// Known array dimensions when the array's element values are unavailable.
    ///
    /// This lets the shared evaluator handle `size`/`ndims` without allocating
    /// placeholder arrays or dropping multidimensional shape metadata.
    array_dimensions: EvalIndexMap<Vec<i64>>,

    /// Values of declarations whose exact occurrence identity is known.
    ///
    /// This is the identity-keyed inventory: a caller that holds the
    /// [`InstanceId`] of the declaration it evaluated registers the value here,
    /// and every decision about that declaration is then a lookup on the
    /// identity itself rather than on a rendered name that two occurrences of
    /// the same declaration would share.
    values_by_instance: IndexMap<InstanceId, Value, FxBuildHasher>,

    /// Lexical instance scope used for modification-binding lookup.
    lookup_scope: Option<ComponentPath>,
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}

impl EvalContext {
    /// Create an empty evaluation context.
    pub fn new() -> Self {
        Self::with_capacity(0, 0, 0)
    }

    /// Create an evaluation context sized for the expected contents.
    pub fn with_capacity(parameters: usize, enum_literals: usize, functions: usize) -> Self {
        Self {
            parameters: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            enum_literals: IndexMap::with_capacity_and_hasher(enum_literals, FxBuildHasher),
            functions: IndexMap::with_capacity_and_hasher(functions, FxBuildHasher),
            array_dimensions: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            values_by_instance: IndexMap::with_capacity_and_hasher(parameters, FxBuildHasher),
            lookup_scope: None,
        }
    }

    /// Add a function definition for constant evaluation.
    pub fn add_function(&mut self, func: Function) {
        let full_name = func.name.to_string();
        // Add with full name
        self.functions.insert(full_name.clone(), func.clone());
        // Also add with short name (last component) for function body lookups
        // This enables recursive calls inside function bodies that use unqualified names
        let short_name = func.name.last_segment().to_string();
        if short_name != full_name && !self.functions.contains_key(&short_name) {
            let mut short_func = func;
            short_func.name = VarName::new(&short_name);
            self.functions.insert(short_name, short_func);
        }
    }

    /// Add a parameter value.
    pub fn add_parameter(&mut self, name: impl Into<String>, value: Value) {
        self.parameters.insert(name.into(), value);
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

    /// Add shape metadata without materializing placeholder element values.
    pub fn add_array_dimensions(&mut self, name: impl Into<String>, dims: Vec<i64>) {
        self.array_dimensions.insert(name.into(), dims);
    }

    /// Record the value of the declaration occurrence `instance_id`.
    ///
    /// `name` is the rendered key the expression evaluator resolves references
    /// through; `instance_id` is the exact identity every *decision* about the
    /// declaration is keyed on.
    pub fn add_instance_parameter(
        &mut self,
        instance_id: InstanceId,
        name: impl Into<String>,
        value: Value,
    ) {
        self.values_by_instance.insert(instance_id, value.clone());
        self.parameters.insert(name.into(), value);
    }

    /// Value of one declaration occurrence, keyed on its exact identity.
    pub fn instance_value(&self, instance_id: InstanceId) -> Option<&Value> {
        self.values_by_instance.get(&instance_id)
    }

    /// Set the lexical scope used for unqualified parameter references.
    pub fn set_lookup_scope(&mut self, scope: Option<ComponentPath>) {
        self.lookup_scope = scope;
    }

    /// Look up a variable/parameter by name.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.lookup_value(&self.parameters, name)
    }

    /// Look up an enum literal by qualified name.
    pub fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.lookup_value(&self.enum_literals, name)
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

    fn lookup_enum<'a>(&'a self, name: &str, scope: &str) -> Option<Cow<'a, str>> {
        if let Some((type_name, literal)) = lookup_scoped(&self.enum_literals, name, scope) {
            return Some(Cow::Owned(format!("{type_name}.{literal}")));
        }
        lookup_scoped(&self.parameters, name, scope)
            .and_then(Value::as_enum)
            .map(|(type_name, literal)| Cow::Owned(format!("{type_name}.{literal}")))
    }
}
