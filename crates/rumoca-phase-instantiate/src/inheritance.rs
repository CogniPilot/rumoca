//! Inheritance processing for the instantiate phase (MLS §7.1).
//!
//! This module handles the `extends` clause processing, merging inherited
//! components and equations into the derived class.
//!
//! ## MLS Compliance Status
//!
//! ### Extends/Inheritance (MLS §7)
//! - [x] MLS §7.1 - Basic extends clause processing
//! - [x] MLS §7.1 - Multiple inheritance order preservation
//! - [x] MLS §7.1 - O(1) base class lookup via DefId
//! - [x] MLS §7.1 - Inheritance caching for diamond inheritance
//! - [x] MLS §7.1.2 - Protected extends visibility
//! - [x] MLS §7.2 - Modification environment (outer overrides inner)
//! - [x] MLS §7.2.5 - `each` modifier prefix (tracked via `ModificationValue.each`)
//! - [x] MLS §7.2.6 - `final` modifier prefix (validation implemented)
//! - [x] MLS §7.3 - Redeclarations (replaceable/final validation)
//! - [x] MLS §7.3.2 - Constrainedby validation (subtype checking)
//! - [x] MLS §7.4 - Selective model extension (`break` names with validation)
//!
//! ### Inner/Outer (MLS §5.4) - implemented in lib.rs
//! - [x] MLS §5.4 - Inner declaration tracking (scope-based)
//! - [x] MLS §5.4 - Outer reference resolution (nearest inner)
//! - [x] MLS §5.4 - Type compatibility checking (inheritance-aware)

use indexmap::IndexMap;
use rumoca_core::{DefId, SourceMap, Span, is_builtin_type};
use rumoca_ir_ast as ast;
use std::sync::Arc;

use crate::errors::{InstantiateError, InstantiateResult};

const CONSTRAINEDBY_MOD_PREFIX: &str = "__constrainedby__.";

/// Cache for inheritance results to avoid recomputation.
///
/// This is particularly important for diamond inheritance patterns where
/// a base class may be inherited through multiple paths.
///
/// Using `Arc<InheritedContent>` for O(1) cache retrieval - no deep cloning needed
/// when the same base class is inherited through multiple paths.
pub type InheritanceCache = IndexMap<DefId, Arc<InheritedContent>>;

/// Cache for subtype check results to avoid recomputation.
///
/// Maps (subtype_name, supertype_name) to the result of the subtype check.
/// This is useful for deeply nested inheritance hierarchies.
pub type SubtypeCache = IndexMap<(String, String), bool>;

/// Check if two type names refer to the same type.
///
/// This handles cases where names differ in qualification level:
/// - "Interfaces.CompositeStepState" matches "StateGraph.Interfaces.CompositeStepState"
/// - "A.B.C" matches "X.A.B.C" if either name exists in the tree
///
/// MLS §5.4, §7.3: Type name matching must handle relative vs qualified names.
pub fn type_names_match(tree: &ast::ClassTree, name_a: &str, name_b: &str) -> bool {
    if name_a == name_b {
        return true;
    }

    // Try DefId-based comparison: resolve both names and compare
    let def_a = tree.name_map.get(name_a).copied();
    let def_b = tree.name_map.get(name_b).copied();

    if let (Some(a), Some(b)) = (def_a, def_b) {
        return a == b;
    }

    // Fallback: if only one resolved, check if the other is a suffix of its qualified name
    if let Some(a_id) = def_a
        && let Some(qualified_a) = tree.def_map.get(&a_id)
    {
        return qualified_a.ends_with(&format!(".{}", name_b))
            || name_b.ends_with(&format!(".{}", qualified_a));
    }
    if let Some(b_id) = def_b
        && let Some(qualified_b) = tree.def_map.get(&b_id)
    {
        return qualified_b.ends_with(&format!(".{}", name_a))
            || name_a.ends_with(&format!(".{}", qualified_b));
    }

    false
}

/// Result of processing inheritance for a class.
#[derive(Debug, Clone, Default)]
pub struct InheritedContent {
    /// Components inherited from all base classes.
    pub components: IndexMap<String, ast::Component>,
    /// Equations inherited from all base classes.
    pub equations: Vec<ast::Equation>,
    /// Initial equations inherited from all base classes.
    pub initial_equations: Vec<ast::Equation>,
    /// Algorithm sections inherited from all base classes.
    pub algorithms: Vec<Vec<ast::Statement>>,
    /// Initial algorithm sections inherited from all base classes.
    pub initial_algorithms: Vec<Vec<ast::Statement>>,
    /// Nested classes inherited from all base classes.
    pub classes: IndexMap<String, ast::ClassDef>,
}

/// Apply protected visibility to a component if the extend is protected.
///
/// MLS §7.1.2: Protected extends makes inherited elements protected.
fn apply_protected_visibility(comp: &mut ast::Component, is_protected: bool) {
    if is_protected {
        comp.is_protected = true;
    }
}

/// Apply protected visibility to a class if the extend is protected.
///
/// MLS §7.1.2: Protected extends makes inherited elements protected.
fn apply_protected_class_visibility(class: &mut ast::ClassDef, is_protected: bool) {
    if is_protected {
        class.is_protected = true;
    }
}

/// Extract the target name from a modification expression.
///
/// Returns the first part of the component reference for modifications like:
/// - `ast::Expression::Modification { target, .. }` -> target name
/// - `ast::Expression::ClassModification { target, .. }` -> target name
fn extract_modification_target(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. } => {
            target.parts.first().map(|p| p.ident.text.to_string())
        }
        // For named arguments like `x = value`, extract the name
        ast::Expression::NamedArgument { name, .. } => Some(name.text.to_string()),
        _ => None,
    }
}

/// Extract the value from a modification expression.
///
/// MLS §7.2: Modifications can override component bindings.
/// Handles forms like:
/// - `extends Foo(n=2)` -> returns Literal(2)
/// - `extends Foo(final n=2)` -> returns Literal(2)
/// - `SomeType x(start=0)` -> returns Literal(0)
///
/// Returns None if no value can be extracted.
fn extract_modification_value(expr: &ast::Expression) -> Option<ast::Expression> {
    let value = match expr {
        ast::Expression::Modification { value, .. } => Some(value),
        ast::Expression::NamedArgument { value, .. } => Some(value),
        _ => None,
    }?;

    // Don't return Empty values
    if matches!(value.as_ref(), ast::Expression::Empty) {
        None
    } else {
        Some(value.as_ref().clone())
    }
}

/// Try to extract a value modification for a component from an extends modification.
///
/// MLS §7.2: Value modifications in extends clauses override inherited bindings.
/// Returns Some((name, value)) if this modification applies to a component in the class.
fn try_extract_value_modification(
    modification: &ast::ExtendModification,
    class: &ast::ClassDef,
) -> Option<(String, ast::Expression)> {
    // Only non-redeclare modifications can be value modifications
    if modification.redeclare {
        return None;
    }
    let target_name = extract_modification_target(&modification.expr)?;
    // Only apply if the base class has this component
    if !class.components.contains_key(&target_name) {
        return None;
    }
    let value = extract_modification_value(&modification.expr)?;
    Some((target_name, value))
}

/// Extract a value modification target/value from an extends modification without
/// constraining the target to immediate base-class local components.
///
/// This is used after inherited components are merged so modifications can apply
/// to transitively inherited members (MLS §7.2), e.g. `extends Mid(a(x=2))`
/// when `a` is declared in a grandparent class.
fn try_extract_value_modification_any(
    modification: &ast::ExtendModification,
) -> Option<(String, ast::Expression)> {
    if modification.redeclare {
        return None;
    }
    let target_name = extract_modification_target(&modification.expr)?;
    let value = extract_modification_value(&modification.expr)?;
    // Nested class modifications are merged separately.
    if matches!(value, ast::Expression::ClassModification { .. }) {
        return None;
    }
    Some((target_name, value))
}

/// Extract the new type from a redeclaration modification.
///
/// Redeclarations can have forms like:
/// - `redeclare model M = NewM` -> returns "NewM"
/// - `redeclare Real x` -> returns "Real"
/// - `redeclare type T = Integer` -> returns "Integer"
/// - `redeclare TransientData.CellData cellData` -> returns "TransientData.CellData"
///
/// Returns None if the new type cannot be determined from the expression.
fn extract_redeclare_type(expr: &ast::Expression) -> Option<String> {
    match expr {
        // Type assignment: `redeclare model M = NewM` or `redeclare type T = Integer`
        // Also handles: `redeclare TransientData.CellData cellData` where value is ClassModification
        ast::Expression::Modification { value, .. } => {
            // The value might be a component reference to the new type
            if let ast::Expression::ComponentReference(comp_ref) = value.as_ref() {
                return Some(comp_ref.to_string());
            }
            // Or it might be a class modification with the type as target
            // This handles: `Modification { target: cellData, value: ClassModification { target: TypeName, ... } }`
            if let ast::Expression::ClassModification { target, .. } = value.as_ref() {
                return Some(target.to_string());
            }
            None
        }
        // Class modification with type: might have type info in the modification
        ast::Expression::ClassModification { target, .. } => {
            // For class modifications like `redeclare Real x(...)`, the target itself is the type
            // This is a simplified extraction; full parsing would need access to component decl
            Some(target.to_string())
        }
        // Named argument: `redeclare type T = Integer` where value is the new type
        ast::Expression::NamedArgument { value, .. } => {
            if let ast::Expression::ComponentReference(comp_ref) = value.as_ref() {
                return Some(comp_ref.to_string());
            }
            None
        }
        _ => None,
    }
}

/// Extract the new type from a redeclaration modification with tree lookup for full qualification.
///
/// This version uses the ast::ClassTree to resolve short type names to fully qualified names
/// via the def_id (if available) or tree lookup.
fn extract_redeclare_type_qualified(
    expr: &ast::Expression,
    tree: &ast::ClassTree,
) -> Option<String> {
    // Try to get the def_id directly from the expression's ast::ComponentReference
    let def_id_opt = match expr {
        ast::Expression::Modification { value, .. } => {
            if let ast::Expression::ComponentReference(comp_ref) = value.as_ref() {
                comp_ref.def_id
            } else if let ast::Expression::ClassModification { target, .. } = value.as_ref() {
                target.def_id
            } else {
                None
            }
        }
        ast::Expression::ClassModification { target, .. } => target.def_id,
        _ => None,
    };

    // If we have a def_id, use it to get the fully qualified name
    if let Some(def_id) = def_id_opt
        && let Some(qualified) = tree.def_map.get(&def_id)
    {
        return Some(qualified.clone());
    }

    // Fall back to extracting the type name from the expression
    let type_name = extract_redeclare_type(expr)?;

    // Try to find the fully qualified name via tree lookup
    // Check if this short name maps to a class in the tree
    if let Some(&def_id) = tree.name_map.get(&type_name)
        && let Some(qualified) = tree.def_map.get(&def_id)
    {
        return Some(qualified.clone());
    }

    // Also try looking for the class directly (handles already-qualified names)
    if find_class_in_tree(tree, &type_name).is_some() {
        return Some(type_name);
    }

    // Return the original name if we can't find a qualified version
    // This allows the subtype check to handle the name matching
    Some(type_name)
}

/// Validate a redeclaration against the base class component.
///
/// MLS §7.3: Redeclarations are only valid for replaceable elements.
/// MLS §7.2.6: Final elements cannot be redeclared.
/// MLS §7.3.2: Redeclared type must satisfy constrainedby.
///
/// # Arguments
/// * `tree` - The class tree for type compatibility checking
/// * `component` - The base class component being redeclared
/// * `target_name` - Name of the component being redeclared
/// * `new_type` - The new type being redeclared to (if known)
/// * `span` - Source location for error reporting
fn validate_redeclaration(
    tree: &ast::ClassTree,
    component: &ast::Component,
    target_name: &str,
    new_type: Option<&str>,
    span: Span,
) -> InstantiateResult<()> {
    // MLS §7.2.6: Check if component is final
    if component.is_final {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }

    // MLS §7.3: Check if component is replaceable
    if !component.is_replaceable {
        return Err(Box::new(InstantiateError::redeclare_non_replaceable(
            target_name,
            span,
        )));
    }

    // MLS §7.3.2: Validate constrainedby
    // The redeclared type must be a subtype of the constraining type.
    // If no constrainedby is specified, the original type is the constraint.
    if let Some(new_type_name) = new_type {
        // Resolve the constraint type to fully qualified name
        let constraint_type_raw = component
            .constrainedby
            .as_ref()
            .map(|n| n.to_string())
            .unwrap_or_else(|| component.type_name.to_string());

        // Try to resolve constraint type using def_id or tree lookup
        let constraint_type = if let Some(def_id) = component.type_def_id
            && let Some(qualified) = tree.def_map.get(&def_id)
        {
            qualified.clone()
        } else if let Some(&def_id) = tree.name_map.get(&constraint_type_raw)
            && let Some(qualified) = tree.def_map.get(&def_id)
        {
            qualified.clone()
        } else {
            constraint_type_raw.clone()
        };

        // Try to resolve new type name using the constraint type's package as context
        // This handles cases like GearType1 in the same package as GearType2
        let resolved_new_type = resolve_type_in_context(tree, new_type_name, &constraint_type);

        if !is_type_subtype(tree, &resolved_new_type, &constraint_type) {
            return Err(Box::new(InstantiateError::redeclare_constraint_violation(
                target_name,
                &resolved_new_type,
                &constraint_type,
                span,
            )));
        }
    }

    Ok(())
}

/// Try to resolve a type name using the context of another type's package.
///
/// For example, if context_type is "Package.SubPackage.TypeB" and type_name is "TypeA",
/// this will try "Package.SubPackage.TypeA" first.
fn resolve_type_in_context(tree: &ast::ClassTree, type_name: &str, context_type: &str) -> String {
    // Builtins are always fully qualified
    if is_builtin_type(type_name) {
        return type_name.to_string();
    }

    // If the name already exists in the tree, return as-is
    if tree.name_map.contains_key(type_name) {
        return type_name.to_string();
    }

    // Try to resolve by prepending context package prefixes
    // For context "A.B.C.TypeX", try: "A.B.C.{type_name}", "A.B.{type_name}", "A.{type_name}"
    let mut package = context_type.to_string();
    while let Some(last_dot) = package.rfind('.') {
        package.truncate(last_dot);
        let qualified = format!("{}.{}", package, type_name);
        if tree.name_map.contains_key(&qualified) {
            return qualified;
        }
    }

    // Fall back to the original name
    type_name.to_string()
}

/// Check if `subtype` is a subtype of `supertype`.
///
/// MLS §7.3.2: A type is a subtype if it's the same type or extends the supertype.
/// MLS §5.4: Also used for inner/outer type compatibility checking.
///
/// This is a simplified check that handles:
/// 1. Exact type match
/// 2. Built-in type matching (Real, Integer, Boolean, String)
/// 3. Class inheritance via extends
///
/// For performance-critical code with deeply nested inheritance, use
/// `is_type_subtype_cached` instead.
pub fn is_type_subtype(tree: &ast::ClassTree, subtype: &str, supertype: &str) -> bool {
    let mut cache = SubtypeCache::new();
    is_type_subtype_cached(tree, subtype, supertype, &mut cache)
}

/// Check if `subtype` is a subtype of `supertype` with caching.
///
/// This cached version avoids recomputation for deeply nested inheritance
/// hierarchies. The cache maps (subtype, supertype) pairs to their results.
///
/// For replaceable component redeclarations, this function also considers
/// "sibling types" as compatible. Two types A and B are siblings if they
/// both directly extend the same base class. This supports common MSL patterns
/// where CellStack and CellRCStack both extend BaseCellStack.
pub fn is_type_subtype_cached(
    tree: &ast::ClassTree,
    subtype: &str,
    supertype: &str,
    cache: &mut SubtypeCache,
) -> bool {
    // Exact match is always a subtype
    if subtype == supertype {
        return true;
    }

    // Check if the types match when considering short vs qualified names
    if type_names_match(tree, subtype, supertype) {
        return true;
    }

    // Check cache first
    let cache_key = (subtype.to_string(), supertype.to_string());
    if let Some(&result) = cache.get(&cache_key) {
        return result;
    }

    // A built-in subtype can't extend anything else - no subtyping between primitives
    // But a class type CAN extend a built-in type (e.g., SI.Voltage extends Real)
    if is_builtin_type(subtype) {
        cache.insert(cache_key, false);
        return false;
    }

    // For class types, check if subtype's class extends supertype's class
    let result = if let Some(subtype_class) = find_class_in_tree(tree, subtype) {
        if class_extends_cached(tree, subtype_class, supertype, cache) {
            true
        } else {
            // Check for sibling types: both extend the same base class
            // This supports replaceable component redeclarations where both types
            // share a common base (e.g., CellRCStack and CellStack both extend BaseCellStack)
            if let Some(supertype_class) = find_class_in_tree(tree, supertype) {
                types_share_common_base(tree, subtype_class, supertype_class, cache)
            } else if is_builtin_type(supertype) {
                // Supertype is a built-in type (Real, Integer, Boolean, String) not
                // in the class tree. Check if subtype transitively extends this built-in.
                // This handles e.g. Resistance -> Real, Voltage -> Real chains.
                class_extends_builtin(tree, subtype_class, supertype)
            } else {
                false
            }
        }
    } else {
        false
    };

    cache.insert(cache_key, result);
    result
}

/// Check if two types share a common direct base class.
///
/// This is used for replaceable component redeclarations where sibling types
/// (both extending the same base) should be considered compatible per MLS §6.4's
/// interface compatibility requirements.
fn types_share_common_base(
    tree: &ast::ClassTree,
    type_a: &ast::ClassDef,
    type_b: &ast::ClassDef,
    cache: &mut SubtypeCache,
) -> bool {
    // Get direct base classes of type_a
    for extend_a in &type_a.extends {
        let base_a_name = extend_a.base_name.to_string();

        // Check if type_b also extends this base (directly or via name matching)
        for extend_b in &type_b.extends {
            let base_b_name = extend_b.base_name.to_string();

            // Check for exact match or name equivalence
            if base_a_name == base_b_name || type_names_match(tree, &base_a_name, &base_b_name) {
                return true;
            }

            // Also check transitively - if type_b extends something that extends base_a
            let base_b_class = extend_b
                .base_def_id
                .and_then(|id| tree.get_class_by_def_id(id))
                .or_else(|| find_class_in_tree(tree, &base_b_name));
            if base_b_class.is_some_and(|c| class_extends_cached(tree, c, &base_a_name, cache)) {
                return true;
            }
        }
    }

    false
}

/// Check if a class transitively extends a built-in type (Real, Integer, Boolean, String).
///
/// Built-in types are not stored in the class tree, so `class_extends_cached` may fail
/// to detect the chain. This function walks the extends chain with a depth limit,
/// checking if any base_name matches the target built-in type.
///
/// This handles type alias chains like:
/// ```modelica
/// type Resistance = Real(final quantity="ElectricalResistance", final unit="Ohm");
/// ```
fn class_extends_builtin(tree: &ast::ClassTree, class: &ast::ClassDef, builtin: &str) -> bool {
    const MAX_DEPTH: usize = 10;
    let mut current = Some(class);
    for _ in 0..MAX_DEPTH {
        let Some(cls) = current else { return false };
        for extend in &cls.extends {
            let base_name = extend.base_name.to_string();
            if base_name == builtin || type_names_match(tree, &base_name, builtin) {
                return true;
            }
        }
        // Follow the first extends clause (type aliases have exactly one)
        if cls.extends.len() == 1 {
            let ext = &cls.extends[0];
            current = ext
                .base_def_id
                .and_then(|id| tree.get_class_by_def_id(id))
                .or_else(|| find_class_in_tree(tree, &ext.base_name.to_string()));
        } else {
            return false;
        }
    }
    false
}

/// Find a class by name in the tree (top-level or nested).
///
/// Uses O(1) lookup via the name_map (populated during resolve phase).
/// For nested classes, use the qualified name (e.g., "Package.Inner").
/// Also tries common MSL prefixes for short names (e.g., "Resistance" -> "Modelica.Units.SI.Resistance").
///
/// # Panics
/// Debug builds panic if name_map is empty (indicates resolve phase wasn't run).
pub fn find_class_in_tree<'a>(tree: &'a ast::ClassTree, name: &str) -> Option<&'a ast::ClassDef> {
    // O(1) lookup via name_map (populated during resolve phase)
    if let Some(&def_id) = tree.name_map.get(name) {
        return tree.get_class_by_def_id(def_id);
    }

    // Also check top-level classes directly (handles cases where name_map
    // uses qualified names but caller uses short names for top-level classes)
    if let Some(class) = tree.definitions.classes.get(name) {
        return Some(class);
    }

    // Suffix matching: find fully qualified names ending with ".{name}".
    // This handles unresolved import aliases (e.g., `import SI = Modelica.Units.SI`)
    // and short names like `Resistance` → `Modelica.Units.SI.Resistance`.
    let suffix = format!(".{}", name);
    for (qualified, &def_id) in &tree.name_map {
        if qualified.ends_with(&suffix)
            && let Some(class) = tree.get_class_by_def_id(def_id)
        {
            return Some(class);
        }
    }

    None
}

/// Check if a class is effectively primitive (a short class definition extending a primitive type).
///
/// Short class definitions like `connector BooleanInput = input Boolean;` are syntactic sugar
/// for `connector BooleanInput extends Boolean; end BooleanInput;` with causality.
/// Such classes should be treated as primitive for variable creation purposes.
///
/// Per Flatten Phase Roadmap: Components using such types should become flat variables with the
/// type's causality applied.
///
/// Check if a type is effectively primitive, resolving type alias chains transitively.
///
/// This handles cases like:
/// ```modelica
/// type SpecificHeatCapacity = Real(...);
/// type SpecificHeatCapacityAtConstantPressure = SpecificHeatCapacity;
/// ```
///
/// Where `SpecificHeatCapacityAtConstantPressure` should be considered primitive
/// because it ultimately resolves to `Real`.
///
/// MLS §4.6: Type classes (short class definitions) create type aliases.
pub fn is_effectively_primitive_transitive(tree: &ast::ClassTree, class: &ast::ClassDef) -> bool {
    // A class is effectively primitive if it:
    // 1. Has no components (not a container)
    // 2. Has no equations (not a model with behavior)
    // 3. Either:
    //    a. Has exactly one extends clause that transitively leads to a built-in type
    //    b. Is an enumeration type (has enum_literals)
    if !class.components.is_empty() {
        return false;
    }
    if !class.equations.is_empty() || !class.initial_equations.is_empty() {
        return false;
    }

    // Check for enumeration types - they are primitive values
    if !class.enum_literals.is_empty() {
        return true;
    }

    // Check for extends to a type that is primitive (built-in or transitively primitive)
    if class.extends.len() != 1 {
        return false;
    }

    let extend = &class.extends[0];
    let base_name = extend.base_name.to_string();

    // If the direct base is a built-in, we're done
    if is_builtin_type(&base_name) {
        return true;
    }

    // Otherwise, try to look up the base type and check transitively
    // (with a depth limit to avoid infinite loops on malformed models)
    const MAX_DEPTH: usize = 10;

    // Use base_def_id for O(1) lookup when available (populated during resolve phase)
    // This handles cases where the base name is unqualified (e.g., "DigitalSignal")
    // but the actual class is in a package (e.g., "Interfaces.DigitalSignal")
    let mut current_class = extend
        .base_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, &base_name));

    for _ in 0..MAX_DEPTH {
        // Look up the current type
        let Some(bc) = current_class else {
            // Can't find the class - might be unresolved, assume not primitive
            return false;
        };
        // If this class has components or equations, not primitive
        if !bc.components.is_empty() || !bc.equations.is_empty() || !bc.initial_equations.is_empty()
        {
            return false;
        }
        // If this is an enumeration type, it's primitive
        if !bc.enum_literals.is_empty() {
            return true;
        }
        // If it extends exactly one thing, follow the chain
        if bc.extends.len() != 1 {
            return false;
        }
        let next_extend = &bc.extends[0];
        let next_name = next_extend.base_name.to_string();
        if is_builtin_type(&next_name) {
            return true;
        }
        // Use base_def_id for O(1) lookup, fall back to name lookup
        current_class = next_extend
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| find_class_in_tree(tree, &next_name));
    }

    // Exceeded max depth, assume not primitive
    false
}

/// Check if a type is Integer or Boolean (discrete by default per MLS §4.5).
///
/// This function resolves type alias chains to determine if the base type
/// is Integer or Boolean, which are discrete by default even without an
/// explicit `discrete` variability prefix.
///
/// MLS §4.5: "A discrete-time variable is a variable that is discrete-valued
/// (that is, not of Real type) or assigned in when-clauses."
/// Integer and Boolean variables are discrete by definition.
pub fn is_discrete_by_type(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
) -> bool {
    // Helper to check if a name is Integer or Boolean
    fn is_discrete_builtin(name: &str) -> bool {
        let simple_name = name.rsplit('.').next().unwrap_or(name);
        simple_name == "Integer" || simple_name == "Boolean"
    }

    // Direct check on the type name
    if is_discrete_builtin(type_name) {
        return true;
    }

    // If we have a class definition, check its inheritance chain
    let Some(class) = class_def else {
        return false;
    };

    // Enumerations are discrete values
    if !class.enum_literals.is_empty() {
        return true;
    }

    // Follow the inheritance chain with a depth limit
    const MAX_DEPTH: usize = 10;

    // If the class extends something, follow the chain
    if class.extends.len() == 1 {
        let extend = &class.extends[0];
        let base_name = extend.base_name.to_string();

        if is_discrete_builtin(&base_name) {
            return true;
        }

        // Use base_def_id for O(1) lookup when available (populated during resolve phase)
        let mut current_class = extend
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| find_class_in_tree(tree, &base_name));

        for _ in 0..MAX_DEPTH {
            // Look up the current type
            let Some(bc) = current_class else {
                return false;
            };

            // Enumerations are discrete
            if !bc.enum_literals.is_empty() {
                return true;
            }

            // Follow the chain if there's exactly one extends
            if bc.extends.len() != 1 {
                return false;
            }
            let next_extend = &bc.extends[0];
            let next_name = next_extend.base_name.to_string();
            if is_discrete_builtin(&next_name) {
                return true;
            }
            // Use base_def_id for O(1) lookup, fall back to name lookup
            current_class = next_extend
                .base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &next_name));
        }
    }

    false
}

/// Check if a class extends a base class (by name) directly or transitively.
///
/// MLS §7.1: A class that extends another inherits all its contents.
/// This creates a subtype relationship.
///
/// For performance-critical code with deeply nested inheritance, use
/// `class_extends_cached` instead.
pub fn class_extends(tree: &ast::ClassTree, class: &ast::ClassDef, base_name: &str) -> bool {
    let mut cache = SubtypeCache::new();
    class_extends_cached(tree, class, base_name, &mut cache)
}

/// Check if a class extends a base class (by name) directly or transitively, with caching.
///
/// This cached version avoids recomputation for deeply nested inheritance hierarchies.
/// It handles cases where type names differ in qualification level (short vs full paths).
pub fn class_extends_cached(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    base_name: &str,
    cache: &mut SubtypeCache,
) -> bool {
    let class_name = class.name.text.to_string();
    let target_base_def_id = tree
        .get_def_id_by_name(base_name)
        .or_else(|| find_class_in_tree(tree, base_name).and_then(|c| c.def_id));

    // Check cache first
    let cache_key = (class_name.clone(), base_name.to_string());
    if let Some(&result) = cache.get(&cache_key) {
        return result;
    }

    for extend in &class.extends {
        let extend_name = extend.base_name.to_string();
        // DefId-based direct match handles relative extends names that do not
        // string-match the queried supertype (e.g. "StateGraph.Interfaces.X"
        // vs "Interfaces.X").
        if let Some(target_id) = target_base_def_id
            && extend.base_def_id == Some(target_id)
        {
            cache.insert(cache_key, true);
            return true;
        }
        // Direct extension - use type_names_match for short vs qualified name handling
        if type_names_match(tree, &extend_name, base_name) {
            cache.insert(cache_key, true);
            return true;
        }
        // Transitive extension - use def_id for O(1) lookup when available
        let base_class = if let Some(def_id) = extend.base_def_id {
            tree.get_class_by_def_id(def_id)
        } else {
            find_class_in_tree(tree, &extend_name)
        };
        if let Some(base_class) = base_class {
            if let Some(target_id) = target_base_def_id
                && base_class.def_id == Some(target_id)
            {
                cache.insert(cache_key.clone(), true);
                return true;
            }
            if class_extends_cached(tree, base_class, base_name, cache) {
                cache.insert(cache_key.clone(), true);
                return true;
            }
        }
    }

    cache.insert(cache_key, false);
    false
}

/// Process extends clauses and collect inherited content.
///
/// MLS §7.1: "The extends-clause results in including the contents of the
/// base class at the point of the extends-clause."
///
/// MLS §7.1: "The ordering of multiple extends-clauses defines the order
/// in which the base-class contents are merged."
///
/// The merge order per MLS is: first the base class's own content, then
/// recursively its base classes. This ensures shallow inheritance takes
/// precedence over deep inheritance.
///
/// This function creates a fresh cache for each call. For processing multiple
/// classes that share base classes, use `process_extends_with_cache` instead.
pub fn process_extends(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<InheritedContent> {
    let mut cache = InheritanceCache::new();
    process_extends_with_cache(tree, class, &mut cache)
}

/// Process extends clauses with caching to avoid recomputation.
///
/// This is the internal implementation that uses a cache to handle diamond
/// inheritance efficiently. The cache stores processed inheritance results
/// keyed by DefId.
///
/// ## Diamond Inheritance
///
/// Consider: D extends B, C; B extends A; C extends A;
/// Without caching, A's content would be processed twice.
/// With caching, A's content is computed once and reused.
pub fn process_extends_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<InheritedContent> {
    // Check cache first (requires class to have a DefId)
    if let Some(def_id) = class.def_id
        && let Some(cached) = cache.get(&def_id)
    {
        // Cache hit: clone the inner InheritedContent
        // Note: We can't avoid cloning here because the cache keeps the Arc
        // and we need to return an owned InheritedContent for mutation
        return Ok((**cached).clone());
    }

    let mut inherited = InheritedContent::default();

    for extend in &class.extends {
        // Skip built-in types (Real, Integer, Boolean, String, ExternalObject)
        // They don't have components/equations to inherit, just type properties
        if is_builtin_type(&extend.base_name.to_string()) {
            continue;
        }

        // Look up the base class
        let base_class = resolve_base_class(tree, extend)?;

        // MLS §7.1: First merge the base class's own content
        merge_class_content(tree, &mut inherited, base_class, extend)?;

        // Then recursively process the base class's extends (with cache)
        let base_inherited = process_extends_with_cache(tree, base_class, cache)?;
        merge_inherited(&mut inherited, base_inherited, extend, &tree.source_map)?;

        // MLS §7.2: Apply extends modifications after recursive merge so
        // transitively inherited targets are available.
        apply_extends_modifications(&mut inherited, extend);
    }

    // Store in cache for reuse, then return
    // Wrap in Arc first, clone Arc (cheap, just refcount increment) for cache, then unwrap to return
    let inherited_arc = Arc::new(inherited);
    if let Some(def_id) = class.def_id {
        cache.insert(def_id, Arc::clone(&inherited_arc));
    }

    // If we're the only reference (refcount=1), move without cloning; otherwise clone
    Ok(Arc::unwrap_or_clone(inherited_arc))
}

/// Apply non-redeclare extends modifications to merged inherited components.
///
/// This post-merge pass ensures modifications like `extends Mid(c(k=2))` also
/// apply when `c` is declared in a grandparent class.
fn apply_extends_modifications(target: &mut InheritedContent, extend: &ast::Extend) {
    for modification in &extend.modifications {
        let Some((name, value)) = try_extract_value_modification_any(modification) else {
            continue;
        };
        let Some(comp) = target.components.get_mut(&name) else {
            continue;
        };
        comp.start = value.clone();
        comp.binding = Some(value);
        comp.has_explicit_binding = true;
    }

    merge_nested_extends_modifications(target, extend);
}

/// Resolve a base class from an extends clause.
///
/// Uses O(1) DefId lookup via ast::ClassTree.get_class_by_def_id().
/// Requires base_def_id to be set (done during resolve phase).
fn resolve_base_class<'a>(
    tree: &'a ast::ClassTree,
    extend: &ast::Extend,
) -> InstantiateResult<&'a ast::ClassDef> {
    let base_name = extend.base_name.to_string();
    let def_id = extend
        .base_def_id
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(base_name.clone())))?;

    tree.get_class_by_def_id(def_id)
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(base_name)))
}

/// Return the output size of a class's `equalityConstraint` function (MLS §9.4).
///
/// Returns `Some(n)` where `n` is the scalar size of the function's output
/// (e.g., 3 for `Orientation` whose `equalityConstraint` returns `Real[3]`).
/// Returns `None` if the class has no `equalityConstraint` function.
pub fn equality_constraint_output_size(class: &ast::ClassDef) -> Option<usize> {
    let eq_func = class.classes.values().find(|c| {
        c.class_type == rumoca_ir_ast::ClassType::Function
            && c.name.text.as_ref() == "equalityConstraint"
    })?;

    // Find the output component of the function
    for comp in eq_func.components.values() {
        if matches!(comp.causality, ast::Causality::Output(_)) {
            // Compute the product of array dimensions (e.g., Real[3] → 3, Real[3,3] → 9)
            if comp.shape.is_empty() {
                return Some(1); // scalar output
            }
            return Some(comp.shape.iter().product());
        }
    }

    // If we found the function but no output component, default to 3
    // (common case for Orientation's equalityConstraint returning Real[3])
    Some(3)
}

/// Create a Span from a ast::Location using the source map for file resolution.
pub fn location_to_span(loc: &rumoca_ir_ast::Location, source_map: &SourceMap) -> Span {
    source_map.location_to_span(&loc.file_name, loc.start as usize, loc.end as usize)
}

/// Create a Span from an Option<ast::Location> using the source map.
/// Returns Span::DUMMY if the location is None.
pub fn option_location_to_span(
    loc: Option<&rumoca_ir_ast::Location>,
    source_map: &SourceMap,
) -> Span {
    loc.map(|l| location_to_span(l, source_map))
        .unwrap_or(Span::DUMMY)
}

/// Compare variability by semantic kind (ignoring ast::Token locations).
fn variability_eq(a: &ast::Variability, b: &ast::Variability) -> bool {
    matches!(
        (a, b),
        (ast::Variability::Empty, ast::Variability::Empty)
            | (ast::Variability::Constant(_), ast::Variability::Constant(_))
            | (ast::Variability::Discrete(_), ast::Variability::Discrete(_))
            | (
                ast::Variability::Parameter(_),
                ast::Variability::Parameter(_)
            )
    )
}

/// Compare causality by semantic kind (ignoring ast::Token locations).
fn causality_eq(a: &ast::Causality, b: &ast::Causality) -> bool {
    matches!(
        (a, b),
        (ast::Causality::Empty, ast::Causality::Empty)
            | (ast::Causality::Input(_), ast::Causality::Input(_))
            | (ast::Causality::Output(_), ast::Causality::Output(_))
    )
}

/// Check if two components are compatible for diamond inheritance or equivalent declarations.
///
/// MLS §5.6: If the same element is inherited multiple times (diamond inheritance),
/// it should only contribute one element. Components are also compatible if they
/// have "equivalent" declarations (same type, variability, and causality).
///
/// Compatible conditions (in priority order):
/// 1. Same def_id (same original declaration) - always OK (diamond inheritance)
/// 2. Same type_def_id (same resolved type class) - OK for type aliases
/// 3. Same type_name string + same variability + same causality - equivalent declarations
///
/// Returns true if the components are from the same origin or have compatible types.
fn components_are_compatible(existing: &ast::Component, incoming: &ast::Component) -> bool {
    // Fast path: same def_id means same original declaration (diamond inheritance)
    if let (Some(existing_def_id), Some(incoming_def_id)) = (existing.def_id, incoming.def_id)
        && existing_def_id == incoming_def_id
    {
        return true;
    }

    // Check type compatibility via type_def_id (handles import aliases)
    if let (Some(existing_type), Some(incoming_type)) = (existing.type_def_id, incoming.type_def_id)
        && existing_type == incoming_type
    {
        return true;
    }

    // MLS §5.6: Components with equivalent declarations are compatible.
    // Compare by string representation (avoids ast::Location/token_number differences in ast::Token).
    // Also verify variability and causality match for true equivalence.
    // Use semantic comparison for variability/causality (ignoring ast::Token internals).
    existing.type_name.to_string() == incoming.type_name.to_string()
        && variability_eq(&existing.variability, &incoming.variability)
        && causality_eq(&existing.causality, &incoming.causality)
}

/// Merge inherited content from a base class.
fn merge_inherited(
    target: &mut InheritedContent,
    base: InheritedContent,
    extend: &ast::Extend,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    // Merge components, checking for conflicts
    for (name, comp) in base.components {
        // Check if this component is deselected via `break`
        if extend.break_names.contains(&name) {
            continue;
        }

        if let Some(existing) = target.components.get(&name) {
            // MLS §5.6: Check if components are from same origin or have compatible types
            if !components_are_compatible(existing, &comp) {
                return Err(Box::new(InstantiateError::conflicting_inheritance(
                    name.clone(),
                    "previous base",
                    extend.base_name.to_string(),
                    location_to_span(&extend.location, source_map),
                )));
            }
            // Compatible - diamond inheritance is OK, keep existing
        } else {
            let mut inherited_comp = comp;
            apply_protected_visibility(&mut inherited_comp, extend.is_protected);
            target.components.insert(name, inherited_comp);
        }
    }

    // Merge equations (no conflict checking - all equations are accumulated)
    target.equations.extend(base.equations);
    target.initial_equations.extend(base.initial_equations);

    // Merge algorithms
    target.algorithms.extend(base.algorithms);
    target.initial_algorithms.extend(base.initial_algorithms);

    // Merge nested classes
    for (name, class) in base.classes {
        if !target.classes.contains_key(&name) {
            let mut inherited_class = class;
            apply_protected_class_visibility(&mut inherited_class, extend.is_protected);
            target.classes.insert(name, inherited_class);
        }
    }

    Ok(())
}

/// Merge content from a class definition into inherited content.
///
/// MLS §7.3: Validates redeclarations against replaceable/final constraints.
/// Collect and validate redeclarations from extends modifications (MLS §7.3).
fn collect_redeclarations(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    extend: &ast::Extend,
    extend_span: Span,
) -> InstantiateResult<IndexMap<String, String>> {
    let mut redeclare_types = IndexMap::new();

    for modification in &extend.modifications {
        if !modification.redeclare {
            continue;
        }
        let Some(target_name) = extract_modification_target(&modification.expr) else {
            continue;
        };
        let new_type = extract_redeclare_type_qualified(&modification.expr, tree);
        let Some(component) = class.components.get(&target_name) else {
            continue;
        };

        validate_redeclaration(
            tree,
            component,
            &target_name,
            new_type.as_deref(),
            extend_span,
        )?;

        if let Some(new_type_name) = new_type {
            redeclare_types.insert(target_name, new_type_name);
        }
    }
    Ok(redeclare_types)
}

/// MLS §7.3.2: Validates constrainedby type constraints.
/// Full type replacement is deferred to later phases; here we validate structural constraints.
///
/// # Performance Note
///
/// This function clones components, equations, algorithms, and nested classes from the
/// borrowed `&ast::ClassDef`. Cloning is necessary because:
/// 1. We borrow from the ast::ClassTree which must remain immutable during compilation
/// 2. Inherited content may need mutations (e.g., applying protected visibility)
/// 3. The same base class may be inherited through multiple paths (diamond inheritance)
///
/// The inheritance cache (`InheritanceCache`) mitigates the cost by caching results
/// per DefId, avoiding redundant processing of the same base class.
fn merge_class_content(
    tree: &ast::ClassTree,
    target: &mut InheritedContent,
    class: &ast::ClassDef,
    extend: &ast::Extend,
) -> InstantiateResult<()> {
    let extend_span = location_to_span(&extend.location, &tree.source_map);

    // MLS §7.4: Validate break names exist in base class
    let base_class_name = extend.base_name.to_string();
    for break_name in &extend.break_names {
        // Check if break name exists as a component or nested class
        let exists_as_component = class.components.contains_key(break_name);
        let exists_as_class = class.classes.contains_key(break_name);
        if !exists_as_component && !exists_as_class {
            return Err(Box::new(InstantiateError::invalid_break_name(
                break_name,
                &base_class_name,
                extend_span,
            )));
        }
    }

    // MLS §7.2: Collect value modifications (non-redeclare) from extends clause
    // These override default bindings in inherited components, e.g., extends Foo(n=2)
    let value_modifications: IndexMap<String, ast::Expression> = extend
        .modifications
        .iter()
        .filter_map(|m| try_extract_value_modification(m, class))
        .collect();

    // MLS §7.3: Validate redeclarations and collect type changes
    let redeclare_types = collect_redeclarations(tree, class, extend, extend_span)?;

    // Merge components
    for (name, comp) in &class.components {
        // Check if this component is deselected via `break`
        if extend.break_names.contains(name) {
            continue;
        }

        if let Some(existing) = target.components.get(name) {
            // MLS §5.6: Check if components are from same origin or have compatible types
            if !components_are_compatible(existing, comp) {
                return Err(Box::new(InstantiateError::conflicting_inheritance(
                    name.clone(),
                    "previous base",
                    extend.base_name.to_string(),
                    location_to_span(&extend.location, &tree.source_map),
                )));
            }
            // Compatible - diamond inheritance is OK, keep existing
        } else {
            let mut inherited_comp = comp.clone();
            apply_protected_visibility(&mut inherited_comp, extend.is_protected);
            target.components.insert(name.clone(), inherited_comp);
        }
    }

    // MLS §7.3: Apply redeclared types to inherited components
    // This updates the component's type so that instantiation uses the new type's fields
    for (comp_name, new_type_name) in &redeclare_types {
        if let Some(comp) = target.components.get_mut(comp_name) {
            // Update the type_name to the new type
            comp.type_name = rumoca_ir_ast::Name {
                name: new_type_name
                    .split('.')
                    .map(|part| rumoca_ir_ast::Token {
                        text: std::sync::Arc::from(part),
                        location: rumoca_ir_ast::Location::default(),
                        token_number: 0,
                        token_type: 0,
                    })
                    .collect(),
                def_id: None,
            };
            // Update type_def_id by looking up the new type in the tree
            comp.type_def_id = tree.name_map.get(new_type_name).copied().or_else(|| {
                // Try with shorter name (last segment) for unqualified lookups
                let short_name = new_type_name.rsplit('.').next().unwrap_or(new_type_name);
                tree.name_map.get(short_name).copied()
            });

            // MLS §7.3.2: Activate constraining-clause defaults for redeclared
            // replaceable components.
            activate_constrainedby_defaults_for_redeclare(comp);
        }
    }

    // MLS §7.2: Apply value modifications to inherited components
    // This overrides default bindings, e.g., extends MIMOs(final n=2) sets n=2
    for (comp_name, new_value) in value_modifications {
        if let Some(comp) = target.components.get_mut(&comp_name) {
            // Override the binding with the new value from extends modification
            // Must update BOTH start and binding fields:
            // - start: used for parameter default values and attribute evaluation
            // - binding: used by extract_binding() for component instantiation
            comp.start = new_value.clone();
            comp.binding = Some(new_value);
            comp.has_explicit_binding = true;
        }
    }

    // MLS §7.2: Merge nested class modifications into inherited components
    // Handles extends clauses like: extends Foo(friction(useHeatPort=true))
    // where the modification targets a sub-parameter of an inherited component
    merge_nested_extends_modifications(target, extend);

    // Merge equations
    target.equations.extend(class.equations.clone());
    target
        .initial_equations
        .extend(class.initial_equations.clone());

    // Merge algorithms
    target.algorithms.extend(class.algorithms.clone());
    target
        .initial_algorithms
        .extend(class.initial_algorithms.clone());

    // Merge nested classes
    for (name, nested) in &class.classes {
        if !target.classes.contains_key(name) {
            let mut inherited_class = nested.clone();
            apply_protected_class_visibility(&mut inherited_class, extend.is_protected);
            target.classes.insert(name.clone(), inherited_class);
        }
    }

    Ok(())
}

fn activate_constrainedby_defaults_for_redeclare(comp: &mut ast::Component) {
    let mut inserts: Vec<(String, ast::Expression)> = Vec::new();
    let mut prefixed_keys: Vec<String> = Vec::new();

    for (key, value) in &comp.modifications {
        let Some(target_name) = key.strip_prefix(CONSTRAINEDBY_MOD_PREFIX) else {
            continue;
        };
        prefixed_keys.push(key.clone());
        if comp.modifications.contains_key(target_name) {
            continue;
        }
        inserts.push((target_name.to_string(), value.clone()));
    }

    for (target_name, value) in inserts {
        comp.modifications.insert(target_name.clone(), value);
        let prefixed_key = format!("{CONSTRAINEDBY_MOD_PREFIX}{target_name}");
        if comp.each_modifications.contains(&prefixed_key) {
            comp.each_modifications.insert(target_name.clone());
        }
        if comp.final_attributes.contains(&prefixed_key) {
            comp.final_attributes.insert(target_name.clone());
        }
    }

    for key in prefixed_keys {
        comp.modifications.shift_remove(&key);
        comp.each_modifications.remove(&key);
        comp.final_attributes.remove(&key);
    }
}

/// Merge nested class modifications from extends clause into inherited components.
///
/// MLS §7.2: When an extends clause has modifications like
/// `extends Foo(friction(useHeatPort=true))`, the nested modifications should be
/// merged into the inherited `friction` component's `modifications` map. This ensures
/// that when `friction` is later instantiated, the modification `useHeatPort=true`
/// is visible via `shift_modifications_down` and `populate_modification_environment`.
fn merge_nested_extends_modifications(target: &mut InheritedContent, extend: &ast::Extend) {
    for modification in &extend.modifications {
        // Extract target name and nested modifications from the expression.
        // Two formats exist:
        //   1. ClassModification { target: comp_name, modifications: [...] }
        //      For: extends Foo(friction(useHeatPort=true))
        //   2. Modification { target: comp_name, value: ClassModification { target: TypeName, modifications: [...] } }
        //      For: extends Foo(redeclare final NewType comp(nested=val))
        //      Type changes are handled by collect_redeclarations(); here we merge nested mods.
        let (target_name, modifications) = match &modification.expr {
            ast::Expression::ClassModification {
                target: mod_target,
                modifications,
            } => {
                let Some(name) = mod_target.parts.first().map(|p| p.ident.text.to_string()) else {
                    continue;
                };
                (name, modifications.as_slice())
            }
            ast::Expression::Modification {
                target: mod_target,
                value,
            } => {
                let Some(name) = mod_target.parts.first().map(|p| p.ident.text.to_string()) else {
                    continue;
                };
                if let ast::Expression::ClassModification { modifications, .. } = value.as_ref() {
                    (name, modifications.as_slice())
                } else {
                    continue;
                }
            }
            _ => continue,
        };
        let Some(comp) = target.components.get_mut(&target_name) else {
            continue;
        };
        for nested_mod in modifications {
            insert_nested_modification(comp, nested_mod);
        }
    }
}

/// Insert a single nested modification into a component's modifications map.
fn insert_nested_modification(comp: &mut ast::Component, nested_mod: &ast::Expression) {
    match nested_mod {
        ast::Expression::Modification { target: t, value } => {
            if let Some(name) = t.parts.first().map(|p| p.ident.text.to_string()) {
                comp.modifications.insert(name, value.as_ref().clone());
            }
        }
        ast::Expression::NamedArgument { name, value } => {
            comp.modifications
                .insert(name.text.to_string(), value.as_ref().clone());
        }
        ast::Expression::ClassModification { .. } => {
            if let Some(name) = extract_modification_target(nested_mod) {
                comp.modifications.insert(name, nested_mod.clone());
            }
        }
        _ => {}
    }
}

/// Get the effective components for a class (own + inherited).
pub fn get_effective_components(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<IndexMap<String, ast::Component>> {
    let mut cache = InheritanceCache::new();
    get_effective_components_with_cache(tree, class, &mut cache)
}

/// Get the effective components for a class with caching.
pub fn get_effective_components_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<IndexMap<String, ast::Component>> {
    let mut inherited = process_extends_with_cache(tree, class, cache)?;

    // The class's own components override inherited ones
    for (name, comp) in &class.components {
        inherited.components.insert(name.clone(), comp.clone());
    }

    // MLS §7.1/§7.3: local class names (including inherited replaceable classes)
    // are valid type names for component declarations in the effective class scope.
    // Preserve their resolved DefIds so later phases don't treat names like
    // `FlowModel` as undefined global types.
    let local_type_def_ids =
        collect_local_type_def_ids(&inherited.classes, &class.classes, &inherited.components);
    populate_local_component_type_def_ids(&mut inherited.components, &local_type_def_ids);

    Ok(inherited.components)
}

fn collect_local_type_def_ids(
    inherited_classes: &IndexMap<String, ast::ClassDef>,
    own_classes: &IndexMap<String, ast::ClassDef>,
    components: &IndexMap<String, ast::Component>,
) -> IndexMap<String, DefId> {
    let mut local = IndexMap::new();

    for (name, class) in inherited_classes {
        if let Some(def_id) = class.def_id {
            local.insert(name.clone(), def_id);
        }
    }

    for (name, class) in own_classes {
        if let Some(def_id) = class.def_id {
            local.insert(name.clone(), def_id);
        }
    }

    // Components with explicit type_def_id can also anchor short local names
    // during inherited-content synthesis.
    for comp in components.values() {
        if let Some(def_id) = comp.type_def_id
            && let Some(short) = comp.type_name.to_string().rsplit('.').next()
            && !short.is_empty()
        {
            local.entry(short.to_string()).or_insert(def_id);
        }
    }

    local
}

fn populate_local_component_type_def_ids(
    components: &mut IndexMap<String, ast::Component>,
    local_type_def_ids: &IndexMap<String, DefId>,
) {
    for comp in components.values_mut() {
        if comp.type_def_id.is_some() {
            continue;
        }

        let type_name = comp.type_name.to_string();
        if type_name.is_empty() {
            continue;
        }
        let is_dotted = type_name.contains('.');

        // `type_name.def_id` may be a partial first-segment anchor (e.g. `Medium`
        // for `Medium.AbsolutePressure`). Promote it only for short names.
        if let Some(def_id) = comp.type_name.def_id {
            if !is_dotted {
                comp.type_def_id = Some(def_id);
            }
            continue;
        }

        // Dotted names are already scope-qualified or package-member references;
        // this fix only resolves local short names in the effective class scope.
        if is_dotted {
            continue;
        }

        if let Some(def_id) = local_type_def_ids.get(&type_name).copied() {
            comp.type_def_id = Some(def_id);
            comp.type_name.def_id = Some(def_id);
        }
    }
}

/// Get the effective equations for a class (own + inherited).
pub fn get_effective_equations(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<Vec<ast::Equation>> {
    let mut cache = InheritanceCache::new();
    get_effective_equations_with_cache(tree, class, &mut cache)
}

/// Get the effective equations for a class with caching.
pub fn get_effective_equations_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<Vec<ast::Equation>> {
    let mut inherited = process_extends_with_cache(tree, class, cache)?;
    inherited.equations.extend(class.equations.clone());
    Ok(inherited.equations)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    /// Create a minimal component for testing.
    fn make_component(name: &str, is_replaceable: bool, is_final: bool) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            is_replaceable,
            is_final,
            ..Default::default()
        }
    }

    /// Create a component reference for testing.
    fn make_component_ref(name: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: ast::Token {
                    text: std::sync::Arc::from(name),
                    location: ast::Location::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
            }],
            def_id: None,
        }
    }

    /// Create a token for testing.
    fn make_token(text: &str) -> ast::Token {
        ast::Token {
            text: std::sync::Arc::from(text),
            location: ast::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    /// Create a Name for testing.
    fn make_name(text: &str) -> rumoca_ir_ast::Name {
        rumoca_ir_ast::Name {
            name: vec![make_token(text)],
            def_id: None,
        }
    }

    #[test]
    fn test_validate_redeclaration_non_replaceable() {
        // A non-replaceable component should fail redeclaration
        let tree = ast::ClassTree::default();
        let comp = make_component("x", false, false);
        let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("not replaceable"));
    }

    #[test]
    fn test_validate_redeclaration_final() {
        // A final component should fail redeclaration (even if replaceable)
        let tree = ast::ClassTree::default();
        let comp = make_component("x", true, true);
        let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("final"));
    }

    #[test]
    fn test_validate_redeclaration_replaceable() {
        // A replaceable, non-final component should succeed
        let tree = ast::ClassTree::default();
        let comp = make_component("x", true, false);
        let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
        assert!(result.is_ok());
    }

    // -------------------------------------------------------------------------
    // Constrainedby validation tests (MLS §7.3.2)
    // -------------------------------------------------------------------------

    /// Create a component with constrainedby for testing.
    fn make_constrained_component(
        name: &str,
        type_name: &str,
        constrainedby: Option<&str>,
    ) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            type_name: make_name(type_name),
            is_replaceable: true,
            is_final: false,
            constrainedby: constrainedby.map(make_name),
            ..Default::default()
        }
    }

    #[test]
    fn test_constrainedby_exact_match() {
        // Redeclaring with exact same type as constraint should succeed
        let tree = ast::ClassTree::default();
        let comp = make_constrained_component("x", "Real", Some("Real"));
        let result = validate_redeclaration(&tree, &comp, "x", Some("Real"), Span::DUMMY);
        assert!(result.is_ok());
    }

    #[test]
    fn test_constrainedby_violation_builtin() {
        // Redeclaring Real constrained component to Integer should fail
        let tree = ast::ClassTree::default();
        let comp = make_constrained_component("x", "Real", Some("Real"));
        let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("violates constrainedby"));
    }

    #[test]
    fn test_constrainedby_default_uses_original_type() {
        // When no constrainedby is specified, the original type is the constraint
        let tree = ast::ClassTree::default();
        let comp = make_constrained_component("x", "Real", None);
        // Redeclaring to Integer should fail (Real is implicit constraint)
        let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
        assert!(result.is_err());
    }

    #[test]
    fn test_constrainedby_subtype_allowed() {
        // Redeclaring to a subtype of the constraint should succeed

        let mut tree = ast::ClassTree::default();

        // Create base class
        let base = ast::ClassDef {
            name: make_token("BaseConnector"),
            ..Default::default()
        };

        // Create derived class that extends base
        let derived = ast::ClassDef {
            name: make_token("DerivedConnector"),
            extends: vec![ast::Extend {
                base_name: make_name("BaseConnector"),
                ..Default::default()
            }],
            ..Default::default()
        };

        tree.definitions
            .classes
            .insert("BaseConnector".to_string(), base);
        tree.definitions
            .classes
            .insert("DerivedConnector".to_string(), derived);

        // ast::Component constrained to BaseConnector
        let comp = make_constrained_component("c", "BaseConnector", Some("BaseConnector"));

        // Redeclaring to DerivedConnector (a subtype) should succeed
        let result =
            validate_redeclaration(&tree, &comp, "c", Some("DerivedConnector"), Span::DUMMY);
        assert!(result.is_ok());
    }

    #[test]
    fn test_constrainedby_non_subtype_rejected() {
        // Redeclaring to a non-subtype should fail
        let mut tree = ast::ClassTree::default();

        // Create two unrelated classes
        let class_a = ast::ClassDef {
            name: make_token("ClassA"),
            ..Default::default()
        };
        let class_b = ast::ClassDef {
            name: make_token("ClassB"),
            ..Default::default()
        };

        tree.definitions
            .classes
            .insert("ClassA".to_string(), class_a);
        tree.definitions
            .classes
            .insert("ClassB".to_string(), class_b);

        // ast::Component constrained to ClassA
        let comp = make_constrained_component("c", "ClassA", Some("ClassA"));

        // Redeclaring to ClassB (not a subtype) should fail
        let result = validate_redeclaration(&tree, &comp, "c", Some("ClassB"), Span::DUMMY);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("violates constrainedby"));
    }

    // -------------------------------------------------------------------------
    // is_type_subtype tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_is_type_subtype_exact_match() {
        let tree = ast::ClassTree::default();
        assert!(is_type_subtype(&tree, "Real", "Real"));
        assert!(is_type_subtype(&tree, "MyClass", "MyClass"));
    }

    #[test]
    fn test_is_type_subtype_builtin_mismatch() {
        let tree = ast::ClassTree::default();
        assert!(!is_type_subtype(&tree, "Real", "Integer"));
        assert!(!is_type_subtype(&tree, "Boolean", "String"));
    }

    #[test]
    fn test_is_type_subtype_via_extends() {
        let mut tree = ast::ClassTree::default();

        // A extends nothing
        let class_a = ast::ClassDef {
            name: make_token("A"),
            ..Default::default()
        };

        // B extends A
        let class_b = ast::ClassDef {
            name: make_token("B"),
            extends: vec![ast::Extend {
                base_name: make_name("A"),
                ..Default::default()
            }],
            ..Default::default()
        };

        // C extends B (transitive: C -> B -> A)
        let class_c = ast::ClassDef {
            name: make_token("C"),
            extends: vec![ast::Extend {
                base_name: make_name("B"),
                ..Default::default()
            }],
            ..Default::default()
        };

        tree.definitions.classes.insert("A".to_string(), class_a);
        tree.definitions.classes.insert("B".to_string(), class_b);
        tree.definitions.classes.insert("C".to_string(), class_c);

        // B is subtype of A
        assert!(is_type_subtype(&tree, "B", "A"));
        // C is subtype of B
        assert!(is_type_subtype(&tree, "C", "B"));
        // C is subtype of A (transitive)
        assert!(is_type_subtype(&tree, "C", "A"));
        // A is NOT subtype of B
        assert!(!is_type_subtype(&tree, "A", "B"));
    }

    #[test]
    fn test_class_extends_cached_matches_base_def_id_for_relative_extends_name() {
        use rumoca_core::DefId;

        let mut tree = ast::ClassTree::default();

        let c_id = DefId::new(1);
        let interfaces_id = DefId::new(2);
        let d_id = DefId::new(3);
        let pkg_id = DefId::new(4);
        let root_id = DefId::new(5);

        let class_c = ast::ClassDef {
            def_id: Some(c_id),
            name: make_token("C"),
            ..Default::default()
        };

        let mut class_interfaces = ast::ClassDef {
            def_id: Some(interfaces_id),
            name: make_token("Interfaces"),
            class_type: ast::ClassType::Package,
            ..Default::default()
        };
        class_interfaces
            .classes
            .insert("C".to_string(), class_c.clone());

        let class_d = ast::ClassDef {
            def_id: Some(d_id),
            name: make_token("D"),
            extends: vec![ast::Extend {
                // Relative name intentionally omits top-level prefix.
                base_name: make_name("Pkg.Interfaces.C"),
                base_def_id: Some(c_id),
                ..Default::default()
            }],
            ..Default::default()
        };

        let mut class_pkg = ast::ClassDef {
            def_id: Some(pkg_id),
            name: make_token("Pkg"),
            class_type: ast::ClassType::Package,
            ..Default::default()
        };
        class_pkg
            .classes
            .insert("Interfaces".to_string(), class_interfaces);
        class_pkg.classes.insert("D".to_string(), class_d);

        let mut class_root = ast::ClassDef {
            def_id: Some(root_id),
            name: make_token("Root"),
            class_type: ast::ClassType::Package,
            ..Default::default()
        };
        class_root.classes.insert("Pkg".to_string(), class_pkg);
        tree.definitions
            .classes
            .insert("Root".to_string(), class_root);

        tree.def_map
            .insert(c_id, "Root.Pkg.Interfaces.C".to_string());
        tree.def_map
            .insert(interfaces_id, "Root.Pkg.Interfaces".to_string());
        tree.def_map.insert(d_id, "Root.Pkg.D".to_string());
        tree.def_map.insert(pkg_id, "Root.Pkg".to_string());
        tree.def_map.insert(root_id, "Root".to_string());

        tree.name_map
            .insert("Root.Pkg.Interfaces.C".to_string(), c_id);
        tree.name_map
            .insert("Root.Pkg.Interfaces".to_string(), interfaces_id);
        tree.name_map.insert("Root.Pkg.D".to_string(), d_id);
        tree.name_map.insert("Root.Pkg".to_string(), pkg_id);
        tree.name_map.insert("Root".to_string(), root_id);

        let d_class = tree
            .get_class_by_qualified_name("Root.Pkg.D")
            .expect("Root.Pkg.D class should exist");
        let mut cache = SubtypeCache::new();
        assert!(
            class_extends_cached(&tree, d_class, "Interfaces.C", &mut cache),
            "relative extends with base_def_id should match short queried supertype"
        );
    }

    #[test]
    fn test_extract_modification_target_modification() {
        // Test extracting target from ast::Expression::Modification
        let expr = ast::Expression::Modification {
            target: make_component_ref("myVar"),
            value: Arc::new(ast::Expression::Empty),
        };
        assert_eq!(
            extract_modification_target(&expr),
            Some("myVar".to_string())
        );
    }

    #[test]
    fn test_extract_modification_target_class_modification() {
        // Test extracting target from ast::Expression::ClassModification
        let expr = ast::Expression::ClassModification {
            target: make_component_ref("myClass"),
            modifications: vec![],
        };
        assert_eq!(
            extract_modification_target(&expr),
            Some("myClass".to_string())
        );
    }

    #[test]
    fn test_extract_modification_target_named_argument() {
        // Test extracting target from ast::Expression::NamedArgument
        let expr = ast::Expression::NamedArgument {
            name: make_token("param"),
            value: Arc::new(ast::Expression::Empty),
        };
        assert_eq!(
            extract_modification_target(&expr),
            Some("param".to_string())
        );
    }

    #[test]
    fn test_is_effectively_primitive_transitive_enumeration_chain() {
        // Test that type alias chains leading to enumerations are detected as primitive
        // type Logic = enumeration(...)
        // connector DigitalSignal = Logic
        // connector DigitalInput = input DigitalSignal

        // Create a tree with these classes
        let mut tree = ast::ClassTree::new();

        // Logic enumeration
        let mut logic = ast::ClassDef {
            name: make_token("Logic"),
            ..Default::default()
        };
        logic.enum_literals.push(ast::EnumLiteral {
            ident: make_token("U"),
            description: vec![],
        });
        logic.enum_literals.push(ast::EnumLiteral {
            ident: make_token("X"),
            description: vec![],
        });

        // DigitalSignal = Logic
        let digital_signal = ast::ClassDef {
            name: make_token("DigitalSignal"),
            extends: vec![ast::Extend {
                base_name: make_name("Logic"),
                ..Default::default()
            }],
            ..Default::default()
        };

        // DigitalInput = input DigitalSignal
        let digital_input = ast::ClassDef {
            name: make_token("DigitalInput"),
            extends: vec![ast::Extend {
                base_name: make_name("DigitalSignal"),
                ..Default::default()
            }],
            ..Default::default()
        };

        tree.definitions.classes.insert("Logic".to_string(), logic);
        tree.definitions
            .classes
            .insert("DigitalSignal".to_string(), digital_signal);
        tree.definitions
            .classes
            .insert("DigitalInput".to_string(), digital_input.clone());

        // DigitalInput should be effectively primitive because it chains to Logic (enumeration)
        assert!(is_effectively_primitive_transitive(&tree, &digital_input));
    }
}
