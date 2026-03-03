//! Type system for the Class Tree (MLS §4).
//!
//! This module provides type definitions and the TypeTable for tracking
//! all types in the compilation unit.

use indexmap::IndexMap;
use rumoca_core::{DefId, TypeId};
use serde::{Deserialize, Serialize};

/// MLS §4: Type definitions
///
/// The TypeTable stores all types encountered during compilation.
/// Types are referenced by TypeId throughout the compiler.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TypeTable {
    /// All types indexed by TypeId.
    types: Vec<Type>,
    /// Map from type names to TypeIds for quick lookup.
    by_name: IndexMap<String, TypeId>,
}

impl TypeTable {
    /// Create a new empty type table.
    pub fn new() -> Self {
        let mut table = Self::default();
        // Register built-in types
        table.register_builtins();
        table
    }

    /// Register built-in Modelica types.
    fn register_builtins(&mut self) {
        // MLS §4.9: Predefined types
        self.add_type(Type::Builtin(BuiltinType::Real));
        self.add_type(Type::Builtin(BuiltinType::Integer));
        self.add_type(Type::Builtin(BuiltinType::Boolean));
        self.add_type(Type::Builtin(BuiltinType::String));
        self.add_type(Type::Builtin(BuiltinType::Clock));

        // MLS §4.9/§8.6: predefined enumeration types used by attributes/builtins.
        self.add_type(Type::Enumeration(EnumerationType {
            name: "StateSelect".to_string(),
            literals: vec![
                "never".to_string(),
                "avoid".to_string(),
                "default".to_string(),
                "prefer".to_string(),
                "always".to_string(),
            ],
        }));
        self.add_type(Type::Enumeration(EnumerationType {
            name: "AssertionLevel".to_string(),
            literals: vec!["warning".to_string(), "error".to_string()],
        }));
    }

    /// Add a type to the table and return its TypeId.
    pub fn add_type(&mut self, ty: Type) -> TypeId {
        let id = TypeId::new(self.types.len() as u32);
        if let Some(name) = ty.name() {
            self.by_name.insert(name.to_string(), id);
        }
        self.types.push(ty);
        id
    }

    /// Get a type by its TypeId.
    pub fn get(&self, id: TypeId) -> Option<&Type> {
        if id.is_unknown() {
            None
        } else {
            self.types.get(id.index() as usize)
        }
    }

    /// Get a mutable reference to a type by its TypeId.
    pub fn get_mut(&mut self, id: TypeId) -> Option<&mut Type> {
        if id.is_unknown() {
            None
        } else {
            self.types.get_mut(id.index() as usize)
        }
    }

    /// Look up a type by name.
    pub fn lookup(&self, name: &str) -> Option<TypeId> {
        self.by_name.get(name).copied()
    }

    /// Get the TypeId for Real.
    pub fn real(&self) -> TypeId {
        self.lookup("Real").unwrap_or(TypeId::UNKNOWN)
    }

    /// Get the TypeId for Integer.
    pub fn integer(&self) -> TypeId {
        self.lookup("Integer").unwrap_or(TypeId::UNKNOWN)
    }

    /// Get the TypeId for Boolean.
    pub fn boolean(&self) -> TypeId {
        self.lookup("Boolean").unwrap_or(TypeId::UNKNOWN)
    }

    /// Get the TypeId for String.
    pub fn string(&self) -> TypeId {
        self.lookup("String").unwrap_or(TypeId::UNKNOWN)
    }

    /// Get the number of types in the table.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Check if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

/// A type in Modelica (MLS §4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    /// Built-in primitive type (Real, Integer, Boolean, String).
    Builtin(BuiltinType),
    /// Class type (model, record, connector, etc.).
    Class(ClassType),
    /// Array type with element type and dimensions.
    Array(ArrayType),
    /// Enumeration type.
    Enumeration(EnumerationType),
    /// Type alias (type X = Y).
    Alias(TypeAlias),
    /// Function type.
    Function(FunctionType),
    /// Unknown/error type (used during type inference).
    Unknown,
}

impl Type {
    /// Get the name of this type, if it has one.
    pub fn name(&self) -> Option<&str> {
        match self {
            Type::Builtin(b) => Some(b.name()),
            Type::Class(c) => Some(&c.name),
            Type::Enumeration(e) => Some(&e.name),
            Type::Alias(a) => Some(&a.name),
            Type::Function(f) => Some(&f.name),
            Type::Array(_) | Type::Unknown => None,
        }
    }

    /// Check if this is a numeric type (Real or Integer).
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Builtin(BuiltinType::Real) | Type::Builtin(BuiltinType::Integer)
        )
    }

    /// Check if this is a scalar type (not array).
    pub fn is_scalar(&self) -> bool {
        !matches!(self, Type::Array(_))
    }
}

/// MLS §4.9: Predefined types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType {
    /// 64-bit floating point.
    Real,
    /// Machine integer.
    Integer,
    /// Boolean (true/false).
    Boolean,
    /// Unicode string.
    String,
    /// Clock domain type (MLS §16.9, Modelica 3.3+ synchronous features).
    Clock,
}

impl BuiltinType {
    /// Get the name of this builtin type.
    pub fn name(&self) -> &'static str {
        match self {
            BuiltinType::Real => "Real",
            BuiltinType::Integer => "Integer",
            BuiltinType::Boolean => "Boolean",
            BuiltinType::String => "String",
            BuiltinType::Clock => "Clock",
        }
    }
}

/// A class type (model, record, connector, etc.).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassType {
    /// The class name.
    pub name: String,
    /// Reference to the class definition.
    pub def_id: DefId,
    /// The kind of class (MLS §4.7).
    pub kind: ClassKind,
}

/// MLS §4.7: Specialized classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClassKind {
    /// General class.
    Class,
    /// Model (equations and components).
    Model,
    /// Block (causal with inputs/outputs).
    Block,
    /// Record (data structure).
    Record,
    /// Connector (for connections).
    Connector,
    /// Type alias.
    Type,
    /// Package (namespace).
    Package,
    /// Function.
    Function,
    /// Operator (for operator overloading).
    Operator,
}

/// An array type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayType {
    /// Element type.
    pub element: TypeId,
    /// Array dimensions (None means unknown size, e.g., from `:` subscript).
    pub dims: Vec<Option<i64>>,
}

impl ArrayType {
    /// Create a new array type.
    pub fn new(element: TypeId, dims: Vec<Option<i64>>) -> Self {
        Self { element, dims }
    }

    /// Get the number of dimensions.
    pub fn ndims(&self) -> usize {
        self.dims.len()
    }

    /// Check if all dimensions are known.
    pub fn has_known_dims(&self) -> bool {
        self.dims.iter().all(|d| d.is_some())
    }
}

/// An enumeration type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumerationType {
    /// The enumeration name.
    pub name: String,
    /// The literal values.
    pub literals: Vec<String>,
}

/// A type alias (type X = Y).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeAlias {
    /// The alias name.
    pub name: String,
    /// The underlying type.
    pub aliased: TypeId,
}

/// A function type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    /// Function name.
    pub name: String,
    /// Input parameter types.
    pub inputs: Vec<(String, TypeId)>,
    /// Output parameter types.
    pub outputs: Vec<(String, TypeId)>,
}

// =============================================================================
// Interface Structure (MLS §6.4)
// =============================================================================

/// MLS §6.4: Interface (type) of a class.
///
/// The interface comprises the information needed for subtype compatibility checking.
/// "The interface of a class is also the interface of the component having that class as type."
///
/// Two classes are compatible if their interfaces are compatible according to the
/// subtype rules in MLS §6.4-6.6.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Interface {
    /// Whether this is transitively non-replaceable (MLS §6.4).
    pub transitively_non_replaceable: bool,

    /// Whether this represents a class (vs component).
    pub is_class: bool,

    /// Specialized class kind (model, record, connector, function, etc.).
    pub class_kind: Option<ClassKind>,

    /// Component prefixes (flow, stream, variability, causality).
    pub prefixes: InterfacePrefixes,

    /// Array dimensions.
    pub dimensions: Vec<Option<i64>>,

    /// Whether this is a conditional component.
    pub conditional: bool,

    /// Whether this is final.
    pub is_final: bool,

    /// Whether this is inner/outer.
    pub inner: bool,
    pub outer: bool,

    /// Public named elements with their interfaces (recursive).
    pub elements: IndexMap<String, InterfaceElement>,

    /// If this is an operator record, the base class identity.
    pub operator_record_base: Option<DefId>,

    /// If derived from ExternalObject, the full name.
    pub external_object_name: Option<String>,

    /// If this is an enumeration, the literal names in order.
    pub enum_literals: Option<Vec<String>>,
}

/// Prefixes that affect interface compatibility (MLS §6.4).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InterfacePrefixes {
    /// Flow prefix.
    pub flow: bool,
    /// Stream prefix.
    pub stream: bool,
    /// Variability (constant, parameter, discrete, continuous).
    pub variability: InterfaceVariability,
    /// Causality (input, output).
    pub causality: InterfaceCausality,
}

/// Variability levels for interface compatibility (MLS §4.5).
///
/// Per MLS §4.5: "constant < parameter < discrete < continuous"
/// This ordering is used for variability constraint checking.
/// A higher variability expression can depend on lower/equal variability expressions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum InterfaceVariability {
    /// Constant (fixed at translation) - most restrictive.
    Constant,
    /// Parameter (fixed after initialization).
    Parameter,
    /// Discrete-time.
    Discrete,
    /// Continuous-time (default for Real) - least restrictive.
    #[default]
    Continuous,
}

/// Causality for interface compatibility (MLS §4.4.2).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum InterfaceCausality {
    /// No causality prefix.
    #[default]
    None,
    /// Input prefix.
    Input,
    /// Output prefix.
    Output,
}

/// An element in an interface (MLS §6.4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterfaceElement {
    /// Name of the element.
    pub name: String,
    /// Whether this is a class element (vs component).
    pub is_class: bool,
    /// The interface of this element.
    pub interface: Box<Interface>,
}
