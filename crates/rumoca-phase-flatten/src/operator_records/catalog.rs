//! The operator functions an operator record declares (MLS 3.7 §14.2).
//!
//! An operator record owns its operators as `operator function 'op'` classes
//! (one function) or `operator 'op'` classes (several functions). A record
//! declared by a short class definition, `operator record ComplexVoltage =
//! Complex(...)`, has the operators of its base (MLS §4.6 allows extending an
//! operator record only that way), so every lookup is keyed by the class that
//! actually declares the operators: the record's operator owner.

use std::collections::HashMap;

use rumoca_core::{ClassType, DefId};
use rumoca_ir_ast as ast;

/// The type of one function input or output, or of one operand.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum OperandType {
    /// A scalar value of the operator record owned by the given class.
    Record(DefId),
    /// A vector of that operator record.
    RecordVector(DefId),
    /// A scalar Real or Integer.
    Numeric { integer: bool },
    /// Anything else, including a type this pass does not need to know.
    Other,
}

/// One operator function and its call signature.
#[derive(Clone, Debug)]
pub(super) struct OperatorFunction {
    pub(super) def_id: DefId,
    pub(super) inputs: Vec<OperandType>,
    /// Inputs without a default, which every call must supply.
    pub(super) required: usize,
}

/// Operator records and their operators, discovered on demand.
pub(super) struct OperatorCatalog<'tree> {
    pub(super) classes: &'tree ast::ClassDefIndex<'tree>,
    owners: HashMap<DefId, Option<DefId>>,
    operators: HashMap<(DefId, String), Vec<OperatorFunction>>,
    /// Every component declaration by its `DefId`, built on first use.
    declarations: std::cell::OnceCell<rustc_hash::FxHashMap<DefId, &'tree ast::Component>>,
}

impl<'tree> OperatorCatalog<'tree> {
    pub(super) fn new(classes: &'tree ast::ClassDefIndex<'tree>) -> Self {
        Self {
            classes,
            owners: HashMap::new(),
            operators: HashMap::new(),
            declarations: std::cell::OnceCell::new(),
        }
    }

    /// The class declaring the operators of record class `def_id`, if it is
    /// an operator record.
    pub(super) fn owner(&mut self, def_id: DefId) -> Option<DefId> {
        if let Some(owner) = self.owners.get(&def_id) {
            return *owner;
        }
        let owner = self.find_owner(def_id, 0);
        self.owners.insert(def_id, owner);
        owner
    }

    fn find_owner(&self, def_id: DefId, depth: usize) -> Option<DefId> {
        let class = self.classes.get(def_id)?;
        if !class.operator_record || depth > 16 {
            return None;
        }
        if declares_operators(class) {
            return Some(def_id);
        }
        class
            .extends
            .iter()
            .filter_map(|extend| extend.base_def_id)
            .find_map(|base| self.find_owner(base, depth + 1))
    }

    /// The functions of operator `name` (such as `'+'`) declared by `owner`.
    pub(super) fn functions(&mut self, owner: DefId, name: &str) -> Vec<OperatorFunction> {
        let key = (owner, name.to_string());
        if let Some(functions) = self.operators.get(&key) {
            return functions.clone();
        }
        let functions = self.collect_functions(owner, name);
        self.operators.insert(key, functions.clone());
        functions
    }

    fn collect_functions(&mut self, owner: DefId, name: &str) -> Vec<OperatorFunction> {
        let Some(class) = self.classes.get(owner) else {
            return Vec::new();
        };
        let Some(operator) = class.classes.get(name) else {
            return Vec::new();
        };
        let definitions: Vec<&ast::ClassDef> = if operator.class_type == ClassType::Function {
            vec![operator]
        } else {
            operator
                .classes
                .values()
                .filter(|function| function.class_type == ClassType::Function)
                .collect()
        };
        definitions
            .into_iter()
            .filter_map(|function| self.signature(function))
            .collect()
    }

    fn signature(&mut self, function: &ast::ClassDef) -> Option<OperatorFunction> {
        let def_id = function.def_id?;
        let input_components: Vec<&ast::Component> = function
            .components
            .values()
            .filter(|component| matches!(component.causality, rumoca_core::Causality::Input(_)))
            .collect();
        let has_output = function
            .components
            .values()
            .any(|component| matches!(component.causality, rumoca_core::Causality::Output(_)));
        let required = input_components
            .iter()
            .rposition(|component| component.binding.is_none() && !component.has_explicit_binding)
            .map_or(0, |last| last + 1);
        let inputs = input_components
            .into_iter()
            .map(|component| self.component_type(component))
            .collect();
        has_output.then_some(OperatorFunction {
            def_id,
            inputs,
            required,
        })
    }

    /// The type of the value `reference` denotes, from its declarations: a
    /// package constant not yet injected into the flat model
    /// (`Modelica.ComplexMath.j`), or a field read through an array of
    /// components. Every dimension a part declares and the reference does not
    /// subscript is a dimension of the value (MLS §10.1), so `plug.pin.v` of
    /// `Pin pin[m]` is a vector of `v`'s type.
    pub(super) fn reference_type(
        &mut self,
        reference: &rumoca_core::ComponentReference,
    ) -> OperandType {
        let mut rank = 0usize;
        let mut element = OperandType::Other;
        for part in reference.parts() {
            // A part that names a class (a package prefix) has no dimensions.
            let Some(component) = self.declaration(part.def_id) else {
                continue;
            };
            let declared = component.shape_expr.len().max(component.shape.len());
            if part
                .subs
                .iter()
                .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
                || part.subs.len() > declared
            {
                return OperandType::Other;
            }
            rank += declared - part.subs.len();
            element = self.element_type(component);
        }
        match (element, rank) {
            (OperandType::Record(owner), 1) => OperandType::RecordVector(owner),
            (element, 0) => element,
            _ => OperandType::Other,
        }
    }

    /// The declared record class of the declaration `def_id` names.
    pub(super) fn declared_record(&self, def_id: DefId) -> Option<DefId> {
        self.declaration(def_id)?.type_def_id
    }

    /// Whether declaration `def_id` is a vector declared with no elements
    /// (`C e[0]`), which Flat declares no element records for.
    pub(super) fn declared_empty_vector(&self, def_id: DefId) -> bool {
        self.declaration(def_id)
            .is_some_and(|component| component.shape.as_slice() == [0])
    }

    fn declaration(&self, def_id: DefId) -> Option<&'tree ast::Component> {
        self.declarations
            .get_or_init(|| {
                self.classes
                    .def_ids()
                    .filter_map(|class| self.classes.get(class))
                    .flat_map(|class| class.components.values())
                    .filter_map(|component| Some((component.def_id?, component)))
                    .collect()
            })
            .get(&def_id)
            .copied()
    }

    pub(super) fn component_type(&mut self, component: &ast::Component) -> OperandType {
        let rank = component.shape_expr.len().max(component.shape.len());
        match (self.element_type(component), rank) {
            (OperandType::Record(owner), 1) => OperandType::RecordVector(owner),
            (element, 0) => element,
            _ => OperandType::Other,
        }
    }

    /// The scalar type of one element of `component`, ignoring its dimensions.
    fn element_type(&mut self, component: &ast::Component) -> OperandType {
        let Some(def_id) = component.type_def_id else {
            return numeric_type_name(declared_type_leaf(&component.type_name));
        };
        if let Some(owner) = self.owner(def_id) {
            return OperandType::Record(owner);
        }
        self.type_class_root(def_id, &component.type_name)
    }

    /// The predefined root of a `type` class (`SI.Voltage` is `Real`): Real
    /// and Integer roots are numeric; an enumeration, a non-`type` class, or
    /// a Boolean or String root is not.
    ///
    /// A predefined type has no class definition, so it is known by the name
    /// that declares it.
    fn type_class_root(&self, def_id: DefId, name: &ast::Name) -> OperandType {
        let mut current = def_id;
        let mut current_name = name;
        let mut visited = rustc_hash::FxHashSet::default();
        while visited.insert(current) {
            let Some(class) = self.classes.get(current) else {
                return numeric_type_name(declared_type_leaf(current_name));
            };
            if class.class_type != ClassType::Type || !class.enum_literals.is_empty() {
                return OperandType::Other;
            }
            let [extend] = class.extends.as_slice() else {
                return OperandType::Other;
            };
            current_name = &extend.base_name;
            match extend.base_def_id {
                Some(base) => current = base,
                None => return numeric_type_name(declared_type_leaf(current_name)),
            }
        }
        OperandType::Other
    }
}

fn declares_operators(class: &ast::ClassDef) -> bool {
    class.classes.keys().any(|name| name.starts_with('\''))
}

/// The operand type of a predefined type named by `leaf`, the last identifier
/// of a declared type name that resolves to no class: `Real` and `Integer`
/// are numeric, every other predefined type is not.
pub(super) fn numeric_type_name(leaf: &str) -> OperandType {
    match leaf {
        "Integer" => OperandType::Numeric { integer: true },
        "Real" => OperandType::Numeric { integer: false },
        _ => OperandType::Other,
    }
}

/// The last identifier of a declared type name (`Voltage` of `SI.Voltage`).
pub(super) fn declared_type_leaf(name: &ast::Name) -> &str {
    name.name.last().map_or("", |token| &*token.text)
}
