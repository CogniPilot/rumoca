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

    /// The type of the declaration `def_id` names, such as a package constant
    /// (`Modelica.ComplexMath.j`) not yet injected into the flat model.
    pub(super) fn declared_type(&mut self, def_id: DefId) -> OperandType {
        let Some(component) = self.declaration(def_id) else {
            return OperandType::Other;
        };
        self.component_type(component)
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
        if let Some(owner) = component.type_def_id.and_then(|def| self.owner(def)) {
            return match rank {
                0 => OperandType::Record(owner),
                1 => OperandType::RecordVector(owner),
                _ => OperandType::Other,
            };
        }
        if rank != 0 {
            return OperandType::Other;
        }
        numeric_type_name(declared_type_leaf(&component.type_name))
    }
}

fn declares_operators(class: &ast::ClassDef) -> bool {
    class.classes.keys().any(|name| name.starts_with('\''))
}

/// `Real`, `Integer`, and the `type` aliases of `Real` the MSL declares
/// (`SI.Voltage`) are numeric; `Boolean` and `String` are not. `leaf` is the
/// last identifier of the declared type name.
pub(super) fn numeric_type_name(leaf: &str) -> OperandType {
    match leaf {
        "Integer" => OperandType::Numeric { integer: true },
        "Boolean" | "String" => OperandType::Other,
        _ => OperandType::Numeric { integer: false },
    }
}

/// The last identifier of a declared type name (`Voltage` of `SI.Voltage`).
pub(super) fn declared_type_leaf(name: &ast::Name) -> &str {
    name.name.last().map_or("", |token| &*token.text)
}
