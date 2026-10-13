//! Canonical serialization of a flat model.
//!
//! `DefId`s and `TypeId`s are local to the compilation unit that allocated
//! them (SPEC_0001): the same source compiled in a session that also holds
//! unrelated classes receives different numbers. The canonical form replaces
//! every such identity by its order of first appearance, so two flat models of
//! the same target serialize to identical bytes exactly when they are the same
//! model. Instance identities are allocated per model and are kept as is.

use std::collections::HashMap;

use serde_json::{Map, Value};

use crate::Model;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Domain {
    Plain,
    Def,
    Type,
}

/// Field names whose numeric value is a `DefId`.
fn is_def_field(name: &str) -> bool {
    name.ends_with("def_id") || name.ends_with("declaration")
}

/// Field names whose numeric value is a `TypeId`.
fn is_type_field(name: &str) -> bool {
    matches!(
        name,
        "type_id" | "effective_type_id" | "nominal_type" | "canonical_type"
    )
}

#[derive(Default)]
struct Numbering {
    defs: HashMap<u64, u64>,
    types: HashMap<u64, u64>,
}

impl Numbering {
    fn number(&mut self, domain: Domain, old: u64) -> u64 {
        let table = match domain {
            Domain::Def => &mut self.defs,
            Domain::Type => &mut self.types,
            Domain::Plain => return old,
        };
        let next = table.len() as u64;
        *table.entry(old).or_insert(next)
    }

    /// Renumber the keys of an identity-keyed table, visiting them in
    /// ascending old-identity order so the assignment depends only on the
    /// relative order of the retained identities.
    fn rekey(&mut self, domain: Domain, table: &mut Map<String, Value>) {
        let mut entries: Vec<(Option<u64>, String, Value)> = std::mem::take(table)
            .into_iter()
            .map(|(key, value)| (key.parse().ok(), key, value))
            .collect();
        entries.sort_by_key(|(old, _, _)| *old);
        for (old, key, value) in entries {
            let key = old.map_or(key, |old| self.number(domain, old).to_string());
            table.insert(key, value);
        }
    }

    fn walk(&mut self, value: &mut Value, domain: Domain) {
        match value {
            Value::Number(number) => {
                if let Some(old) = number.as_u64().filter(|_| domain != Domain::Plain) {
                    *value = self.number(domain, old).into();
                }
            }
            Value::Array(items) => items.iter_mut().for_each(|item| self.walk(item, domain)),
            Value::Object(map) => {
                for (key, child) in map.iter_mut() {
                    let child_domain = match () {
                        _ if domain != Domain::Plain => domain,
                        _ if is_def_field(key) => Domain::Def,
                        _ if is_type_field(key) => Domain::Type,
                        _ => Domain::Plain,
                    };
                    self.walk(child, child_domain);
                }
            }
            _ => {}
        }
    }

    fn table(&mut self, key: &str, value: &mut Value) {
        let (key_domain, value_domain) = match key {
            "type_ids_by_def_id" => (Domain::Def, Domain::Type),
            "record_types" => (Domain::Def, Domain::Plain),
            "effective_types" | "enumeration_declarations" => (Domain::Type, Domain::Plain),
            _ => return self.walk(value, domain_of_field(key)),
        };
        let Some(map) = value.as_object_mut() else {
            return self.walk(value, Domain::Plain);
        };
        self.rekey(key_domain, map);
        map.values_mut()
            .for_each(|child| self.walk(child, value_domain));
    }
}

fn domain_of_field(name: &str) -> Domain {
    match name {
        "predefined_types" | "enumeration_types" | "enumeration_type_roots" => Domain::Type,
        "predefined_string_declaration" => Domain::Def,
        _ => Domain::Plain,
    }
}

impl Model {
    /// Serialize with every `DefId` and `TypeId` replaced by its order of
    /// first appearance, independent of the identities the compilation unit
    /// that produced this model happened to allocate.
    pub fn canonical_json(&self) -> Result<String, serde_json::Error> {
        let mut value = serde_json::to_value(self)?;
        let mut numbering = Numbering::default();
        if let Some(fields) = value.as_object_mut() {
            for (key, field) in fields.iter_mut() {
                numbering.table(key, field);
            }
        }
        Ok(value.to_string())
    }
}
