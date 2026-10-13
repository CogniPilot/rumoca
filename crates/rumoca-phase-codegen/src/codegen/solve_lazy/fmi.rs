//! Preserve the checked FMI field inventory with borrowed instantiation buffers.

use std::sync::Arc;

use minijinja::Value;
use rumoca_ir_solve as solve;
use serde::Serialize;
use serde::ser::{Error, SerializeMap};

use crate::errors::CodegenError;

#[cfg(test)]
mod tests;

enum Inventory {
    C(Arc<solve::fmi::FmiCCodegenView>),
    Native(solve::fmi::FmiNativeDeploymentView),
}

impl Inventory {
    fn variables(&self) -> &[solve::fmi::FmiVariable] {
        match self {
            Self::C(view) => view.variables(),
            Self::Native(view) => view.variables(),
        }
    }
    fn serialize_entries<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
        match self {
            Self::C(view) => view.serialize_entries(entries),
            Self::Native(view) => view.serialize_entries(entries),
        }
    }
    fn serialize_variable_entries<M: SerializeMap>(
        &self,
        index: usize,
        entries: &mut M,
    ) -> Result<(), M::Error> {
        match self {
            Self::C(view) => view.serialize_variable_entries(index, entries),
            Self::Native(view) => view.serialize_variable_entries(index, entries),
        }
    }
    fn instantiation_values(
        &self,
        buffer: solve::fmi::FmiInstantiationBuffer,
    ) -> &solve::SolveInitialValues {
        match self {
            Self::C(view) => view.instantiation_values(buffer),
            Self::Native(view) => view.instantiation_values(buffer),
        }
    }
}

pub(super) fn value(component: Arc<solve::fmi::FmiCCodegenView>) -> Result<Value, CodegenError> {
    inventory_value(Arc::new(Inventory::C(component)))
}

pub(in crate::codegen) fn native_value(
    component: solve::fmi::FmiNativeDeploymentView,
) -> Result<Value, CodegenError> {
    inventory_value(Arc::new(Inventory::Native(component)))
}

fn inventory_value(component: Arc<Inventory>) -> Result<Value, CodegenError> {
    let mut entries = Entries {
        component: Arc::clone(&component),
        fields: Vec::new(),
    };
    component
        .serialize_entries(&mut entries)
        .and_then(|()| entries.end())
        .map_err(|error| CodegenError::template(error.to_string()))
}

struct Entries {
    component: Arc<Inventory>,
    fields: Vec<(String, Value)>,
}

impl SerializeMap for Entries {
    type Ok = Value;
    type Error = serde_json::Error;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
        Err(Self::Error::custom("FMI fields require paired entries"))
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
        Err(Self::Error::custom("FMI fields require paired entries"))
    }

    fn serialize_entry<K: Serialize + ?Sized, V: Serialize + ?Sized>(
        &mut self,
        key: &K,
        source: &V,
    ) -> Result<(), Self::Error> {
        let key = Value::from_serialize(key);
        let key = key
            .as_str()
            .ok_or_else(|| Self::Error::custom("FMI field names must be strings"))?;
        let buffer = solve::fmi::FmiInstantiationBuffer::ALL
            .into_iter()
            .find(|buffer| buffer.name() == key);
        let value = match buffer {
            Some(buffer) => initial_values(Arc::clone(&self.component), buffer),
            None if key == "variables" => variables(Arc::clone(&self.component)),
            None => Value::from_serialize(source),
        };
        self.fields.push((key.to_owned(), value));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.fields.into_iter().collect())
    }
}

fn variables(component: Arc<Inventory>) -> Value {
    super::lazy_seq(component.variables().len(), move |index| {
        let mut entries = VariableEntries {
            component: Arc::clone(&component),
            index,
            fields: Vec::new(),
        };
        component
            .serialize_variable_entries(index, &mut entries)
            .and_then(|()| entries.end())
            .expect("issued checked declaration field inventory")
    })
}

struct VariableEntries {
    component: Arc<Inventory>,
    index: usize,
    fields: Vec<(String, Value)>,
}

impl SerializeMap for VariableEntries {
    type Ok = Value;
    type Error = serde_json::Error;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
        Err(Self::Error::custom("FMI fields require paired entries"))
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
        Err(Self::Error::custom("FMI fields require paired entries"))
    }

    fn serialize_entry<K: Serialize + ?Sized, V: Serialize + ?Sized>(
        &mut self,
        key: &K,
        source: &V,
    ) -> Result<(), Self::Error> {
        let key = Value::from_serialize(key);
        let key = key
            .as_str()
            .ok_or_else(|| Self::Error::custom("FMI field name is not a string"))?;
        let attribute = solve::fmi::FmiNumericAttribute::ALL
            .into_iter()
            .find(|attribute| attribute.name() == key);
        let value = if key == "scalar_names" {
            scalar_names(Arc::clone(&self.component), self.index)
        } else if let Some(attribute) = attribute.filter(|_| {
            self.component.variables()[self.index].value_kind()
                != solve::SolveVariableValueKind::String
        }) {
            numeric_values(Arc::clone(&self.component), self.index, attribute)
        } else {
            Value::from_serialize(source)
        };
        self.fields.push((key.to_owned(), value));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.fields.into_iter().collect())
    }
}

fn scalar_names(component: Arc<Inventory>, index: usize) -> Value {
    super::lazy_seq(component.variables()[index].scalar_count(), move |scalar| {
        Value::from(
            component.variables()[index]
                .scalar_name(scalar)
                .expect("checked scalar name"),
        )
    })
}

fn numeric_values(
    component: Arc<Inventory>,
    index: usize,
    attribute: solve::fmi::FmiNumericAttribute,
) -> Value {
    let Some(count) = component.variables()[index].numeric_attribute_count(attribute) else {
        return Value::from(());
    };
    super::lazy_seq(count, move |scalar| {
        if matches!(
            attribute,
            solve::fmi::FmiNumericAttribute::Start | solve::fmi::FmiNumericAttribute::StartFmi3
        ) {
            return match component.variables()[index]
                .typed_start()
                .expect("issued typed start")
            {
                solve::SolveDeclarationStart::Real(values) => {
                    Value::from(values.value(scalar).expect("checked Real start"))
                }
                solve::SolveDeclarationStart::Integer(values)
                | solve::SolveDeclarationStart::Enumeration(values) => {
                    Value::from(*values.value(scalar).expect("checked Integer start"))
                }
                solve::SolveDeclarationStart::Boolean(values) => {
                    Value::from(*values.value(scalar).expect("checked Boolean start"))
                }
                solve::SolveDeclarationStart::Missing { .. } => Value::from(()),
            };
        }
        Value::from(
            component.variables()[index]
                .numeric_attribute(attribute)
                .expect("retained numeric attribute")
                .value(scalar)
                .expect("checked scalar value"),
        )
    })
}

fn initial_values(component: Arc<Inventory>, buffer: solve::fmi::FmiInstantiationBuffer) -> Value {
    super::lazy_map(&["count", "runs"], move |key| match key {
        "count" => Some(Value::from(component.instantiation_values(buffer).len())),
        "runs" => {
            let component = Arc::clone(&component);
            Some(super::lazy_seq(
                component.instantiation_values(buffer).run_count(),
                move |index| initial_run(Arc::clone(&component), buffer, index),
            ))
        }
        _ => None,
    })
}

fn initial_run(
    component: Arc<Inventory>,
    buffer: solve::fmi::FmiInstantiationBuffer,
    index: usize,
) -> Value {
    let run = component
        .instantiation_values(buffer)
        .run(index)
        .expect("issued run ordinal");
    let keys: &'static [&'static str] = if run.repeated_bits().is_some() {
        &["kind", "start", "count", "bits"]
    } else {
        &["kind", "start", "bits"]
    };
    super::lazy_map(keys, move |key| {
        let run = component.instantiation_values(buffer).run(index)?;
        match key {
            "kind" => Some(Value::from(if run.repeated_bits().is_some() {
                "repeat"
            } else {
                "literal"
            })),
            "start" => Some(Value::from(run.start())),
            "count" => Some(Value::from(run.count())),
            "bits" => match run.repeated_bits() {
                Some(bits) => Some(Value::from(bits)),
                None => {
                    let component = Arc::clone(&component);
                    Some(super::lazy_seq(run.count(), move |ordinal| {
                        Value::from(
                            component
                                .instantiation_values(buffer)
                                .run(index)
                                .expect("retained run owner")
                                .literal_bits()
                                .expect("literal source run")[ordinal],
                        )
                    }))
                }
            },
            _ => None,
        }
    })
}
