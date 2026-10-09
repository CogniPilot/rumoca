//! Preserve the checked FMI field inventory with borrowed instantiation buffers.

use std::sync::Arc;

use minijinja::Value;
use rumoca_ir_solve as solve;
use serde::Serialize;
use serde::ser::{Error, SerializeMap};

use crate::errors::CodegenError;

#[cfg(test)]
mod tests;

pub(super) fn value(component: Arc<solve::fmi::FmiCCodegenView>) -> Result<Value, CodegenError> {
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
    component: Arc<solve::fmi::FmiCCodegenView>,
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
            None => Value::from_serialize(source),
        };
        self.fields.push((key.to_owned(), value));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.fields.into_iter().collect())
    }
}

fn initial_values(
    component: Arc<solve::fmi::FmiCCodegenView>,
    buffer: solve::fmi::FmiInstantiationBuffer,
) -> Value {
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
    component: Arc<solve::fmi::FmiCCodegenView>,
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
