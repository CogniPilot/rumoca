//! Read-only, demand-driven projection of the routed family catalog.

use std::sync::Arc;

use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use serde_json::Value as Json;

#[derive(Debug)]
struct JsonArray(Arc<[Json]>);

pub(super) fn array_value(entries: Arc<[Json]>) -> Value {
    Value::from_object(JsonArray(entries))
}

impl Object for JsonArray {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.0.len())
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        let json = self.0.get(index)?;
        Some(node_value(Arc::clone(&self.0), index, Vec::new(), json))
    }
}

#[derive(Debug, Clone)]
enum Step {
    Field(String),
    Index(usize),
}

#[derive(Debug)]
struct JsonNode {
    entries: Arc<[Json]>,
    owner: usize,
    path: Vec<Step>,
    repr: ObjectRepr,
}

fn node_value(entries: Arc<[Json]>, owner: usize, path: Vec<Step>, json: &Json) -> Value {
    let repr = match json {
        Json::Array(_) => ObjectRepr::Seq,
        Json::Object(_) => ObjectRepr::Map,
        _ => return Value::from_serialize(json),
    };
    Value::from_object(JsonNode {
        entries,
        owner,
        path,
        repr,
    })
}

impl JsonNode {
    fn json(&self) -> Option<&Json> {
        let mut node = self.entries.get(self.owner)?;
        for step in &self.path {
            node = match step {
                Step::Field(key) => node.get(key)?,
                Step::Index(index) => node.get(*index)?,
            };
        }
        Some(node)
    }
}

impl Object for JsonNode {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        self.repr
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        match self.json() {
            Some(Json::Array(items)) => Enumerator::Seq(items.len()),
            Some(Json::Object(fields)) => Enumerator::Iter(Box::new(
                fields
                    .keys()
                    .cloned()
                    .map(Value::from)
                    .collect::<Vec<_>>()
                    .into_iter(),
            )),
            _ => Enumerator::Empty,
        }
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let (step, child) = match self.json()? {
            Json::Array(items) => {
                let index = key.as_usize()?;
                (Step::Index(index), items.get(index)?)
            }
            Json::Object(fields) => {
                let key = key.as_str()?;
                (Step::Field(key.to_owned()), fields.get(key)?)
            }
            _ => return None,
        };
        let mut path = self.path.clone();
        path.push(step);
        Some(node_value(
            Arc::clone(&self.entries),
            self.owner,
            path,
            child,
        ))
    }
}

#[cfg(test)]
mod tests;
