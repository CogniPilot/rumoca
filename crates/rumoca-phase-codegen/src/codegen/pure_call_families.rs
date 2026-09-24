//! Content-addressed families of the issued pure-call owners a C component
//! emits.
//!
//! One Modelica function called with the same argument types from many
//! components issues one owner per call site, and those owners carry the same
//! typed body. Two owners belong to one family exactly when their primal and
//! directional interfaces and bodies serialize identically once source
//! provenance is dropped and every nested call names its callee's family
//! instead of its owner id. Equal serializations render equal C, so each
//! family is emitted once, under the lowest owner id of its members, and every
//! call site (nested calls and scalar-program calls alike) is routed to that
//! symbol.

use std::collections::BTreeMap;

use minijinja::Value;
use rumoca_ir_solve as solve;
use serde_json::Value as Json;

use crate::errors::CodegenError;

/// The C view of one owner (primal or directional), exactly the fields the
/// typed-function templates read.
#[derive(serde::Serialize)]
struct OwnerView<'a> {
    id: solve::SolvePureCallOwnerId,
    inputs: &'a [solve::SolveValueType],
    outputs: &'a [solve::SolvePureCallOutput],
    body: &'a solve::TypedProgram,
}

/// Serialized primal and directional views of one owner.
struct OwnerJson {
    primal: Json,
    directional: Option<Json>,
}

/// The owners one C component emits and the symbol every call site uses.
#[derive(Debug)]
pub(super) struct PureCallFamilies {
    /// Per owner id, the owner id of its family representative.
    symbols: Vec<u32>,
    /// Representative primal owners, in owner order.
    owners: Vec<Json>,
    /// Representative directional owners, in owner order.
    directional: Vec<Json>,
}

impl PureCallFamilies {
    pub(super) fn new(table: &solve::SolvePureCallTable) -> Result<Self, CodegenError> {
        let views = table
            .owners()
            .iter()
            .enumerate()
            .map(|(position, owner)| owner_json(position, owner))
            .collect::<Result<Vec<_>, _>>()?;
        let symbols = representatives(&views)?;
        let mut owners = Vec::new();
        let mut directional = Vec::new();
        for (id, view) in views.into_iter().enumerate() {
            if symbols[id] as usize != id {
                continue;
            }
            owners.push(route_calls(view.primal, &symbols)?);
            if let Some(view) = view.directional {
                directional.push(route_calls(view, &symbols)?);
            }
        }
        Ok(Self {
            symbols,
            owners,
            directional,
        })
    }

    /// `{ owners: [...] }`, the table shape `functions(table, ...)` reads.
    pub(super) fn owners_value(&self) -> Value {
        Value::from_serialize(serde_json::json!({ "owners": self.owners }))
    }

    /// Forward-mode owners of the emitted families that have one, in owner
    /// order. Directional scalar programs (`PureCallDirectional`) evaluate
    /// these bodies with adjacent primal and tangent operands, exactly as the
    /// typed evaluator's directional mode does; nested calls inside them
    /// dispatch to directional owners as well.
    pub(super) fn directional_value(&self) -> Value {
        Value::from_serialize(&self.directional)
    }

    /// Owner id -> emitted symbol id, indexed by `op.owner`.
    pub(super) fn symbols_value(&self) -> Value {
        Value::from_serialize(&self.symbols)
    }

    #[cfg(test)]
    pub(super) fn symbols(&self) -> &[u32] {
        &self.symbols
    }

    #[cfg(test)]
    pub(super) fn owners(&self) -> &[Json] {
        &self.owners
    }
}

fn owner_json(
    position: usize,
    owner: &solve::SolvePureCallOwner,
) -> Result<OwnerJson, CodegenError> {
    if owner.id().index() as usize != position {
        return Err(CodegenError::template(
            "pure-call owner ids are not dense in table order",
        ));
    }
    let primal = to_json(&OwnerView {
        id: owner.id(),
        inputs: owner.inputs(),
        outputs: owner.outputs(),
        body: owner.body(),
    })?;
    let directional = owner
        .directional()
        .map(|directional| {
            to_json(&OwnerView {
                id: owner.id(),
                inputs: directional.inputs(),
                outputs: directional.outputs(),
                body: directional.body(),
            })
        })
        .transpose()?;
    Ok(OwnerJson {
        primal,
        directional,
    })
}

fn to_json(view: &OwnerView<'_>) -> Result<Json, CodegenError> {
    serde_json::to_value(view).map_err(|error| CodegenError::template(error.to_string()))
}

/// Family representative of every owner. An owner calls only owners issued
/// before it, so one pass in owner order sees every callee's family first.
fn representatives(views: &[OwnerJson]) -> Result<Vec<u32>, CodegenError> {
    let mut families: BTreeMap<String, u32> = BTreeMap::new();
    let mut symbols = Vec::with_capacity(views.len());
    for (id, view) in views.iter().enumerate() {
        let key = family_key(id, view, &symbols)?;
        let id = u32::try_from(id)
            .map_err(|_| CodegenError::template("pure-call owner id exceeds u32"))?;
        symbols.push(*families.entry(key).or_insert(id));
    }
    Ok(symbols)
}

/// The owner's content with provenance and its own id dropped and every
/// nested callee named by its (already known) family representative.
fn family_key(id: usize, view: &OwnerJson, known: &[u32]) -> Result<String, CodegenError> {
    let key_of = |json: &Json| -> Result<Json, CodegenError> {
        let mut json = json.clone();
        if let Json::Object(fields) = &mut json {
            fields.remove("id");
        }
        strip_provenance(&mut json);
        rename_calls(&mut json, &mut |callee| {
            known.get(callee).copied().ok_or_else(|| {
                CodegenError::template(format!(
                    "pure-call owner {id} calls owner {callee}, which is not issued before it"
                ))
            })
        })?;
        Ok(json)
    };
    let primal = key_of(&view.primal)?;
    let directional = view.directional.as_ref().map(key_of).transpose()?;
    serde_json::to_string(&(primal, directional))
        .map_err(|error| CodegenError::template(error.to_string()))
}

fn route_calls(mut json: Json, symbols: &[u32]) -> Result<Json, CodegenError> {
    rename_calls(&mut json, &mut |callee| {
        symbols
            .get(callee)
            .copied()
            .ok_or_else(|| CodegenError::template(format!("unknown pure-call owner {callee}")))
    })?;
    Ok(json)
}

fn strip_provenance(json: &mut Json) {
    match json {
        Json::Object(fields) => {
            fields.remove("provenance");
            fields.values_mut().for_each(strip_provenance);
        }
        Json::Array(items) => items.iter_mut().for_each(strip_provenance),
        _ => {}
    }
}

/// Rewrite the callee of every nested `call` operation.
fn rename_calls(
    json: &mut Json,
    rename: &mut impl FnMut(usize) -> Result<u32, CodegenError>,
) -> Result<(), CodegenError> {
    match json {
        Json::Object(fields) => {
            if fields.get("operation").and_then(Json::as_str) == Some("call") {
                let callee = fields
                    .get("owner")
                    .and_then(Json::as_u64)
                    .and_then(|owner| usize::try_from(owner).ok())
                    .ok_or_else(|| CodegenError::template("pure call names no owner"))?;
                fields.insert("owner".to_string(), Json::from(rename(callee)?));
            }
            fields
                .values_mut()
                .try_for_each(|value| rename_calls(value, rename))
        }
        Json::Array(items) => items
            .iter_mut()
            .try_for_each(|value| rename_calls(value, rename)),
        _ => Ok(()),
    }
}

#[cfg(test)]
#[path = "pure_call_families_tests.rs"]
mod tests;
