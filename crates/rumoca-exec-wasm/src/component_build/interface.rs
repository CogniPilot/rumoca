//! Retained target WIT owns the contract; decoded components prove conformance.

use rumoca_core::artifact_build::{PreparedBuildRequest, validate_artifact_relative_path};
use std::collections::BTreeSet;
use std::path::Path;
use wit_parser::{Resolve, WorldId, WorldItem, WorldKey};

pub(super) struct ExpectedInterface {
    resolve: Resolve,
    world: WorldId,
    allowed_extra_imports: BTreeSet<String>,
}

impl ExpectedInterface {
    pub(super) fn from_request<S>(request: &PreparedBuildRequest<'_, S>) -> Result<Self, String> {
        let parameters = request.parameters();
        let directory = Path::new(
            parameters
                .get("wit-directory")
                .ok_or("missing retained WIT directory")?,
        );
        validate_artifact_relative_path(directory)?;
        let world = parameters.get("world").ok_or("missing target WIT world")?;
        let mut sources = wit_parser::SourceMap::new();
        let mut count = 0;
        for (path, bytes) in request.files() {
            if path.parent() != Some(directory) || path.extension().is_none_or(|ext| ext != "wit") {
                continue;
            }
            let text =
                std::str::from_utf8(bytes).map_err(|error| format!("retained WIT: {error}"))?;
            sources.push(path, text);
            count += 1;
        }
        if count == 0 {
            return Err("missing retained target WIT files".into());
        }
        let mut resolve = Resolve::default();
        let package = resolve
            .push_group(
                sources
                    .parse()
                    .map_err(|error| format!("parse retained WIT: {error:#}"))?,
            )
            .map_err(|error| format!("resolve retained WIT: {error:#}"))?;
        let world = resolve
            .select_world(&[package], Some(world))
            .map_err(|error| format!("select target WIT world: {error:#}"))?;
        let imports: Vec<String> = serde_json::from_str(
            parameters
                .get("allowed-extra-imports")
                .map(String::as_str)
                .unwrap_or("[]"),
        )
        .map_err(|error| format!("target extra import list: {error}"))?;
        let allowed_extra_imports: BTreeSet<_> = imports.iter().cloned().collect();
        if imports.iter().any(String::is_empty) || imports.len() != allowed_extra_imports.len() {
            return Err("target extra import IDs must be nonempty and unique".into());
        }
        if resolve.worlds[world]
            .imports
            .keys()
            .any(|key| allowed_extra_imports.contains(&resolve.name_world_key(key)))
        {
            return Err("target extra imports cannot replace retained WIT imports".into());
        }
        Ok(Self {
            resolve,
            world,
            allowed_extra_imports,
        })
    }

    pub(super) fn check(mut self, bytes: &[u8]) -> Result<(), String> {
        let wit_component::DecodedWasm::Component(actual, actual_world) =
            wit_component::decode(bytes).map_err(|error| format!("decode component: {error:#}"))?
        else {
            return Err("compiled artifact is not a component".into());
        };
        let expected_imports: BTreeSet<_> = self.resolve.worlds[self.world]
            .imports
            .keys()
            .map(|key| self.resolve.name_world_key(key))
            .collect();
        let mut extras = Vec::new();
        for (key, item) in &actual.worlds[actual_world].imports {
            let name = actual.name_world_key(key);
            if expected_imports.contains(&name) {
                continue;
            }
            let (WorldKey::Interface(key_id), WorldItem::Interface { id, stability }) = (key, item)
            else {
                return Err(format!("undeclared component import: {name}"));
            };
            if key_id != id || !self.allowed_extra_imports.contains(&name) {
                return Err(format!("undeclared component import: {name}"));
            }
            extras.push((*id, stability.clone()));
        }
        let remap = self
            .resolve
            .merge(actual)
            .map_err(|error| format!("merge component interface: {error:#}"))?;
        for (id, stability) in extras {
            let id = remap
                .map_interface(id, None)
                .map_err(|error| format!("map declared extra import: {error:#}"))?;
            self.resolve.worlds[self.world].imports.insert(
                WorldKey::Interface(id),
                WorldItem::Interface { id, stability },
            );
        }
        wit_component::targets(&self.resolve, self.world, bytes)
            .map_err(|error| format!("component does not implement target WIT world: {error:#}"))
    }
}

#[cfg(test)]
mod tests;
