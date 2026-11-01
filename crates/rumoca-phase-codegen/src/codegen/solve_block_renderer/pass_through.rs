//! #483 size pass: conditional outputs that pass a capture through.
//!
//! A conditional whose two arms both store output `k` from an unchanged load
//! of the same capture `j` leaves `k` equal to that capture on every path. A
//! scalar register is written once, so the C names the captured register for
//! the output (`pass_of`) and drops both arm stores (`passthrough`) and the
//! output's declaration.
//!
//! Repeated stores: once outputs are renamed, a program often stores the very same
//! scalar register into a slot it already holds (the method's status bits
//! after every statement). Such a re-store is flagged `redundant` and dropped.

/// Give every serialized program a `pass_of` table: register -> captured
/// register for a scalar conditional output that both arms pass through
/// unchanged, and flag those arm stores `passthrough`. Runs after the
/// `alias_of` tables exist: a qualifying arm load is one the C already serves
/// by naming the capture, so dropping its store leaves nothing behind.
pub(super) fn mark_pass_through(json: &mut serde_json::Value) {
    match json {
        serde_json::Value::Object(fields) => {
            for value in fields.values_mut() {
                mark_pass_through(value);
            }
            if let Some(pass) = program_pass_through(fields) {
                mark_redundant_stores(fields, &pass);
                fields.insert("pass_of".to_owned(), pass);
            }
        }
        serde_json::Value::Array(items) => items.iter_mut().for_each(mark_pass_through),
        _ => {}
    }
}

fn program_pass_through(
    program: &mut serde_json::Map<String, serde_json::Value>,
) -> Option<serde_json::Value> {
    let register_types = program.get("register_types")?.as_array()?.clone();
    let unserved = |register: usize| {
        ["alias_of", "forward_to", "same_as", "pool_of", "inline_of"]
            .iter()
            .all(|table| {
                program
                    .get(*table)
                    .and_then(|table| table.get(register))
                    .is_none_or(serde_json::Value::is_null)
            })
    };
    let scalar = |register: usize| {
        register_types
            .get(register)
            .is_some_and(|ty| ty["dimensions"].as_array().is_none_or(Vec::is_empty))
    };
    let mut pass = vec![serde_json::Value::Null; register_types.len()];
    let mut flagged = Vec::new();
    let operations = program.get("operations")?.as_array()?;
    for (index, issued) in operations.iter().enumerate() {
        let operation = &issued["operation"];
        if operation["operation"] != "conditional" {
            continue;
        }
        let (Some(captures), Some(destinations)) = (
            registers(&operation["captures"]),
            registers(&operation["destinations"]),
        ) else {
            continue;
        };
        let arms = [
            arm_pass_through(&operation["if_true"]["body"], destinations.len()),
            arm_pass_through(&operation["if_false"]["body"], destinations.len()),
        ];
        for (k, destination) in destinations.iter().copied().enumerate() {
            let (Some((j, true_store)), Some((other, false_store))) = (arms[0][k], arms[1][k])
            else {
                continue;
            };
            let Some(capture) = captures.get(j).copied() else {
                continue;
            };
            if j == other
                && scalar(destination)
                && register_types.get(capture) == register_types.get(destination)
                && unserved(destination)
            {
                // Name the chain's root: a capture passed through an earlier
                // conditional is already named for its own root, so `rn`
                // resolves every output in one step (EC002).
                let root = pass[capture].as_u64().unwrap_or(capture as u64);
                pass[destination] = serde_json::Value::from(root);
                flagged.push((index, "if_true", true_store));
                flagged.push((index, "if_false", false_store));
            }
        }
    }
    let operations = program.get_mut("operations")?.as_array_mut()?;
    for (index, arm, store) in flagged {
        if let Some(store) = operations[index]["operation"][arm]["body"]["operations"]
            .get_mut(store)
            .and_then(|issued| issued["operation"].as_object_mut())
        {
            store.insert("passthrough".to_owned(), serde_json::Value::Bool(true));
        }
    }
    Some(serde_json::Value::Array(pass))
}

/// Flag `redundant` every top-level store of a scalar register into a slot
/// whose latest top-level store wrote the same register (after `pass`
/// renaming). Registers are written once and scalars are never pooled; nested
/// regions store only their own slots and a call writes only its destination
/// registers, so the slot still holds that register's value and the store is
/// a no-op, also for any fault in between.
fn mark_redundant_stores(
    program: &mut serde_json::Map<String, serde_json::Value>,
    pass: &serde_json::Value,
) {
    let scalar = |register: u64| {
        program
            .get("register_types")
            .and_then(|types| types.get(usize::try_from(register).ok()?))
            .is_some_and(|ty| ty["dimensions"].as_array().is_none_or(Vec::is_empty))
    };
    let Some(operations) = program.get("operations").and_then(|ops| ops.as_array()) else {
        return;
    };
    let mut held: std::collections::BTreeMap<u64, u64> = std::collections::BTreeMap::new();
    let mut redundant = Vec::new();
    for (index, issued) in operations.iter().enumerate() {
        let store = &issued["operation"];
        if store["operation"] != "store" || store["passthrough"] == true {
            continue;
        }
        let (Some(slot), Some(source)) = (store["slot"].as_u64(), store["source"].as_u64()) else {
            continue;
        };
        let root = usize::try_from(source)
            .ok()
            .and_then(|source| pass.get(source))
            .and_then(serde_json::Value::as_u64)
            .unwrap_or(source);
        if !scalar(source) {
            held.remove(&slot);
        } else if held.insert(slot, root) == Some(root) {
            redundant.push(index);
        }
    }
    if let Some(operations) = program
        .get_mut("operations")
        .and_then(|ops| ops.as_array_mut())
    {
        for index in redundant {
            if let Some(store) = operations[index]["operation"].as_object_mut() {
                store.insert("redundant".to_owned(), serde_json::Value::Bool(true));
            }
        }
    }
}

fn registers(value: &serde_json::Value) -> Option<Vec<usize>> {
    value
        .as_array()?
        .iter()
        .map(|id| id.as_u64().and_then(|id| usize::try_from(id).ok()))
        .collect()
}

/// Per output position `k` of an arm body: `(j, store)` when the arm's only
/// store to output `k` is operation `store`, of a register the arm loads from
/// input position `j` and the C serves by naming that input (`alias_of`).
/// One indexed pass over the body: slot positions and each register's
/// defining load are indexed once.
fn arm_pass_through(body: &serde_json::Value, outputs: usize) -> Vec<Option<(usize, usize)>> {
    let mut found = vec![None; outputs];
    let mut stores = vec![0usize; outputs];
    let (Some(slots), Some(operations)) = (body["slots"].as_array(), body["operations"].as_array())
    else {
        return found;
    };
    // slot id -> (storage, position among the slots of that storage).
    let mut counts = std::collections::BTreeMap::<String, usize>::new();
    let mut positions = std::collections::BTreeMap::<u64, (String, usize)>::new();
    let mut ordered: Vec<&serde_json::Value> = slots.iter().collect();
    ordered.sort_by_key(|slot| slot["id"].as_u64());
    for slot in ordered {
        let (Some(id), Some(storage)) = (slot["id"].as_u64(), slot["storage"].as_str()) else {
            continue;
        };
        let next = counts.entry(storage.to_owned()).or_default();
        positions.insert(id, (storage.to_owned(), *next));
        *next += 1;
    }
    let position = |slot: &serde_json::Value, storage: &str| {
        positions
            .get(&slot.as_u64()?)
            .filter(|(declared, _)| declared == storage)
            .map(|(_, position)| *position)
    };
    // register -> (defining load's index, loaded slot).
    let loads: std::collections::BTreeMap<u64, (usize, &serde_json::Value)> = operations
        .iter()
        .enumerate()
        .filter(|(_, issued)| issued["operation"]["operation"] == "load")
        .filter_map(|(index, issued)| {
            let load = &issued["operation"];
            Some((load["destination"].as_u64()?, (index, &load["slot"])))
        })
        .collect();
    for (index, issued) in operations.iter().enumerate() {
        let store = &issued["operation"];
        if store["operation"] != "store" {
            continue;
        }
        let Some(k) = position(&store["slot"], "output").filter(|k| *k < outputs) else {
            continue;
        };
        stores[k] += 1;
        let source = store["source"].as_u64();
        let loaded = source
            .and_then(|register| loads.get(&register))
            .filter(|(load_index, _)| *load_index < index)
            .map(|(_, slot)| *slot);
        let served = source
            .and_then(|register| usize::try_from(register).ok())
            .and_then(|register| body["alias_of"].get(register))
            .is_some_and(|alias| !alias.is_null());
        found[k] = loaded
            .and_then(|slot| position(slot, "input"))
            .filter(|_| served)
            .map(|j| (j, index));
    }
    for (k, count) in stores.iter().enumerate() {
        if *count != 1 {
            found[k] = None;
        }
    }
    found
}

#[cfg(test)]
mod tests {
    use serde_json::{Value, json};

    use super::mark_pass_through;

    fn boolean() -> Value {
        json!({ "scalar": { "kind": "boolean" }, "dimensions": [] })
    }

    fn op(operation: Value) -> Value {
        json!({ "operation": operation })
    }

    /// An arm over inputs (slots 0, 1) and outputs (slots 2, 3) whose
    /// operations are `ops`; `aliased` registers are served by naming their
    /// loaded input.
    fn arm(ops: Vec<Value>, aliased: &[(usize, u64)]) -> Value {
        let mut alias_of = vec![Value::Null; 4];
        for (register, slot) in aliased {
            alias_of[*register] = json!(slot);
        }
        json!({
            "slots": [
                { "id": 0, "storage": "input" }, { "id": 1, "storage": "input" },
                { "id": 2, "storage": "output" }, { "id": 3, "storage": "output" },
            ],
            "register_types": [boolean(), boolean(), boolean(), boolean()],
            "alias_of": alias_of,
            "operations": ops.into_iter().map(op).collect::<Vec<_>>(),
        })
    }

    fn load(destination: u64, slot: u64) -> Value {
        json!({ "operation": "load", "destination": destination, "slot": slot })
    }

    fn store(slot: u64, source: u64) -> Value {
        json!({ "operation": "store", "slot": slot, "source": source })
    }

    fn parent(if_true: Value, if_false: Value, types: Vec<Value>) -> Value {
        json!({
            "register_types": types,
            "operations": [op(json!({
                "operation": "conditional", "condition": 4,
                "captures": [0, 1], "destinations": [2, 3],
                "if_true": { "body": if_true }, "if_false": { "body": if_false },
            }))],
        })
    }

    fn passed(program: &Value) -> Vec<Option<u64>> {
        program["pass_of"]
            .as_array()
            .unwrap()
            .iter()
            .map(Value::as_u64)
            .collect()
    }

    fn flagged(program: &Value, arm: &str) -> Vec<bool> {
        program["operations"][0]["operation"][arm]["body"]["operations"]
            .as_array()
            .unwrap()
            .iter()
            .map(|issued| issued["operation"]["passthrough"] == true)
            .collect()
    }

    #[test]
    fn an_output_both_arms_copy_from_one_capture_names_that_capture() {
        let same = arm(
            vec![load(0, 0), store(2, 0), load(1, 1), store(3, 1)],
            &[(0, 0), (1, 1)],
        );
        let changed = arm(
            vec![
                load(0, 0),
                store(2, 0),
                json!({ "operation": "constant", "destination": 1 }),
                store(3, 1),
            ],
            &[(0, 0)],
        );
        let mut program = parent(same, changed, vec![boolean(); 5]);
        mark_pass_through(&mut program);
        assert_eq!(passed(&program), vec![None, None, Some(0), None, None]);
        assert_eq!(
            flagged(&program, "if_true"),
            vec![false, true, false, false]
        );
        assert_eq!(
            flagged(&program, "if_false"),
            vec![false, true, false, false]
        );
    }

    #[test]
    fn different_captures_unaliased_loads_and_aggregates_keep_their_output() {
        let swapped = |first: u64| {
            arm(
                vec![load(0, first), store(2, 0), load(1, 1 - first), store(3, 1)],
                &[(0, first), (1, 1 - first)],
            )
        };
        let mut program = parent(swapped(0), swapped(1), vec![boolean(); 5]);
        mark_pass_through(&mut program);
        assert!(
            passed(&program).iter().all(Option::is_none),
            "arms disagree on the capture"
        );

        let unaliased = arm(vec![load(0, 0), store(2, 0), load(1, 1), store(3, 1)], &[]);
        let mut program = parent(unaliased.clone(), unaliased, vec![boolean(); 5]);
        mark_pass_through(&mut program);
        assert!(
            passed(&program).iter().all(Option::is_none),
            "an unaliased load prints a copy"
        );

        let both = arm(
            vec![load(0, 0), store(2, 0), load(1, 1), store(3, 1)],
            &[(0, 0), (1, 1)],
        );
        let vector = json!({ "scalar": { "kind": "real" }, "dimensions": [3] });
        let mut program = parent(both.clone(), both, vec![vector; 5]);
        mark_pass_through(&mut program);
        assert!(
            passed(&program).iter().all(Option::is_none),
            "only scalars are renamed"
        );
    }

    #[test]
    fn a_chain_of_pass_throughs_names_its_root() {
        let copy = || arm(vec![load(0, 0), store(2, 0)], &[(0, 0)]);
        let conditional = |captures: [u64; 2], destinations: [u64; 2]| {
            op(json!({
                "operation": "conditional", "condition": 9,
                "captures": captures, "destinations": destinations,
                "if_true": { "body": copy() }, "if_false": { "body": copy() },
            }))
        };
        let mut program = json!({
            "register_types": vec![boolean(); 10],
            "operations": [conditional([0, 1], [2, 3]), conditional([2, 1], [4, 5]), conditional([4, 1], [6, 7])],
        });
        mark_pass_through(&mut program);
        let passed = passed(&program);
        assert_eq!(
            (passed[2], passed[4], passed[6]),
            (Some(0), Some(0), Some(0))
        );
    }

    #[test]
    fn a_store_of_the_register_a_slot_already_holds_is_redundant() {
        let mut program = json!({
            "register_types": vec![boolean(); 3],
            "operations": [op(store(7, 0)), op(store(7, 0)), op(store(7, 1)), op(store(7, 0)), op(store(8, 0))],
        });
        mark_pass_through(&mut program);
        let redundant: Vec<bool> = program["operations"]
            .as_array()
            .unwrap()
            .iter()
            .map(|issued| issued["operation"]["redundant"] == true)
            .collect();
        assert_eq!(redundant, vec![false, true, false, false, false]);
    }

    #[test]
    fn a_second_store_to_the_output_keeps_it() {
        let twice = arm(
            vec![
                load(0, 0),
                store(2, 0),
                load(1, 1),
                store(2, 1),
                store(3, 1),
            ],
            &[(0, 0), (1, 1)],
        );
        let mut program = parent(twice.clone(), twice, vec![boolean(); 5]);
        mark_pass_through(&mut program);
        assert_eq!(passed(&program)[2], None);
        assert_eq!(passed(&program)[3], Some(1));
    }
}
