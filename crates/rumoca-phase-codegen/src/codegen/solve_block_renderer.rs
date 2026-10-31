//! Template context for embedded C rendered from a refined Solve algorithm
//! block (#483: DAE → GALEC → Solve IR → C).
//!
//! The Algorithm Code view still supplies the header ABI and the C symbol
//! policy, so the block struct, its field names and the method signatures are
//! the ones the GALEC-derived export already ships. The Solve view supplies
//! what the method bodies execute: the block slot table, the three method
//! programs and the function owners.

use std::collections::BTreeMap;

use minijinja::Value;
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveArithmeticProfile, SolvePureCallTable, SolveStorageClass,
    SolveValueType, TypedProgram,
};
use serde::Serialize;

use super::{CodegenError, create_environment};

/// One block slot: its checked name, storage and type, and for a signal
/// status slot its bit in the eFMI `ErrorSignalStatus` word.
#[derive(Serialize)]
struct SlotView<'a> {
    id: usize,
    name: &'a str,
    storage: SolveStorageClass,
    value_type: &'a SolveValueType,
    status_bit: Option<usize>,
}

#[derive(Serialize)]
struct SolveBlockView<'a> {
    arithmetic: SolveArithmeticProfile,
    slots: Vec<SlotView<'a>>,
    /// The checked function each pure-call owner refines, in owner order:
    /// C keeps the model's function names.
    function_names: Vec<&'a str>,
    /// Each function owner's status slots in its body, in signal order
    /// (#483 EL007): a fault stores them to its status outputs.
    status_slots: Vec<Vec<usize>>,
    startup: &'a TypedProgram,
    recalibrate: &'a TypedProgram,
    do_step: &'a TypedProgram,
    functions: &'a SolvePureCallTable,
}

/// One immutable context per target invocation, as for
/// [`super::AlgorithmCodeTemplateRenderer`].
#[derive(Debug)]
pub struct SolveBlockTemplateRenderer {
    context: Value,
}

impl SolveBlockTemplateRenderer {
    /// `block` must be the refinement of `package`.
    pub fn new(
        package: &AlgorithmCodePackage,
        block: &SolveAlgorithmBlock,
        sources: &rumoca_core::SourceMap,
    ) -> Result<Self, CodegenError> {
        let algorithm_code = crate::views::algorithm_code::AlgorithmCodeView::new(package, sources)
            .map_err(CodegenError::template)?;
        Ok(Self {
            context: minijinja::context! {
                algorithm_code => Value::from_serialize(algorithm_code),
                solve_block => solve_block_value(block, sources),
                ir_kind => "solve_algorithm_block",
            },
        })
    }

    /// Render one template source for `model_name`.
    pub fn render_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(minijinja::context! {
            model_name => model_name,
            ..self.context.clone()
        })?)
    }
}

/// The template view of a refined Solve block, with the Modelica trace of
/// every operation's provenance (SPEC_0034 GAL-032) keyed `source:start:end`.
pub(super) fn solve_block_value(
    block: &SolveAlgorithmBlock,
    sources: &rumoca_core::SourceMap,
) -> Value {
    let mut status_bits = 0..;
    let slots = block
        .slots()
        .iter()
        .enumerate()
        .map(|(id, slot)| SlotView {
            id,
            name: slot.source().qualified_name(),
            storage: slot.storage(),
            value_type: slot.value_type(),
            status_bit: (slot.storage() == SolveStorageClass::SignalStatus)
                .then(|| status_bits.next())
                .flatten(),
        })
        .collect();
    let view = SolveBlockView {
        arithmetic: block.arithmetic(),
        slots,
        startup: block.startup().program(),
        recalibrate: block.recalibrate().program(),
        do_step: block.do_step().program(),
        functions: block.functions(),
        status_slots: block
            .functions()
            .owners()
            .iter()
            .map(|owner| {
                block
                    .status_binding(owner.id())
                    .map(|binding| binding.slots().iter().map(|slot| slot.index()).collect())
                    .unwrap_or_default()
            })
            .collect(),
        function_names: block
            .function_sources()
            .iter()
            .map(|source| source.qualified_name())
            .collect(),
    };
    let resolver = crate::views::source_trace::SourceTraceResolver::new(sources);
    let mut traces = BTreeMap::new();
    let mut literals = BTreeMap::new();
    let mut json = serde_json::to_value(&view).unwrap_or_default();
    collect_traces(&json, &resolver, &mut traces);
    collect_literals(&json, &mut literals);
    mark_aliased_loads(&mut json);
    pass_through::mark_pass_through(&mut json);
    for owner in ["/startup", "/recalibrate", "/do_step"] {
        if let Some(program) = json.pointer_mut(owner) {
            allocate_pools(program);
        }
    }
    if let Some(owners) = json
        .pointer_mut("/functions/owners")
        .and_then(serde_json::Value::as_array_mut)
    {
        for owner in owners {
            if let Some(body) = owner.get_mut("body") {
                allocate_pools(body);
            }
        }
    }
    assign_call_levels(&mut json);
    Value::from_serialize(TracedView {
        view: json,
        traces,
        literals,
    })
}

#[derive(Serialize)]
struct TracedView {
    #[serde(flatten)]
    view: serde_json::Value,
    traces: BTreeMap<String, crate::views::source_trace::SourceTrace>,
    /// C literal of every finite or infinite Real constant, keyed
    /// `<kind>:<bits>`: the shortest decimal that reads back to the same bits.
    literals: BTreeMap<String, String>,
}

mod pass_through;

/// Give every serialized program an `alias_of` table: register -> slot for a
/// load the C can serve by reading the slot in place, so the register needs
/// no storage and no copy. A load qualifies when no later operation of its
/// program stores that slot (nested regions store only their own slots, and
/// a call writes only its destination registers) and the register has the
/// slot's exact type, Integer excepted: an `int32_t` field must still widen
/// into the `int64_t` arithmetic.
fn mark_aliased_loads(json: &mut serde_json::Value) {
    match json {
        serde_json::Value::Object(fields) => {
            if let Some(aliases) = program_aliases(fields) {
                fields.insert("alias_of".to_owned(), aliases);
            }
            if let Some(forwards) = program_forwards(fields) {
                fields.insert("forward_to".to_owned(), forwards);
            }
            if let Some(shared) = program_in_place(fields) {
                fields.insert("same_as".to_owned(), shared);
            }
            mark_static_offsets(fields);
            if let Some((unread, loaded)) = program_reads(fields) {
                fields.insert("unread".to_owned(), unread);
                fields.insert("loaded_slots".to_owned(), loaded);
            }
            if let Some(inlined) = program_inlines(fields) {
                fields.insert("inline_of".to_owned(), inlined);
            }
            for value in fields.values_mut() {
                mark_aliased_loads(value);
            }
        }
        serde_json::Value::Array(items) => items.iter_mut().for_each(mark_aliased_loads),
        _ => {}
    }
}

fn program_aliases(
    program: &serde_json::Map<String, serde_json::Value>,
) -> Option<serde_json::Value> {
    let operations = program.get("operations")?.as_array()?;
    let register_types = program.get("register_types")?.as_array()?;
    let slots = program.get("slots")?.as_array()?;
    let mut aliases = vec![serde_json::Value::Null; register_types.len()];
    for (index, issued) in operations.iter().enumerate() {
        let operation = &issued["operation"];
        if operation["operation"] != "load" {
            continue;
        }
        let (Some(slot), Some(register)) = (
            operation["slot"].as_u64(),
            operation["destination"].as_u64(),
        ) else {
            continue;
        };
        let (Ok(slot_index), Ok(register_index)) =
            (usize::try_from(slot), usize::try_from(register))
        else {
            continue;
        };
        let value_type = &register_types[register_index];
        let same_type = slots
            .get(slot_index)
            .is_some_and(|declared| declared["value_type"] == *value_type);
        let integer = value_type["scalar"]["kind"] == "integer";
        let stored_later = operations[index + 1..].iter().any(|later| {
            later["operation"]["operation"] == "store" && later["operation"]["slot"] == slot
        });
        if same_type && !integer && !stored_later {
            aliases[register_index] = serde_json::Value::from(slot);
        }
    }
    Some(serde_json::Value::Array(aliases))
}

/// Operations that compute element-wise into their destination and never
/// fault (Integer results excepted, whose domain check can).
const POINTWISE: [&str; 7] = [
    "constant", "unary", "binary", "compare", "convert", "select", "fill",
];

/// Give every serialized program a `forward_to` table: register -> slot for a
/// value computed only to be stored, which the C then computes straight into
/// the slot. The register is defined by a non-faulting pointwise operation and
/// read exactly once, by a `store` of the whole register into a slot of its
/// exact (non-Integer) type; that slot is not loaded before the store, and
/// nothing between the definition and the store stores it or may fault, so
/// writing it early is unobservable (a non-Startup fault keeps earlier
/// stores).
fn program_forwards(
    program: &serde_json::Map<String, serde_json::Value>,
) -> Option<serde_json::Value> {
    let operations = program.get("operations")?.as_array()?;
    let register_types = program.get("register_types")?.as_array()?;
    let slots = program.get("slots")?.as_array()?;
    let mut uses = vec![0usize; register_types.len()];
    for issued in operations {
        count_uses(&issued["operation"], &mut uses);
    }
    let mut forwards = vec![serde_json::Value::Null; register_types.len()];
    for (store_index, issued) in operations.iter().enumerate() {
        let store = &issued["operation"];
        if store["operation"] != "store" {
            continue;
        }
        let Some(register) = store["source"]
            .as_u64()
            .and_then(|id| usize::try_from(id).ok())
        else {
            continue;
        };
        let slot = &store["slot"];
        let Some(slot_type) = slot
            .as_u64()
            .and_then(|id| usize::try_from(id).ok())
            .and_then(|id| slots.get(id))
            .map(|declared| &declared["value_type"])
        else {
            continue;
        };
        let value_type = &register_types[register];
        if uses[register] != 1 || *slot_type != *value_type || is_integer(value_type) {
            continue;
        }
        let Some(definition) = operations[..store_index].iter().position(|earlier| {
            earlier["operation"]["destination"].as_u64() == Some(register as u64)
        }) else {
            continue;
        };
        let defining = &operations[definition]["operation"];
        let pointwise = POINTWISE.iter().any(|kind| defining["operation"] == *kind);
        let loaded_before = operations[..store_index].iter().any(|earlier| {
            earlier["operation"]["operation"] == "load" && earlier["operation"]["slot"] == *slot
        });
        let between = &operations[definition + 1..store_index];
        let quiet_between = between.iter().all(|later| {
            let operation = &later["operation"];
            let stores_slot = operation["operation"] == "store" && operation["slot"] == *slot;
            !stores_slot && cannot_fault(operation, register_types)
        });
        if pointwise && !loaded_before && quiet_between && cannot_fault(defining, register_types) {
            forwards[register] = slot.clone();
        }
    }
    Some(serde_json::Value::Array(forwards))
}

fn is_integer(value_type: &serde_json::Value) -> bool {
    value_type["scalar"]["kind"] == "integer"
}

/// A load, a store, or a pointwise operation with a non-Integer result.
fn cannot_fault(operation: &serde_json::Value, register_types: &[serde_json::Value]) -> bool {
    let kind = &operation["operation"];
    if kind == "load" || kind == "store" {
        return true;
    }
    let integer_result = operation["destination"]
        .as_u64()
        .and_then(|id| usize::try_from(id).ok())
        .and_then(|id| register_types.get(id))
        .is_none_or(is_integer);
    POINTWISE.iter().any(|pointwise| kind == *pointwise) && !integer_result
}

/// Count every register an operation may read: any number outside its
/// definitions, slot and owner, nested programs excluded (a region reads its
/// parent's registers only through the operation's `captures`). Counting a
/// non-register number too only makes forwarding rarer.
fn count_uses(value: &serde_json::Value, uses: &mut [usize]) {
    match value {
        serde_json::Value::Number(number) => {
            if let Some(slot) = number
                .as_u64()
                .and_then(|id| usize::try_from(id).ok())
                .and_then(|id| uses.get_mut(id))
            {
                *slot += 1;
            }
        }
        serde_json::Value::Array(items) => items.iter().for_each(|item| count_uses(item, uses)),
        serde_json::Value::Object(fields) => {
            if fields.contains_key("operations") {
                return;
            }
            for (key, field) in fields {
                if !matches!(
                    key.as_str(),
                    "destination" | "destinations" | "slot" | "owner"
                ) {
                    count_uses(field, uses);
                }
            }
        }
        _ => {}
    }
}

/// Which registers the C never reads, and which slots some load reads: a C
/// local is `(void)`-referenced only when it is never read, since every such
/// statement is a structural-coverage obligation, and an unread constant is
/// not printed at all. The indices of a statically placed element access are
/// not read.
fn program_reads(
    program: &serde_json::Map<String, serde_json::Value>,
) -> Option<(serde_json::Value, serde_json::Value)> {
    let operations = program.get("operations")?.as_array()?;
    let register_types = program.get("register_types")?.as_array()?;
    let mut uses = vec![0usize; register_types.len()];
    for issued in operations {
        let operation = &issued["operation"];
        if operation.get("static_offset").is_some() {
            let mut read = operation.clone();
            if let Some(fields) = read.as_object_mut() {
                fields.remove("indices");
            }
            count_uses(&read, &mut uses);
        } else {
            count_uses(operation, &mut uses);
        }
    }
    let loaded = operations
        .iter()
        .filter(|issued| issued["operation"]["operation"] == "load")
        .map(|issued| issued["operation"]["slot"].clone())
        .collect();
    // An unread constant is not printed, so its register is not declared.
    let mut unread: Vec<serde_json::Value> = uses
        .iter()
        .map(|count| serde_json::Value::from(*count == 0))
        .collect();
    for issued in operations {
        let operation = &issued["operation"];
        if operation["operation"] == "constant"
            && let Some(register) = operation["destination"]
                .as_u64()
                .and_then(|id| usize::try_from(id).ok())
            && uses.get(register) == Some(&0)
        {
            unread[register] = serde_json::Value::from("dead");
        }
    }
    Some((
        serde_json::Value::Array(unread),
        serde_json::Value::Array(loaded),
    ))
}

/// Give every serialized program a `same_as` table: an `update_element`
/// destination -> its aggregate register, when that update is the aggregate's
/// only use and the aggregate is ordinary scratch (neither read from a slot in
/// place nor computed into one). The update then writes the aggregate's
/// storage in place instead of copying it first.
fn program_in_place(
    program: &serde_json::Map<String, serde_json::Value>,
) -> Option<serde_json::Value> {
    let operations = program.get("operations")?.as_array()?;
    let register_types = program.get("register_types")?.as_array()?;
    let aliases = program
        .get("alias_of")
        .and_then(serde_json::Value::as_array);
    let forwards = program
        .get("forward_to")
        .and_then(serde_json::Value::as_array);
    let mut uses = vec![0usize; register_types.len()];
    for issued in operations {
        count_uses(&issued["operation"], &mut uses);
    }
    let unbound = |table: Option<&Vec<serde_json::Value>>, register: usize| {
        table.is_none_or(|table| table.get(register).is_none_or(serde_json::Value::is_null))
    };
    let mut shared = vec![serde_json::Value::Null; register_types.len()];
    for issued in operations {
        let operation = &issued["operation"];
        if operation["operation"] != "update_element" {
            continue;
        }
        let (Some(destination), Some(aggregate)) = (
            operation["destination"]
                .as_u64()
                .and_then(|id| usize::try_from(id).ok()),
            operation["aggregate"]
                .as_u64()
                .and_then(|id| usize::try_from(id).ok()),
        ) else {
            continue;
        };
        if uses.get(aggregate) == Some(&1)
            && unbound(aliases, aggregate)
            && unbound(forwards, aggregate)
            && register_types.get(aggregate) == register_types.get(destination)
        {
            shared[destination] = serde_json::Value::from(aggregate);
        }
    }
    // A chain of updates (one per element of a filled matrix) shares one
    // storage: every link is an aggregate with no alias or forward of its
    // own, so each entry names the chain's root directly and the template
    // resolves it in one step, whatever the chain's length. Registers are
    // defined before use, so a chain cannot cycle.
    for register in 0..shared.len() {
        let mut root = register;
        while let Some(next) = shared[root]
            .as_u64()
            .and_then(|id| usize::try_from(id).ok())
        {
            root = next;
        }
        if root != register {
            shared[register] = serde_json::Value::from(root);
        }
    }
    Some(serde_json::Value::Array(shared))
}

/// Mark each element read or update whose indices are all Integer constants
/// inside the aggregate with its `static_offset` (row-major, from the 1-based
/// indices): its bounds check cannot fail, so the C indexes directly.
fn mark_static_offsets(program: &mut serde_json::Map<String, serde_json::Value>) {
    let Some(register_types) = program
        .get("register_types")
        .and_then(serde_json::Value::as_array)
        .cloned()
    else {
        return;
    };
    let Some(operations) = program
        .get_mut("operations")
        .and_then(serde_json::Value::as_array_mut)
    else {
        return;
    };
    let mut constants = BTreeMap::new();
    for issued in operations.iter_mut() {
        let operation = &mut issued["operation"];
        let constant = &operation["value"]["kind"];
        if operation["operation"] == "constant" && constant["kind"] == "integer" {
            if let (Some(register), Some(value)) =
                (operation["destination"].as_u64(), constant["bits"].as_i64())
            {
                constants.insert(register, value);
            }
            continue;
        }
        let kind = &operation["operation"];
        if kind != "project_element_dynamic" && kind != "update_element" {
            continue;
        }
        let Some(dimensions) = operation["aggregate"]
            .as_u64()
            .and_then(|id| usize::try_from(id).ok())
            .and_then(|id| register_types.get(id))
            .and_then(|value_type| value_type["dimensions"].as_array())
        else {
            continue;
        };
        let Some(indices) = operation["indices"].as_array() else {
            continue;
        };
        if indices.len() != dimensions.len() {
            continue;
        }
        let mut offset = 0u64;
        let mut known = true;
        for (index, extent) in indices.iter().zip(dimensions) {
            let value = index.as_u64().and_then(|register| constants.get(&register));
            match (value, extent.as_u64()) {
                (Some(&value), Some(extent))
                    if value >= 1 && u64::try_from(value).is_ok_and(|value| value <= extent) =>
                {
                    offset = offset * extent + (value.unsigned_abs() - 1);
                }
                _ => known = false,
            }
        }
        if known {
            operation["static_offset"] = serde_json::Value::from(offset);
        }
    }
}

/// Give every function owner its call `level`: 1 + the deepest level of any
/// caller, methods being level 0. Calls do not recurse, so every call chain
/// strictly climbs levels, and frames of one level are never live together:
/// the header overlays them in one union per level.
fn assign_call_levels(json: &mut serde_json::Value) {
    let count = json
        .pointer("/functions/owners")
        .and_then(serde_json::Value::as_array)
        .map_or(0, Vec::len);
    let callees = |program: &serde_json::Value| {
        let mut called = Vec::new();
        find_calls(program, &mut called);
        called
    };
    let roots: Vec<usize> = ["/startup", "/recalibrate", "/do_step"]
        .iter()
        .filter_map(|method| json.pointer(method))
        .flat_map(callees)
        .collect();
    let edges: Vec<Vec<usize>> = (0..count)
        .map(|owner| {
            json.pointer(&format!("/functions/owners/{owner}/body"))
                .map(callees)
                .unwrap_or_default()
        })
        .collect();
    let mut level = vec![1usize; count];
    let mut frontier: Vec<(usize, usize)> = roots.into_iter().map(|owner| (owner, 1)).collect();
    while let Some((owner, depth)) = frontier.pop() {
        if owner >= count || depth > count || level[owner] > depth {
            continue;
        }
        level[owner] = depth;
        frontier.extend(edges[owner].iter().map(|callee| (*callee, depth + 1)));
    }
    if let Some(owners) = json
        .pointer_mut("/functions/owners")
        .and_then(serde_json::Value::as_array_mut)
    {
        for (owner, entry) in owners.iter_mut().enumerate() {
            if let Some(fields) = entry.as_object_mut() {
                fields.insert("level".to_owned(), serde_json::Value::from(level[owner]));
            }
        }
    }
    if let Some(fields) = json.as_object_mut() {
        let deepest = level.iter().copied().max().unwrap_or(0);
        fields.insert(
            "function_levels".to_owned(),
            serde_json::Value::from(deepest),
        );
    }
}

/// The owner of every call operation in a program, nested programs included.
fn find_calls(value: &serde_json::Value, called: &mut Vec<usize>) {
    match value {
        serde_json::Value::Object(fields) => {
            if fields.get("operation").is_some_and(|kind| kind == "call")
                && let Some(owner) = fields
                    .get("owner")
                    .and_then(serde_json::Value::as_u64)
                    .and_then(|id| usize::try_from(id).ok())
            {
                called.push(owner);
            }
            fields.values().for_each(|field| find_calls(field, called));
        }
        serde_json::Value::Array(items) => items.iter().for_each(|item| find_calls(item, called)),
        _ => {}
    }
}

/// One array register's storage demand within an owner: the program it
/// belongs to (a JSON pointer below the owner), its id, its live interval in
/// the owner's depth-first operation order, and its storage key.
struct Demand {
    program: String,
    register: usize,
    first: usize,
    last: usize,
    key: serde_json::Value,
}

/// Share instance-scratch storage between array registers of one owner
/// whose live intervals do not overlap, as `pool_of[register] = k` on each
/// (nested) program and a `pools` list on the owner. Every operation is
/// numbered depth-first; a register's interval spans its definition to its
/// last reference, and every register an operation with nested programs
/// references (captures, carried values, destinations) counts as referenced
/// for that operation's whole span, so a value a region or loop reads stays
/// live throughout it. Intervals are inclusive and only strictly disjoint
/// ones share, so no operation ever reads and writes one storage.
/// Registers already served elsewhere (read in place, forwarded, updated in
/// place, unread constants) get no storage, and scalars are C locals.
fn allocate_pools(owner: &mut serde_json::Value) {
    let mut demands = Vec::new();
    let mut clock = 0usize;
    collect_demands(owner, String::new(), &mut clock, &mut demands);
    demands.sort_by_key(|demand| (demand.first, demand.last));
    let mut pools: Vec<(serde_json::Value, usize)> = Vec::new();
    let mut placed = Vec::new();
    for demand in &demands {
        let pool = pools
            .iter()
            .position(|(key, end)| *key == demand.key && *end < demand.first)
            .unwrap_or_else(|| {
                pools.push((demand.key.clone(), 0));
                pools.len() - 1
            });
        pools[pool].1 = demand.last;
        placed.push((demand.program.clone(), demand.register, pool));
    }
    for (program, register, pool) in placed {
        if let Some(entry) = owner
            .pointer_mut(&format!("{program}/pool_of"))
            .and_then(serde_json::Value::as_array_mut)
            .and_then(|table| table.get_mut(register))
        {
            *entry = serde_json::Value::from(pool);
        }
    }
    if let Some(fields) = owner.as_object_mut() {
        fields.insert(
            "pools".to_owned(),
            serde_json::Value::Array(pools.into_iter().map(|(key, _)| key).collect()),
        );
    }
}

fn collect_demands(
    program: &mut serde_json::Value,
    path: String,
    clock: &mut usize,
    demands: &mut Vec<Demand>,
) {
    let Some(register_types) = program
        .get("register_types")
        .and_then(serde_json::Value::as_array)
        .cloned()
    else {
        return;
    };
    let served = |table: &str, register: usize| {
        program
            .get(table)
            .and_then(|table| table.get(register))
            .is_some_and(|entry| !entry.is_null() && *entry != false)
    };
    let pooled: Vec<bool> = register_types
        .iter()
        .enumerate()
        .map(|(register, value_type)| {
            value_type["scalar_count"].as_u64().unwrap_or(0) > 1
                && !["alias_of", "forward_to", "same_as"]
                    .iter()
                    .any(|table| served(table, register))
                && program["unread"].get(register) != Some(&serde_json::Value::from("dead"))
        })
        .collect();
    // Reference intervals, with each in-place update's destination folded into
    // the aggregate whose storage it names.
    let mut root: Vec<usize> = (0..register_types.len()).collect();
    if let Some(shared) = program.get("same_as").and_then(serde_json::Value::as_array) {
        for (register, aggregate) in shared.iter().enumerate() {
            if let Some(aggregate) = aggregate.as_u64().and_then(|id| usize::try_from(id).ok()) {
                root[register] = aggregate;
            }
        }
    }
    let resolve = |mut register: usize| {
        while root[register] != register {
            register = root[register];
        }
        register
    };
    let mut interval: Vec<Option<(usize, usize)>> = vec![None; register_types.len()];
    if let Some(operations) = program
        .get_mut("operations")
        .and_then(serde_json::Value::as_array_mut)
    {
        for (index, issued) in operations.iter_mut().enumerate() {
            let start = *clock;
            *clock += 1;
            let operation = &mut issued["operation"];
            let mut programs = Vec::new();
            find_programs(
                operation,
                format!("{path}/operations/{index}/operation"),
                &mut programs,
            );
            for (pointer, child) in programs {
                collect_demands(child, pointer, clock, demands);
            }
            let end = (*clock).max(start + 1) - 1;
            let mut referenced = Vec::new();
            op_registers(operation, &mut referenced);
            for register in referenced.into_iter().filter(|id| *id < root.len()) {
                let register = resolve(register);
                let slot = &mut interval[register];
                *slot = Some(match *slot {
                    None => (start, end),
                    Some((first, last)) => (first.min(start), last.max(end)),
                });
            }
        }
    }
    for (register, span) in interval.iter().enumerate() {
        if let (true, Some((first, last))) = (pooled[register], span) {
            demands.push(Demand {
                program: path.clone(),
                register,
                first: *first,
                last: *last,
                key: serde_json::json!({
                    "scalar": register_types[register]["scalar"],
                    "scalar_count": register_types[register]["scalar_count"],
                }),
            });
        }
    }
    if let Some(fields) = program.as_object_mut() {
        fields.insert(
            "pool_of".to_owned(),
            serde_json::Value::Array(vec![serde_json::Value::Null; register_types.len()]),
        );
    }
}

/// The nested programs of an operation, with their JSON pointers.
fn find_programs<'v>(
    value: &'v mut serde_json::Value,
    path: String,
    found: &mut Vec<(String, &'v mut serde_json::Value)>,
) {
    if value.get("operations").is_some() {
        found.push((path, value));
        return;
    }
    match value {
        serde_json::Value::Object(fields) => {
            for (key, field) in fields.iter_mut() {
                let pointer = format!("{path}/{}", key.replace('~', "~0").replace('/', "~1"));
                find_programs(field, pointer, found);
            }
        }
        serde_json::Value::Array(items) => {
            for (index, item) in items.iter_mut().enumerate() {
                find_programs(item, format!("{path}/{index}"), found);
            }
        }
        _ => {}
    }
}

/// Every number an operation names outside its slot, owner and placement,
/// nested programs excluded: its registers, read or written (over-reporting
/// only lengthens intervals).
fn op_registers(value: &serde_json::Value, registers: &mut Vec<usize>) {
    match value {
        serde_json::Value::Number(number) => {
            registers.extend(number.as_u64().and_then(|id| usize::try_from(id).ok()));
        }
        serde_json::Value::Array(items) => {
            items.iter().for_each(|item| op_registers(item, registers))
        }
        serde_json::Value::Object(fields) => {
            if fields.contains_key("operations") {
                return;
            }
            for (key, field) in fields {
                if !matches!(key.as_str(), "slot" | "owner" | "static_offset") {
                    op_registers(field, registers);
                }
            }
        }
        _ => {}
    }
}

/// Every `real32`/`real64` constant kind in the serialized programs.
fn collect_literals(json: &serde_json::Value, literals: &mut BTreeMap<String, String>) {
    match json {
        serde_json::Value::Object(fields) => {
            if let (Some(kind), Some(bits)) = (
                fields.get("kind").and_then(serde_json::Value::as_str),
                fields.get("bits").and_then(serde_json::Value::as_u64),
            ) && let Some(literal) = real_literal(kind, bits)
            {
                literals.insert(format!("{kind}:{bits}"), literal);
            }
            for value in fields.values() {
                collect_literals(value, literals);
            }
        }
        serde_json::Value::Array(items) => {
            for item in items {
                collect_literals(item, literals);
            }
        }
        _ => {}
    }
}

/// A C literal with exactly the constant's bits; NaN keeps its exact-bits
/// form (no literal carries a payload).
fn real_literal(kind: &str, bits: u64) -> Option<String> {
    let (value, suffix) = match kind {
        "real32" => (f64::from(f32::from_bits(u32::try_from(bits).ok()?)), "f"),
        "real64" => (f64::from_bits(bits), ""),
        _ => return None,
    };
    if value.is_nan() {
        return None;
    }
    if value.is_infinite() {
        let sign = if value < 0.0 { "-" } else { "" };
        return Some(format!("({sign}HUGE_VAL{})", suffix.to_uppercase()));
    }
    let text = if kind == "real32" {
        format!("{:?}", f32::from_bits(u32::try_from(bits).ok()?))
    } else {
        format!("{value:?}")
    };
    Some(format!("{text}{suffix}"))
}

/// Every `provenance` span in the serialized programs, resolved once.
fn collect_traces(
    json: &serde_json::Value,
    resolver: &crate::views::source_trace::SourceTraceResolver<'_>,
    traces: &mut BTreeMap<String, crate::views::source_trace::SourceTrace>,
) {
    match json {
        serde_json::Value::Object(fields) => {
            for (key, value) in fields {
                if key == "provenance"
                    && let Ok(span) = serde_json::from_value::<rumoca_core::Span>(value.clone())
                    && let Some(trace) = resolver.trace(&span)
                {
                    traces.insert(trace_key(value), trace);
                }
                collect_traces(value, resolver, traces);
            }
        }
        serde_json::Value::Array(items) => {
            for item in items {
                collect_traces(item, resolver, traces);
            }
        }
        _ => {}
    }
}

/// `source:start:end`, as a template spells it from the same serialized
/// provenance.
fn trace_key(provenance: &serde_json::Value) -> String {
    format!(
        "{}:{}:{}",
        provenance["source"], provenance["start"], provenance["end"]
    )
}

/// The sentinel the Solve C template appends to each `T name[1] = {0};
/// (void)name;` declaration whose `(void)` use may be needless.
pub(super) const VOID_CANDIDATE: &str = " /*rumoca_galec_void_candidate*/";

/// Resolve the template's unused-marker candidates in a Solve-printed C unit:
/// keep a scalar local's `(void)` use only where the local is never read.
///
/// The Solve template declares every scalar local with a `(void)` use and
/// [`VOID_CANDIDATE`], because whether a local is read depends on how its
/// operations print. Every `(void)` is a statement a structural-coverage
/// obligation has to account for, so a candidate the emitted function itself
/// reads loses it: a read is any occurrence in code, outside comments and
/// literals, other than the left-hand side of a plain `=` (passing it to a
/// call counts, as it does for the C compiler). Names are scoped per
/// top-level C function. Every candidate loses its sentinel, and lines
/// without one are copied unchanged, so output with no candidates (any
/// render that is not a Solve C unit) is returned byte-exact.
pub(super) fn drop_needless_void_uses(text: &str) -> String {
    if !text.contains(VOID_CANDIDATE) {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len());
    let mut function = Vec::new();
    for line in text.split_inclusive('\n') {
        function.push(line);
        if line.trim_end() == "}" && !line.starts_with(char::is_whitespace) {
            flush_function(&mut function, &mut out);
        }
    }
    flush_function(&mut function, &mut out);
    out
}

fn flush_function(lines: &mut Vec<&str>, out: &mut String) {
    let body = without_comments_or_literals(&lines.concat());
    for line in lines.drain(..) {
        match void_candidate(line) {
            Some((name, unread, read)) => {
                out.push_str(if is_read(&body, name) { &read } else { &unread });
            }
            None => out.push_str(line),
        }
    }
}

/// For a tagged `T name[1] = {0}; (void)name;` line, the local's name and the
/// line as printed when the local is unread (marker kept) and when it is read
/// (marker dropped), both without the sentinel.
fn void_candidate(line: &str) -> Option<(&str, String, String)> {
    let trimmed = line.trim_end_matches('\n');
    let untagged = trimmed.strip_suffix(VOID_CANDIDATE)?;
    let (declaration, void_use) = untagged.split_once("[1] = {0}; (void)")?;
    let name = void_use.strip_suffix(';')?;
    if !name.starts_with("rumoca_galec_") || !declaration.ends_with(name) {
        return None;
    }
    let ending = &line[trimmed.len()..];
    Some((
        name,
        format!("{untagged}{ending}"),
        format!("{declaration}[1] = {{0}};{ending}"),
    ))
}

/// `text` with every comment and string or character literal blanked to
/// spaces (newlines kept), so only code can count as a read.
fn without_comments_or_literals(text: &str) -> String {
    let bytes = text.as_bytes();
    let mut out = bytes.to_vec();
    let mut i = 0;
    while i < bytes.len() {
        let end = match (bytes[i], bytes.get(i + 1)) {
            (b'/', Some(b'*')) => text[i + 2..]
                .find("*/")
                .map_or(bytes.len(), |n| i + 2 + n + 2),
            (b'/', Some(b'/')) => text[i..].find('\n').map_or(bytes.len(), |n| i + n),
            (quote @ (b'"' | b'\''), _) => literal_end(bytes, i, quote),
            _ => {
                i += 1;
                continue;
            }
        };
        for byte in &mut out[i..end] {
            if *byte != b'\n' {
                *byte = b' ';
            }
        }
        i = end;
    }
    // Only whole multi-byte characters inside blanked spans were replaced, so
    // the result is still UTF-8.
    String::from_utf8(out).unwrap_or_default()
}

/// The end of the string or character literal opening at `start`: past its
/// closing quote, or at the line end of an unterminated one.
fn literal_end(bytes: &[u8], start: usize, quote: u8) -> usize {
    let mut j = start + 1;
    while j < bytes.len() && bytes[j] != quote && bytes[j] != b'\n' {
        j += if bytes[j] == b'\\' { 2 } else { 1 };
    }
    (j + 1).min(bytes.len())
}

/// Whether `name` occurs anywhere in `body` except its declaration, its
/// `(void)` use and the target of a plain assignment.
fn is_read(body: &str, name: &str) -> bool {
    let mut rest = body;
    let mut offset = 0;
    while let Some(found) = rest.find(name) {
        let start = offset + found;
        let end = start + name.len();
        offset = end;
        rest = &body[end..];
        let before = body[..start].chars().next_back();
        let after = body[end..].chars().next();
        let identifier = |c: Option<char>| c.is_some_and(|c| c.is_ascii_alphanumeric() || c == '_');
        if identifier(before) || identifier(after) || body[..start].ends_with("(void)") {
            continue;
        }
        let tail = &body[end..];
        if tail.starts_with("[1] = {0};") {
            continue;
        }
        let assigned = tail
            .strip_prefix('[')
            .and_then(|index| index.split_once(']'))
            .map(|(_, after)| after.trim_start())
            .is_some_and(|after| after.starts_with('=') && !after.starts_with("=="));
        if !assigned {
            return true;
        }
    }
    false
}

/// The operators `operation` itself prints: none for a constant, load or store.
fn arithmetic(operation: &serde_json::Value) -> usize {
    usize::from(!matches!(
        operation["operation"].as_str(),
        Some("constant" | "load" | "store")
    ))
}

/// The registers below `count` that `value` reads, once each, found by the
/// same walk as [`count_uses`]; `work` counts the nodes visited.
fn operand_registers(value: &serde_json::Value, count: usize, work: &mut usize) -> Vec<usize> {
    fn walk(value: &serde_json::Value, count: usize, found: &mut Vec<usize>, work: &mut usize) {
        *work += 1;
        match value {
            serde_json::Value::Number(number) => {
                if let Some(id) = number
                    .as_u64()
                    .and_then(|id| usize::try_from(id).ok())
                    .filter(|id| *id < count)
                {
                    found.push(id);
                }
            }
            serde_json::Value::Array(items) => {
                items.iter().for_each(|item| walk(item, count, found, work));
            }
            serde_json::Value::Object(fields) if !fields.contains_key("operations") => fields
                .iter()
                .filter(|(key, _)| {
                    !matches!(
                        key.as_str(),
                        "destination" | "destinations" | "slot" | "owner"
                    )
                })
                .for_each(|(_, field)| walk(field, count, found, work)),
            _ => {}
        }
    }
    let mut found = Vec::new();
    walk(value, count, &mut found, work);
    found.sort_unstable();
    found.dedup();
    found
}

fn register_id(value: &serde_json::Value) -> Option<usize> {
    value.as_u64().and_then(|id| usize::try_from(id).ok())
}

fn is_real_scalar(register_types: &[serde_json::Value], register: usize) -> bool {
    register_types.get(register).is_some_and(|ty| {
        ty["scalar"]["kind"] == "real" && ty["dimensions"].as_array().is_none_or(Vec::is_empty)
    })
}

/// A Real `+`, `-`, `*` or negation, a constant, a load or an Integer-to-Real
/// conversion: the operations an inlined register may be defined by, and the
/// only ones allowed between a definition and its use.
fn inline_pure(operation: &serde_json::Value, register_types: &[serde_json::Value]) -> bool {
    let real = || {
        register_id(&operation["destination"])
            .is_some_and(|register| is_real_scalar(register_types, register))
    };
    match operation["operation"].as_str() {
        Some("constant" | "load") => true,
        Some("binary") => {
            real()
                && matches!(
                    operation["operator"].as_str(),
                    Some("add" | "subtract" | "multiply")
                )
        }
        Some("unary") => real() && operation["operator"] == "negate",
        Some("convert") => operation["operator"] == "integer_to_real",
        _ => false,
    }
}

/// An operation that may print an inlined operand: a Real binary, any unary,
/// a scalar store, or an Integer-to-Real conversion.
fn inline_consumer(operation: &serde_json::Value, register_types: &[serde_json::Value]) -> bool {
    match operation["operation"].as_str() {
        Some("binary") => register_id(&operation["destination"])
            .is_some_and(|register| is_real_scalar(register_types, register)),
        Some("unary" | "store") => true,
        Some("convert") => operation["operator"] == "integer_to_real",
        _ => false,
    }
}

/// The deepest expression an inlined register may expand to: deeper
/// definitions keep their temporary.
const INLINE_DEPTH: usize = 3;

/// The most arithmetic operators (`+`, `-`, `*`, negation, conversion) one
/// printed statement may hold once its operands are inlined, so every line
/// stays short enough to review; loads, constants and stores count nothing.
const INLINE_OPERATORS: usize = 4;

/// Give every serialized program an `inline_of` table: register -> the index
/// of the operation that defines it, when the C prints that definition at its
/// one use instead of declaring and assigning a temporary (#483 size pass:
/// expression folding). A register qualifies only when
/// - it is a Real scalar defined by Real `+`, `-`, `*`, negation, a
///   constant, a load or an Integer-to-Real conversion (the cast, and so its
///   rounding, stays in the expression);
/// - it occurs exactly once anywhere in its program, as an operand of a Real
///   binary, unary, scalar store or conversion in the same program;
/// - every operation between its definition and that use is itself one of
///   those pure kinds, so no store, call, check, raise or region moves past
///   it, and chained inlines stay barrier-free end to end;
/// - its definition carries the same provenance as its use, so the use's
///   trace comment is exact;
/// - it is not served by `alias_of`, `forward_to`, `same_as` or a pool; and
/// - its expansion is at most [`INLINE_DEPTH`] levels deep, and its use's
///   statement, with every operand inlined so far, holds at most
///   [`INLINE_OPERATORS`] operators.
fn program_inlines(
    program: &serde_json::Map<String, serde_json::Value>,
) -> Option<serde_json::Value> {
    program_inlines_counted(program, &mut 0)
}

/// [`program_inlines`] in one indexed sweep: each
/// operation's operands are collected once, sparsely; a single-use register's
/// consumer is recorded during that sweep; and a prefix count of barrier
/// operations answers "barrier-free between definition and use" in O(1).
/// `work` counts every JSON node and operand visited.
fn program_inlines_counted(
    program: &serde_json::Map<String, serde_json::Value>,
    work: &mut usize,
) -> Option<serde_json::Value> {
    let operations = program.get("operations")?.as_array()?;
    let register_types = program.get("register_types")?.as_array()?;
    let count = register_types.len();
    let operand_lists: Vec<Vec<usize>> = operations
        .iter()
        .map(|issued| operand_registers(&issued["operation"], count, work))
        .collect();
    let mut uses = vec![0usize; count];
    for issued in operations {
        count_uses(&issued["operation"], &mut uses);
    }
    if let Some(states) = program.get("assigned_states") {
        count_uses(states, &mut uses);
    }
    let mut consumer_of = vec![None; count];
    for (index, operands) in operand_lists.iter().enumerate() {
        for operand in operands {
            *work += 1;
            consumer_of[*operand] = Some(index);
        }
    }
    let served = |table: &str, register: usize| {
        program
            .get(table)
            .and_then(|table| table.get(register))
            .is_some_and(|entry| !entry.is_null() && *entry != false)
    };
    let real_scalar = |register: usize| is_real_scalar(register_types, register);
    let pure = |operation: &serde_json::Value| inline_pure(operation, register_types);
    let consumer = |operation: &serde_json::Value| inline_consumer(operation, register_types);
    let register = register_id;
    // barriers[i]: operations before index i that are not pure.
    let mut barriers = Vec::with_capacity(operations.len() + 1);
    barriers.push(0usize);
    for issued in operations {
        let last = barriers[barriers.len() - 1];
        barriers.push(last + usize::from(!pure(&issued["operation"])));
    }
    let mut inline_of = vec![serde_json::Value::Null; count];
    let mut depth = vec![0usize; count];
    // Per operation: its own operator plus those of the operands inlined into it.
    let mut operators: Vec<usize> = operations
        .iter()
        .map(|issued| arithmetic(&issued["operation"]))
        .collect();
    for (index, issued) in operations.iter().enumerate() {
        let operation = &issued["operation"];
        let Some(defined) = register(&operation["destination"]) else {
            continue;
        };
        if !real_scalar(defined)
            || !pure(operation)
            || uses.get(defined) != Some(&1)
            || ["alias_of", "forward_to", "same_as", "pool_of"]
                .iter()
                .any(|table| served(table, defined))
            || program["unread"].get(defined) != Some(&serde_json::Value::from(false))
        {
            continue;
        }
        // The single use is in a later operation unless it is an assigned state.
        let Some(use_index) = consumer_of[defined].filter(|use_index| *use_index > index) else {
            continue;
        };
        let used = &operations[use_index];
        let barrier_free = barriers[use_index] == barriers[index + 1];
        *work += operand_lists[index].len();
        let operands_depth = operand_lists[index]
            .iter()
            .map(|r| depth[*r])
            .max()
            .unwrap_or(0);
        if consumer(&used["operation"])
            && barrier_free
            && issued["provenance"] == used["provenance"]
            && operands_depth < INLINE_DEPTH
            && operators[use_index] + operators[index] <= INLINE_OPERATORS
        {
            inline_of[defined] = serde_json::Value::from(index);
            depth[defined] = operands_depth + 1;
            operators[use_index] += operators[index];
        }
    }
    Some(serde_json::Value::Array(inline_of))
}

#[cfg(test)]
mod void_use_tests {
    use super::{VOID_CANDIDATE, drop_needless_void_uses};

    fn candidate(name: &str) -> String {
        format!("    float {name}[1] = {{0}}; (void){name};{VOID_CANDIDATE}\n")
    }

    #[test]
    fn a_self_read_on_the_right_hand_side_drops_the_marker() {
        let text = format!(
            "void f(void) {{\n{}    rumoca_galec_r0[0] = rumoca_galec_r0[0] + 1.0f;\n}}\n",
            candidate("rumoca_galec_r0")
        );
        assert_eq!(
            drop_needless_void_uses(&text),
            "void f(void) {\n    float rumoca_galec_r0[1] = {0};\n    rumoca_galec_r0[0] = rumoca_galec_r0[0] + 1.0f;\n}\n"
        );
    }

    #[test]
    fn comments_and_literals_are_not_reads() {
        let text = format!(
            "void f(void) {{\n{}    /* rumoca_galec_s0.mo:3:1 */\n    // rumoca_galec_s0\n    \
             const char *p = \"rumoca_galec_s0\";\n    rumoca_galec_s0[0] = 2.0f;\n}}\n",
            candidate("rumoca_galec_s0")
        );
        let out = drop_needless_void_uses(&text);
        assert!(
            out.contains("float rumoca_galec_s0[1] = {0}; (void)rumoca_galec_s0;\n"),
            "{out}"
        );
        assert!(!out.contains(VOID_CANDIDATE), "{out}");
    }

    #[test]
    fn reads_are_scoped_to_their_function() {
        let text = format!(
            "void f(void) {{\n{}    rumoca_galec_r0[0] = 1.0f;\n}}\nvoid g(float *rumoca_galec_r0) {{\n    \
             (void)rumoca_galec_r0[0];\n}}\n",
            candidate("rumoca_galec_r0")
        );
        assert!(
            drop_needless_void_uses(&text).contains("(void)rumoca_galec_r0;\n"),
            "a read in another function must not drop the marker"
        );
    }

    #[test]
    fn untagged_text_is_returned_byte_exact() {
        let text = "void f(void) {\n    float rumoca_galec_r0[1] = {0}; (void)rumoca_galec_r0;\n    \
                    g(rumoca_galec_r0);\n}\n";
        assert_eq!(drop_needless_void_uses(text), text);
    }
}

#[cfg(test)]
mod inline_tests {
    //! #483 expression folding: each eligibility rule of `program_inlines`, with a
    //! case that fails if the rule is dropped.

    use serde_json::{Value, json};

    use super::{program_inlines, program_inlines_counted};

    fn span(start: u64) -> Value {
        json!({ "source": 1, "start": start, "end": start + 1 })
    }

    fn real() -> Value {
        json!({ "scalar": { "kind": "real" }, "dimensions": [] })
    }

    /// A program over `types.len()` registers whose operations all carry
    /// the provenance `span(1)` unless given as `(operation, start)`.
    fn program(types: Vec<Value>, operations: Vec<(Value, u64)>) -> serde_json::Map<String, Value> {
        let count = types.len();
        let mut program = serde_json::Map::new();
        program.insert("register_types".to_owned(), Value::Array(types));
        program.insert(
            "operations".to_owned(),
            Value::Array(
                operations
                    .into_iter()
                    .map(|(operation, start)| json!({ "operation": operation, "provenance": span(start) }))
                    .collect(),
            ),
        );
        program.insert(
            "unread".to_owned(),
            Value::Array(vec![Value::from(false); count]),
        );
        program
    }

    fn load(destination: u64, slot: u64) -> Value {
        json!({ "operation": "load", "destination": destination, "slot": slot })
    }

    fn add(destination: u64, lhs: u64, rhs: u64) -> Value {
        json!({ "operation": "binary", "operator": "add", "destination": destination, "lhs": lhs, "rhs": rhs })
    }

    fn store(slot: u64, source: u64) -> Value {
        json!({ "operation": "store", "slot": slot, "source": source })
    }

    fn inlined(program: &serde_json::Map<String, Value>) -> Vec<bool> {
        program_inlines(program)
            .unwrap()
            .as_array()
            .unwrap()
            .iter()
            .map(|entry| !entry.is_null())
            .collect()
    }

    #[test]
    fn a_single_use_pure_real_chain_is_inlined() {
        let p = program(
            vec![real(), real(), real()],
            vec![
                (load(0, 0), 1),
                (load(1, 1), 1),
                (add(2, 0, 1), 1),
                (store(2, 2), 1),
            ],
        );
        assert_eq!(inlined(&p), vec![true, true, true]);
    }

    #[test]
    fn a_register_with_two_occurrences_keeps_its_temporary() {
        let p = program(
            vec![real(), real()],
            vec![(load(0, 0), 1), (add(1, 0, 0), 1), (store(2, 1), 1)],
        );
        assert_eq!(inlined(&p), vec![false, true]);
    }

    #[test]
    fn a_store_between_definition_and_use_is_a_barrier() {
        // r0 reads slot 0; slot 0 is stored before r0's use.
        let p = program(
            vec![real(), real(), real()],
            vec![
                (load(0, 0), 1),
                (load(1, 1), 1),
                (store(0, 1), 1),
                (add(2, 0, 0), 1),
            ],
        );
        assert!(!inlined(&p)[0]);
    }

    #[test]
    fn a_call_compare_or_integer_operation_between_is_a_barrier() {
        for between in [
            json!({ "operation": "call", "owner": 0, "arguments": [], "destinations": [] }),
            json!({ "operation": "compare", "operator": "less", "destination": 3, "lhs": 3, "rhs": 3 }),
            json!({ "operation": "binary", "operator": "add", "destination": 3, "lhs": 3, "rhs": 3 }),
        ] {
            let mut types = vec![real(), real(), real()];
            types.push(json!({ "scalar": { "kind": "integer" }, "dimensions": [] }));
            let p = program(
                types,
                vec![
                    (load(0, 0), 1),
                    (between, 1),
                    (add(1, 0, 2), 1),
                    (store(5, 1), 1),
                ],
            );
            assert!(!inlined(&p)[0], "a barrier keeps r0 materialized");
        }
    }

    #[test]
    fn a_use_inside_a_region_keeps_the_value_materialized() {
        let p = program(
            vec![real(), real()],
            vec![
                (load(0, 0), 1),
                (
                    json!({ "operation": "conditional", "captures": [0], "destinations": [1] }),
                    1,
                ),
            ],
        );
        assert!(!inlined(&p)[0]);
    }

    #[test]
    fn a_producer_with_another_source_span_keeps_its_temporary() {
        let p = program(
            vec![real(), real(), real()],
            vec![
                (load(0, 0), 7),
                (load(1, 1), 1),
                (add(2, 0, 1), 1),
                (store(2, 2), 1),
            ],
        );
        assert_eq!(inlined(&p), vec![false, true, true]);
    }

    #[test]
    fn expansion_stops_at_the_depth_bound() {
        // r1 = r0 + r0' chains: depth 1, 2, 3, then the fourth keeps its temporary.
        let p = program(
            vec![real(); 6],
            vec![
                (load(0, 0), 1),
                (add(1, 0, 5), 1),
                (add(2, 1, 5), 1),
                (add(3, 2, 5), 1),
                (add(4, 3, 5), 1),
                (store(9, 4), 1),
                (load(5, 1), 1),
            ],
        );
        let inlined = inlined(&p);
        assert!(inlined[0] && inlined[1] && inlined[2]);
        assert!(!inlined[3], "a fourth level keeps its temporary");
    }

    /// Regression for the analysis cost: two shared leaves, then `n`
    /// independent Real adds each read once by its own store.
    fn independent_adds(n: u64) -> (serde_json::Map<String, Value>, usize) {
        let mut operations = vec![(load(0, 0), 1), (load(1, 1), 1)];
        operations.extend((0..n).map(|i| (add(2 + i, 0, 1), 1)));
        operations.extend((0..n).map(|i| (store(10 + i, 2 + i), 1)));
        let p = program(vec![real(); 2 + n as usize], operations);
        let mut work = 0;
        program_inlines_counted(&p, &mut work);
        (p, work)
    }

    #[test]
    fn the_eligibility_pass_avoids_quadratic_work() {
        let (small, at_16) = independent_adds(16);
        let (large, at_128) = independent_adds(128);
        // Every store is a barrier, so only the first add reaches its store
        // with nothing impure in between.
        for (p, n) in [(&small, 16), (&large, 128)] {
            let mut expected = vec![false; 2 + n];
            expected[2] = true;
            assert_eq!(inlined(p), expected);
        }
        // 8x the operations may cost at most ~8x the work; the earlier
        // per-definition scans grew it 64x or more.
        assert!(
            at_128 <= 9 * at_16,
            "work grew from {at_16} at n=16 to {at_128} at n=128"
        );
    }

    #[test]
    fn expansion_stops_at_the_operator_cap() {
        // r0, r1 are shared leaves; r4 and r7 each hold 3 operators. r4 fits
        // r8's statement (1 + 3), r7 would make it 7 and keeps its own line,
        // and r8's 4 then fit the store.
        let p = program(
            vec![real(); 9],
            vec![
                (load(0, 0), 1),
                (load(1, 1), 1),
                (add(2, 0, 1), 1),
                (add(3, 0, 1), 1),
                (add(4, 2, 3), 1),
                (add(5, 0, 1), 1),
                (add(6, 0, 1), 1),
                (add(7, 5, 6), 1),
                (add(8, 4, 7), 1),
                (store(2, 8), 1),
            ],
        );
        assert_eq!(
            inlined(&p),
            vec![false, false, true, true, true, true, true, false, true]
        );
    }

    #[test]
    fn served_or_integer_registers_are_never_inlined() {
        let mut p = program(
            vec![real(), real()],
            vec![(load(0, 0), 1), (add(1, 0, 0), 1), (store(2, 1), 1)],
        );
        p.insert("forward_to".to_owned(), json!([null, 3]));
        assert!(
            !inlined(&p)[1],
            "a forwarded register is computed into its slot"
        );
        let p = program(
            vec![
                json!({ "scalar": { "kind": "integer" }, "dimensions": [] }),
                real(),
            ],
            vec![
                (load(0, 0), 1),
                (
                    json!({ "operation": "convert", "operator": "integer_to_real", "destination": 1, "operand": 0 }),
                    1,
                ),
                (store(2, 1), 1),
            ],
        );
        assert_eq!(inlined(&p), vec![false, true], "only Real scalars inline");
    }
}
