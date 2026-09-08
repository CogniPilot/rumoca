//! Exact mixed-scalar whole-record equation ownership (SPEC_0043 §4).

use rumoca::{Compiler, CompilerError};
use rumoca_compile::compile::FailedPhase;
use rumoca_core::{
    ClassType, DefId, EffectiveType, Expression, ExpressionRewriter, FunctionCallKind, Literal,
    Reference, Span, TypeId, VarName,
};

const MIXED_PARTITIONS: &str = r#"
record Mixed
  Real continuous;
  discrete Real sampled;
  Boolean valid;
end Mixed;

model MixedPartitions
  output Mixed value;
equation
  value = Mixed(2.5, 3.5, true);
end MixedPartitions;
"#;

const BOOLEAN_ONLY: &str = r#"
record Flags
  Boolean first;
  Boolean second;
end Flags;

model BooleanOnly
  output Flags value;
equation
  value = Flags(true, false);
end BooleanOnly;
"#;

const NESTED_MIXED: &str = r#"
record Inner
  Real weight;
  Boolean enabled;
end Inner;

record Outer
  Inner inner;
  Integer count;
end Outer;

model NestedMixed
  output Outer value;
equation
  value = Outer(Inner(4.0, true), 3);
end NestedMixed;
"#;

const INPUT_AS_SOURCE: &str = r#"
record Packet
  Real value;
  Boolean valid;
end Packet;

model InputAsSource
  input Packet source;
  output Packet target;
equation
  target = source;
end InputAsSource;

model SwappedInputAsSource
  input Packet source;
  output Packet target;
equation
  source = target;
end SwappedInputAsSource;
"#;

const WIRE_CALL_CATALOG: &str = r#"
function exactValue
  input Real value;
  output Real result;
algorithm
  result := value;
end exactValue;

model WireCallCatalog
  output Real result;
equation
  result = exactValue(1.0);
end WireCallCatalog;
"#;

#[test]
fn mixed_record_fields_enter_their_exact_appendix_b_partitions() {
    let compiled = Compiler::new()
        .model("MixedPartitions")
        .compile_str_dae(MIXED_PARTITIONS, "mixed_partitions.mo")
        .expect("ordinary Real, discrete Real, and Boolean leaves have checked owners");
    assert_eq!(compiled.balance_detail.continuous_equations, 1);
    assert_eq!(compiled.balance_detail.discrete_real_equations, 1);
    assert_eq!(compiled.balance_detail.discrete_value_definitions, 1);
    compiled.dae.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.targets().len(), 1);
    });
}

#[test]
fn boolean_record_fields_form_one_atomic_source_ordered_owner() {
    let compiled = Compiler::new()
        .model("BooleanOnly")
        .compile_str_dae(BOOLEAN_ONLY, "boolean_only.mo")
        .expect("Boolean record constructor is a direct B.1c definition");
    assert_eq!(compiled.balance_detail.continuous_equations, 0);
    assert_eq!(compiled.balance_detail.discrete_value_definitions, 2);
    compiled.dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.targets().len(), 2);
        assert_eq!(owner.branches().len(), 1);
    });
}

#[test]
fn two_discrete_record_sides_require_causal_orientation_in_both_directions() {
    let source = r#"
record Flag
  Boolean value;
end Flag;
model LeftToRight
  discrete Flag left;
  discrete Flag right;
equation
  left = right;
end LeftToRight;
model RightToLeft
  discrete Flag left;
  discrete Flag right;
equation
    right = left;
end RightToLeft;
"#;
    for model in ["LeftToRight", "RightToLeft"] {
        assert_ed019(source, model, "checked causal orientation");
    }
}

#[test]
fn nested_scalar_record_layout_is_partitioned_recursively() {
    let compiled = Compiler::new()
        .model("NestedMixed")
        .compile_str_dae(NESTED_MIXED, "nested_mixed.mo")
        .expect("nested scalar records retain one complete exact leaf plan");
    assert_eq!(compiled.balance_detail.continuous_equations, 1);
    assert_eq!(compiled.balance_detail.discrete_value_definitions, 2);
    compiled.dae.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 1);
        assert_eq!(view.discrete_value_owner_count(), 1);
        assert_eq!(
            view.discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
                .unwrap()
                .targets()
                .len(),
            2
        );
    });
}

#[test]
fn input_record_is_a_source_in_both_equation_orientations() {
    for model in ["InputAsSource", "SwappedInputAsSource"] {
        let compiled = Compiler::new()
            .model(model)
            .compile_str_dae(INPUT_AS_SOURCE, "input_as_source.mo")
            .unwrap_or_else(|error| panic!("{model} must select the output as target: {error}"));
        assert_eq!(compiled.balance_detail.continuous_equations, 1);
        assert_eq!(compiled.balance_detail.discrete_value_definitions, 1);
    }
}

#[test]
fn record_equality_without_a_definable_endpoint_fails_early() {
    let source = r#"
record Packet
  Real value;
  Boolean valid;
end Packet;
model NoTarget
  input Packet left;
  input Packet right;
equation
  left = right;
end NoTarget;
"#;
    assert_ed019(
        source,
        "NoTarget",
        "no state, algebraic, output, or discrete leaf",
    );
}

#[test]
fn record_arrays_and_initial_record_equations_fail_before_dae_mutation() {
    let arrays = r#"
record Packet
  Real value;
  Boolean valid;
end Packet;
model RecordArrays
  input Packet source[2];
  output Packet target[2];
equation
  target = source;
end RecordArrays;
"#;
    assert_ed019(arrays, "RecordArrays", "arrays of records");

    let nested_arrays = r#"
record Packet
  Boolean valid;
end Packet;
record Bundle
  Packet members[2];
end Bundle;
model NestedRecordArrays
  input Bundle source;
  output Bundle target;
equation
  target = source;
end NestedRecordArrays;
"#;
    assert_ed019(nested_arrays, "NestedRecordArrays", "nested record arrays");

    let initial = r#"
record Packet
  Real value;
  Boolean valid;
end Packet;
model InitialRecord
  output Packet target;
initial equation
  target = Packet(1.0, true);
equation
  target = Packet(2.0, false);
end InitialRecord;
"#;
    assert_ed019(initial, "InitialRecord", "whole-record initialization");
}

#[test]
fn same_layout_different_nominal_record_mutation_is_rejected() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let target = VarName::new("target");
    let original = flat.record_instances[&target].type_def_id;
    let forged = DefId::new(
        flat.record_types
            .keys()
            .map(DefId::index)
            .max()
            .unwrap_or(1)
            + 1,
    );
    flat.record_types
        .insert(forged, flat.record_types[&original].clone());
    flat.record_instances.get_mut(&target).unwrap().type_def_id = forged;
    let error = rumoca_phase_dae::construct(
        &flat,
        compiled
            .source_map
            .clone()
            .expect("DAE compile retains source map"),
    )
    .expect_err("same field spelling and layout cannot replace nominal record identity");
    assert!(
        error.to_string().contains("nominal type identities"),
        "{error}"
    );
}

#[test]
fn conflicting_record_occurrence_layout_mutation_is_rejected() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let target = flat.record_instances[&VarName::new("target")].instance_id;
    let class = flat
        .instance_relations
        .iter()
        .find_map(|(instance, relation)| {
            (relation.owner == Some(target) && relation.kind == rumoca_ir_flat::InstanceKind::Class)
                .then_some(*instance)
        })
        .expect("record occurrence has one class body");
    let field = flat
        .instance_relations
        .iter()
        .find_map(|(instance, relation)| {
            (relation.owner == Some(class)
                && relation.kind == rumoca_ir_flat::InstanceKind::Materialized)
                .then_some(*instance)
        })
        .expect("record class has one materialized field");
    flat.instance_relations.get_mut(&field).unwrap().declaration = Some(DefId::new(999_999));
    let error = rumoca_phase_dae::construct(
        &flat,
        compiled
            .source_map
            .clone()
            .expect("DAE compile retains source map"),
    )
    .expect_err("a forged occurrence edge cannot inherit the record layout certificate");
    assert!(
        error.to_string().contains("concrete occurrences"),
        "{error}"
    );
}

#[test]
fn swapped_leaf_instance_id_mutation_is_rejected() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let source = VarName::new("source.valid");
    let target = VarName::new("target.valid");
    let source_id = flat.variables[&source].instance_id;
    let target_id = flat.variables[&target].instance_id;
    flat.variables.get_mut(&source).unwrap().instance_id = target_id;
    flat.variables.get_mut(&target).unwrap().instance_id = source_id;
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("a plausible name cannot replace the checked leaf occurrence");
    assert!(
        error.to_string().contains("exact declaration identity"),
        "{error}"
    );
}

#[test]
fn nested_same_layout_different_nominal_mutation_is_rejected() {
    let source = r#"
record Inner
  Boolean valid;
end Inner;
record Outer
  Inner inner;
end Outer;
model NestedDirect
  input Outer source;
  output Outer target;
equation
  target = source;
end NestedDirect;
"#;
    let compiled = Compiler::new()
        .model("NestedDirect")
        .compile_str_dae(source, "nested_direct.mo")
        .expect("nested mutation seed compiles");
    let mut flat = (*compiled.flat).clone();
    let nested = VarName::new("target.inner");
    let original = flat.record_instances[&nested].type_def_id;
    let forged = fresh_def_id(&flat);
    flat.record_types
        .insert(forged, flat.record_types[&original].clone());
    let declared_type = flat.type_ids_by_def_id[&original];
    flat.type_ids_by_def_id.insert(forged, declared_type);
    flat.record_instances.get_mut(&nested).unwrap().type_def_id = forged;
    let nested_instance = flat.record_instances[&nested].instance_id;
    let class = exact_class_child(&flat, nested_instance, original);
    flat.instance_relations.get_mut(&class).unwrap().declaration = Some(forged);
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("nested nominal identity is part of every leaf certificate");
    assert!(
        error.to_string().contains("complete exact field layout"),
        "{error}"
    );
}

#[test]
fn wrong_record_class_and_effective_type_mutations_are_rejected() {
    let (compiled, mut wrong_class) = valid_direct_record_flat();
    let target = wrong_class.record_instances[&VarName::new("target")].clone();
    let class = exact_class_child(&wrong_class, target.instance_id, target.type_def_id);
    let forged_class = fresh_def_id(&wrong_class);
    wrong_class
        .instance_relations
        .get_mut(&class)
        .unwrap()
        .declaration = Some(forged_class);
    let error = to_dae_mutation(&compiled, &wrong_class)
        .expect_err("a class body with another declaration cannot own the record");
    assert!(error.to_string().contains("exact class bodies"), "{error}");

    let (compiled, mut wrong_effective) = valid_direct_record_flat();
    let real = wrong_effective.predefined_types.real;
    wrong_effective
        .record_instances
        .get_mut(&VarName::new("target"))
        .unwrap()
        .effective_type_id = real;
    let error = to_dae_mutation(&compiled, &wrong_effective)
        .expect_err("a primitive effective identity cannot authenticate a record occurrence");
    assert!(
        error.to_string().contains("nominal type identities"),
        "{error}"
    );
}

#[test]
fn record_occurrence_component_identity_is_distinct_from_nominal_type_identity() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let target = flat.record_instances[&VarName::new("target")].clone();
    let component_declaration = target.component_ref.target_def_id();
    assert_ne!(
        component_declaration, target.type_def_id,
        "a record occurrence is owned by its component declaration, not its record type"
    );
    to_dae_mutation(&compiled, &flat)
        .expect("distinct component and record-type DefIds form an ordinary checked occurrence");

    let mut parts = target.component_ref.parts().to_vec();
    parts.last_mut().unwrap().def_id = fresh_def_id(&flat);
    flat.record_instances
        .get_mut(&VarName::new("target"))
        .unwrap()
        .component_ref = target
        .component_ref
        .with_replaced_parts(parts)
        .expect("same spelling with a different exact identity remains representable");
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("same spelling cannot replace the component occurrence declaration");
    assert!(error.to_string().contains("declaration path"), "{error}");
}

#[test]
fn scalar_record_inside_arrayed_owner_preserves_ancestor_coordinates() {
    let source = r#"
record Packet
  Boolean valid;
end Packet;
model Sensor
  input Packet source;
  output Packet target;
equation
  target = source;
end Sensor;
model ArrayedOwner
  Sensor sensor[2];
equation
  sensor[1].source.valid = true;
  sensor[2].source.valid = false;
end ArrayedOwner;
"#;
    let compiled = Compiler::new()
        .model("ArrayedOwner")
        .compile_str_dae(source, "arrayed_record_owner.mo")
        .expect("ancestor array coordinates are part of the exact record occurrence path");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("arrayed Flat serializes");
    let _: rumoca_ir_flat::Model = serde_json::from_value(encoded)
        .expect("distinct sibling coordinates survive exact membership replay");

    let mut duplicate_coordinate = (*compiled.flat).clone();
    let relations = duplicate_coordinate
        .instance_relations
        .iter()
        .map(|(instance, relation)| (*instance, relation.clone()))
        .collect::<Vec<_>>();
    let (duplicate, claimed_indices) = relations
        .iter()
        .enumerate()
        .find_map(|(index, (_, first))| {
            relations[index + 1..]
                .iter()
                .find_map(|(second_id, second)| {
                    (first.owner.is_some()
                        && first.owner == second.owner
                        && first.declaration == second.declaration
                        && first.indices != second.indices)
                        .then(|| (*second_id, first.indices.clone()))
                })
        })
        .expect("the seed has two exact array siblings");
    duplicate_coordinate
        .instance_relations
        .get_mut(&duplicate)
        .unwrap()
        .indices = claimed_indices;
    assert_wire_error(
        &duplicate_coordinate,
        "two sibling occurrences claim one declaration coordinate",
    );

    let mut flat = (*compiled.flat).clone();
    let target_name = flat
        .record_instances
        .keys()
        .find(|name| name.as_str().contains("sensor[2].target"))
        .cloned()
        .expect("the second arrayed owner retains its scalar record occurrence");
    let record = flat.record_instances[&target_name].clone();
    assert!(
        record
            .component_ref
            .parts()
            .iter()
            .any(|part| !part.subs.is_empty()),
        "the acceptance seed exercises structured ancestor subscripts"
    );
    let mut parts = record.component_ref.parts().to_vec();
    let coordinate = parts
        .iter_mut()
        .find(|part| !part.subs.is_empty())
        .expect("the owner coordinate is explicit");
    coordinate.subs = vec![rumoca_core::Subscript::Index {
        value: 1,
        span: record.source_span,
    }];
    flat.record_instances
        .get_mut(&target_name)
        .unwrap()
        .component_ref = record
        .component_ref
        .with_replaced_parts(parts)
        .expect("the mismatched coordinate remains structurally valid");
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("a different ancestor coordinate cannot replay the record occurrence");
    assert!(error.to_string().contains("declaration path"), "{error}");
}

#[test]
fn duplicate_root_effective_type_identity_is_rejected() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let target = VarName::new("target");
    let original = flat.record_instances[&target].effective_type_id;
    let duplicate = fresh_type_id(&flat);
    flat.effective_types
        .insert(duplicate, flat.effective_types[&original].clone());
    flat.record_instances
        .get_mut(&target)
        .unwrap()
        .effective_type_id = duplicate;
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("an equal descriptor under another Flat TypeId is not the same identity");
    assert!(
        error.to_string().contains("nominal type identities"),
        "{error}"
    );
}

#[test]
fn equally_forged_leaf_effective_types_are_rejected_by_declared_layout() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let wrong = flat.predefined_types.real;
    for name in [VarName::new("source.valid"), VarName::new("target.valid")] {
        flat.variables.get_mut(&name).unwrap().type_id = wrong;
    }
    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("agreement between two forged leaves cannot replace declaration evidence");
    assert!(
        error.to_string().contains("exact declaration identity"),
        "{error}"
    );
}

#[test]
fn record_field_wire_requires_exact_declared_and_effective_type_evidence() {
    let (compiled, flat) = valid_direct_record_flat();
    let encoded = serde_json::to_value(&flat).expect("Flat serializes its checked record layout");

    let mut missing = encoded.clone();
    assert!(
        first_record_field_json(&mut missing)
            .remove("type_def_id")
            .is_some()
    );
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(missing)
        .expect_err("wire replay cannot invent a record field type declaration");
    assert!(
        error.to_string().contains("missing field `type_def_id`"),
        "{error}"
    );

    let mut missing = encoded.clone();
    assert!(
        first_record_field_json(&mut missing)
            .remove("effective_type")
            .is_some()
    );
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(missing)
        .expect_err("wire replay cannot invent a record field effective type");
    assert!(
        error.to_string().contains("missing field `effective_type`"),
        "{error}"
    );

    let mut unknown = encoded.clone();
    first_record_field_json(&mut unknown).insert("invented_type".to_string(), serde_json::json!(1));
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(unknown)
        .expect_err("wire replay rejects obsolete or invented record field evidence");
    assert!(
        error.to_string().contains("unknown field `invented_type`"),
        "{error}"
    );

    let mut wrong_nominal = encoded.clone();
    first_record_field_json(&mut wrong_nominal)["effective_type"]["nominal_type"] =
        serde_json::to_value(flat.predefined_types.real).unwrap();
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(wrong_nominal)
        .expect_err("wire replay checks the field's nominal type against its declaration");
    assert!(error.to_string().contains("invalid shape"), "{error}");

    let mut wrong_effective = encoded;
    first_record_field_json(&mut wrong_effective)["effective_type"]["canonical_type"] =
        serde_json::to_value(flat.predefined_types.real).unwrap();
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(wrong_effective)
        .expect_err("wire replay checks member occurrences against the exact field type");
    assert!(
        error.to_string().contains("declared or effective type"),
        "{error}"
    );

    drop(compiled);
}

#[test]
fn primitive_and_nested_record_flat_wire_roundtrip_remains_checked() {
    let source = r#"
type DeferredExternal = MissingLibrary.Value;

record Inner
  Real weight;
  Boolean enabled;
end Inner;

record Outer
  Inner inner;
  Integer count;
end Outer;

model NestedMixed
  output Outer value;
equation
  value = Outer(Inner(4.0, true), 3);
end NestedMixed;
"#;
    let compiled = Compiler::new()
        .model("NestedMixed")
        .compile_str_dae(source, "nested_record_wire.mo")
        .expect("unused deferred aliases do not poison a primitive+nested record model");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("Flat serializes");
    let decoded: rumoca_ir_flat::Model =
        serde_json::from_value(encoded).expect("the complete current Flat wire replays");
    assert_eq!(decoded.type_roots, compiled.flat.type_roots);
    assert_eq!(decoded.record_types, compiled.flat.record_types);
    let _product = rumoca_phase_dae::construct(
        &decoded,
        compiled
            .source_map
            .clone()
            .expect("wire seed retains sources"),
    )
    .expect("replayed primitive and nested records retain their DAE proof");
}

#[test]
fn flat_wire_rejects_missing_unknown_cyclic_and_wrong_type_roots() {
    let (compiled, flat) = valid_direct_record_flat();
    let mut missing = serde_json::to_value(&flat).unwrap();
    assert!(
        missing
            .as_object_mut()
            .unwrap()
            .remove("type_roots")
            .is_some()
    );
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(missing)
        .expect_err("the root authority is required wire evidence");
    assert!(
        error.to_string().contains("missing field `type_roots`"),
        "{error}"
    );

    let mut unknown = flat.clone();
    unknown.type_roots.insert(TypeId::UNKNOWN, TypeId::UNKNOWN);
    assert_type_root_wire_error(&unknown, "unknown, cyclic, or foreign");

    let record_type = flat
        .record_instances
        .values()
        .next()
        .expect("seed has a record")
        .effective_type_id;
    let leaf_type = flat
        .variables
        .values()
        .next()
        .expect("seed has a leaf")
        .type_id;
    assert_ne!(record_type, leaf_type);
    let mut cyclic = flat.clone();
    cyclic.type_roots.insert(record_type, leaf_type);
    cyclic.type_roots.insert(leaf_type, record_type);
    assert_type_root_wire_error(&cyclic, "unknown, cyclic, or foreign");

    let mut missing_mapping = flat.clone();
    missing_mapping.type_roots.shift_remove(&record_type);
    assert_type_root_wire_error(
        &missing_mapping,
        "unresolved identity, shape, or issued roots",
    );

    let mut wrong_root = flat.clone();
    wrong_root
        .type_roots
        .insert(record_type, flat.predefined_types.real);
    assert_type_root_wire_error(&wrong_root, "unresolved identity, shape, or issued roots");

    let mut foreign = flat;
    let foreign_type = fresh_type_id(&foreign);
    foreign.type_roots.insert(record_type, foreign_type);
    assert_type_root_wire_error(&foreign, "unknown, cyclic, or foreign");

    let mut foreign_key = compiled.flat.as_ref().clone();
    let foreign_type = fresh_type_id(&foreign_key);
    foreign_key.type_roots.insert(foreign_type, foreign_type);
    assert_type_root_wire_error(&foreign_key, "unissued foreign identity");
    drop(compiled);
}

#[test]
fn record_call_vs_call_and_structured_rows_fail_early() {
    let call_vs_call = r#"
record Packet
  Boolean valid;
end Packet;
model CallVsCall
equation
  Packet(true) = Packet(false);
end CallVsCall;
"#;
    assert_ed019(call_vs_call, "CallVsCall", "whole-record equality requires");

    let source = r#"
record Packet
  Boolean valid;
end Packet;
model StructuredSeed
  input Packet source;
  output Packet target;
  input Real supplied[2];
  Real values[2];
equation
  target = source;
  for i in 1:2 loop
    values[i] = supplied[i];
  end for;
end StructuredSeed;
"#;
    let compiled = Compiler::new()
        .model("StructuredSeed")
        .compile_str_dae(source, "structured_seed.mo")
        .expect("structured mutation seed compiles");
    let mut regular = (*compiled.flat).clone();
    let record_residual = regular
        .equations
        .iter()
        .find(|equation| {
            let mut references = Vec::new();
            equation.residual.collect_var_refs(&mut references);
            references.contains(&VarName::new("target"))
        })
        .expect("seed has one whole-record row")
        .residual
        .clone();
    let family = regular
        .structured_equations
        .first_mut()
        .expect("seed has one structured family");
    family
        .template
        .as_mut()
        .expect("structured family retains its template")
        .body[0] = record_residual.clone();
    let error = to_dae_mutation(&compiled, &regular)
        .expect_err("structured whole-record subtraction requires its own aggregate owner");
    assert!(error.to_string().contains("structured families"), "{error}");

    let mut initial = (*compiled.flat).clone();
    let mut family = initial.structured_equations[0].clone();
    family.template.as_mut().unwrap().body[0] = record_residual;
    initial.initial_structured_equations.push(family);
    let error = to_dae_mutation(&compiled, &initial)
        .expect_err("initial structured whole-record subtraction must also fail early");
    assert!(error.to_string().contains("structured families"), "{error}");
}

#[test]
fn leading_record_output_with_an_extra_result_has_one_checked_call_owner() {
    let source = r#"
record Packet
  Boolean valid;
end Packet;

function makePacket
  output Packet packet;
  output Real diagnostic;
algorithm
  packet.valid := true;
  diagnostic := 1.0;
end makePacket;

model RecordFirstOutput
  output Packet target;
equation
  target = makePacket();
end RecordFirstOutput;
"#;
    let compiled = Compiler::new()
        .model("RecordFirstOutput")
        .compile_str_dae(source, "record_first_output.mo")
        .expect("the exact leading record output owns the aggregate call result");
    assert_eq!(compiled.balance_detail.discrete_value_definitions, 1);

    let mut zero_output = (*compiled.flat).clone();
    let function = zero_output
        .functions
        .values_mut()
        .find(|function| function.name.as_str().ends_with("makePacket"))
        .expect("the called function remains in Flat");
    function.outputs.clear();
    let error = to_dae_mutation(&compiled, &zero_output)
        .expect_err("a semantic ordinary call cannot invent an absent first result");
    assert!(error.to_string().contains("no outputs"), "{error}");
    assert_wire_error(&zero_output, "ordinary function with no output slot");

    let mut renamed = (*compiled.flat).clone();
    let original_key = renamed
        .functions
        .iter()
        .find_map(|(key, function)| {
            function
                .name
                .as_str()
                .ends_with("makePacket")
                .then_some(key.clone())
        })
        .expect("called function has one map key");
    let mut function = renamed.functions.shift_remove(&original_key).unwrap();
    let renamed_key = VarName::new("semantic_rename_without_call_cache_rewrite");
    function.name = renamed_key.clone();
    renamed.functions.insert(renamed_key, function);
    to_dae_mutation(&compiled, &renamed)
        .expect("exact FunctionInstanceId lookup is independent of the function-map key spelling");

    let mut colliding_spelling = (*compiled.flat).clone();
    let key = colliding_spelling
        .functions
        .iter()
        .find_map(|(key, function)| {
            function
                .name
                .as_str()
                .ends_with("makePacket")
                .then_some(key.clone())
        })
        .unwrap();
    let mut impostor = colliding_spelling.functions.shift_remove(&key).unwrap();
    impostor.instance_id = Some(fresh_function_instance_id(&colliding_spelling));
    colliding_spelling.functions.insert(key, impostor);
    let error = to_dae_mutation(&compiled, &colliding_spelling)
        .expect_err("same spelling cannot substitute for the call's exact function instance");
    assert!(error.to_string().contains("semantic function"), "{error}");
}

#[test]
fn genuine_constructor_builtin_and_range_arguments_roundtrip_with_exact_shapes() {
    let source = r#"
record Packet
  Boolean valid;
end Packet;

function scalarValue
  input Real value;
  output Real result;
algorithm
  result := value;
end scalarValue;

function vectorValue
  input Real value[3];
  output Real result[3];
algorithm
  result := value;
end vectorValue;

function integerVector
  input Integer value[3];
  output Integer result[3];
algorithm
  result := value;
end integerVector;

model CheckedWireShapes
  Real scalar;
  Real vector[3];
  Integer integers[3];
  Packet packet;
equation
  scalar = scalarValue(sin(cos(1.0)));
  vector = vectorValue(zeros(3));
  integers = integerVector(1:3);
  packet = Packet(true);
end CheckedWireShapes;
"#;
    let compiled = Compiler::new()
        .model("CheckedWireShapes")
        .compile_str_dae(source, "checked_wire_shapes.mo")
        .expect("genuine scalar, shaped, range, and constructor calls compile");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("Flat serializes");
    let _: rumoca_ir_flat::Model = serde_json::from_value(encoded)
        .expect("checked wire shape replay accepts genuine compiler output");
}

#[test]
fn flat_wire_replays_equation_calls_after_closing_root_catalogs() {
    let compiled = Compiler::new()
        .model("WireCallCatalog")
        .compile_str_dae(WIRE_CALL_CATALOG, "wire_call_catalog.mo")
        .expect("exact semantic call seed compiles");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("Flat serializes");
    let _: rumoca_ir_flat::Model = serde_json::from_value(encoded)
        .expect("the closed callable and occurrence catalogs admit the exact call");
}

#[test]
fn flat_wire_equation_calls_fail_closed_on_catalog_mutations() {
    let compiled = Compiler::new()
        .model("WireCallCatalog")
        .compile_str_dae(WIRE_CALL_CATALOG, "wire_call_catalog_mutation.mo")
        .expect("call-catalog mutation seed compiles");

    let mut missing_callable = (*compiled.flat).clone();
    let callable = missing_callable
        .functions
        .iter()
        .find_map(|(name, function)| {
            function
                .name
                .as_str()
                .ends_with("exactValue")
                .then_some(name.clone())
        })
        .expect("the exact callable is retained in Flat");
    missing_callable.functions.shift_remove(&callable);
    assert_wire_error(&missing_callable, "resolved function instance is absent");

    let mut missing_occurrence = (*compiled.flat).clone();
    let result_occurrence = missing_occurrence.variables[&VarName::new("result")].instance_id;
    missing_occurrence
        .instance_relations
        .shift_remove(&result_occurrence);
    assert_wire_error(&missing_occurrence, "absent from the occurrence graph");
}

#[test]
fn exact_record_nominal_identity_survives_dae_and_wire_replay() {
    let (compiled, mut flat) = valid_direct_record_flat();
    let target = flat
        .record_instances
        .get(&VarName::new("target"))
        .expect("seed has the target record")
        .clone();
    let original = flat.effective_types[&target.effective_type_id].clone();
    let forged_nominal = fresh_type_id(&flat);
    flat.type_roots
        .insert(forged_nominal, original.canonical_type());
    flat.effective_types.insert(
        target.effective_type_id,
        EffectiveType::new(
            forged_nominal,
            original.canonical_type(),
            original.dimensions().to_vec(),
        )
        .expect("the forged descriptor is structurally representable"),
    );

    let error = to_dae_mutation(&compiled, &flat)
        .expect_err("canonical-root equality cannot replace exact nominal record identity");
    assert!(
        error.to_string().contains("nominal type identities"),
        "{error}"
    );
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(
        serde_json::to_value(&flat).expect("mutated Flat serializes"),
    )
    .expect_err("wire replay also requires the declaration's exact nominal record identity");
    assert!(error.to_string().contains("nominal"), "{error}");
}

#[test]
fn function_record_params_and_actuals_require_exact_layout_identity() {
    let source = r#"
record Expected
  Boolean valid;
end Expected;
record Other
  Boolean valid;
end Other;
function consume
  input Expected value;
  output Real result;
algorithm
  result := if value.valid then 1.0 else 0.0;
end consume;
model ExactCall
  input Expected source;
  input Other other;
  output Real result;
equation
  result = consume(source);
end ExactCall;
"#;
    let compiled = Compiler::new()
        .model("ExactCall")
        .compile_str_dae(source, "exact_record_call.mo")
        .expect("exact record call mutation seed compiles");
    let flat = (*compiled.flat).clone();

    let mut wrong_class = flat.clone();
    record_input_mut(&mut wrong_class, "consume").type_class = Some(ClassType::Model);
    assert_wire_error(&wrong_class, "record class");

    let mut wrong_layout = flat.clone();
    let other = wrong_layout.record_instances[&VarName::new("other")].type_def_id;
    let other_nominal = wrong_layout.type_ids_by_def_id[&other];
    let other_canonical = wrong_layout.type_roots[&other_nominal];
    let dimensions = record_input_mut(&mut wrong_layout, "consume")
        .effective_type
        .dimensions()
        .to_vec();
    let input = record_input_mut(&mut wrong_layout, "consume");
    input.type_def_id = Some(other);
    input.effective_type = EffectiveType::new(other_nominal, other_canonical, dimensions)
        .expect("the alternate exact record descriptor is structurally representable");
    assert_wire_error(&wrong_layout, "exact layout");

    let mut wrong_nominal = flat.clone();
    let param = record_input_mut(&mut wrong_nominal, "consume").clone();
    let forged_nominal = fresh_type_id(&wrong_nominal);
    let forged_declaration = fresh_def_id(&wrong_nominal);
    wrong_nominal
        .type_roots
        .insert(forged_nominal, param.effective_type.canonical_type());
    wrong_nominal
        .type_ids_by_def_id
        .insert(forged_declaration, forged_nominal);
    record_input_mut(&mut wrong_nominal, "consume").effective_type = EffectiveType::new(
        forged_nominal,
        param.effective_type.canonical_type(),
        param.effective_type.dimensions().to_vec(),
    )
    .expect("same-root alternate nominal descriptor is structurally representable");
    assert_wire_error(&wrong_nominal, "nominal");

    let mut wrong_actual = flat;
    let other_record = wrong_actual.record_instances[&VarName::new("other")].clone();
    let replacement = Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(other_record.component_ref)
            .with_instance_id(other_record.instance_id),
        subscripts: Vec::new(),
        span: other_record.source_span,
    };
    replace_first_call_argument(&mut wrong_actual, "consume", replacement);
    assert_wire_error(&wrong_actual, "record identity");
}

#[test]
fn forged_call_shapes_and_unsupported_shape_evidence_are_rejected() {
    let source = r#"
function vectorValue
  input Real value[3];
  output Real result[3];
algorithm
  result := value;
end vectorValue;
model ShapeMutation
  Real vector[3];
equation
  vector = vectorValue(zeros(3));
end ShapeMutation;
"#;
    let compiled = Compiler::new()
        .model("ShapeMutation")
        .compile_str_dae(source, "shape_mutation.mo")
        .expect("shape mutation seed compiles");

    let mut wrong_extent = (*compiled.flat).clone();
    replace_first_call_argument(
        &mut wrong_extent,
        "vectorValue",
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args: vec![Expression::Literal {
                value: Literal::Integer(2),
                span: compiled.flat.functions.values().next().unwrap().span,
            }],
            span: compiled.flat.functions.values().next().unwrap().span,
        },
    );
    assert_wire_error(&wrong_extent, "argument shape");

    let mut unsupported = (*compiled.flat).clone();
    let span = unsupported.functions.values().next().unwrap().span;
    replace_first_call_argument(&mut unsupported, "vectorValue", Expression::Empty { span });
    assert_wire_error(&unsupported, "shape authority");
}

#[test]
fn binary_wire_shapes_distinguish_matrix_product_and_elementwise_operators() {
    let source = r#"
function matrixValue
  input Real value[2, 4];
  output Real result[2, 4];
algorithm
  result := value;
end matrixValue;
model BinaryShapes
  Real product[2, 4];
  Real elementwise[2, 4];
equation
  product = matrixValue(zeros(2, 3) * zeros(3, 4));
  elementwise = matrixValue(zeros(2, 4) .* 2.0);
end BinaryShapes;
"#;
    let compiled = Compiler::new()
        .model("BinaryShapes")
        .compile_str_dae(source, "binary_shapes.mo")
        .expect("matrix product and elementwise broadcast compile with distinct exact shapes");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("Flat serializes");
    let _: rumoca_ir_flat::Model = serde_json::from_value(encoded)
        .expect("wire replay preserves matrix product and elementwise result shapes");

    let span = compiled.flat.functions.values().next().unwrap().span;
    let mut incompatible = (*compiled.flat).clone();
    replace_first_call_argument(
        &mut incompatible,
        "matrixValue",
        binary_expression(
            rumoca_core::OpBinary::Mul,
            zeros_expression(&[2, 3], span),
            zeros_expression(&[2, 4], span),
            span,
        ),
    );
    assert_wire_error(&incompatible, "matrix-product operands");

    let mut matrix_power = (*compiled.flat).clone();
    replace_first_call_argument(
        &mut matrix_power,
        "matrixValue",
        binary_expression(
            rumoca_core::OpBinary::Exp,
            zeros_expression(&[2, 4], span),
            Expression::Literal {
                value: Literal::Integer(2),
                span,
            },
            span,
        ),
    );
    assert_wire_error(&matrix_power, "scalar shape");
}

#[test]
fn record_field_shape_replay_requires_exact_declaration_catalog_identity() {
    let source = r#"
record Expected
  Boolean valid;
end Expected;
record Other
  Boolean valid;
end Other;
function consume
  input Expected value;
  output Boolean result;
algorithm
  result := value.valid;
end consume;
model FieldShapeIdentity
  input Expected source;
  output Boolean result;
equation
  result = consume(source);
end FieldShapeIdentity;
"#;
    let compiled = Compiler::new()
        .model("FieldShapeIdentity")
        .compile_str_dae(source, "field_shape_identity.mo")
        .expect("exact record-field shape seed compiles");
    let mut wrong_declaration = (*compiled.flat).clone();
    let expected = record_input_mut(&mut wrong_declaration, "consume")
        .type_def_id
        .expect("record formal retains its declaration");
    let other = wrong_declaration
        .record_types
        .iter()
        .find_map(|(declaration, layout)| (layout.name.ends_with("Other")).then_some(*declaration))
        .expect("same-layout alternate record is retained");
    wrong_declaration
        .record_types
        .get_mut(&expected)
        .unwrap()
        .fields[0]
        .type_def_id = other;
    assert_wire_error(&wrong_declaration, "declared and effective type identities");

    let mut missing_catalog = (*compiled.flat).clone();
    let field_type = missing_catalog.record_types[&expected].fields[0].type_def_id;
    missing_catalog.type_ids_by_def_id.shift_remove(&field_type);
    assert_wire_error(&missing_catalog, "declaration");
}

#[test]
fn statement_call_outputs_require_exact_record_identity() {
    let source = r#"
record Expected
  Boolean valid;
end Expected;
record Other
  Boolean valid;
end Other;
function make
  output Expected value;
  output Real diagnostic;
algorithm
  value.valid := true;
  diagnostic := 1.0;
end make;
model StatementOutputs
  output Expected target;
  output Real diagnostic;
  Other other;
algorithm
  (target, diagnostic) := make();
end StatementOutputs;
"#;
    let compiled = Compiler::new()
        .model("StatementOutputs")
        .compile_str_dae(source, "statement_record_outputs.mo")
        .expect("exact record and scalar statement outputs compile");
    let mut wrong_record = (*compiled.flat).clone();
    let other = wrong_record.record_instances[&VarName::new("other")]
        .component_ref
        .clone();
    statement_call_outputs_mut(&mut wrong_record)[0] = Some(other.clone());
    assert_wire_error(&wrong_record, "shape or record identity");

    let mut record_to_scalar = (*compiled.flat).clone();
    let scalar = record_to_scalar.variables[&VarName::new("diagnostic")]
        .component_ref
        .clone()
        .expect("scalar output retains exact structured identity");
    statement_call_outputs_mut(&mut record_to_scalar)[0] = Some(scalar);
    assert_wire_error(&record_to_scalar, "shape or record identity");

    let mut scalar_to_record = (*compiled.flat).clone();
    statement_call_outputs_mut(&mut scalar_to_record)[1] = Some(other);
    assert_wire_error(&scalar_to_record, "shape or record identity");
}

#[test]
fn foreign_record_subtrees_and_wide_deep_layouts_use_one_membership_index() {
    let (compiled, mut foreign) = valid_direct_record_flat();
    let target = foreign.record_instances[&VarName::new("target")].clone();
    let target_class = exact_class_child(&foreign, target.instance_id, target.type_def_id);
    let target_leaf_name = VarName::new("target.valid");
    let mut injected = foreign.variables[&target_leaf_name].clone();
    let injected_id = fresh_instance_id(&foreign);
    let injected_declaration = fresh_def_id(&foreign);
    let mut parts = injected
        .component_ref
        .as_ref()
        .expect("seed leaf retains its structured path")
        .parts()
        .to_vec();
    let field = parts.last_mut().expect("seed leaf path is nonempty");
    field.ident = "foreign".to_string();
    field.def_id = injected_declaration;
    let component_ref = injected
        .component_ref
        .as_ref()
        .unwrap()
        .with_replaced_parts(parts)
        .expect("the injected path remains structurally valid");
    let injected_name = component_ref.to_var_name();
    injected.instance_id = injected_id;
    injected.name = injected_name.clone();
    injected.component_ref = Some(component_ref);
    foreign.variables.insert(injected_name, injected);
    let mut relation =
        foreign.instance_relations[&foreign.variables[&target_leaf_name].instance_id].clone();
    relation.owner = Some(target_class);
    relation.declaration = Some(injected_declaration);
    foreign.instance_relations.insert(injected_id, relation);
    let error = to_dae_mutation(&compiled, &foreign)
        .expect_err("a foreign but graph-connected subtree cannot join a record occurrence");
    assert!(error.to_string().contains("exact fields"), "{error}");
    assert_wire_error(&foreign, "child inventory");

    let mut source = String::from("record Wide\n");
    for index in 0..128 {
        source.push_str(&format!("  Boolean field{index};\n"));
    }
    source.push_str("end Wide;\nrecord Level0\n  Boolean value;\nend Level0;\n");
    for index in 1..40 {
        source.push_str(&format!(
            "record Level{index}\n  Level{} nested;\nend Level{index};\n",
            index - 1
        ));
    }
    source.push_str(
        "model WideDeep\n  input Wide wideSource;\n  output Wide wideTarget;\n  input Level39 deepSource;\n  output Level39 deepTarget;\nequation\n  wideTarget = wideSource;\n  deepTarget = deepSource;\nend WideDeep;\n",
    );
    let compiled = Compiler::new()
        .model("WideDeep")
        .compile_str_dae(&source, "wide_deep_record.mo")
        .expect("wide and deep scalar record layouts retain linear checked membership");
    let encoded = serde_json::to_value(compiled.flat.as_ref()).expect("stress Flat serializes");
    let _: rumoca_ir_flat::Model =
        serde_json::from_value(encoded).expect("wide/deep occurrence replay remains exact");
}

fn valid_direct_record_flat() -> (
    rumoca_compile::compile::DaeCompilationResult,
    rumoca_ir_flat::Model,
) {
    let source = r#"
record Packet
  Boolean valid;
end Packet;
model ValidDirect
  input Packet source;
  output Packet target;
equation
  target = source;
end ValidDirect;
"#;
    let compiled = Compiler::new()
        .model("ValidDirect")
        .compile_str_dae(source, "valid_direct.mo")
        .expect("mutation seed compiles");
    let flat = (*compiled.flat).clone();
    (compiled, flat)
}

fn to_dae_mutation(
    compiled: &rumoca_compile::compile::DaeCompilationResult,
    flat: &rumoca_ir_flat::Model,
) -> Result<rumoca_ir_dae::Dae, rumoca_phase_dae::ToDaeError> {
    rumoca_phase_dae::construct(
        flat,
        compiled
            .source_map
            .clone()
            .expect("DAE compile retains source map"),
    )
    .map(|product| product.into_parts().0)
}

fn fresh_def_id(flat: &rumoca_ir_flat::Model) -> DefId {
    let declared = flat
        .record_types
        .keys()
        .chain(flat.type_ids_by_def_id.keys())
        .map(DefId::index);
    let fields = flat
        .record_types
        .values()
        .flat_map(|record| record.fields.iter().map(|field| field.def_id.index()));
    DefId::new(declared.chain(fields).max().unwrap_or(1) + 1)
}

fn fresh_type_id(flat: &rumoca_ir_flat::Model) -> TypeId {
    TypeId::new(
        flat.effective_types
            .keys()
            .chain(flat.type_roots.keys())
            .map(TypeId::index)
            .max()
            .unwrap_or(1)
            + 1,
    )
}

fn fresh_function_instance_id(flat: &rumoca_ir_flat::Model) -> rumoca_core::FunctionInstanceId {
    rumoca_core::FunctionInstanceId::new(
        flat.functions
            .values()
            .filter_map(|function| function.instance_id)
            .map(rumoca_core::FunctionInstanceId::index)
            .max()
            .unwrap_or(0)
            + 1,
    )
}

fn fresh_instance_id(flat: &rumoca_ir_flat::Model) -> rumoca_core::InstanceId {
    rumoca_core::InstanceId::new(
        flat.instance_relations
            .keys()
            .map(|instance| instance.index())
            .max()
            .unwrap_or(0)
            + 1,
    )
}

fn first_record_field_json(
    encoded: &mut serde_json::Value,
) -> &mut serde_json::Map<String, serde_json::Value> {
    encoded["record_types"]
        .as_object_mut()
        .and_then(|types| types.values_mut().next())
        .and_then(|record| record["fields"].as_array_mut())
        .and_then(|fields| fields.first_mut())
        .and_then(serde_json::Value::as_object_mut)
        .expect("mutation seed serializes one record field")
}

fn assert_type_root_wire_error(flat: &rumoca_ir_flat::Model, detail: &str) {
    let encoded = serde_json::to_value(flat).expect("mutated Flat serializes");
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(encoded)
        .expect_err("contradictory root evidence cannot replay");
    assert!(error.to_string().contains(detail), "{error}");
}

fn assert_wire_error(flat: &rumoca_ir_flat::Model, detail: &str) {
    let encoded = serde_json::to_value(flat).expect("mutated Flat serializes");
    let error = serde_json::from_value::<rumoca_ir_flat::Model>(encoded)
        .expect_err("contradictory semantic evidence cannot replay");
    assert!(error.to_string().contains(detail), "{error}");
}

fn record_input_mut<'a>(
    flat: &'a mut rumoca_ir_flat::Model,
    function_suffix: &str,
) -> &'a mut rumoca_core::FunctionParam {
    flat.functions
        .values_mut()
        .find(|function| function.name.as_str().ends_with(function_suffix))
        .and_then(|function| function.inputs.first_mut())
        .expect("mutation seed has one record input")
}

fn statement_call_outputs_mut(
    flat: &mut rumoca_ir_flat::Model,
) -> &mut Vec<Option<rumoca_core::ComponentReference>> {
    flat.algorithms
        .iter_mut()
        .flat_map(|algorithm| &mut algorithm.statements)
        .find_map(|statement| match statement {
            rumoca_core::Statement::FunctionCall { outputs, .. } => Some(outputs),
            _ => None,
        })
        .expect("mutation seed has one statement function call")
}

fn zeros_expression(dimensions: &[i64], span: Span) -> Expression {
    Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Zeros,
        args: dimensions
            .iter()
            .map(|dimension| Expression::Literal {
                value: Literal::Integer(*dimension),
                span,
            })
            .collect(),
        span,
    }
}

fn binary_expression(
    op: rumoca_core::OpBinary,
    lhs: Expression,
    rhs: Expression,
    span: Span,
) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn replace_first_call_argument(
    flat: &mut rumoca_ir_flat::Model,
    function_suffix: &str,
    replacement: Expression,
) {
    let mut rewriter = FirstCallArgumentRewriter {
        function_suffix,
        replacement: Some(replacement),
        replaced: false,
    };
    for equation in &mut flat.equations {
        equation.residual = rewriter.rewrite_expression(&equation.residual);
    }
    assert!(rewriter.replaced, "mutation seed has the requested call");
}

struct FirstCallArgumentRewriter<'a> {
    function_suffix: &'a str,
    replacement: Option<Expression>,
    replaced: bool,
}

impl ExpressionRewriter for FirstCallArgumentRewriter<'_> {
    fn walk_function_call_expression(
        &mut self,
        name: &Reference,
        arguments: &[Expression],
        is_constructor: bool,
        call_kind: FunctionCallKind,
        span: Span,
    ) -> Expression {
        let mut arguments = self.rewrite_expressions(arguments);
        if !self.replaced && name.as_str().ends_with(self.function_suffix) {
            assert!(!arguments.is_empty(), "mutation call has one input");
            arguments[0] = self
                .replacement
                .take()
                .expect("one replacement is consumed exactly once");
            self.replaced = true;
        }
        Expression::FunctionCall {
            name: name.clone(),
            args: arguments,
            is_constructor,
            call_kind,
            span,
        }
    }
}

fn exact_class_child(
    flat: &rumoca_ir_flat::Model,
    owner: rumoca_core::InstanceId,
    declaration: DefId,
) -> rumoca_core::InstanceId {
    flat.instance_relations
        .iter()
        .find_map(|(instance, relation)| {
            (relation.owner == Some(owner)
                && relation.kind == rumoca_ir_flat::InstanceKind::Class
                && relation.declaration == Some(declaration))
            .then_some(*instance)
        })
        .expect("mutation seed has one exact record class body")
}

fn assert_ed019(source: &str, model: &str, detail: &str) {
    let error = Compiler::new()
        .model(model)
        .compile_str_dae(source, "unsupported_record_equation.mo")
        .expect_err("unsupported record ownership must fail before DAE construction");
    match error {
        CompilerError::CompileDiagnosticsError { failures, .. } => assert!(
            failures.iter().any(|failure| {
                failure.phase == Some(FailedPhase::ToDae)
                    && failure.error_code.as_deref() == Some("ED019")
                    && failure.error.contains(detail)
                    && failure.primary_label.is_some()
            }),
            "missing exact ED019 `{detail}` failure: {failures:?}"
        ),
        other => panic!("expected structured ToDae refusal, got {other:?}"),
    }
}
