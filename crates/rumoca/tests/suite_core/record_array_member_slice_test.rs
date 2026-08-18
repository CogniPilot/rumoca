//! Regression: a connector/record-array member slice such as `pin[:].v`
//! must scalarize into the per-element component variables `pin[k].v`
//! (MLS §10.5.1 slice of an array of components).
//!
//! Before the fix, structural scalarization split the array equation into
//! per-element rows but left the colon slice in each residual, and the Solve
//! lowering rejected it with "slice subscript `:` is unsupported". This is
//! the MSL PowerConverters `vAC = ac.pin[:].v` pattern.

use rumoca::Compiler;
use rumoca_core::Expression;
use rumoca_ir_dae::{DaeGeneration, DaeProvenanceOrigin};
use rumoca_sim::{SimOptions, eval_dae_at};

const SLICE_MEMBER_MODEL: &str = r#"
within;
connector Pin
  Real v;
  flow Real i;
end Pin;
model SliceMember
  Pin pin[3];
  Real v[3] = pin[:].v;
  Real x(start = 0, fixed = true);
equation
  for k in 1:3 loop
    pin[k].v = k * 10.0;
  end for;
  der(x) = v[2];
end SliceMember;
"#;

#[test]
fn record_array_member_slice_scalarizes_per_element() {
    let flat = Compiler::new()
        .model("SliceMember")
        .compile_str_flat(SLICE_MEMBER_MODEL, "SliceMember.mo")
        .expect("member slice model should compile to Flat");
    let binding = flat
        .variables
        .get(&rumoca_core::VarName::new("v"))
        .and_then(|variable| variable.binding.as_ref())
        .expect("v retains its Flat binding");
    let (base, field, field_def_id, field_span) =
        first_projection(binding).expect("member slice retains an exact field projection");
    let scope = base
        .instance_id()
        .expect("Flat projection base retains exact instance scope");
    let component_ref = base
        .component_ref()
        .expect("Flat projection base retains its resolved component path");
    assert!(
        flat.instance_relations.contains_key(&scope),
        "reference scope must belong to this Flat model"
    );
    assert_eq!(
        component_ref.root_def_id(),
        component_ref.parts()[0].def_id,
        "projection root identity is the first path-segment identity"
    );
    assert_eq!(
        component_ref.target_def_id(),
        component_ref.parts()[component_ref.parts().len() - 1].def_id,
        "projection target identity is the final path-segment identity"
    );
    assert!(
        component_ref
            .parts()
            .iter()
            .all(|part| !part.span.is_dummy()),
        "every written projection segment retains source provenance"
    );
    assert_eq!(field, "v");
    assert_ne!(
        field_def_id,
        component_ref.target_def_id(),
        "the field declaration is distinct from its component-array base"
    );
    assert!(
        !field_span.is_dummy(),
        "the projected field occurrence retains source provenance"
    );

    let compiled = Compiler::new()
        .model("SliceMember")
        .compile_str(SLICE_MEMBER_MODEL, "SliceMember.mo")
        .expect("member slice model should compile to DAE");

    let projected_expressions = compiled.dae.inspect(|dae| {
        (0..dae.expression_count())
            .filter_map(|index| {
                let expression = dae.expression(dae.expression_id(index)?)?;
                (expression.provenance().origin()
                    == DaeProvenanceOrigin::Generated(DaeGeneration::RecordEquationProjection))
                .then_some(expression.provenance())
            })
            .collect::<Vec<_>>()
    });
    assert!(
        projected_expressions
            .iter()
            .any(|provenance| compiled.dae.source_text(*provenance) == Some("pin[:].v")),
        "record projection expressions must retain the exact source occurrence"
    );

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("member slice model should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    let der_x = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .unwrap_or_else(|| {
            panic!(
                "missing der(x); have: {:?}",
                report
                    .derivatives
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value;
    // v[2] = pin[2].v = 20.0
    assert_eq!(der_x, 20.0, "v = pin[:].v must select per-element values");
}

/// A slice written on a middle part of a path: `ac.pin[:].v` selects the member
/// `v` of the connector array `pin` declared *inside* `ac` (MLS §10.5), so the
/// sliced array is not the head of the path. This is the MSL
/// `Electrical.PowerConverters.Interfaces.ACDC.ACplug` shape.
///
/// Two sibling converters project the same declarations from different
/// instances, so the projection must be settled by the occurrence graph and not
/// by the two identical spellings.
const NESTED_SLICE_MEMBER_MODEL: &str = r#"
within;
connector Pin
  Real v;
  flow Real i;
end Pin;
connector Plug
  parameter Integer m = 3;
  Pin pin[m];
end Plug;
model Converter
  Plug ac;
  Real vAC[3] = ac.pin[:].v;
end Converter;
model NestedSliceMember
  Converter left;
  Converter right;
  Real x(start = 0, fixed = true);
equation
  for k in 1:3 loop
    left.ac.pin[k].v = k * 10.0;
    right.ac.pin[k].v = k * 100.0;
  end for;
  der(x) = left.vAC[2] + right.vAC[3];
end NestedSliceMember;
"#;

#[test]
fn nested_component_array_member_slice_selects_its_own_instance() {
    let compiled = Compiler::new()
        .model("NestedSliceMember")
        .compile_str(NESTED_SLICE_MEMBER_MODEL, "NestedSliceMember.mo")
        .expect("a slice on a nested component array should compile to DAE");

    let projected = compiled.dae.inspect(|dae| {
        (0..dae.expression_count())
            .filter_map(|index| {
                let expression = dae.expression(dae.expression_id(index)?)?;
                (expression.provenance().origin()
                    == DaeProvenanceOrigin::Generated(DaeGeneration::RecordEquationProjection))
                .then_some(expression.provenance())
            })
            .collect::<Vec<_>>()
    });
    assert!(
        projected
            .iter()
            .any(|provenance| compiled.dae.source_text(*provenance) == Some("ac.pin[:].v")),
        "the nested projection must retain its exact source occurrence"
    );

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("nested member slice should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    let der_x = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .expect("model defines der(x)")
        .value;
    // left.vAC[2] = left.ac.pin[2].v = 20.0, right.vAC[3] = right.ac.pin[3].v = 300.0.
    // A projection that crossed instances would read 200.0 or 30.0 instead.
    assert_eq!(
        der_x, 320.0,
        "each converter's slice must project its own instance's pins"
    );
}

/// The sliced array reaches its owner through an `extends`: `pin` is declared in
/// `Plug` and inherited into `PositivePlug`, so the occurrence graph puts a
/// class occurrence between the `ac` component and the `pin` elements. An
/// `extends` adds class occurrences that no part of the reference spells
/// (MLS §7.1), so the projection proof has to step over them to line its chain
/// up with the written parts `ac`, `pin`, `v`.
///
/// This is the exact MSL `Polyphase.Interfaces.PositivePlug extends Plug` shape,
/// and it is the second thing the chain walk fixed: before it, this model was
/// rejected with ED019 even though the equivalent non-inherited spelling
/// compiled.
const INHERITED_PLUG_SLICE_MODEL: &str = r#"
within;
connector Pin
  Real v;
  flow Real i;
end Pin;
connector Plug
  parameter Integer m = 3;
  Pin pin[m];
end Plug;
connector PositivePlug
  extends Plug;
end PositivePlug;
model Probe
  PositivePlug ac;
  Real y[3] = ac.pin[:].v;
end Probe;
model InheritedPlugSlice
  Probe probe;
  Real x(start = 0, fixed = true);
equation
  probe.ac.pin[1].v = 10.0;
  probe.ac.pin[2].v = 20.0;
  probe.ac.pin[3].v = 30.0;
  der(x) = probe.y[2];
end InheritedPlugSlice;
"#;

#[test]
fn member_slice_steps_over_the_class_occurrences_an_extends_adds() {
    let compiled = Compiler::new()
        .model("InheritedPlugSlice")
        .compile_str(INHERITED_PLUG_SLICE_MODEL, "InheritedPlugSlice.mo")
        .expect("a slice through an inherited connector array should compile to DAE");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("inherited plug slice should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    let der_x = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .expect("model defines der(x)")
        .value;
    // probe.y = {10, 20, 30}, so probe.y[2] = 20. An off-by-one in the chain
    // walk would select pin[1] or pin[3] and read 10 or 30 instead.
    assert_eq!(
        der_x, 20.0,
        "the slice must project the inherited pin array in declaration order"
    );
}

fn first_projection(
    expression: &Expression,
) -> Option<(
    &rumoca_core::Reference,
    &str,
    rumoca_core::DefId,
    rumoca_core::Span,
)> {
    match expression {
        Expression::Binary { lhs, rhs, .. } => {
            first_projection(lhs).or_else(|| first_projection(rhs))
        }
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => {
            first_projection(rhs)
        }
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => first_reference(base)
            .map(|reference| (reference, field.as_str(), *field_def_id, *span))
            .or_else(|| first_projection(base)),
        _ => None,
    }
}

fn first_reference(expression: &Expression) -> Option<&rumoca_core::Reference> {
    match expression {
        Expression::VarRef { name, .. } => Some(name),
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => first_reference(rhs),
        _ => None,
    }
}
