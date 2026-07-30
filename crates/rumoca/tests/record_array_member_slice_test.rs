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
