//! Coverage for the homogeneity gate that decides whether an array component
//! keeps a compact family (SPEC_0032 §1).
//!
//! The gate is the only thing standing between a genuinely per-element array and
//! a wrong model, so each input it must refuse is exercised here, together with
//! the domain probing that feeds it.
//!
//! The gate's verdict itself is deliberately not observable: SPEC_0032 §1
//! requires compaction to produce the same overlay as element-by-element
//! expansion, so nothing in Instance IR distinguishes the two paths. What the
//! gate protects — that a genuinely per-element array is not collapsed onto its
//! template — is exactly what `assert_overlays_equivalent` checks, so each case
//! below pins the per-element content plus that differential.
//!
//! That covers safety but not liveness: the differential stays trivially true
//! if the gate silently stops compacting anything at all, so nothing here would
//! notice the optimization regressing to element-by-element expansion. Closing
//! that needs an out-of-band compaction counter, which SPEC_0032 §1 permits and
//! records as a follow-up; it must not come back as IR state.

use super::homogeneous_family_tests::{assert_overlays_equivalent, instantiate};
use crate::array_expansion::domain_probe_tuples;

fn domain(uppers: &[i64]) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: uppers
            .iter()
            .enumerate()
            .map(|(position, upper)| rumoca_core::StructuredIndexBinder {
                id: position,
                display_name: format!("i{position}"),
                lower: 1,
                upper: *upper,
                step: 1,
            })
            .collect(),
    }
}

#[test]
fn domain_probe_tuples_covers_both_ends_of_the_domain() {
    // The gate runs each per-element rewrite at the first *and* last domain
    // point, so an index-dependent rewrite that is invisible at ordinal 0 is
    // still observed.
    assert_eq!(
        domain_probe_tuples(&domain(&[2, 3])).expect("rank-2 domain"),
        vec![vec![1, 1], vec![2, 3]]
    );
    assert_eq!(
        domain_probe_tuples(&domain(&[4])).expect("rank-1 domain"),
        vec![vec![1], vec![4]]
    );
}

#[test]
fn domain_probe_tuples_deduplicates_a_single_point_domain() {
    // A one-element domain has the same first and last tuple; probing it twice
    // would double the gate's work for no extra information.
    assert_eq!(
        domain_probe_tuples(&domain(&[1])).expect("single point domain"),
        vec![vec![1]]
    );
}

#[test]
fn domain_probe_tuples_of_an_empty_domain_is_empty() {
    assert!(
        domain_probe_tuples(&domain(&[0]))
            .expect("empty domain")
            .is_empty()
    );
}

#[test]
fn nested_class_modification_keeps_scalar_expansion() {
    // Reaches `HomogeneityVerdict::Scalar("indexed nested modifier")`: a
    // non-`each` nested class modification is rewritten per element by
    // `index_nested_modification_for_element`, so the gate refuses the family.
    //
    // This particular modification happens to be index-independent, so the
    // refusal is conservative rather than necessary — see SPEC_0032 §1.
    const SOURCE: &str = r"
        model Inner
            parameter Real R = 1.0;
        end Inner;
        model Cell
            Inner sub;
        end Cell;
        model Stack
            Cell c[3](sub(R = 2.0));
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "Stack", false));
    for index in 1..=3 {
        let path = format!("c[{index}].sub.R");
        let data = compact
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing {path}"));
        assert!(
            format!("{:?}", data.binding).contains("2.0"),
            "expected {path} to bind 2.0"
        );
    }
}

#[test]
fn component_reference_modifier_to_an_array_keeps_scalar_expansion() {
    // Reaches `HomogeneityVerdict::Scalar("indexed component-reference
    // modifier")`. `pre_resolve_array_modifications` deliberately leaves
    // component references symbolic, so this modifier is invisible to the
    // `resolved_mods` gate and only the probe catches it: element `k` really
    // does get `R = Rs[k]`.
    const SOURCE: &str = r"
        model Cell
            parameter Real R = 1.0;
        end Cell;
        model Stack
            parameter Real Rs[3] = {1.0, 2.0, 3.0};
            Cell c[3](R = Rs);
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "Stack", false));
    for index in 1..=3 {
        let path = format!("c[{index}].R");
        let data = compact
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing {path}"));
        let rendered = format!("{:?}", data.binding);
        assert!(
            rendered.contains(&format!("Rs[{index}]")),
            "expected {path} to be bound to Rs[{index}], got {rendered}"
        );
    }
}

#[test]
fn scalar_component_reference_modifier_stays_compact() {
    // The negative branch of the probe: a modifier naming a *scalar* parent
    // component has no array part to index, so `index_binding_for_element`
    // returns an `ArrayIndex` wrapper that `distribute_component_ref_mods_for_element`
    // discards. Every element is identical and the family survives.
    const SOURCE: &str = r"
        model Cell
            parameter Real R = 1.0;
        end Cell;
        model Stack
            parameter Real Rref = 7.0;
            Cell c[3](R = Rref);
        end Stack;
    ";
    let compact = instantiate(SOURCE, "Stack", true);
    assert_overlays_equivalent(&compact, &instantiate(SOURCE, "Stack", false));
    for index in 1..=3 {
        let path = format!("c[{index}].R");
        let data = compact
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == path)
            .unwrap_or_else(|| panic!("missing {path}"));
        assert!(
            format!("{:?}", data.binding).contains("Rref"),
            "expected {path} to bind Rref"
        );
    }
}

#[test]
fn enclosing_scope_modification_compacts_exactly_as_scalar_expansion_does() {
    // Every per-element rewrite lives in `prepare_element_declaration`, and its
    // only inputs are the array component's own `start`, the resolved
    // `original_binding` (which *does* consult `ctx.mod_env()`), and
    // `comp.modifications` — the three things the gate inspects.
    //
    // A class modification arriving from an enclosing scope, as here, reaches
    // none of them: rumoca does not currently distribute `s(c(R = {...}))` over
    // the elements of `c` in either path. So the gate compacting this array is
    // not a divergence — the compact and scalar overlays agree, wrong
    // distribution and all. Pinning that keeps the "gate == negation of the
    // per-element rewrites" claim honest: if element distribution ever learns
    // this case, this test fails and the gate must learn it too.
    const SOURCE: &str = r"
        model Cell
            parameter Real R = 1.0;
        end Cell;
        model Stack
            Cell c[3];
        end Stack;
        model Top
            Stack s(c(R = {1.0, 2.0, 3.0}));
        end Top;
    ";
    let compact = instantiate(SOURCE, "Top", true);
    let scalar = instantiate(SOURCE, "Top", false);
    assert_overlays_equivalent(&compact, &scalar);
    for index in 1..=3 {
        let path = format!("s.c[{index}].R");
        let rendered = |overlay: &rumoca_ir_ast::InstanceOverlay| {
            let data = overlay
                .components
                .values()
                .find(|data| data.qualified_name.to_flat_string() == path)
                .unwrap_or_else(|| panic!("missing {path}"));
            format!("{:?}", data.binding)
        };
        assert_eq!(rendered(&compact), rendered(&scalar), "{path} differs");
    }
}
