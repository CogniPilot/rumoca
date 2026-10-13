use super::*;

struct CountedValue<'count> {
    value: usize,
    comparisons: &'count std::sync::atomic::AtomicUsize,
}

impl Eq for CountedValue<'_> {}

impl PartialEq for CountedValue<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.comparisons
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.value == other.value
    }
}

#[test]
fn shared_call_slices_do_not_rewalk_elements() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let comparisons = AtomicUsize::new(0);
    let make_slice = |last| -> Arc<[_]> {
        [1, 2, last]
            .map(|value| CountedValue {
                value,
                comparisons: &comparisons,
            })
            .into()
    };
    let left = make_slice(3);
    assert!(shared_slice_eq(&left, &left.clone()));
    assert_eq!(comparisons.load(Ordering::Relaxed), 0);

    let independent = make_slice(3);
    assert!(!Arc::ptr_eq(&left, &independent));
    assert!(shared_slice_eq(&left, &independent));
    assert_eq!(comparisons.swap(0, Ordering::Relaxed), 3);
    assert!(!shared_slice_eq(&left, &make_slice(4)));
    assert_eq!(comparisons.load(Ordering::Relaxed), 3);
}

fn two_owners() -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        let vector = vector_type();
        add_passthrough_owner(table, identity(1), &vector, span(0))?;
        add_passthrough_owner(table, identity(2), &vector, span(1))?;
        Ok(())
    })
    .unwrap()
}

fn copy_only<T: Copy>(value: T) -> T {
    value
}

#[test]
fn call_interfaces_borrow_the_checked_owner_storage() {
    let table = two_owners();
    let owner = &table.owners[1];
    let interface = copy_only(owner.interface());
    assert!(std::ptr::eq(
        interface.inputs.as_ptr(),
        owner.inputs.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.outputs.as_ptr(),
        owner.outputs.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.dependencies.as_ptr(),
        owner.dependencies.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.projections.unwrap(),
        owner.projections.as_deref().unwrap()
    ));
    assert!(std::ptr::eq(
        interface.affinity.unwrap(),
        owner.affinity.as_deref().unwrap()
    ));
    let owner = owner.directional.as_ref().unwrap();
    let interface = copy_only(owner.interface(SolvePureCallOwnerId::from_index(1)));
    assert!(std::ptr::eq(
        interface.inputs.as_ptr(),
        owner.inputs.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.outputs.as_ptr(),
        owner.outputs.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.dependencies.as_ptr(),
        owner.dependencies.as_ptr()
    ));
    assert!(std::ptr::eq(
        interface.projections.unwrap(),
        owner.projections.as_deref().unwrap()
    ));
    assert!(std::ptr::eq(
        interface.affinity.unwrap(),
        owner.affinity.as_deref().unwrap()
    ));
}

#[test]
fn shared_call_facts_retain_value_equality_after_wire_replay() {
    let table = two_owners();
    let _scope = table.enter_wire_scope();
    let owner = &table.owners[0];
    let site = owner.call_site();
    assert!(Arc::ptr_eq(&owner.inputs, &site.inputs));
    assert!(Arc::ptr_eq(&owner.outputs, &site.outputs));
    assert!(Arc::ptr_eq(&owner.dependencies, &site.dependencies));
    assert!(std::ptr::eq(
        owner.projections.as_deref().unwrap(),
        site.projections.as_deref().unwrap(),
    ));
    assert!(std::ptr::eq(
        owner.affinity.as_deref().unwrap(),
        site.affinity.as_deref().unwrap(),
    ));
    let encoded = serde_json::to_value(&site).unwrap();
    let replayed: SolvePureCallSite = serde_json::from_value(encoded.clone()).unwrap();
    assert!(std::ptr::eq(site.inputs(), replayed.inputs()));
    assert_eq!(site, replayed);
    assert_eq!(encoded, serde_json::to_value(&replayed).unwrap());
    assert!(table.matches_site(&replayed));
    assert!(table.matches_directional_site(replayed.directional().unwrap()));

    for field in ["inputs", "outputs", "dependencies"] {
        let mut claim = encoded.clone();
        claim[field] = serde_json::json!([]);
        assert!(serde_json::from_value::<SolvePureCallSite>(claim).is_err());
    }
    assert!(table.matches_site(&site));
}

#[test]
fn borrowed_prefix_rejects_an_owner_outside_the_issued_prefix() {
    let table = two_owners();
    let prefix = &table.owners[..1];
    for view in [
        SolvePureCallTableView::primal(prefix),
        SolvePureCallTableView::directional(prefix),
    ] {
        let view = copy_only(view);
        assert_eq!(view.get(0).unwrap().id, table.owners[0].id);
        assert!(view.get(1).is_none());
        let error = TypedProgram::construct_with_calls(profile(), view, |builder| {
            builder.call(table.owners[1].id, &[], span(3))?;
            Ok(())
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::UnknownCallOwner {
                provenance: span(3)
            }
        );
    }
}

#[test]
fn borrowed_view_cannot_rebind_a_suffix_owner_to_another_id() {
    let table = two_owners();
    let suffix = &table.owners[1..];
    assert!(SolvePureCallTableView::primal(suffix).get(0).is_none());
    assert!(SolvePureCallTableView::directional(suffix).get(0).is_none());
}

#[test]
fn shared_call_site_equality_preserves_owner_and_directional_interfaces() {
    let table = two_owners();
    let _scope = table.enter_wire_scope();
    let site = table.owners[0].call_site();
    let shared = site.clone();
    assert!(Arc::ptr_eq(&site.dependencies, &shared.dependencies));
    assert_eq!(site, shared);
    assert_ne!(site, table.owners[1].call_site());

    let mut missing_directional = site.clone();
    missing_directional.directional = None;
    assert_ne!(site, missing_directional);

    let directional = site.directional().unwrap();
    assert_eq!(directional, &directional.clone());
    let mut foreign = directional.clone();
    foreign.owner = table.owners[1].id;
    assert_ne!(directional, &foreign);
    let wire = serde_json::to_value(directional).unwrap();
    let replayed: SolvePureCallDirectionalSite = serde_json::from_value(wire).unwrap();
    assert!(Arc::ptr_eq(
        &directional.dependencies,
        &replayed.dependencies
    ));
    assert_eq!(directional, &replayed);
}
