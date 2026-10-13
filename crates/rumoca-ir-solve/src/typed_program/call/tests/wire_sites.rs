use super::*;

fn two_owner_table() -> SolvePureCallTable {
    let vector = vector_type();
    SolvePureCallTable::construct(profile(), |table| {
        add_passthrough_owner(table, identity(1), &vector, span(0))?;
        add_passthrough_owner(table, identity(2), &vector, span(1))?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_site_serializes_only_its_owner_id() {
    let table = two_owner_table();
    let wire = serde_json::to_value(table.owners()[1].call_site()).unwrap();
    assert_eq!(wire, serde_json::json!({ "owner": 1 }));
    let directional = table.owners()[1]
        .call_site()
        .directional()
        .cloned()
        .unwrap();
    assert_eq!(
        serde_json::to_value(directional).unwrap(),
        serde_json::json!({ "owner": 1 })
    );
}

#[test]
fn sites_resolve_to_the_shared_owner_interface_in_scope() {
    let table = two_owner_table();
    let _scope = table.enter_wire_scope();
    for owner in table.owners() {
        let site = owner.call_site();
        let replayed: SolvePureCallSite =
            serde_json::from_value(serde_json::to_value(&site).unwrap()).unwrap();
        assert_eq!(replayed, site);
        assert!(table.matches_site(&replayed));
        assert!(Arc::ptr_eq(&replayed.inputs, &owner.inputs));
        assert!(Arc::ptr_eq(&replayed.dependencies, &owner.dependencies));
        let directional: SolvePureCallDirectionalSite =
            serde_json::from_value(serde_json::to_value(site.directional().unwrap()).unwrap())
                .unwrap();
        assert!(table.matches_directional_site(&directional));
    }
}

#[test]
fn a_forged_owner_id_resolves_to_exactly_that_owner() {
    let table = two_owner_table();
    let _scope = table.enter_wire_scope();
    let forged: SolvePureCallSite =
        serde_json::from_value(serde_json::json!({ "owner": 1 })).unwrap();
    assert_eq!(forged, table.owners()[1].call_site());
    assert_ne!(forged, table.owners()[0].call_site());
}

#[test]
fn a_dangling_owner_id_is_a_typed_error() {
    let table = two_owner_table();
    let _scope = table.enter_wire_scope();
    let error =
        serde_json::from_value::<SolvePureCallSite>(serde_json::json!({ "owner": 9 })).unwrap_err();
    assert!(error.to_string().contains("unknown owner 9"), "{error}");
}

#[test]
fn reading_a_site_without_an_owner_table_is_refused() {
    let error =
        serde_json::from_value::<SolvePureCallSite>(serde_json::json!({ "owner": 0 })).unwrap_err();
    assert!(error.to_string().contains("owner table"), "{error}");
}

#[test]
fn the_scope_ends_with_its_guard_and_restores_the_enclosing_table() {
    let outer = two_owner_table();
    let _outer_scope = outer.enter_wire_scope();
    {
        let inner = SolvePureCallTable::default();
        let _inner_scope = inner.enter_wire_scope();
        assert!(
            serde_json::from_value::<SolvePureCallSite>(serde_json::json!({ "owner": 0 })).is_err()
        );
    }
    assert!(serde_json::from_value::<SolvePureCallSite>(serde_json::json!({ "owner": 0 })).is_ok());
}

#[test]
fn a_scoped_table_stays_current_for_later_fields_of_one_document() {
    #[derive(Deserialize)]
    struct Document {
        pure_calls: ScopedSolvePureCallTable,
        site: SolvePureCallSite,
    }
    let table = two_owner_table();
    let wire = serde_json::json!({
        "pure_calls": serde_json::to_value(&table).unwrap(),
        "site": serde_json::to_value(table.owners()[1].call_site()).unwrap(),
    });
    let document: Document = serde_json::from_value(wire.clone()).unwrap();
    let replayed = document.pure_calls.into_table();
    assert!(replayed.matches_site(&document.site));
    let reversed = serde_json::json!({ "site": wire["site"], "pure_calls": wire["pure_calls"] });
    assert!(serde_json::from_value::<Document>(reversed).is_err());
}
