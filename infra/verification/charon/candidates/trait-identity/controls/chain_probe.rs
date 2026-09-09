use charon_lib::ast::*;
use charon_lib::export::CrateData;
use charon_lib::options::SerializationFormat;
use charon_lib::pretty::FmtWithCtx;
use charon_lib::formatter::IntoFormatter;

#[test]
fn probe() {
    let path = std::env::var("CHAIN_PROBE_LLBC").unwrap();
    let krate: TranslatedCrate =
        CrateData::deserialize_from_file(std::path::Path::new(&path), SerializationFormat::Json)
            .unwrap()
            .translated;
    let fmt = &(&krate).into_fmt();
    let mut total = 0;
    for item in krate.all_items() {
        let name = &item.item_meta().name;
        let records: Vec<bool> = name
            .name
            .iter()
            .rev()
            .take_while(|e| matches!(e, PathElem::Instantiated(_)))
            .map(|e| match e {
                PathElem::Instantiated(b) => Name::binder_is_generics_extension(b),
                _ => unreachable!(),
            })
            .collect();
        if !records.is_empty() {
            total += 1;
            if records.len() > 1 || matches!(item.id(), ItemId::TraitImpl(_) | ItemId::TraitDecl(_)) {
                for e in name.name.iter().rev().take_while(|e| matches!(e, PathElem::Instantiated(_))) {
                    if let PathElem::Instantiated(b) = e {
                        println!("PROBE   {:?} record params(r,t,c,tc)=({},{},{},{}) args(r,t,c,tr)=({},{},{},{}) args={}", item.id(), b.params.regions.len(), b.params.types.len(), b.params.const_generics.len(), b.params.trait_clauses.len(), b.skip_binder.regions.len(), b.skip_binder.types.len(), b.skip_binder.const_generics.len(), b.skip_binder.trait_refs.len(), b.skip_binder.with_ctx(fmt));
                    }
                }
                if let ItemRef::TraitDecl(td) = item { println!("PROBE   {:?} decl generics={}", item.id(), format!("(r,t,c,tc)=({},{},{},{})", td.generics.regions.len(), td.generics.types.len(), td.generics.const_generics.len(), td.generics.trait_clauses.len())); }
                if let ItemRef::TraitImpl(ti) = item { println!("PROBE   {:?} impl_trait={}", item.id(), ti.impl_trait.with_ctx(fmt)); }
            }
            let kinds: Vec<&str> = records.iter().rev().map(|ext| if *ext { "ext" } else { "copy" }).collect();
            let base = Name { name: name.as_slice_uninstantiated().to_vec() };
            println!("PROBE {:?} records={} base={}", item.id(), kinds.join(","), base.with_ctx(fmt));
        }
    }
    for item in krate.all_items() {
        if let ItemRef::TraitImpl(ti) = item {
            let td = ti.impl_trait.id;
            let chained = krate.trait_decls.get(td).map(|d| d.item_meta.name.name.iter().rev().take_while(|e| matches!(e, PathElem::Instantiated(_))).count() > 1).unwrap_or(false);
            if chained { println!("PROBE   impl {:?} of {:?}: impl_trait={} impl generics(r,t,c,tc)=({},{},{},{})", item.id(), td, ti.impl_trait.with_ctx(fmt), ti.generics.regions.len(), ti.generics.types.len(), ti.generics.const_generics.len(), ti.generics.trait_clauses.len()); }
        }
    }
    println!("PROBE items with records: {total}");
}
