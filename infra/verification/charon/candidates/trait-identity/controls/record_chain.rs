//! Laws of instantiation records on item names, checked through the actual matcher.
//!
//! An `Extended` record is provenance: the associated-type lifting extended the item's generics in
//! place and the item keeps its identity. An `Instantiated` record is a copy: a distinct item made
//! from its parent with the given arguments. Consumers fold every trailing record, last to first.

mod util;

use charon_lib::ast::*;
use charon_lib::formatter::IntoFormatter;
use charon_lib::name_matcher::Pattern;
use charon_lib::pretty::FmtWithCtx;

const CODE: &str = r#"
    pub trait Tr {
        type Out;
        fn m(&self) -> Self::Out;
    }
    impl<A: Copy> Tr for Option<A> {
        type Out = A;
        fn m(&self) -> A {
            self.unwrap()
        }
    }
    impl Tr for u32 {
        type Out = u64;
        fn m(&self) -> u64 {
            *self as u64
        }
    }
    pub fn call<T: Tr>(x: T) -> T::Out {
        x.m()
    }
    pub fn use_it(a: Option<u8>, b: Option<u16>, c: u32) -> (u8, u16, u64) {
        (call(a), call(b), call(c))
    }
"#;

/// The roles of the trailing records of a name, first to last.
fn records(name: &Name) -> Vec<&'static str> {
    let mut roles: Vec<_> = name
        .name
        .iter()
        .rev()
        .take_while(|e| matches!(e, PathElem::Instantiated(_) | PathElem::Extended(_)))
        .map(|e| match e {
            PathElem::Extended(_) => "ext",
            _ => "copy",
        })
        .collect();
    roles.reverse();
    roles
}

fn last_ident_is(name: &Name, ident: &str) -> bool {
    matches!(
        name.as_slice_uninstantiated().last(),
        Some(PathElem::Ident(s, _)) if s == ident
    )
}

fn find_trait<'a>(krate: &'a TranslatedCrate, ident: &str) -> &'a TraitDecl {
    krate
        .trait_decls
        .iter()
        .find(|d| last_ident_is(&d.item_meta.name, ident))
        .unwrap_or_else(|| panic!("no trait `{ident}`"))
}

fn find_fun<'a>(krate: &'a TranslatedCrate, ident: &str) -> &'a FunDecl {
    krate
        .fun_decls
        .iter()
        .find(|d| last_ident_is(&d.item_meta.name, ident) && records(&d.item_meta.name).is_empty())
        .unwrap_or_else(|| panic!("no function `{ident}`"))
}

fn pat(s: &str) -> Pattern {
    Pattern::parse(s).unwrap_or_else(|e| panic!("pattern `{s}`: {e}"))
}

/// Lifting appends an extension record; the trait keeps its identity for a bare pattern and for
/// a pattern at the declared arity; the printer shows nothing of the record.
#[test]
fn extension_keeps_original_identity() -> anyhow::Result<()> {
    let krate = util::translate_rust_text(CODE, &["--remove-associated-types=*"])?;
    let fmt = &krate.into_fmt();
    let tr = find_trait(&krate, "Tr");
    assert_eq!(records(&tr.item_meta.name), ["ext"]);
    assert_eq!(tr.generics.types.len(), 2, "Self and the lifted Out");
    let item = ItemRef::TraitDecl(tr);
    assert!(pat("test_crate::Tr").matches_item(&krate, item));
    assert!(pat("test_crate::Tr<_>").matches_item(&krate, item));
    assert!(
        !pat("test_crate::Tr<_, _>").matches_item(&krate, item),
        "the extended arity is not the declared identity"
    );
    let printed = tr.item_meta.name.with_ctx(fmt).to_string();
    assert_eq!(printed, "test_crate::Tr", "extension records print nothing");
    assert_eq!(
        tr.item_meta.name.as_slice_uninstantiated().len(),
        tr.item_meta.name.name.len() - 1
    );
    Ok(())
}

/// A copy record whose arguments are the identity of its parameters is still a copy: it is
/// stripped by `as_slice_uninstantiated`, prints its arguments, and matches through the fold.
#[test]
fn identity_copy_is_a_copy() -> anyhow::Result<()> {
    let krate = util::translate_rust_text(CODE, &["--remove-associated-types=*"])?;
    let fmt = &krate.into_fmt();
    let call = find_fun(&krate, "call");
    let params = call.generics.clone();
    let identity = params.identity_args();
    let name = call
        .item_meta
        .name
        .clone()
        .instantiate(Binder::new(BinderKind::Other, params.clone(), identity.clone()));
    assert_eq!(records(&name), ["copy"]);
    assert_eq!(
        name.as_slice_uninstantiated(),
        call.item_meta.name.as_slice_uninstantiated()
    );
    let original = call.item_meta.name.with_ctx(fmt).to_string();
    let copied = name.with_ctx(fmt).to_string();
    assert_ne!(copied, original, "a copy names distinctly");
    assert!(copied.starts_with(&original));
    // Lifting extended `call` to `<T, T_Out>`; the copy matches at that arity through the fold.
    let original = ItemRef::Fun(call);
    assert!(pat("test_crate::call<_, _>").matches_item(&krate, original));
    assert!(!pat("test_crate::call<_>").matches_item(&krate, original));
    assert!(pat("test_crate::call<_, _>").matches_with_generics(&krate, &name, Some(&identity)));
    assert!(!pat("test_crate::call<_>").matches_with_generics(&krate, &name, Some(&identity)));
    assert!(pat("test_crate::call").matches_with_generics(&krate, &name, Some(&identity)));
    Ok(())
}

/// A copy record whose arguments differ from the identity only in trait evidence is a copy: the
/// role is carried by the record, not read off the shape of its arguments.
#[test]
fn trait_evidence_only_copy_is_a_copy() -> anyhow::Result<()> {
    let krate = util::translate_rust_text(CODE, &["--remove-associated-types=*"])?;
    let fmt = &krate.into_fmt();
    let call = find_fun(&krate, "call");
    let tr = find_trait(&krate, "Tr");
    let u32_impl = krate
        .trait_impls
        .iter()
        .find(|i| {
            i.impl_trait.id == tr.def_id
                && i.impl_trait.generics.types.first().is_some_and(|t| {
                    matches!(t.kind(), TyKind::Literal(LiteralTy::UInt(UIntTy::U32)))
                })
        })
        .expect("impl Tr for u32");
    let params = call.generics.clone();
    let mut args = params.identity_args();
    assert!(!args.trait_refs.is_empty(), "call has the clause T: Tr");
    let evidence = TraitRef::new(
        TraitRefKind::TraitImpl(TraitImplRef {
            id: u32_impl.def_id,
            generics: Box::new(GenericArgs::empty()),
        }),
        RegionBinder::empty(u32_impl.impl_trait.clone()),
    );
    args.trait_refs[TraitClauseId::from_raw(0)] = evidence;
    let identity = params.identity_args();
    assert_eq!(args.types, identity.types);
    assert_eq!(args.regions, identity.regions);
    assert_eq!(args.const_generics, identity.const_generics);
    assert_ne!(args.trait_refs, identity.trait_refs);
    let name = call
        .item_meta
        .name
        .clone()
        .instantiate(Binder::new(BinderKind::Other, params, args));
    assert_eq!(records(&name), ["copy"]);
    assert_eq!(
        name.as_slice_uninstantiated(),
        call.item_meta.name.as_slice_uninstantiated()
    );
    assert_ne!(
        name.with_ctx(fmt).to_string(),
        call.item_meta.name.with_ctx(fmt).to_string()
    );
    assert!(pat("test_crate::call<_, _>").matches_with_generics(&krate, &name, Some(&identity)));
    Ok(())
}

/// A copy of an extended item keeps both records; a copy of that copy composes into the copy
/// record and leaves the extension record in place; matching folds both back to the declared
/// identity with the composed arguments.
#[test]
fn repeated_composition_folds_to_the_declared_identity() -> anyhow::Result<()> {
    let krate = util::translate_rust_text(CODE, &["--remove-associated-types=*"])?;
    let tr = find_trait(&krate, "Tr");
    let option = krate
        .type_decls
        .iter()
        .find(|d| last_ident_is(&d.item_meta.name, "Option"))
        .expect("Option");
    let u8_ty = TyKind::Literal(LiteralTy::UInt(UIntTy::U8)).into_ty();
    let option_of = |t: Ty| {
        TyKind::Adt(TypeDeclRef {
            id: option.def_id,
            builtin: None,
            generics: Box::new(GenericArgs {
                regions: IndexVec::new(),
                types: [t].into_iter().collect(),
                const_generics: IndexVec::new(),
                trait_refs: IndexVec::new(),
            }),
        })
        .into_ty()
    };
    // First copy: `Tr<Option<X>, X>` over one parameter `X`.
    let mut params1 = GenericParams::empty();
    params1.types.push(TypeParam {
        index: TypeVarId::from_raw(0),
        name: "X".to_string(),
        variance: Variance::Unknown,
    });
    let x = TyKind::TypeVar(DeBruijnVar::bound(DeBruijnId::zero(), TypeVarId::from_raw(0))).into_ty();
    let args1 = GenericArgs {
        regions: IndexVec::new(),
        types: [option_of(x.clone()), x].into_iter().collect(),
        const_generics: IndexVec::new(),
        trait_refs: IndexVec::new(),
    };
    let once = tr
        .item_meta
        .name
        .clone()
        .instantiate(Binder::new(BinderKind::Other, params1, args1));
    assert_eq!(records(&once), ["ext", "copy"]);
    // Second copy: `X := u8`, no parameters left.
    let args2 = GenericArgs {
        regions: IndexVec::new(),
        types: [u8_ty].into_iter().collect(),
        const_generics: IndexVec::new(),
        trait_refs: IndexVec::new(),
    };
    let twice = once
        .clone()
        .instantiate(Binder::new(BinderKind::Other, GenericParams::empty(), args2));
    assert_eq!(records(&twice), ["ext", "copy"], "composition keeps one copy record");
    let empty = GenericArgs::empty();
    assert!(pat("test_crate::Tr<core::option::Option<u8>>").matches_with_generics(
        &krate,
        &twice,
        Some(&empty)
    ));
    assert!(!pat("test_crate::Tr<u8>").matches_with_generics(&krate, &twice, Some(&empty)));
    assert!(pat("test_crate::Tr").matches_with_generics(&krate, &twice, Some(&empty)));
    Ok(())
}

/// The producer itself emits chains: under partial monomorphization the blanket `FnOnce`
/// implementation for `&mut F` specializes the lifted `FnOnce` declaration at `Self := &mut F`,
/// so the copied declaration carries the extension record followed by the copy record and still
/// matches at the declared arity.
#[test]
fn producer_emits_chains_under_partial_monomorphization() -> anyhow::Result<()> {
    let code = r#"
        pub fn once<G: FnOnce(u32) -> u32>(g: G) -> u32 {
            g(1)
        }
        pub fn wrap<F: FnMut(u32) -> u32>(f: &mut F) -> u32 {
            once(f)
        }
        pub fn use_it(k: u32) -> u32 {
            let mut acc = 0;
            wrap(&mut |x| {
                acc += x;
                acc + k
            })
        }
    "#;
    let krate = util::translate_rust_text(
        code,
        &[
            "--preset=aeneas",
            "--remove-associated-types=*",
            "--monomorphize-mut",
            "--include=core::ops::function::impls::*::call_once",
        ],
    )?;
    let fmt = &krate.into_fmt();
    let chained: Vec<_> = krate
        .trait_decls
        .iter()
        .filter(|d| {
            last_ident_is(&d.item_meta.name, "FnOnce")
                && records(&d.item_meta.name) == ["ext", "copy"]
        })
        .collect();
    assert_eq!(chained.len(), 1, "one copied lifted FnOnce");
    let copy = chained[0];
    let item = ItemRef::TraitDecl(copy);
    assert!(pat("core::ops::function::FnOnce").matches_item(&krate, item));
    assert!(pat("core::ops::function::FnOnce<&mut _, _>").matches_item(&krate, item));
    assert!(
        !pat("core::ops::function::FnOnce<_, _, _>").matches_item(&krate, item),
        "the lifted arity is not the declared identity"
    );
    let printed = copy.item_meta.name.with_ctx(fmt).to_string();
    assert!(printed.starts_with("core::ops::function::FnOnce::<"), "{printed}");
    let original = find_trait(&krate, "FnOnce");
    assert_eq!(records(&original.item_meta.name), ["ext"]);
    assert_eq!(
        original.item_meta.name.with_ctx(fmt).to_string(),
        "core::ops::function::FnOnce"
    );
    Ok(())
}
