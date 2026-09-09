mod util;

use charon_lib::ast::*;
use charon_lib::errors::ErrorCtx;
use charon_lib::options::{MonomorphizeMut, TranslateOptions};
use charon_lib::transform::TransformCtx;
use charon_lib::transform::ctx::TransformPass;
use charon_lib::transform::normalize::partial_monomorphization;
use charon_lib::transform::typecheck_and_unify::Check;

#[test]
fn specialization_preserves_typechecked_parent_trait_contracts() -> anyhow::Result<()> {
    assert_parent_contracts(include_str!("fixtures/specialized_traits.rs"))
}

#[test]
fn specialization_preserves_closed_cyclic_parent_contracts() -> anyhow::Result<()> {
    assert_parent_contracts(include_str!("fixtures/cyclic-assoc-source.rs"))
}

#[test]
fn specialization_preserves_noninverse_cyclic_parent_contracts() -> anyhow::Result<()> {
    assert_parent_contracts(include_str!("fixtures/cyclic-noninverse-source.rs"))
}

#[test]
fn cyclic_lifting_preserves_distinct_associated_types() -> anyhow::Result<()> {
    util::translate_rust_text(
        include_str!("fixtures/cyclic-distinct-associated-source.rs"),
        &[
            "--error-on-warnings",
            "--lift-associated-types=*",
            "--hide-marker-traits",
        ],
    )?;
    Ok(())
}

fn assert_parent_contracts(source: &str) -> anyhow::Result<()> {
    let translated = util::translate_rust_text(
        source,
        &[
            "--error-on-warnings",
            "--include=core::option::*::branch",
            "--include=core::option::*::from_residual",
            "--lift-associated-types=*",
            "--hide-marker-traits",
            "--hide-allocator",
            "--remove-unused-self-clauses",
            "--remove-adt-clauses",
            "--ops-to-function-calls",
            "--index-to-function-calls",
            "--treat-box-as-builtin",
            "--no-gen-tuple-structs",
            "--reconstruct-fallible-operations",
            "--reconstruct-asserts",
            "--reconstruct-matches",
            "--deallocate-all-locals",
        ],
    )?;
    // Match the input to partial monomorphization: item variables remain bound.
    assert!(!translated.options.unbind_item_vars);
    let mut errors = ErrorCtx::new();
    let options = TranslateOptions::new(&mut errors, &translated.options);
    let mut ctx = TransformCtx {
        options,
        translated,
        errors: errors.into(),
    };
    Check::PostTransformation.transform_ctx(&mut ctx);
    assert_eq!(
        ctx.errors.borrow().error_count,
        0,
        "source-valid predecessor"
    );

    let original_trait_count = ctx.translated.trait_decls.elem_count();
    ctx.options.monomorphize_mut = Some(MonomorphizeMut::All);
    partial_monomorphization::Transform.transform_ctx(&mut ctx);
    assert!(ctx.translated.trait_decls.elem_count() > original_trait_count);
    let mut identity_mismatches = Vec::new();
    for implementation in &ctx.translated.trait_impls {
        let declaration = &ctx.translated.trait_decls[implementation.impl_trait.id];
        let self_witness = TraitRefKind::TraitImpl(TraitImplRef {
            id: implementation.def_id,
            generics: Box::new(implementation.generics.identity_args()),
        });
        for (clause, actual) in declaration
            .implied_clauses
            .iter()
            .zip(&implementation.implied_trait_refs)
        {
            let expected = clause
                .clone()
                .substitute_with_self(&implementation.impl_trait.generics, &self_witness);
            if expected.trait_.skip_binder.id != actual.trait_id() {
                identity_mismatches.push((
                    implementation.def_id,
                    expected.trait_.skip_binder.id,
                    actual.trait_id(),
                ));
            }
        }
    }
    Check::PostTransformation.transform_ctx(&mut ctx);
    assert_eq!(
        ctx.errors.borrow().error_count,
        0,
        "specialization must preserve type validity; (implementation, expected parent, actual parent): {identity_mismatches:?}",
    );
    Ok(())
}
