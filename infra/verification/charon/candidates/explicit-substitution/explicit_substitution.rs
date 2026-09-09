use charon_lib::ast::*;
use charon_lib::ids::IndexVec;

#[test]
fn explicit_substitution_preserves_implicit_self() {
    assert_eq!(
        TraitRefKind::SelfId.substitute_explicits(&GenericArgs::empty()),
        TraitRefKind::SelfId,
    );
}

#[test]
#[should_panic(expected = "used `substitute` on an item coming from a trait")]
fn full_substitution_still_requires_self_witness() {
    let _ = TraitRefKind::SelfId.substitute(&GenericArgs::empty());
}

fn self_constraint(args: GenericArgs, ty: Ty) -> TraitTypeConstraint {
    let self_ref = TraitRef::new(
        TraitRefKind::SelfId,
        RegionBinder::empty(TraitDeclRef {
            id: TraitDeclId::ZERO,
            generics: Box::new(args.clone()),
        }),
    );
    TraitTypeConstraint {
        trait_ref: TraitRef::new(
            TraitRefKind::ParentClause(Box::new(self_ref), TraitClauseId::ZERO),
            RegionBinder::empty(TraitDeclRef {
                id: TraitDeclId::from_usize(1),
                generics: Box::new(args),
            }),
        ),
        type_id: AssocTypeId::ZERO,
        ty,
    }
}

#[test]
fn nested_associated_constraint_replaces_explicits_but_keeps_self_path() {
    let ty: Ty = TypeDbVar::new_at_zero(TypeVarId::ZERO).into();
    let mut variables = GenericArgs::empty();
    variables.types.push(ty.clone());
    variables
        .regions
        .push(RegionDbVar::new_at_zero(RegionId::ZERO).into());
    variables.const_generics.push(ConstantExpr::new(
        ConstGenericDbVar::new_at_zero(ConstGenericVarId::ZERO).into(),
        Ty::mk_usize(),
    ));
    let mut values = GenericArgs::empty();
    values.types.push(Ty::mk_bool());
    values.regions.push(Region::Static);
    values.const_generics.push(ConstantExpr::mk_usize(7));
    let input = RegionBinder::empty(RegionBinder::empty(self_constraint(variables, ty)));
    let expected = RegionBinder::empty(RegionBinder::empty(self_constraint(
        values.clone(),
        Ty::mk_bool(),
    )));
    assert_eq!(input.substitute_explicits(&values), expected);
}

#[test]
fn explicit_substitution_respects_a_genuine_inner_binder() {
    let mut regions = IndexVec::new();
    regions.push(RegionParam::new(
        RegionId::ZERO,
        Some("local".into()),
        Variance::Unknown,
    ));
    let local_region = RegionDbVar::new_at_zero(RegionId::ZERO).into();
    let input_ty = TyKind::Ref(
        local_region,
        TypeDbVar::bound(DeBruijnId::one(), TypeVarId::ZERO).into(),
        RefKind::Shared,
    )
    .into();
    let input = RegionBinder {
        regions: regions.clone(),
        skip_binder: self_constraint(GenericArgs::empty(), input_ty),
    };
    let expected = RegionBinder {
        regions,
        skip_binder: self_constraint(
            GenericArgs::empty(),
            TyKind::Ref(local_region, Ty::mk_bool(), RefKind::Shared).into(),
        ),
    };
    let mut args = GenericArgs::empty();
    args.types.push(Ty::mk_bool());
    args.regions.push(Region::Static);
    assert_eq!(input.substitute_explicits(&args), expected);
}

#[test]
fn explicit_substitution_does_not_consume_supplied_clause_witnesses() {
    let mut args = GenericArgs::empty();
    args.trait_refs.push(TraitRef::new(
        TraitRefKind::Dyn,
        RegionBinder::empty(TraitDeclRef {
            id: TraitDeclId::ZERO,
            generics: Box::new(GenericArgs::empty()),
        }),
    ));
    for clause in [
        ClauseDbVar::new_at_zero(TraitClauseId::ZERO),
        ClauseDbVar::bound(DeBruijnId::one(), TraitClauseId::ZERO),
        ClauseDbVar::free(TraitClauseId::ZERO),
    ] {
        let witness = TraitRefKind::Clause(clause);
        assert_eq!(witness.clone().substitute_explicits(&args), witness);
    }
}

#[test]
fn full_substitution_shifts_supplied_self_under_inner_binders() {
    let witness = TraitRefKind::Clause(ClauseDbVar::new_at_zero(TraitClauseId::ZERO));
    assert_eq!(
        RegionBinder::empty(TraitRefKind::SelfId)
            .substitute_with_self(&GenericArgs::empty(), &witness),
        RegionBinder::empty(witness),
    );
}

#[test]
#[should_panic(expected = "GenericsMismatch")]
fn explicit_substitution_still_rejects_missing_explicit_arguments() {
    let ty: Ty = TypeDbVar::new_at_zero(TypeVarId::ZERO).into();
    let _ = ty.substitute_explicits(&GenericArgs::empty());
}
