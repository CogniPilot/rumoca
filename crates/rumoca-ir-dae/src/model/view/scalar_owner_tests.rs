use super::*;

thread_local! {
    pub(super) static OWNER_VIEWS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[test]
fn scalar_owner_lookup_does_not_materialize_preceding_owners() {
    let mut source = rumoca_core::SourceMap::new();
    let file = source.add("scalar_owners.mo", "equation 0 = 0;");
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(file, 9, 14)).unwrap();
    const COUNT: usize = 256;
    let dae = Dae::construct(source, |dae| {
        let zero = dae
            .expressions(|expressions| expressions.at(at).literal(crate::DaeLiteral::Real(0.0)))?;
        dae.continuous(|equations| {
            for _ in 0..COUNT {
                equations.value_equation(at, zero)?;
            }
            Ok(())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        OWNER_VIEWS.set(0);
        for row in (0..COUNT).rev() {
            let Some(ContinuousOwnerView::Residual { id, .. }) =
                view.continuous_owner_for_scalar_row(row)
            else {
                panic!("each scalar row has its checked residual owner");
            };
            assert_eq!(id.index() as usize, row);
        }
        let materialized = OWNER_VIEWS.get();
        assert!(
            materialized <= COUNT,
            "{COUNT} random-access queries materialized {materialized} owners"
        );
    });
}

fn repeat_family_body<'dae>(
    family: &mut crate::StructuredResiduals<'_, 'dae>,
    residual: ExprId<'dae>,
    count: usize,
) -> Result<(), DaeConstructionError> {
    for _ in 0..count {
        family.body(residual)?;
    }
    Ok(())
}

fn mixed_owners(owners: &[Option<(u32, usize)>]) -> Dae {
    use rumoca_core::{ComprehensionScalarView, StructuredIndexBinder, StructuredIndexDomain};

    let mut source = rumoca_core::SourceMap::new();
    let file = source.add("structured_owners.mo", "equation 0 = 0;");
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(file, 9, 14)).unwrap();
    Dae::construct(source, |dae| {
        let zero = dae
            .expressions(|expressions| expressions.at(at).literal(crate::DaeLiteral::Real(0.0)))?;
        for owner in owners {
            let Some((extent, body_count)) = owner else {
                dae.continuous(|equations| equations.value_equation(at, zero))?;
                continue;
            };
            let domain = dae.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_owned(),
                            lower: 1,
                            upper: i64::from(*extent),
                            step: 1,
                        }],
                    },
                    at,
                )
            })?;
            dae.continuous(|equations| {
                equations.structured_family(
                    at,
                    domain,
                    ComprehensionScalarView::BinderSubstitution,
                    |family| repeat_family_body(family, zero, *body_count),
                )?;
                Ok(())
            })?;
        }
        Ok(())
    })
    .unwrap()
}

fn assert_mixed_row_owners(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 6);
        for row in [7, 1, 6, 0, 4, 2, 3, 5] {
            let owner = view.continuous_owner_for_scalar_row(row).unwrap();
            match (row, owner) {
                (0 | 7, ContinuousOwnerView::Residual { id, .. }) => {
                    assert_eq!(id.index(), u32::from(row == 7));
                }
                (1..=6, ContinuousOwnerView::Structured { id, family }) => {
                    assert_eq!(id.index(), 1);
                    assert_eq!(family.scalar_rows(), 6);
                    assert_eq!(family.bodies().len(), 2);
                }
                _ => panic!("wrong owner for scalar row {row}"),
            }
        }
        assert!(view.continuous_owner_for_scalar_row(8).is_none());
        assert!(view.continuous_owner_for_scalar_row(usize::MAX).is_none());
    });
}

#[test]
fn scalar_owner_lookup_preserves_mixed_family_boundaries_and_wire_replay() {
    let dae = mixed_owners(&[
        Some((0, 1)),
        None,
        Some((3, 2)),
        Some((0, 1)),
        None,
        Some((0, 1)),
    ]);
    assert_mixed_row_owners(&dae);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(!json.contains("scalar_row_end"));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_mixed_row_owners(&decoded);
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_mixed_row_owners(&decoded);
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn scalar_owner_lookup_rejects_rows_in_empty_systems_and_empty_families() {
    for owners in [&[][..], &[Some((0, 1)), Some((0, 2))]] {
        mixed_owners(owners).inspect(|view| {
            assert!(view.continuous_owner_for_scalar_row(0).is_none());
            assert!(view.continuous_owner_for_scalar_row(usize::MAX).is_none());
        });
    }
}

#[test]
#[cfg(target_pointer_width = "64")]
fn scalar_owner_lookup_keeps_large_domains_compact_and_uses_full_row_width() {
    let dae = mixed_owners(&[Some((u32::MAX, 1)), Some((u32::MAX, 1)), None]);
    let family_rows = u32::MAX as usize;
    dae.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 3);
        assert_eq!(view.expression_count(), 1);
        for (row, expected) in [
            (0, 0),
            (family_rows - 1, 0),
            (family_rows, 1),
            (2 * family_rows - 1, 1),
        ] {
            let Some(ContinuousOwnerView::Structured { id, .. }) =
                view.continuous_owner_for_scalar_row(row)
            else {
                panic!("large domain row belongs to its compact family");
            };
            assert_eq!(id.index(), expected);
        }
        assert!(matches!(
            view.continuous_owner_for_scalar_row(2 * family_rows),
            Some(ContinuousOwnerView::Residual { .. })
        ));
        assert!(
            view.continuous_owner_for_scalar_row(2 * family_rows + 1)
                .is_none()
        );
    });
    assert!(serde_json::to_vec(&dae).unwrap().len() < 4096);
}
