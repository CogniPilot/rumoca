//! Canonical exact identity relation for resolved Flat component references.

use rumoca_core::{ComponentReference, Reference};

pub(super) fn same_exact_reference(left: &ComponentReference, right: &ComponentReference) -> bool {
    left.local() == right.local()
        && left.parts().len() == right.parts().len()
        && left.parts().iter().zip(right.parts()).all(|(left, right)| {
            left.ident == right.ident
                && left.def_id == right.def_id
                && rumoca_core::subscripts_semantically_equal(&left.subs, &right.subs)
        })
}

pub(super) fn reference_has_exact_identity(
    reference: &Reference,
    expected: &ComponentReference,
) -> bool {
    reference.var_name() == &expected.to_var_name()
        && reference
            .component_ref()
            .is_some_and(|actual| same_exact_reference(actual, expected))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ComponentRefPart, DefId, Span, Subscript, VarName};

    const ROOT_DEF: u32 = 41;
    const LEAF_DEF: u32 = 42;

    fn exact_component() -> ComponentReference {
        ComponentReference::construct(
            false,
            Span::DUMMY,
            vec![
                part("plant", DefId::new(ROOT_DEF), 1),
                part("signal", DefId::new(LEAF_DEF), 2),
            ],
        )
        .expect("fixture component reference is resolved")
    }

    fn part(ident: &str, def_id: DefId, index: i64) -> ComponentRefPart {
        ComponentRefPart {
            ident: ident.to_owned(),
            span: Span::DUMMY,
            subs: vec![Subscript::index(index, Span::DUMMY)],
            def_id,
        }
    }

    fn replace_part(
        reference: &ComponentReference,
        ordinal: usize,
        mutate: impl FnOnce(&mut ComponentRefPart),
    ) -> ComponentReference {
        let mut parts = reference.parts().to_vec();
        mutate(&mut parts[ordinal]);
        reference
            .with_replaced_parts(parts)
            .expect("mutation preserves resolved component-reference shape")
    }

    fn assert_component_mutation_rejected(
        actual: ComponentReference,
        expected: &ComponentReference,
    ) {
        let cached_name = expected.to_var_name();
        let reference = Reference::with_component_reference(cached_name.as_str(), actual);

        assert!(!reference_has_exact_identity(&reference, expected));
    }

    #[test]
    fn exact_reference_accepts_subscript_span_aliases() {
        let expected = exact_component();
        let mut parts = expected.parts().to_vec();
        parts[0].subs = vec![Subscript::index(
            1,
            Span::from_offsets(rumoca_core::SourceId::from_source_name("alias.mo"), 3, 4),
        )];
        let alias = expected
            .with_replaced_parts(parts)
            .expect("span alias retains resolved component identity");
        let reference = Reference::from_component_reference(alias.clone());

        assert!(same_exact_reference(&alias, &expected));
        assert!(reference_has_exact_identity(&reference, &expected));
    }

    #[test]
    fn cached_var_name_mutation_is_rejected() {
        let expected = exact_component();
        let reference = Reference::from_component_reference(expected.clone())
            .with_var_name(VarName::new("plant[1].other[2]"));

        assert!(!reference_has_exact_identity(&reference, &expected));
    }

    #[test]
    fn locality_mutation_is_rejected() {
        let expected = exact_component();
        let mutated =
            ComponentReference::construct(true, expected.span(), expected.parts().to_vec())
                .expect("locality mutation retains resolved parts");

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn part_count_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = expected
            .with_replaced_parts(expected.parts()[..1].to_vec())
            .expect("shortened reference remains nonempty and resolved");

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn root_identifier_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 0, |part| part.ident = "other".to_owned());

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn leaf_identifier_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 1, |part| part.ident = "other".to_owned());

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn root_declaration_identity_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 0, |part| part.def_id = DefId::new(43));

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn leaf_declaration_identity_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 1, |part| part.def_id = DefId::new(43));

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn root_subscript_alias_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 0, |part| {
            part.subs = vec![Subscript::index(3, Span::DUMMY)];
        });

        assert_component_mutation_rejected(mutated, &expected);
    }

    #[test]
    fn leaf_subscript_alias_mutation_is_rejected() {
        let expected = exact_component();
        let mutated = replace_part(&expected, 1, |part| {
            part.subs = vec![Subscript::index(3, Span::DUMMY)];
        });

        assert_component_mutation_rejected(mutated, &expected);
    }
}
