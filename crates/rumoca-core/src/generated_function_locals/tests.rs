use super::*;

fn source() -> (SourceMap, Span) {
    let mut sources = SourceMap::new();
    let id = sources.add("generated-locals.mo", "function f\nalgorithm\nend f;");
    (sources, Span::from_offsets(id, 11, 20))
}

fn function(declaration: u32, instance: u32, span: Span) -> Function {
    let mut function = Function::new("f", span);
    function.def_id = Some(DefId::new(declaration));
    function.instance_id = Some(FunctionInstanceId::new(instance));
    function
}

#[test]
fn generated_local_reservations_are_dense_and_do_not_alias_occurrences() {
    let (sources, span) = source();
    let original = function(10, 0, span);
    let sibling = function(10, 1, span);
    let other_declaration = function(11, 0, span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let first = catalog
            .function(&original)
            .unwrap()
            .reserve_boolean(span)
            .unwrap();
        let second = catalog
            .function(&original)
            .unwrap()
            .reserve_boolean(span)
            .unwrap();
        assert_eq!(first.key.ordinal, 0);
        assert_eq!(second.key.ordinal, 1);
        assert_ne!(first.key(), second.key());
        assert_eq!(first.provenance(), span);
        assert_eq!(
            catalog.function(&original).unwrap().declarations(),
            &[first, second]
        );
        for owner in [&sibling, &other_declaration] {
            let mut locals = catalog.function(owner).unwrap();
            let different = locals.reserve_boolean(span).unwrap();
            assert_eq!(different.key.ordinal, 0);
            assert_ne!(different.key(), first.key());
            assert_eq!(
                locals.declaration(first.key()),
                Err(GeneratedLocalError::UnknownLocal)
            );
        }
    });
}

#[test]
fn generated_local_identity_does_not_depend_on_display_name() {
    let (sources, span) = source();
    let original = function(10, 0, span);
    let mut renamed = original.clone();
    renamed.name = crate::VarName::new("SomeOtherDisplayName");
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let issued = catalog
            .function(&original)
            .unwrap()
            .reserve_boolean(span)
            .unwrap();
        assert_eq!(
            catalog
                .function(&renamed)
                .unwrap()
                .declaration(issued.key()),
            Ok(issued)
        );
    });
}

#[test]
fn generated_locals_require_actual_source_and_function_instance_owners() {
    let (sources, span) = source();
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut owner = function(10, 0, span);
        owner.def_id = None;
        assert!(matches!(
            catalog.function(&owner),
            Err(GeneratedLocalError::MissingFunctionDeclaration)
        ));
        owner.def_id = Some(DefId::new(0));
        assert!(matches!(
            catalog.function(&owner),
            Err(GeneratedLocalError::MissingFunctionDeclaration)
        ));
        owner.def_id = Some(DefId::new(10));
        owner.instance_id = None;
        assert!(matches!(
            catalog.function(&owner),
            Err(GeneratedLocalError::MissingFunctionInstance)
        ));
        // Flat's first FunctionInstanceId is zero; absence alone is missing.
        owner.instance_id = Some(FunctionInstanceId::new(0));
        assert!(catalog.function(&owner).is_ok());
    });
}

#[test]
fn invalid_provenance_does_not_reserve_or_skip_a_local() {
    let (sources, span) = source();
    let owner = function(10, 0, span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&owner).unwrap();
        let invalid = [
            Span::DUMMY,
            Span::from_offsets(crate::SourceId::from_source_name("missing.mo"), 1, 2),
            Span::from_offsets(span.source, 20, 11),
            Span::from_offsets(span.source, 1, usize::MAX),
        ];
        for at in invalid {
            assert_eq!(
                locals.reserve_boolean(at),
                Err(GeneratedLocalError::MissingProvenance)
            );
            assert!(locals.declarations().is_empty());
        }
        assert_eq!(locals.reserve_boolean(span).unwrap().key.ordinal, 0);
    });
}

#[test]
fn nonexistent_local_is_refused_even_with_a_known_function_owner() {
    let (sources, span) = source();
    let owner = function(10, 0, span);
    GeneratedFunctionLocalCatalog::construct(&sources, |catalog| {
        let mut locals = catalog.function(&owner).unwrap();
        let issued = locals.reserve_boolean(span).unwrap();
        // Only the owning module can construct this deliberately invalid key.
        let absent = GeneratedFunctionLocalKey {
            ordinal: 1,
            ..issued.key()
        };
        assert_eq!(
            locals.declaration(absent),
            Err(GeneratedLocalError::UnknownLocal)
        );
    });
}
