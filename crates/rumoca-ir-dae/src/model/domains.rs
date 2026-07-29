use super::*;

pub struct Domains<'storage, 'dae> {
    pub(super) source_map: &'storage SourceMap,
    pub(super) storage: &'storage mut Storage,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Domains<'_, 'dae> {
    pub fn structured(
        &mut self,
        domain: StructuredIndexDomain,
        provenance: DaeProvenance,
    ) -> Result<DomainId<'dae>, DaeConstructionError> {
        insert_domain(self.source_map, self.storage, None, domain, provenance)
    }

    pub fn nested(
        &mut self,
        parent: DomainId<'dae>,
        domain: StructuredIndexDomain,
        provenance: DaeProvenance,
    ) -> Result<DomainId<'dae>, DaeConstructionError> {
        insert_domain(
            self.source_map,
            self.storage,
            Some(parent),
            domain,
            provenance,
        )
    }

    pub fn nested_in_scope(
        &mut self,
        enclosing: impl IntoIterator<Item = DomainBinderId<'dae>>,
        domain: StructuredIndexDomain,
        provenance: DaeProvenance,
    ) -> Result<DomainId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let mut parent = None;
        for binder in enclosing {
            let candidate = binder.domain();
            self.storage
                .domain_binder(candidate.index(), binder.ordinal(), provenance)?;
            parent = Some(match parent {
                None => candidate,
                Some(active) => innermost_domain(self.storage, active, candidate, provenance)?,
            });
        }
        match parent {
            Some(parent) => self.nested(parent, domain, provenance),
            None => self.structured(domain, provenance),
        }
    }

    pub fn binder(
        &self,
        domain: DomainId<'dae>,
        ordinal: usize,
        provenance: DaeProvenance,
    ) -> Result<DomainBinderId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let ordinal = checked_u32(ordinal, "domain binder ordinal", provenance)?;
        self.storage
            .domain_binder(domain.index(), ordinal, provenance)?;
        Ok(DomainBinderId::from_raw(domain.index(), ordinal))
    }
}

fn innermost_domain<'dae>(
    storage: &Storage,
    active: DomainId<'dae>,
    candidate: DomainId<'dae>,
    provenance: DaeProvenance,
) -> Result<DomainId<'dae>, DaeConstructionError> {
    if storage.domain_is_ancestor_or_same(active.index(), candidate.index(), provenance)? {
        return Ok(candidate);
    }
    if storage.domain_is_ancestor_or_same(candidate.index(), active.index(), provenance)? {
        return Ok(active);
    }
    Err(DaeConstructionError::InvalidBinderScope {
        expected_domain: Some(active.index()),
        found_domain: candidate.index(),
        span: provenance.span(),
    })
}

pub(crate) fn insert_domain<'dae>(
    source_map: &SourceMap,
    storage: &mut Storage,
    parent: Option<DomainId<'dae>>,
    domain: StructuredIndexDomain,
    provenance: DaeProvenance,
) -> Result<DomainId<'dae>, DaeConstructionError> {
    check_provenance(source_map, provenance)?;
    let parent = parent
        .map(|parent| {
            storage
                .domains
                .get(parent.index() as usize)
                .map(|_| parent.index())
                .ok_or_else(|| unknown("domain", parent.index(), provenance))
        })
        .transpose()?;
    let scalar_count =
        domain
            .scalar_count()
            .map_err(|source| DaeConstructionError::InvalidDomain {
                source,
                span: provenance.span(),
            })?;
    for (index, binder) in domain.binders.iter().enumerate() {
        if domain.binders[..index]
            .iter()
            .any(|candidate| candidate.id == binder.id)
        {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "domain binder",
                key: binder.display_name.clone(),
                span: provenance.span(),
            });
        }
    }
    let extents = domain
        .extents()
        .map_err(|source| DaeConstructionError::InvalidDomain {
            source,
            span: provenance.span(),
        })?
        .into_iter()
        .map(|extent| checked_u32(extent, "domain extent", provenance))
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    let scalar_count = checked_u32(scalar_count, "domain scalar count", provenance)?;
    let raw = checked_u32(storage.domains.len(), "domain arena", provenance)?;
    storage.domains.push(DomainEntry {
        parent,
        domain,
        extents,
        scalar_count,
        provenance,
    });
    Ok(DomainId::from_raw(raw))
}
