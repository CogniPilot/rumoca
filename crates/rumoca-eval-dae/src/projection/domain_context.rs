use std::collections::HashMap;
use std::sync::Arc;

use rumoca_ir_dae as dae;

/// Equality of one exact lexical domain-point sequence within a projection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(super) struct DomainContextId(usize);

impl DomainContextId {
    /// Address within a sparse fixed-size page, never a dense allocation index.
    pub(super) fn page_address(self) -> (usize, usize) {
        (self.0 / 64, self.0 % 64)
    }

    #[cfg(test)]
    pub(super) fn test_identity(index: usize) -> Self {
        Self(index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::projection::tests::domain_context::domains_model;

    #[test]
    fn full_context_empty_and_denied_admission_preserve_allocation_counts() {
        domains_model().inspect(|view| {
            let domain = view.domain_id(0).unwrap();
            let mut contexts = DomainContexts::default();
            assert_eq!(
                contexts.full_context(false),
                Some(DomainContextId::default())
            );
            assert_eq!(contexts.full_created, 0);
            assert!(contexts.contexts.is_empty());
            contexts.push(domain, vec![1]);
            assert_eq!(contexts.full_context(false), None);
            assert_eq!(contexts.full_created, 0);
            assert!(contexts.interned.is_empty());
            let identity = contexts.full_context(true).unwrap();
            assert_eq!(contexts.full_created, 1);
            contexts.pop();
            contexts.push(domain, vec![1]);
            assert_eq!(contexts.full_context(false), Some(identity));
            assert_eq!(contexts.full_created, 1);
            assert_eq!(contexts.contexts.len(), 1);
        });
    }

    #[test]
    fn full_context_cap_retains_existing_and_ordinary_interned_identities() {
        domains_model().inspect(|view| {
            let outer = view.domain_id(0).unwrap();
            let inner = view.domain_id(1).unwrap();
            let mut contexts = DomainContexts::new(vec![(outer, vec![1])]);
            contexts.full_created = 65_535;
            let retained = contexts.full_context(true).unwrap();
            assert_eq!(contexts.full_created, 65_536);
            contexts.push(inner, vec![1]);
            assert_eq!(contexts.full_context(true), None);
            assert_eq!(contexts.full_context(false), None);
            assert_eq!(contexts.contexts.len(), 1);
            let ordinary = contexts.for_domain(view, Some(inner));
            assert_eq!(contexts.full_context(false), Some(ordinary));
            assert_eq!(contexts.full_created, 65_536);
            contexts.pop();
            assert_eq!(contexts.full_context(false), Some(retained));
            assert_eq!(
                contexts.snapshot(retained).as_ref(),
                &vec![(outer.index(), vec![1])]
            );
            assert_eq!(contexts.contexts.len(), 2);
        });
    }
}

pub(super) type DomainPoint<'dae> = (dae::DomainId<'dae>, Vec<i64>);
pub(super) type Context = Vec<(u32, Vec<i64>)>;

/// Projection-local identities for checked lexical environments, never IR data.
///
/// Point order and repeated domains remain significant. Changing the active
/// stack invalidates its derived lookup; equal sequences reuse an identity.
#[derive(Default)]
pub(super) struct DomainContexts<'dae> {
    pub(super) points: Vec<DomainPoint<'dae>>,
    lexical_domains: HashMap<u32, Vec<u32>>,
    current: HashMap<u32, DomainContextId>,
    full_current: Option<DomainContextId>,
    full_created: usize,
    interned: HashMap<Arc<Context>, DomainContextId>,
    contexts: Vec<Arc<Context>>,
    empty: Arc<Context>,
    #[cfg(test)]
    pub(super) builds: usize,
}

impl<'dae> DomainContexts<'dae> {
    pub(super) fn new(points: Vec<DomainPoint<'dae>>) -> Self {
        Self {
            points,
            ..Self::default()
        }
    }

    pub(super) fn push(&mut self, domain: dae::DomainId<'dae>, point: Vec<i64>) {
        self.points.push((domain, point));
        self.current.clear();
        self.full_current = None;
    }

    pub(super) fn pop(&mut self) {
        self.points.pop();
        self.current.clear();
        self.full_current = None;
    }

    pub(super) fn replace(&mut self, points: Vec<DomainPoint<'dae>>) -> Vec<DomainPoint<'dae>> {
        self.current.clear();
        self.full_current = None;
        std::mem::replace(&mut self.points, points)
    }

    pub(super) fn snapshot(&self, identity: DomainContextId) -> Arc<Context> {
        if identity == DomainContextId::default() {
            Arc::clone(&self.empty)
        } else {
            Arc::clone(&self.contexts[identity.0 - 1])
        }
    }

    pub(super) fn for_domain(
        &mut self,
        view: dae::DaeView<'dae>,
        domain: Option<dae::DomainId<'dae>>,
    ) -> DomainContextId {
        let Some(domain) = domain else {
            return DomainContextId::default();
        };
        if let Some(context) = self.current.get(&domain.index()) {
            return *context;
        }
        #[cfg(test)]
        {
            self.builds += 1;
        }
        let lexical_domains = self
            .lexical_domains
            .entry(domain.index())
            .or_insert_with(|| {
                let mut domains = Vec::new();
                let mut current = Some(domain);
                while let Some(domain) = current {
                    domains.push(domain.index());
                    current = view
                        .domain(domain)
                        .expect("checked expression binder domain resolves")
                        .parent();
                }
                domains
            });
        let context = self
            .points
            .iter()
            .filter(|(domain, _)| lexical_domains.contains(&domain.index()))
            .map(|(domain, point)| (domain.index(), point.clone()))
            .collect::<Context>();
        let identity = self.intern(context);
        self.current.insert(domain.index(), identity);
        identity
    }

    /// Complete ordered lexical stack, including unrelated/repeated domains.
    pub(super) fn full_context(&mut self, allow_new: bool) -> Option<DomainContextId> {
        if let Some(identity) = self.full_current {
            return Some(identity);
        }
        let context = self
            .points
            .iter()
            .map(|(domain, point)| (domain.index(), point.clone()))
            .collect::<Context>();
        let identity = if context.is_empty() {
            DomainContextId::default()
        } else if let Some(identity) = self.interned.get(&context) {
            *identity
        } else {
            if !allow_new || self.full_created == 65_536 {
                return None;
            }
            self.full_created += 1;
            self.intern_new(context)
        };
        self.full_current = Some(identity);
        Some(identity)
    }

    fn intern(&mut self, context: Context) -> DomainContextId {
        if context.is_empty() {
            DomainContextId::default()
        } else if let Some(identity) = self.interned.get(&context) {
            *identity
        } else {
            self.intern_new(context)
        }
    }

    /// The caller has already proved this nonempty exact context is absent.
    fn intern_new(&mut self, context: Context) -> DomainContextId {
        let next = DomainContextId(self.contexts.len() + 1);
        let context = Arc::new(context);
        self.contexts.push(Arc::clone(&context));
        let previous = self.interned.insert(context, next);
        debug_assert!(previous.is_none());
        next
    }
}
