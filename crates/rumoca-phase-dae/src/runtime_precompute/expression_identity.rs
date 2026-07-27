use indexmap::IndexMap;

#[derive(Default)]
pub(super) struct UniqueExpressions {
    expressions: Vec<rumoca_core::Expression>,
    buckets: IndexMap<u64, Vec<usize>>,
}

impl UniqueExpressions {
    pub(super) fn push(&mut self, candidate: rumoca_core::Expression) {
        let fingerprint = rumoca_core::expression_semantic_fingerprint(&candidate);
        if self.has_semantic_match(fingerprint, &candidate) {
            return;
        }

        let idx = self.expressions.len();
        self.expressions.push(candidate);
        self.buckets.entry(fingerprint).or_default().push(idx);
    }

    fn has_semantic_match(&self, fingerprint: u64, candidate: &rumoca_core::Expression) -> bool {
        self.buckets.get(&fingerprint).is_some_and(|bucket| {
            bucket.iter().any(|idx| {
                rumoca_core::expressions_semantically_equal(&self.expressions[*idx], candidate)
            })
        })
    }

    pub(super) fn into_vec(self) -> Vec<rumoca_core::Expression> {
        self.expressions
    }
}
