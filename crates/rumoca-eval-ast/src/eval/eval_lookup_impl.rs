use super::*;

use rumoca_core::EvalLookup;

impl EvalLookup for TypeCheckEvalContext {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
        lookup_with_scope(name, scope, &self.integers)
            .copied()
            .or_else(|| lookup_with_scope(name, scope, &self.enum_ordinals).copied())
    }

    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
        lookup_with_scope(name, scope, &self.reals)
            .copied()
            .or_else(|| lookup_with_scope(name, scope, &self.integers).map(|value| *value as f64))
    }

    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
        lookup_with_scope(name, scope, &self.booleans).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_trait_resolves_scope_and_enum_ordinals() {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.add_integer("sys.n", 4);
        ctx.add_real("sys.inner.r", 2.5);
        ctx.booleans.insert("sys.flag".to_string(), true);
        ctx.enum_ordinals.insert("sys.phase".to_string(), 3);

        assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(4));
        assert_eq!(ctx.lookup_integer("phase", "sys.inner"), Some(3));
        assert_eq!(ctx.lookup_real("r", "sys.inner"), Some(2.5));
        assert_eq!(ctx.lookup_real("n", "sys.inner"), Some(4.0));
        assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
    }
}
