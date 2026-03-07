use super::*;

use rumoca_core::EvalLookup;
use std::borrow::Cow;

impl EvalLookup for TypeCheckEvalContext {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
        lookup_with_scope(name, scope, &self.integers, self.suffix_index.as_ref())
            .copied()
            .or_else(|| {
                lookup_with_scope(name, scope, &self.enum_ordinals, self.suffix_index.as_ref())
                    .copied()
            })
    }

    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
        lookup_with_scope(name, scope, &self.reals, self.suffix_index.as_ref())
            .copied()
            .or_else(|| {
                lookup_with_scope(name, scope, &self.integers, self.suffix_index.as_ref())
                    .map(|value| *value as f64)
            })
    }

    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
        lookup_with_scope(name, scope, &self.booleans, self.suffix_index.as_ref()).copied()
    }

    fn lookup_enum<'a>(&'a self, name: &str, scope: &str) -> Option<Cow<'a, str>> {
        lookup_with_scope(name, scope, &self.enums, self.suffix_index.as_ref())
            .map(|value| Cow::Borrowed(value.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_trait_resolves_scope_and_enum_ordinals() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("sys.n", 4);
        ctx.add_real("sys.inner.r", 2.5);
        ctx.booleans.insert("sys.flag".to_string(), true);
        ctx.enums
            .insert("sys.mode".to_string(), "Pkg.Mode.Fast".to_string());
        ctx.enum_ordinals.insert("sys.phase".to_string(), 3);
        ctx.build_suffix_index();

        assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(4));
        assert_eq!(ctx.lookup_integer("phase", "sys.inner"), Some(3));
        assert_eq!(ctx.lookup_real("r", "sys.inner"), Some(2.5));
        assert_eq!(ctx.lookup_real("n", "sys.inner"), Some(4.0));
        assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
        assert_eq!(
            ctx.lookup_enum("mode", "sys.inner").as_deref(),
            Some("Pkg.Mode.Fast")
        );
    }
}
