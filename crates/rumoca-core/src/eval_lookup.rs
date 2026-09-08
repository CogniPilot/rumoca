//! Shared lookup contract for compile-time evaluators.
//!
//! Different compiler phases evaluate expressions over different IRs and context
//! representations. This trait defines a common, typed lookup surface so phases
//! can gradually share evaluator logic while keeping phase-local storage.

/// Typed scalar lookup interface used by compile-time evaluators.
///
/// `scope` is a phase-local lexical scope prefix. Implementers may use it for
/// scope-aware lookup (for example `A.B.x`, then `A.x`, then `x`) or ignore it
/// when contexts store fully-qualified names only.
pub trait EvalLookup {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64>;
    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64>;
    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool>;

    fn lookup_integer_root(&self, name: &str) -> Option<i64> {
        self.lookup_integer(name, "")
    }

    fn lookup_real_root(&self, name: &str) -> Option<f64> {
        self.lookup_real(name, "")
    }

    fn lookup_boolean_root(&self, name: &str) -> Option<bool> {
        self.lookup_boolean(name, "")
    }
}

#[cfg(test)]
mod tests {
    use super::EvalLookup;
    use std::collections::HashMap;

    struct MockLookup {
        integers: HashMap<String, i64>,
        reals: HashMap<String, f64>,
        booleans: HashMap<String, bool>,
    }

    impl MockLookup {
        fn scoped_key(name: &str, scope: &str) -> String {
            if scope.is_empty() {
                name.to_string()
            } else {
                format!("{scope}.{name}")
            }
        }

        fn lookup_scoped<T: Copy>(name: &str, scope: &str, map: &HashMap<String, T>) -> Option<T> {
            let scoped = Self::scoped_key(name, scope);
            map.get(&scoped).copied().or_else(|| map.get(name).copied())
        }
    }

    impl EvalLookup for MockLookup {
        fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
            Self::lookup_scoped(name, scope, &self.integers)
        }

        fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
            Self::lookup_scoped(name, scope, &self.reals)
        }

        fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
            Self::lookup_scoped(name, scope, &self.booleans)
        }
    }

    #[test]
    fn root_lookup_helpers_delegate_to_empty_scope() {
        let mut integers = HashMap::new();
        integers.insert("x".to_string(), 3);

        let mut reals = HashMap::new();
        reals.insert("r".to_string(), 2.5);

        let mut booleans = HashMap::new();
        booleans.insert("b".to_string(), true);

        let lookup = MockLookup {
            integers,
            reals,
            booleans,
        };

        assert_eq!(lookup.lookup_integer_root("x"), Some(3));
        assert_eq!(lookup.lookup_real_root("r"), Some(2.5));
        assert_eq!(lookup.lookup_boolean_root("b"), Some(true));
    }

    #[test]
    fn scoped_lookup_uses_scoped_and_root_keys() {
        let mut integers = HashMap::new();
        integers.insert("sys.n".to_string(), 7);
        integers.insert("root".to_string(), 9);

        let lookup = MockLookup {
            integers,
            reals: HashMap::new(),
            booleans: HashMap::new(),
        };

        assert_eq!(lookup.lookup_integer("n", "sys"), Some(7));
        assert_eq!(lookup.lookup_integer("root", "sys"), Some(9));
    }
}
