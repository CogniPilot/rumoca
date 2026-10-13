//! The values a top-level conditional may join path-partially.

use super::*;
use std::sync::Arc;

/// The values the conditional at one top-level position may join
/// path-partially: the function's outputs and every value a flat top-level
/// statement at or after `from` reads.
///
/// The reads of the whole sequence are recorded once as the position of the
/// last flat statement reading each value; a conditional's admission is a view
/// over that table, so a sequence of `n` conditionals costs one walk of the
/// statements rather than one per conditional.
#[derive(Clone, Default)]
pub(in crate::construction::analysis) struct PathPartialAdmission {
    returned: Arc<HashSet<VarName>>,
    last_read: Arc<HashMap<VarName, usize>>,
    from: usize,
}

impl PathPartialAdmission {
    pub(in crate::construction::analysis) fn new(
        returned: HashSet<VarName>,
        last_read: HashMap<VarName, usize>,
    ) -> Self {
        Self {
            returned: Arc::new(returned),
            last_read: Arc::new(last_read),
            from: 0,
        }
    }

    /// The admission of the conditional whose later statements start at `from`.
    pub(in crate::construction::analysis) fn starting_at(&self, from: usize) -> Self {
        Self {
            from,
            ..self.clone()
        }
    }

    pub(in crate::construction::analysis) fn contains(&self, name: &VarName) -> bool {
        self.returned.contains(name)
            || self
                .last_read
                .get(name)
                .is_some_and(|&last| last >= self.from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name(text: &str) -> VarName {
        VarName::new(text)
    }

    #[test]
    fn admission_holds_outputs_and_values_read_at_or_after_its_position() {
        let admission = PathPartialAdmission::new(
            HashSet::from([name("y")]),
            HashMap::from([(name("early"), 1), (name("late"), 5)]),
        );
        let at_three = admission.starting_at(3);
        assert!(at_three.contains(&name("y")));
        assert!(at_three.contains(&name("late")));
        assert!(!at_three.contains(&name("early")));
        assert!(!at_three.contains(&name("never")));
        let at_one = admission.starting_at(1);
        assert!(at_one.contains(&name("early")));
        assert!(admission.starting_at(6).contains(&name("y")));
        assert!(!admission.starting_at(6).contains(&name("late")));
    }
}
