//! Declaration-shaped scalar names: one name and shape per declared variable.

use serde::{Deserialize, Serialize};

/// One declared variable's scalar names, derived from its name and shape.
///
/// Scalar `k` of the run is named by
/// [`rumoca_core::scalar_component_name`] over the declaration name and its
/// row-major dimensions, so the run stores no per-scalar string.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolveScalarNameRun {
    name: String,
    dimensions: Vec<u32>,
}

impl SolveScalarNameRun {
    /// A declaration named `name` with the given row-major `dimensions`
    /// (empty for a scalar).
    #[must_use]
    pub fn new(name: impl Into<String>, dimensions: Vec<u32>) -> Self {
        Self {
            name: name.into(),
            dimensions,
        }
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    fn scalar_count(&self) -> Option<usize> {
        self.dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
    }

    fn scalar_name(&self, ordinal: usize) -> Option<String> {
        rumoca_core::scalar_component_name(&self.name, &self.dimensions, ordinal)
    }

    /// The ordinal of the scalar `target` names, if this run declares it.
    fn ordinal_of(&self, target: &str) -> Option<usize> {
        if self.dimensions.is_empty() {
            return (self.name == target).then_some(0);
        }
        let scalar = rumoca_core::parse_scalar_name(target)?;
        if scalar.base != self.name {
            return None;
        }
        let coordinates = scalar
            .indices
            .iter()
            .map(|index| {
                index
                    .checked_sub(1)
                    .and_then(|index| u32::try_from(index).ok())
            })
            .collect::<Option<Vec<_>>>()?;
        rumoca_core::flatten_coordinates(&self.dimensions, &coordinates)
    }
}

/// The scalar names of an ordered list of declarations, in storage order.
///
/// The read API is that of a name table (`len`, `get`, `iter`, `position`);
/// the storage and the wire form hold one [`SolveScalarNameRun`] per declared
/// variable, never one string per scalar.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "Vec<SolveScalarNameRun>", into = "Vec<SolveScalarNameRun>")]
pub struct SolveScalarNames {
    runs: Vec<SolveScalarNameRun>,
    /// `starts[i]` is the first scalar ordinal of `runs[i]`.
    starts: Vec<usize>,
    len: usize,
}

/// A run list whose scalar extent does not fit the address space.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SolveScalarNamesOverflow;

impl std::fmt::Display for SolveScalarNamesOverflow {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("scalar name runs exceed the addressable scalar count")
    }
}

impl std::error::Error for SolveScalarNamesOverflow {}

impl TryFrom<Vec<SolveScalarNameRun>> for SolveScalarNames {
    type Error = SolveScalarNamesOverflow;

    fn try_from(runs: Vec<SolveScalarNameRun>) -> Result<Self, Self::Error> {
        let mut names = Self::default();
        for run in runs {
            names.push_run(run)?;
        }
        Ok(names)
    }
}

impl From<SolveScalarNames> for Vec<SolveScalarNameRun> {
    fn from(names: SolveScalarNames) -> Self {
        names.runs
    }
}

impl From<Vec<String>> for SolveScalarNames {
    /// Every name is its own scalar declaration.
    fn from(names: Vec<String>) -> Self {
        names.into_iter().collect()
    }
}

impl FromIterator<String> for SolveScalarNames {
    fn from_iter<I: IntoIterator<Item = String>>(names: I) -> Self {
        let mut table = Self::default();
        for name in names {
            table.push_scalar(name);
        }
        table
    }
}

impl SolveScalarNames {
    /// Append one declaration.
    pub fn push_run(&mut self, run: SolveScalarNameRun) -> Result<(), SolveScalarNamesOverflow> {
        let count = run.scalar_count().ok_or(SolveScalarNamesOverflow)?;
        let len = self
            .len
            .checked_add(count)
            .ok_or(SolveScalarNamesOverflow)?;
        self.starts.push(self.len);
        self.runs.push(run);
        self.len = len;
        Ok(())
    }

    /// Append one scalar declaration named `name`.
    pub fn push_scalar(&mut self, name: impl Into<String>) {
        self.starts.push(self.len);
        self.runs.push(SolveScalarNameRun::new(name, Vec::new()));
        self.len += 1;
    }

    /// The declarations, one per variable.
    #[must_use]
    pub fn runs(&self) -> &[SolveScalarNameRun] {
        &self.runs
    }

    /// The number of scalars.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.len
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// The name of scalar `index`, derived from its declaration.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<String> {
        if index >= self.len {
            return None;
        }
        let run = self
            .starts
            .partition_point(|start| *start <= index)
            .checked_sub(1)?;
        self.runs[run].scalar_name(index - self.starts[run])
    }

    /// Every scalar name in storage order, derived on demand.
    pub fn iter(&self) -> impl Iterator<Item = String> + '_ {
        self.runs.iter().flat_map(|run| {
            let count = run.scalar_count().unwrap_or(0);
            (0..count).filter_map(move |ordinal| run.scalar_name(ordinal))
        })
    }

    /// The first scalar ordinal named `name`.
    #[must_use]
    pub fn position(&self, name: &str) -> Option<usize> {
        self.runs
            .iter()
            .zip(&self.starts)
            .find_map(|(run, start)| run.ordinal_of(name).map(|ordinal| start + ordinal))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table() -> SolveScalarNames {
        let mut names = SolveScalarNames::default();
        names.push_scalar("u");
        names
            .push_run(SolveScalarNameRun::new("frame.pixels", vec![2, 3]))
            .unwrap();
        names
            .push_run(SolveScalarNameRun::new("empty", vec![0]))
            .unwrap();
        names
            .push_run(SolveScalarNameRun::new("gain", vec![2]))
            .unwrap();
        names
    }

    #[test]
    fn names_are_derived_from_declarations_in_storage_order() {
        let names = table();
        let expected = [
            "u",
            "frame.pixels[1,1]",
            "frame.pixels[1,2]",
            "frame.pixels[1,3]",
            "frame.pixels[2,1]",
            "frame.pixels[2,2]",
            "frame.pixels[2,3]",
            "gain[1]",
            "gain[2]",
        ];
        assert_eq!(names.len(), expected.len());
        assert_eq!(names.iter().collect::<Vec<_>>(), expected);
        for (index, name) in expected.iter().enumerate() {
            assert_eq!(names.get(index).as_deref(), Some(*name));
            assert_eq!(names.position(name), Some(index));
        }
        assert_eq!(names.get(expected.len()), None);
        assert_eq!(names.position("frame.pixels[3,1]"), None);
        assert_eq!(names.position("frame.pixels[1]"), None);
        assert_eq!(names.position("frame.pixels[0,1]"), None);
        assert_eq!(names.position("missing"), None);
    }

    #[test]
    fn the_wire_holds_one_entry_per_declaration() {
        let mut names = SolveScalarNames::default();
        names
            .push_run(SolveScalarNameRun::new("depth", vec![480, 640]))
            .unwrap();
        let wire = serde_json::to_string(&names).unwrap();
        assert_eq!(wire, r#"[{"name":"depth","dimensions":[480,640]}]"#);
        let replayed: SolveScalarNames = serde_json::from_str(&wire).unwrap();
        assert_eq!(replayed, names);
        assert_eq!(replayed.len(), 480 * 640);
        assert_eq!(replayed.get(640).as_deref(), Some("depth[2,1]"));
    }

    #[test]
    fn a_run_list_that_overflows_the_scalar_count_is_refused() {
        let wire = r#"[{"name":"a","dimensions":[4294967295,4294967295,4294967295]}]"#;
        assert!(serde_json::from_str::<SolveScalarNames>(wire).is_err());
    }

    #[test]
    fn plain_scalar_names_keep_their_text() {
        let names: SolveScalarNames = vec!["x[1]".to_string(), "x[2]".to_string()].into();
        assert_eq!(names.position("x[2]"), Some(1));
        assert_eq!(names.get(0).as_deref(), Some("x[1]"));
    }

    #[test]
    fn a_table_is_empty_until_a_declaration_adds_a_scalar() {
        let mut names = SolveScalarNames::default();
        assert!(names.is_empty());
        names
            .push_run(SolveScalarNameRun::new("none", vec![0]))
            .unwrap();
        assert!(names.is_empty());
        names.push_scalar("x");
        assert!(!names.is_empty());
    }
}
