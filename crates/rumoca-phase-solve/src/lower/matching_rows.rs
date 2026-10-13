//! Matching queries retain issued family recipes and authored scalar matches.

use super::*;
use matched_projection::MatchedFamily;

#[derive(Default)]
pub(super) struct MatchingRows<'dae> {
    explicit: HashMap<usize, UnknownId<'dae>>,
    families: Vec<MatchedFamily<'dae>>,
    algebraic_rows: solve::IndexIntervals,
}

impl<'dae> FromIterator<(usize, UnknownId<'dae>)> for MatchingRows<'dae> {
    fn from_iter<I: IntoIterator<Item = (usize, UnknownId<'dae>)>>(iter: I) -> Self {
        let explicit = iter.into_iter().collect::<HashMap<_, _>>();
        let algebraic_rows =
            solve::IndexIntervals::of(explicit.iter().filter_map(|(&row, unknown)| {
                matches!(unknown, UnknownId::Algebraic { .. }).then_some(row)
            }));
        Self {
            explicit,
            algebraic_rows,
            families: Vec::new(),
        }
    }
}

impl<'dae> MatchingRows<'dae> {
    pub(super) fn construct(
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
        sorted: &structural::SortedDae<'dae>,
    ) -> Result<Self, LowerError> {
        let mut families = Vec::new();
        let mut family_rows = solve::IndexIntervals::default();
        let mut algebraic_rows = solve::IndexIntervals::default();
        for block in &sorted.blocks {
            let BltBlock::StructuredScalar(family) = block else {
                continue;
            };
            let Some(family) = MatchedFamily::admit(view, layout, family)? else {
                continue;
            };
            let range = family.matching().row_range();
            let rows = solve::IndexIntervals::range(range.start, range.end);
            if !family_rows.intersection(&rows).is_empty() {
                return Err(LowerError::contract(
                    "issued matching families overlap",
                    family.matching().span(),
                ));
            }
            family_rows = family_rows.union(&rows);
            if family.all_matches_are_algebraic() {
                algebraic_rows = algebraic_rows.union(&rows);
            }
            families.push(family);
        }
        // The structural producer still owns its scalar matching vector. Read
        // it once, retaining only matches outside the issued family recipes.
        let explicit = sorted
            .matching
            .iter()
            .filter_map(|(row, unknown)| {
                (!family_rows.contains(row.0)).then_some((row.0, *unknown))
            })
            .collect::<HashMap<_, _>>();
        for (&row, unknown) in &explicit {
            if matches!(unknown, UnknownId::Algebraic { .. }) {
                algebraic_rows.insert(row);
            }
        }
        Ok(Self {
            explicit,
            families,
            algebraic_rows,
        })
    }

    pub(super) fn get(&self, row: &usize) -> Option<UnknownId<'dae>> {
        self.explicit.get(row).copied().or_else(|| {
            self.families
                .iter()
                .find_map(|family| family.matched_unknown(*row))
        })
    }

    pub(super) fn checked_row_count(&self) -> Option<usize> {
        self.families
            .iter()
            .try_fold(self.explicit.len(), |count, family| {
                let rows = family.matching().row_range();
                count.checked_add(rows.end.checked_sub(rows.start)?)
            })
    }

    pub(super) fn all_algebraic(&self, rows: std::ops::Range<usize>) -> bool {
        solve::IndexIntervals::range(rows.start, rows.end).is_subset(&self.algebraic_rows)
    }

    #[cfg(test)]
    pub(super) fn storage_counts(&self) -> (usize, usize) {
        (self.explicit.len(), self.families.len())
    }
}
