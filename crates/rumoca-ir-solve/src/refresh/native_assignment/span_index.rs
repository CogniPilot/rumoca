//! Immutable bounding-span candidates; exact periodic ownership stays in Coverage.

#[cfg(test)]
mod tests;

use std::ops::Range;

struct Entry {
    span: Range<usize>,
    source: usize,
    maximum_end: usize,
}

pub(super) struct SpanIndex {
    entries: Vec<Entry>,
}

impl SpanIndex {
    pub(super) fn new(spans: impl Iterator<Item = Range<usize>>) -> Self {
        let mut entries = spans
            .enumerate()
            .map(|(source, span)| Entry {
                maximum_end: span.end,
                span,
                source,
            })
            .collect::<Vec<_>>();
        entries.sort_unstable_by_key(|entry| (entry.span.start, entry.source));
        annotate(&mut entries);
        Self { entries }
    }

    /// A candidate query never validates coverage or evaluates intersection
    /// arithmetic. It omits only disjoint bounding spans, whose exact checker
    /// returns false before its periodic arithmetic. Callers retain source
    /// order and the exact checker for every surviving pair.
    pub(super) fn visit(&self, range: &Range<usize>, visitor: &mut impl FnMut(usize)) {
        if range.start < range.end {
            visit(&self.entries, range, visitor);
        }
    }
}

fn annotate(entries: &mut [Entry]) -> usize {
    let middle = entries.len() / 2;
    let Some((entry, right)) = entries.split_at_mut(middle).1.split_first_mut() else {
        return 0;
    };
    let own_end = entry.span.end;
    let right_end = annotate(right);
    let left_end = annotate(&mut entries[..middle]);
    let maximum_end = own_end.max(left_end).max(right_end);
    entries[middle].maximum_end = maximum_end;
    maximum_end
}

fn visit(entries: &[Entry], range: &Range<usize>, visitor: &mut impl FnMut(usize)) {
    let middle = entries.len() / 2;
    let Some(entry) = entries.get(middle) else {
        return;
    };
    if entry.maximum_end <= range.start {
        return;
    }
    visit(&entries[..middle], range, visitor);
    if entry.span.start < range.end {
        if entry.span.end > range.start {
            visitor(entry.source);
        }
        visit(&entries[middle + 1..], range, visitor);
    }
}
