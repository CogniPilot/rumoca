// Invariant, brand-carrying outputs returned by a closure.
//
// This is the shape the real DAE `1203` sites have and that
// `closure-lifetimes/two-region-iterator.rs` cannot express. Three properties
// matter here and none of them appear in that file:
//
//   1. Two INDEPENDENT lifetime parameters, one used only by a phantom brand
//      and one carrying a real borrow. Nothing in the source ties them.
//   2. Outputs whose lifetimes are INVARIANT, because the brand is
//      `PhantomData<&'x mut &'x ()>`. A repair that supplies an outlives edge
//      is not enough where the position is invariant; the relation has to be
//      equality.
//   3. Output tuples that use the two parameters in different orders, so a
//      reconstruction that is positionally right by luck is distinguishable
//      from one that resolves each slot.
//
// On testing invariance: invariance is a type-level property and cannot be
// asserted at run time, because a covariant type also accepts the exact
// lifetime. The tests below therefore pin layout and referent identity, which
// are checkable, and the invariance is left to the type checker. A compile-fail
// test would be the honest way to pin it and this file does not attempt one.
//
// Expected declaration shape, stated so an emitted contract can be compared
// against the source rather than against current output:
//
//   Table<'brand, 'data>            'brand phantom-only, 'data a real borrow
//   BrandId<'brand>                 phantom-only, invariant, no runtime referent
//   DataId<'data>                   phantom-only, invariant, no runtime referent
//   View<'brand, 'data>             mixed: a real &'data borrow, invariant at BOTH
//                                   'brand and 'data. The DAE VariableView is
//                                   invariant in the same region it stores a
//                                   borrow at, so a borrow region that stayed
//                                   covariant would not exercise the case.
//
//   ordered  -> Item = (BrandId<'brand>, View<'brand, 'data>)
//   swapped  -> Item = (View<'brand, 'data>, BrandId<'brand>)
//   both_ids -> Item = (BrandId<'brand>, DataId<'data>)   two phantom-only slots
//                                                          at DIFFERENT parameters

use std::marker::PhantomData;

/// Phantom-only at `'brand`: a `u32` and a zero-sized invariant marker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BrandId<'brand> {
    raw: u32,
    brand: PhantomData<&'brand mut &'brand ()>,
}

/// Phantom-only at `'data`. Distinct from `BrandId` so a contract that merged
/// the two parameters would place the wrong one in a slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DataId<'data> {
    raw: u32,
    brand: PhantomData<&'data mut &'data ()>,
}

/// Mixed: a genuine borrow at `'data` beside an invariant brand at `'brand`.
#[derive(Clone, Copy, Debug)]
pub struct View<'brand, 'data> {
    entry: &'data u32,
    brand: PhantomData<&'brand mut &'brand ()>,
    // `'data` carries the real borrow above AND is made invariant here, which is
    // the shape of the DAE `VariableView<'dae>`: one region that is both stored
    // and invariant. Without this the borrow region would be covariant and the
    // fixture would not exercise the case that needs equality rather than an
    // outlives edge.
    data_brand: PhantomData<&'data mut &'data ()>,
}

impl<'brand, 'data> View<'brand, 'data> {
    pub fn entry(self) -> &'data u32 {
        self.entry
    }
}

impl BrandId<'_> {
    pub fn raw(self) -> u32 {
        self.raw
    }
}

impl DataId<'_> {
    pub fn raw(self) -> u32 {
        self.raw
    }
}

#[derive(Clone, Copy)]
pub struct Table<'brand, 'data> {
    entries: &'data [u32],
    brand: PhantomData<&'brand mut &'brand ()>,
    // Invariant at 'data as well, matching `DaeView<'dae>`. Without this the
    // source can be SHORTENED at the row call before yielding an invariant
    // `View`, and capture and output land in different constraint components.
    data_brand: PhantomData<&'data mut &'data ()>,
}

impl<'brand, 'data> Table<'brand, 'data> {
    pub fn new(entries: &'data [u32]) -> Self {
        Table {
            entries,
            brand: PhantomData,
            data_brand: PhantomData,
        }
    }

    /// A typed row accessor whose signature relates BOTH parameters to its
    /// branded return values. The iterator methods below go through it, so the
    /// whole-table capture is required by the code rather than incidental.
    pub fn row(self, index: usize) -> (BrandId<'brand>, View<'brand, 'data>) {
        (
            BrandId {
                raw: index as u32,
                brand: PhantomData,
            },
            View {
                entry: &self.entries[index],
                brand: PhantomData,
                data_brand: PhantomData,
            },
        )
    }

    /// Outputs in declaration order.
    pub fn ordered(
        self,
    ) -> impl ExactSizeIterator<Item = (BrandId<'brand>, View<'brand, 'data>)> + use<'brand, 'data>
    {
        (0..self.entries.len()).map(move |index| self.row(index))
    }

    /// The same two components in the opposite order.
    pub fn swapped(
        self,
    ) -> impl ExactSizeIterator<Item = (View<'brand, 'data>, BrandId<'brand>)> + use<'brand, 'data>
    {
        (0..self.entries.len()).map(move |index| {
            let (id, view) = self.row(index);
            (view, id)
        })
    }

    /// Two phantom-only outputs at DIFFERENT parameters. A contract that merged
    /// `'brand` and `'data` would be indistinguishable from the correct one on
    /// `ordered` alone; here the two slots must stay apart.
    pub fn both_ids(
        self,
    ) -> impl ExactSizeIterator<Item = (BrandId<'brand>, DataId<'data>)> + use<'brand, 'data> {
        (0..self.entries.len()).map(move |index| {
            (
                BrandId {
                    raw: index as u32,
                    brand: PhantomData,
                },
                DataId {
                    raw: self.entries[index],
                    brand: PhantomData,
                },
            )
        })
    }

    /// CONTROL, not a counterexample: the closure is called once inside the
    /// defining function before being returned, so the returned value is not the
    /// freshly constructed one. This pins that behaviour so a rule which assumes
    /// a closure is only used by its caller has something to fail against; it
    /// does not by itself falsify canonical metadata generalization.
    pub fn used_before_return(
        self,
    ) -> impl ExactSizeIterator<Item = (BrandId<'brand>, DataId<'data>)> + use<'brand, 'data> {
        let mut rows = self.both_ids();
        let _consumed_internally = rows.next();
        rows
    }
}

/// Covariant-source control. Same invariant `View` output, but the source is
/// covariant at `'data`, so it can be SHORTENED at the row call before yielding
/// the invariant view. Measured on the constraint graph, capture and output then
/// land in different components with only one direction required, which
/// falsifies the tempting rule that an invariant output always forces
/// capture-to-output equality. The variance of the INPUT matters too.
#[derive(Clone, Copy)]
pub struct CovariantTable<'brand, 'data> {
    entries: &'data [u32],
    brand: PhantomData<&'brand mut &'brand ()>,
}

impl<'brand, 'data> CovariantTable<'brand, 'data> {
    pub fn new(entries: &'data [u32]) -> Self {
        CovariantTable {
            entries,
            brand: PhantomData,
        }
    }

    pub fn row(self, index: usize) -> (BrandId<'brand>, View<'brand, 'data>) {
        (
            BrandId {
                raw: index as u32,
                brand: PhantomData,
            },
            View {
                entry: &self.entries[index],
                brand: PhantomData,
                data_brand: PhantomData,
            },
        )
    }

    pub fn ordered(
        self,
    ) -> impl ExactSizeIterator<Item = (BrandId<'brand>, View<'brand, 'data>)> + use<'brand, 'data>
    {
        (0..self.entries.len()).map(move |index| self.row(index))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phantom_only_ids_carry_no_storage() {
        assert_eq!(size_of::<BrandId<'_>>(), size_of::<u32>());
        assert_eq!(size_of::<DataId<'_>>(), size_of::<u32>());
        assert_eq!(size_of::<View<'_, '_>>(), size_of::<&u32>());
        assert_eq!(size_of::<Table<'_, '_>>(), size_of::<&[u32]>());
    }

    #[test]
    fn ordered_view_borrows_the_named_entry() {
        let entries = [7u32, 9u32];
        let table = Table::new(&entries);
        let rows: Vec<_> = table.ordered().collect();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[1].0.raw(), 1);
        assert!(std::ptr::eq(rows[1].1.entry(), &entries[1]));
    }

    #[test]
    fn swapped_agrees_with_ordered_component_wise() {
        let entries = [7u32, 9u32];
        let table = Table::new(&entries);
        for (ordered, swapped) in table.ordered().zip(table.swapped()) {
            assert_eq!(ordered.0, swapped.1);
            assert!(std::ptr::eq(ordered.1.entry(), swapped.0.entry()));
        }
    }

    #[test]
    fn the_two_phantom_slots_are_not_interchangeable() {
        let entries = [7u32, 9u32];
        let table = Table::new(&entries);
        let rows: Vec<_> = table.both_ids().collect();
        // The first slot indexes the row; the second carries the entry value.
        assert_eq!((rows[0].0.raw(), rows[0].1.raw()), (0, 7));
        assert_eq!((rows[1].0.raw(), rows[1].1.raw()), (1, 9));
    }

    #[test]
    fn covariant_source_yields_the_same_rows() {
        let entries = [7u32, 9u32];
        let table = CovariantTable::new(&entries);
        let rows: Vec<_> = table.ordered().collect();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[1].0.raw(), 1);
        assert!(std::ptr::eq(rows[1].1.entry(), &entries[1]));
    }

    #[test]
    fn internal_use_consumes_one_row_before_return() {
        let entries = [7u32, 9u32];
        let table = Table::new(&entries);
        let rows: Vec<_> = table.used_before_return().collect();
        assert_eq!(rows.len(), 1);
        assert_eq!((rows[0].0.raw(), rows[0].1.raw()), (1, 9));
    }
}
