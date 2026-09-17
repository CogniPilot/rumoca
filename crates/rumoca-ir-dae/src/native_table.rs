//! Recognition of native `ModelicaStandardTables` interpolation entry points.
//!
//! The standard-library table blocks read their data through external C
//! functions (MLS §12.9) whose opaque ExternalObject handle the DAE folds to an
//! integer table id. Lowering passes recognize these consuming entry points by
//! their declared C symbol so the interpolation is projected and lowered onto
//! the solver's native table operators instead of foreign C code.

const COMBI_TABLE_1D_PREFIX: &str = "ModelicaStandardTables_CombiTable1D_";
const COMBI_TIME_TABLE_PREFIX: &str = "ModelicaStandardTables_CombiTimeTable_";

/// One native table interpolation operator, identified by its C entry point.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeTableOperator {
    /// `getValue`: interpolate the table at the abscissa argument.
    Lookup,
    /// `getDerValue`: the interpolation slope times the abscissa derivative.
    Slope,
    /// `minimumAbscissa`/`minimumTime`: the lower abscissa bound.
    BoundsMin,
    /// `maximumAbscissa`/`maximumTime`: the upper abscissa bound.
    BoundsMax,
    /// `nextTimeEvent`: the next table time event at or after the argument.
    NextEvent,
}

impl NativeTableOperator {
    /// The native table operator a C symbol denotes, if it is one of the
    /// recognized `ModelicaStandardTables` consuming entry points. Constructor
    /// (`_init*`) and destructor (`_close`) symbols are not operators and
    /// return `None`.
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        if !symbol.starts_with(COMBI_TABLE_1D_PREFIX)
            && !symbol.starts_with(COMBI_TIME_TABLE_PREFIX)
        {
            return None;
        }
        if symbol.ends_with("_getValue") {
            Some(Self::Lookup)
        } else if symbol.ends_with("_getDerValue") {
            Some(Self::Slope)
        } else if symbol.ends_with("_minimumAbscissa") || symbol.ends_with("_minimumTime") {
            Some(Self::BoundsMin)
        } else if symbol.ends_with("_maximumAbscissa") || symbol.ends_with("_maximumTime") {
            Some(Self::BoundsMax)
        } else if symbol.ends_with("_nextTimeEvent") {
            Some(Self::NextEvent)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::NativeTableOperator;

    #[test]
    fn recognized_consuming_entry_points_map_to_their_operator() {
        for (symbol, operator) in [
            (
                "ModelicaStandardTables_CombiTable1D_getValue",
                NativeTableOperator::Lookup,
            ),
            (
                "ModelicaStandardTables_CombiTimeTable_getValue",
                NativeTableOperator::Lookup,
            ),
            (
                "ModelicaStandardTables_CombiTable1D_getDerValue",
                NativeTableOperator::Slope,
            ),
            (
                "ModelicaStandardTables_CombiTable1D_minimumAbscissa",
                NativeTableOperator::BoundsMin,
            ),
            (
                "ModelicaStandardTables_CombiTimeTable_minimumTime",
                NativeTableOperator::BoundsMin,
            ),
            (
                "ModelicaStandardTables_CombiTable1D_maximumAbscissa",
                NativeTableOperator::BoundsMax,
            ),
            (
                "ModelicaStandardTables_CombiTimeTable_maximumTime",
                NativeTableOperator::BoundsMax,
            ),
            (
                "ModelicaStandardTables_CombiTimeTable_nextTimeEvent",
                NativeTableOperator::NextEvent,
            ),
        ] {
            assert_eq!(NativeTableOperator::from_symbol(symbol), Some(operator));
        }
    }

    #[test]
    fn constructor_destructor_and_foreign_symbols_are_not_operators() {
        // The constructor and destructor manage the handle rather than consume
        // it, a two-dimensional table has no runtime operator, and a foreign C
        // symbol is not a table at all.
        for symbol in [
            "ModelicaStandardTables_CombiTable1D_init3",
            "ModelicaStandardTables_CombiTimeTable_init3",
            "ModelicaStandardTables_CombiTable1D_close",
            "ModelicaStandardTables_CombiTable2D_getValue",
            "my_make_handle",
        ] {
            assert_eq!(NativeTableOperator::from_symbol(symbol), None);
        }
    }
}
