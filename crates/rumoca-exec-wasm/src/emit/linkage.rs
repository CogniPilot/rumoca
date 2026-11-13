//! The link names an assembler prints. They are the only bytes in which a
//! linked library differs from the browser module of the same source.
use wasm_encoder::{ExportKind, ExportSection};

/// The browser profile keeps the names and the public memory re-export its
/// products are saved and compared with. A linked library prints the symbols
/// issued for its entries, imports its own derivative helpers by theirs, and
/// leaves the one shared memory to the link.
#[derive(Clone, Copy)]
pub(crate) enum Linkage<'a> {
    Browser,
    Linked {
        /// One issued symbol per entry the module exports, in export order.
        entries: &'a [String],
        /// The prefix of the helper symbols a control module imports.
        operations: &'a str,
    },
}

impl<'a> Linkage<'a> {
    /// An export section holding the public memory re-export when the
    /// browser profile has one; entries follow.
    pub(crate) fn exports(self) -> ExportSection {
        let mut exports = ExportSection::new();
        if let Self::Browser = self {
            exports.export("memory", ExportKind::Memory, 0);
        }
        exports
    }

    /// The names of a module's entries, `browser` in the browser profile.
    /// A linked module must have exactly one issued symbol per entry.
    pub(crate) fn names<S: AsRef<str>>(self, browser: &[S]) -> Result<Vec<String>, String> {
        match self {
            Self::Browser => Ok(browser
                .iter()
                .map(|name| name.as_ref().to_owned())
                .collect()),
            Self::Linked { entries, .. } if entries.len() == browser.len() => Ok(entries.to_vec()),
            Self::Linked { entries, .. } => Err(format!(
                "linked module has {} issued symbols for {} entries",
                entries.len(),
                browser.len()
            )),
        }
    }

    /// The import module and field of a derivative helper by its role.
    pub(crate) fn operation(self, role: &str) -> (&'static str, String) {
        match self {
            Self::Browser => ("operation", role.to_owned()),
            Self::Linked { operations, .. } => {
                ("env", crate::linked::helper_symbol(operations, role))
            }
        }
    }

    /// The linkage of the entries `range` of this module's entries.
    pub(crate) fn entries_in(self, range: std::ops::Range<usize>) -> Self {
        match self {
            Self::Browser => Self::Browser,
            Self::Linked {
                entries,
                operations,
            } => Self::Linked {
                entries: entries.get(range).unwrap_or_default(),
                operations,
            },
        }
    }
}
