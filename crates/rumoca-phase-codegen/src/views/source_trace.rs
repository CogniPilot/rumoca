//! Human-usable source anchors for generated-code traceability (GAL-032).
//!
//! A generated-code trace exists so a reviewer can put the emitted statement
//! next to the Modelica statement it came from. A `SourceId` cannot do that: it
//! is an FNV hash of the source *name*, so the only way to read it is to have
//! the same compiler recompute the same hash. Byte offsets have the same
//! problem in a smaller way — nothing a reviewer runs (an editor, `grep -n`, a
//! diff, a review tool, a DO-178C trace matrix) is addressed by a byte offset.
//!
//! This module converts the checked span into the anchor those tools *do*
//! speak: the source path plus a 1-based line and column. The exact byte range
//! is kept as a secondary field because it is the anchor the compiler actually
//! decided on, and dropping it would remove the only machine-checkable link
//! back to the span. The source id stays available for the file-level legend so
//! nothing that already keys on the hash loses its key.
//!
//! Line/column are computed against the same rule as
//! [`rumoca_core::text_position::byte_offset_to_position`] — `\n` starts a
//! line, columns count UTF-16 code units — and the tests below pin that
//! equivalence. The offsets are cached per source so the conversion is one
//! binary search per trace rather than a scan of the file.

use std::cell::RefCell;
use std::collections::BTreeMap;

use rumoca_core::{SourceId, SourceMap, Span};
use serde::Serialize;

/// A statement anchor as a reviewer addresses it.
///
/// `path`/`line`/`column` are `None` exactly when the span's source is not
/// resolvable to text in the session's [`SourceMap`] (a compiler-synthesized
/// source, or a view built without a source map). Templates must therefore
/// keep a hash-only fallback rather than assume the path is present.
#[derive(Debug, Clone, Serialize)]
pub(super) struct SourceTrace {
    /// Source path as the compiler was given it, or `None` when unresolvable.
    path: Option<String>,
    /// 1-based line of the first byte.
    line: Option<u32>,
    /// 1-based UTF-16 column of the first byte.
    column: Option<u32>,
    /// 1-based line of the last byte (inclusive end of the statement text).
    end_line: Option<u32>,
    /// 1-based UTF-16 column of the last byte.
    end_column: Option<u32>,
    /// Stable source identity, retained for the file legend and for any
    /// consumer that keys on the hash.
    source_id: u64,
    /// Exact `[start, end)` byte range the compiler anchored the statement to.
    byte_start: usize,
    /// Exclusive byte end of that range.
    byte_end: usize,
}

/// One Modelica file this translation unit traces into.
#[derive(Debug, Clone, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct SourceFile {
    /// Path relative to [`TraceLegend::root`], or absolute when there is none.
    pub(super) path: String,
}

/// The file-level trace legend a target emits once per translation unit.
///
/// `root` exists for byte reproducibility, which is a configuration-management
/// property and not a cosmetic one: a compiler is handed absolute paths, so
/// printing them verbatim would make the generated C — and therefore the SHA-1
/// the eFMU manifest records for it — depend on where the source tree happens
/// to live. The root is the longest directory prefix shared by every traced
/// file; stripping it leaves paths that are identical on every checkout, and
/// the root itself is deliberately NOT emitted into the C.
///
/// When the traced files share no directory prefix (sources from unrelated
/// trees, e.g. a library outside the project) `root` is `None` and paths stay
/// absolute. That is an honest degradation rather than a silent one: the
/// emitted legend says which case applies.
#[derive(Debug, Clone, Serialize)]
pub(super) struct TraceLegend {
    root: Option<String>,
    files: Vec<SourceFile>,
}

/// Byte-offset → line/column conversion for one source, plus its path.
///
/// The text is borrowed from the session's [`SourceMap`], which outlives every
/// view built from it; copying a whole model's Modelica sources to answer line
/// numbers would be pure waste.
struct SourceLines<'a> {
    path: &'a str,
    text: &'a str,
    /// Byte offset of the first character of each line; always starts with 0.
    line_starts: Vec<usize>,
}

impl<'a> SourceLines<'a> {
    fn new(path: &'a str, text: &'a str) -> Self {
        let mut line_starts = vec![0usize];
        for (offset, byte) in text.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset + 1);
            }
        }
        Self {
            path,
            text,
            line_starts,
        }
    }

    /// 1-based line and 1-based UTF-16 column of `offset`.
    fn position(&self, offset: usize) -> (u32, u32) {
        let clamped = offset.min(self.text.len());
        // `partition_point` yields the count of line starts at or before
        // `clamped`, which is the 1-based line number.
        let line = self.line_starts.partition_point(|start| *start <= clamped);
        let line_start = self.line_starts[line.saturating_sub(1)];
        let column_units: u32 = self.text[line_start..clamped]
            .chars()
            .map(|character| character.len_utf16() as u32)
            .sum();
        (
            u32::try_from(line).unwrap_or(u32::MAX),
            column_units.saturating_add(1),
        )
    }
}

/// Resolves checked spans into reviewer-addressable anchors.
///
/// Construction is cheap; the per-source line index is built on first use and
/// the set of sources actually reached is recorded so the emitted file can
/// carry a legend of exactly the files it traces into (and no others — a model
/// pulls in far more library files than it traces statements from).
pub(super) struct SourceTraceResolver<'a> {
    sources: &'a SourceMap,
    lines: RefCell<BTreeMap<u64, Option<SourceLines<'a>>>>,
}

impl<'a> SourceTraceResolver<'a> {
    pub(super) fn new(sources: &'a SourceMap) -> Self {
        Self {
            sources,
            lines: RefCell::new(BTreeMap::new()),
        }
    }

    /// The anchor for `span`, or `None` when the span is the dummy span.
    pub(super) fn trace(&self, span: &Span) -> Option<SourceTrace> {
        if span.is_dummy() {
            return None;
        }
        let source_id = span.source.0;
        let byte_start = span.start.0;
        let byte_end = span.end.0;
        self.ensure_loaded(span.source);
        let cache = self.lines.borrow();
        let Some(Some(lines)) = cache.get(&source_id) else {
            return Some(SourceTrace {
                path: None,
                line: None,
                column: None,
                end_line: None,
                end_column: None,
                source_id,
                byte_start,
                byte_end,
            });
        };
        let (line, column) = lines.position(byte_start);
        // The end position names the last byte *inside* the statement, so a
        // one-line statement reports `line:col-line:endcol` rather than
        // pointing one past its own text.
        let (end_line, end_column) = lines.position(byte_end.max(byte_start.saturating_add(1)) - 1);
        Some(SourceTrace {
            path: Some(lines.path.to_owned()),
            line: Some(line),
            column: Some(column),
            end_line: Some(end_line),
            end_column: Some(end_column),
            source_id,
            byte_start,
            byte_end,
        })
    }

    /// The file legend for every source this resolver resolved a path for.
    ///
    /// Sorting by path (not by hash or first-use order) keeps the emitted
    /// legend byte-stable across runs, which the eFMU checksum web requires.
    pub(super) fn legend(&self) -> TraceLegend {
        let cache = self.lines.borrow();
        let mut paths: Vec<&str> = cache
            .values()
            .filter_map(|lines| lines.as_ref().map(|lines| lines.path))
            .collect();
        paths.sort_unstable();
        let root = common_directory_prefix(&paths);
        let files = paths
            .into_iter()
            .map(|path| SourceFile {
                path: relative_to(path, root.as_deref()).to_owned(),
            })
            .collect();
        TraceLegend { root, files }
    }

    fn ensure_loaded(&self, source: SourceId) {
        let mut cache = self.lines.borrow_mut();
        if cache.contains_key(&source.0) {
            return;
        }
        // A `SourceMap` entry whose name is the `<source-id:…>` placeholder has
        // no path to report, so it is cached as unresolvable rather than
        // emitting a synthetic path a reviewer cannot open.
        let entry = self
            .sources
            .get_source(source)
            .filter(|(name, _)| rumoca_core::source_id_for_name(name) == source)
            .filter(|(name, _)| !name.starts_with('<'))
            .map(|(name, text)| SourceLines::new(name, text));
        cache.insert(source.0, entry);
    }
}

/// The longest directory prefix shared by every path, without its trailing
/// separator, or `None` when there is no shared directory to subtract.
///
/// The comparison is per path *component*, never per character: two siblings
/// `.../Estimation.mo` and `.../EstimationHelpers.mo` share the characters
/// `.../Estimation` but not a directory, and stripping that would produce
/// paths that no longer name a file.
fn common_directory_prefix(paths: &[&str]) -> Option<String> {
    let first = paths.first()?;
    let mut shared: Vec<&str> = directory_components(first).collect();
    for path in &paths[1..] {
        let candidate = directory_components(path);
        let kept = shared
            .iter()
            .zip(candidate)
            .take_while(|(left, right)| *left == right)
            .count();
        shared.truncate(kept);
        if shared.is_empty() {
            return None;
        }
    }
    // An absolute path's leading `/` is an empty first component. If that is
    // all the paths share, the only common directory is the filesystem root
    // and there is nothing to subtract: answering `Some("")` would strip the
    // leading slash and turn absolute paths into ones that look relative but
    // resolve nowhere.
    let joined = shared.join("/");
    (!joined.is_empty()).then_some(joined)
}

fn directory_components(path: &str) -> impl Iterator<Item = &str> {
    let directory = match path.rfind('/') {
        Some(index) => &path[..index],
        None => "",
    };
    directory.split('/')
}

/// `path` with `root` and its separator removed, when `root` is a prefix.
fn relative_to<'a>(path: &'a str, root: Option<&str>) -> &'a str {
    let Some(root) = root else {
        return path;
    };
    path.strip_prefix(root)
        .and_then(|rest| rest.strip_prefix('/'))
        .unwrap_or(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::BytePos;
    use rumoca_core::text_position::byte_offset_to_position;

    fn source_map_with(name: &str, text: &str) -> SourceMap {
        let mut map = SourceMap::new();
        map.add(name, text);
        map
    }

    #[test]
    fn resolves_a_span_to_a_one_based_path_line_and_column() {
        let text = "block A\n  x := 1;\n  y := 2;\n";
        let sources = source_map_with("models/A.mo", text);
        let id = sources.get_id("models/A.mo").expect("registered");
        let resolver = SourceTraceResolver::new(&sources);
        let start = text.find("y := 2").expect("statement present");
        let span = Span {
            source: id,
            start: BytePos(start),
            end: BytePos(start + "y := 2".len()),
        };
        let trace = resolver.trace(&span).expect("non-dummy span");
        assert_eq!(trace.path.as_deref(), Some("models/A.mo"));
        assert_eq!(trace.line, Some(3));
        assert_eq!(trace.column, Some(3));
        assert_eq!(trace.end_line, Some(3));
        assert_eq!(trace.end_column, Some(8));
        assert_eq!(trace.byte_start, start);
    }

    #[test]
    fn line_and_column_agree_with_the_shared_text_position_rule() {
        let text = "block A\n  é := 𝔸;\n  z := 1;\nend A;\n";
        let sources = source_map_with("A.mo", text);
        let id = sources.get_id("A.mo").expect("registered");
        let resolver = SourceTraceResolver::new(&sources);
        for (offset, _) in text.char_indices() {
            let span = Span {
                source: id,
                start: BytePos(offset),
                end: BytePos(offset + 1),
            };
            let trace = resolver.trace(&span).expect("non-dummy span");
            let expected = byte_offset_to_position(text, offset);
            assert_eq!(
                (trace.line, trace.column),
                (
                    Some(expected.line.saturating_add(1)),
                    Some(expected.character.saturating_add(1))
                ),
                "byte {offset} disagreed with byte_offset_to_position"
            );
        }
    }

    #[test]
    fn an_unregistered_source_keeps_the_hash_only_anchor() {
        let sources = SourceMap::new();
        let resolver = SourceTraceResolver::new(&sources);
        let span = Span {
            source: SourceId::from_source_name("gone.mo"),
            start: BytePos(4),
            end: BytePos(9),
        };
        let trace = resolver.trace(&span).expect("non-dummy span");
        assert_eq!(trace.path, None);
        assert_eq!(trace.line, None);
        assert_eq!(trace.byte_start, 4);
        assert_eq!(trace.byte_end, 9);
        assert!(resolver.legend().files.is_empty());
        assert_eq!(resolver.legend().root, None);
    }

    #[test]
    fn the_dummy_span_has_no_anchor_at_all() {
        let sources = SourceMap::new();
        let resolver = SourceTraceResolver::new(&sources);
        assert!(resolver.trace(&Span::DUMMY).is_none());
    }

    fn legend_for(names: &[&str], sources: &SourceMap) -> TraceLegend {
        let resolver = SourceTraceResolver::new(sources);
        for name in names {
            let id = sources.get_id(name).expect("registered");
            resolver
                .trace(&Span {
                    source: id,
                    start: BytePos(0),
                    end: BytePos(1),
                })
                .expect("non-dummy span");
        }
        resolver.legend()
    }

    #[test]
    fn the_legend_lists_only_reached_sources_sorted_by_path() {
        let mut sources = SourceMap::new();
        sources.add("/tree/z/Late.mo", "block Z\nend Z;\n");
        sources.add("/tree/a/Early.mo", "block A\nend A;\n");
        sources.add("/tree/m/Untouched.mo", "block M\nend M;\n");
        let legend = legend_for(&["/tree/z/Late.mo", "/tree/a/Early.mo"], &sources);
        assert_eq!(legend.root.as_deref(), Some("/tree"));
        assert_eq!(
            legend
                .files
                .iter()
                .map(|file| file.path.as_str())
                .collect::<Vec<_>>(),
            vec!["a/Early.mo", "z/Late.mo"]
        );
    }

    /// The emitted paths must not depend on where the tree is checked out —
    /// otherwise the generated C, and the SHA-1 the eFMU manifest records for
    /// it, differ between two machines compiling the same sources.
    #[test]
    fn relative_paths_do_not_move_when_the_tree_does() {
        let relative = |prefix: &str| {
            let mut sources = SourceMap::new();
            let names = [
                format!("{prefix}/Estimation/normalize.mo"),
                format!("{prefix}/LieGroups/exp.mo"),
            ];
            for name in &names {
                sources.add(name, "block B\nend B;\n");
            }
            let borrowed: Vec<&str> = names.iter().map(String::as_str).collect();
            legend_for(&borrowed, &sources)
                .files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>()
        };
        assert_eq!(
            relative("/home/alice/git/models"),
            relative("/builds/ci/checkout/models")
        );
        assert_eq!(
            relative("/home/alice/git/models"),
            vec![
                "Estimation/normalize.mo".to_owned(),
                "LieGroups/exp.mo".to_owned()
            ]
        );
    }

    /// Sources from unrelated trees share no directory, so nothing is
    /// subtracted and the paths stay absolute rather than becoming wrong.
    #[test]
    fn unrelated_trees_keep_absolute_paths() {
        let mut sources = SourceMap::new();
        sources.add("/opt/msl/Modelica/package.mo", "block M\nend M;\n");
        sources.add("/home/models/Estimation.mo", "block E\nend E;\n");
        let legend = legend_for(
            &["/opt/msl/Modelica/package.mo", "/home/models/Estimation.mo"],
            &sources,
        );
        assert_eq!(legend.root, None);
        assert_eq!(
            legend
                .files
                .iter()
                .map(|file| file.path.as_str())
                .collect::<Vec<_>>(),
            vec!["/home/models/Estimation.mo", "/opt/msl/Modelica/package.mo"]
        );
    }

    /// A shared character prefix is not a shared directory.
    #[test]
    fn the_root_is_a_directory_not_a_character_prefix() {
        assert_eq!(
            common_directory_prefix(&["/t/Estimation.mo", "/t/EstimationHelpers.mo"]),
            Some("/t".to_owned())
        );
        assert_eq!(
            common_directory_prefix(&["/ab/x.mo", "/abc/y.mo"]),
            None,
            "only the filesystem root is shared, so nothing may be subtracted"
        );
    }
}
