//! Drift guard for the fixed-count GALEC kernel specializations.
//!
//! The `_N` specializations (SPEC_0034 GAL-037 working set) are spelled out in
//! two independent places, neither of which the compiler ties to the other:
//!
//! * `embedded-c-galec/model.c.jinja` decides which literal counts a call site
//!   is allowed to emit as `..._N(...)` instead of the generic counted kernel;
//! * `embedded-c-galec/kernels.h.jinja` defines the `_N` bodies as
//!   `static inline`, which is both their definition and their declaration, so
//!   the compiler can inline them at each call site.
//!
//! Adding a count to the selection list alone is a link error at the target
//! gate; adding one to the library alone is dead flash in every generated
//! container. Neither shows up in this crate's own tests, so this test parses
//! the two lists straight out of the template sources and holds them equal.
//!
//! It reads the templates from the source tree rather than the build-script
//! bundle on purpose: the bundle is what a mismatch would ship, the sources are
//! where a mismatch is introduced, and only the sources name the selection
//! list at all (it never survives rendering).

use std::fs;
use std::path::PathBuf;

/// The kernel families that have fixed-count specializations, as
/// (`specialized_<selector>_counts` in `model.c.jinja`,
/// `rumoca_galec_<kernel>_real_N` in the kernel library).
const FAMILIES: &[(&str, &str)] = &[
    ("copy", "copy"),
    ("fill", "fill"),
    ("dot", "dot"),
    ("scaled_add", "scaled_add"),
];

fn template_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/templates/embedded-c-galec")
}

fn read_template(name: &str) -> String {
    let path = template_dir().join(name);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

/// Parse `[1, 2, 3]` starting at the `[`, returning the integers in order.
fn parse_int_list(text: &str) -> Vec<u32> {
    let open = text.find('[').expect("count list opens with `[`");
    let close = text[open..].find(']').expect("count list closes with `]`") + open;
    text[open + 1..close]
        .split(',')
        .map(|entry| {
            entry
                .trim()
                .parse::<u32>()
                .unwrap_or_else(|error| panic!("count list entry {entry:?}: {error}"))
        })
        .collect()
}

/// The counts `model.c.jinja` lets a call site emit as a specialization.
fn selected_counts(source: &str, selector: &str) -> Vec<u32> {
    let needle = format!("set specialized_{selector}_counts =");
    let line = source
        .lines()
        .find(|line| line.contains(&needle))
        .unwrap_or_else(|| panic!("model.c.jinja declares `{needle}`"));
    parse_int_list(line)
}

/// The counts `kernels.h.jinja` writes `static inline` bodies for, read off the
/// `{% for %}` that drives each family's definitions.
fn defined_counts(source: &str, kernel: &str) -> Vec<u32> {
    let signature = format!("rumoca_galec_{kernel}_real_{{{{ count }}}}(");
    let lines: Vec<&str> = source.lines().collect();
    let definition = lines
        .iter()
        .position(|line| line.contains(&signature))
        .unwrap_or_else(|| panic!("kernels.h.jinja defines `{signature}`"));
    let driver = lines[..definition]
        .iter()
        .rposition(|line| line.contains("for count in ["))
        .unwrap_or_else(|| panic!("`{signature}` is driven by a `for count in [...]`"));
    parse_int_list(lines[driver])
}

#[test]
fn specialized_kernel_counts_agree_across_the_two_hand_written_lists() {
    let model = read_template("model.c.jinja");
    let kernels_header = read_template("kernels.h.jinja");

    for (selector, kernel) in FAMILIES {
        let mut selected = selected_counts(&model, selector);
        let mut defined = defined_counts(&kernels_header, kernel);
        assert!(
            !selected.is_empty(),
            "{selector}: the selection list parsed empty, so this guard would pass vacuously"
        );
        selected.sort_unstable();
        defined.sort_unstable();

        assert_eq!(
            selected, defined,
            "rumoca_galec_{kernel}_real_N: model.c.jinja selects {selected:?} but the \
             kernel library provides {defined:?}; a selected count with no kernel is a \
             link error in every generated model, an unselected kernel is dead flash \
             in every generated container"
        );
    }
}

/// Every `<target>/<file>.jinja` of the two C-emitting GALEC targets, as
/// (display name, source text), sorted by display name.
fn c_target_templates() -> Vec<(String, String)> {
    let templates = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/templates");
    let mut found = Vec::new();
    for target in ["embedded-c-galec", "galec-production"] {
        let dir = templates.join(target);
        let entries = fs::read_dir(&dir).expect("read target template directory");
        let paths = entries
            .map(|entry| entry.expect("template directory entry").path())
            .filter(|path| {
                path.extension()
                    .is_some_and(|extension| extension == "jinja")
            });
        found.extend(paths.map(|path| {
            let name = format!("{target}/{}", path.file_name().unwrap().to_string_lossy());
            (name, fs::read_to_string(&path).expect("read template"))
        }));
    }
    found.sort();
    found
}

/// The templates whose source contains `needle`, by display name.
fn templates_containing(templates: &[(String, String)], needle: &str) -> Vec<String> {
    templates
        .iter()
        .filter(|(_, source)| source.contains(needle))
        .map(|(name, _)| name.clone())
        .collect()
}

#[test]
fn the_count_lists_have_exactly_one_home_per_role() {
    // `galec-production` renders the same kernels through `{% extends %}`
    // (its `kernels.*.jinja` carry no bodies of their own) and its C model
    // template likewise. Re-declaring a count list there would reintroduce the
    // drift this guard exists to prevent, so no second copy may appear.
    let templates = c_target_templates();
    for (selector, kernel) in FAMILIES {
        let selection = format!("set specialized_{selector}_counts =");
        let definition = format!("rumoca_galec_{kernel}_real_{{{{ count }}}}(");
        assert_eq!(
            templates_containing(&templates, &selection),
            vec!["embedded-c-galec/model.c.jinja".to_string()],
            "specialized_{selector}_counts must be declared in exactly one template"
        );
        assert_eq!(
            templates_containing(&templates, &definition),
            vec!["embedded-c-galec/kernels.h.jinja".to_string()],
            "rumoca_galec_{kernel}_real_N must be defined in exactly one template"
        );
    }
}
