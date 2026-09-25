//! Generated FMI C emits each content-identical pure-call family once.
//!
//! `sq` is called from two residual rows and from inside `wrap`, so the Solve
//! lowering issues one pure-call owner per call site. The rendered C sources
//! (every translation unit and the shared `model.h`) must define one C function per family (primal and directional) and route
//! every call site, including the nested call inside `wrap`, through the
//! family representative's symbol.

use std::collections::{BTreeMap, BTreeSet};

const SOURCE: &str = "
function sq
  input Real u;
  output Real y;
algorithm
  y := 0;
  for i in 1:3 loop
    y := y + u*u/3;
  end for;
end sq;
function wrap
  input Real u;
  output Real y;
algorithm
  y := sq(u) + 1;
end wrap;
model TwoCalls
  Real x(start=1, fixed=true);
  Real a;
  Real b;
  Real c;
equation
  der(x) = -a - b - c;
  a + 0.1*sq(a) = x;
  b + 0.1*sq(b) = 2*x;
  c = wrap(x);
end TwoCalls;";

/// Every rendered C translation unit and the shared header, concatenated.
fn c_sources(target: &str) -> String {
    let compiled = match rumoca::Compiler::new()
        .model("TwoCalls")
        .compile_str(SOURCE, "TwoCalls.mo")
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile TwoCalls: {error:#}"),
    };
    let files = match rumoca::render_target_files(&compiled, "TwoCalls", target, None) {
        Ok(files) => files,
        Err(error) => panic!("render TwoCalls {target}: {error:#}"),
    };
    assert!(
        files.iter().any(|file| file.path == "sources/model.c"),
        "TwoCalls {target} emits sources/model.c"
    );
    files
        .into_iter()
        .filter(|file| {
            file.path.starts_with("sources/")
                && (file.path.ends_with(".c") || file.path.ends_with(".h"))
        })
        .map(|file| file.content)
        .collect::<Vec<_>>()
        .join("\n")
}

/// Owner ids named by `<prefix><id>(` occurrences in `text`.
fn symbols(text: &str, prefix: &str) -> Vec<u32> {
    text.match_indices(prefix)
        .filter_map(|(start, _)| {
            let rest = &text[start + prefix.len()..];
            let digits = rest.find(|c: char| !c.is_ascii_digit())?;
            (digits > 0 && rest[digits..].starts_with('('))
                .then(|| rest[..digits].parse().ok())
                .flatten()
        })
        .collect()
}

/// Each defined `static int <prefix><id>(...) {` body, keyed by owner id.
fn definitions(source: &str, prefix: &str) -> BTreeMap<u32, String> {
    let opener = format!("static int {prefix}");
    let mut bodies = BTreeMap::new();
    for (start, _) in source.match_indices(&opener) {
        let rest = &source[start + opener.len()..];
        let Some(digits) = rest.find(|c: char| !c.is_ascii_digit()) else {
            continue;
        };
        let Some(header_end) = rest.find(')') else {
            continue;
        };
        if digits == 0 || !rest[digits..].starts_with('(') || !rest[header_end..].starts_with(") {")
        {
            continue;
        }
        let id: u32 = rest[..digits].parse().expect("owner id is numeric");
        let body_end = rest.find("\n}").expect("definition body terminates");
        let previous = bodies.insert(id, rest[header_end..body_end].to_string());
        assert!(previous.is_none(), "`{prefix}{id}` is defined once");
    }
    bodies
}

fn assert_families(target: &str, source: &str, directional: &str) {
    let pure = format!("rumoca_pure_{directional}");
    let scalar = format!("rumoca_scalar_pure_{directional}");
    let defined = definitions(source, &pure);
    assert_eq!(
        defined.len(),
        2,
        "{target}: `sq` and `wrap` are the only {pure} families: {:?}",
        defined.keys()
    );
    let referenced = symbols(source, &pure)
        .into_iter()
        .chain(symbols(source, &scalar))
        .collect::<BTreeSet<_>>();
    assert!(
        referenced.iter().all(|id| defined.contains_key(id)),
        "{target}: every {pure} call site names an emitted representative \
         (referenced {referenced:?}, defined {:?})",
        defined.keys()
    );
    let nested = defined
        .iter()
        .filter_map(|(&id, body)| {
            let callees = symbols(body, &pure)
                .into_iter()
                .filter(|callee| *callee != id)
                .collect::<BTreeSet<_>>();
            (!callees.is_empty()).then_some((id, callees))
        })
        .collect::<Vec<_>>();
    let [(wrap, callees)] = nested.as_slice() else {
        panic!("{target}: only `wrap` calls another family: {nested:?}");
    };
    let sq = *defined
        .keys()
        .find(|id| *id != wrap)
        .expect("the `sq` family is emitted");
    assert_eq!(
        callees,
        &BTreeSet::from([sq]),
        "{target}: the nested call inside `wrap` routes to the `sq` representative"
    );
}

#[test]
fn identical_pure_call_owners_render_one_routed_family() {
    for target in ["fmi3", "fmi2"] {
        let source = c_sources(target);
        assert_families(target, &source, "");
        assert_families(target, &source, "directional_");
        // Call sites only: every program calls a wrapper as `if (wrapper(...))`.
        let sq_sites = symbols(&source, "if (rumoca_scalar_pure_").len();
        assert!(
            sq_sites >= 3,
            "{target}: every scalar-program call site is emitted ({sq_sites})"
        );
    }
}
