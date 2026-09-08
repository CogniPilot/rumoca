// Cross-crate transport probe.
//
// Two questions, tested independently because they can fail independently:
//
//   1. KEY STABILITY. Is the `DefPathHash` a producer crate computes for one of
//      its items the same value a consumer crate computes for that same item
//      through metadata? This is the property a sidecar keyed by `DefPathHash`
//      rests on, and nothing else in the design works if it fails.
//
//   2. CLOSURE VISIBILITY. Can the consumer observe a FOREIGN closure's `DefId`
//      at all? A closure behind `impl Trait` may be opaque across the crate
//      boundary, in which case the consumer cannot key anything to it even
//      though the key mechanism itself is sound.
//
// This probe collects NO borrowck facts. It runs in `after_analysis` and reads
// only identity and content queries: `def_path_hash`, `crate_hash` and
// `used_crate_source`. Those are safe there because they do not touch bodies.
//
// A real collector cannot work this way. `consumers.rs:115-120` says
// `get_bodies_with_borrowck_facts` panics once bodies have been stolen and names
// the `mir_borrowck` query override as the way to avoid it, so fact collection
// belongs in that override rather than in this callback. Nothing here implements
// that, and nothing here should be read as evidence about it.
//
// Usage: transport-probe <produce|consume> <sidecar-path> -- <rustc args>

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;

use rustc_hir::def_id::{DefId, LOCAL_CRATE};
use rustc_middle::ty::{self, TyCtxt};
use std::collections::BTreeMap;
use std::io::Write;

fn sidecar_path() -> String {
    std::env::var("TRANSPORT_PROBE_SIDECAR").expect("TRANSPORT_PROBE_SIDECAR must be set")
}

fn producing() -> bool {
    std::env::var("TRANSPORT_PROBE_MODE").as_deref() == Ok("produce")
}

/// `DefPathHash` rendered as its two stable halves, which is what a sidecar
/// would actually store.
fn key(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    let hash = tcx.def_path_hash(def_id);
    format!(
        "{:016x}{:016x}",
        hash.stable_crate_id().as_u64(),
        hash.local_hash().as_u64()
    )
}

struct Probe;

impl rustc_driver::Callbacks for Probe {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> rustc_driver::Compilation {
        if producing() {
            produce(tcx);
        } else {
            consume(tcx);
        }
        rustc_driver::Compilation::Continue
    }
}

/// Record every local item's key, so the consumer can look up any of them.
fn produce(tcx: TyCtxt<'_>) {
    let mut out = String::new();
    let mut closures = 0usize;
    // `definitions()` does not include closures or opaques; body owners do.
    let mut locals: Vec<_> = tcx.hir_crate_items(()).definitions().collect();
    for owner in tcx.hir_body_owners() {
        if !locals.contains(&owner) {
            locals.push(owner);
        }
    }
    for local in locals {
        let def_id = local.to_def_id();
        let kind = tcx.def_kind(def_id);
        if matches!(kind, rustc_hir::def::DefKind::Closure) {
            closures += 1;
        }
        out.push_str(&format!(
            "{} {:?} {}\n",
            key(tcx, def_id),
            kind,
            tcx.def_path_str(def_id)
        ));
    }
    // Content binding: the key identifies the ITEM, the crate hash identifies the
    // exact source it came from. Same key with changed source must differ here.
    out.push_str(&format!("CRATE_HASH {}\n", tcx.crate_hash(LOCAL_CRATE)));
    let path = sidecar_path();
    let mut file = std::fs::File::create(&path).expect("sidecar is writable");
    file.write_all(out.as_bytes()).expect("sidecar is writable");
    println!(
        "PRODUCED {} entries ({} closures) to {path}",
        out.lines().count(),
        closures
    );
}

/// Look every foreign item we can see up in the sidecar, and separately report
/// whether any FOREIGN closure type is observable at all.
fn consume(tcx: TyCtxt<'_>) {
    let sidecar: BTreeMap<String, String> = std::fs::read_to_string(sidecar_path())
        .expect("sidecar is readable")
        .lines()
        .filter_map(|line| {
            line.split_once(' ')
                .map(|(k, v)| (k.to_owned(), v.to_owned()))
        })
        .collect();
    println!("SIDECAR entries {}", sidecar.len());

    let mut matched = 0usize;
    let mut missing = 0usize;
    let mut foreign_closures = 0usize;

    // Every foreign item mentioned by this crate's own item signatures.
    for local in tcx.hir_crate_items(()).definitions() {
        let def_id = local.to_def_id();
        if !matches!(
            tcx.def_kind(def_id),
            rustc_hir::def::DefKind::Fn | rustc_hir::def::DefKind::AssocFn
        ) {
            continue;
        }
        let sig = tcx.fn_sig(def_id).instantiate_identity();
        let mut seen: Vec<DefId> = Vec::new();
        collect_foreign_defs(tcx, sig.skip_binder(), &mut seen, &mut foreign_closures);
        for foreign in seen {
            let k = key(tcx, foreign);
            if let Some(name) = sidecar.get(&k) {
                matched += 1;
                println!("  MATCH {k} {} == {name}", tcx.def_path_str(foreign));
            } else {
                missing += 1;
                println!("  MISSING {k} {}", tcx.def_path_str(foreign));
            }
        }
    }
    // Content binding on the consumer side: which artifact did we actually bind
    // to, and does its recorded content hash match the sidecar's?
    for &cnum in tcx.crates(()) {
        let name = tcx.crate_name(cnum);
        if name.as_str() != "transport_dep" {
            continue;
        }
        let observed = format!("{}", tcx.crate_hash(cnum));
        let recorded = sidecar
            .iter()
            .find(|(k, _)| k.as_str() == "CRATE_HASH")
            .map(|(_, v)| v.clone());
        println!("  CRATE_HASH observed={observed} recorded={recorded:?}");
        println!(
            "  CRATE_HASH_AGREES {}",
            recorded.as_deref() == Some(observed.as_str())
        );
        if let Some(path) = &tcx.used_crate_source(cnum).rlib {
            println!("  BOUND_ARTIFACT rlib {}", path.display());
            match file_digest(path) {
                Ok(digest) => println!("  BOUND_ARTIFACT_DIGEST {digest}"),
                Err(error) => {
                    println!("  BOUND_ARTIFACT_DIGEST_UNREADABLE {error}");
                }
            }
        }
        if let Some(path) = &tcx.used_crate_source(cnum).rmeta {
            println!("  BOUND_ARTIFACT rmeta {}", path.display());
        }
    }
    println!("KEY_STABILITY matched={matched} missing={missing}");
    println!("FOREIGN_CLOSURES_OBSERVED {foreign_closures}");
}

fn collect_foreign_defs<'tcx>(
    tcx: TyCtxt<'tcx>,
    sig: ty::FnSig<'tcx>,
    out: &mut Vec<DefId>,
    foreign_closures: &mut usize,
) {
    for ty in sig.inputs_and_output {
        walk(tcx, ty, out, foreign_closures);
    }
}

fn walk<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: ty::Ty<'tcx>,
    out: &mut Vec<DefId>,
    foreign_closures: &mut usize,
) {
    let record = |did: DefId, out: &mut Vec<DefId>| {
        if did.krate != LOCAL_CRATE && !out.contains(&did) {
            out.push(did);
        }
    };
    match ty.kind() {
        ty::Adt(def, args) => {
            record(def.did(), out);
            for arg in args.types() {
                walk(tcx, arg, out, foreign_closures);
            }
        }
        ty::Closure(did, args) => {
            if did.krate != LOCAL_CRATE {
                *foreign_closures += 1;
            }
            record(*did, out);
            for arg in args.types() {
                walk(tcx, arg, out, foreign_closures);
            }
        }
        ty::Alias(_, alias) => {
            if let ty::AliasTyKind::Opaque { def_id } = alias.kind {
                record(def_id, out);
                // The hidden type is what carries a foreign closure, if it is
                // reachable at all from this side of the boundary.
                if let Some(hidden) = opaque_hidden(tcx, def_id) {
                    walk(tcx, hidden, out, foreign_closures);
                }
            }
            for arg in alias.args.types() {
                walk(tcx, arg, out, foreign_closures);
            }
        }
        ty::Ref(_, inner, _) => walk(tcx, *inner, out, foreign_closures),
        _ => {}
    }
}

/// A non-cryptographic identity digest of the artifact the consumer actually
/// resolved. It binds a sidecar to one FILE, which `crate_hash` does not: two
/// files can carry the same crate hash from separate builds of one source. Not a
/// security property and not intended as one.
fn file_digest(path: &std::path::Path) -> Result<String, std::io::Error> {
    // Fail closed. An unreadable artifact must not be reported as a digest,
    // least of all as the digest of an empty file, which a genuinely empty
    // artifact would legitimately produce.
    let bytes = std::fs::read(path)?;
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in &bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    Ok(format!("{hash:016x}:{}", bytes.len()))
}

/// Exercises the refusal and the genuine-empty case, which must be
/// distinguishable. Run with TRANSPORT_PROBE_MODE=digest-controls.
fn digest_controls() {
    // A fresh directory per run, created exclusively, so the controls never
    // observe a file some earlier run left behind.
    let dir = std::env::temp_dir().join(format!(
        "transport-probe-digest-controls-{}",
        std::process::id()
    ));
    std::fs::create_dir(&dir).expect("controls directory must not already exist");
    let empty = dir.join("empty.bin");
    std::fs::File::create_new(&empty).expect("empty control is fresh");
    let missing = dir.join("does-not-exist.bin");

    let empty_digest = file_digest(&empty).expect("a genuinely empty file is readable");
    println!("DIGEST_EMPTY_FILE {empty_digest}");
    assert!(
        empty_digest.ends_with(":0"),
        "empty file reports zero length"
    );

    match file_digest(&missing) {
        Ok(value) => panic!("unreadable artifact produced a digest: {value}"),
        Err(error) => println!("DIGEST_UNREADABLE_REFUSED {}", error.kind() as u8),
    }
    // Remove only the two known children and the directory this run created
    // exclusively. No recursive delete of a path we did not make.
    std::fs::remove_file(&empty).expect("empty control is removable");
    std::fs::remove_dir(&dir).expect("controls directory is removable");
    println!("DIGEST_CONTROLS_OK");
}

/// Try to reveal an opaque's hidden type. Returns `None` when the boundary
/// hides it, which is itself the measurement question 2 asks.
fn opaque_hidden<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> Option<ty::Ty<'tcx>> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        tcx.type_of(def_id).instantiate_identity().skip_norm_wip()
    }))
    .ok()
}

fn main() {
    if std::env::var("TRANSPORT_PROBE_MODE").as_deref() == Ok("digest-controls") {
        digest_controls();
        return;
    }
    let args: Vec<String> = std::env::args().collect();
    rustc_driver::catch_fatal_errors(|| rustc_driver::run_compiler(&args, &mut Probe))
        .expect("probe compilation");
}
