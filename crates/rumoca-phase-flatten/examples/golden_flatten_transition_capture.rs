//! Coverage-only runner for the `TypedInstancedTree -> flat::Model` transition.
//!
//! This is deliberately a standalone process rather than a libtest case:
//! LLVM's profile counters are process-global, so a waiting test-harness thread
//! would make quiescence false. The predecessor is constructed before reset,
//! and the production transition is called exactly once inside the window.

#[cfg(coverage)]
use std::collections::BTreeSet;
#[cfg(coverage)]
use std::path::Path;

#[cfg(coverage)]
const SOURCE_NAME: &str = "UnitDerivative.mo";
#[cfg(coverage)]
const SOURCE: &str = include_str!("../../rumoca/tests/fixtures/golden/UnitDerivative.mo");

#[cfg(coverage)]
#[allow(unsafe_code)]
mod llvm_profile {
    unsafe extern "C" {
        fn __llvm_profile_dump() -> i32;
        fn __llvm_profile_reset_counters();
    }

    pub(super) fn reset() {
        // SAFETY: the standalone runner has proved that it owns the sole live
        // thread and has no caught signals immediately before this call. No
        // compiler work overlaps the process-global counter mutation.
        unsafe { __llvm_profile_reset_counters() };
    }

    pub(super) fn dump() -> i32 {
        // SAFETY: the measured transition has returned and the runner proves
        // quiescence again before reading the process-global counters.
        unsafe { __llvm_profile_dump() }
    }
}

#[cfg(coverage)]
fn linux_process_quiescence() -> String {
    #[cfg(not(target_os = "linux"))]
    panic!("exact golden transition capture currently requires Linux /proc evidence");

    #[cfg(target_os = "linux")]
    {
        let thread_count = std::fs::read_dir("/proc/self/task")
            .expect("read /proc/self/task for the golden capture quiescence witness")
            .count();
        assert_eq!(
            thread_count, 1,
            "golden transition capture requires exactly one live process thread"
        );

        let status = std::fs::read_to_string("/proc/self/status")
            .expect("read /proc/self/status for the golden capture signal witness");
        for field in ["SigPnd:\t", "ShdPnd:\t"] {
            let pending = status
                .lines()
                .find_map(|line| line.strip_prefix(field))
                .expect("/proc/self/status carries pending-signal state")
                .trim();
            assert!(
                pending.bytes().all(|byte| byte == b'0'),
                "golden transition capture cannot run with pending signals: {field}{pending}"
            );
        }
        let caught = status
            .lines()
            .find_map(|line| line.strip_prefix("SigCgt:\t"))
            .expect("/proc/self/status carries SigCgt")
            .trim()
            .to_owned();
        assert_eq!(
            caught, "0000000000000440",
            "golden transition capture requires only rustc's pinned synchronous SIGBUS/SIGSEGV handlers"
        );
        caught
    }
}

#[cfg(coverage)]
fn linux_executable_images() -> Vec<String> {
    #[cfg(not(target_os = "linux"))]
    panic!("exact golden transition capture currently requires Linux /proc evidence");

    #[cfg(target_os = "linux")]
    {
        let maps = std::fs::read_to_string("/proc/self/maps")
            .expect("read /proc/self/maps for the golden capture image witness");
        let mut images = BTreeSet::new();
        for line in maps.lines() {
            let mut fields = line.split_ascii_whitespace();
            let _address = fields.next().expect("mapping has an address range");
            let permissions = fields.next().expect("mapping has permissions");
            let _offset = fields.next().expect("mapping has an offset");
            let _device = fields.next().expect("mapping has a device");
            let _inode = fields.next().expect("mapping has an inode");
            let pathname = fields.collect::<Vec<_>>().join(" ");
            if !permissions.contains('x') || !pathname.starts_with('/') {
                continue;
            }
            assert!(
                !pathname.ends_with(" (deleted)"),
                "golden transition capture refuses a deleted executable mapping: {pathname}"
            );
            images.insert(pathname);
        }
        assert!(
            !images.is_empty(),
            "golden transition capture found no file-backed executable image"
        );
        images.into_iter().collect()
    }
}

#[cfg(coverage)]
fn typed_unit_derivative() -> rumoca_phase_typecheck::TypedInstancedTree {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME)
        .expect("UnitDerivative parses before the measured transition");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
        .expect("UnitDerivative resolves before the measured transition");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        "UnitDerivative",
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("UnitDerivative unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("UnitDerivative instantiation failed: {error}")
        }
    };
    rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "UnitDerivative")
        .expect("UnitDerivative typechecks before the measured transition")
}

#[cfg(coverage)]
fn checked_profile_path() {
    let value = std::env::var_os("LLVM_PROFILE_FILE")
        .expect("exact golden transition capture requires LLVM_PROFILE_FILE");
    let path = Path::new(&value);
    assert!(path.is_absolute(), "LLVM_PROFILE_FILE must be absolute");
    assert!(
        !value.to_string_lossy().contains('%'),
        "LLVM_PROFILE_FILE substitutions are prohibited for exact capture"
    );
}

#[cfg(coverage)]
#[inline(never)]
fn calibration_before_reset() {
    std::hint::black_box(());
}

#[cfg(coverage)]
#[inline(never)]
fn calibration_inside_window() {
    std::hint::black_box(());
}

#[cfg(coverage)]
#[inline(never)]
fn calibration_after_dump() {
    std::hint::black_box(());
}

#[cfg(coverage)]
fn main() {
    checked_profile_path();
    let typed = typed_unit_derivative();
    let signal_baseline = linux_process_quiescence();
    let executable_images = linux_executable_images();
    calibration_before_reset();

    llvm_profile::reset();
    calibration_inside_window();
    let flat = rumoca_phase_flatten::flatten_typed(
        typed,
        rumoca_phase_flatten::FlattenOptions {
            simplify_variable_names: false,
            materialize_structured_families: false,
        },
    );
    let signal_after_transition = linux_process_quiescence();
    assert_eq!(signal_after_transition, signal_baseline);
    let dump_status = llvm_profile::dump();

    calibration_after_dump();
    assert_eq!(dump_status, 0, "LLVM profile dump failed");
    assert_eq!(
        linux_executable_images(),
        executable_images,
        "executable mappings changed across the measured transition"
    );
    println!(
        "{}",
        serde_json::to_string(&executable_images)
            .expect("serialize golden transition executable image witness")
    );
    let flat = flat.expect("measured UnitDerivative flatten transition failed");
    assert_eq!(flat.variables.len(), 1);
    assert_eq!(flat.equations.len(), 1);
}

#[cfg(not(coverage))]
fn main() {
    eprintln!("run this example only through the exact golden transition coverage command");
    std::process::exit(2);
}
