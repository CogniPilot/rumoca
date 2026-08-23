//! The ARM cross toolchain the flight budgets are measured with.
//!
//! # Why the toolchain is named, never searched for
//!
//! A text-size ceiling is a statement about one compiler. The same sources
//! built by a different GCC, or the same GCC with a different multilib, land
//! kilobytes apart, so a gate that picked up whatever `arm-none-eabi-gcc`
//! happened to be on `PATH` would compare this run's bytes against a ceiling
//! measured somewhere else and call the difference a regression. The root
//! therefore arrives on argv, is authoritative once given, and is never
//! replaced by a fallback: an unusable root is a refusal, not a reason to
//! measure something else.

use anyhow::{Result, bail};
use std::path::{Path, PathBuf};

/// Executables the gate drives. `gcc` cross-compiles, `size` reads `.text`,
/// `nm` reads the undefined-symbol set and the state probe's size, and
/// `objdump` disassembles for the floating-point instruction count.
pub(crate) const REQUIRED_TOOLS: [&str; 4] = [
    "arm-none-eabi-gcc",
    "arm-none-eabi-size",
    "arm-none-eabi-nm",
    "arm-none-eabi-objdump",
];

/// The exact cross-compile the measured baseline was taken with, minus the
/// input files. Every emitted translation unit is built with this and nothing
/// else: a flag added here silently rebases every ceiling in the manifest.
pub(crate) const CROSS_COMPILE_FLAGS: [&str; 7] = [
    "-Os",
    "-std=c99",
    "-mcpu=cortex-m7",
    "-mfpu=fpv5-d16",
    "-mfloat-abi=hard",
    "-ffunction-sections",
    "-c",
];

/// Fixed headline for a run that could not reach the cross toolchain.
/// Operators and CI summaries grep for this exact text, so it is spelled once.
pub(crate) const TOOLCHAIN_UNUSABLE_HEADLINE: &str =
    "embedded budget unmeasured: the ARM cross toolchain is not usable";

/// The tools [`REQUIRED_TOOLS`] names that `root/bin` does not carry.
pub(crate) fn missing_tools(root: &Path) -> Vec<&'static str> {
    let bin = root.join("bin");
    REQUIRED_TOOLS
        .into_iter()
        .filter(|tool| !bin.join(tool).is_file())
        .collect()
}

/// The refusal text for a root that cannot run the gate.
pub(crate) fn unusable_report(root: &Path, missing: &[&str]) -> String {
    format!(
        "{TOOLCHAIN_UNUSABLE_HEADLINE}\n  --arm-toolchain {}\n  expected every tool under {}\n  \
         missing: {}\n  Pass the root of a gcc-arm-embedded installation: the directory whose \
         `bin/` holds {}. This is a hard failure rather than a skip, because a green run that \
         cross-compiled nothing would report the flight artifacts as within budget.",
        root.display(),
        root.join("bin").display(),
        missing.join(", "),
        REQUIRED_TOOLS.join(", "),
    )
}

/// A toolchain root already proven to carry every tool the gate runs.
#[derive(Debug, Clone)]
pub(crate) struct ArmToolchain {
    root: PathBuf,
}

impl ArmToolchain {
    /// Accept `root` only when `root/bin` holds all of [`REQUIRED_TOOLS`].
    ///
    /// The check is by name rather than by version: pinning a version string
    /// here would make the gate refuse a toolchain the operator deliberately
    /// chose, and the manifest's recorded measurement already names the
    /// compiler each ceiling came from.
    pub(crate) fn resolve(root: &Path) -> Result<Self> {
        let missing = missing_tools(root);
        if !missing.is_empty() {
            bail!("{}", unusable_report(root, &missing));
        }
        Ok(Self {
            root: root.to_path_buf(),
        })
    }

    pub(crate) fn root(&self) -> &Path {
        &self.root
    }

    pub(crate) fn gcc(&self) -> PathBuf {
        self.tool("arm-none-eabi-gcc")
    }

    pub(crate) fn size(&self) -> PathBuf {
        self.tool("arm-none-eabi-size")
    }

    pub(crate) fn nm(&self) -> PathBuf {
        self.tool("arm-none-eabi-nm")
    }

    pub(crate) fn objdump(&self) -> PathBuf {
        self.tool("arm-none-eabi-objdump")
    }

    fn tool(&self, name: &str) -> PathBuf {
        self.root.join("bin").join(name)
    }
}
