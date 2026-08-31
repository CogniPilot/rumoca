//! Role-indexed paths used by the authenticated benchmark pipeline.

use anyhow::{Context, Result, ensure};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::Read;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

mod sealed {
    pub(in crate::verify_cmd::embedded_head_to_head) trait Sealed {}
}

pub(super) trait Role: sealed::Sealed + 'static {
    const ID: &'static str;
    const SUFFIX: &'static str;
}

pub(super) trait ExecutableRole: Role {}

pub(super) struct AuthenticatedExecutable<R: ExecutableRole> {
    invocation: RolePath<R>,
    canonical_target: PathBuf,
    sha256: String,
}

impl<R: ExecutableRole> Clone for AuthenticatedExecutable<R> {
    fn clone(&self) -> Self {
        Self {
            invocation: self.invocation.clone(),
            canonical_target: self.canonical_target.clone(),
            sha256: self.sha256.clone(),
        }
    }
}

impl<R: ExecutableRole> AuthenticatedExecutable<R> {
    pub(super) fn canonical_target(&self) -> &Path {
        &self.canonical_target
    }

    pub(super) fn checked(invocation: RolePath<R>, expected_sha256: &str) -> Result<Self> {
        ensure!(
            invocation.as_path().is_absolute(),
            "executable role {} must use an absolute invocation path",
            R::ID
        );
        let canonical_target = invocation.as_path().canonicalize().with_context(|| {
            format!(
                "failed to resolve executable role {} at {}",
                R::ID,
                invocation.as_path().display()
            )
        })?;
        ensure!(
            canonical_target.is_file(),
            "resolved executable is not a file"
        );
        let authenticated = Self {
            invocation,
            canonical_target,
            sha256: expected_sha256.to_owned(),
        };
        authenticated.for_execution()?;
        Ok(authenticated)
    }

    pub(super) fn for_execution(&self) -> Result<&RolePath<R>> {
        let canonical = self.invocation.as_path().canonicalize().with_context(|| {
            format!(
                "failed to resolve executable role {} at use boundary",
                R::ID
            )
        })?;
        ensure!(
            canonical == self.canonical_target,
            "executable role {} changed canonical target before use",
            R::ID
        );
        let actual = sha256_file(self.invocation.as_path())?;
        ensure!(
            actual == self.sha256,
            "executable role {} SHA-256 changed before use: observed {actual}, expected {}",
            R::ID,
            self.sha256
        );
        Ok(&self.invocation)
    }
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file = File::open(path)
        .with_context(|| format!("failed to open executable {}", path.display()))?;
    let mut digest = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        digest.update(&buffer[..count]);
    }
    Ok(format!("{:x}", digest.finalize()))
}

#[derive(Debug)]
pub(super) struct RolePath<R: Role> {
    path: PathBuf,
    role: PhantomData<R>,
}

impl<R: Role> Clone for RolePath<R> {
    fn clone(&self) -> Self {
        Self {
            path: self.path.clone(),
            role: PhantomData,
        }
    }
}

impl<R: Role> RolePath<R> {
    pub(super) fn checked(path: PathBuf) -> Result<Self> {
        ensure!(
            path.ends_with(R::SUFFIX),
            "path {} cannot inhabit role {} (expected suffix {})",
            path.display(),
            R::ID,
            R::SUFFIX
        );
        Ok(Self::unchecked(path))
    }

    #[cfg(test)]
    pub(super) fn normalized() -> Self {
        Self::unchecked(PathBuf::from(format!("<{}>", R::ID)))
    }

    pub(super) fn as_path(&self) -> &Path {
        &self.path
    }

    fn unchecked(path: PathBuf) -> Self {
        Self {
            path,
            role: PhantomData,
        }
    }
}

macro_rules! roles {
    ($($name:ident => ($id:literal, $suffix:literal)),+ $(,)?) => {
        $(
            pub(super) enum $name {}
            impl sealed::Sealed for $name {}
            impl Role for $name {
                const ID: &'static str = $id;
                const SUFFIX: &'static str = $suffix;
            }
        )+
    };
}

roles! {
    GccExecutable => ("tool:arm-none-eabi-gcc", "arm-none-eabi-gcc"),
    NmExecutable => ("tool:arm-none-eabi-nm", "arm-none-eabi-nm"),
    QemuExecutable => ("tool:qemu-system-arm", "qemu-system-arm"),
    PythonExecutable => ("tool:python", "python"),
    GitExecutable => ("tool:git", "git"),
    PrlimitExecutable => ("tool:prlimit", "prlimit"),
    CargoExecutable => ("tool:cargo", "cargo"),
    RustcExecutable => ("tool:rustc", "rustc"),
    CompilerExecutable => ("tool:gate-owned-rumoca-compiler", "compiler/rumoca"),
    FixtureSource => ("source:snapshot/fixture/ExpMixedStep.mo", "fixture/ExpMixedStep.mo"),
    ComparatorGenerator => ("source:snapshot/comparator/gen_exp_mixed.py", "comparator/gen_exp_mixed.py"),
    ComparatorWrapper => ("source:snapshot/comparator/float_libm_wrapper.c", "comparator/float_libm_wrapper.c"),
    HarnessInclude => ("include:snapshot/harness", "harness"),
    OracleInclude => ("include:gate-owned-oracle", "oracle"),
    RumocaEmissionInclude => ("include:gate-owned-rumoca-emission", "rumoca"),
    CasadiEmissionInclude => ("include:gate-owned-casadi-emission", "casadi"),
    RumocaCacheDirectory => ("directory:gate-owned-rumoca-cache", "cache"),
    StartupSource => ("source:snapshot/harness/startup.S", "harness/startup.S"),
    TraceSource => ("source:snapshot/harness/trace_output.c", "harness/trace_output.c"),
    RumocaMeasuredDriver => ("source:snapshot/harness/driver_rumoca.c", "harness/driver_rumoca.c"),
    RumocaCorrectnessDriver => ("source:snapshot/harness/driver_rumoca_correctness.c", "harness/driver_rumoca_correctness.c"),
    CasadiMeasuredDriver => ("source:snapshot/harness/driver_casadi.c", "harness/driver_casadi.c"),
    CasadiCorrectnessDriver => ("source:snapshot/harness/driver_casadi_correctness.c", "harness/driver_casadi_correctness.c"),
    CounterSource => ("source:snapshot/harness/counter_fixtures.c", "harness/counter_fixtures.c"),
    LinkerScript => ("source:snapshot/harness/linker.ld", "harness/linker.ld"),
    InputHeader => ("source:snapshot/harness/inputs.h", "harness/inputs.h"),
    RumocaModel => ("source:emission/ExpMixedStep.c", "rumoca/ExpMixedStep.c"),
    RumocaHeader => ("source:emission/ExpMixedStep.h", "rumoca/ExpMixedStep.h"),
    RumocaKernels => ("source:emission/rumoca_galec_kernels.c", "rumoca/rumoca_galec_kernels.c"),
    RumocaKernelsHeader => ("source:emission/rumoca_galec_kernels.h", "rumoca/rumoca_galec_kernels.h"),
    CasadiGeneratedC => ("source:emission/casadi_exp_mixed.c", "casadi/casadi_exp_mixed.c"),
    CasadiGeneratedH => ("source:emission/casadi_exp_mixed.h", "casadi/casadi_exp_mixed.h"),
    RumocaDeletedCallDriver => ("source:gate-owned/mutation-deleted-call/driver_rumoca.c", "mutation-deleted-call/driver_rumoca.c"),
    RumocaConstantOutputDriver => ("source:gate-owned/mutation-constant-output/driver_rumoca.c", "mutation-constant-output/driver_rumoca.c"),
    RumocaClosedBranchDriver => ("source:gate-owned/mutation-closed-branches/driver_rumoca.c", "mutation-closed-branches/driver_rumoca.c"),
    RumocaClosedBranchModel => ("source:gate-owned/mutation-closed-branches/ExpMixedStep.c", "mutation-closed-branches/ExpMixedStep.c"),
    CasadiRowMajorDriver => ("source:gate-owned/mutation-row-major/driver_casadi.c", "mutation-row-major/driver_casadi.c"),
    StartupObject => ("object:startup.o", "startup.o"),
    TraceObject => ("object:trace_output.o", "trace_output.o"),
    DriverObject => ("object:driver.o", "driver.o"),
    ModelObject => ("object:ExpMixedStep.o", "ExpMixedStep.o"),
    KernelsObject => ("object:rumoca_galec_kernels.o", "rumoca_galec_kernels.o"),
    ComparatorObject => ("object:casadi_exp_mixed.o", "casadi_exp_mixed.o"),
    CounterObject => ("object:fixture.o", "fixture.o"),
    RumocaElf => ("output:rumoca_exp_mixed.elf", "rumoca_exp_mixed.elf"),
    CasadiElf => ("output:casadi_exp_mixed.elf", "casadi_exp_mixed.elf"),
    CounterStraightElf => ("output:counter-straight.elf", "counter-straight.elf"),
    CounterCalledLeafElf => ("output:counter-called-leaf.elf", "counter-called-leaf.elf"),
    CounterExitStatusElf => ("output:counter-exit-status.elf", "counter-exit-status.elf"),
    RumocaSymbols => ("capture:rumoca_exp_mixed.symbols.txt", "rumoca_exp_mixed.symbols.txt"),
    CasadiSymbols => ("capture:casadi_exp_mixed.symbols.txt", "casadi_exp_mixed.symbols.txt"),
    CounterStraightSymbols => ("capture:counter-straight.symbols.txt", "counter-straight.symbols.txt"),
    CounterCalledLeafSymbols => ("capture:counter-called-leaf.symbols.txt", "counter-called-leaf.symbols.txt"),
    CounterExitStatusSymbols => ("capture:counter-exit-status.symbols.txt", "counter-exit-status.symbols.txt"),
}

impl ExecutableRole for GccExecutable {}
impl ExecutableRole for NmExecutable {}
impl ExecutableRole for QemuExecutable {}
impl ExecutableRole for PythonExecutable {}
impl ExecutableRole for GitExecutable {}
impl ExecutableRole for PrlimitExecutable {}
impl ExecutableRole for CargoExecutable {}
impl ExecutableRole for RustcExecutable {}
impl ExecutableRole for CompilerExecutable {}

#[cfg(test)]
mod tests {
    use super::{AuthenticatedExecutable, PythonExecutable, RolePath, RumocaKernels, RumocaModel};
    use sha2::{Digest, Sha256};
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn source_roles_reject_swapped_filenames() {
        assert!(
            RolePath::<RumocaModel>::checked(PathBuf::from(
                "row/emitted/rumoca/rumoca_galec_kernels.c"
            ))
            .is_err()
        );
        assert!(
            RolePath::<RumocaKernels>::checked(PathBuf::from("row/emitted/rumoca/ExpMixedStep.c"))
                .is_err()
        );
    }

    #[cfg(unix)]
    #[test]
    fn authenticated_executable_rejects_content_mutation_and_symlink_swap_at_use() {
        use std::os::unix::fs::symlink;

        let temporary = tempfile::tempdir().unwrap();
        let first = temporary.path().join("python-first");
        let second = temporary.path().join("python-second");
        let invocation = temporary.path().join("python");
        fs::write(&first, "first executable bytes").unwrap();
        fs::write(&second, "second executable bytes").unwrap();
        symlink(&first, &invocation).unwrap();
        let expected = format!("{:x}", Sha256::digest(fs::read(&first).unwrap()));
        let authenticated = AuthenticatedExecutable::<PythonExecutable>::checked(
            RolePath::checked(invocation.clone()).unwrap(),
            &expected,
        )
        .unwrap();
        assert!(authenticated.for_execution().is_ok());

        fs::write(&first, "mutated executable bytes").unwrap();
        assert!(authenticated.for_execution().is_err());
        fs::write(&first, "first executable bytes").unwrap();
        fs::remove_file(&invocation).unwrap();
        symlink(&second, &invocation).unwrap();
        let error = authenticated
            .for_execution()
            .err()
            .expect("symlink swap must fail authentication");
        assert!(error.to_string().contains("changed canonical target"));
    }
}
