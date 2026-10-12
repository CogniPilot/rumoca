# Immutable numerical flight provisioning

The numerical corpus now provisions revision
`3d6204e7a907e452e08c46cd63352d6365089a38` from the verification-only manifest.
Its actual GitHub ZIP is 931,464 bytes with SHA-256
`9475fdc2e27df233a1c8b8debddbbc56765c9559c69938a1583df2210c3aec6f`.
The measured archive payload matches all 747 files in the qualified flight
snapshot. This is the ZIP checksum, independently measured from the URL;
it is not the local Git-TAR checksum.

The existing typed dependency loader, checksum/download owner and ZIP path
projection are reused. A retained checksum-verified archive supplies the complete
source inventory. Root resolution constructs one `QualifiedSource` before
workers start; neither workers nor a later availability check rederive it.
Every archived file must be a regular file with identical bytes, and additional
source files are refused. Git metadata and the legacy revision marker are not
source authority. Explicit argv/config roots remain authoritative and never
fall back. Existing CMM and compatibility a41 manifests are unchanged. MSL-only
selections do not provision flight sources.

At source `bca63578e52a5e23c8df7fdda3d694a6daa706de`, eight dependency-owner controls
and the existing argv/config authority control passed. They cover default
provisioning/reuse, wrong-revision packages with a forged marker, dirty cached
source, additional source, archive corruption, Git metadata, source symlinks
and distinct cache/pin declarations. Scoped xtask all-target Clippy and formatter
checks passed. The public `verify corpus-pin` command then provisioned the
default cache and passed the unchanged Cubs2 Takeoff row using the sealed 5e6
compiler in 2.1 seconds. The actual a41 explicit root was refused before compiler
execution at its differing `.github/workflows/ci.yml`.

The compound qualification finished in 42.943 seconds, peaking at 1,229,256 KiB
aggregate RSS plus swap and 6,612,140,032 bytes in the owned native cache. Its
600-second/4-GiB memory/8-GiB cache and scratch/16-GiB host-reserve guards were
unchanged. Tracked source seals matched before and after. The initial candidate
also passed the nine tests but failed Clippy on one redundant manifest-loader
borrow; that failed attempt is retained separately.

[The receipt](immutable-flight-provisioning.json) binds source, actual ZIP,
xtask executable, logs, resource guardian, tests and public command outcomes.
Raw evidence is retained under
`$HOME/scratch/rumoca/pr427-flight-dependency-20261012`. This qualifies the
provisioning owner and one default numerical row; it does not claim all 28 rows,
canonical quick/full, compatibility CI or a later combined source passed.
