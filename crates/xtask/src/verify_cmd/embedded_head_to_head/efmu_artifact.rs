//! Content authentication for one candidate ECM-003 eFMU artifact.
//!
//! A history revision is derived from the raw archive. Callers cannot supply
//! member or checksum-web pins and ask this boundary to bless them.

mod xml_topology;
mod zip_preflight;

use anyhow::{Context, Result, ensure};
use sha1::Sha1;
use sha2::Digest;
#[cfg(test)]
use sha2::Sha256;
use std::collections::BTreeMap;
use std::fs;
use std::io::{Cursor, Read};
use std::path::Path;
use uuid::Uuid;

use super::manifest::{ArtifactSessionInputs, ChecksumWebEdge, Entry};
#[cfg(test)]
use super::manifest::{
    CorrectnessCase, EfmuArchiveMember, EfmuProductionMember, OracleSuccessReceipt,
    RumocaArtifactRevision, correctness_cases_sha256,
};

const ROOT_MEMBER: &str = "__content.xml";
const AC_MANIFEST_MEMBER: &str = "AlgorithmCode/manifest.xml";
const PC_MANIFEST_MEMBER: &str = "ProductionCode/manifest.xml";
#[cfg(test)]
const RECEIPT_DOMAIN: &[u8] = b"rumoca-embedded-head-to-head-efmu-oracle-success-v1";
const IDENTITY_DOMAIN: &str = "rumoca-artifact-identity-v1";
const MAX_ARCHIVE_BYTES: u64 = 64 * 1024 * 1024;
const MAX_MEMBER_BYTES: u64 = 16 * 1024 * 1024;
const MAX_TOTAL_MEMBER_BYTES: u64 = MAX_ARCHIVE_BYTES;
const CANONICAL_MEMBER_COUNT: usize = 52;

/// Exact producer-first inventory issued by the schema-9 `efmu` target's
/// explicit `[[package.members]]` closed sum.
const CANONICAL_MEMBERS: [&str; CANONICAL_MEMBER_COUNT] = [
    "AlgorithmCode/model.alg",
    "AlgorithmCode/manifest.xml",
    "ProductionCode/sources/production.h",
    "ProductionCode/sources/production.c",
    "ProductionCode/manifest.xml",
    "__content.xml",
    "schemas/AlgorithmCode/VERSION.txt",
    "schemas/AlgorithmCode/efmiAlgorithmCodeManifest.xsd",
    "schemas/AlgorithmCode/efmiVariable.xsd",
    "schemas/BehavioralModel/VERSION.txt",
    "schemas/BehavioralModel/efmiBehavioralModelManifest.xsd",
    "schemas/BehavioralModel/efmiClocks.xsd",
    "schemas/BehavioralModel/efmiCsvMappings.xsd",
    "schemas/BehavioralModel/efmiScenarios.xsd",
    "schemas/BehavioralModel/efmiTolerancesSetups.xsd",
    "schemas/BehavioralModel/efmiVariables.xsd",
    "schemas/BinaryCode/VERSION.txt",
    "schemas/BinaryCode/efmiBinaryCodeManifest.xsd",
    "schemas/BinaryCode/efmiBinaryContainerInfoFileReferences.xsd",
    "schemas/BinaryCode/efmiBuildInformation.xsd",
    "schemas/BinaryCode/efmiModules.xsd",
    "schemas/BinaryCode/efmiObjectFile.xsd",
    "schemas/BinaryCode/efmiRunTimeComplianceInformation.xsd",
    "schemas/LICENSE",
    "schemas/ProductionCode/VERSION.txt",
    "schemas/ProductionCode/efmiCodeFiles.xsd",
    "schemas/ProductionCode/efmiDimensions.xsd",
    "schemas/ProductionCode/efmiFunctions.xsd",
    "schemas/ProductionCode/efmiIncludes.xsd",
    "schemas/ProductionCode/efmiLogicalData.xsd",
    "schemas/ProductionCode/efmiMacros.xsd",
    "schemas/ProductionCode/efmiProductionCodeManifest.xsd",
    "schemas/ProductionCode/efmiSupportedLanguages.xsd",
    "schemas/ProductionCode/efmiSupportedPlatforms.xsd",
    "schemas/ProductionCode/efmiTargetTypes.xsd",
    "schemas/ProductionCode/efmiTechnicalLookUps.xsd",
    "schemas/ProductionCode/efmiTypeDefs.xsd",
    "schemas/ProductionCode/efmiVariables.xsd",
    "schemas/README.md",
    "schemas/VERSION.txt",
    "schemas/efmiAnnotation.xsd",
    "schemas/efmiCompilerOptions.xsd",
    "schemas/efmiContainerManifest.xsd",
    "schemas/efmiFiles.xsd",
    "schemas/efmiFloatingPointPrecision.xsd",
    "schemas/efmiIdentifierType.xsd",
    "schemas/efmiLinkerOptions.xsd",
    "schemas/efmiManifestAttributes.xsd",
    "schemas/efmiManifestReferences.xsd",
    "schemas/efmiModelRepresentationKind.xsd",
    "schemas/efmiUnits.xsd",
    "schemas/efmiWildcard.xsd",
];

#[derive(Clone, Copy)]
#[repr(usize)]
enum CoreMember {
    AlgorithmCodeSource = 0,
    AlgorithmCodeManifest = 1,
    ProductionCodeHeader = 2,
    ProductionCodeSource = 3,
    ProductionCodeManifest = 4,
    ContainerManifest = 5,
}

impl CoreMember {
    #[cfg(test)]
    const ALL: [Self; 6] = [
        Self::AlgorithmCodeSource,
        Self::AlgorithmCodeManifest,
        Self::ProductionCodeHeader,
        Self::ProductionCodeSource,
        Self::ProductionCodeManifest,
        Self::ContainerManifest,
    ];

    const fn path(self) -> &'static str {
        CANONICAL_MEMBERS[self as usize]
    }
}

struct ArchiveEvidence {
    #[cfg(test)]
    raw: Vec<u8>,
    members: [ArchiveMember; CANONICAL_MEMBER_COUNT],
}

struct ArchiveMember {
    #[cfg(test)]
    path: String,
    bytes: Vec<u8>,
}

/// One archive whose inventory, session-bound manifests, and checksum web are
/// authenticated before its exact Production C/H members are exposed.
///
/// The schema fixture retains the row/session/cohort evidence under `cfg(test)`
/// because production has no receipt issuer until the consumed
/// build/oracle/measurement chain exists.
pub(super) struct AuthenticatedEfmuBeforeOracle {
    #[cfg(test)]
    entry_id: String,
    #[cfg(test)]
    expected_cohort: Vec<CorrectnessCase>,
    #[cfg(test)]
    artifact_session: ArtifactSessionInputs,
    archive: ArchiveEvidence,
    #[cfg(test)]
    checksum_web_membership: Vec<ChecksumWebEdge>,
}

/// Role-preserving access to the exact Production C/H members retained by an
/// authenticated archive. The source and header cannot be supplied as free
/// paths or paired from another package.
pub(super) struct AuthenticatedProductionCode<'archive> {
    source: &'archive ArchiveMember,
    header: &'archive ArchiveMember,
}

impl ArchiveEvidence {
    fn member_record(&self, member: CoreMember) -> &ArchiveMember {
        &self.members[member as usize]
    }

    fn member(&self, member: CoreMember) -> &[u8] {
        &self.member_record(member).bytes
    }

    #[cfg(test)]
    fn package_sha256(&self) -> String {
        sha256_hex(&self.raw)
    }

    #[cfg(test)]
    fn authenticated_members(&self) -> Vec<EfmuArchiveMember> {
        self.members
            .iter()
            .map(|member| EfmuArchiveMember {
                archive_path: member.path.clone(),
                sha256: sha256_hex(&member.bytes),
            })
            .collect()
    }
}

impl AuthenticatedEfmuBeforeOracle {
    pub(super) fn production_code(&self) -> AuthenticatedProductionCode<'_> {
        AuthenticatedProductionCode {
            source: self.archive.member_record(CoreMember::ProductionCodeSource),
            header: self.archive.member_record(CoreMember::ProductionCodeHeader),
        }
    }

    #[cfg(test)]
    fn close(
        self,
        rumoca_metric: u64,
        rationale: &str,
        observed_cohort: &[CorrectnessCase],
    ) -> Result<RumocaArtifactRevision> {
        ensure!(
            rumoca_metric > 0,
            "{}: artifact metric must be nonzero",
            self.entry_id
        );
        ensure!(
            !rationale.is_empty()
                && rationale.trim() == rationale
                && !rationale.contains(['\r', '\n']),
            "{}: artifact rationale must be one canonical non-empty line",
            self.entry_id
        );
        ensure!(
            observed_cohort == self.expected_cohort,
            "{}: oracle-success receipt requires the exact ordered row correctness cohort",
            self.entry_id
        );
        let production_code = self.production_code();
        let production_c = production_code.source_pin();
        let production_h = production_code.header_pin();
        let efmu_package_sha256 = self.archive.package_sha256();
        let archive_members = self.archive.authenticated_members();
        let receipt_sha256 = correctness_receipt_sha256(
            &self.entry_id,
            &self.artifact_session,
            observed_cohort,
            &self.archive.raw,
        );
        Ok(RumocaArtifactRevision {
            rumoca_metric,
            rationale: rationale.to_owned(),
            artifact_session: self.artifact_session,
            efmu_package_sha256: efmu_package_sha256.clone(),
            archive_members,
            production_c,
            production_h,
            checksum_web_membership: self.checksum_web_membership,
            oracle_success_receipt: OracleSuccessReceipt {
                receipt_sha256,
                efmu_package_sha256,
                correctness_cases_sha256: correctness_cases_sha256(&self.expected_cohort),
            },
        })
    }
}

impl AuthenticatedProductionCode<'_> {
    fn source_bytes(&self) -> &[u8] {
        &self.source.bytes
    }

    fn header_bytes(&self) -> &[u8] {
        &self.header.bytes
    }

    #[cfg(test)]
    fn source_pin(&self) -> EfmuProductionMember {
        EfmuProductionMember {
            archive_path: self.source.path.clone(),
            sha256: sha256_hex(self.source_bytes()),
        }
    }

    #[cfg(test)]
    fn header_pin(&self) -> EfmuProductionMember {
        EfmuProductionMember {
            archive_path: self.header.path.clone(),
            sha256: sha256_hex(self.header_bytes()),
        }
    }
}

pub(super) fn authenticated_production_source_bytes<'view, 'archive>(
    production: &'view AuthenticatedProductionCode<'archive>,
) -> &'view [u8] {
    production.source_bytes()
}

pub(super) fn authenticated_production_header_bytes<'view, 'archive>(
    production: &'view AuthenticatedProductionCode<'archive>,
) -> &'view [u8] {
    production.header_bytes()
}

/// Authenticate the archive before any compiler or oracle execution consumes
/// its Production C/H bytes. The returned affine value retains the exact row,
/// session, archive, and expected cohort, so closing a receipt cannot pair
/// evidence from another row or package.
pub(super) fn authenticate_before_oracle(
    archive_path: &Path,
    entry: &Entry,
    artifact_session: ArtifactSessionInputs,
) -> Result<AuthenticatedEfmuBeforeOracle> {
    super::manifest::validate_artifact_session(&entry.id, &artifact_session)?;
    let archive = read_archive(archive_path)?;
    let xml = xml_topology::EfmuXmlTopology::authenticate(
        archive.member(CoreMember::ContainerManifest),
        archive.member(CoreMember::AlgorithmCodeManifest),
        archive.member(CoreMember::ProductionCodeManifest),
    )?;
    authenticate_session(&xml, &artifact_session)?;
    #[cfg(test)]
    let checksum_web_membership = authenticate_checksum_web(&archive, &xml)?;
    #[cfg(not(test))]
    authenticate_checksum_web(&archive, &xml)?;
    Ok(AuthenticatedEfmuBeforeOracle {
        #[cfg(test)]
        entry_id: entry.id.clone(),
        #[cfg(test)]
        expected_cohort: entry.correctness_cases.clone(),
        #[cfg(test)]
        artifact_session,
        archive,
        #[cfg(test)]
        checksum_web_membership,
    })
}

/// Test-only fixture derivation for the schema-9 receipt value shape. Production code
/// has no receipt issuer until the consumed build/oracle/measurement typestate
/// chain is complete.
#[cfg(test)]
pub(super) fn derive_revision(
    archive_path: &Path,
    entry: &Entry,
    rumoca_metric: u64,
    rationale: &str,
    artifact_session: ArtifactSessionInputs,
    observed_cohort: &[CorrectnessCase],
) -> Result<RumocaArtifactRevision> {
    authenticate_before_oracle(archive_path, entry, artifact_session)?.close(
        rumoca_metric,
        rationale,
        observed_cohort,
    )
}

/// Test-only mutation helper. This is deliberately absent from the production
/// binary: the eventual linear evidence chain must retain and consume the one
/// authenticated archive instead of reopening it.
#[cfg(test)]
pub(super) fn authenticate_revision(
    archive_path: &Path,
    entry: &Entry,
    revision: &RumocaArtifactRevision,
    observed_cohort: &[CorrectnessCase],
) -> Result<()> {
    let derived = derive_revision(
        archive_path,
        entry,
        revision.rumoca_metric,
        &revision.rationale,
        revision.artifact_session.clone(),
        observed_cohort,
    )?;
    ensure!(
        &derived == revision,
        "{}: proposed eFMU history revision differs from raw archive/oracle evidence",
        entry.id
    );
    Ok(())
}

const _: for<'archive> fn(
    &'archive AuthenticatedEfmuBeforeOracle,
) -> AuthenticatedProductionCode<'archive> = AuthenticatedEfmuBeforeOracle::production_code;
const _: fn(&Path, &Entry, ArtifactSessionInputs) -> Result<AuthenticatedEfmuBeforeOracle> =
    authenticate_before_oracle;
const _: for<'view, 'archive> fn(&'view AuthenticatedProductionCode<'archive>) -> &'view [u8] =
    authenticated_production_source_bytes;
const _: for<'view, 'archive> fn(&'view AuthenticatedProductionCode<'archive>) -> &'view [u8] =
    authenticated_production_header_bytes;

fn read_archive(path: &Path) -> Result<ArchiveEvidence> {
    let file = open_archive_no_follow(path)?;
    let metadata = file
        .metadata()
        .with_context(|| format!("inspect opened eFMU archive `{}`", path.display()))?;
    ensure!(
        metadata.is_file() && metadata.len() <= MAX_ARCHIVE_BYTES,
        "eFMU archive `{}` must be a regular file of at most {MAX_ARCHIVE_BYTES} bytes",
        path.display()
    );
    read_archive_handle(file, metadata.len(), path)
}

#[cfg(unix)]
fn open_archive_no_follow(path: &Path) -> Result<fs::File> {
    use std::os::unix::fs::OpenOptionsExt as _;

    fs::OpenOptions::new()
        .read(true)
        .custom_flags(nix::libc::O_NOFOLLOW | nix::libc::O_NONBLOCK)
        .open(path)
        .with_context(|| {
            format!(
                "open eFMU archive `{}` without following a symbolic link",
                path.display()
            )
        })
}

#[cfg(windows)]
fn open_archive_no_follow(path: &Path) -> Result<fs::File> {
    use std::os::windows::fs::OpenOptionsExt as _;

    // FILE_FLAG_OPEN_REPARSE_POINT makes metadata describe the named reparse
    // point itself; the regular-file check then rejects links and junctions.
    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
    fs::OpenOptions::new()
        .read(true)
        .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
        .open(path)
        .with_context(|| {
            format!(
                "open eFMU archive `{}` without traversing its final reparse point",
                path.display()
            )
        })
}

#[cfg(not(any(unix, windows)))]
fn open_archive_no_follow(path: &Path) -> Result<fs::File> {
    anyhow::bail!(
        "eFMU archive authentication is unavailable on this host: `{}` cannot be opened with a supported no-follow policy",
        path.display()
    )
}

fn read_archive_handle(file: fs::File, handle_size: u64, path: &Path) -> Result<ArchiveEvidence> {
    let raw = read_bounded_archive(file, handle_size, path)?;
    zip_preflight::authenticate(&raw, &CANONICAL_MEMBERS)?;
    let mut zip = zip::ZipArchive::new(Cursor::new(&raw))
        .with_context(|| format!("open eFMU archive `{}`", path.display()))?;
    let mut members = Vec::with_capacity(zip.len());
    let mut total_decoded_size = 0_u64;
    for index in 0..zip.len() {
        let expected = CANONICAL_MEMBERS.get(index).with_context(|| {
            format!(
                "eFMU archive has more than the {CANONICAL_MEMBER_COUNT} canonical members; unexpected member at index {index}"
            )
        })?;
        let member = zip
            .by_index(index)
            .with_context(|| format!("read eFMU archive member at index {index}"))?;
        ensure!(
            member.name_raw() == expected.as_bytes(),
            "eFMU archive member {index} raw name bytes differ from canonical `{expected}`"
        );
        let path = member.name().to_owned();
        ensure_portable_member_path(&path)?;
        ensure!(
            member.is_file(),
            "eFMU archive member `{path}` must be a regular file"
        );
        ensure!(
            member.unix_mode() == Some(0o100644),
            "eFMU archive member `{path}` must carry canonical regular-file mode 0100644"
        );
        ensure!(
            member.size() <= MAX_MEMBER_BYTES,
            "eFMU archive member `{path}` exceeds the {MAX_MEMBER_BYTES}-byte evidence bound"
        );
        ensure!(
            path == *expected,
            "eFMU archive member {index} is `{path}`; canonical member is `{expected}`"
        );
        let declared_size = member.size();
        let remaining_total = MAX_TOTAL_MEMBER_BYTES
            .checked_sub(total_decoded_size)
            .context("verified eFMU decoded-member total exceeded its construction bound")?;
        let read_limit = MAX_MEMBER_BYTES.min(remaining_total) + 1;
        let mut bytes = Vec::new();
        member
            .take(read_limit)
            .read_to_end(&mut bytes)
            .with_context(|| format!("read eFMU member `{path}` raw content"))?;
        let decoded_size = u64::try_from(bytes.len())
            .with_context(|| format!("decoded eFMU member `{path}` length exceeds u64"))?;
        ensure!(
            decoded_size <= MAX_MEMBER_BYTES,
            "eFMU archive member `{path}` expands beyond the {MAX_MEMBER_BYTES}-byte evidence bound"
        );
        let next_total = total_decoded_size
            .checked_add(decoded_size)
            .with_context(|| format!("decoded eFMU member total overflows at `{path}`"))?;
        ensure!(
            next_total <= MAX_TOTAL_MEMBER_BYTES,
            "eFMU archive decoded-member inventory exceeds the {MAX_TOTAL_MEMBER_BYTES}-byte cumulative evidence bound at `{path}`"
        );
        ensure!(
            decoded_size == declared_size,
            "eFMU archive member `{path}` declares {declared_size} uncompressed bytes but decodes to {decoded_size}"
        );
        total_decoded_size = next_total;
        members.push(ArchiveMember {
            #[cfg(test)]
            path,
            bytes,
        });
    }
    let members: [ArchiveMember; CANONICAL_MEMBER_COUNT] =
        members.try_into().map_err(|members: Vec<ArchiveMember>| {
            anyhow::anyhow!(
                "eFMU archive has {} members; exact canonical inventory requires {CANONICAL_MEMBER_COUNT}",
                members.len()
            )
        })?;
    Ok(ArchiveEvidence {
        #[cfg(test)]
        raw,
        members,
    })
}

fn read_bounded_archive(file: fs::File, handle_size: u64, path: &Path) -> Result<Vec<u8>> {
    let mut raw = Vec::new();
    file.take(MAX_ARCHIVE_BYTES + 1)
        .read_to_end(&mut raw)
        .with_context(|| format!("read opened eFMU archive `{}`", path.display()))?;
    let observed_size = u64::try_from(raw.len()).with_context(|| {
        format!(
            "opened eFMU archive `{}` length exceeds u64",
            path.display()
        )
    })?;
    ensure!(
        observed_size <= MAX_ARCHIVE_BYTES,
        "eFMU archive `{}` grew beyond the {MAX_ARCHIVE_BYTES}-byte evidence bound while reading",
        path.display()
    );
    ensure!(
        observed_size == handle_size,
        "eFMU archive `{}` changed size while reading: opened handle reported {handle_size} bytes but yielded {observed_size}",
        path.display()
    );
    Ok(raw)
}

fn ensure_portable_member_path(path: &str) -> Result<()> {
    ensure!(
        !path.is_empty()
            && path.len() <= 255
            && path.bytes().all(|byte| {
                byte.is_ascii_alphanumeric() || matches!(byte, b'/' | b'.' | b'_' | b'-')
            })
            && path
                .split('/')
                .all(|component| !component.is_empty() && component != "." && component != ".."),
        "`{path}` is not a portable exact eFMU archive member path"
    );
    Ok(())
}

fn authenticate_session(
    xml: &xml_topology::EfmuXmlTopology,
    session: &ArtifactSessionInputs,
) -> Result<()> {
    let seed =
        Uuid::parse_str(&session.identity_seed).context("artifact identity seed must be a UUID")?;
    ensure!(
        seed.hyphenated().to_string() == session.identity_seed,
        "artifact identity seed must be a canonical lowercase hyphenated UUID"
    );
    for (attributes, member, identity_key) in [
        (&xml.container.root, ROOT_MEMBER, "content"),
        (&xml.algorithm.root, AC_MANIFEST_MEMBER, "ac_manifest"),
        (&xml.production.root, PC_MANIFEST_MEMBER, "pc_manifest"),
    ] {
        require_attribute(
            attributes,
            "generationDateAndTime",
            &session.generation_instant,
            member,
        )?;
        let identity = format!("{{{}}}", artifact_identity(seed, identity_key));
        require_attribute(attributes, "id", &identity, member)?;
    }
    Ok(())
}

fn authenticate_checksum_web(
    archive: &ArchiveEvidence,
    xml: &xml_topology::EfmuXmlTopology,
) -> Result<Vec<ChecksumWebEdge>> {
    let ac = archive.member(CoreMember::AlgorithmCodeManifest);
    let pc = archive.member(CoreMember::ProductionCodeManifest);
    let ac_id = required_attribute(&xml.algorithm.root, "id", AC_MANIFEST_MEMBER)?;
    let pc_id = required_attribute(&xml.production.root, "id", PC_MANIFEST_MEMBER)?;
    authenticate_representation(
        &xml.container.algorithm_representation,
        "AlgorithmCode",
        ac_id,
        &sha1_hex(ac),
    )?;
    authenticate_representation(
        &xml.container.production_representation,
        "ProductionCode",
        pc_id,
        &sha1_hex(pc),
    )?;

    authenticate_file(
        &xml.algorithm.source_file,
        "./",
        "model.alg",
        &sha1_hex(archive.member(CoreMember::AlgorithmCodeSource)),
        AC_MANIFEST_MEMBER,
    )?;

    authenticate_file(
        &xml.production.header_file,
        "./sources/",
        "production.h",
        &sha1_hex(archive.member(CoreMember::ProductionCodeHeader)),
        PC_MANIFEST_MEMBER,
    )?;
    authenticate_file(
        &xml.production.source_file,
        "./sources/",
        "production.c",
        &sha1_hex(archive.member(CoreMember::ProductionCodeSource)),
        PC_MANIFEST_MEMBER,
    )?;
    require_attribute(
        &xml.production.algorithm_reference,
        "manifestRefId",
        ac_id,
        PC_MANIFEST_MEMBER,
    )?;
    require_attribute(
        &xml.production.algorithm_reference,
        "checksum",
        &sha1_hex(ac),
        PC_MANIFEST_MEMBER,
    )?;
    require_attribute(
        &xml.production.algorithm_reference,
        "origin",
        "true",
        PC_MANIFEST_MEMBER,
    )?;

    [
        (
            CoreMember::AlgorithmCodeSource,
            CoreMember::AlgorithmCodeManifest,
        ),
        (
            CoreMember::ProductionCodeSource,
            CoreMember::ProductionCodeManifest,
        ),
        (
            CoreMember::ProductionCodeHeader,
            CoreMember::ProductionCodeManifest,
        ),
        (
            CoreMember::AlgorithmCodeManifest,
            CoreMember::ProductionCodeManifest,
        ),
        (
            CoreMember::AlgorithmCodeManifest,
            CoreMember::ContainerManifest,
        ),
        (
            CoreMember::ProductionCodeManifest,
            CoreMember::ContainerManifest,
        ),
    ]
    .into_iter()
    .map(|(producer, consumer)| {
        Ok(ChecksumWebEdge {
            producer_archive_path: producer.path().to_owned(),
            producer_sha1: sha1_hex(archive.member(producer)),
            consumer_archive_path: consumer.path().to_owned(),
        })
    })
    .collect::<Result<Vec<_>>>()
}

fn authenticate_representation(
    attributes: &BTreeMap<String, String>,
    name: &str,
    manifest_id: &str,
    checksum: &str,
) -> Result<()> {
    require_attribute(attributes, "name", name, ROOT_MEMBER)?;
    require_attribute(attributes, "kind", name, ROOT_MEMBER)?;
    require_attribute(attributes, "manifest", "manifest.xml", ROOT_MEMBER)?;
    require_attribute(attributes, "checksum", checksum, ROOT_MEMBER)?;
    require_attribute(attributes, "manifestRefId", manifest_id, ROOT_MEMBER)
}

fn authenticate_file(
    attributes: &BTreeMap<String, String>,
    path: &str,
    name: &str,
    checksum: &str,
    manifest: &str,
) -> Result<()> {
    require_attribute(attributes, "path", path, manifest)?;
    require_attribute(attributes, "name", name, manifest)?;
    require_attribute(attributes, "checksum", checksum, manifest)?;
    require_attribute(attributes, "needsChecksum", "true", manifest)
}

fn required_attribute<'a>(
    attributes: &'a BTreeMap<String, String>,
    name: &str,
    member: &str,
) -> Result<&'a str> {
    attributes
        .get(name)
        .map(String::as_str)
        .with_context(|| format!("`{member}` is missing `{name}`"))
}

fn require_attribute(
    attributes: &BTreeMap<String, String>,
    name: &str,
    expected: &str,
    member: &str,
) -> Result<()> {
    let observed = required_attribute(attributes, name, member)?;
    ensure!(
        observed == expected,
        "`{member}` `{name}` is `{observed}`; expected `{expected}`"
    );
    Ok(())
}

#[cfg(test)]
fn correctness_receipt_sha256(
    entry_id: &str,
    session: &ArtifactSessionInputs,
    observed_cohort: &[CorrectnessCase],
    raw_archive: &[u8],
) -> String {
    let mut digest = Sha256::new();
    frame(&mut digest, RECEIPT_DOMAIN);
    frame(&mut digest, b"oracle-success");
    frame(&mut digest, entry_id.as_bytes());
    frame(&mut digest, session.generation_instant.as_bytes());
    frame(&mut digest, session.identity_seed.as_bytes());
    frame(&mut digest, raw_archive);
    frame(&mut digest, &(observed_cohort.len() as u64).to_be_bytes());
    for case in observed_cohort {
        frame(&mut digest, case.id.as_bytes());
        frame(
            &mut digest,
            &(case.expected_output_bits.len() as u64).to_be_bytes(),
        );
        for word in &case.expected_output_bits {
            frame(&mut digest, word.as_bytes());
        }
    }
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
fn frame(digest: &mut Sha256, bytes: &[u8]) {
    digest.update((bytes.len() as u64).to_be_bytes());
    digest.update(bytes);
}

fn artifact_identity(namespace: Uuid, identity_key: &str) -> String {
    let mut name = Vec::new();
    for value in [
        IDENTITY_DOMAIN,
        "builtin-registry-key",
        "efmu",
        identity_key,
    ] {
        name.extend_from_slice(&(value.len() as u64).to_be_bytes());
        name.extend_from_slice(value.as_bytes());
    }
    Uuid::new_v5(&namespace, &name).hyphenated().to_string()
}

fn sha1_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha1::digest(bytes))
}

#[cfg(test)]
fn sha256_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

#[cfg(test)]
pub(super) const fn canonical_members() -> &'static [&'static str] {
    &CANONICAL_MEMBERS
}

#[cfg(test)]
pub(super) fn artifact_identity_for_test(seed: &str, identity_key: &str) -> String {
    artifact_identity(
        Uuid::parse_str(seed).expect("canonical test UUID"),
        identity_key,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;
    use std::mem::size_of;

    const LOCAL_HEADER: &[u8; 4] = b"PK\x03\x04";
    const CENTRAL_HEADER: &[u8; 4] = b"PK\x01\x02";

    #[test]
    fn archive_reader_bounds_verified_decompressed_bytes_not_declared_size() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("lying-expansion.efmu");
        let expanding = vec![
            0_u8;
            usize::try_from(MAX_MEMBER_BYTES + 1)
                .expect("member test bound fits the host")
        ];
        write_canonical_archive(&archive_path, &expanding);

        let mut raw = fs::read(&archive_path).expect("read expanding archive");
        rewrite_declared_size(&mut raw, CANONICAL_MEMBERS[0], 1);
        fs::write(&archive_path, raw).expect("write lying archive headers");

        let error = match read_archive(&archive_path) {
            Ok(_) => panic!("verified decompression must reject expansion beyond the member bound"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("expands beyond"),
            "unexpected expansion rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_bounds_the_cumulative_decoded_inventory() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("cumulative-expansion.efmu");
        let member_size = MAX_TOTAL_MEMBER_BYTES
            .checked_div(CANONICAL_MEMBERS.len() as u64)
            .and_then(|size| size.checked_add(1))
            .expect("canonical member count yields a bounded test size");
        assert!(member_size < MAX_MEMBER_BYTES);
        let member_bytes =
            vec![0_u8; usize::try_from(member_size).expect("cumulative test member fits the host")];
        write_uniform_canonical_archive(&archive_path, &member_bytes);

        let error = match read_archive(&archive_path) {
            Ok(_) => panic!("cumulative decoded inventory beyond the archive bound must reject"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("cumulative evidence bound"),
            "unexpected cumulative expansion rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_rejects_growth_after_opening_the_authenticated_handle() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("growing.efmu");
        write_canonical_archive(&archive_path, b"bounded first member\n");
        let file = open_archive_no_follow(&archive_path).expect("open bounded archive handle");
        let handle_size = file.metadata().expect("opened archive metadata").len();
        fs::OpenOptions::new()
            .append(true)
            .open(&archive_path)
            .expect("open archive for deterministic growth")
            .write_all(b"growth")
            .expect("grow archive after evidence handle opens");

        let error = match read_archive_handle(file, handle_size, &archive_path) {
            Ok(_) => panic!("archive growth after open must reject"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("changed size while reading"),
            "unexpected growth rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_bounds_raw_growth_from_the_authenticated_handle() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("over-bound-growth.efmu");
        fs::write(&archive_path, b"opened evidence").expect("write initial archive bytes");
        let file = open_archive_no_follow(&archive_path).expect("open bounded archive handle");
        let handle_size = file.metadata().expect("opened archive metadata").len();
        fs::OpenOptions::new()
            .write(true)
            .open(&archive_path)
            .expect("open archive for deterministic sparse growth")
            .set_len(MAX_ARCHIVE_BYTES + 1)
            .expect("grow archive beyond the raw evidence bound");

        let error = match read_archive_handle(file, handle_size, &archive_path) {
            Ok(_) => panic!("raw archive growth beyond the evidence bound must reject"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("grew beyond"),
            "unexpected raw-bound rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_authenticates_the_open_handle_across_path_replacement() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("swapped.efmu");
        let displaced_path = temporary.path().join("opened.efmu");
        write_canonical_archive(&archive_path, b"authenticated first member\n");
        let expected_raw = fs::read(&archive_path).expect("read original archive bytes");
        let file =
            open_archive_no_follow(&archive_path).expect("open authenticated archive handle");
        let handle_size = file.metadata().expect("opened archive metadata").len();
        fs::rename(&archive_path, &displaced_path).expect("move opened archive path");
        fs::write(&archive_path, b"attacker replacement").expect("replace archive path");

        let evidence = read_archive_handle(file, handle_size, &archive_path)
            .expect("the opened handle remains the authenticated byte source");
        assert_eq!(evidence.raw, expected_raw);
    }

    #[test]
    fn core_member_lookup_uses_the_authenticated_producer_order() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("producer-order.efmu");
        write_canonical_archive(&archive_path, b"algorithm source\n");

        let evidence = read_archive(&archive_path).expect("authenticate canonical archive");
        for role in CoreMember::ALL {
            assert_eq!(evidence.member_record(role).path, role.path());
        }
        assert_eq!(
            evidence.member(CoreMember::AlgorithmCodeSource),
            b"algorithm source\n"
        );
        assert!(
            CANONICAL_MEMBERS
                .windows(2)
                .any(|members| members[0] > members[1]),
            "fixture must remain producer-ordered rather than lexically sorted"
        );
    }

    #[test]
    fn archive_reader_rejects_zip64_entry_count_before_zip_construction() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("zip64-count.efmu");
        write_canonical_archive(&archive_path, b"algorithm source\n");
        let mut raw = fs::read(&archive_path).expect("read canonical archive");
        let end = raw.len().checked_sub(22).expect("canonical end record");
        assert_eq!(raw.get(end..end + 4), Some(b"PK\x05\x06".as_slice()));
        write_u16_at(&mut raw, end + 8, u16::MAX);
        write_u16_at(&mut raw, end + 10, u16::MAX);
        fs::write(&archive_path, raw).expect("write ZIP64-count mutation");

        let error = match read_archive(&archive_path) {
            Ok(_) => panic!("ZIP64 member count must reject before ZipArchive construction"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("65535/65535 members"),
            "unexpected ZIP64 count rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_rejects_local_and_central_name_disagreement() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("split-name.efmu");
        write_canonical_archive(&archive_path, b"algorithm source\n");
        let mut raw = fs::read(&archive_path).expect("read canonical archive");
        assert_eq!(raw.get(0..4), Some(LOCAL_HEADER.as_slice()));
        let local_name = raw.get_mut(30).expect("first local member name byte");
        *local_name = b'X';
        fs::write(&archive_path, raw).expect("write local-name mutation");

        let error = match read_archive(&archive_path) {
            Ok(_) => panic!("a central/local raw-name disagreement must reject"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("local raw name differs"),
            "unexpected split-name rejection: {error:#}"
        );
    }

    #[test]
    fn archive_reader_rejects_unicode_path_extra_fields() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("unicode-extra.efmu");
        write_canonical_archive(&archive_path, b"algorithm source\n");
        let mut raw = fs::read(&archive_path).expect("read canonical archive");
        let central = raw
            .windows(CENTRAL_HEADER.len())
            .position(|window| window == CENTRAL_HEADER)
            .expect("first central header");
        let name_length = usize::from(read_u16_at(&raw, central + 28));
        let extra_offset = central + 46 + name_length;
        let unicode_path_extra = [b'u', b'p', 5, 0, 1, 0, 0, 0, 0];
        raw.splice(extra_offset..extra_offset, unicode_path_extra);
        write_u16_at(
            &mut raw,
            central + 30,
            u16::try_from(unicode_path_extra.len()).expect("test extra length fits u16"),
        );
        let end = raw.len().checked_sub(22).expect("shifted end record");
        let central_size = read_u32_at(&raw, end + 12);
        write_u32_at(
            &mut raw,
            end + 12,
            central_size
                .checked_add(
                    u32::try_from(unicode_path_extra.len()).expect("test extra length fits u32"),
                )
                .expect("test central size remains bounded"),
        );
        fs::write(&archive_path, raw).expect("write Unicode-path-extra mutation");

        let error = match read_archive(&archive_path) {
            Ok(_) => panic!("a Unicode path extra field must reject before name normalization"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("central extra fields"),
            "unexpected Unicode-extra rejection: {error:#}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn archive_reader_refuses_a_symbolic_link_target() {
        use std::os::unix::fs::symlink;

        let temporary = tempfile::tempdir().expect("temporary archive directory");
        let archive_path = temporary.path().join("regular.efmu");
        let link_path = temporary.path().join("linked.efmu");
        write_canonical_archive(&archive_path, b"bounded first member\n");
        symlink(&archive_path, &link_path).expect("create archive symlink");

        assert!(
            read_archive(&link_path).is_err(),
            "archive authentication must never follow a symbolic link target"
        );
    }

    #[test]
    fn archive_reader_refuses_a_non_regular_target() {
        let temporary = tempfile::tempdir().expect("temporary archive directory");
        assert!(
            read_archive(temporary.path()).is_err(),
            "archive authentication must admit only an opened regular file"
        );
    }

    fn write_canonical_archive(path: &Path, first_member: &[u8]) {
        let file = fs::File::create(path).expect("create canonical archive");
        let mut archive = zip::ZipWriter::new(file);
        let options = zip::write::SimpleFileOptions::default()
            .compression_method(zip::CompressionMethod::Deflated)
            .last_modified_time(zip::DateTime::default())
            .unix_permissions(0o644);
        for member in CANONICAL_MEMBERS {
            archive
                .start_file(member, options)
                .expect("start canonical archive member");
            archive
                .write_all(if member == CANONICAL_MEMBERS[0] {
                    first_member
                } else {
                    b"bounded fixture\n"
                })
                .expect("write canonical archive member");
        }
        archive.finish().expect("finish canonical archive");
    }

    fn write_uniform_canonical_archive(path: &Path, member_bytes: &[u8]) {
        let file = fs::File::create(path).expect("create uniform canonical archive");
        let mut archive = zip::ZipWriter::new(file);
        let options = zip::write::SimpleFileOptions::default()
            .compression_method(zip::CompressionMethod::Deflated)
            .last_modified_time(zip::DateTime::default())
            .unix_permissions(0o644);
        for member in CANONICAL_MEMBERS {
            archive
                .start_file(member, options)
                .expect("start uniform canonical archive member");
            archive
                .write_all(member_bytes)
                .expect("write uniform canonical archive member");
        }
        archive.finish().expect("finish uniform canonical archive");
    }

    fn rewrite_declared_size(raw: &mut [u8], member_name: &str, declared_size: u32) {
        let mut local_headers = 0;
        let mut central_headers = 0;
        for offset in 0..raw.len().saturating_sub(4) {
            let (name_offset, name_length_offset, size_offset, count) =
                if raw[offset..].starts_with(LOCAL_HEADER) {
                    (30, 26, 22, &mut local_headers)
                } else if raw[offset..].starts_with(CENTRAL_HEADER) {
                    (46, 28, 24, &mut central_headers)
                } else {
                    continue;
                };
            let Some(name_length_bytes) = raw
                .get(offset + name_length_offset..offset + name_length_offset + size_of::<u16>())
            else {
                continue;
            };
            let name_length = u16::from_le_bytes(
                name_length_bytes
                    .try_into()
                    .expect("ZIP name-length field has fixed width"),
            ) as usize;
            let Some(name) = raw.get(offset + name_offset..offset + name_offset + name_length)
            else {
                continue;
            };
            if name != member_name.as_bytes() {
                continue;
            }
            raw[offset + size_offset..offset + size_offset + size_of::<u32>()]
                .copy_from_slice(&declared_size.to_le_bytes());
            *count += 1;
        }
        assert_eq!(local_headers, 1, "rewrite one local member header");
        assert_eq!(central_headers, 1, "rewrite one central member header");
    }

    fn read_u16_at(raw: &[u8], offset: usize) -> u16 {
        u16::from_le_bytes(
            raw.get(offset..offset + 2)
                .expect("test u16 field is in bounds")
                .try_into()
                .expect("test u16 field has fixed width"),
        )
    }

    fn read_u32_at(raw: &[u8], offset: usize) -> u32 {
        u32::from_le_bytes(
            raw.get(offset..offset + 4)
                .expect("test u32 field is in bounds")
                .try_into()
                .expect("test u32 field has fixed width"),
        )
    }

    fn write_u16_at(raw: &mut [u8], offset: usize, value: u16) {
        raw.get_mut(offset..offset + 2)
            .expect("test u16 field is in bounds")
            .copy_from_slice(&value.to_le_bytes());
    }

    fn write_u32_at(raw: &mut [u8], offset: usize, value: u32) {
        raw.get_mut(offset..offset + 4)
            .expect("test u32 field is in bounds")
            .copy_from_slice(&value.to_le_bytes());
    }
}
