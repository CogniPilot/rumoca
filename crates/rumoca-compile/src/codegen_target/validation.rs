use super::*;

pub(super) fn validate_target_manifest(manifest: &TargetManifest) -> Result<()> {
    if manifest.version != 1 {
        bail!(
            "Unsupported target manifest version {}; expected version 1",
            manifest.version
        );
    }
    if manifest.name.as_deref().is_some_and(str::is_empty) {
        bail!("target name must not be empty when present");
    }
    if manifest.description.as_deref().is_some_and(str::is_empty) {
        bail!("target description must not be empty when present");
    }
    if manifest
        .completion_message
        .as_deref()
        .is_some_and(str::is_empty)
    {
        bail!("target completion_message must not be empty when present");
    }
    if manifest
        .readiness_level
        .is_some_and(|readiness_level| readiness_level > 5)
    {
        bail!("target readiness_level must be between 0 and 5");
    }
    if manifest.files.is_empty() {
        bail!("target.toml must contain at least one file entry");
    }
    let requires_declared_capabilities =
        manifest.required_product().requires_declared_capabilities();
    if requires_declared_capabilities && manifest.capabilities.is_none() {
        bail!(
            "unsupported-feature:target-capabilities-undeclared: DAE-derived target manifest \
             must declare a [capabilities] table"
        );
    }
    if let Some(capabilities) = &manifest.capabilities {
        validate_target_capabilities(manifest, capabilities)?;
    }
    for file in &manifest.files {
        if manifest.package.is_some() && file.mode.is_some() {
            bail!(
                "[package] targets do not support per-file `mode` (file '{}'): the package writer owns the on-disk layout",
                file.path
            );
        }
    }
    validate_asset_bundles(&manifest.assets)?;
    validate_package(manifest.package.as_ref())?;
    Ok(())
}

/// Close the per-file GAL-043 compatibility relation before `TargetFile`
/// construction, so no unchecked file value can escape deserialization.
pub(super) fn validate_target_file_contract(
    artifact_kind: TargetArtifactKind,
    semantic_context: TargetSemanticContext,
    path: &str,
) -> Result<()> {
    if !artifact_kind.admits_context(semantic_context) {
        bail!(
            "[[files]] path '{}' declares artifact_kind = '{:?}' with forbidden semantic_context = '{}': 'galec' admits only Algorithm Code/XML artifacts and C-family artifacts require 'solve'",
            path,
            artifact_kind,
            semantic_context.as_str()
        );
    }
    let suffix = Path::new(path)
        .extension()
        .and_then(|extension| extension.to_str());
    if suffix != Some(artifact_kind.suffix()) {
        bail!(
            "[[files]] path '{}' must end in '.{}' for its declared artifact_kind = '{:?}'",
            path,
            artifact_kind.suffix(),
            artifact_kind
        );
    }
    Ok(())
}

pub(super) fn validate_product_member_roles(
    product: TargetRequiredProduct,
    files: &[TargetFile],
    assets: &[AssetBundle],
    package: Option<&TargetPackage>,
) -> Result<ValidatedTargetProductMembers> {
    if product == TargetRequiredProduct::AlgorithmCodePackage {
        return validate_algorithm_code_product_member_roles(files, assets, package);
    }
    if product != TargetRequiredProduct::SolveAlgorithmProduct {
        if files.iter().any(|file| file.product_role.is_some())
            || assets.iter().any(|asset| asset.product_role.is_some())
        {
            bail!(
                "product_role is reserved for a complete packaged AlgorithmCodePackage or correlated SolveAlgorithmProduct member layout"
            );
        }
        return Ok(ValidatedTargetProductMembers {
            render_members: files
                .iter()
                .map(|_| TargetPreparedMemberPlan::Direct)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        });
    }

    let mut counts = SolveAlgorithmMemberRoleCounts::empty();
    let mut render_members = Vec::with_capacity(files.len());
    for file in files {
        let role = file.product_role.ok_or_else(|| {
            anyhow::anyhow!(
                "SolveAlgorithmProduct file '{}' must declare one closed product_role",
                file.path
            )
        })?;
        let file_role = validate_file_product_role(file, role)?;
        if file.path.contains('{') || file.path.contains('}') {
            bail!(
                "SolveAlgorithmProduct member '{}' must be a static path; the complete member set is collision-proved during target construction",
                file.path
            );
        }
        counts.increment(role);
        render_members.push(solve_algorithm_prepared_member(file_role));
    }
    for role in [
        TargetProductMemberRole::PackageManifest,
        TargetProductMemberRole::AlgorithmCodeManifest,
        TargetProductMemberRole::AlgorithmCodeSource,
        TargetProductMemberRole::ProductionManifest,
        TargetProductMemberRole::ProductionHeader,
        TargetProductMemberRole::ProductionSource,
    ] {
        let count = counts.count(role);
        if count != 1 {
            bail!(
                "SolveAlgorithmProduct must declare exactly one product_role = '{}'; found {count}",
                product_member_role_name(role)
            );
        }
    }
    if counts.count(TargetProductMemberRole::Schema) != 0 {
        bail!("rendered [[files]] cannot declare product_role = 'schema'; schemas are assets");
    }
    if assets.is_empty() {
        bail!("SolveAlgorithmProduct must declare at least one schema asset bundle");
    }
    for asset in assets {
        let Some(role @ TargetProductMemberRole::Schema) = asset.product_role else {
            bail!(
                "every SolveAlgorithmProduct [[assets]] bundle must declare product_role = 'schema'"
            );
        };
        if asset.dest.contains('{') || asset.dest.contains('}') {
            bail!("SolveAlgorithmProduct schema asset destination must be static");
        }
        let _ = role;
    }
    Ok(ValidatedTargetProductMembers {
        render_members: render_members.into_boxed_slice(),
    })
}

/// Close the standalone Algorithm Code source/package split.
///
/// With no roles, every declaration must be `.alg` source and the generic
/// renderer can never observe XML. Once any package role appears, all three
/// rendered singleton roles and every schema asset are mandatory; no partial
/// package plan escapes construction.
pub(super) fn validate_algorithm_code_product_member_roles(
    files: &[TargetFile],
    assets: &[AssetBundle],
    package: Option<&TargetPackage>,
) -> Result<ValidatedTargetProductMembers> {
    let package_declared = files.iter().any(|file| file.product_role.is_some())
        || assets.iter().any(|asset| asset.product_role.is_some());
    if !package_declared {
        return validate_source_only_algorithm_code_members(files, assets, package);
    }
    validate_packaged_algorithm_code_members(files, assets, package)
}

fn validate_source_only_algorithm_code_members(
    files: &[TargetFile],
    assets: &[AssetBundle],
    package: Option<&TargetPackage>,
) -> Result<ValidatedTargetProductMembers> {
    if package.is_some() || !assets.is_empty() {
        bail!(
            "source-only AlgorithmCodePackage cannot declare [package] or [[assets]]; packaging requires one complete product_role family"
        );
    }
    if let Some(file) = files
        .iter()
        .find(|file| file.artifact_kind != TargetArtifactKind::AlgorithmCode)
    {
        bail!(
            "source-only AlgorithmCodePackage file '{}' must be artifact_kind = 'algorithm-code'; package XML requires one complete product_role family",
            file.path
        );
    }
    let render_members = files
        .iter()
        .map(source_only_algorithm_code_member)
        .collect::<Result<Vec<_>>>()?;
    Ok(ValidatedTargetProductMembers {
        render_members: render_members.into_boxed_slice(),
    })
}

fn source_only_algorithm_code_member(file: &TargetFile) -> Result<TargetPreparedMemberPlan> {
    let output_path_template =
        rumoca_phase_codegen::AlgorithmCodeSourceOutputPathTemplate::construct(
            file.path.clone().into_boxed_str(),
        )
        .map_err(|error| {
            anyhow::anyhow!(
                "construct source-only Algorithm Code output path for '{}': {error}",
                file.path,
            )
        })?;
    Ok(TargetPreparedMemberPlan::AlgorithmCodeSource {
        output_path_template,
    })
}

fn validate_packaged_algorithm_code_members(
    files: &[TargetFile],
    assets: &[AssetBundle],
    package: Option<&TargetPackage>,
) -> Result<ValidatedTargetProductMembers> {
    if package.is_none() {
        bail!("packaged AlgorithmCodePackage role family requires a [package] declaration");
    }

    let mut counts = AlgorithmCodeMemberRoleCounts::empty();
    let mut render_members = Vec::with_capacity(files.len());
    for file in files {
        let layout_role = validate_packaged_algorithm_code_file(file)?;
        counts.increment(layout_role);
        render_members.push(TargetPreparedMemberPlan::PackagedAlgorithmCode { role: layout_role });
    }
    validate_algorithm_code_role_counts(&counts)?;
    validate_algorithm_code_schema_assets(assets)?;
    Ok(ValidatedTargetProductMembers {
        render_members: render_members.into_boxed_slice(),
    })
}

fn validate_packaged_algorithm_code_file(
    file: &TargetFile,
) -> Result<rumoca_phase_codegen::AlgorithmCodeArtifactRole> {
    let role = file.product_role.ok_or_else(|| {
        anyhow::anyhow!(
            "packaged AlgorithmCodePackage file '{}' must declare one closed product_role",
            file.path
        )
    })?;
    let narrowed_role = match role {
        TargetProductMemberRole::PackageManifest => TargetAlgorithmCodeFileRole::PackageManifest,
        TargetProductMemberRole::AlgorithmCodeManifest => {
            TargetAlgorithmCodeFileRole::AlgorithmCodeManifest
        }
        TargetProductMemberRole::AlgorithmCodeSource => {
            TargetAlgorithmCodeFileRole::AlgorithmCodeSource
        }
        TargetProductMemberRole::ProductionManifest
        | TargetProductMemberRole::ProductionHeader
        | TargetProductMemberRole::ProductionSource
        | TargetProductMemberRole::Schema => {
            bail!(
                "packaged AlgorithmCodePackage cannot declare product_role = '{}'",
                product_member_role_name(role)
            );
        }
    };
    let layout_role = algorithm_code_artifact_role(narrowed_role);
    validate_file_product_role(file, role)?;
    if file.path.contains('{') || file.path.contains('}') {
        bail!(
            "packaged AlgorithmCodePackage member '{}' must be a static path",
            file.path
        );
    }
    Ok(layout_role)
}

fn validate_algorithm_code_role_counts(counts: &AlgorithmCodeMemberRoleCounts) -> Result<()> {
    for role in [
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest,
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource,
    ] {
        let count = counts.count(role);
        if count != 1 {
            bail!(
                "packaged AlgorithmCodePackage must declare exactly one product_role = '{}'; found {count}",
                algorithm_code_member_role_name(role)
            );
        }
    }
    Ok(())
}

fn validate_algorithm_code_schema_assets(assets: &[AssetBundle]) -> Result<()> {
    if assets.is_empty() {
        bail!("packaged AlgorithmCodePackage must declare at least one schema asset bundle");
    }
    for asset in assets {
        let Some(role @ TargetProductMemberRole::Schema) = asset.product_role else {
            bail!(
                "every packaged AlgorithmCodePackage [[assets]] bundle must declare product_role = 'schema'"
            );
        };
        if asset.dest.contains('{') || asset.dest.contains('}') {
            bail!("packaged AlgorithmCodePackage schema asset destination must be static");
        }
        let _ = role;
    }
    Ok(())
}

/// Closed cardinality witness for standalone packaged Algorithm Code roles.
pub(super) struct AlgorithmCodeMemberRoleCounts {
    package_manifest: usize,
    algorithm_code_manifest: usize,
    algorithm_code_source: usize,
}

impl AlgorithmCodeMemberRoleCounts {
    const fn empty() -> Self {
        Self {
            package_manifest: 0,
            algorithm_code_manifest: 0,
            algorithm_code_source: 0,
        }
    }

    fn increment(&mut self, role: rumoca_phase_codegen::AlgorithmCodeArtifactRole) {
        let count = match role {
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest => {
                &mut self.package_manifest
            }
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest => {
                &mut self.algorithm_code_manifest
            }
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource => {
                &mut self.algorithm_code_source
            }
        };
        *count += 1;
    }

    const fn count(&self, role: rumoca_phase_codegen::AlgorithmCodeArtifactRole) -> usize {
        match role {
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest => {
                self.package_manifest
            }
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest => {
                self.algorithm_code_manifest
            }
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource => {
                self.algorithm_code_source
            }
        }
    }
}

pub(super) const fn algorithm_code_member_role_name(
    role: rumoca_phase_codegen::AlgorithmCodeArtifactRole,
) -> &'static str {
    match role {
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest => "package-manifest",
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest => {
            "algorithm-code-manifest"
        }
        rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource => {
            "algorithm-code-source"
        }
    }
}

pub(super) const fn algorithm_code_artifact_role(
    role: TargetAlgorithmCodeFileRole,
) -> rumoca_phase_codegen::AlgorithmCodeArtifactRole {
    match role {
        TargetAlgorithmCodeFileRole::PackageManifest => {
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest
        }
        TargetAlgorithmCodeFileRole::AlgorithmCodeManifest => {
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest
        }
        TargetAlgorithmCodeFileRole::AlgorithmCodeSource => {
            rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource
        }
    }
}

/// Closed cardinality witness for the correlated product's member roles.
///
/// A map makes a missing closed-enum case look like an ordinary absent key and
/// requires a fallback value at every read. Keeping one field per role makes
/// additions fail to compile until both admission and cardinality checking are
/// updated deliberately.
pub(super) struct SolveAlgorithmMemberRoleCounts {
    package_manifest: usize,
    algorithm_code_manifest: usize,
    algorithm_code_source: usize,
    production_manifest: usize,
    production_header: usize,
    production_source: usize,
    schema: usize,
}

impl SolveAlgorithmMemberRoleCounts {
    const fn empty() -> Self {
        Self {
            package_manifest: 0,
            algorithm_code_manifest: 0,
            algorithm_code_source: 0,
            production_manifest: 0,
            production_header: 0,
            production_source: 0,
            schema: 0,
        }
    }

    fn increment(&mut self, role: TargetProductMemberRole) {
        let count = match role {
            TargetProductMemberRole::PackageManifest => &mut self.package_manifest,
            TargetProductMemberRole::AlgorithmCodeManifest => &mut self.algorithm_code_manifest,
            TargetProductMemberRole::AlgorithmCodeSource => &mut self.algorithm_code_source,
            TargetProductMemberRole::ProductionManifest => &mut self.production_manifest,
            TargetProductMemberRole::ProductionHeader => &mut self.production_header,
            TargetProductMemberRole::ProductionSource => &mut self.production_source,
            TargetProductMemberRole::Schema => &mut self.schema,
        };
        *count += 1;
    }

    const fn count(&self, role: TargetProductMemberRole) -> usize {
        match role {
            TargetProductMemberRole::PackageManifest => self.package_manifest,
            TargetProductMemberRole::AlgorithmCodeManifest => self.algorithm_code_manifest,
            TargetProductMemberRole::AlgorithmCodeSource => self.algorithm_code_source,
            TargetProductMemberRole::ProductionManifest => self.production_manifest,
            TargetProductMemberRole::ProductionHeader => self.production_header,
            TargetProductMemberRole::ProductionSource => self.production_source,
            TargetProductMemberRole::Schema => self.schema,
        }
    }
}

pub(super) fn validate_file_product_role(
    file: &TargetFile,
    role: TargetProductMemberRole,
) -> Result<TargetProductFileRole> {
    let checked_role = match role {
        TargetProductMemberRole::PackageManifest => (file.artifact_kind == TargetArtifactKind::Xml
            && file.semantic_view == TargetSemanticView::AlgorithmCodePackage)
            .then_some(TargetProductFileRole::PackageManifest),
        TargetProductMemberRole::AlgorithmCodeManifest => (file.artifact_kind
            == TargetArtifactKind::Xml
            && file.semantic_view == TargetSemanticView::AlgorithmCodePackage)
            .then_some(TargetProductFileRole::AlgorithmCodeManifest),
        TargetProductMemberRole::AlgorithmCodeSource => (file.artifact_kind
            == TargetArtifactKind::AlgorithmCode
            && file.semantic_view == TargetSemanticView::AlgorithmCodePackage)
            .then_some(TargetProductFileRole::AlgorithmCodeSource),
        TargetProductMemberRole::ProductionManifest => (file.artifact_kind
            == TargetArtifactKind::Xml
            && file.semantic_view == TargetSemanticView::SolveAlgorithmBlock)
            .then_some(TargetProductFileRole::ProductionManifest),
        TargetProductMemberRole::ProductionHeader => (file.artifact_kind
            == TargetArtifactKind::CHeader
            && file.semantic_view == TargetSemanticView::SolveAlgorithmBlock)
            .then_some(TargetProductFileRole::ProductionHeader),
        TargetProductMemberRole::ProductionSource => (file.artifact_kind
            == TargetArtifactKind::CSource
            && file.semantic_view == TargetSemanticView::SolveAlgorithmBlock)
            .then_some(TargetProductFileRole::ProductionSource),
        TargetProductMemberRole::Schema => None,
    };
    checked_role.ok_or_else(|| {
        anyhow::anyhow!(
            "[[files]] path '{}' declares product_role = '{}' with an incompatible artifact_kind/view",
            file.path,
            product_member_role_name(role)
        )
    })
}

pub(super) const fn solve_algorithm_prepared_member(
    role: TargetProductFileRole,
) -> TargetPreparedMemberPlan {
    match role {
        TargetProductFileRole::PackageManifest => {
            TargetPreparedMemberPlan::CorrelatedAlgorithmCode {
                role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::PackageManifest,
            }
        }
        TargetProductFileRole::AlgorithmCodeManifest => {
            TargetPreparedMemberPlan::CorrelatedAlgorithmCode {
                role:
                    rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
            }
        }
        TargetProductFileRole::AlgorithmCodeSource => {
            TargetPreparedMemberPlan::CorrelatedAlgorithmCode {
                role:
                    rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
            }
        }
        TargetProductFileRole::ProductionManifest => TargetPreparedMemberPlan::ProductionCode {
            role: rumoca_phase_codegen::ProductionCodeFileRole::Manifest,
        },
        TargetProductFileRole::ProductionHeader => TargetPreparedMemberPlan::ProductionCode {
            role: rumoca_phase_codegen::ProductionCodeFileRole::Header,
        },
        TargetProductFileRole::ProductionSource => TargetPreparedMemberPlan::ProductionCode {
            role: rumoca_phase_codegen::ProductionCodeFileRole::Source,
        },
    }
}

pub(super) const fn product_member_role_name(role: TargetProductMemberRole) -> &'static str {
    match role {
        TargetProductMemberRole::PackageManifest => "package-manifest",
        TargetProductMemberRole::AlgorithmCodeManifest => "algorithm-code-manifest",
        TargetProductMemberRole::AlgorithmCodeSource => "algorithm-code-source",
        TargetProductMemberRole::ProductionManifest => "production-manifest",
        TargetProductMemberRole::ProductionHeader => "production-header",
        TargetProductMemberRole::ProductionSource => "production-source",
        TargetProductMemberRole::Schema => "schema",
    }
}

pub(super) fn validate_package(package: Option<&TargetPackage>) -> Result<()> {
    let Some(package) = package else {
        return Ok(());
    };
    if package.root.trim().is_empty() {
        bail!("[package] root must not be empty");
    }
    if package.members.is_empty() {
        bail!(
            "[package] must declare a nonempty [[package.members]] sequence containing every rendered file and expanded asset member exactly once"
        );
    }
    for required in &package.required_files {
        if required.trim().is_empty() {
            bail!("[package] required_files entries must not be empty");
        }
    }
    if let Some(archive) = &package.archive
        && archive.path.trim().is_empty()
    {
        bail!("[package.archive] path must not be empty");
    }
    Ok(())
}

/// Resolve the sole target-issued render authority once.
///
/// A packaged target retains one mixed `[[package.members]]` sequence. Every
/// checksum edge is a capability pointing to an earlier position in that
/// exact mixed sequence, so no file-only order or later asset append exists.
pub(super) fn construct_render_authority(
    files: &[TargetFile],
    render_members: Box<[TargetPreparedMemberPlan]>,
    package: Option<&TargetPackage>,
) -> Result<TargetDeclaredRenderAuthority> {
    let pending_files = pair_declared_files_with_product_members(files, render_members)?;
    let pending = match package {
        None => PendingTargetRenderAuthority::Unpackaged(pending_files.into_boxed_slice()),
        Some(package) => close_packaged_render_authority(package, pending_files)?,
    };
    close_checksum_render_authority(pending)
}

fn close_packaged_render_authority(
    package: &TargetPackage,
    mut remaining: Vec<PendingTargetRenderStep>,
) -> Result<PendingTargetRenderAuthority> {
    validate_packaged_render_files(&remaining)?;
    let mut admitted_files = BTreeSet::new();
    let mut members = Vec::with_capacity(package.members.len());
    for declared in &package.members {
        members.push(close_declared_package_member(
            declared,
            &mut admitted_files,
            &mut remaining,
        )?);
    }
    validate_no_render_files_omitted(&remaining)?;
    Ok(PendingTargetRenderAuthority::Packaged(
        members.into_boxed_slice(),
    ))
}

fn validate_packaged_render_files(files: &[PendingTargetRenderStep]) -> Result<()> {
    for file in files {
        if file.file.path.contains('{') || file.file.path.contains('}') {
            bail!(
                "packaged [[files]] member '{}' must have one static portable path; interpolation is allowed only in package root/archive policy",
                file.file.path
            );
        }
        if file.file_id.is_none() {
            bail!(
                "packaged [[files]] member '{}' must declare an id referenced exactly once by [[package.members]]",
                file.file.path
            );
        }
    }
    Ok(())
}

fn close_declared_package_member<'a>(
    declared: &'a TargetPackageMember,
    admitted_files: &mut BTreeSet<&'a str>,
    remaining: &mut Vec<PendingTargetRenderStep>,
) -> Result<PendingTargetPackageMemberPlan> {
    match declared {
        TargetPackageMember::File { file } => {
            if !admitted_files.insert(file.as_str()) {
                bail!(
                    "[[package.members]] includes [[files]].id '{file}' more than once; every rendered file must occur exactly once"
                );
            }
            let position = remaining
                .iter()
                .position(|pending| pending.file_id.as_deref() == Some(file.as_str()))
                .with_context(|| {
                    format!(
                        "[[package.members]] kind = 'file' references unknown [[files]].id '{file}'"
                    )
                })?;
            Ok(PendingTargetPackageMemberPlan::File(
                remaining.remove(position),
            ))
        }
        TargetPackageMember::Asset { source, path } => Ok(PendingTargetPackageMemberPlan::Asset {
            source: source.clone(),
            relative_path: path.clone(),
        }),
    }
}

fn validate_no_render_files_omitted(remaining: &[PendingTargetRenderStep]) -> Result<()> {
    if remaining.is_empty() {
        return Ok(());
    }
    let missing = remaining
        .iter()
        .map(|file| format!("path '{}'", file.file.path))
        .collect::<Vec<_>>();
    bail!(
        "[[package.members]] omits rendered files {}; every rendered file must occur exactly once",
        missing.join(", ")
    )
}

pub(super) fn pair_declared_files_with_product_members(
    files: &[TargetFile],
    render_members: Box<[TargetPreparedMemberPlan]>,
) -> Result<Vec<PendingTargetRenderStep>> {
    let mut members = render_members.into_vec().into_iter();
    let mut pending = Vec::with_capacity(files.len());
    for (position, file) in files.iter().enumerate() {
        let prepared_member = members.next().with_context(|| {
            format!(
                "target product construction omitted file declaration {} path '{}'",
                position + 1,
                file.path
            )
        })?;
        pending.push(PendingTargetRenderStep {
            file: TargetDeclaredFileSpec::from_file(file),
            file_id: file.id.as_deref().map(Box::<str>::from),
            checksum_needs: file.checksums.clone().into_boxed_slice(),
            prepared_member,
        });
    }
    if members.next().is_some() {
        bail!("target product construction issued more member roles than file declarations");
    }
    Ok(pending)
}

#[derive(Clone)]
pub(super) struct ChecksumProducerPlan {
    result: TargetFileResultId,
    position: usize,
    role_name: &'static str,
    path: Box<str>,
}

pub(super) fn close_checksum_render_authority(
    pending: PendingTargetRenderAuthority,
) -> Result<TargetDeclaredRenderAuthority> {
    let mut producers = BTreeMap::<Box<str>, ChecksumProducerPlan>::new();
    let mut file_result_position = 0;
    match &pending {
        PendingTargetRenderAuthority::Unpackaged(steps) => {
            for (position, step) in steps.iter().enumerate() {
                admit_checksum_producer(
                    &mut producers,
                    step,
                    position,
                    TargetFileResultId(file_result_position),
                )?;
                file_result_position += 1;
            }
        }
        PendingTargetRenderAuthority::Packaged(members) => {
            for (position, member) in members.iter().enumerate() {
                if let PendingTargetPackageMemberPlan::File(step) = member {
                    admit_checksum_producer(
                        &mut producers,
                        step,
                        position,
                        TargetFileResultId(file_result_position),
                    )?;
                    file_result_position += 1;
                }
            }
        }
    }
    match pending {
        PendingTargetRenderAuthority::Unpackaged(steps) => steps
            .into_vec()
            .into_iter()
            .enumerate()
            .map(|(position, step)| close_checksum_step(step, position, &producers))
            .collect::<Result<Vec<_>>>()
            .map(Vec::into_boxed_slice)
            .map(TargetDeclaredRenderAuthority::Unpackaged),
        PendingTargetRenderAuthority::Packaged(members) => members
            .into_vec()
            .into_iter()
            .enumerate()
            .map(|(position, member)| match member {
                PendingTargetPackageMemberPlan::File(step) => {
                    close_checksum_step(step, position, &producers)
                        .map(TargetDeclaredPackageMemberPlan::File)
                }
                PendingTargetPackageMemberPlan::Asset {
                    source,
                    relative_path,
                } => Ok(TargetDeclaredPackageMemberPlan::Asset {
                    source,
                    relative_path,
                }),
            })
            .collect::<Result<Vec<_>>>()
            .map(Vec::into_boxed_slice)
            .map(TargetDeclaredRenderAuthority::Packaged),
    }
}

pub(super) fn admit_checksum_producer(
    producers: &mut BTreeMap<Box<str>, ChecksumProducerPlan>,
    step: &PendingTargetRenderStep,
    position: usize,
    result: TargetFileResultId,
) -> Result<()> {
    let Some(file_id) = step.file_id.as_deref() else {
        return Ok(());
    };
    let plan = ChecksumProducerPlan {
        result,
        position,
        role_name: step.file.role_name,
        path: step.file.path.clone(),
    };
    if producers.insert(Box::from(file_id), plan).is_some() {
        bail!("duplicate [[files]] id '{file_id}' (ids must be unique per target)");
    }
    Ok(())
}

pub(super) fn close_checksum_step(
    step: PendingTargetRenderStep,
    consumer_position: usize,
    producers: &BTreeMap<Box<str>, ChecksumProducerPlan>,
) -> Result<TargetDeclaredRenderPlanStep> {
    let mut as_keys = BTreeSet::new();
    let mut incoming_checksums = Vec::with_capacity(step.checksum_needs.len());
    for need in step.checksum_needs {
        if need.as_key.trim().is_empty() {
            bail!(
                "[[files.checksums]] `as` must not be empty (file '{}', of = '{}')",
                step.file.path,
                need.of
            );
        }
        if !as_keys.insert(need.as_key.clone()) {
            bail!(
                "[[files.checksums]] `as` = '{}' is declared twice on file '{}'; each `as` key names one distinct injected checksum",
                need.as_key,
                step.file.path
            );
        }
        let producer = producers.get(need.of.as_str()).with_context(|| {
            format!(
                "[[files.checksums]] of = '{}' on file '{}' names no [[files]] id (declare `id = \"{}\"` on the producer file)",
                need.of, step.file.path, need.of
            )
        })?;
        if producer.position >= consumer_position {
            bail!(
                "[[files.checksums]] edge violates strict producer-before-consumer target-issued order: consumer position {} role = '{}' path '{}' references producer position {} role = '{}' path '{}' through of = '{}'; place the producer before the consumer",
                consumer_position + 1,
                step.file.role_name,
                step.file.path,
                producer.position + 1,
                producer.role_name,
                producer.path,
                need.of,
            );
        }
        incoming_checksums.push(match need.algorithm {
            ChecksumAlgorithm::Sha1 => TargetResolvedChecksumBinding::Sha1 {
                producer: producer.result,
                as_key: need.as_key,
            },
        });
    }
    Ok(TargetDeclaredRenderPlanStep {
        file: step.file,
        incoming_checksums,
        prepared_member: step.prepared_member,
    })
}

pub(super) fn construct_artifact_identity_catalog(
    files: &[TargetFileDraft],
) -> Result<Box<[String]>> {
    let mut ids = BTreeSet::new();
    for file in files {
        if let Some(id) = &file.id {
            if super::artifact_identity_name::artifact_identity_template_name(id).is_none() {
                bail!(
                    "[[files]] id '{id}' must use the exact Jinja dot-addressable ASCII key grammar [a-z_][a-z0-9_]* (path '{}')",
                    file.path
                );
            }
            if !ids.insert(id.as_str()) {
                bail!("duplicate [[files]] id '{id}' (ids must be unique per target)");
            }
        }
    }
    Ok(ids
        .iter()
        .map(|identity| (*identity).to_owned())
        .collect::<Vec<_>>()
        .into_boxed_slice())
}

pub(super) fn target_file_role_name(file: &TargetFile) -> &'static str {
    match file.product_role {
        Some(role) => product_member_role_name(role),
        None => file.semantic_view.as_str(),
    }
}

/// Fail-early checks on declared target-relative asset trees.
pub(super) fn validate_asset_bundles(assets: &[AssetBundle]) -> Result<()> {
    for asset in assets {
        if asset.source.trim().is_empty() {
            bail!("[[assets]] source must not be empty");
        }
        if asset.dest.trim().is_empty() {
            bail!(
                "[[assets]] dest must not be empty (source '{}')",
                asset.source
            );
        }
        if asset
            .shared_from
            .as_deref()
            .is_some_and(|owner| owner.trim().is_empty())
        {
            bail!(
                "[[assets]] shared_from must name the owning target when present (source '{}')",
                asset.source
            );
        }
    }
    Ok(())
}

pub(super) fn ensure_target_has_rendered_files(manifest: &TargetManifest) -> Result<()> {
    if manifest.files.is_empty() {
        bail!(
            "target '{}' is manifest-only and does not define generated files yet",
            manifest.name.as_deref().unwrap_or("custom")
        );
    }
    Ok(())
}

pub(super) fn validate_target_capabilities(
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    if capabilities.structured_equation_families.is_some()
        && manifest.required_product() != TargetRequiredProduct::Dae
    {
        bail!(
            "structured_equation_families capability is only valid for targets requiring the checked Dae product"
        );
    }
    if capabilities.exact_algebraic_assignments.is_some()
        && !manifest
            .required_product()
            .admits_exact_algebraic_assignment_capability()
    {
        bail!("exact_algebraic_assignments capability is only valid for Solve-derived targets");
    }
    if capabilities.tensor.is_some() && !manifest.required_product().carries_solve_tensor_program()
    {
        bail!("tensor capabilities are only valid for Solve-derived targets");
    }
    if capabilities.scalar_fallback && !manifest.required_product().carries_solve_tensor_program() {
        bail!("scalar_fallback is only valid for targets requiring a Solve-derived product");
    }
    if !capabilities.scalar_fallback {
        let Some(tensor) = &capabilities.tensor else {
            return Ok(());
        };
        let scalar_tensor_ops = [
            ("tensor.matmul", tensor.matmul),
            ("tensor.linsolve", tensor.linsolve),
            ("tensor.elementwise", tensor.elementwise),
            ("tensor.stencil", tensor.stencil),
            ("tensor.reductions", tensor.reductions),
        ]
        .into_iter()
        .filter_map(|(name, mode)| (mode == Some(TensorCapability::Scalar)).then_some(name))
        .collect::<Vec<_>>();
        if !scalar_tensor_ops.is_empty() {
            bail!(
                "target.toml sets scalar_fallback = false but marks {} as scalar",
                scalar_tensor_ops.join(", ")
            );
        }
    }
    if let Some(tensor) = &capabilities.tensor
        && tensor
            .dtypes
            .as_ref()
            .is_some_and(|dtypes| dtypes.iter().any(|dtype| dtype.trim().is_empty()))
    {
        bail!("target tensor dtypes must not contain empty entries");
    }
    Ok(())
}

pub(super) fn unsupported_feature(
    target: CapabilityTarget<'_>,
    feature: &'static str,
    detail: impl std::fmt::Display,
) -> Result<()> {
    unsupported_feature_at(target, feature, detail, None)
}

pub(super) fn unsupported_feature_at(
    target: CapabilityTarget<'_>,
    feature: &'static str,
    detail: impl std::fmt::Display,
    span: Option<rumoca_core::Span>,
) -> Result<()> {
    let mut detail = detail.to_string();
    if target.names_galec_projection() {
        detail.push_str("; not yet supported by the Rumoca GALEC projection");
    }
    Err(
        rumoca_phase_codegen::CodegenError::unsupported_target_feature(
            target.label(),
            feature,
            detail,
            span,
        )
        .into(),
    )
}
