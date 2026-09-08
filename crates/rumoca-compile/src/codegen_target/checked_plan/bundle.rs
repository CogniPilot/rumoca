use super::*;

impl TargetBundle {
    /// Check one complete in-memory target input in the same construction path
    /// as built-in and directory targets. Exact template and asset inventories
    /// are consumed together; missing and foreign entries are refused.
    pub fn check_in_memory(
        label: String,
        manifest: String,
        templates: BTreeMap<String, String>,
        assets: BTreeMap<String, BTreeMap<String, Vec<u8>>>,
    ) -> Result<CheckedTargetBundle> {
        Self {
            source: TargetBundleSource::InMemory {
                label,
                manifest,
                templates,
                assets,
            },
        }
        .check()
    }

    pub fn load(target: &str) -> Result<Self> {
        if let Some(bundle) = Self::builtin(target) {
            return Ok(bundle);
        }

        let dir = PathBuf::from(target);
        let manifest_path = dir.join("target.toml");
        let manifest = read_utf8_regular_file_bounded(&manifest_path).with_context(|| {
            format!(
                "Read target manifest for '{}' at {}",
                target,
                manifest_path.display()
            )
        })?;
        Ok(Self {
            source: TargetBundleSource::Directory { dir, manifest },
        })
    }

    pub fn builtin(target: &str) -> Option<Self> {
        templates::builtin_target(target).map(|target| Self {
            source: TargetBundleSource::Builtin { target },
        })
    }

    /// Parse the manifest and snapshot every exact template and asset byte.
    /// No filesystem path remains in the checked authority.
    pub fn check(self) -> Result<CheckedTargetBundle> {
        self.check_with_descriptor_facts()
            .map(|(checked, _descriptor)| checked)
    }

    pub(in crate::codegen_target) fn check_with_descriptor_facts(
        self,
    ) -> Result<(CheckedTargetBundle, TargetDescriptorFacts)> {
        let mut snapshot_budget = TargetSnapshotBudget::new();
        snapshot_budget.admit("target.toml", self.manifest_source().len())?;
        let TargetManifestConstruction {
            mut manifest,
            render_authority,
        } = self.parse_manifest_construction()?;
        ensure_target_has_rendered_files(&manifest)?;
        let label: Box<str> = self.checked_label(&manifest)?.into();
        let descriptor = TargetDescriptorFacts {
            label: label.to_string(),
            description: manifest.description.clone(),
            required_product: manifest.required_product,
            capabilities: manifest.capabilities.clone(),
            execution_mode: manifest.execution_mode.clone(),
            deployment_class: manifest.deployment_class.clone(),
            readiness_level: manifest.readiness_level,
            file_plans: manifest
                .files
                .iter()
                .map(|file| TargetFileDescriptor {
                    path: file.path.clone(),
                    semantic_context: file.semantic_context,
                    semantic_view: file.semantic_view,
                })
                .collect(),
        };
        let artifact_identity_scope = self.construct_artifact_identity_scope(&manifest)?;
        self.validate_in_memory_inventory(&manifest)?;
        let render_authority =
            self.snapshot_render_authority(render_authority, &mut snapshot_budget)?;
        let assets = self.snapshot_assets(&manifest, &mut snapshot_budget)?;
        let arithmetic = manifest.algorithm_code_arithmetic();
        let production_profile = manifest.solve_algorithm_production_profile();
        let capability_contract =
            manifest
                .capabilities
                .take()
                .map(|capabilities| CheckedTargetCapabilityContract {
                    label: label.clone(),
                    required_product: manifest.required_product,
                    capabilities,
                });
        let render_authority =
            close_checked_render_authority(CloseCheckedRenderAuthorityRequest {
                authority: render_authority,
                assets: &assets,
                package: manifest.package.as_ref(),
                product: manifest.required_product,
                arithmetic,
                production_profile,
                capability_contract,
            })?;
        let metadata = CheckedTargetMetadata {
            description: manifest.description.take().map(Into::into),
            completion_message: manifest.completion_message.take().map(Into::into),
            artifact_identity_scope,
            artifact_identity_keys: std::mem::take(&mut manifest.artifact_identity_keys),
            label,
        };
        Ok((
            CheckedTargetBundle {
                metadata,
                render_authority,
            },
            descriptor,
        ))
    }

    fn parse_manifest_construction(&self) -> Result<TargetManifestConstruction> {
        let construction = match &self.source {
            TargetBundleSource::Builtin { target } => {
                parse_target_manifest_construction(target.manifest)
            }
            TargetBundleSource::Directory { manifest, .. } => {
                parse_target_manifest_construction(manifest)
            }
            TargetBundleSource::InMemory { manifest, .. } => {
                parse_target_manifest_construction(manifest)
            }
        }?;
        if let TargetBundleSource::Directory { dir, .. } = &self.source {
            validate_borrowed_template_ownership(dir, &construction.manifest)?;
        }
        Ok(construction)
    }

    fn manifest_source(&self) -> &str {
        match &self.source {
            TargetBundleSource::Builtin { target } => target.manifest,
            TargetBundleSource::Directory { manifest, .. }
            | TargetBundleSource::InMemory { manifest, .. } => manifest,
        }
    }

    /// Issue the stable identity scope for one already-checked manifest.
    /// Presentation labels and source formatting cannot enter this value.
    pub(super) fn construct_artifact_identity_scope(
        &self,
        manifest: &TargetManifest,
    ) -> Result<TargetArtifactIdentityScope> {
        match &self.source {
            TargetBundleSource::Builtin { target } => Ok(TargetArtifactIdentityScope {
                kind: TargetArtifactIdentityScopeKind::BuiltinRegistryKey,
                value: target.name.into(),
            }),
            TargetBundleSource::Directory { .. } | TargetBundleSource::InMemory { .. } => {
                Ok(TargetArtifactIdentityScope {
                    kind: TargetArtifactIdentityScopeKind::CanonicalManifestDigest,
                    value: manifest.canonical_artifact_identity_digest()?.into(),
                })
            }
        }
    }

    fn checked_label<'a>(&'a self, manifest: &'a TargetManifest) -> Result<&'a str> {
        if let Some(name) = manifest.name.as_deref() {
            return Ok(name);
        }
        match &self.source {
            TargetBundleSource::Builtin { target } => Ok(target.name),
            TargetBundleSource::Directory { dir, .. } => dir.to_str().with_context(|| {
                format!(
                    "target directory without an explicit name must be valid UTF-8: '{}'",
                    dir.display()
                )
            }),
            TargetBundleSource::InMemory { label, .. } => {
                if label.is_empty() || label.chars().any(char::is_control) {
                    bail!(
                        "in-memory target label must be nonempty and contain no control characters"
                    );
                }
                Ok(label)
            }
        }
    }

    fn validate_in_memory_inventory(&self, manifest: &TargetManifest) -> Result<()> {
        let TargetBundleSource::InMemory {
            templates, assets, ..
        } = &self.source
        else {
            return Ok(());
        };

        let expected_templates = manifest
            .files
            .iter()
            .filter(|file| file.template_shared_from().is_none())
            .map(|file| file.template.as_str())
            .collect::<BTreeSet<_>>();
        let supplied_templates = templates
            .keys()
            .map(String::as_str)
            .collect::<BTreeSet<_>>();
        if expected_templates != supplied_templates {
            let missing = expected_templates
                .difference(&supplied_templates)
                .copied()
                .collect::<Vec<_>>();
            let foreign = supplied_templates
                .difference(&expected_templates)
                .copied()
                .collect::<Vec<_>>();
            bail!(
                "in-memory target template inventory is not exact; missing [{}], foreign [{}]",
                missing.join(", "),
                foreign.join(", ")
            );
        }

        let expected_assets = manifest
            .assets
            .iter()
            .filter(|bundle| bundle.shared_from.is_none())
            .map(|bundle| bundle.source.as_str())
            .collect::<BTreeSet<_>>();
        let supplied_assets = assets.keys().map(String::as_str).collect::<BTreeSet<_>>();
        if expected_assets != supplied_assets {
            let missing = expected_assets
                .difference(&supplied_assets)
                .copied()
                .collect::<Vec<_>>();
            let foreign = supplied_assets
                .difference(&expected_assets)
                .copied()
                .collect::<Vec<_>>();
            bail!(
                "in-memory target asset inventory is not exact; missing [{}], foreign [{}]",
                missing.join(", "),
                foreign.join(", ")
            );
        }
        Ok(())
    }

    fn snapshot_render_authority(
        &self,
        authority: TargetDeclaredRenderAuthority,
        budget: &mut TargetSnapshotBudget,
    ) -> Result<TargetSnapshottedRenderAuthority> {
        let mut snapshot = |step: TargetDeclaredRenderPlanStep| {
            let source = target_template_source_for(self, &step.file).with_context(|| {
                format!(
                    "Snapshot checked target template '{}' for output '{}'",
                    step.file.template, step.file.path
                )
            })?;
            budget.admit(&step.file.template, source.len())?;
            validate_artifact_identity_dependencies(&step.file, &source)?;
            Ok(TargetRenderPlanStep {
                file: TargetSnapshottedFile {
                    declaration: step.file,
                    template_body: source.into_owned().into_boxed_str(),
                },
                incoming_checksums: step.incoming_checksums,
                prepared_member: step.prepared_member,
            })
        };
        match authority {
            TargetDeclaredRenderAuthority::Unpackaged(steps) => steps
                .into_vec()
                .into_iter()
                .map(&mut snapshot)
                .collect::<Result<Vec<_>>>()
                .map(Vec::into_boxed_slice)
                .map(TargetSnapshottedRenderAuthority::Unpackaged),
            TargetDeclaredRenderAuthority::Packaged(members) => members
                .into_vec()
                .into_iter()
                .map(|member| match member {
                    TargetDeclaredPackageMemberPlan::File(step) => {
                        snapshot(step).map(TargetSnapshottedPackageMemberPlan::File)
                    }
                    TargetDeclaredPackageMemberPlan::Asset {
                        source,
                        relative_path,
                    } => Ok(TargetSnapshottedPackageMemberPlan::Asset {
                        source,
                        relative_path,
                    }),
                })
                .collect::<Result<Vec<_>>>()
                .map(Vec::into_boxed_slice)
                .map(TargetSnapshottedRenderAuthority::Packaged),
        }
    }

    fn snapshot_assets(
        &self,
        manifest: &TargetManifest,
        budget: &mut TargetSnapshotBudget,
    ) -> Result<Box<[CheckedTargetAssetBundle]>> {
        manifest
            .assets
            .iter()
            .map(|bundle| {
                let members = self
                    .snapshot_asset_files(bundle)
                    .with_context(|| format!("Snapshot target asset source '{}'", bundle.source))?
                    .into_iter()
                    .map(|member| {
                        budget.admit(&member.relative_path, member.bytes.len())?;
                        Ok(CheckedTargetAssetMember {
                            relative_path: Arc::from(member.relative_path),
                            bytes: Arc::from(member.bytes),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?
                    .into_boxed_slice();
                Ok(CheckedTargetAssetBundle {
                    source: Arc::from(bundle.source.as_str()),
                    destination: bundle.dest.clone().into_boxed_str(),
                    members,
                })
            })
            .collect::<Result<Vec<_>>>()
            .map(Vec::into_boxed_slice)
    }

    /// Read one declared `[[assets]]` bundle's files.
    ///
    /// A bundle that declares `shared_from` names the built-in target that
    /// owns its bytes, and is read from that target either way: a built-in
    /// borrower already carries the owner's files, because the codegen build
    /// script grafts them into its bundle, and a directory target reads them
    /// straight out of the owner's embedded bundle. A target directory copied
    /// out of the tree therefore emits the bytes the built-in it came from
    /// emits. Without that, a borrowing target would work as a built-in and
    /// fail as a directory, which is the shape `--target <dir>` documents.
    fn snapshot_asset_files(&self, bundle: &AssetBundle) -> Result<Vec<TargetAssetFile>> {
        let source = bundle.source.as_str();
        if let Some(owner) = bundle.shared_from.as_deref()
            && !matches!(self.source, TargetBundleSource::Builtin { .. })
        {
            return borrowed_asset_files(owner, source).and_then(close_asset_member_order);
        }
        match &self.source {
            TargetBundleSource::Builtin { target } => target
                .asset_files(source)
                .map(|files| {
                    files
                        .into_iter()
                        .map(|(relative_path, bytes)| TargetAssetFile {
                            relative_path: relative_path.to_owned(),
                            bytes: bytes.to_vec(),
                        })
                        .collect::<Vec<_>>()
                })
                .with_context(|| {
                    format!(
                        "Built-in target '{}' contains no asset source '{source}'",
                        target.name
                    )
                })
                .and_then(close_asset_member_order),
            TargetBundleSource::Directory { dir, .. } => {
                let root = safe_target_join(dir, source)?;
                collect_target_assets(&root)
            }
            TargetBundleSource::InMemory { assets, .. } => assets
                .get(source)
                .with_context(|| {
                    format!("In-memory target contains no exact asset source '{source}'")
                })
                .map(|members| {
                    members
                        .iter()
                        .map(|(relative_path, bytes)| TargetAssetFile {
                            relative_path: relative_path.clone(),
                            bytes: bytes.clone(),
                        })
                        .collect::<Vec<_>>()
                })
                .and_then(close_asset_member_order),
        }
    }
}

fn validate_artifact_identity_dependencies(
    file: &TargetDeclaredFileSpec,
    source: &str,
) -> Result<()> {
    let environment = rumoca_phase_codegen::target_template_environment();
    let template = environment.template_from_str(source).with_context(|| {
        format!(
            "Compile snapshotted target template '{}' for artifact identity analysis",
            file.template
        )
    })?;

    // This consumes MiniJinja's own AST analysis after the exact snapshotted
    // bytes compiled successfully; it is neither a second text reader nor
    // dependency inference. `multi_template` is disabled workspace-wide, so
    // composition syntax cannot exist in the successfully compiled grammar.
    // Identity values are flattened reserved scalars, not a map: analysis
    // compares the visible scalar names, while the render context's physical
    // absence prevents self-shadow ordering from exposing undeclared values.
    let mut referenced = BTreeSet::new();
    for variable in template.undeclared_variables(true) {
        if variable == "artifact"
            || variable == "artifact.identities"
            || variable.starts_with("artifact.identities.")
        {
            bail!(
                "target template '{}' uses the removed artifact.identities map; use one exact flattened artifact identity scalar",
                file.template
            );
        }
        if !super::super::artifact_identity_name::is_artifact_identity_template_namespace(&variable)
        {
            continue;
        }
        if super::super::artifact_identity_name::artifact_identity_template_key(&variable).is_none()
        {
            bail!(
                "target template '{}' artifact identity reference '{}' is not an exact flattened canonical identity scalar",
                file.template,
                variable
            );
        }
        referenced.insert(variable);
    }

    let mut declared = BTreeSet::new();
    for key in file.artifact_identity_dependencies.keys() {
        let Some(name) = super::super::artifact_identity_name::artifact_identity_template_name(key)
        else {
            bail!(
                "checked artifact identity key '{}' has no canonical flattened template name",
                key
            );
        };
        declared.insert(name);
    }
    let missing = referenced
        .iter()
        .map(String::as_str)
        .filter(|key| !declared.contains(*key))
        .collect::<Vec<_>>();
    if !missing.is_empty() {
        bail!(
            "target template '{}' uses undeclared artifact identity scalars [{}]",
            file.template,
            missing.join(", ")
        );
    }
    let unused = declared
        .iter()
        .map(String::as_str)
        .filter(|key| !referenced.contains(*key))
        .collect::<Vec<_>>();
    if !unused.is_empty() {
        bail!(
            "target template '{}' declares unused artifact identity scalars [{}]",
            file.template,
            unused.join(", ")
        );
    }
    Ok(())
}

fn validate_borrowed_template_ownership(dir: &Path, manifest: &TargetManifest) -> Result<()> {
    for file in manifest.files() {
        if file.template_shared_from().is_none() {
            continue;
        }
        let local = safe_target_join(dir, file.template())?;
        if !local.exists() {
            continue;
        }
        bail!(
            "borrowed template '{}' also exists locally at {}; one target must own the bytes",
            file.template(),
            local.display()
        );
    }
    Ok(())
}
