//! Generic two-stage publication with immutable render bytes and named inputs.

use super::*;
use std::collections::BTreeSet;

/// An immutable prepared product. Build input writes cannot replace its retained
/// bytes; publication consumes the original rendered metadata and exact roles.
pub struct PreparedTargetPackage {
    inventory: rumoca_core::artifact_build::PreparedBuildInventory<ArtifactSession>,
    preparation_required: Vec<PathBuf>,
    build_only: BTreeSet<PathBuf>,
    inputs: BTreeMap<String, PathBuf>,
    final_required: Vec<PathBuf>,
    root: String,
    archive: Option<String>,
}

impl PreparedTargetPackage {
    pub(crate) fn construct(
        rendered: Vec<(String, Vec<u8>)>,
        assets: Vec<(String, Vec<TargetAssetFile>)>,
        declared: &rumoca_compile::codegen::targets::TargetPackage,
        render_path: impl Fn(&str) -> Result<String>,
        artifact: ArtifactSession,
    ) -> Result<Self> {
        let product = prepare_product(rendered, assets, Vec::new())?;
        let build_only = declared
            .build_only
            .iter()
            .map(|path| resolve_relative_path(render_path(path)?, "build-only path"))
            .collect::<Result<BTreeSet<_>>>()?;
        let mut inputs = BTreeMap::new();
        let mut destinations = BTreeSet::new();
        for input in &declared.inputs {
            let path = resolve_relative_path(render_path(&input.path)?, "compiled input path")?;
            if input.name.is_empty()
                || inputs.contains_key(&input.name)
                || !destinations.insert(path.clone())
                || product.files.contains_key(&path)
                || build_only.iter().any(|prefix| path.starts_with(prefix))
            {
                bail!("compiled input has an empty/duplicate role or colliding destination");
            }
            inputs.insert(input.name.clone(), path);
        }
        let all_paths = product
            .files
            .keys()
            .chain(destinations.iter())
            .cloned()
            .collect();
        rumoca_core::artifact_build::validate_artifact_file_paths(&all_paths)
            .map_err(anyhow::Error::msg)?;
        let final_required = declared
            .required_files
            .iter()
            .map(|path| render_path(path))
            .collect::<Result<Vec<_>>>()?;
        let final_required = resolve_required_files(&final_required)?;
        for path in &final_required {
            if !destinations.contains(path)
                && (!product.files.contains_key(path)
                    || build_only.iter().any(|prefix| path.starts_with(prefix)))
            {
                bail!("required final path is not supplied by the declared artifact graph");
            }
        }
        let root = render_path(&declared.root)?;
        resolve_relative_path(&root, "prepared product root")?;
        let archive = declared
            .archive
            .as_ref()
            .map(|archive| render_path(&archive.path))
            .transpose()?;
        let preparation_required: Vec<_> = final_required
            .iter()
            .filter(|path| product.files.contains_key(*path))
            .cloned()
            .collect();
        if preparation_required.is_empty() {
            bail!("prepared product requires a rendered marker before build input writes");
        }
        let slots = declared
            .inputs
            .iter()
            .map(|input| {
                let parameters = input
                    .build
                    .iter()
                    .map(|(key, value)| Ok((key.clone(), render_path(value)?)))
                    .collect::<Result<BTreeMap<_, _>>>()?;
                Ok((input.name.clone(), parameters))
            })
            .collect::<Result<BTreeMap<_, _>>>()?;
        let inventory = rumoca_core::artifact_build::PreparedBuildInventory::construct(
            product.files.into_iter().collect(),
            artifact,
            slots,
        )
        .map_err(anyhow::Error::msg)?;
        Ok(Self {
            inventory,
            preparation_required,
            build_only,
            inputs,
            final_required,
            root,
            archive,
        })
    }

    /// Borrow immutable provenance shared by every rendered file in this object.
    pub fn artifact(&self) -> &ArtifactSession {
        self.inventory.session()
    }

    /// Issue one borrowing request from the immutable inventory's declared slot.
    pub fn build_request(
        &self,
        slot: &str,
    ) -> Result<rumoca_core::artifact_build::PreparedBuildRequest<'_, ArtifactSession>> {
        self.inventory.request(slot).map_err(anyhow::Error::msg)
    }

    #[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
    pub(crate) fn build_slots(&self) -> Vec<String> {
        self.inputs.keys().cloned().collect()
    }

    pub(crate) fn accepts_build(
        &self,
        binding: &rumoca_core::artifact_build::ArtifactBuildBinding<ArtifactSession>,
    ) -> bool {
        self.inventory.accepts(binding, binding.slot())
    }

    /// Write build sources to a separate caller-owned directory. No final archive
    /// is created and the original retained metadata is never read back from it.
    pub fn write_build_inputs(&self, directory: &Path) -> Result<()> {
        let package = PackageSpec {
            required_files: self
                .preparation_required
                .iter()
                .map(|path| path.to_string_lossy().into_owned())
                .collect(),
            zip: None,
        };
        install_prepared_files(
            self.inventory.files(),
            &self.preparation_required,
            &package,
            directory,
        )
    }

    /// Consume the original product and exact declared input roles. This raw
    /// format-neutral publisher checks structure only, never binary semantics or
    /// prepared build provenance. Checked delivery uses its sealed adapter result.
    pub fn publish(self, inputs: BTreeMap<String, Vec<u8>>, output: &Path) -> Result<()> {
        if inputs.len() != self.inputs.len()
            || inputs.keys().any(|name| !self.inputs.contains_key(name))
        {
            bail!("compiled inputs do not match the exact declared roles");
        }
        let mut files = self.inventory.into_files().map_err(anyhow::Error::msg)?;
        files.retain(|path, _| {
            !self
                .build_only
                .iter()
                .any(|prefix| path.starts_with(prefix))
        });
        for (name, bytes) in inputs {
            if bytes.is_empty() {
                bail!("compiled input '{name}' is empty");
            }
            insert_prepared_file(&mut files, self.inputs[&name].clone(), bytes)?;
        }
        validate_file_tree(&files)?;
        let package = PackageSpec {
            required_files: self
                .preparation_required
                .iter()
                .filter(|path| !self.inputs.values().any(|input| input == *path))
                .map(|path| path.to_string_lossy().into_owned())
                .collect(),
            zip: self
                .archive
                .map(|path| {
                    safe_target_join(output, path).map(|archive_path| ZipPackage { archive_path })
                })
                .transpose()?,
        };
        let root = safe_target_join(output, &self.root)?;
        install_prepared_files(&files, &self.final_required, &package, &root)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::codegen::targets::parse_target_manifest;

    fn prepared(binary: &str) -> PreparedTargetPackage {
        let manifest = parse_target_manifest(&format!(
            r#"
version = 1
ir = "dae"
[capabilities]
[package]
root = "Product"
required_files = ["metadata.xml", "{binary}"]
build_only = ["build"]
[[package.inputs]]
name = "binary"
path = "{binary}"
[package.archive]
path = "Product.zip"
format = "zip"
root = "flat"
[[files]]
path = "metadata.xml"
template = "metadata"
id = "identity"
"#
        ))
        .unwrap();
        let artifact = ArtifactSession::new(&manifest.files).unwrap();
        PreparedTargetPackage::construct(
            vec![
                (
                    "metadata.xml".into(),
                    artifact.identities["identity"].as_bytes().to_vec(),
                ),
                ("build/source.txt".into(), b"original source".to_vec()),
            ],
            Vec::new(),
            manifest.package.as_ref().unwrap(),
            |path| Ok(path.into()),
            artifact,
        )
        .unwrap()
    }

    #[test]
    fn compiled_publication_retains_metadata_and_excludes_mutated_build_inputs() {
        let work = tempfile::tempdir().unwrap();
        let prepared = prepared("bin/first.bin");
        let identity = prepared.artifact().identities["identity"].clone();
        let build = work.path().join("build");
        prepared.write_build_inputs(&build).unwrap();
        std::fs::write(build.join("metadata.xml"), b"foreign identity").unwrap();
        prepared
            .publish(
                BTreeMap::from([("binary".into(), b"compiled bytes".to_vec())]),
                work.path(),
            )
            .unwrap();
        let root = work.path().join("Product");
        assert_eq!(
            std::fs::read(root.join("metadata.xml")).unwrap(),
            identity.as_bytes()
        );
        assert_eq!(
            std::fs::read(root.join("bin/first.bin")).unwrap(),
            b"compiled bytes"
        );
        assert!(!root.join("build").exists());
        let mut archive =
            zip::ZipArchive::new(std::fs::File::open(work.path().join("Product.zip")).unwrap())
                .unwrap();
        assert_eq!(archive.len(), 2);
        assert!(archive.by_name("bin/first.bin").is_ok());
    }

    #[test]
    fn missing_unknown_and_empty_compiled_inputs_preserve_existing_complete_product() {
        let work = tempfile::tempdir().unwrap();
        prepared("bin/old.bin")
            .publish(BTreeMap::from([("binary".into(), vec![1])]), work.path())
            .unwrap();
        let previous = std::fs::read(work.path().join("Product.zip")).unwrap();
        for inputs in [
            BTreeMap::new(),
            BTreeMap::from([("foreign".into(), vec![2])]),
            BTreeMap::from([("binary".into(), Vec::new())]),
        ] {
            assert!(
                prepared("bin/new.bin")
                    .publish(inputs, work.path())
                    .is_err()
            );
            assert_eq!(
                std::fs::read(work.path().join("Product.zip")).unwrap(),
                previous
            );
        }
        prepared("bin/new.bin")
            .publish(BTreeMap::from([("binary".into(), vec![3])]), work.path())
            .unwrap();
        assert!(!work.path().join("Product/bin/old.bin").exists());
        assert_eq!(
            std::fs::read(work.path().join("Product/bin/new.bin")).unwrap(),
            vec![3]
        );
    }
}
