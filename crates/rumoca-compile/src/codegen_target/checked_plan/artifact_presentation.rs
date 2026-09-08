use super::*;

impl CheckedTargetOutputPathTemplate {
    pub(super) fn construct(declared: &str) -> Result<Self> {
        const ARTIFACT_STEM: &str = "{{ model_name }}";
        let form = if let Some(suffix) = declared.strip_prefix(ARTIFACT_STEM) {
            if suffix.contains('{') || suffix.contains('}') || suffix.contains('/') {
                bail!(
                    "package output path template '{}' may contain only one leading '{{{{ model_name }}}}' segment and a static filename suffix",
                    declared
                );
            }
            validate_portable_member_path(&format!("artifact{suffix}"))?;
            CheckedTargetOutputPathTemplateForm::ArtifactStem {
                suffix: suffix.into(),
            }
        } else {
            if declared.contains('{') || declared.contains('}') {
                bail!(
                    "package output path template '{}' contains an unsupported dynamic segment",
                    declared
                );
            }
            let components = validate_portable_member_path(declared)?;
            CheckedTargetOutputPathTemplateForm::Static(CheckedTargetPackagePath(
                components.join("/").into_boxed_str(),
            ))
        };
        Ok(Self { form })
    }

    /// Render only the checked presentation segment. Host-root joining remains
    /// owned by the package destination constructor.
    #[must_use]
    pub(crate) fn render(&self, artifact_stem: &CheckedTargetArtifactStem) -> PathBuf {
        match &self.form {
            CheckedTargetOutputPathTemplateForm::Static(path) => PathBuf::from(path.as_str()),
            CheckedTargetOutputPathTemplateForm::ArtifactStem { suffix } => {
                PathBuf::from(format!("{}{suffix}", artifact_stem.as_str()))
            }
        }
    }
}

/// One portable artifact-stem presentation segment checked before package
/// root/archive rendering. The type has no unchecked constructor.
#[derive(Debug)]
pub(crate) struct CheckedTargetArtifactStem(Box<str>);

impl CheckedTargetArtifactStem {
    pub(crate) fn from_model_components(components: &[Box<str>]) -> Self {
        const DIRECT_LIMIT: usize = 120;
        const HASH_PREFIX_BYTES: usize = 48;
        const HASH_DOMAIN: &[u8] = b"rumoca-artifact-stem-v1";
        const HEX: &[u8; 16] = b"0123456789abcdef";

        let mut direct_payload = String::new();
        for component in components {
            direct_payload.push('_');
            direct_payload.push_str(&component.len().to_string());
            direct_payload.push('_');
            for byte in component.as_bytes() {
                direct_payload.push(HEX[(byte >> 4) as usize] as char);
                direct_payload.push(HEX[(byte & 0x0f) as usize] as char);
            }
        }
        let direct = format!("rm1{direct_payload}");
        if direct.len() <= DIRECT_LIMIT {
            return Self(direct.into_boxed_str());
        }

        let mut hasher = Sha256::new();
        hasher.update(HASH_DOMAIN);
        for component in components {
            hasher.update((component.len() as u64).to_be_bytes());
            hasher.update(component.as_bytes());
        }
        let digest = hasher.finalize();
        let mut digest_hex = String::with_capacity(64);
        for byte in digest {
            digest_hex.push(HEX[(byte >> 4) as usize] as char);
            digest_hex.push(HEX[(byte & 0x0f) as usize] as char);
        }
        let prefix = &direct_payload[..HASH_PREFIX_BYTES];
        Self(format!("rm1h_{prefix}_{digest_hex}").into_boxed_str())
    }

    #[must_use]
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}
