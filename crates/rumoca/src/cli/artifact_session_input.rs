//! CLI-entry resolution of explicit artifact-session inputs.

use anyhow::{Context, Result};
use time::OffsetDateTime;
use time::format_description::well_known::Rfc3339;
use uuid::Uuid;

use rumoca_compile::codegen::targets::{
    ArtifactGenerationInstant, ArtifactIdentitySeed, ArtifactSessionInput,
};

pub(super) fn resolve(
    generation_instant: Option<ArtifactGenerationInstant>,
    identity_seed: Option<ArtifactIdentitySeed>,
) -> Result<ArtifactSessionInput> {
    let input = match (generation_instant, identity_seed) {
        (Some(generation_instant), Some(identity_seed)) => {
            ArtifactSessionInput::construct(generation_instant, identity_seed)
        }
        (None, None) => fresh()?,
        (Some(_), None) | (None, Some(_)) => {
            anyhow::bail!("artifact generation instant and identity seed must be supplied together")
        }
    };
    eprintln!(
        "Artifact session: generation_instant={} identity_seed={}",
        input.generation_instant().as_str(),
        input.identity_seed().as_str()
    );
    Ok(input)
}

pub(super) fn fresh() -> Result<ArtifactSessionInput> {
    let instant = OffsetDateTime::now_utc()
        .replace_nanosecond(0)
        .context("Round artifact generation instant to UTC seconds")?
        .format(&Rfc3339)
        .context("Format artifact generation instant")?
        .parse::<ArtifactGenerationInstant>()?;
    let mut seed_bytes = [0_u8; 16];
    getrandom::fill(&mut seed_bytes).context("Read artifact identity entropy")?;
    seed_bytes[6] = (seed_bytes[6] & 0x0f) | 0x40;
    seed_bytes[8] = (seed_bytes[8] & 0x3f) | 0x80;
    let seed = Uuid::from_bytes(seed_bytes)
        .hyphenated()
        .to_string()
        .parse::<ArtifactIdentitySeed>()?;
    Ok(ArtifactSessionInput::construct(instant, seed))
}
