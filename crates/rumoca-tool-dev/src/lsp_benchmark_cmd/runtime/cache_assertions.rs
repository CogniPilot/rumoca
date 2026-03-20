use super::*;

pub(super) fn ensure_no_model_query_activity(
    context: &str,
    delta: &CompletionSessionCacheDelta,
) -> Result<()> {
    for (stage, hits, misses, builds) in [
        (
            "instantiated_model",
            delta.instantiated_model_cache_hits,
            delta.instantiated_model_cache_misses,
            delta.instantiated_model_builds,
        ),
        (
            "typed_model",
            delta.typed_model_cache_hits,
            delta.typed_model_cache_misses,
            delta.typed_model_builds,
        ),
        (
            "flat_model",
            delta.flat_model_cache_hits,
            delta.flat_model_cache_misses,
            delta.flat_model_builds,
        ),
        (
            "dae_model",
            delta.dae_model_cache_hits,
            delta.dae_model_cache_misses,
            delta.dae_model_builds,
        ),
    ] {
        ensure!(hits == 0, "{context} should not hit {stage}");
        ensure!(misses == 0, "{context} should not miss {stage}");
        ensure!(builds == 0, "{context} should not build {stage}");
    }
    Ok(())
}
