// The compiler issues exact DAE JSON text alongside its JavaScript inspection
// view. Re-encoding that view would round 64-bit source identities.
export function compiledDaeJson(result) {
    return typeof result?.pretty === 'string' && result.pretty.trim()
        ? result.pretty
        : null;
}
