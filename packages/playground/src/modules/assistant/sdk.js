// One owner for the bundled third-party SDKs (AI SDK, provider packages,
// oauth4webapi, jose). The bundle is built into vendor/ by
// packages/rumoca-web/build.mjs and loaded only when the assistant is used.

let sdkPromise = null;

export function loadSdk() {
    sdkPromise ??= import('../../../vendor/assistant_sdk.js');
    return sdkPromise;
}
