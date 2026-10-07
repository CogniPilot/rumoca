import { kv } from '../store.js';
import { loadSdk } from '../sdk.js';

// Strategy: OAuth 2.0 authorization code with PKCE as a public client
// (Sign in with ChatGPT). Protocol mechanics come from oauth4webapi and jose;
// this file adds the Sign in with ChatGPT authorize parameters and the
// dynamic client registration handling.

export const REQUIRED_SCOPE = 'chatgpt.tokens.use.direct';
const SCOPES = `openid profile email offline_access resource.invoke ${REQUIRED_SCOPE}`;
const DYNAMIC_CLIENT_ID = 'dynamic_agent_client';
const REFRESH_MARGIN_MS = 60_000;
const PENDING_KEY = 'rumoca-assistant-oauth-pending';

const KEYS = {
    tokens: 'oauth:openai:tokens',
    clientId: 'oauth:openai:client_id',
    hostId: 'oauth:openai:host_id',
    notice: 'oauth:openai:first_sign_in_notice',
};

export function createOAuthPkceStrategy({
    config,
    signIn,
    fetchImpl = (...args) => globalThis.fetch(...args),
    now = () => Date.now(),
    pendingStorage = globalThis.sessionStorage,
}) {
    const resource = config.openaiApiBase;
    let refreshInFlight = null;

    const authorizationServer = {
        issuer: config.openaiIssuer,
        authorization_endpoint: config.openaiAuthorizeUrl,
        token_endpoint: config.openaiTokenUrl,
    };

    // oauth4webapi refuses plain-http endpoints unless allowed; only a local
    // test authorization server is ever http.
    async function requestOptions(extra = {}) {
        const { oauth } = await loadSdk();
        const options = { ...extra, [oauth.customFetch]: fetchImpl };
        if (config.openaiTokenUrl.startsWith('http:')) {
            options[oauth.allowInsecureRequests] = true;
        }
        return options;
    }

    async function hostId() {
        let id = await kv.get(KEYS.hostId);
        if (!id) {
            id = `urn:uuid:${crypto.randomUUID()}`;
            await kv.set(KEYS.hostId, id);
        }
        return id;
    }

    async function activeClientId() {
        if (signIn.mode === 'hosted') return signIn.clientId;
        return (await kv.get(KEYS.clientId)) || DYNAMIC_CLIENT_ID;
    }

    async function begin() {
        const { oauth } = await loadSdk();
        const verifier = oauth.generateRandomCodeVerifier();
        const attempt = {
            verifier,
            state: oauth.generateRandomState(),
            nonce: oauth.generateRandomNonce(),
            redirectUri: signIn.redirectUri,
            clientId: await activeClientId(),
        };
        const url = new URL(config.openaiAuthorizeUrl);
        const params = {
            response_type: 'code',
            client_id: attempt.clientId,
            ext_agent_host_id: await hostId(),
            redirect_uri: attempt.redirectUri,
            scope: SCOPES,
            resource,
            state: attempt.state,
            nonce: attempt.nonce,
            code_challenge_method: 'S256',
            code_challenge: await oauth.calculatePKCECodeChallenge(verifier),
        };
        if (attempt.clientId === DYNAMIC_CLIENT_ID) {
            params.agent_name_hint = config.agentName;
        }
        for (const [name, value] of Object.entries(params)) {
            url.searchParams.set(name, value);
        }
        pendingStorage.setItem(PENDING_KEY, JSON.stringify(attempt));
        return url.toString();
    }

    // A response while an attempt is pending must carry that attempt's state.
    function takePending(state) {
        const raw = pendingStorage.getItem(PENDING_KEY);
        if (!raw) return null;
        pendingStorage.removeItem(PENDING_KEY);
        const attempt = JSON.parse(raw);
        if (attempt.state !== state) {
            throw new Error('The sign-in response did not match this attempt (state mismatch). Try again.');
        }
        return attempt;
    }

    async function verifyIdToken(idToken, clientId, nonce) {
        const { createLocalJWKSet, jwtVerify } = await loadSdk();
        const discovery = await fetchImpl(
            `${config.openaiIssuer}/.well-known/openid-configuration`,
        );
        if (!discovery.ok) throw new Error(`OpenID discovery failed (${discovery.status}).`);
        const { jwks_uri: jwksUri } = await discovery.json();
        const jwksResponse = await fetchImpl(jwksUri);
        if (!jwksResponse.ok) throw new Error(`Signing key download failed (${jwksResponse.status}).`);
        const { payload } = await jwtVerify(idToken, createLocalJWKSet(await jwksResponse.json()), {
            issuer: config.openaiIssuer,
            audience: clientId,
        });
        if (payload.nonce !== nonce) throw new Error('The ID token nonce does not match this sign-in attempt.');
        return payload;
    }

    function grantedScopes(tokens) {
        return String(tokens.scope || '').split(/\s+/u).filter(Boolean);
    }

    function toRecord(tokens, previous = {}) {
        const scope = grantedScopes(tokens);
        if (!scope.includes(REQUIRED_SCOPE)) {
            throw new Error(`ChatGPT did not grant ${REQUIRED_SCOPE}; the plan cannot be used here.`);
        }
        return {
            accessToken: tokens.access_token,
            refreshToken: tokens.refresh_token ?? previous.refreshToken,
            expiresAt: now() + Number(tokens.expires_in) * 1000,
            scope,
            sub: previous.sub,
        };
    }

    // Finishes a sign-in when the page was opened by the authorization
    // redirect. Returns null for an ordinary page load.
    async function completeFromUrl(urlString) {
        const url = new URL(urlString);
        const state = url.searchParams.get('state');
        if (!state) return null;
        const attempt = takePending(state);
        if (!attempt) return null;
        const { oauth } = await loadSdk();
        if (url.searchParams.has('error')) {
            throw new Error(
                `Sign-in was not completed: ${url.searchParams.get('error_description') || url.searchParams.get('error')}`,
            );
        }
        const issuedClientId = url.searchParams.get('client_id');
        const clientId = attempt.clientId === DYNAMIC_CLIENT_ID ? issuedClientId : attempt.clientId;
        if (!clientId) throw new Error('The authorization server did not issue a client id.');
        const client = { client_id: clientId };
        const params = oauth.validateAuthResponse(
            { ...authorizationServer, authorization_response_iss_parameter_supported: false },
            client,
            url,
            attempt.state,
        );
        const response = await oauth.authorizationCodeGrantRequest(
            authorizationServer,
            client,
            oauth.None(),
            params,
            attempt.redirectUri,
            attempt.verifier,
            await requestOptions({ additionalParameters: { resource } }),
        );
        const tokens = await oauth.processAuthorizationCodeResponse(
            authorizationServer,
            client,
            response,
            { expectedNonce: attempt.nonce, requireIdToken: true },
        );
        const claims = await verifyIdToken(tokens.id_token, clientId, attempt.nonce);
        const record = { ...toRecord(tokens), sub: claims.sub };
        const firstSignIn = (await kv.get(KEYS.notice)) === null;
        if (attempt.clientId === DYNAMIC_CLIENT_ID) await kv.set(KEYS.clientId, clientId);
        await kv.set(KEYS.tokens, record);
        return { sub: claims.sub, firstSignIn };
    }

    async function refresh(record) {
        const { oauth } = await loadSdk();
        const client = { client_id: await activeClientId() };
        const response = await oauth.refreshTokenGrantRequest(
            authorizationServer,
            client,
            oauth.None(),
            record.refreshToken,
            await requestOptions({ additionalParameters: { resource } }),
        );
        let tokens;
        try {
            tokens = await oauth.processRefreshTokenResponse(authorizationServer, client, response);
        } catch (error) {
            await kv.delete(KEYS.tokens);
            throw new Error(`Your ChatGPT session expired; sign in again. (${error.message})`);
        }
        // Access token, expiry, scopes and the rotated refresh token replace
        // the stored record together.
        const next = toRecord(tokens, record);
        await kv.set(KEYS.tokens, next);
        return next;
    }

    // The stored record is read inside the single-flight so a caller that
    // arrives after a rotation never refreshes with the spent refresh token.
    async function currentRecord() {
        const record = await kv.get(KEYS.tokens);
        if (!record) throw new Error('Not signed in to ChatGPT.');
        return record.expiresAt - now() > REFRESH_MARGIN_MS ? record : await refresh(record);
    }

    async function getAccessToken() {
        refreshInFlight ??= currentRecord().finally(() => {
            refreshInFlight = null;
        });
        return (await refreshInFlight).accessToken;
    }

    return {
        kind: 'oauth_pkce',
        signIn,
        load: () => kv.get(KEYS.tokens),
        begin,
        completeFromUrl,
        getAccessToken,
        markNoticeShown: () => kv.set(KEYS.notice, true),
        async signOut() {
            await kv.delete(KEYS.tokens);
        },
        async forget() {
            await kv.deletePrefix('oauth:openai:');
        },
    };
}
