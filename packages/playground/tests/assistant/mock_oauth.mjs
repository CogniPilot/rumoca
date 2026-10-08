import http from 'node:http';
import crypto from 'node:crypto';

// Mock Sign in with ChatGPT authorization server: dynamic client
// registration at authorize time, PKCE S256 verification at the token
// endpoint, refresh-token rotation, RS256 id tokens and a JWKS document.

const CORS = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': '*',
  'Access-Control-Allow-Methods': 'GET,POST,OPTIONS',
};
const REQUIRED_SCOPE = 'chatgpt.tokens.use.direct';
const b64url = (value) => Buffer.from(value).toString('base64url');

export async function startMockOAuth({ expiresIn = 3600, grantScope = true, forgeState = false } = {}) {
  const { publicKey, privateKey } = crypto.generateKeyPairSync('rsa', { modulusLength: 2048 });
  const jwk = { ...publicKey.export({ format: 'jwk' }), kid: 'k1', alg: 'RS256', use: 'sig' };
  const codes = new Map();
  const refreshTokens = new Set();
  const log = { authorize: [], token: [], rejected: [] };
  let counter = 0;
  let origin = '';

  const sign = (claims) => {
    const signingInput = `${b64url(JSON.stringify({ alg: 'RS256', kid: 'k1', typ: 'JWT' }))}.${b64url(JSON.stringify(claims))}`;
    return `${signingInput}.${crypto.sign('RSA-SHA256', Buffer.from(signingInput), privateKey).toString('base64url')}`;
  };

  const issue = (entry) => {
    counter += 1;
    const refreshToken = `refresh_${counter}`;
    refreshTokens.add(refreshToken);
    const now = Math.floor(Date.now() / 1000);
    const body = {
      access_token: `access_${counter}`,
      refresh_token: refreshToken,
      token_type: 'Bearer',
      expires_in: expiresIn,
      scope: entry.scope,
    };
    if (entry.nonce) {
      body.id_token = sign({ iss: origin, aud: entry.clientId, sub: 'user-1', exp: now + 600, iat: now, nonce: entry.nonce });
    }
    return body;
  };

  const reject = (response, message) => {
    log.rejected.push(message);
    response.writeHead(400, { ...CORS, 'Content-Type': 'application/json' });
    response.end(JSON.stringify({ error: 'invalid_request', error_description: message }));
  };

  const handle = async (request, response) => {
    if (request.method === 'OPTIONS') { response.writeHead(204, CORS); response.end(); return; }
    const url = new URL(request.url, origin);
    if (url.pathname === '/.well-known/openid-configuration') {
      response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
      response.end(JSON.stringify({ issuer: origin, jwks_uri: `${origin}/jwks` }));
    } else if (url.pathname === '/jwks') {
      response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
      response.end(JSON.stringify({ keys: [jwk] }));
    } else if (url.pathname === '/authorize') {
      const query = Object.fromEntries(url.searchParams);
      log.authorize.push(query);
      const dynamic = query.client_id === 'dynamic_agent_client';
      const problems = [];
      if (query.response_type !== 'code') problems.push('response_type');
      if (query.code_challenge_method !== 'S256') problems.push('code_challenge_method');
      if (!query.state || !query.nonce || !query.code_challenge) problems.push('state/nonce/challenge');
      if (!query.scope?.split(' ').includes(REQUIRED_SCOPE)) problems.push('scope');
      if (!query.resource) problems.push('resource');
      if (!/^urn:uuid:/u.test(query.ext_agent_host_id ?? '')) problems.push('ext_agent_host_id');
      if (dynamic && !query.agent_name_hint) problems.push('agent_name_hint');
      if (problems.length) { reject(response, `authorize: ${problems.join(', ')}`); return; }
      const clientId = dynamic ? 'oaiapp_mock123' : query.client_id;
      const code = `code_${++counter}`;
      codes.set(code, { challenge: query.code_challenge, redirectUri: query.redirect_uri, clientId, nonce: query.nonce, scope: grantScope ? query.scope : 'openid' });
      const back = new URL(query.redirect_uri);
      back.searchParams.set('code', code);
      back.searchParams.set('state', forgeState ? 'forged' : query.state);
      if (dynamic) back.searchParams.set('client_id', clientId);
      response.writeHead(302, { Location: back.toString() });
      response.end();
    } else if (url.pathname === '/token') {
      const form = Object.fromEntries(new URLSearchParams(await readBody(request)));
      log.token.push(form);
      if (form.grant_type === 'authorization_code') {
        const entry = codes.get(form.code);
        codes.delete(form.code);
        if (!entry) return reject(response, 'unknown code');
        const challenge = crypto.createHash('sha256').update(form.code_verifier ?? '').digest('base64url');
        if (challenge !== entry.challenge) return reject(response, 'PKCE verifier mismatch');
        if (form.redirect_uri !== entry.redirectUri) return reject(response, 'redirect_uri mismatch');
        if (form.client_id !== entry.clientId) return reject(response, 'client_id is not the issued id');
        if (!form.resource) return reject(response, 'resource missing');
        response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
        response.end(JSON.stringify(issue(entry)));
      } else if (form.grant_type === 'refresh_token') {
        if (!refreshTokens.delete(form.refresh_token)) return reject(response, 'refresh token already used or unknown');
        if (form.client_id !== 'oaiapp_mock123') return reject(response, 'refresh client_id');
        response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
        response.end(JSON.stringify(issue({ scope: `openid ${REQUIRED_SCOPE}`, clientId: form.client_id })));
      } else {
        reject(response, `unsupported grant ${form.grant_type}`);
      }
    } else {
      response.writeHead(404, CORS);
      response.end();
    }
  };

  const server = http.createServer(handle);
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  origin = `http://127.0.0.1:${server.address().port}`;
  return { origin, log, close: () => new Promise((resolve) => server.close(resolve)) };
}

async function readBody(request) {
  const chunks = [];
  for await (const chunk of request) chunks.push(chunk);
  return Buffer.concat(chunks).toString();
}
