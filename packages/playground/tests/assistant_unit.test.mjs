import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { resolveChatgptSignIn, DEFAULT_CONFIG } from '../src/modules/assistant/config.js';
import { applyEdits } from '../src/modules/assistant/tools/proposals.js';
import { searchChunks } from '../src/modules/assistant/tools/docs_index.js';
import { classifyProviderError } from '../src/modules/assistant/providers/errors.js';
import { normalizeServerUrl } from '../src/modules/assistant/auth/local_url.js';
import { diagnosticsFromRust, buildAssistantIndex } from '../../rumoca-web/assistant_index.mjs';

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..', '..');
const where = (host, pathname = '/rumoca/') => ({
  hostname: host, origin: `https://${host}`, pathname,
});

test('sign in is dynamic on loopback, hosted with a client id, otherwise unavailable', () => {
  const loopback = { hostname: '127.0.0.1', origin: 'http://127.0.0.1:8080', pathname: '/rumoca/' };
  assert.deepEqual(resolveChatgptSignIn(loopback, DEFAULT_CONFIG), {
    mode: 'dynamic', redirectUri: 'http://127.0.0.1:8080/callback',
  });
  const site = where('cognipilot.github.io');
  assert.deepEqual(resolveChatgptSignIn(site, DEFAULT_CONFIG), { mode: 'unavailable' });
  assert.deepEqual(
    resolveChatgptSignIn(site, { ...DEFAULT_CONFIG, openaiSiwcClientId: 'oaiapp_x' }),
    { mode: 'hosted', clientId: 'oaiapp_x', redirectUri: 'https://cognipilot.github.io/rumoca/' },
  );
});

test('edits apply only to text that occurs exactly once', () => {
  assert.equal(applyEdits('a b a', [{ old: 'b', new: 'c' }]), 'a c a');
  assert.throws(() => applyEdits('a b a', [{ old: 'a', new: 'x' }]), /more than once/u);
  assert.throws(() => applyEdits('a b a', [{ old: 'z', new: 'x' }]), /not found/u);
});

test('doc search ranks heading matches first', () => {
  const chunks = [
    { heading: 'Solvers', text: 'choose bdf for stiff problems', title: 't', path: 'a' },
    { heading: 'Install', text: 'download the binary', title: 't', path: 'b' },
  ].map((chunk) => ({ ...chunk, search: `${chunk.title} ${chunk.heading} ${chunk.text}`.toLowerCase() }));
  assert.equal(searchChunks(chunks, 'stiff solvers')[0].path, 'a');
  assert.deepEqual(searchChunks(chunks, 'zzz'), []);
});

test('provider errors map to explicit UI kinds', () => {
  const limit = classifyProviderError({ statusCode: 429, responseBody: '{"error":{"code":"subscription_sharing_usage_limit_exceeded"}}', message: 'limit' });
  assert.equal(limit.kind, 'usage_limit');
  assert.equal(classifyProviderError({ statusCode: 401, message: 'x' }).kind, 'auth');
  assert.equal(classifyProviderError(new TypeError('Failed to fetch')).kind, 'network');
  assert.equal(classifyProviderError(Object.assign(new Error('a'), { name: 'AbortError' })).kind, 'cancelled');
});

test('server URLs are normalized to an origin', () => {
  assert.equal(normalizeServerUrl('http://localhost:11434/'), 'http://localhost:11434');
  assert.throws(() => normalizeServerUrl('ftp://x'), /http/u);
  assert.throws(() => normalizeServerUrl('nope'), /full URL/u);
});

test('diagnostics are read from attribute definitions, not copied', () => {
  const source = [
    '/// A name was not found.',
    '#[error("undefined variable: {name}")]',
    '#[diagnostic(',
    '    code(rumoca::flatten::EF001),',
    '    help("check that the variable is declared (see MLS \\"5.3\\")")',
    ')]',
    'UndefinedVariable { name: String },',
    '//! code(rumoca::flatten::EF999) in a comment is ignored',
  ].join('\n');
  const [entry, ...rest] = diagnosticsFromRust(source, 'x.rs');
  assert.equal(rest.length, 0);
  assert.equal(entry.code, 'EF001');
  assert.equal(entry.message, 'undefined variable: {name}');
  assert.equal(entry.help, 'check that the variable is declared (see MLS "5.3")');
  assert.equal(entry.description, 'A name was not found.');
});

test('the built index covers the compiler catalogue and the user guide', async () => {
  const index = await buildAssistantIndex(repoRoot);
  const codes = new Set(index.diagnostics.map((entry) => entry.code));
  for (const code of ['EF001', 'ER001']) assert.ok(codes.has(code), `${code} indexed`);
  assert.ok(index.diagnostics.every((entry) => entry.message), 'every code has its message');
  assert.ok(index.docs.some((chunk) => chunk.path.endsWith('scenario-tomls.md')));
});
