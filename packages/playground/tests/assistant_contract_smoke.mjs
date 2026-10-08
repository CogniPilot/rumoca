// Browser contract for the playground assistant: connect page, the three
// connection cards, OAuth PKCE round trip, remembered sessions, forget, a
// tool-use round trip with proposal review, and the mobile layout. Model and
// authorization servers are local mocks; nothing leaves the machine.
//
//   node tests/assistant_contract_smoke.mjs --browser-binary chromium [--dist <dir>]
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import crypto from 'node:crypto';

import { chromium } from 'playwright-core';

import { startSiteServer } from './assistant/site_server.mjs';
import { startOpenAiStub, startAnthropicStub, startOllamaStub } from './assistant/mock_providers.mjs';
import { startMockOAuth } from './assistant/mock_oauth.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..', '..', '..');

function arg(name, fallback) {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

const browserBinary = arg('--browser-binary', 'chromium');
const distDir = path.resolve(arg('--dist', path.join(repoRoot, 'packages/rumoca/dist')));
const pkgSubdir = arg('--pkg-subdir', 'release-full-web');
const only = arg('--only', '');
const executablePath = path.isAbsolute(browserBinary)
  ? browserBinary
  : spawnSync('which', [browserBinary], { encoding: 'utf8' }).stdout.trim();

const TIMEOUT = Number(process.env.ASSISTANT_TIMEOUT ?? 90000);
const BROKEN_MODEL = 'model Fix\n  Real x(start = 1);\nequation\n  der(x) = -x\nend Fix;\n';

const browser = await chromium.launch({ executablePath, headless: true, args: ['--no-sandbox', '--disable-gpu'] });
const site = await startSiteServer({ root: repoRoot, distDir, pkgSubdir });
const origin = `http://127.0.0.1:${site.port}`;

async function openPlayground(context, { config = {}, host = '127.0.0.1', search = '' } = {}) {
  const page = await context.newPage();
  page.setDefaultTimeout(TIMEOUT);
  if (process.env.ASSISTANT_TRACE) {
    page.on('crash', () => console.error('PAGE CRASH'));
    page.on('close', () => console.error('PAGE CLOSE'));
    page.on('framenavigated', (frame) => frame === page.mainFrame() && console.error('nav', frame.url()));
    page.on('requestfailed', (request) => console.error('requestfailed', request.url(), request.failure()?.errorText));
    page.on('console', (message) => console.error('console', message.type(), message.text()));
  }
  page.on('pageerror', (error) => console.error('[pageerror]', error.message));
  await page.route('https://models.dev/**', (route) => route.abort());
  await page.addInitScript((value) => { window.rumocaAssistantConfig = value; }, config);
  await page.goto(`http://${host}:${site.port}/rumoca/${search}`);
  await page.waitForFunction(() => document.getElementById('outputRuntimeStatus')?.dataset.tone === 'ready');
  await page.waitForFunction(() => Boolean(window.editor?.setValue));
  return page;
}

// The playground restores its workspace asynchronously after the runtime is
// ready and replaces the editor text when it does; write until the text sticks.
async function setEditorSource(page, source) {
  for (let attempt = 0; attempt < 20; attempt += 1) {
    await page.evaluate((text) => window.editor.setValue(text), source);
    await page.waitForTimeout(500);
    if ((await page.evaluate(() => window.editor.getValue())) === source) return;
  }
  throw new Error('editor text did not settle');
}

async function openAssistant(page) {
  await page.click('#assistantBtn');
  await page.waitForSelector('[data-testid="assistant-panel"]:not([hidden])');
}

const idbKeys = (page) => page.evaluate(() => new Promise((resolve, reject) => {
  const open = indexedDB.open('rumoca-assistant');
  open.onerror = () => reject(open.error);
  open.onsuccess = () => {
    const request = open.result.transaction('kv').objectStore('kv').getAllKeys();
    request.onsuccess = () => resolve(request.result);
  };
}));

async function say(page, text) {
  await page.fill('[data-testid="assistant-input"]', text);
  await page.click('[data-action="send"]');
}

const unwrap = (output) => {
  const parsed = typeof output === 'string' ? JSON.parse(output) : output;
  return parsed?.value ?? parsed;
};

const scenarios = {
  async connectPageCards(context) {
    // Served from a non-loopback name with no client id: sign-in is "coming soon".
    const hosted = await openPlayground(context, { host: 'localhost' });
    await openAssistant(hosted);
    for (const card of ['chatgpt', 'anthropic', 'local']) {
      await hosted.waitForSelector(`[data-card="${card}"]`);
    }
    assert.equal(await hosted.locator('[data-testid="chatgpt-coming-soon"]').isDisabled(), true);
    assert.equal(await hosted.locator('[data-card="chatgpt"] input[type="password"]').count(), 1, 'ChatGPT card keeps the API key fallback');
    assert.equal(await hosted.locator('[data-testid="assistant-advanced"] [data-card="compatible"]').count(), 1);
    await hosted.close();

    // On loopback the plan sign-in is active.
    const local = await openPlayground(context);
    await openAssistant(local);
    assert.match(await local.locator('[data-testid="chatgpt-continue"]').innerText(), /Continue with ChatGPT/u);
    await local.close();
  },

  async claudeKeyRememberedAndForgotten(context) {
    const stub = await startAnthropicStub({ script: () => ({ text: 'Hello from Claude stub' }) });
    const page = await openPlayground(context, { config: { anthropicApiBase: stub.base } });
    await openAssistant(page);
    await page.fill('[data-testid="key-anthropic"]', 'sk-ant-test');
    await page.click('[data-action="connect-anthropic"]');
    await page.waitForSelector('[data-testid="model-select"]');
    await say(page, 'hi');
    await page.waitForSelector('.assistant-assistant:has-text("Hello from Claude stub")');
    const call = stub.requests.find((request) => request.path === '/v1/messages');
    assert.equal(call.headers['anthropic-dangerous-direct-browser-access'], 'true');
    assert.equal(call.headers['x-api-key'], 'sk-ant-test');
    assert.ok(call.body.system.length > 0 || call.body.system?.length > 0, 'system prompt sent');
    assert.ok(call.body.tools.some((tool) => tool.name === 'propose_edit'));
    await page.waitForSelector('[data-testid="assistant-usage"]');

    // Remembered after reload (stored per device in IndexedDB).
    await page.reload();
    await page.waitForFunction(() => Boolean(window.toggleAssistant));
    await openAssistant(page);
    await page.waitForSelector('[data-testid="model-select"]');
    assert.ok((await idbKeys(page)).includes('apikey:anthropic'));

    // Forget removes it.
    await page.click('[data-action="open-connect"]');
    await page.click('[data-card="anthropic"] button:has-text("Forget")');
    await page.waitForSelector('[data-testid="state-anthropic"]:has-text("Not connected")');
    assert.ok(!(await idbKeys(page)).includes('apikey:anthropic'));
    await page.close();
    await stub.close();
  },

  async localOllamaTestAndCors(context) {
    const blocked = await startOllamaStub({ cors: false });
    const open = await startOllamaStub({ script: () => ({ text: 'Hello from Ollama stub' }) });
    const closed = await startOllamaStub();
    const closedUrl = closed.url;
    await closed.close();
    const page = await openPlayground(context);
    await openAssistant(page);
    const result = page.locator('[data-testid="local-test-result"]');

    await page.fill('[data-testid="local-url"]', blocked.url);
    await page.click('[data-action="test-local"]');
    await page.waitForFunction(() => document.querySelector('[data-testid="local-test-result"]')?.dataset.kind === 'unreachable_or_cors');
    assert.ok((await result.innerText()).includes(`OLLAMA_ORIGINS=${origin}`));

    await page.fill('[data-testid="local-url"]', closedUrl);
    await page.click('[data-action="test-local"]');
    await page.waitForFunction(() => document.querySelector('[data-testid="local-test-result"]')?.dataset.kind === 'unreachable_or_cors');
    await page.waitForFunction((url) => document.querySelector('[data-testid="local-test-result"]').textContent.includes(url), closedUrl);

    await page.fill('[data-testid="local-url"]', open.url);
    await page.click('[data-action="connect-local"]');
    await page.waitForSelector('[data-testid="model-select"]');
    await say(page, 'hi');
    await page.waitForSelector('.assistant-assistant:has-text("Hello from Ollama stub")');
    await page.close();
    await Promise.all([blocked.close(), open.close()]);
  },

  async chatgptOAuthRoundTrip(context) {
    const oauth = await startMockOAuth({ expiresIn: 30 });
    const bearer = [];
    const stub = await startOpenAiStub({ script: () => ({ text: 'Hello from plan stub' }) });
    const config = {
      openaiAuthorizeUrl: `${oauth.origin}/authorize`,
      openaiTokenUrl: `${oauth.origin}/token`,
      openaiIssuer: oauth.origin,
      openaiApiBase: stub.base,
    };
    const page = await openPlayground(context, { config });
    await openAssistant(page);
    await page.click('[data-testid="chatgpt-continue"]');

    // Back from the authorization server: sign-in completes, first-use notice shows once.
    await page.waitForSelector('[data-testid="first-sign-in-notice"]');
    assert.match(await page.locator('[data-testid="first-sign-in-notice"]').innerText(), /Eligible usage in this app uses your ChatGPT plan/u);
    await page.click('[data-action="dismiss-notice"]');
    await page.waitForSelector('[data-testid="first-sign-in-notice"]', { state: 'detached' });
    await page.waitForSelector('[data-testid="plan-indicator"]');
    assert.equal(new URL(page.url()).search, '', 'authorization response is removed from the URL');

    const authorize = oauth.log.authorize[0];
    assert.equal(authorize.client_id, 'dynamic_agent_client');
    assert.equal(authorize.code_challenge_method, 'S256');
    assert.equal(authorize.redirect_uri, `${origin}/callback`);
    const token = oauth.log.token[0];
    assert.equal(token.client_id, 'oaiapp_mock123', 'token exchange uses the issued client id');
    assert.equal(
      crypto.createHash('sha256').update(token.code_verifier).digest('base64url'),
      authorize.code_challenge,
      'verifier matches the challenge',
    );
    assert.deepEqual(oauth.log.rejected, []);

    // Responses API call: bearer token, store:false, none of the prohibited fields.
    await say(page, 'hi');
    await page.waitForSelector('.assistant-assistant:has-text("Hello from plan stub")');
    const first = stub.requests.find((request) => request.path === '/v1/responses');
    assert.match(first.authorization, /^Bearer access_\d+$/u);
    bearer.push(first.authorization);
    assert.equal(first.body.store, false);
    assert.equal(first.body.stream, true);
    for (const field of ['background', 'conversation', 'max_output_tokens', 'max_tool_calls', 'metadata', 'moderation',
      'multi_agent', 'prompt', 'prompt_cache_retention', 'safety_identifier', 'temperature', 'top_logprobs', 'top_p',
      'truncation', 'user']) {
      assert.ok(!(field in first.body), `prohibited field ${field} is not sent`);
    }
    const models = stub.requests.find((request) => request.path === '/v1/models');
    assert.ok(models);

    // expires_in is 30 s (inside the refresh margin), so the next call refreshes and rotates.
    await say(page, 'again');
    await page.waitForFunction(() => document.querySelectorAll('.assistant-assistant').length >= 2);
    const refreshes = oauth.log.token.filter((entry) => entry.grant_type === 'refresh_token');
    assert.ok(refreshes.length >= 1, 'silent refresh happened');
    assert.deepEqual(oauth.log.rejected, [], 'rotated refresh tokens were never reused');
    const last = stub.requests.filter((request) => request.path === '/v1/responses').at(-1);
    assert.notEqual(last.authorization, bearer[0], 'rotated access token is used');

    // Remembered across reload.
    await page.reload();
    await page.waitForFunction(() => Boolean(window.toggleAssistant));
    await openAssistant(page);
    await page.waitForSelector('[data-testid="plan-indicator"]');

    // Sign out clears tokens; the notice is not shown again and sign-in works again.
    await page.click('[data-action="open-connect"]');
    await page.click('[data-action="signout-chatgpt"]');
    await page.waitForSelector('[data-testid="chatgpt-continue"]');
    let keys = await idbKeys(page);
    assert.ok(!keys.includes('oauth:openai:tokens'));
    assert.ok(keys.includes('oauth:openai:host_id'));
    await page.click('[data-testid="chatgpt-continue"]');
    await page.waitForSelector('[data-testid="plan-indicator"]');
    assert.equal(await page.locator('[data-testid="first-sign-in-notice"]').count(), 0);

    // Forget removes host id and client id as well.
    await page.click('[data-action="open-connect"]');
    await page.click('[data-card="chatgpt"] button:has-text("Forget")');
    await page.waitForSelector('[data-testid="chatgpt-continue"]');
    keys = await idbKeys(page);
    assert.deepEqual(keys.filter((key) => String(key).startsWith('oauth:openai:')), []);
    await page.close();
    await Promise.all([oauth.close(), stub.close()]);
  },

  async oauthRejectsTamperedState(context) {
    const oauth = await startMockOAuth({ forgeState: true });
    const page = await openPlayground(context, {
      config: { openaiAuthorizeUrl: `${oauth.origin}/authorize`, openaiTokenUrl: `${oauth.origin}/token`, openaiIssuer: oauth.origin },
    });
    await openAssistant(page);
    await page.click('[data-testid="chatgpt-continue"]');
    await page.waitForSelector('[data-testid="assistant-error"]');
    assert.equal(oauth.log.token.length, 0, 'no token request is made for a forged state');
    await page.close();
    await oauth.close();
  },

  async toolUseRoundTripWithProposalReview(context) {
    const script = (body) => {
      const outputs = body.input.filter((item) => item.type === 'function_call_output');
      if (outputs.length === 0) return { call: { name: 'compile', input: {} } };
      if (outputs.length === 1) {
        const compiled = unwrap(outputs[0].output);
        assert.ok(compiled.diagnostics.length > 0, 'compile reported the syntax error');
        return { call: { name: 'propose_edit', input: {
          path: compiled.file,
          summary: 'Add the missing semicolon',
          edits: [{ old: 'der(x) = -x\nend Fix;', new: 'der(x) = -x;\nend Fix;' }],
        } } };
      }
      return { text: 'I proposed a fix: add a semicolon.' };
    };
    const stub = await startOpenAiStub({ script });
    const page = await openPlayground(context, { config: { openaiApiBase: stub.base } });
    await setEditorSource(page, BROKEN_MODEL);
    await openAssistant(page);
    await page.fill('[data-card="chatgpt"] input[type="password"]', 'sk-test');
    await page.click('[data-action="connect-chatgpt"]');
    await page.waitForSelector('[data-testid="model-select"]');
    await say(page, 'Why does this model not compile?');

    await page.waitForSelector('[data-proposal][data-status="pending"]');
    assert.ok(await page.locator('.monaco-diff-editor').count() >= 1, 'Monaco diff view is shown');
    assert.ok((await page.evaluate(() => window.editor.getValue())).includes('-x\nend Fix;'), 'nothing is written before acceptance');
    await page.waitForSelector('.assistant-assistant:has-text("I proposed a fix")');

    await page.click('[data-action="accept-proposal"]');
    await page.waitForFunction(() => window.editor.getValue().includes('der(x) = -x;'));
    await page.waitForSelector('[data-proposal][data-status="accepted"]');
    assert.equal(stub.requests.filter((request) => request.path === '/v1/responses').length, 3);
    await page.close();
    await stub.close();
  },

  async mobileLayout(context) {
    const mobile = await browser.newContext({ viewport: { width: 390, height: 844 }, hasTouch: true, isMobile: true });
    const page = await openPlayground(mobile);
    await page.click('#mobileAssistantBtn');
    await page.waitForSelector('[data-testid="assistant-panel"]:not([hidden])');
    const box = await page.locator('[data-testid="assistant-panel"]').boundingBox();
    assert.ok(Math.abs(box.width - 390) < 2, `bottom sheet spans the viewport width (${box.width})`);
    assert.ok(Math.abs(box.y + box.height - 844) < 2, 'sheet is anchored to the bottom');
    assert.ok(box.height < 844 * 0.85 && box.height > 844 * 0.6, `sheet height is a partial sheet (${box.height})`);
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth), 'no horizontal overflow');
    await page.waitForSelector('[data-testid="local-mobile-note"]');
    await page.click('[data-action="close-assistant"]');
    assert.equal(await page.locator('[data-testid="assistant-panel"]').isHidden(), true);
    assert.ok(await page.locator('#editor').isVisible(), 'the editor layout is intact with the panel closed');
    await mobile.close();
  },
};

let failures = 0;
for (const [name, run] of Object.entries(scenarios)) {
  if (only && name !== only) continue;
  const context = await browser.newContext();
  try {
    await run(context);
    console.log(`ok   ${name}`);
  } catch (error) {
    failures += 1;
    console.error(`FAIL ${name}\n${error.stack}`);
    for (const page of context.pages()) {
      const text = await page.locator('[data-testid="assistant-panel"]').innerText({ timeout: 2000 }).catch(() => '(no panel)');
      console.error(`panel text:\n${text}`);
    }
  } finally {
    await context.close();
  }
}
await browser.close();
await site.close();
process.exit(failures ? 1 : 0);
