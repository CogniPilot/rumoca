# SPEC_0054: Playground Assistant

## Status
PROPOSED

## Summary
The playground assistant helps users write Modelica, set configuration and read
diagnostics through user-chosen providers, keeps no secret on the site, and only
ever proposes edits that the user accepts.

**Status note:** the intended status is DRAFT. The active spec count is already at the
SPEC_0000 section 3 cap (21), so this spec stays PROPOSED until a slot is freed.

## Specification

### 1. Contracts

| Rule | Owner | Why |
|---|---|---|
| No credential is stored on or sent to the site; remembered state lives in the browser's IndexedDB | `assistant/store.js` | Per-device memory, nothing to leak or maintain |
| Every provider has a per-card "Forget" that deletes its stored state | `assistant/session.js` | The user controls retention |
| The model never writes: `propose_edit` and `propose_config_edit` create proposals; the user accepts or rejects a Monaco diff | `assistant/tools/proposals.js` | Edits stay under user control |
| Accepting re-checks that the file is unchanged since the proposal | `assistant/tools/proposals.js` | No stale overwrite |
| Tools call existing playground owners through one `host` adapter; tools hold no playground state | `assistant/host.js`, `main.js` | One owner per concept |
| Providers are adapters behind one `stream()` interface; the agent and panel never name a vendor | `assistant/providers/` | Providers are swappable |
| Auth strategies are slots on a card; adding one changes `session.js` only | `assistant/session.js` | Panel stays generic |
| Diagnostic and docs knowledge is generated at build time from the Rust attributes and `docs/user-guide` | `packages/rumoca-web/assistant_index.mjs` | No hand-copied text |
| Failures are explicit UI states (`auth`, `network`, `usage_limit`, `rate_limit`, `provider`) | `providers/errors.js` | No silent fallbacks |
| Provider SDKs are bundled into `vendor/assistant_sdk.js` with their licenses, versions pinned | `packages/rumoca-web/build.mjs` | Same route as Monaco, no CDN |

### 2. Connect page

| Card | Auth strategy | Notes |
|---|---|---|
| ChatGPT | OAuth 2.0 authorization code with PKCE (Sign in with ChatGPT) | Public client, no secret; falls back to an OpenAI API key on the same card |
| Claude | Anthropic API key | Direct browser calls with `anthropic-dangerous-direct-browser-access: true` |
| Local | Ollama base URL | Through the OpenAI-compatible `/v1` endpoint; connection test |
| Advanced | OpenAI-compatible endpoint plus key | OpenRouter appears only as a preset base URL |

### 3. Sign in with ChatGPT

| Rule | Detail |
|---|---|
| Availability | Active on `127.0.0.1` (dynamic registration) or when `openaiSiwcClientId` is set at build time; otherwise the button reads "Sign-in coming soon" |
| Redirect | Loopback: `http://127.0.0.1:{port}/callback`, handed to the page by the server; hosted: the site URL |
| Authorize | `response_type=code`, `client_id`, `ext_agent_host_id` (`urn:uuid:` kept in IndexedDB), `redirect_uri`, scope `openid profile email offline_access resource.invoke chatgpt.tokens.use.direct`, `resource=https://api.openai.com/v1`, fresh `state` and `nonce`, `code_challenge_method=S256` |
| Dynamic registration | First time `client_id=dynamic_agent_client` with `agent_name_hint`; the callback carries the issued `client_id`, persisted and used afterwards |
| Exchange | POST to the token endpoint with `code_verifier`, identical `redirect_uri` and `resource`; no secret |
| ID token | Signature checked against the JWKS, `iss`, `aud` (issued client id), `exp`, `nonce`; `sub` is the account |
| Scope | `chatgpt.tokens.use.direct` must be granted |
| Refresh | Silent, single flight; access token, expiry, scopes and the rotated refresh token replace the record together |
| Inference | Only the Responses API with `store:false` and streaming; prohibited request fields are never set |
| Sign out / Forget | Sign out deletes tokens; Forget also deletes the host id and issued client id |

Required UI: the button reads "Continue with ChatGPT" with the ChatGPT logo; a one-time modal
"Eligible usage in this app uses your ChatGPT plan. Manage usage in your ChatGPT settings."; a
"Using ChatGPT plan" indicator beside the model selector; "Manage usage" links to ChatGPT settings and is
the primary action when `subscription_sharing_usage_limit_exceeded` or `subscription_sharing_unavailable`
is returned.

### 4. Providers

```js
provider.listModels()  -> Promise<string[]>
provider.stream({ modelId, system, messages, tools, signal, maxSteps })
  -> AsyncIterable<{ type: 'text' | 'tool_call' | 'tool_result' | 'tool_error' | 'step' | 'done' }>
```

| Rule | Where | Why |
|---|---|---|
| The AI SDK (`streamText`, step limit) runs the tool loop for SDK providers | `providers/ai_sdk_stream.js` | One loop, not three |
| Ollama uses `@ai-sdk/openai-compatible` at `/v1`, not a native client | `providers/local.js` | One chat wire format; tool calls need a real Ollama check |
| Model metadata comes from models.dev: vendored snapshot, live fetch cached in IndexedDB | `models_catalog.js` | Tool-capable models only |
| A later local-agent provider (`opencode serve`, ACP bridge) only implements `stream()` and `listModels()` | `session.js` card table | Interface is agent-agnostic |

### 5. Tools

| Tool | Owner it calls |
|---|---|
| `list_files`, `read_file` | `workspace_fs.js` |
| `propose_edit` | proposal store, then `workspace_fs.js` on accept |
| `compile` | `rumoca.language.diagnostics` (structured code, severity, file, span, message, help) |
| `lint` | `rumoca.language.lint` (the WASM `lint` export) |
| `simulate`, `read_results` | scenario interface and the saved run file |
| `read_config`, `propose_config_edit` | `rumoca.scenario.*` parsers for validation |
| `explain_diagnostic`, `search_docs` | static retrieval index |

There is no `fmt` tool: the browser runtime exposes no formatter.

### 6. Verification

| Check | Test |
|---|---|
| Cards, "coming soon", Local CORS instruction, remembered, forget | `packages/playground/tests/assistant_contract_smoke.mjs` |
| PKCE S256, dynamic registration, JWKS id token, refresh rotation, forged state | same, with a mock authorization server |
| Tool round trip with proposal review and mobile layout | same, with Responses, Messages and Ollama stubs |
| Redirect mode, edit application, retrieval, error mapping, index generation | `packages/playground/tests/assistant_unit.test.mjs` |

## References

- SPEC_0008 (diagnostic codes), SPEC_0018 (configuration), SPEC_0000 (spec rules)
