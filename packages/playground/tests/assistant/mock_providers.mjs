import http from 'node:http';

// Mock model servers for the assistant contract: an OpenAI Responses API
// stub, an Anthropic Messages stub and an Ollama (OpenAI-compatible) stub.
// Each records requests and answers from a script so tests can assert both
// what the browser sent and how the UI reacted.

const CORS = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': '*',
  'Access-Control-Allow-Methods': 'GET,POST,OPTIONS',
};

async function readBody(request) {
  const chunks = [];
  for await (const chunk of request) chunks.push(chunk);
  return Buffer.concat(chunks).toString();
}

async function listen(handler) {
  const server = http.createServer(handler);
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  return { server, port: server.address().port, close: () => new Promise((resolve) => server.close(resolve)) };
}

function sse(response, events) {
  response.writeHead(200, { ...CORS, 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-cache' });
  for (const [event, data] of events) {
    response.write(`${event ? `event: ${event}\n` : ''}data: ${JSON.stringify(data)}\n\n`);
  }
  response.end();
}

// A script is a function (requestBody) -> { text } | { call: {name, input} }.
// The tool round trip is driven by what the request already contains.

function responsesEvents({ text, call }, n) {
  const id = `resp_${n}`;
  const base = { id, created_at: 0, model: 'gpt-stub' };
  const usage = { input_tokens: 11, output_tokens: 7, input_tokens_details: { cached_tokens: 0 }, output_tokens_details: { reasoning_tokens: 0 } };
  if (call) {
    const item = { type: 'function_call', id: `fc_${n}`, call_id: `call_${n}`, name: call.name, arguments: JSON.stringify(call.input), status: 'completed' };
    return [
      ['response.created', { type: 'response.created', response: base }],
      ['response.output_item.added', { type: 'response.output_item.added', output_index: 0, item: { ...item, arguments: '', status: 'in_progress' } }],
      ['response.function_call_arguments.delta', { type: 'response.function_call_arguments.delta', item_id: item.id, output_index: 0, delta: item.arguments }],
      ['response.output_item.done', { type: 'response.output_item.done', output_index: 0, item }],
      ['response.completed', { type: 'response.completed', response: { ...base, status: 'completed', output: [item], usage } }],
    ];
  }
  const message = { type: 'message', id: `msg_${n}`, role: 'assistant', status: 'completed', content: [{ type: 'output_text', text, annotations: [] }] };
  return [
    ['response.created', { type: 'response.created', response: base }],
    ['response.output_item.added', { type: 'response.output_item.added', output_index: 0, item: { ...message, status: 'in_progress', content: [] } }],
    ['response.content_part.added', { type: 'response.content_part.added', item_id: message.id, output_index: 0, content_index: 0, part: { type: 'output_text', text: '', annotations: [] } }],
    ['response.output_text.delta', { type: 'response.output_text.delta', item_id: message.id, output_index: 0, content_index: 0, delta: text }],
    ['response.output_item.done', { type: 'response.output_item.done', output_index: 0, item: message }],
    ['response.completed', { type: 'response.completed', response: { ...base, status: 'completed', output: [message], usage } }],
  ];
}

export async function startOpenAiStub({ script, models = [{ id: 'gpt-stub', visibility: 'list' }, { id: 'gpt-hidden', visibility: 'hide' }] }) {
  const requests = [];
  const handle = async (request, response) => {
    if (request.method === 'OPTIONS') { response.writeHead(204, CORS); response.end(); return; }
    const url = new URL(request.url, 'http://x');
    const record = { path: url.pathname, authorization: request.headers.authorization, body: null };
    requests.push(record);
    if (url.pathname === '/v1/models') {
      response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
      response.end(JSON.stringify({ data: models }));
      return;
    }
    if (url.pathname === '/v1/responses') {
      record.body = JSON.parse(await readBody(request));
      sse(response, responsesEvents(script(record.body), requests.length));
      return;
    }
    response.writeHead(404, CORS);
    response.end();
  };
  const { port, close } = await listen(handle);
  return { port, base: `http://127.0.0.1:${port}/v1`, requests, close };
}

export async function startAnthropicStub({ script }) {
  const requests = [];
  const handle = async (request, response) => {
    if (request.method === 'OPTIONS') { response.writeHead(204, CORS); response.end(); return; }
    const url = new URL(request.url, 'http://x');
    const record = { path: url.pathname, headers: request.headers, body: null };
    requests.push(record);
    if (url.pathname === '/v1/models') {
      response.writeHead(200, { ...CORS, 'Content-Type': 'application/json' });
      response.end(JSON.stringify({ data: [{ id: 'claude-stub', type: 'model' }] }));
      return;
    }
    if (url.pathname === '/v1/messages') {
      record.body = JSON.parse(await readBody(request));
      const reply = script(record.body);
      sse(response, [
        ['message_start', { type: 'message_start', message: { id: 'msg_1', type: 'message', role: 'assistant', model: 'claude-stub', content: [], usage: { input_tokens: 13, output_tokens: 1 } } }],
        ['content_block_start', { type: 'content_block_start', index: 0, content_block: { type: 'text', text: '' } }],
        ['content_block_delta', { type: 'content_block_delta', index: 0, delta: { type: 'text_delta', text: reply.text } }],
        ['content_block_stop', { type: 'content_block_stop', index: 0 }],
        ['message_delta', { type: 'message_delta', delta: { stop_reason: 'end_turn', stop_sequence: null }, usage: { output_tokens: 9 } }],
        ['message_stop', { type: 'message_stop' }],
      ]);
      return;
    }
    response.writeHead(404, CORS);
    response.end();
  };
  const { port, close } = await listen(handle);
  return { port, base: `http://127.0.0.1:${port}/v1`, requests, close };
}

export async function startOllamaStub({ cors = true, script = () => ({ text: 'ok' }) } = {}) {
  const requests = [];
  const handle = async (request, response) => {
    const headers = cors ? CORS : {};
    if (request.method === 'OPTIONS') { response.writeHead(204, headers); response.end(); return; }
    const url = new URL(request.url, 'http://x');
    requests.push(url.pathname);
    if (url.pathname === '/v1/models') {
      response.writeHead(200, { ...headers, 'Content-Type': 'application/json' });
      response.end(JSON.stringify({ data: [{ id: 'llama-stub' }] }));
      return;
    }
    if (url.pathname === '/v1/chat/completions') {
      const reply = script(JSON.parse(await readBody(request)));
      response.writeHead(200, { ...headers, 'Content-Type': 'text/event-stream' });
      const chunk = (delta, finish = null, usage) => `data: ${JSON.stringify({ id: 'c1', object: 'chat.completion.chunk', created: 0, model: 'llama-stub', choices: [{ index: 0, delta, finish_reason: finish }], ...(usage ? { usage } : {}) })}\n\n`;
      response.write(chunk({ role: 'assistant', content: reply.text }));
      response.write(chunk({}, 'stop', { prompt_tokens: 5, completion_tokens: 3, total_tokens: 8 }));
      response.write('data: [DONE]\n\n');
      response.end();
      return;
    }
    response.writeHead(404, headers);
    response.end();
  };
  const { port, close } = await listen(handle);
  return { port, url: `http://127.0.0.1:${port}`, requests, close };
}
