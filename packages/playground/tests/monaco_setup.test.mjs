import test from "node:test";
import assert from "node:assert/strict";

import { setupMonacoWorkspace } from "../src/modules/monaco_setup.js";

class FakeEmitter {
  constructor() {
    this.event = () => {};
  }

  fire() {}
}

function createFakeEditor(options = {}) {
  const model = {
    id: "model-1",
    uri: {
      toString() {
        return "inmemory://model-1";
      },
    },
    getVersionId() {
      return 1;
    },
    getLanguageId() {
      return options.language || "plaintext";
    },
    getValue() {
      return options.value || "";
    },
  };

  return {
    getModel() {
      return model;
    },
    setValue() {},
    layout() {},
    dispose() {},
  };
}

function createFakeMonaco() {
  const languageConfigurations = [];
  const registeredLanguages = [];
  const tokenProviders = [];
  const semanticTokenProviders = [];

  return {
    captured: {
      languageConfigurations,
      registeredLanguages,
      tokenProviders,
      semanticTokenProviders,
    },
    Emitter: FakeEmitter,
    MarkerSeverity: {
      Error: 8,
      Warning: 4,
      Info: 2,
      Hint: 1,
    },
    Uri: {
      parse(value) {
        return {
          value,
          toString() {
            return value;
          },
        };
      },
    },
    languages: {
      CompletionItemKind: {
        Variable: 1,
        Property: 2,
        Field: 3,
        Snippet: 4,
        Text: 5,
        Method: 6,
        Function: 7,
        Constructor: 8,
        Class: 9,
        Interface: 10,
        Module: 11,
        Keyword: 12,
        Constant: 13,
      },
      CompletionItemInsertTextRule: {
        InsertAsSnippet: 4,
      },
      register(language) {
        registeredLanguages.push(language);
      },
      getLanguages() {
        return [];
      },
      setLanguageConfiguration(languageId, config) {
        languageConfigurations.push([languageId, config]);
      },
      setMonarchTokensProvider(languageId, provider) {
        tokenProviders.push([languageId, provider]);
      },
      registerCompletionItemProvider() {
        return { dispose() {} };
      },
      registerDocumentSemanticTokensProvider(languageId, provider) {
        semanticTokenProviders.push([languageId, provider]);
        return { dispose() {} };
      },
      registerHoverProvider() {
        return { dispose() {} };
      },
      registerDefinitionProvider() {
        return { dispose() {} };
      },
      registerCodeActionProvider() {
        return { dispose() {} };
      },
      registerCodeLensProvider() {
        return { dispose() {} };
      },
    },
    editor: {
      defineTheme() {},
      create(_element, options) {
        return createFakeEditor(options);
      },
    },
  };
}

test("setupMonacoWorkspace wires comment metadata for editable languages", async () => {
  const fakeMonaco = createFakeMonaco();
  const originalWindow = globalThis.window;
  const originalDocument = globalThis.document;
  globalThis.window = {};
  globalThis.document = {
    getElementById() {
      return {};
    },
  };

  try {
    setupMonacoWorkspace({
      monaco: fakeMonaco,
      async sendLanguageCommand() {
        return JSON.stringify({});
      },
      layoutAllEditors() {},
    });

    await Promise.resolve();

    const languageConfigurations = new Map(fakeMonaco.captured.languageConfigurations);
    assert.deepEqual(languageConfigurations.get("modelica"), {
      comments: {
        lineComment: "//",
        blockComment: ["/*", "*/"],
      },
      brackets: [
        ["{", "}"],
        ["[", "]"],
        ["(", ")"],
      ],
      autoClosingPairs: [
        { open: "{", close: "}" },
        { open: "[", close: "]" },
        { open: "(", close: ")" },
        { open: '"', close: '"', notIn: ["string", "comment"] },
      ],
    });
    assert.deepEqual(languageConfigurations.get("jinja2"), {
      comments: {
        blockComment: ["{#", "#}"],
      },
    });
    assert.equal(languageConfigurations.get("toml").comments.lineComment, "#");
    assert(
      fakeMonaco.captured.registeredLanguages.some((language) => language?.id === "toml"),
      "expected setup to register a local TOML language",
    );
    assert(
      fakeMonaco.captured.tokenProviders.some(([languageId]) => languageId === "toml"),
      "expected setup to register local TOML tokenization",
    );
  } finally {
    globalThis.window = originalWindow;
    globalThis.document = originalDocument;
  }
});

test("modelica semantic token cache refreshes after model edits", async () => {
  const fakeMonaco = createFakeMonaco();
  const originalWindow = globalThis.window;
  const originalDocument = globalThis.document;
  let semanticTokenRequests = 0;

  globalThis.window = {};
  globalThis.document = {
    getElementById() {
      return {};
    },
  };

  try {
    setupMonacoWorkspace({
      monaco: fakeMonaco,
      async sendLanguageCommand(command) {
        if (command === "rumoca.language.semanticTokenLegend") {
          return JSON.stringify({ tokenTypes: ["function"], tokenModifiers: [] });
        }
        if (command === "rumoca.language.semanticTokens") {
          semanticTokenRequests += 1;
          return JSON.stringify({ data: [0, 2, 9, 0, 0] });
        }
        return JSON.stringify({});
      },
      layoutAllEditors() {},
    });

    const provider = fakeMonaco.captured.semanticTokenProviders
      .find(([languageId]) => languageId === "modelica")?.[1];
    assert(provider, "expected Modelica semantic token provider");

    let version = 1;
    const model = {
      id: "semantic-cache-model",
      uri: {
        toString() {
          return "inmemory://semantic-cache-model";
        },
      },
      getVersionId() {
        return version;
      },
      getValue() {
        return version === 1
          ? '  terminate("Ball has hit the ground");'
          : '  // terminate("Ball has hit the ground");';
      },
    };

    await provider.provideDocumentSemanticTokens(model);
    await provider.provideDocumentSemanticTokens(model);
    assert.equal(
      semanticTokenRequests,
      1,
      "unchanged model version should reuse cached semantic tokens",
    );

    version = 2;
    await provider.provideDocumentSemanticTokens(model);
    assert.equal(
      semanticTokenRequests,
      2,
      "changed model version must request fresh semantic tokens",
    );
  } finally {
    globalThis.window = originalWindow;
    globalThis.document = originalDocument;
  }
});
