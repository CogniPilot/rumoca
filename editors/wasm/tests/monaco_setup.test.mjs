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

  return {
    captured: {
      languageConfigurations,
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
      register() {},
      setLanguageConfiguration(languageId, config) {
        languageConfigurations.push([languageId, config]);
      },
      setMonarchTokensProvider() {},
      registerCompletionItemProvider() {
        return { dispose() {} };
      },
      registerDocumentSemanticTokensProvider() {
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
    });
    assert.deepEqual(languageConfigurations.get("jinja2"), {
      comments: {
        blockComment: ["{#", "#}"],
      },
    });
  } finally {
    globalThis.window = originalWindow;
    globalThis.document = originalDocument;
  }
});
