import test from "node:test";
import assert from "node:assert/strict";

import { buildNotebookPythonSnippet } from "../out/notebook_python_snippets.js";

test("saved output snippets load DAE JSON instead of recompiling", () => {
  const snippet = buildNotebookPythonSnippet(
    "Ball",
    "model Ball\nend Ball;\n",
    "/tmp/ball.json",
  );

  assert.match(snippet.summaryText, /saved to: \/tmp\/ball\.json/);
  assert.match(snippet.pythonCode, /import json/);
  assert.match(snippet.pythonCode, /from pathlib import Path/);
  assert.match(snippet.pythonCode, /Path\("\/tmp\/ball\.json"\)\.read_text/);
  assert.match(snippet.pythonCode, /json\.loads\(Ball_dae_json\)/);
  assert.doesNotMatch(snippet.pythonCode, /rumoca\.compile/);
});

test("inline snippets call the canonical rumoca compile API and decode JSON", () => {
  const snippet = buildNotebookPythonSnippet(
    "Pkg.Ball",
    'model Ball\n  String note = """triple""";\nend Ball;\n',
  );

  assert.match(snippet.summaryText, /compiled successfully/);
  assert.match(snippet.pythonCode, /import rumoca/);
  assert.match(snippet.pythonCode, /import json/);
  assert.match(snippet.pythonCode, /rumoca\.compile\(/);
  assert.match(snippet.pythonCode, /"Pkg\.Ball"/);
  assert.match(snippet.pythonCode, /filename="Ball\.mo"/);
  assert.match(snippet.pythonCode, /json\.loads\(Ball_dae_json\)/);
  assert.match(snippet.pythonCode, /\\"""triple\\"""/);
  assert.doesNotMatch(snippet.pythonCode, /to_base_modelica_dict/);
});

test("invalid model names are normalized into usable Python variables", () => {
  const snippet = buildNotebookPythonSnippet(
    "3DOF-Model",
    "model ThreeDofModel\nend ThreeDofModel;\n",
  );

  assert.match(snippet.pythonCode, /model_3DOF_Model_dae_json/);
  assert.match(snippet.pythonCode, /model_3DOF_Model_dae = json\.loads/);
});
