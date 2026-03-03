export function setupMonacoWorkspace({ monaco, sendRequest, layoutAllEditors }) {
    let editor;
monaco.languages.register({ id: 'modelica' });
monaco.languages.register({ id: 'jinja2' });

// Jinja2 language definition for template highlighting
monaco.languages.setMonarchTokensProvider('jinja2', {
    defaultToken: '',
    tokenPostfix: '.jinja2',

    keywords: ['for', 'endfor', 'if', 'elif', 'else', 'endif', 'block', 'endblock',
               'extends', 'include', 'import', 'from', 'macro', 'endmacro', 'call', 'endcall',
               'filter', 'endfilter', 'set', 'endset', 'raw', 'endraw', 'with', 'endwith',
               'autoescape', 'endautoescape', 'trans', 'endtrans', 'pluralize',
               'in', 'not', 'and', 'or', 'is', 'as', 'loop', 'recursive', 'scoped'],

    builtins: ['abs', 'attr', 'batch', 'capitalize', 'center', 'default', 'd', 'dictsort',
               'escape', 'e', 'filesizeformat', 'first', 'float', 'forceescape', 'format',
               'groupby', 'indent', 'int', 'join', 'last', 'length', 'list', 'lower', 'map',
               'max', 'min', 'pprint', 'random', 'reject', 'rejectattr', 'replace', 'reverse',
               'round', 'safe', 'select', 'selectattr', 'slice', 'sort', 'string', 'striptags',
               'sum', 'title', 'trim', 'truncate', 'unique', 'upper', 'urlencode', 'urlize',
               'wordcount', 'wordwrap', 'xmlattr', 'tojson', 'range', 'lipsum', 'dict',
               'cycler', 'joiner', 'namespace', 'true', 'false', 'none', 'True', 'False', 'None'],

    operators: ['==', '!=', '<', '>', '<=', '>=', '+', '-', '*', '/', '//', '%', '**', '~', '|'],

    tokenizer: {
        root: [
            // Jinja2 comments {# ... #}
            [/\{#/, 'comment', '@jinjaComment'],

            // Jinja2 statements {% ... %}
            [/\{%-?/, { token: 'delimiter.jinja', next: '@jinjaStatement' }],

            // Jinja2 expressions {{ ... }}
            [/\{\{-?/, { token: 'delimiter.jinja', next: '@jinjaExpression' }],

            // Plain text (anything else)
            [/[^{]+/, ''],
            [/\{/, ''],
        ],

        jinjaComment: [
            [/#\}/, 'comment', '@pop'],
            [/./, 'comment'],
        ],

        jinjaStatement: [
            [/-?%\}/, { token: 'delimiter.jinja', next: '@pop' }],
            [/\b(for|endfor|if|elif|else|endif|block|endblock|extends|include|import|from|macro|endmacro|set|endset|raw|endraw|with|endwith|in|not|and|or|is|as|loop|recursive)\b/, 'keyword'],
            [/\b(true|false|none|True|False|None)\b/, 'constant'],
            [/[a-zA-Z_]\w*/, {
                cases: {
                    '@builtins': 'predefined',
                    '@default': 'variable'
                }
            }],
            [/"([^"\\]|\\.)*"/, 'string'],
            [/'([^'\\]|\\.)*'/, 'string'],
            [/\d+\.?\d*/, 'number'],
            [/[|.]/, 'delimiter'],
            [/[(),\[\]]/, 'delimiter.bracket'],
            [/\s+/, ''],
        ],

        jinjaExpression: [
            [/-?\}\}/, { token: 'delimiter.jinja', next: '@pop' }],
            [/\b(not|and|or|is|in)\b/, 'keyword'],
            [/\b(true|false|none|True|False|None)\b/, 'constant'],
            [/[a-zA-Z_]\w*/, {
                cases: {
                    '@builtins': 'predefined',
                    '@default': 'variable'
                }
            }],
            [/"([^"\\]|\\.)*"/, 'string'],
            [/'([^'\\]|\\.)*'/, 'string'],
            [/\d+\.?\d*/, 'number'],
            [/[|.]/, 'delimiter'],
            [/[(),\[\]]/, 'delimiter.bracket'],
            [/\s+/, ''],
        ],
    }
});

// Store current DAE for dynamic completions
window.currentDaeForCompletions = null;

// Extract dynamic completions from the current DAE
function getDynamicDaeCompletions(dae) {
    if (!dae) return [];
    const completions = [];

    // Component fields that can be accessed on variables
    const componentFields = [
        { name: 'type_name', detail: 'Type name (e.g., Real, Integer)' },
        { name: 'shape', detail: 'Array shape' },
        { name: 'start', detail: 'Start value' },
        { name: 'variability', detail: 'Variability (parameter, constant, etc.)' },
        { name: 'causality', detail: 'Causality (input, output, etc.)' },
        { name: 'description', detail: 'Description string' },
    ];

    // Variable maps to iterate over
    const varMaps = [
        { key: 'x', name: 'State', desc: 'state variable' },
        { key: 'y', name: 'Algebraic', desc: 'algebraic variable' },
        { key: 'p', name: 'Parameter', desc: 'parameter' },
        { key: 'cp', name: 'Constant', desc: 'constant parameter' },
        { key: 'u', name: 'Input', desc: 'input variable' },
        { key: 'z', name: 'Discrete Real', desc: 'discrete Real variable' },
        { key: 'm', name: 'Discrete', desc: 'discrete-valued variable' },
        { key: 'c', name: 'Condition', desc: 'condition variable' },
    ];

    // Add variable name completions for each map
    for (const { key, name, desc } of varMaps) {
        const varMap = dae[key];
        if (varMap && typeof varMap === 'object') {
            for (const varName of Object.keys(varMap)) {
                // Add dae.x.varname style completion
                completions.push({
                    label: `dae.${key}.${varName}`,
                    kind: monaco.languages.CompletionItemKind.Variable,
                    insertText: `dae.${key}.${varName}`,
                    detail: `${name}: ${varName} (${desc})`,
                    sortText: `0_${key}_${varName}` // Sort model vars first
                });

                // Add component field completions (dae.x.varname.start, etc.)
                for (const field of componentFields) {
                    completions.push({
                        label: `dae.${key}.${varName}.${field.name}`,
                        kind: monaco.languages.CompletionItemKind.Property,
                        insertText: `dae.${key}.${varName}.${field.name}`,
                        detail: `${varName}.${field.name} - ${field.detail}`,
                        sortText: `1_${key}_${varName}_${field.name}`
                    });
                }
            }
        }
    }

    return completions;
}

// Jinja2 completion provider for DAE fields
monaco.languages.registerCompletionItemProvider('jinja2', {
    triggerCharacters: ['.', '{'],
    provideCompletionItems: (model, position) => {
        const textUntilPosition = model.getValueInRange({
            startLineNumber: 1,
            startColumn: 1,
            endLineNumber: position.lineNumber,
            endColumn: position.column
        });

        // Get the word being typed
        const lineText = model.getLineContent(position.lineNumber);
        const textBeforeCursor = lineText.substring(0, position.column - 1);

        // Check for context-specific completions
        // Match patterns like "dae.", "dae.x.", "dae.x.varname."
        const daePathMatch = textBeforeCursor.match(/dae\.(\w*)$/);
        const daeVarMatch = textBeforeCursor.match(/dae\.(\w+)\.(\w*)$/);
        const daeFieldMatch = textBeforeCursor.match(/dae\.(\w+)\.(\w+)\.(\w*)$/);

        // Static DAE field suggestions (always available)
        const daeFields = [
            { label: 'dae.model_name', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.model_name', detail: 'Model name', sortText: '2_model_name' },
            { label: 'dae.rumoca_version', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.rumoca_version', detail: 'Rumoca version', sortText: '2_rumoca_version' },
            { label: 'dae.x', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.x', detail: 'State variables (IndexMap)', sortText: '2_x' },
            { label: 'dae.y', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.y', detail: 'Algebraic variables (IndexMap)', sortText: '2_y' },
            { label: 'dae.p', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.p', detail: 'Parameters (IndexMap)', sortText: '2_p' },
            { label: 'dae.cp', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.cp', detail: 'Constant parameters (IndexMap)', sortText: '2_cp' },
            { label: 'dae.u', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.u', detail: 'Inputs (IndexMap)', sortText: '2_u' },
            { label: 'dae.z', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.z', detail: 'Discrete Real variables (IndexMap)', sortText: '2_z' },
            { label: 'dae.m', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.m', detail: 'Discrete-valued variables (IndexMap)', sortText: '2_m' },
            { label: 'dae.c', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.c', detail: 'Conditions (IndexMap)', sortText: '2_c' },
            { label: 'dae.fx', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fx', detail: 'Continuous equations (Vec)', sortText: '2_fx' },
            { label: 'dae.fx_init', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fx_init', detail: 'Initial equations (Vec)', sortText: '2_fx_init' },
            { label: 'dae.fz', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fz', detail: 'Algebraic equations (Vec)', sortText: '2_fz' },
            { label: 'dae.fm', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fm', detail: 'Discrete equations (Vec)', sortText: '2_fm' },
            { label: 'dae.fr', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fr', detail: 'Reset statements (IndexMap)', sortText: '2_fr' },
            { label: 'dae.fc', kind: monaco.languages.CompletionItemKind.Field, insertText: 'dae.fc', detail: 'Condition updates (IndexMap)', sortText: '2_fc' },
        ];

        // Jinja2 snippets
        const snippets = [
            { label: 'for loop', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{% for ${1:item} in ${2:items} %}\n  $0\n{% endfor %}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'For loop', sortText: '3_for' },
            { label: 'for key,val', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{% for ${1:name}, ${2:value} in ${3:dict} %}\n  $0\n{% endfor %}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'For loop over dict items', sortText: '3_for_kv' },
            { label: 'if', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{% if ${1:condition} %}\n  $0\n{% endif %}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'If statement', sortText: '3_if' },
            { label: 'if-else', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{% if ${1:condition} %}\n  $2\n{% else %}\n  $0\n{% endif %}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'If-else statement', sortText: '3_if_else' },
            { label: 'expression', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{{ ${1:expr} }}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'Output expression', sortText: '3_expr' },
            { label: 'comment', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{# ${1:comment} #}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'Comment', sortText: '3_comment' },
            { label: 'set variable', kind: monaco.languages.CompletionItemKind.Snippet, insertText: '{% set ${1:var} = ${2:value} %}', insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet, detail: 'Set variable', sortText: '3_set' },
            { label: 'loop.index', kind: monaco.languages.CompletionItemKind.Property, insertText: 'loop.index', detail: 'Current iteration (1-indexed)', sortText: '4_loop_index' },
            { label: 'loop.index0', kind: monaco.languages.CompletionItemKind.Property, insertText: 'loop.index0', detail: 'Current iteration (0-indexed)', sortText: '4_loop_index0' },
            { label: 'loop.first', kind: monaco.languages.CompletionItemKind.Property, insertText: 'loop.first', detail: 'True if first iteration', sortText: '4_loop_first' },
            { label: 'loop.last', kind: monaco.languages.CompletionItemKind.Property, insertText: 'loop.last', detail: 'True if last iteration', sortText: '4_loop_last' },
            { label: 'loop.length', kind: monaco.languages.CompletionItemKind.Property, insertText: 'loop.length', detail: 'Total number of items', sortText: '4_loop_length' },
        ];

        // Get dynamic completions from current DAE
        const dynamicCompletions = getDynamicDaeCompletions(window.currentDaeForCompletions);

        return { suggestions: [...dynamicCompletions, ...daeFields, ...snippets] };
    }
});

monaco.languages.setMonarchTokensProvider('modelica', {
    keywords: [
        'algorithm', 'and', 'annotation', 'assert', 'block', 'break',
        'class', 'connect', 'connector', 'constant', 'constrainedby',
        'der', 'discrete', 'each', 'else', 'elseif', 'elsewhen',
        'encapsulated', 'end', 'enumeration', 'equation', 'expandable',
        'extends', 'external', 'false', 'final', 'flow', 'for',
        'function', 'if', 'import', 'impure', 'in', 'initial',
        'inner', 'input', 'loop', 'model', 'not', 'operator',
        'or', 'outer', 'output', 'package', 'parameter', 'partial',
        'protected', 'public', 'pure', 'record', 'redeclare',
        'replaceable', 'return', 'stream', 'then', 'true', 'type',
        'when', 'while', 'within'
    ],
    types: ['Real', 'Integer', 'Boolean', 'String'],
    builtins: [
        // Event/state functions
        'der', 'pre', 'edge', 'change', 'initial', 'terminal', 'sample',
        'smooth', 'delay', 'cardinality', 'homotopy', 'semiLinear',
        'inStream', 'actualStream', 'getInstanceName', 'spatialDistribution',
        'reinit', 'assert', 'terminate',
        // Math functions
        'abs', 'sign', 'sqrt', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'atan2',
        'sinh', 'cosh', 'tanh', 'exp', 'log', 'log10',
        'floor', 'ceil', 'mod', 'rem', 'div', 'integer',
        // Array functions
        'size', 'ndims', 'scalar', 'vector', 'matrix', 'transpose',
        'outerProduct', 'symmetric', 'cross', 'skew', 'identity', 'diagonal',
        'zeros', 'ones', 'fill', 'linspace', 'min', 'max', 'sum', 'product', 'cat',
        // Builtin variables/special names frequently used in expressions
        'time', 'noEvent', 'Connections'
    ],
    tokenizer: {
        root: [
            [/[a-zA-Z_]\w*/, {
                cases: {
                    '@keywords': 'keyword',
                    '@types': 'type',
                    '@builtins': 'predefined',
                    '@default': 'identifier'
                }
            }],
            [/[{}()\[\]]/, 'delimiter.bracket'],
            [/[;,.]/, 'delimiter'],
            // Comments must be matched before '/' operator tokens.
            [/\/\/.*$/, 'comment'],
            [/\/\*/, 'comment', '@comment'],
            [/[<>=!]+/, 'operator'],
            [/[+\-*\/^:]/, 'operator'],
            [/\d+\.?\d*([eE][-+]?\d+)?/, 'number'],
            [/"([^"\\]|\\.)*"/, 'string'],
        ],
        comment: [
            [/[^/*]+/, 'comment'],
            [/\*\//, 'comment', '@pop'],
            [/[/*]/, 'comment']
        ],
    }
});

const fallbackSemanticLegend = {
    tokenTypes: [
        'namespace',
        'type',
        'class',
        'parameter',
        'variable',
        'property',
        'function',
        'keyword',
        'comment',
        'string',
        'number',
        'operator'
    ],
    tokenModifiers: ['declaration', 'definition', 'readonly', 'modification']
};
let modelicaSemanticLegend = fallbackSemanticLegend;
const modelicaSemanticTokenEmitter = new monaco.Emitter();
const semanticTokenCache = new Map();

function normalizeSemanticLegend(payload) {
    if (!payload || typeof payload !== 'object') return null;
    const tokenTypes = payload.tokenTypes || payload.token_types;
    const tokenModifiers = payload.tokenModifiers || payload.token_modifiers;
    if (!Array.isArray(tokenTypes) || !Array.isArray(tokenModifiers)) return null;
    return {
        tokenTypes: tokenTypes.map(t => String(t)),
        tokenModifiers: tokenModifiers.map(t => String(t))
    };
}

function semanticTokenCacheKey(model) {
    return model.uri ? model.uri.toString() : `model:${model.id}`;
}

function flattenSemanticTokenData(data) {
    if (!Array.isArray(data) || data.length === 0) return new Uint32Array(0);
    if (typeof data[0] === 'number') return new Uint32Array(data);
    if (typeof data[0] !== 'object' || data[0] === null) return new Uint32Array(0);

    const flattened = new Uint32Array(data.length * 5);
    for (let i = 0; i < data.length; i++) {
        const tok = data[i] || {};
        const base = i * 5;
        flattened[base] = Number(tok.deltaLine ?? tok.delta_line ?? 0);
        flattened[base + 1] = Number(tok.deltaStart ?? tok.delta_start ?? 0);
        flattened[base + 2] = Number(tok.length ?? 0);
        flattened[base + 3] = Number(tok.tokenType ?? tok.token_type ?? 0);
        flattened[base + 4] = Number(tok.tokenModifiersBitset ?? tok.token_modifiers_bitset ?? 0);
    }
    return flattened;
}

function normalizeSemanticTokens(payload) {
    if (!payload || typeof payload !== 'object') {
        return { data: new Uint32Array(0), resultId: null };
    }

    // serde may encode enum variant as { "Tokens": { ... } }
    const tokenSet = payload.Tokens || payload.tokens || payload;
    const data = flattenSemanticTokenData(tokenSet.data || []);
    const resultId = tokenSet.resultId ?? tokenSet.result_id ?? null;
    return { data, resultId };
}

async function loadSemanticLegend() {
    try {
        const legendJson = await sendRequest('semanticTokenLegend', {});
        const parsed = JSON.parse(legendJson);
        const legend = normalizeSemanticLegend(parsed);
        if (legend) {
            modelicaSemanticLegend = legend;
            modelicaSemanticTokenEmitter.fire();
        }
    } catch (e) {
        console.warn('Semantic legend load failed (using fallback):', e);
    }
}

async function requestSemanticTokens(model) {
    const key = semanticTokenCacheKey(model);
    const version = model.getVersionId();
    const cached = semanticTokenCache.get(key);

    if (cached && cached.version === version) {
        if (cached.tokens) return cached.tokens;
        if (cached.pending) return cached.pending;
    }

    const pending = (async () => {
        try {
            const json = await sendRequest('semanticTokens', { source: model.getValue() });
            const parsed = JSON.parse(json);
            return normalizeSemanticTokens(parsed);
        } catch (e) {
            // Parse errors are expected while typing; keep lexical highlighting.
            return { data: new Uint32Array(0), resultId: null };
        }
    })();

    semanticTokenCache.set(key, { version, pending });
    const tokens = await pending;
    const latest = semanticTokenCache.get(key);
    if (latest && latest.version === version && latest.pending === pending) {
        // Don't pin empty results in cache: they are often transient while typing
        // or during startup race before worker readiness.
        if (tokens.data && tokens.data.length > 0) {
            semanticTokenCache.set(key, { version, tokens });
        } else {
            semanticTokenCache.delete(key);
        }
    }
    return tokens;
}

window.refreshModelicaSemanticTokens = () => {
    const model = window.editor && window.editor.getModel ? window.editor.getModel() : null;
    if (model) {
        semanticTokenCache.delete(semanticTokenCacheKey(model));
    }
    modelicaSemanticTokenEmitter.fire();
};

monaco.languages.registerDocumentSemanticTokensProvider('modelica', {
    onDidChange: modelicaSemanticTokenEmitter.event,
    getLegend() {
        return modelicaSemanticLegend;
    },
    provideDocumentSemanticTokens(model) {
        return requestSemanticTokens(model);
    },
    releaseDocumentSemanticTokens() {}
});

loadSemanticLegend();

monaco.languages.registerHoverProvider('modelica', {
    provideHover: async (model, position) => {
        try {
            const source = model.getValue();
            const json = await sendRequest('hover', {
                source,
                line: position.lineNumber - 1,
                character: position.column - 1
            });
            const hover = JSON.parse(json);
            if (hover && hover.contents) {
                const content = typeof hover.contents === 'string'
                    ? hover.contents
                    : (hover.contents.value || JSON.stringify(hover.contents));
                return { contents: [{ value: content }] };
            }
        } catch (e) { console.warn('Hover error:', e); }
        return null;
    }
});

monaco.languages.registerCompletionItemProvider('modelica', {
    triggerCharacters: ['.', ' ', '(', ','],
    provideCompletionItems: async (model, position) => {
        try {
            const source = model.getValue();
            const json = await sendRequest('completion', {
                source,
                line: position.lineNumber - 1,
                character: position.column - 1
            });
            const completions = JSON.parse(json);
            const items = completions?.items || completions || [];
            const kindMap = {
                1: monaco.languages.CompletionItemKind.Text,
                2: monaco.languages.CompletionItemKind.Method,
                3: monaco.languages.CompletionItemKind.Function,
                4: monaco.languages.CompletionItemKind.Constructor,
                5: monaco.languages.CompletionItemKind.Field,
                6: monaco.languages.CompletionItemKind.Variable,
                7: monaco.languages.CompletionItemKind.Class,
                8: monaco.languages.CompletionItemKind.Interface,
                9: monaco.languages.CompletionItemKind.Module,
                10: monaco.languages.CompletionItemKind.Property,
                14: monaco.languages.CompletionItemKind.Keyword,
                21: monaco.languages.CompletionItemKind.Constant,
            };
            return {
                suggestions: items.map(item => {
                    const suggestion = {
                        label: item.label,
                        kind: kindMap[item.kind] || monaco.languages.CompletionItemKind.Text,
                        insertText: item.insertText || item.label,
                        detail: item.detail,
                        documentation: item.documentation
                    };
                    // If insertTextFormat is 2 (Snippet), tell Monaco to interpret as snippet
                    if (item.insertTextFormat === 2) {
                        suggestion.insertTextRules = monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet;
                    }
                    return suggestion;
                })
            };
        } catch (e) { console.warn('Completion error:', e); }
        return { suggestions: [] };
    }
});

function markerSeverityToLspSeverity(severity) {
    if (severity === monaco.MarkerSeverity.Error) return 1;
    if (severity === monaco.MarkerSeverity.Warning) return 2;
    if (severity === monaco.MarkerSeverity.Info) return 3;
    return 4;
}

function markerCodeToLspCode(code) {
    if (code === undefined || code === null) return undefined;
    if (typeof code === 'string' || typeof code === 'number') return code;
    if (typeof code === 'object') {
        if (typeof code.value === 'string' || typeof code.value === 'number') {
            return code.value;
        }
    }
    return String(code);
}

function markerToLspDiagnostic(marker) {
    if (!marker) return null;
    return {
        range: {
            start: {
                line: Math.max(0, Number(marker.startLineNumber || 1) - 1),
                character: Math.max(0, Number(marker.startColumn || 1) - 1),
            },
            end: {
                line: Math.max(0, Number(marker.endLineNumber || marker.startLineNumber || 1) - 1),
                character: Math.max(0, Number(marker.endColumn || marker.startColumn || 1) - 1),
            },
        },
        severity: markerSeverityToLspSeverity(marker.severity),
        code: markerCodeToLspCode(marker.code),
        source: 'rumoca',
        message: String(marker.message || ''),
    };
}

function lspRangeToMonacoRange(range) {
    const start = range?.start || {};
    const end = range?.end || {};
    return {
        startLineNumber: Math.max(1, Number(start.line ?? 0) + 1),
        startColumn: Math.max(1, Number(start.character ?? 0) + 1),
        endLineNumber: Math.max(1, Number(end.line ?? start.line ?? 0) + 1),
        endColumn: Math.max(1, Number(end.character ?? start.character ?? 0) + 1),
    };
}

function definitionUriToMonacoUri(uri, model) {
    if (!uri || uri === 'file:///input.mo') {
        return model.uri;
    }
    try {
        return monaco.Uri.parse(uri);
    } catch (_) {
        return model.uri;
    }
}

function lspLocationToMonacoDefinition(location, model) {
    if (!location || typeof location !== 'object' || !location.range) return null;
    return {
        uri: definitionUriToMonacoUri(location.uri, model),
        range: lspRangeToMonacoRange(location.range),
    };
}

function lspLocationLinkToMonacoDefinition(link, model) {
    if (!link || typeof link !== 'object') return null;
    const range = link.targetSelectionRange || link.targetRange;
    if (!range) return null;
    return {
        uri: definitionUriToMonacoUri(link.targetUri, model),
        range: lspRangeToMonacoRange(range),
    };
}

function normalizeDefinitionResult(definition, model) {
    if (!definition) return [];

    if (definition.Scalar) {
        return normalizeDefinitionResult(definition.Scalar, model);
    }
    if (Array.isArray(definition.Array)) {
        return normalizeDefinitionResult(definition.Array, model);
    }

    if (Array.isArray(definition)) {
        return definition
            .map(item => (
                item && (item.targetUri || item.targetRange || item.targetSelectionRange)
                    ? lspLocationLinkToMonacoDefinition(item, model)
                    : lspLocationToMonacoDefinition(item, model)
            ))
            .filter(Boolean);
    }

    if (definition.targetUri || definition.targetRange || definition.targetSelectionRange) {
        const mapped = lspLocationLinkToMonacoDefinition(definition, model);
        return mapped ? [mapped] : [];
    }

    const mapped = lspLocationToMonacoDefinition(definition, model);
    return mapped ? [mapped] : [];
}

function lspWorkspaceEditToMonacoEdit(workspaceEdit, model) {
    const changes = workspaceEdit?.changes;
    if (!changes || typeof changes !== 'object') return null;
    const edits = [];
    for (const [uri, textEdits] of Object.entries(changes)) {
        const resource = uri === 'file:///input.mo'
            ? model.uri
            : monaco.Uri.parse(uri);
        if (!Array.isArray(textEdits)) continue;
        for (const textEdit of textEdits) {
            edits.push({
                resource,
                textEdit: {
                    range: lspRangeToMonacoRange(textEdit?.range),
                    text: String(textEdit?.newText ?? textEdit?.text ?? ''),
                },
            });
        }
    }
    return edits.length > 0 ? { edits } : null;
}

function lspCodeActionToMonacoAction(action, model) {
    const edit = lspWorkspaceEditToMonacoEdit(action?.edit, model);
    if (!edit) return null;
    return {
        title: String(action?.title || 'Quick Fix'),
        kind: String(action?.kind || 'quickfix'),
        isPreferred: Boolean(action?.isPreferred),
        edit,
    };
}

monaco.languages.registerDefinitionProvider('modelica', {
    provideDefinition: async (model, position) => {
        try {
            const source = model.getValue();
            const json = await sendRequest('definition', {
                source,
                line: position.lineNumber - 1,
                character: position.column - 1
            });
            const parsed = JSON.parse(json);
            const definitions = normalizeDefinitionResult(parsed, model);
            if (!definitions.length) return null;
            return definitions.length === 1 ? definitions[0] : definitions;
        } catch (error) {
            console.warn('Definition error:', error);
            return null;
        }
    }
});

monaco.languages.registerCodeActionProvider('modelica', {
    provideCodeActions: async (model, range, context) => {
        const markers = Array.isArray(context?.markers) ? context.markers : [];
        const diagnostics = markers
            .map(markerToLspDiagnostic)
            .filter(Boolean);
        try {
            const json = await sendRequest('codeActions', {
                source: model.getValue(),
                rangeStartLine: Math.max(0, range.startLineNumber - 1),
                rangeStartCharacter: Math.max(0, range.startColumn - 1),
                rangeEndLine: Math.max(0, range.endLineNumber - 1),
                rangeEndCharacter: Math.max(0, range.endColumn - 1),
                diagnosticsJson: JSON.stringify(diagnostics),
            });
            const parsed = JSON.parse(json);
            const lspActions = Array.isArray(parsed) ? parsed : [];
            const actions = lspActions
                .map(action => lspCodeActionToMonacoAction(action, model))
                .filter(Boolean);
            return { actions, dispose: () => {} };
        } catch (error) {
            console.warn('Code action error:', error);
            return { actions: [], dispose: () => {} };
        }
    }
});

monaco.editor.defineTheme('rumoca-dark', {
    base: 'vs-dark',
    inherit: true,
    semanticHighlighting: true,
    rules: [
        { token: 'keyword', foreground: '569CD6' },
        { token: 'function', foreground: 'DCDCAA' },
        { token: 'parameter', foreground: '9CDCFE' },
        { token: 'variable', foreground: '9CDCFE' },
        { token: 'predefined', foreground: '4EC9B0' },
        { token: 'type', foreground: '4FC1FF' },
        { token: 'comment', foreground: '6A9955' }
    ],
    semanticTokenColors: {
        keyword: '#569CD6',
        function: '#DCDCAA',
        parameter: '#9CDCFE',
        variable: '#9CDCFE',
        type: '#4FC1FF',
        namespace: '#C586C0',
        property: '#4EC9B0'
    },
    colors: {}
});

editor = monaco.editor.create(document.getElementById('editor'), {
    value: `model Ball
  Real x(start=0);
  Real v(start=1);
equation
  der(x) = v;
  der(v) = -9.81;
end Ball;`,
    language: 'modelica',
    theme: 'rumoca-dark',
    minimap: { enabled: false },
    fontSize: 14,
    lineNumbers: 'on',
    lineNumbersMinChars: 3,
    automaticLayout: true,
    quickSuggestions: true,
    suggestOnTriggerCharacters: true,
    glyphMargin: false,
    folding: true,
    semanticHighlighting: true,
    'semanticHighlighting.enabled': true
});

window.editor = editor;

// Create template editor with Jinja2 highlighting
// Default template: CasADi (from examples/templates/casadi.jinja)
const defaultTemplate = `{%- macro render_dae(dae) -%}
{%- set vars_vects = ['u', 'p', 'cp', 'x', 'y', 'z'] -%}
"""
Generated by Rumoca
rumoca pkg version : {{ dae.rumoca_version }} - {{ dae.git_version }}
model hash : {{ dae.model_hash }}
template hash : {{ dae.template_hash }}
"""

import casadi as ca
import numpy as np

cos = ca.cos
sin = ca.sin
tan = ca.tan


class Model:
"""
Flattened Modelica Model
"""

def __init__(self):
pass

def __repr__(self):
return repr(self.__dict__)

def simulate(self, t=None, u=None):
"""
Simulate the modelica model
"""
if t is None:
t = np.arange(0, 1, 0.01)
if u is None:
u = 0

# ============================================
# Declare time
time = ca.SX.sym('time')

{%- for var in vars_vects %}

# ============================================
# Declare {{ var }}

{% for name, comp in dae[var] | items -%}
{{ name }} = ca.SX.sym('{{ name }}')
{% endfor -%}

self.{{var }} {{ "= ca.vertcat(" }}{%- for name, comp in dae[var] | items %}
{{ name }} {%- if not loop.last -%}{{ ", " }}{%- endif -%}
{% endfor -%} {{ ")" }}

self.{{ var }}0 = {{ "{" }} {% for name, comp in dae[var] | items %}
'{{ name }}': {{ render_expression(comp.start) }} {%- if not loop.last -%}{{ ", " }}{%- endif -%}
{%- endfor -%}{{ "}" }}
{{ var }}0 = np.array([self.{{ var }}0[k] for k in self.{{ var }}0.keys()])


{%- endfor %}

# ============================================
# Define Continous Update Function: fx
{% for eq in dae.fx -%}
{{ render_equation(eq) }}
{% endfor%}

# ============================================
# Create Integrator
F = ca.integrator(
'F', 'idas',
{'x': self.x, 'z': self.z, 'p': self.p, 'u': self.u, 'ode': self.ode, 'alg': self.alg},
t[0], t)

res = F(x0=x0, z0=z0, p=p0, u=u)
return {
't': t,
'x': res['xf'].T
}

def linearize(self):
"""
Linearize the model
"""
A = ca.jacobian(self.ode, self.x)
B = ca.jacobian(self.ode, self.u)
C = ca.jacobian(self.y, self.x)
D = ca.jacobian(self.y, self.u)
return (A, B, C, D)


def cat(axis, *args):
return ca.vertcat(*args)
{%- endmacro -%}

{%- macro render_expression(expr) -%}
{%- if "Terminal" in expr -%}
{{- expr.Terminal.token.text -}}
{%- elif "FunctionCall" in expr -%}
{{- render_function(expr.FunctionCall) -}}
{%- elif "ComponentReference" in expr -%}
{{- render_component_reference(expr.ComponentReference) -}}
{%- elif "Binary" in expr -%}
{{- render_binary(expr.Binary) -}}
{%- elif "Unary" in expr -%}
{{- render_unary(expr.Unary) -}}
{%- else -%}
UNHANDLED EXPRESSION: {{ expr }}
{%- endif -%}
{%- endmacro -%}

{%- macro render_equation(eq) -%}
{%- if "Simple" in eq -%}
{{- render_expression(eq.Simple.lhs) -}} {{- " = " -}}
{{- render_expression(eq.Simple.rhs) -}}
{%- endif -%}
{%- endmacro -%}

{%- macro render_terminal(term) -%}
{% if term.terimanal_type == "UnsignedInteger" %}
{{ term.token.text | float }}
{% elif term.terimanal_type == "UnsignedReal" %}
{{ term.token.text | float }}
{% endif %}
{%- endmacro -%}

{%- macro render_binary(expr) -%}
{{- render_expression(expr.lhs) -}} {{- " " -}}
{%- if "Add" in expr.op -%}
{{ "+" }}
{%- elif "Sub" in expr.op -%}
{{ "-" }}
{%- elif "Mul" in expr.op -%}
{{ "*" }}
{%- elif "Div" in expr.op -%}
{{ "/" }}
{%- else -%}
UNHANDLED OP: {{ expr.op }}
{%- endif -%}
{{- " " -}} {{- render_expression(expr.rhs) -}}
{%- endmacro -%}


{%- macro render_unary(expr) -%}
{{ expr.op.text }} {{ render_expression(expr.rhs) }}
{%- endmacro -%}

{%- macro render_component_reference(comp) -%}
{%- for part in comp.parts -%}
{{ part.ident.text }}{% if not loop.last %}.{% endif %}
{%- endfor -%}
{%- endmacro -%}

{%- macro render_function(func) -%}
{{ render_component_reference(func.comp) }} {{- "(" -}}{%- for arg in func.args -%}
{{- render_expression(arg) -}} {%- if not loop.last -%}, {%- endif -%}
{%- endfor -%}{{ ")" }}
{%- endmacro -%}

{{ render_dae(dae) }}
`;

const templateEditor = monaco.editor.create(document.getElementById('templateEditor'), {
    value: defaultTemplate,
    language: 'jinja2',
    theme: 'vs-dark',
    minimap: { enabled: false },
    fontSize: 13,
    lineNumbers: 'on',
    lineNumbersMinChars: 3,
    automaticLayout: true,
    quickSuggestions: true,
    suggestOnTriggerCharacters: true,
    wordWrap: 'on',
    folding: true
});

window.templateEditor = templateEditor;

// Create read-only output editor with JSON/text highlighting
const outputEditor = monaco.editor.create(document.getElementById('outputEditor'), {
    value: '',
    language: 'plaintext',
    theme: 'vs-dark',
    readOnly: true,
    minimap: { enabled: false },
    fontSize: 13,
    lineNumbers: 'on',
    lineNumbersMinChars: 3,
    automaticLayout: true,
    wordWrap: 'on',
    folding: true,
    scrollBeyondLastLine: false,
    renderLineHighlight: 'none',
    occurrencesHighlight: 'off',
    selectionHighlight: false
});

window.outputEditor = outputEditor;

// Create read-only codegen output editor
const codegenOutputEditor = monaco.editor.create(document.getElementById('codegenOutputEditor'), {
    value: '',
    language: 'plaintext',
    theme: 'vs-dark',
    readOnly: true,
    minimap: { enabled: false },
    fontSize: 13,
    lineNumbers: 'on',
    lineNumbersMinChars: 3,
    automaticLayout: true,
    wordWrap: 'on',
    folding: true,
    scrollBeyondLastLine: false,
    renderLineHighlight: 'none',
    occurrencesHighlight: 'off',
    selectionHighlight: false
});

window.codegenOutputEditor = codegenOutputEditor;

// Codegen tab vertical resize between template and output
const resizeHandleCodegen = document.getElementById('resizeHandleCodegen');
let isResizingCodegen = false;
resizeHandleCodegen.addEventListener('mousedown', (e) => {
    isResizingCodegen = true;
    document.body.style.cursor = 'ns-resize';
    document.body.style.userSelect = 'none';
    e.preventDefault();
});
document.addEventListener('mousemove', (e) => {
    if (!isResizingCodegen) return;
    const codegenTab = document.getElementById('codegenTab');
    const tabRect = codegenTab.getBoundingClientRect();
    const templateEl = document.getElementById('templateEditor');
    const codegenEl = document.getElementById('codegenOutputEditor');
    const relY = e.clientY - tabRect.top;
    const totalH = tabRect.height;
    const ratio = Math.max(0.15, Math.min(0.85, relY / totalH));
    templateEl.style.flex = ratio.toString();
    codegenEl.style.flex = (1 - ratio).toString();
    layoutAllEditors();
});
document.addEventListener('mouseup', () => {
    if (isResizingCodegen) {
        isResizingCodegen = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
        layoutAllEditors();
    }
});

// Preset template definitions
const templatePresets = {
    casadi_sx: null, // Will be initialized with the default template content
    julia_mtk: `{# Julia ModelingToolkit template #}
using ModelingToolkit

@variables t
D = Differential(t)

{% for name, comp in dae.x %}
@variables {{ name }}(t){% if comp.start and comp.start != "Empty" %} = {{ comp.start }}{% endif %}

{% endfor %}
{% for name, comp in dae.p %}
@parameters {{ name }}{% if comp.start and comp.start != "Empty" %} = {{ comp.start }}{% endif %}

{% endfor %}
eqs = [
{% for eq in dae.fx %}
{{ eq }},
{% endfor %}
]

@named model = ODESystem(eqs, t)`,
    c_code: `{# C Code template #}
#include <math.h>

// Model: {{ dae.model_name }}
// States: {{ dae.x | length }}
// Parameters: {{ dae.p | length }}

{% for name, comp in dae.x %}
#define IDX_{{ name | upper }} {{ loop.index0 }}
{% endfor %}

void rhs(double t, const double *x, const double *p, double *dx) {
{% for eq in dae.fx %}
// {{ eq }}
{% endfor %}
}`,
    jax: `{# JAX template #}
import jax
import jax.numpy as jnp
from functools import partial

# Model: {{ dae.model_name }}

@partial(jax.jit)
def rhs(t, x, p):
{% for name, comp in dae.x %}
{{ name }} = x[{{ loop.index0 }}]
{% endfor %}
{% for name, comp in dae.p %}
{{ name }} = p[{{ loop.index0 }}]
{% endfor %}

dx = jnp.zeros({{ dae.x | length }})
{% for eq in dae.fx %}
# {{ eq }}
{% endfor %}
return dx

# Initial state
x0 = jnp.array([{% for name, comp in dae.x %}{{ comp.start if comp.start and comp.start != "Empty" else "0.0" }}{% if not loop.last %}, {% endif %}{% endfor %}])
# Parameters
p0 = jnp.array([{% for name, comp in dae.p %}{{ comp.start if comp.start and comp.start != "Empty" else "0.0" }}{% if not loop.last %}, {% endif %}{% endfor %}])`
};

// Store initial CasADi template from the template editor's default value
templatePresets.casadi_sx = templateEditor.getValue();

// Load template preset
window.loadTemplatePreset = function() {
    const preset = document.getElementById('templatePresetSelect').value;
    if (preset === 'custom') return; // Don't overwrite user's custom template
    const template = templatePresets[preset];
    if (template && window.templateEditor) {
        window.templateEditor.setValue(template);
    }
};

// CodeLens event emitter for triggering refresh
const codeLensEmitter = new monaco.Emitter();
window.refreshCodeLens = () => codeLensEmitter.fire();

monaco.languages.registerCodeLensProvider('modelica', {
    onDidChange: codeLensEmitter.event,
    provideCodeLenses: (model) => {
        const lenses = [];
        const text = model.getValue();
        const lines = text.split('\n');
        const classRegex = /^(model|class|block|connector|record)\s+(\w+)/;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();
            const match = classRegex.exec(line);
            if (match) {
                const modelName = match[2];
                const compiled = window.compiledModels?.[modelName];

                if (compiled?.balance) {
                    const b = compiled.balance;
                    const statusIcon = b.is_balanced ? '\u2713' : (b.status === 'Partial' ? '~' : '\u2717');
                    const statusText = typeof b.status === 'string' ? b.status.toLowerCase() :
                        (b.status?.CompileError ? 'error' : 'unknown');
                    lenses.push({
                        range: { startLineNumber: i + 1, startColumn: 1, endLineNumber: i + 1, endColumn: 1 },
                        command: {
                            id: 'showBalance',
                            title: `[${statusIcon}] ${modelName}: ${b.num_equations} eqs / ${b.num_unknowns} unknowns (${statusText})`
                        }
                    });
                } else if (compiled?.error) {
                    lenses.push({
                        range: { startLineNumber: i + 1, startColumn: 1, endLineNumber: i + 1, endColumn: 1 },
                        command: {
                            id: 'showBalance',
                            title: `[\u2717] ${modelName}: compile error`
                        }
                    });
                }
            }
        }
        return { lenses, dispose: () => {} };
    }
});

    return { editor, templateEditor, outputEditor, codegenOutputEditor };
}
