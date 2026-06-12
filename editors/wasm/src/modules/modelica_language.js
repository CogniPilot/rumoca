// Shared Modelica language definition for Monaco editors.
//
// Used by the full WASM playground (`monaco_setup.js`) and by the mini
// editors embedded in the mdBook guides (`docs/user-guide/live/rumoca-live.js`).
// Keep this the single source of truth for Modelica tokenization in Monaco.

export function registerModelicaLanguage(monaco) {
    if (monaco.languages.getLanguages().some((lang) => lang.id === 'modelica')) {
        return;
    }
    monaco.languages.register({ id: 'modelica' });
    monaco.languages.setLanguageConfiguration('modelica', {
        comments: {
            lineComment: '//',
            blockComment: ['/*', '*/']
        },
        brackets: [
            ['{', '}'],
            ['[', ']'],
            ['(', ')']
        ],
        autoClosingPairs: [
            { open: '{', close: '}' },
            { open: '[', close: ']' },
            { open: '(', close: ')' },
            { open: '"', close: '"', notIn: ['string', 'comment'] }
        ]
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
}
