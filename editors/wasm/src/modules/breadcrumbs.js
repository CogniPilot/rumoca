function findEnclosingClass(lines, lineNumber) {
    const classRegex = /^\s*(model|class|block|connector|record|function|package|type)\s+([A-Za-z_]\w*)\b/;
    for (let i = Math.min(lineNumber - 1, lines.length - 1); i >= 0; i--) {
        const match = classRegex.exec(lines[i]);
        if (match) {
            return { kind: match[1], name: match[2] };
        }
    }
    return null;
}

function findNearestSection(lines, lineNumber) {
    const sectionPatterns = [
        { re: /^\s*initial\s+equation\b/i, label: 'initial equation' },
        { re: /^\s*initial\s+algorithm\b/i, label: 'initial algorithm' },
        { re: /^\s*equation\b/i, label: 'equation' },
        { re: /^\s*algorithm\b/i, label: 'algorithm' },
        { re: /^\s*protected\b/i, label: 'protected' },
        { re: /^\s*public\b/i, label: 'public' },
        { re: /^\s*annotation\b/i, label: 'annotation' },
    ];

    for (let i = Math.min(lineNumber - 1, lines.length - 1); i >= 0; i--) {
        const line = lines[i];
        for (const pattern of sectionPatterns) {
            if (pattern.re.test(line)) return pattern.label;
        }
    }
    return null;
}

function currentSymbolAtPosition(model, position) {
    if (!model || !position) return '';
    const word = model.getWordAtPosition(position);
    if (word && word.word) return word.word;

    const lineText = model.getLineContent(position.lineNumber || 1);
    if (!lineText) return '';
    const trimmed = lineText.trim();
    if (!trimmed) return '';
    return trimmed.length > 24 ? `${trimmed.slice(0, 24)}...` : trimmed;
}

export function createSourceBreadcrumbs() {
    return {
        update() {
            const breadcrumbs = document.getElementById('sourceBreadcrumbs');
            if (!breadcrumbs || !window.editor || !window.editor.getModel) return;

            const model = window.editor.getModel();
            if (!model) {
                breadcrumbs.textContent = '';
                return;
            }

            const pos = window.editor.getPosition() || { lineNumber: 1, column: 1 };
            const lines = model.getValue().split('\n');
            const cls = findEnclosingClass(lines, pos.lineNumber);
            const section = findNearestSection(lines, pos.lineNumber);
            const symbol = currentSymbolAtPosition(model, pos);

            const parts = [];
            if (cls) {
                parts.push(`${cls.kind} ${cls.name}`);
            } else {
                parts.push('source');
            }
            if (section) parts.push(section);
            if (symbol) parts.push(symbol);
            breadcrumbs.textContent = parts.join(' > ');
        },
    };
}
