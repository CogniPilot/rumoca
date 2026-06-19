export interface NotebookPythonSnippet {
    summaryText: string;
    pythonCode: string;
}

function pythonStringLiteral(value: string): string {
    return JSON.stringify(value);
}

function escapeTripleQuotedPython(value: string): string {
    return value.replace(/\\/g, '\\\\').replace(/"""/g, '\\"""');
}

function pythonVariableBaseFromModelName(modelName: string): string {
    const shortName = modelName.split('.').pop() || 'model';
    const normalized = shortName.replace(/[^A-Za-z0-9_]/g, '_');
    if (!normalized) {
        return 'model';
    }
    if (/^[0-9]/.test(normalized)) {
        return `model_${normalized}`;
    }
    return normalized;
}

export function buildNotebookPythonSnippet(
    modelName: string,
    modelicaSource: string,
    outputFile?: string
): NotebookPythonSnippet {
    const summaryText = outputFile
        ? `✓ Model "${modelName}" compiled and saved to: ${outputFile}\n\nCopy the Python code below to a Python cell to use the DAE JSON:`
        : `✓ Model "${modelName}" compiled successfully.\n\nCopy the Python code below to a Python cell to use the DAE JSON:`;
    const variableBase = pythonVariableBaseFromModelName(modelName);
    const daeJsonVariable = `${variableBase}_dae_json`;
    const daeVariable = `${variableBase}_dae`;

    if (outputFile) {
        return {
            summaryText,
            pythonCode: `import json
from pathlib import Path

${daeJsonVariable} = Path(${pythonStringLiteral(outputFile)}).read_text(encoding="utf-8")
${daeVariable} = json.loads(${daeJsonVariable})
print(json.dumps(${daeVariable}, indent=2))`,
        };
    }

    const escapedModelica = escapeTripleQuotedPython(modelicaSource);
    const notebookFilename = `${variableBase}.mo`;
    return {
        summaryText,
        pythonCode: `import json
import rumoca

    ${daeJsonVariable} = rumoca.compile(
    """
${escapedModelica}
""",
    ${pythonStringLiteral(modelName)},
    filename=${pythonStringLiteral(notebookFilename)},
)
${daeVariable} = json.loads(${daeJsonVariable})
print(json.dumps(${daeVariable}, indent=2))`,
    };
}
