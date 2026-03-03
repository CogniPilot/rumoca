export function installFileActions({ getEditor, getCompiledModels, getDaeFormat, getCodegenOutputEditor }) {
    window.saveFile = function() {
        const editor = getEditor();
        const content = editor && editor.getValue ? editor.getValue() : '';
        const match = content.match(/(?:model|class|block|connector|record|function)\s+(\w+)/);
        const filename = match ? `${match[1]}.mo` : 'Model.mo';
        const blob = new Blob([content], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = filename;
        document.body.appendChild(anchor);
        anchor.click();
        document.body.removeChild(anchor);
        URL.revokeObjectURL(url);
    };

    window.saveOutput = function() {
        const modelName = document.getElementById('modelSelect').value;
        const compiledModels = getCompiledModels();
        if (!modelName || !compiledModels[modelName]) {
            alert('No output available yet. Please select a model and wait for compilation.');
            return;
        }

        const result = compiledModels[modelName];
        if (result.error || !result.dae) {
            alert('No output available - model has compilation errors.');
            return;
        }

        let content;
        let filename;
        let mimeType;
        if (getDaeFormat() === 'pretty') {
            content = result.pretty || JSON.stringify(result.dae, null, 2);
            filename = `${modelName}.txt`;
            mimeType = 'text/plain';
        } else {
            content = JSON.stringify(result.dae_native, null, 2);
            filename = `${modelName}.json`;
            mimeType = 'application/json';
        }

        const blob = new Blob([content], { type: mimeType });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = filename;
        document.body.appendChild(anchor);
        anchor.click();
        document.body.removeChild(anchor);
        URL.revokeObjectURL(url);
    };

    window.saveCodegenOutput = function() {
        const modelName = document.getElementById('modelSelect').value;
        const codegenOutputEditor = getCodegenOutputEditor();
        const content = codegenOutputEditor && codegenOutputEditor.getValue
            ? codegenOutputEditor.getValue()
            : '';
        if (!content || content.startsWith('Enter') || content.startsWith('No DAE')) {
            alert('No valid codegen output to save.');
            return;
        }
        const filename = `${modelName || 'output'}.txt`;
        const blob = new Blob([content], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = filename;
        document.body.appendChild(anchor);
        anchor.click();
        document.body.removeChild(anchor);
        URL.revokeObjectURL(url);
    };
}
