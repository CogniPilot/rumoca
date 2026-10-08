import { createProposalStore } from './proposals.js';
import { createDocsRetrieval } from './docs_index.js';
import { createWorkspaceTools } from './workspace_tools.js';
import { createBuildTools } from './build_tools.js';
import { createConfigTools } from './config_tools.js';
import { createDocsTools } from './docs_tools.js';

// Every tool is a provider-neutral definition { name, description, parameters
// (JSON schema), run(input) } implemented over the `host` the playground
// supplies; the tools hold no playground state of their own.
export async function createToolbox({ host }) {
    const docs = await createDocsRetrieval();
    const proposals = createProposalStore({ host });
    const tools = [
        ...createWorkspaceTools({ host, proposals }),
        ...createBuildTools({ host, docs }),
        ...createConfigTools({ host, proposals }),
        ...createDocsTools({ docs }),
    ];
    return { tools, proposals };
}
