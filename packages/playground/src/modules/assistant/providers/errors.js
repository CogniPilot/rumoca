// One place that turns provider failures into explicit UI states.

export class AssistantError extends Error {
    constructor(kind, message, extra = {}) {
        super(message);
        this.kind = kind;
        Object.assign(this, extra);
    }
}

const USAGE_CODES = new Set([
    'subscription_sharing_usage_limit_exceeded',
    'subscription_sharing_unavailable',
]);

function responseErrorCode(body) {
    try {
        const parsed = JSON.parse(body);
        return parsed?.error?.code || parsed?.error?.type || '';
    } catch {
        return '';
    }
}

export function classifyProviderError(error) {
    if (error instanceof AssistantError) return error;
    if (error?.name === 'AbortError') return new AssistantError('cancelled', 'Cancelled.');
    const status = error?.statusCode;
    const code = responseErrorCode(error?.responseBody || '');
    if (USAGE_CODES.has(code)) {
        return new AssistantError('usage_limit', error.message, { code });
    }
    if (status === 401 || status === 403) {
        return new AssistantError('auth', `The provider rejected the credential (${status}). Reconnect or forget it.`);
    }
    if (status === 429) {
        return new AssistantError('rate_limit', 'The provider is rate limiting requests. Try again shortly.');
    }
    if (status) {
        return new AssistantError('provider', error.message);
    }
    if (error instanceof TypeError) {
        return new AssistantError('network', `The provider could not be reached: ${error.message}`);
    }
    return new AssistantError('other', error?.message || String(error));
}
