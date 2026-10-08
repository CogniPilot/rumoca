// Per-device persistence for assistant connections. Everything lives in this
// browser's IndexedDB; nothing is sent to or kept by the site.

const DB_NAME = 'rumoca-assistant';
const STORE_NAME = 'kv';

let dbPromise = null;

function openDb() {
    if (dbPromise) return dbPromise;
    dbPromise = new Promise((resolve, reject) => {
        const request = indexedDB.open(DB_NAME, 1);
        request.onupgradeneeded = () => request.result.createObjectStore(STORE_NAME);
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => {
            dbPromise = null;
            reject(new Error(`Browser storage is unavailable: ${request.error?.message || 'open failed'}`));
        };
    });
    return dbPromise;
}

async function run(mode, action) {
    const db = await openDb();
    return await new Promise((resolve, reject) => {
        const transaction = db.transaction(STORE_NAME, mode);
        const request = action(transaction.objectStore(STORE_NAME));
        transaction.oncomplete = () => resolve(request.result);
        transaction.onerror = () => reject(new Error(`Browser storage failed: ${transaction.error?.message}`));
    });
}

export const kv = {
    get: async (key) => (await run('readonly', (store) => store.get(key))) ?? null,
    set: (key, value) => run('readwrite', (store) => store.put(value, key)),
    delete: (key) => run('readwrite', (store) => store.delete(key)),
    async deletePrefix(prefix) {
        const keys = await run('readonly', (store) => store.getAllKeys());
        await Promise.all(keys.filter((key) => String(key).startsWith(prefix)).map((key) => kv.delete(key)));
    },
};
