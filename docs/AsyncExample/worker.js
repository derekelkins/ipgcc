import { clearCache, contentLength, fetchRange, InputWrapper, ISO9660 } from "./iso9660.js";

onmessage = async (e) => {
    if (e.data.type === "GetDirectoryStructure") {
        const uri = e.data.uri;
        const len = await contentLength(uri);
        postMessage({request: e.data, result: await ISO9660(new InputWrapper(uri), 0, len)});
        clearCache(uri);
    } else if (e.data.type === "GetFile") {
        const uri = e.data.uri;
        const offset = e.data.offset;
        const len = e.data.len;
        const result = (await fetchRange(uri, offset, offset + len)).buffer;
        postMessage({request: e.data, result}, [result]);
    } else {
        console.warn(`Unknown worker request: ${JSON.stringify(e.data)}`);
    }
};
