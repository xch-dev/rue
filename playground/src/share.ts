// Snippets are shared in the URL hash, as deflated JSON in base64url, so nothing is stored on a
// server. The hash looks like `#code=...`.

export interface Snippet {
  source: string;
  solution?: string;
  debug?: boolean;
}

const PREFIX = "#code=";

function toBase64Url(bytes: Uint8Array): string {
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function fromBase64Url(text: string): Uint8Array {
  const base64 = text.replace(/-/g, "+").replace(/_/g, "/");
  const binary = atob(base64 + "=".repeat((4 - (base64.length % 4)) % 4));
  return Uint8Array.from(binary, (char) => char.charCodeAt(0));
}

async function transform(
  bytes: Uint8Array,
  stream: CompressionStream | DecompressionStream,
): Promise<Uint8Array> {
  const output = new Blob([bytes as BlobPart]).stream().pipeThrough(stream);
  return new Uint8Array(await new Response(output).arrayBuffer());
}

export async function encodeSnippet(snippet: Snippet): Promise<string> {
  const json = new TextEncoder().encode(JSON.stringify(snippet));
  const compressed = await transform(json, new CompressionStream("deflate-raw"));
  return PREFIX + toBase64Url(compressed);
}

/** Reads the snippet in a hash, or returns `null` if there isn't a valid one. */
export async function decodeSnippet(hash: string): Promise<Snippet | null> {
  if (!hash.startsWith(PREFIX)) return null;

  try {
    const compressed = fromBase64Url(hash.slice(PREFIX.length));
    const json = await transform(compressed, new DecompressionStream("deflate-raw"));
    const snippet: unknown = JSON.parse(new TextDecoder().decode(json));

    if (
      typeof snippet !== "object" ||
      snippet === null ||
      typeof (snippet as Snippet).source !== "string"
    ) {
      return null;
    }

    const { source, solution, debug } = snippet as Snippet;
    return {
      source,
      solution: typeof solution === "string" ? solution : undefined,
      debug: typeof debug === "boolean" ? debug : undefined,
    };
  } catch {
    return null;
  }
}
