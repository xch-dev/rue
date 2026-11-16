import { useCallback, useState } from "react";

/** Reads a JSON value from local storage, which may be unavailable (such as in private windows). */
export function readStored<T>(key: string, fallback: T): T {
  try {
    const value = localStorage.getItem(key);
    return value === null ? fallback : (JSON.parse(value) as T);
  } catch {
    return fallback;
  }
}

function writeStored(key: string, value: unknown) {
  try {
    localStorage.setItem(key, JSON.stringify(value));
  } catch {
    // The value still works for this visit
  }
}

/** State that's kept in local storage between visits, starting from `fallback` if there's none. */
export function useStored<T>(key: string, fallback: T | (() => T)) {
  const [value, setValue] = useState(() =>
    readStored(key, typeof fallback === "function" ? (fallback as () => T)() : fallback),
  );

  const set = useCallback(
    (next: T) => {
      setValue(next);
      writeStored(key, next);
    },
    [key],
  );

  return [value, set] as const;
}
