import { EditorSelection } from "@codemirror/state";
import type { EditorView } from "@codemirror/view";
import {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type PointerEvent as ReactPointerEvent,
} from "react";
import {
  compile,
  examples,
  format,
  run,
  runTests,
  version,
  type Compilation,
  type Diagnostic,
  type Example,
  type Suggestion,
} from "./compiler";
import { Editor } from "./Editor";
import {
  BookIcon,
  ChevronIcon,
  FormatIcon,
  GitHubIcon,
  MonitorIcon,
  MoonIcon,
  ShareIcon,
  SunIcon,
} from "./icons";
import { OutputPanel, type OutputTab, type RunResult, type TestsResult } from "./OutputPanel";
import { decodeSnippet, encodeSnippet } from "./share";
import { readStored, useStored } from "./storage";

type Theme = "system" | "light" | "dark";

const COMPILE_DELAY_MS = 150;

const defaultExample = examples[0];

/** The source to start with if there's none saved. */
function defaultSource(): string {
  // The playground used to keep the source under `source`, as JSON. An empty value was saved by
  // visiting it without typing anything.
  return readStored<string | null>("source", null) || defaultExample.content;
}

export default function App() {
  const [source, setSource] = useStored("rue.source", defaultSource);
  const [solution, setSolution] = useStored("rue.solution", defaultExample.solution);
  const [debug, setDebug] = useStored("rue.debug", false);
  const [theme, setTheme] = useStored<Theme>("rue.theme", "system");
  const [split, setSplit] = useStored("rue.split", 0.55);

  const [compilation, setCompilation] = useState<Compilation | null>(null);
  const [runResult, setRunResult] = useState<RunResult | null>(null);
  const [tests, setTests] = useState<TestsResult | null>(null);
  const [tab, setTab] = useState<OutputTab>("program");
  const [mobileView, setMobileView] = useState<"editor" | "output">("editor");
  const [toast, setToast] = useState<string | null>(null);
  const [loadingShared, setLoadingShared] = useState(() =>
    window.location.hash.startsWith("#code="),
  );

  const view = useRef<EditorView | null>(null);
  const main = useRef<HTMLElement>(null);
  const sharedHash = useRef<string | null>(null);

  // A shared snippet in the URL replaces whatever was there before
  useEffect(() => {
    const hash = window.location.hash;
    if (!hash) return;

    void decodeSnippet(hash).then((snippet) => {
      setLoadingShared(false);
      if (!snippet) {
        showToast("That link doesn't contain a valid snippet");
        return;
      }
      sharedHash.current = hash;
      setSource(snippet.source);
      setSolution(snippet.solution ?? "()");
      if (snippet.debug !== undefined) setDebug(snippet.debug);
      setRunResult(null);
      setTests(null);
    });
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  useEffect(() => {
    const root = document.documentElement;
    if (theme === "system") delete root.dataset.theme;
    else root.dataset.theme = theme;
  }, [theme]);

  // Compile shortly after typing stops, and right away when the build mode changes
  const lastDebug = useRef(debug);
  useEffect(() => {
    const immediate = compilation === null || lastDebug.current !== debug;
    lastDebug.current = debug;

    const timeout = setTimeout(
      () => setCompilation(compile(source, debug)),
      immediate ? 0 : COMPILE_DELAY_MS,
    );
    return () => clearTimeout(timeout);
  }, [source, debug]); // eslint-disable-line react-hooks/exhaustive-deps

  const toastTimeout = useRef<ReturnType<typeof setTimeout>>(undefined);
  const showToast = useCallback((message: string) => {
    setToast(message);
    clearTimeout(toastTimeout.current);
    toastTimeout.current = setTimeout(() => setToast(null), 2500);
  }, []);

  const onSourceChange = useCallback(
    (value: string) => {
      setSource(value);

      // Once the shared snippet is edited, the link no longer describes it
      if (sharedHash.current !== null && window.location.hash === sharedHash.current) {
        history.replaceState(null, "", window.location.pathname + window.location.search);
        sharedHash.current = null;
      }
    },
    [setSource],
  );

  // Compiles the current source if the last compilation is out of date, so that running never
  // waits for the debounce
  const currentCompilation = useCallback((): Compilation => {
    if (compilation && compilation.source === source && compilation.debug === debug) {
      return compilation;
    }
    const fresh = compile(source, debug);
    setCompilation(fresh);
    return fresh;
  }, [compilation, source, debug]);

  const onRun = useCallback(() => {
    const current = currentCompilation();
    setTab("run");
    setMobileView("output");
    if (!current.hex) return;
    setRunResult({ hex: current.hex, solution, execution: run(current.hex, solution) });
  }, [currentCompilation, solution]);

  const onRunTests = useCallback(() => {
    setTab("tests");
    setMobileView("output");
    setTests({ source, results: runTests(source) });
  }, [source]);

  const onFormat = useCallback(() => {
    const result = format(source);
    if ("error" in result) {
      showToast(`Can't format: ${result.error}`);
      return;
    }
    if (result.source === source) {
      showToast("Already formatted");
      return;
    }
    // Go through the editor, so that formatting can be undone
    const editor = view.current;
    if (editor) {
      editor.dispatch({
        changes: { from: 0, to: editor.state.doc.length, insert: result.source },
      });
    } else {
      onSourceChange(result.source);
    }
  }, [source, showToast, onSourceChange]);

  const onShare = useCallback(async () => {
    const hash = await encodeSnippet({ source, solution, debug });
    sharedHash.current = hash;
    history.replaceState(null, "", hash);
    try {
      await navigator.clipboard.writeText(window.location.href);
      showToast("Link copied to the clipboard");
    } catch {
      showToast("Copy the link from the address bar");
    }
  }, [source, solution, debug, showToast]);

  const loadExample = useCallback(
    (example: Example) => {
      onSourceChange(example.content);
      setSolution(example.solution);
      setRunResult(null);
      setTests(null);
      setMobileView("editor");
      view.current?.dispatch({ selection: EditorSelection.cursor(0), scrollIntoView: true });
    },
    [onSourceChange, setSolution],
  );

  const onJump = useCallback((diagnostic: Diagnostic) => {
    const editor = view.current;
    if (!editor) return;
    setMobileView("editor");
    const length = editor.state.doc.length;
    editor.dispatch({
      selection: EditorSelection.range(
        Math.min(diagnostic.from, length),
        Math.min(diagnostic.to, length),
      ),
      scrollIntoView: true,
    });
    editor.focus();
  }, []);

  const onFix = useCallback((suggestion: Suggestion) => {
    view.current?.dispatch({ changes: suggestion.edits });
  }, []);

  // Global shortcuts: run with Ctrl/Cmd+Enter, format with Shift+Alt+F. The editor handles its own
  // Ctrl/Cmd+Enter and marks the event as handled.
  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.defaultPrevented) return;
      if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
        event.preventDefault();
        onRun();
      } else if (event.code === "KeyF" && event.shiftKey && event.altKey) {
        event.preventDefault();
        onFormat();
      }
    };
    window.addEventListener("keydown", onKeyDown);
    return () => window.removeEventListener("keydown", onKeyDown);
  }, [onRun, onFormat]);

  const onDividerDown = (event: ReactPointerEvent) => {
    event.preventDefault();
    const bounds = main.current!.getBoundingClientRect();
    const onMove = (move: PointerEvent) => {
      const ratio = (move.clientX - bounds.left) / bounds.width;
      setSplit(Math.min(0.8, Math.max(0.2, ratio)));
    };
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      document.body.style.cursor = "";
    };
    document.body.style.cursor = "col-resize";
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
  };

  const editorDiagnostics = useMemo(
    () =>
      compilation ? { source: compilation.source, diagnostics: compilation.diagnostics } : null,
    [compilation],
  );

  return (
    <div className="flex h-full flex-col">
      <header className="flex flex-none flex-wrap items-center gap-x-3 gap-y-2 border-b border-line bg-surface px-3 py-2 sm:px-4">
        <a href="./" className="mr-1 flex items-center gap-2" aria-label="Rue Playground">
          <Logo />
          <span className="text-[15px] font-semibold tracking-tight">
            Rue <span className="font-normal text-muted">Playground</span>
          </span>
        </a>
        <span
          className="rounded-full border border-line px-2 py-px font-mono text-[11px] text-muted"
          title="Compiler version"
        >
          v{version}
        </span>

        <div className="order-last flex w-full items-center gap-2 sm:order-none sm:ml-4 sm:w-auto">
          <ExamplePicker onPick={loadExample} />
          <ToolbarButton onClick={onFormat} title="Format (Shift+Alt+F)" aria-label="Format">
            <FormatIcon /> <span className="hidden min-[480px]:inline">Format</span>
          </ToolbarButton>
          <ToolbarButton
            onClick={() => void onShare()}
            title="Copy a link to this snippet"
            aria-label="Share"
          >
            <ShareIcon /> <span className="hidden min-[480px]:inline">Share</span>
          </ToolbarButton>
          <label
            className="ml-auto flex cursor-pointer select-none items-center gap-2 text-sm text-muted sm:ml-0"
            title="Debug builds keep `debug` statements and where assertions failed, without optimizing"
          >
            <input
              type="checkbox"
              checked={debug}
              onChange={(e) => setDebug(e.target.checked)}
              className="peer sr-only"
            />
            <span className="relative h-5 w-9 rounded-full bg-line transition-colors peer-checked:bg-accent peer-focus-visible:ring-2 peer-focus-visible:ring-accent after:absolute after:left-0.5 after:top-0.5 after:h-4 after:w-4 after:rounded-full after:bg-surface after:shadow after:transition-transform peer-checked:after:translate-x-4" />
            Debug
          </label>
        </div>

        <div className="ml-auto flex items-center gap-1">
          <ThemeButton theme={theme} onTheme={setTheme} />
          <a
            href="https://github.com/xch-dev/rue"
            target="_blank"
            rel="noreferrer"
            className="rounded-md p-2 text-muted transition-colors hover:bg-raised hover:text-fg"
            aria-label="Rue on GitHub"
            title="Rue on GitHub"
          >
            <GitHubIcon />
          </a>
        </div>
      </header>

      {/* On narrow screens, the editor and output are tabs instead of side by side */}
      <div className="flex flex-none border-b border-line bg-surface md:hidden" role="tablist">
        {(["editor", "output"] as const).map((id) => (
          <button
            key={id}
            role="tab"
            aria-selected={mobileView === id}
            onClick={() => setMobileView(id)}
            className={`flex-1 border-b-2 py-2 text-sm font-medium capitalize ${
              mobileView === id ? "border-accent text-fg" : "border-transparent text-muted"
            }`}
          >
            {id}
          </button>
        ))}
      </div>

      <main
        ref={main}
        className="relative flex min-h-0 flex-1"
        style={{ "--split": `${split * 100}%` } as React.CSSProperties}
      >
        <div
          className={`min-w-0 flex-1 md:flex-none md:basis-[var(--split)] ${
            mobileView === "editor" ? "block" : "hidden md:block"
          }`}
        >
          {loadingShared ? (
            <div className="flex h-full items-center justify-center text-sm text-muted">
              Loading the shared snippet…
            </div>
          ) : (
            <Editor
              value={source}
              onChange={onSourceChange}
              diagnostics={editorDiagnostics}
              onRun={onRun}
              onView={(editor) => (view.current = editor)}
            />
          )}
        </div>

        <div
          role="separator"
          aria-orientation="vertical"
          aria-label="Resize the panes"
          onPointerDown={onDividerDown}
          onDoubleClick={() => setSplit(0.55)}
          className="group relative hidden w-px flex-none cursor-col-resize bg-line md:block"
        >
          <div className="absolute inset-y-0 -left-1.5 -right-1.5 transition-colors group-hover:bg-accent/30" />
        </div>

        <div
          className={`min-w-0 flex-1 ${mobileView === "output" ? "block" : "hidden md:block"}`}
        >
          <OutputPanel
            tab={tab}
            onTab={setTab}
            compilation={compilation}
            solution={solution}
            onSolution={setSolution}
            run={runResult}
            onRun={onRun}
            tests={tests}
            onRunTests={onRunTests}
            onJump={onJump}
            onFix={onFix}
          />
        </div>
      </main>

      {toast && (
        <div
          role="status"
          className="pointer-events-none fixed bottom-4 left-1/2 z-50 -translate-x-1/2 rounded-md bg-fg px-3 py-2 text-sm text-bg shadow-lg"
        >
          {toast}
        </div>
      )}
    </div>
  );
}

function Logo() {
  return (
    <svg viewBox="0 0 32 32" width="26" height="26" aria-hidden="true">
      <rect width="32" height="32" rx="8" fill="var(--accent)" />
      <path
        d="M11 23V9h6a4.5 4.5 0 0 1 0 9h-6m6 0 4.5 5"
        fill="none"
        stroke="var(--accent-contrast)"
        strokeWidth="2.6"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
}

function ToolbarButton({
  children,
  ...props
}: React.ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      {...props}
      className="inline-flex items-center gap-1.5 rounded-md border border-line bg-raised px-2.5 py-1.5 text-sm text-fg transition-colors hover:border-faint"
    >
      {children}
    </button>
  );
}

function ExamplePicker({ onPick }: { onPick: (example: Example) => void }) {
  const [open, setOpen] = useState(false);
  const container = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!open) return;
    const onPointerDown = (event: PointerEvent) => {
      if (!container.current?.contains(event.target as Node)) setOpen(false);
    };
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") setOpen(false);
    };
    window.addEventListener("pointerdown", onPointerDown);
    window.addEventListener("keydown", onKeyDown);
    return () => {
      window.removeEventListener("pointerdown", onPointerDown);
      window.removeEventListener("keydown", onKeyDown);
    };
  }, [open]);

  const groups = useMemo(() => {
    const map = new Map<string, Example[]>();
    for (const example of examples) {
      map.set(example.group, [...(map.get(example.group) ?? []), example]);
    }
    return [...map];
  }, []);

  return (
    <div ref={container} className="relative">
      <ToolbarButton onClick={() => setOpen(!open)} aria-expanded={open} aria-haspopup="menu">
        <BookIcon /> <span>Examples</span> <ChevronIcon />
      </ToolbarButton>
      {open && (
        <div
          role="menu"
          className="absolute left-0 top-full z-40 mt-1 max-h-[70vh] w-72 overflow-auto rounded-lg border border-line bg-raised p-1 shadow-xl"
        >
          {groups.map(([group, items]) => (
            <div key={group} className="py-1">
              <div className="px-2.5 pb-1 pt-1.5 text-[11px] font-semibold uppercase tracking-wide text-faint">
                {group}
              </div>
              {items.map((example) => (
                <button
                  key={example.name}
                  role="menuitem"
                  onClick={() => {
                    setOpen(false);
                    onPick(example);
                  }}
                  className="block w-full rounded-md px-2.5 py-1.5 text-left transition-colors hover:bg-accent-soft"
                >
                  <span className="block text-sm text-fg">{example.name}</span>
                  <span className="block text-xs text-muted">{example.description}</span>
                </button>
              ))}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

const themes: { id: Theme; label: string; icon: () => React.JSX.Element }[] = [
  { id: "system", label: "System theme", icon: MonitorIcon },
  { id: "light", label: "Light theme", icon: SunIcon },
  { id: "dark", label: "Dark theme", icon: MoonIcon },
];

function ThemeButton({ theme, onTheme }: { theme: Theme; onTheme: (theme: Theme) => void }) {
  const index = themes.findIndex((t) => t.id === theme);
  const current = themes[index === -1 ? 0 : index];
  const next = themes[(index + 1) % themes.length];
  const Icon = current.icon;

  return (
    <button
      onClick={() => onTheme(next.id)}
      className="rounded-md p-2 text-muted transition-colors hover:bg-raised hover:text-fg"
      title={`${current.label} (switch to ${next.label.toLowerCase()})`}
      aria-label={`${current.label}. Switch to ${next.label.toLowerCase()}`}
    >
      <Icon />
    </button>
  );
}
