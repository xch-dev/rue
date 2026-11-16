import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  completionKeymap,
} from "@codemirror/autocomplete";
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
} from "@codemirror/commands";
import {
  bracketMatching,
  foldGutter,
  indentOnInput,
  indentUnit,
  syntaxHighlighting,
} from "@codemirror/language";
import {
  lintGutter,
  lintKeymap,
  setDiagnostics,
  type Diagnostic as EditorDiagnostic,
} from "@codemirror/lint";
import { highlightSelectionMatches, searchKeymap } from "@codemirror/search";
import { EditorState } from "@codemirror/state";
import {
  crosshairCursor,
  drawSelection,
  dropCursor,
  EditorView,
  highlightActiveLine,
  highlightActiveLineGutter,
  highlightSpecialChars,
  keymap,
  lineNumbers,
  rectangularSelection,
} from "@codemirror/view";
import { useEffect, useRef } from "react";
import type { Diagnostic } from "./compiler";
import { rue, rueHighlightStyle } from "./rue-language";

const theme = EditorView.theme({
  "&": {
    height: "100%",
    fontSize: "14px",
    backgroundColor: "var(--editor-bg)",
    color: "var(--text)",
  },
  ".cm-scroller": {
    fontFamily: "var(--font-mono)",
    lineHeight: "1.6",
  },
  ".cm-content": { caretColor: "var(--accent)" },
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: "var(--accent)" },
  "&.cm-focused": { outline: "none" },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, ::selection":
    { backgroundColor: "var(--selection)" },
  ".cm-gutters": {
    backgroundColor: "var(--editor-bg)",
    color: "var(--text-faint)",
    border: "none",
  },
  ".cm-activeLine": { backgroundColor: "var(--active-line)" },
  ".cm-activeLineGutter": {
    backgroundColor: "var(--active-line)",
    color: "var(--text-muted)",
  },
  ".cm-matchingBracket": {
    backgroundColor: "var(--selection)",
    outline: "none",
  },
  ".cm-tooltip": {
    backgroundColor: "var(--surface-raised)",
    color: "var(--text)",
    border: "1px solid var(--border)",
    borderRadius: "6px",
    overflow: "hidden",
  },
  ".cm-diagnostic": { fontFamily: "var(--font-sans)", whiteSpace: "pre-wrap" },
  ".cm-diagnosticSource": { color: "var(--text-muted)", opacity: "1" },
  ".cm-diagnosticAction": {
    backgroundColor: "var(--accent)",
    color: "var(--accent-contrast)",
    borderRadius: "4px",
  },
  ".cm-panels": {
    backgroundColor: "var(--surface-raised)",
    color: "var(--text)",
  },
});

/** Converts the compiler's diagnostics, with a quick fix for each suggestion. */
function toEditorDiagnostics(
  diagnostics: Diagnostic[],
  length: number,
): EditorDiagnostic[] {
  return diagnostics.map((diagnostic) => {
    const from = Math.min(diagnostic.from, length);
    const to = Math.min(Math.max(diagnostic.to, from), length);

    return {
      from,
      to,
      severity: diagnostic.severity,
      source: diagnostic.lint,
      message: [diagnostic.message, ...diagnostic.notes.map((n) => `note: ${n}`)].join(
        "\n",
      ),
      actions: diagnostic.suggestions.map((suggestion) => ({
        name: suggestion.message,
        apply: (view) => view.dispatch({ changes: suggestion.edits }),
      })),
    };
  });
}

interface EditorProps {
  value: string;
  onChange: (value: string) => void;
  /** Diagnostics, along with the source they were reported for. */
  diagnostics: { source: string; diagnostics: Diagnostic[] } | null;
  /** Called for Ctrl/Cmd+Enter, which would otherwise insert a blank line. */
  onRun: () => void;
  onView?: (view: EditorView | null) => void;
}

export function Editor({ value, onChange, diagnostics, onRun, onView }: EditorProps) {
  const parent = useRef<HTMLDivElement>(null);
  const view = useRef<EditorView | null>(null);
  const onChangeRef = useRef(onChange);
  onChangeRef.current = onChange;
  const onRunRef = useRef(onRun);
  onRunRef.current = onRun;

  useEffect(() => {
    const editor = new EditorView({
      parent: parent.current!,
      state: EditorState.create({
        doc: value,
        extensions: [
          lineNumbers(),
          highlightActiveLineGutter(),
          highlightSpecialChars(),
          history(),
          foldGutter(),
          drawSelection(),
          dropCursor(),
          EditorState.allowMultipleSelections.of(true),
          indentOnInput(),
          indentUnit.of("    "),
          bracketMatching(),
          closeBrackets(),
          autocompletion(),
          rectangularSelection(),
          crosshairCursor(),
          highlightActiveLine(),
          highlightSelectionMatches(),
          lintGutter(),
          keymap.of([
            {
              key: "Mod-Enter",
              run: () => {
                onRunRef.current();
                return true;
              },
            },
            ...closeBracketsKeymap,
            ...defaultKeymap,
            ...searchKeymap,
            ...historyKeymap,
            ...completionKeymap,
            ...lintKeymap,
            indentWithTab,
          ]),
          rue,
          syntaxHighlighting(rueHighlightStyle),
          theme,
          EditorView.contentAttributes.of({ "aria-label": "Rue source" }),
          EditorView.updateListener.of((update) => {
            if (update.docChanged) {
              onChangeRef.current(update.state.doc.toString());
            }
          }),
        ],
      }),
    });

    view.current = editor;
    onView?.(editor);

    return () => {
      editor.destroy();
      view.current = null;
      onView?.(null);
    };
    // The editor is only created once, and later values are synced below
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  // Replace the document when the value changes from outside, such as loading an example
  useEffect(() => {
    const editor = view.current;
    if (!editor) return;

    const current = editor.state.doc.toString();
    if (current !== value) {
      editor.dispatch({
        changes: { from: 0, to: current.length, insert: value },
      });
    }
  }, [value]);

  useEffect(() => {
    const editor = view.current;
    if (!editor) return;

    // Diagnostics for an older version of the source would be in the wrong place, and a newer
    // compilation will replace them shortly
    if (diagnostics && diagnostics.source !== editor.state.doc.toString()) {
      return;
    }

    editor.dispatch(
      setDiagnostics(
        editor.state,
        toEditorDiagnostics(
          diagnostics?.diagnostics ?? [],
          editor.state.doc.length,
        ),
      ),
    );
  }, [diagnostics]);

  return <div ref={parent} className="h-full min-h-0 overflow-hidden" />;
}
