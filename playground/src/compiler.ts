// Wraps `rue-wasm`, copying its results into plain objects so the WASM memory is freed right away

import * as wasm from "rue-wasm";

export interface TextEdit {
  from: number;
  to: number;
  insert: string;
}

export interface Suggestion {
  message: string;
  edits: TextEdit[];
  machineApplicable: boolean;
}

export interface Diagnostic {
  severity: "error" | "warning";
  message: string;
  lint?: string;
  from: number;
  to: number;
  line: number;
  column: number;
  notes: string[];
  suggestions: Suggestion[];
}

export interface Compilation {
  /** The source that was compiled. */
  source: string;
  debug: boolean;
  program?: string;
  hex?: string;
  treeHash?: string;
  size?: number;
  diagnostics: Diagnostic[];
  /** Set if the compiler itself failed, rather than reporting a diagnostic. */
  internalError?: string;
}

export interface Print {
  location: string;
  value: string;
}

export interface Execution {
  output?: string;
  error?: string;
  runtimeCost?: number;
  byteCost: number;
  prints: Print[];
}

export interface TestResult {
  name: string;
  error?: string;
  prints: Print[];
}

export interface Example {
  name: string;
  group: string;
  description: string;
  content: string;
  solution: string;
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function copyPrints(prints: wasm.Print[]): Print[] {
  return prints.map((print) => {
    const copy = { location: print.location, value: print.value };
    print.free();
    return copy;
  });
}

export function compile(source: string, debug: boolean): Compilation {
  let compilation: wasm.Compilation;

  try {
    compilation = wasm.compile(source, debug);
  } catch (error) {
    return { source, debug, diagnostics: [], internalError: errorMessage(error) };
  }

  const result: Compilation = {
    source,
    debug,
    program: compilation.program,
    hex: compilation.hex,
    treeHash: compilation.treeHash,
    size: compilation.size,
    diagnostics: compilation.diagnostics.map((diagnostic) => {
      const copy: Diagnostic = {
        severity: diagnostic.severity === "error" ? "error" : "warning",
        message: diagnostic.message,
        lint: diagnostic.lint,
        from: diagnostic.from,
        to: diagnostic.to,
        line: diagnostic.line,
        column: diagnostic.column,
        notes: diagnostic.notes,
        suggestions: diagnostic.suggestions.map((suggestion) => {
          const copy: Suggestion = {
            message: suggestion.message,
            machineApplicable: suggestion.machineApplicable,
            edits: suggestion.edits.map((edit) => {
              const copy = { from: edit.from, to: edit.to, insert: edit.insert };
              edit.free();
              return copy;
            }),
          };
          suggestion.free();
          return copy;
        }),
      };
      diagnostic.free();
      return copy;
    }),
  };

  compilation.free();
  return result;
}

export function run(hex: string, solution: string): Execution {
  try {
    const execution = wasm.run(hex, solution);
    const result: Execution = {
      output: execution.output,
      error: execution.error,
      runtimeCost: execution.runtimeCost,
      byteCost: execution.byteCost,
      prints: copyPrints(execution.prints),
    };
    execution.free();
    return result;
  } catch (error) {
    return { error: errorMessage(error), byteCost: 0, prints: [] };
  }
}

export function runTests(source: string): TestResult[] | string {
  try {
    return wasm.runTests(source).map((test) => {
      const result = {
        name: test.name,
        error: test.error,
        prints: copyPrints(test.prints),
      };
      test.free();
      return result;
    });
  } catch (error) {
    return errorMessage(error);
  }
}

/** Formats the source, or returns why it couldn't. */
export function format(source: string): { source: string } | { error: string } {
  try {
    return { source: wasm.format(source) };
  } catch (error) {
    return { error: errorMessage(error) };
  }
}

export const version = wasm.version();

export const examples: Example[] = wasm.getExamples().map((example) => {
  const copy = {
    name: example.name,
    group: example.group,
    description: example.description,
    content: example.content,
    solution: example.solution,
  };
  example.free();
  return copy;
});
