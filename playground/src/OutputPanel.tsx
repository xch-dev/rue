import { useState, type ButtonHTMLAttributes, type ReactNode } from "react";
import type {
  Compilation,
  Diagnostic,
  Execution,
  Print,
  Suggestion,
  TestResult,
} from "./compiler";
import { CheckIcon, CopyIcon, CrossIcon, PlayIcon } from "./icons";

export type OutputTab = "program" | "run" | "tests" | "problems";

/** A run, along with the program it ran, so that it can be marked as outdated. */
export interface RunResult {
  hex: string;
  solution: string;
  execution: Execution;
}

export interface TestsResult {
  source: string;
  results: TestResult[] | string;
}

interface OutputPanelProps {
  tab: OutputTab;
  onTab: (tab: OutputTab) => void;
  compilation: Compilation | null;
  solution: string;
  onSolution: (solution: string) => void;
  run: RunResult | null;
  onRun: () => void;
  tests: TestsResult | null;
  onRunTests: () => void;
  onJump: (diagnostic: Diagnostic) => void;
  onFix: (suggestion: Suggestion) => void;
}

const formatNumber = (value: number) => value.toLocaleString("en-US");

export function OutputPanel(props: OutputPanelProps) {
  const { tab, onTab, compilation } = props;
  const diagnostics = compilation?.diagnostics ?? [];
  const errors = diagnostics.filter((d) => d.severity === "error").length;
  const warnings = diagnostics.length - errors;

  const tabs: { id: OutputTab; label: string; badge?: ReactNode }[] = [
    { id: "program", label: "Program" },
    { id: "run", label: "Run" },
    { id: "tests", label: "Tests" },
    {
      id: "problems",
      label: "Problems",
      badge:
        diagnostics.length > 0 ? (
          <span
            className={`ml-1.5 rounded-full px-1.5 text-[11px] font-semibold tabular-nums ${
              errors > 0 ? "bg-err-soft text-err" : "bg-warn-soft text-warn"
            }`}
          >
            {diagnostics.length}
          </span>
        ) : null,
    },
  ];

  return (
    <section className="flex h-full min-h-0 flex-col bg-surface">
      <div
        role="tablist"
        aria-label="Output"
        className="flex flex-none items-center gap-1 overflow-x-auto border-b border-line px-2"
      >
        {tabs.map(({ id, label, badge }) => (
          <button
            key={id}
            role="tab"
            aria-selected={tab === id}
            onClick={() => onTab(id)}
            className={`-mb-px flex items-center whitespace-nowrap border-b-2 px-3 py-2.5 text-sm font-medium transition-colors ${
              tab === id
                ? "border-accent text-fg"
                : "border-transparent text-muted hover:text-fg"
            }`}
          >
            {label}
            {badge}
          </button>
        ))}
        <div className="ml-auto hidden whitespace-nowrap pr-2 text-xs text-muted sm:block">
          {compilation?.internalError
            ? "Compiler error"
            : errors > 0
              ? `${errors} error${errors === 1 ? "" : "s"}`
              : warnings > 0
                ? `${warnings} warning${warnings === 1 ? "" : "s"}`
                : compilation
                  ? "No problems"
                  : ""}
        </div>
      </div>

      <div className="min-h-0 flex-1 overflow-auto p-4">
        {compilation?.internalError && (
          <ErrorCallout title="The compiler failed">
            <pre className="whitespace-pre-wrap font-mono text-xs">
              {compilation.internalError}
            </pre>
          </ErrorCallout>
        )}
        {tab === "program" && <ProgramTab compilation={compilation} />}
        {tab === "run" && <RunTab {...props} />}
        {tab === "tests" && <TestsTab {...props} />}
        {tab === "problems" && <ProblemsTab {...props} diagnostics={diagnostics} />}
      </div>
    </section>
  );
}

function ProgramTab({ compilation }: { compilation: Compilation | null }) {
  if (!compilation) return null;

  if (!compilation.program) {
    const hasErrors = compilation.diagnostics.some((d) => d.severity === "error");
    return (
      <Empty>
        {hasErrors
          ? "Fix the errors to see the compiled program."
          : "Add a `main` function to compile a program."}
      </Empty>
    );
  }

  return (
    <div className="space-y-4">
      <dl className="grid grid-cols-2 gap-2 sm:grid-cols-[repeat(auto-fit,minmax(9rem,1fr))]">
        <Stat label="Size" value={`${formatNumber(compilation.size ?? 0)} bytes`} />
        <Stat label="Build" value={compilation.debug ? "Debug" : "Release"} />
      </dl>

      <Block label="Tree hash" copy={compilation.treeHash!}>
        <code className="break-all font-mono text-xs">{compilation.treeHash}</code>
      </Block>

      <Block label="CLVM" copy={compilation.program}>
        <pre className="whitespace-pre-wrap break-all font-mono text-xs leading-relaxed">
          {compilation.program}
        </pre>
      </Block>

      <Block label="Serialized (hex)" copy={compilation.hex!}>
        <pre className="max-h-40 overflow-auto whitespace-pre-wrap break-all font-mono text-xs text-muted">
          {compilation.hex}
        </pre>
      </Block>
    </div>
  );
}

function RunTab({ compilation, solution, onSolution, run, onRun }: OutputPanelProps) {
  const canRun = Boolean(compilation?.hex);
  const outdated =
    run !== null && (run.hex !== compilation?.hex || run.solution !== solution);
  const execution = run?.execution;
  const modifier = /Mac|iPhone|iPad/.test(navigator.userAgent) ? "⌘" : "Ctrl";

  return (
    <div className="space-y-4">
      <div>
        <label htmlFor="solution" className="mb-1.5 block text-xs font-medium text-muted">
          Solution
          <span className="font-normal text-faint"> — the arguments to `main`, as CLVM</span>
        </label>
        <textarea
          id="solution"
          value={solution}
          onChange={(e) => onSolution(e.target.value)}
          spellCheck={false}
          rows={3}
          placeholder="()"
          className="w-full resize-y rounded-md border border-line bg-raised px-3 py-2 font-mono text-xs text-fg outline-none placeholder:text-faint focus:border-accent"
        />
        <div className="mt-2 flex items-center gap-3">
          <PrimaryButton onClick={onRun} disabled={!canRun}>
            <PlayIcon /> Run
          </PrimaryButton>
          <span className="text-xs text-faint">
            {canRun ? `${modifier}+Enter` : "There's no program to run yet."}
          </span>
        </div>
      </div>

      {execution && (
        <div className={`space-y-4 ${outdated ? "opacity-60" : ""}`}>
          {outdated && (
            <p className="text-xs text-muted">
              The program or solution changed since this run.
            </p>
          )}
          {execution.error !== undefined ? (
            <ErrorCallout title="Failed">
              <pre className="whitespace-pre-wrap break-all font-mono text-xs">
                {execution.error}
              </pre>
            </ErrorCallout>
          ) : (
            <Block label="Output" copy={execution.output!}>
              <pre className="whitespace-pre-wrap break-all font-mono text-sm">
                {execution.output}
              </pre>
            </Block>
          )}

          {execution.runtimeCost !== undefined && (
            <dl className="grid grid-cols-3 gap-2">
              <Stat label="Runtime cost" value={formatNumber(execution.runtimeCost)} />
              <Stat label="Byte cost" value={formatNumber(execution.byteCost)} />
              <Stat
                label="Total cost"
                value={formatNumber(execution.runtimeCost + execution.byteCost)}
              />
            </dl>
          )}

          <Prints
            prints={execution.prints}
            hint={
              compilation !== null &&
              !compilation.debug &&
              /\bdebug\b/.test(compilation.source)
            }
          />
        </div>
      )}
    </div>
  );
}

function Prints({ prints, hint }: { prints: Print[]; hint: boolean }) {
  if (prints.length === 0) {
    return !hint ? null : (
      <p className="text-xs text-faint">
        `debug` statements only print in debug builds.
      </p>
    );
  }

  return (
    <Block label="Debug output">
      <PrintList prints={prints} />
    </Block>
  );
}

function PrintList({ prints, className = "" }: { prints: Print[]; className?: string }) {
  return (
    <ul className={`space-y-1 font-mono text-xs ${className}`}>
      {prints.map((print, index) => (
        <li key={index} className="flex gap-2">
          <span className="flex-none text-faint">{print.location}</span>
          <span className="break-all">{print.value}</span>
        </li>
      ))}
    </ul>
  );
}

function TestsTab({ compilation, tests, onRunTests }: OutputPanelProps) {
  const outdated = tests !== null && tests.source !== compilation?.source;
  const results = tests?.results;
  const hasErrors = compilation?.diagnostics.some((d) => d.severity === "error");

  return (
    <div className="space-y-4">
      <div className="flex items-center gap-3">
        <PrimaryButton onClick={onRunTests} disabled={!compilation || hasErrors}>
          <PlayIcon /> Run tests
        </PrimaryButton>
        <span className="text-xs text-faint">
          {hasErrors ? "Fix the errors to run the tests." : "Runs each `test fn` in a debug build."}
        </span>
      </div>

      {typeof results === "string" && (
        <ErrorCallout title="The tests couldn't run">
          <pre className="whitespace-pre-wrap font-mono text-xs">{results}</pre>
        </ErrorCallout>
      )}

      {Array.isArray(results) && (
        <div className={outdated ? "opacity-60" : ""}>
          {outdated && (
            <p className="mb-2 text-xs text-muted">The source changed since these ran.</p>
          )}
          {results.length === 0 ? (
            <Empty>There are no tests. Add one with `test fn`.</Empty>
          ) : (
            <>
              <p className="mb-2 text-xs text-muted">
                {results.filter((r) => r.error === undefined).length} of {results.length}{" "}
                passed
              </p>
              <ul className="divide-y divide-line rounded-md border border-line bg-raised">
                {results.map((result) => (
                  <li key={result.name} className="px-3 py-2">
                    <div className="flex items-center gap-2 text-sm">
                      <span className={result.error === undefined ? "text-ok" : "text-err"}>
                        {result.error === undefined ? <CheckIcon /> : <CrossIcon />}
                      </span>
                      <span className="font-mono">{result.name}</span>
                    </div>
                    {result.error !== undefined && (
                      <pre className="mt-1 whitespace-pre-wrap break-all pl-6 font-mono text-xs text-err">
                        {result.error}
                      </pre>
                    )}
                    {result.prints.length > 0 && (
                      <PrintList prints={result.prints} className="mt-1 pl-6 text-muted" />
                    )}
                  </li>
                ))}
              </ul>
            </>
          )}
        </div>
      )}
    </div>
  );
}

function ProblemsTab({
  diagnostics,
  onJump,
  onFix,
}: OutputPanelProps & { diagnostics: Diagnostic[] }) {
  if (diagnostics.length === 0) return <Empty>No problems.</Empty>;

  return (
    <ul className="space-y-2">
      {diagnostics.map((diagnostic, index) => (
        <li
          key={index}
          className="rounded-md border border-line bg-raised text-sm transition-colors hover:border-faint"
        >
          <button
            onClick={() => onJump(diagnostic)}
            className="flex w-full items-start gap-2 px-3 py-2 text-left"
          >
            <span
              className={`mt-0.5 flex-none rounded px-1.5 py-px text-[10px] font-bold uppercase tracking-wide ${
                diagnostic.severity === "error"
                  ? "bg-err-soft text-err"
                  : "bg-warn-soft text-warn"
              }`}
            >
              {diagnostic.severity}
            </span>
            <span className="min-w-0 flex-1">
              <span className="block break-words">{diagnostic.message}</span>
              {diagnostic.notes.map((note, i) => (
                <span key={i} className="block text-xs text-muted">
                  note: {note}
                </span>
              ))}
              <span className="mt-0.5 block font-mono text-[11px] text-faint">
                {diagnostic.line}:{diagnostic.column}
                {diagnostic.lint && ` · ${diagnostic.lint}`}
              </span>
            </span>
          </button>
          {diagnostic.suggestions.length > 0 && (
            <div className="flex flex-wrap gap-1.5 border-t border-line px-3 py-2">
              {diagnostic.suggestions.map((suggestion, i) => (
                <button
                  key={i}
                  onClick={() => onFix(suggestion)}
                  title={
                    suggestion.machineApplicable
                      ? undefined
                      : "This is probably right, but check it first"
                  }
                  className="rounded border border-line px-2 py-0.5 text-xs text-fg transition-colors hover:border-accent hover:text-accent"
                >
                  {suggestion.message}
                  {!suggestion.machineApplicable && <span className="text-faint"> ?</span>}
                </button>
              ))}
            </div>
          )}
        </li>
      ))}
    </ul>
  );
}

function Stat({ label, value }: { label: string; value: string }) {
  return (
    <div className="rounded-md border border-line bg-raised px-3 py-2">
      <dt className="text-[11px] font-medium uppercase tracking-wide text-faint">{label}</dt>
      <dd className="mt-0.5 font-mono text-sm tabular-nums">{value}</dd>
    </div>
  );
}

function Block({
  label,
  copy,
  children,
}: {
  label: string;
  copy?: string;
  children: ReactNode;
}) {
  return (
    <div className="rounded-md border border-line bg-raised">
      <div className="flex items-center justify-between border-b border-line px-3 py-1.5">
        <span className="text-[11px] font-medium uppercase tracking-wide text-faint">
          {label}
        </span>
        {copy !== undefined && <CopyButton text={copy} />}
      </div>
      <div className="px-3 py-2">{children}</div>
    </div>
  );
}

function CopyButton({ text }: { text: string }) {
  const [copied, setCopied] = useState(false);

  return (
    <button
      onClick={() => {
        void navigator.clipboard.writeText(text).then(() => {
          setCopied(true);
          setTimeout(() => setCopied(false), 1500);
        });
      }}
      className="inline-flex items-center gap-1 text-xs text-muted transition-colors hover:text-fg"
      aria-label="Copy"
    >
      {copied ? <CheckIcon /> : <CopyIcon />}
      {copied ? "Copied" : "Copy"}
    </button>
  );
}

function PrimaryButton(props: ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      {...props}
      className="inline-flex items-center gap-1.5 rounded-md bg-accent px-3 py-1.5 text-sm font-medium text-accent-contrast transition-opacity hover:opacity-90 disabled:cursor-not-allowed disabled:opacity-40"
    />
  );
}

function ErrorCallout({ title, children }: { title: string; children: ReactNode }) {
  return (
    <div className="mb-4 rounded-md border border-err/40 bg-err-soft px-3 py-2 text-err">
      <div className="mb-1 text-sm font-semibold">{title}</div>
      {children}
    </div>
  );
}

function Empty({ children }: { children: ReactNode }) {
  return <p className="py-8 text-center text-sm text-muted">{children}</p>;
}
