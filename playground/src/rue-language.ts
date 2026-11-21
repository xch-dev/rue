import {
  HighlightStyle,
  StreamLanguage,
  type StringStream,
} from "@codemirror/language";
import { tags } from "@lezer/highlight";

// The keywords the lexer recognizes (`crates/rue-lexer/src/lexer.rs`)
const keywords = new Set([
  "import",
  "export",
  "extern",
  "from",
  "inline",
  "test",
  "mod",
  "fn",
  "const",
  "type",
  "struct",
  "enum",
  "let",
  "if",
  "else",
  "for",
  "match",
  "return",
  "assert",
  "raise",
  "debug",
  "is",
  "as",
  "super",
  "project",
]);

const atoms = new Set(["nil", "true", "false"]);

interface State {
  // How many block comments deep the tokenizer is, since they nest
  commentDepth: number;
  // The quote that ends the string the tokenizer is in, if any
  stringQuote: string | null;
}

function blockComment(stream: StringStream, state: State): string {
  while (!stream.eol()) {
    if (stream.match("/*")) {
      state.commentDepth++;
    } else if (stream.match("*/")) {
      state.commentDepth--;
      if (state.commentDepth === 0) break;
    } else {
      stream.next();
    }
  }
  return "comment";
}

// Strings end at the next quote of the kind they started with that isn't escaped by a backslash
function string(stream: StringStream, state: State): string {
  while (!stream.eol()) {
    const char = stream.next();
    if (char === "\\") {
      stream.next();
    } else if (char === state.stringQuote) {
      state.stringQuote = null;
      break;
    }
  }
  return "string";
}

export const rue = StreamLanguage.define<State>({
  name: "rue",
  startState: () => ({ commentDepth: 0, stringQuote: null }),
  token(stream, state) {
    if (state.commentDepth > 0) return blockComment(stream, state);
    if (state.stringQuote) return string(stream, state);
    if (stream.eatSpace()) return null;

    if (stream.match("//")) {
      stream.skipToEnd();
      return "comment";
    }
    if (stream.match("/*")) {
      state.commentDepth = 1;
      return blockComment(stream, state);
    }
    const quote = stream.eat(/^["']/);
    if (quote) {
      state.stringQuote = quote;
      return string(stream, state);
    }
    if (stream.match(/^#!?\[/)) {
      // Attributes, such as `#[warn(unused)]`, up to the closing bracket on this line
      let depth = 1;
      while (!stream.eol() && depth > 0) {
        const char = stream.next();
        if (char === "[") depth++;
        else if (char === "]") depth--;
      }
      return "meta";
    }
    if (
      stream.match(/^0x[0-9a-fA-F_]*/) ||
      stream.match(/^0b[01_]*/) ||
      stream.match(/^0o[0-7_]*/) ||
      stream.match(/^[0-9][0-9_]*/)
    ) {
      return "number";
    }

    const ident = stream.match(/^[a-zA-Z_][a-zA-Z0-9_]*/) as
      | RegExpMatchArray
      | false;
    if (ident) {
      const word = ident[0];
      if (keywords.has(word)) return "keyword";
      if (atoms.has(word)) return "atom";
      if (/^[A-Z]/.test(word)) {
        return /^[A-Z][A-Z0-9_]*$/.test(word) && word.length > 1
          ? "constant"
          : "typeName";
      }
      if (stream.match(/^\s*(::\s*<[^>]*>\s*)?\(/, false)) return "function";
      return "variableName";
    }

    if (stream.match(/^(\.\.\.?|::|->|=>|[-+*/%<>=!&|^~]=?)/)) return "operator";
    stream.next();
    return "punctuation";
  },
  tokenTable: {
    function: tags.function(tags.variableName),
    constant: tags.constant(tags.variableName),
  },
  languageData: {
    commentTokens: { line: "//", block: { open: "/*", close: "*/" } },
    closeBrackets: { brackets: ["(", "[", "{", '"'] },
  },
});

// Colors come from CSS variables, so the same style works in light and dark mode
export const rueHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, color: "var(--syntax-keyword)" },
  { tag: tags.atom, color: "var(--syntax-atom)" },
  { tag: tags.number, color: "var(--syntax-number)" },
  { tag: tags.string, color: "var(--syntax-string)" },
  { tag: tags.comment, color: "var(--syntax-comment)", fontStyle: "italic" },
  { tag: tags.typeName, color: "var(--syntax-type)" },
  { tag: tags.constant(tags.variableName), color: "var(--syntax-atom)" },
  { tag: tags.function(tags.variableName), color: "var(--syntax-function)" },
  { tag: tags.meta, color: "var(--syntax-meta)" },
  { tag: tags.operator, color: "var(--syntax-operator)" },
  { tag: tags.punctuation, color: "var(--syntax-punctuation)" },
]);
