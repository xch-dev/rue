# Rue Playground

The playground at [play.rue-lang.com](https://play.rue-lang.com) compiles, checks, formats and runs Rue programs in the browser, using the compiler built to WASM (`../wasm`).

- The editor is CodeMirror 6, with a Rue mode (`src/rue-language.ts`) and the compiler's diagnostics and quick fixes.
- The output shows the compiled CLVM, its tree hash, size and serialization, the result of running it with a solution, the tests, and the problems.
- Snippets are shared in the URL hash (`#code=...`, deflated JSON), so nothing is stored on a server.

## Development

Build the WASM package first, then start the dev server:

```bash
cd ../wasm && pnpm install && pnpm build
cd ../playground && pnpm install && pnpm dev
```

`pnpm build` type-checks and builds the site into `dist`, and `pnpm lint` runs ESLint.
