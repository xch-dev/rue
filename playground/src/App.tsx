import ReactAce from "react-ace";

import "ace-builds/src-noconflict/ext-language_tools";
import "ace-builds/src-noconflict/mode-rust";
import "ace-builds/src-noconflict/theme-monokai";
import { useState } from "react";
import { compile, type Compilation } from "rue-wasm";

function App() {
  const [compilation, setCompilation] = useState<Compilation | null>(null);
  const [source, setSource] = useState("");

  const onLoad = () => {};
  const onChange = (source: string) => {
    setSource(source);

    try {
      const compiled = compile(source);

      setCompilation(compiled);
    } catch (error: unknown) {
      console.error(error);
      setCompilation(null);
    }
  };

  return (
    <div className="h-screen overflow-hidden bg-gray-900 text-white flex flex-col">
      <header className="bg-gray-800 border-b border-gray-700 px-4 py-3 shadow-lg flex-none">
        <div className="max-w-7xl mx-auto">
          <h1 className="text-2xl font-bold text-gray-100">Rue Playground</h1>
        </div>
      </header>

      <main className="flex-1 p-4 max-w-7xl mx-auto w-full overflow-hidden flex flex-col justify-center">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 h-full lg:max-h-[600px] grid-rows-[1fr_1fr] lg:grid-rows-1">
          <div className="rounded-md overflow-hidden border border-gray-700 shadow-xl bg-gray-800 h-full">
            <ReactAce
              placeholder="Enter your code here"
              mode="rust"
              theme="monokai"
              name="rue-editor"
              onLoad={onLoad}
              onChange={onChange}
              fontSize={16}
              lineHeight={19}
              width="100%"
              height="100%"
              showPrintMargin={false}
              showGutter={true}
              highlightActiveLine={true}
              value={source}
              enableMobileMenu={true}
              tabSize={2}
            />
          </div>

          <div className="rounded-md border border-gray-700 shadow-xl bg-gray-800 overflow-auto h-full">
            <div className="p-4">
              <div className="mb-1">
                <h2 className="text-sm font-semibold text-gray-300">Output</h2>
              </div>
              <pre className="text-sm font-mono text-gray-300 whitespace-pre-wrap mb-4">
                {compilation?.program ?? "None"}
              </pre>

              <div className="mb-1">
                <h2 className="text-sm font-semibold text-gray-300">
                  Diagnostics
                </h2>
              </div>
              <pre className="text-sm font-mono text-gray-300 whitespace-pre-wrap">
                {compilation?.diagnostics?.length
                  ? compilation.diagnostics.map((diagnostic, index) => (
                      <div key={index} className="mb-2">
                        {diagnostic}
                      </div>
                    ))
                  : "No diagnostics to display"}
              </pre>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}

export default App;
