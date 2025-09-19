import ReactAce from "react-ace";

import "ace-builds/src-noconflict/ext-language_tools";
import "ace-builds/src-noconflict/mode-rust";
import "ace-builds/src-noconflict/theme-monokai";
import { useEffect, useState } from "react";
import { compile, type Compilation } from "rue-wasm";
import { useLocalStorage } from "usehooks-ts";

const examples = [
  {
    name: "Hello World",
    code: `fn main() -> Bytes {
  "Hello, world!"
}`,
  },
  {
    name: "Factorial",
    code: `fn main(num: Int) -> Int {
    factorial(num)
}

fn factorial(num: Int) -> Int {
    if num > 1 {
        num * factorial(num - 1)
    } else {
        1
    }
}`,
  },
  {
    name: "Fizz Buzz",
    code: `fn main(num: Int) -> List<Bytes> {
    fizz_buzz(1, num)
}

fn fizz_buzz(num: Int, limit: Int) -> List<Bytes> {
    let rest = if num == limit {
        nil
    } else {
        fizz_buzz(num + 1, limit)
    };
    (fizz_buzz_value(num), rest)
}

fn fizz_buzz_value(num: Int) -> Bytes {
    if num % 3 == 0 && num % 5 == 0 {
        "FizzBuzz"
    } else if num % 3 == 0 {
        "Fizz"
    } else if num % 5 == 0 {
        "Buzz"
    } else {
        integer_to_string(num)
    }
}

fn integer_to_string(num: Int) -> Bytes {
    if num < 0 {
        "-" + integer_to_string(-num)
    } else if num < 10 {
        single_digit_to_string(num)
    } else {
        integer_to_string(num / 10) + single_digit_to_string(num % 10)
    }
}

fn single_digit_to_string(digit: Int) -> Bytes {
    ("0" as Int + digit) as Bytes
}
`,
  },
  {
    name: "Royalty Split Puzzle",
    code: `// This puzzle has not been audited or tested, and is for example purposes only.

struct Payout {
    puzzle_hash: Bytes32,
    share: Int,
}

fn main(payouts: List<Payout>, my_amount: Int, total_shares: Int) -> List<Condition> {
    [
        CreateCoinAnnouncement { message: nil },
        AssertMyAmount { amount: my_amount },
        ...calculate_amount_and_split(payouts, my_amount, total_shares, 0, my_amount),
    ]
}

fn calculate_share(total_amount: Int, share: Int, total_shares: Int) -> Int {
    total_amount * share / total_shares
}

fn get_amount(payout: Payout, total_amount: Int, total_shares: Int) -> Int {
    calculate_share(total_amount, payout.share, total_shares)
}

fn calculate_amount_and_split(
    payouts: List<Payout>,
    total_amount: Int,
    total_shares: Int,
    shares_sum: Int,
    remaining_amount: Int,
) -> List<Condition> {
    if payouts is nil {
        if total_shares == shares_sum {
            return nil;
        }

        raise "Share sum doesn't match total";
    }

    split_amount_and_create_coins(
        payouts,
        calculate_share(total_amount, payouts.first.share, total_shares),
        total_amount,
        total_shares,
        shares_sum,
        remaining_amount,
    )
}

fn split_amount_and_create_coins(
    payouts: (Payout, List<Payout>),
    this_amount: Int,
    total_amount: Int,
    total_shares: Int,
    shares_sum: Int,
    remaining_amount: Int,
) -> List<Condition> {
    let create_coin = CreateCoin {
        puzzle_hash: payouts.first.puzzle_hash,
        amount: if !(payouts.rest is nil) { this_amount } else { remaining_amount },
        memos: Memos { value: [payouts.first.puzzle_hash] },
    };

    [
        create_coin,
        ...calculate_amount_and_split(
            payouts.rest,
            total_amount,
            total_shares,
            shares_sum + payouts.first.share,
            remaining_amount - this_amount,
        ),
    ]
}`,
  },
];

export default function App() {
  const [compilation, setCompilation] = useState<Compilation | null>(null);
  const [source, setSource] = useLocalStorage("source", "");
  const [selectedExample, setSelectedExample] = useState("");

  const onLoad = () => {};
  const onChange = (source: string) => {
    setSource(source);
    setSelectedExample(""); // Reset selected example when code is modified

    try {
      const compiled = compile(source);

      setCompilation(compiled);
    } catch (error: unknown) {
      console.error(error);
      setCompilation(null);
    }
  };

  useEffect(() => {
    onChange(source);
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <div className="h-screen overflow-hidden bg-gray-900 text-white flex flex-col">
      <header className="bg-gray-800 border-b border-gray-700 px-4 py-3 shadow-lg flex-none">
        <div className="max-w-7xl mx-auto">
          <h1 className="text-2xl font-bold text-gray-100">Rue Playground</h1>
        </div>
      </header>

      <main className="flex-1 p-4 max-w-7xl mx-auto w-full overflow-hidden flex flex-col justify-center">
        <div className="flex justify-end mb-4">
          <select
            className="bg-gray-800 text-gray-300 border border-gray-700 rounded-md px-3 py-1.5 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            value={selectedExample}
            onChange={(e) => {
              const selected = e.target.value;
              setSelectedExample(selected);
              if (selected) {
                const example = examples.find((ex) => ex.name === selected);
                if (example) {
                  setSource(example.code);
                  try {
                    const compiled = compile(example.code);
                    setCompilation(compiled);
                  } catch (error: unknown) {
                    console.error(error);
                    setCompilation(null);
                  }
                }
              }
            }}
          >
            <option value="">Select an example...</option>
            {examples.map((example) => (
              <option key={example.name} value={example.name}>
                {example.name}
              </option>
            ))}
          </select>
        </div>
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
