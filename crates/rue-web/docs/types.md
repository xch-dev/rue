# Types

Rue is a statically typed language, and types are used both to prevent mistakes at compile time and to influence behavior at runtime.

There are a few built in named types:

- `Nil` only represents the explicit value `nil`.
- `Int` for integer values.
- `Bool` for the boolean values `true` and `false`.
- `Bytes` for strings or byte values of any length.
- `Bytes32` for 32 byte values, commonly used for sha256 hashes.
- `PublicKey` for 48 byte values (BLS12-381 public keys, also known as G1 elements).
- `Any` is the most generic type and can be used to represent any value.

For defining more complex types, see [Structs](structs) and [Enums](enums).

## Pair Type

A pair type represents two values, the first and rest.

For example:

```rue
(A, B)
```

## Optional Type

An optional type represents a value which may or may not be `nil`.

For example, this means either a pair of `Int`s or `nil`:

```rue
(Int, Int)?
```

You can differentiate between these cases with a [type guard](#type-guard):

```rue
let value: (Int, Int)? = (42, 34);

// This checks at runtime whether the value is `nil` or not.
// At compile time, it can assume the type is not `nil` going forward.
assert value is (Int, Int);

// Since we know the type is `(Int, Int)` now, we can use it as such.
let sum = value.first + value.rest;
```

## List Type

A list type can be represented like so:

```rue
Condition[]
```

You can create multi-dimensional lists:

```rue
Bool[][]
```

## Function Type

Every function signature can be represented as a type.

For example this function:

```rue
fun add(a: Int, b: Int) -> Int {
    a + b
}
```

Has this type and can be assigned like so:

```rue
let fn: fun(Int, Int) -> Int = add;
```

You can even pass a function to another this way:

```rue
fun add_with(adder: fun(Int, Int) -> Int, a: Int, b: Int) -> Int {
    adder(a, b)
}

add_with(add, 10, 20) // 30
```

## Type Aliases

You can give a reusable name to complex or unclear types:

```rue
type Person = (Bytes, Int);
```

## Type Cast

The `as` keyword allows you to cast a value from one type to another at compile time, and does not affect runtime code directly.

For example, to cast an integer to bytes:

```rue
42 as Bytes
```

However, note that certain casts are not possible.

For example, this cast is not valid because `Int` values are not guaranteed to be 32 bytes in length:

```rue
42 as Bytes32
```

You can always use a cast to widen a type to something less specific, and in some cases this is done implicitly. But if you want to hard cast a type to something that is incompatible, you will have to go through `Any` first:

```rue
42 as Any as Bytes32
```

Please note that this can put your code into an invalid state in certain cases, which can crash the program or result in unexpected behavior.

## Type Guard

A type guard allows you to perform type checking on certain values at runtime, whilst in some cases having the compiler infer the type based on that.

The requirements for a type guard to have an effect beyond runtime type checking are:

- The expression you're checking the type of is a symbol (not a field or other computed value).
- The return value eis used directly as a condition for a branch.

Here is an example:

```rue
fun tree_hash(value: Any) -> Bytes32 {
    // Here we check if the value is a pair at runtime.
    if value is (Any, Any) {
        // Since this branch will only run if that's the case, the type is narrowed.
        sha256(2 as Bytes + tree_hash(value.first) + tree_hash(value.rest))
    } else {
        // Whereas here, the type is known to be `Bytes` instead of `(Any, Any)`.
        sha256(1 as Bytes + value)
    }
}
```

This also works for assertions and `if` statements.

It's important to note that type checking is limited to simple types for now. For example, this is not possible:

```rue
(1, (2, 3)) is (Int, (Int, Int))
```

The following type guards are implemented:

- `Any` => `(Any, Any)` or `Bytes` to check if a value is an atom or not.
- `Bytes` => `Bytes32` or `Bytes` to check if the atom's length is 32.
- `Bytes` => `PublicKey` or `Bytes` to check the atom's length is 48.
- `T[]` => `(T, T[])` or `Nil` to check if a list is empty or not.

List guarding can be used like so:

```rue
fun sum(...nums: Int[]) -> Int {
    if nums is (Int, Int[]) {
        nums.first + sum(...nums.rest)
    } else {
        0
    }
}
```
