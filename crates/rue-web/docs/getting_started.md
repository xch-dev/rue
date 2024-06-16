# Getting Started

## Example

This is the classic hello world example in Rue:

```rue
fun main() -> Bytes {
    "Hello, world!"
}
```

Give it a try, then read on to learn more in the following sections.

## Atomic Values

There are different ways of representing atomic values (bytes). Each of which has their own type, to prevent mistakes at compile time and help the compiler determine the intent of your code.

### Strings

A string has the type `Bytes`. String characters are encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8).

For example:

```rue
"I'm a string"
```

You can also use single quotes:

```rue
'Hello'
```

There is currently no way to escape characters like you may be used to in other languages.

### Numbers

All numbers have the type `Int`, since there are no decimal numbers in CLVM. It's worth noting that the number `0` is the same as `nil`, or in other words no bytes are used to encode it as an atom.

Here is an example number:

```rue
42_000
```

Note that you can optionally include `_` as a visual separator for digits.

### Booleans

There are two boolean values, which each have the type `Bool`. The values `true` and `false` are encoded as `1` and `0` (nil), respectively.

### Nil

Finally, `nil` is yet another way of representing a nil atom, except more explicitly since it's the only valid value of its type `Nil`.

## Functions

Functions let you group your code into reusable chunks. There are a few things to be aware of with functions in Rue:

- They can be passed around as values (see [higher-order functions](https://en.wikipedia.org/wiki/Higher-order_function)).
- They are pure, so their return value is deterministic.
- They have a bit of overhead at runtime.

Here is an example function:

```rue
fun square(num: Int) -> Int {
    num * num
}
```

Functions must return a value, since everything simplifies down to an expression. As you can see here, the last expression in a block is implicitly the return value unless specified otherwise. This helps to keep code concise.

It's also possible to call functions from within other functions, or even within themselves recursively:

```rue
fun factorial(num: Int) -> Int {
    if num > 1 {
        num * factorial(num - 1)
    } else {
        1
    }
}
```

Finally, every program must contain a `main` function (with any number of arguments), which serves as the entrypoint that gets compiled to CLVM.

## Variables

Since Rue is a [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) language, there is no concept of mutable variables. However, you can still define immutable `let` bindings.

Here is an example:

```rue
let x = 5;
let y = 10;

x + y
```

This allows you to give a name to values and reuse them in multiple places.

You can also explicitly specify the type of the binding if desired:

```rue
let num: Int = 42;
```

## Pairs

The simplest data type other than atoms is the pair. Pairs are comprised of two values, the `first` and `rest`.

For example:

```rue
let pair = ('a', 'b');
```

This defines a pair with the first value being `'a'` and the rest `'b'`.

You can access these properties with the `.` operator:

```rue
let first = pair.first; // 'a'
let rest = pair.rest; // 'b'
```

## Lists

A list is a chain of pairs, ending in `nil`. It's represented by the type `T[]`, where `T` is the type of each list item.

This is how you construct a list:

```rue
let nums = [1, 2, 3];
```

You can access individual items of the list by index:

```rue
let second = nums[1];
```

It's important to note a few things about list indices:

- They are indexed by `0`, like most modern languages.
- If the index is out of bounds, it will crash the program at runtime.
- It's not currently possible to use a computed value to index a list in this way.

## Structs

A `struct` is a way to store a group of fields with different types together in a single reusable type. They are otherwise represented the same way as lists (chain of pairs, ending in `nil`).

Here is an example of a struct:

```rue
struct Person {
    name: Bytes,
    age: Int,
}
```

You can construct a struct with this syntax:

```rue
let person = Person {
    name: 'Bob',
    age: 24,
};
```

And you can access fields of the struct like you would expect:

```rue
let name = person.name;
```

However, like with everything else in Rue, structs are immutable. You cannot change a field without reconstructing a new struct value.

## Enums

With `enums`, you can store various different structs together in the same type. Each enum variant has a discriminator value (an `Int`) which can differentiate between the variants at runtime.

This is an example of an enum:

```rue
enum Outcome {
    Success = 0 {
        value: Int,
    },
    Error = 1 {
        message: Bytes,
    },
}
```

And you can construct the variants using the `::` separator:

```rue
let outcome = Outcome::Success {
    value: 42,
};
```

Because the type of the `Outcome::Success` constructor is assumed to be that of the `Success` variant automatically, you can access its field as if it were a normal struct:

```rue
let value = outcome.value;
```

But if you were to widen the type to `Outcome`, you would need to check the variant at runtime to get access to the fields:

```rue
let outcome: Outcome = Outcome::Success {
    value: 42,
};

let value = if outcome is Outcome::Success {
    outcome.value
} else {
    0
};
```
