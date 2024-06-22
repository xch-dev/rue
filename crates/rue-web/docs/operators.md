# Operators

There a number of unary and binary operators in Rue.

## Arithmetic

The `+` operator adds two integers together:

```rue
3 + 2 // 5
```

The `-` operator subtracts the second integer from the first:

```rue
10 - 4 // 6
```

The `*` operator multiplies two integers together:

```rue
6 * 8 // 48
```

The `/` operator divides the left hand side (numerator) by the right hand side (denominator):

```rue
100 / 2 // 50
```

Finally, the `%` operator performs the CLVM `divmod` operation and returns the remainder:

```rue
5 % 2 // 1
```

## Boolean

The `!` operator returns the opposite of a boolean value (or `false` if it's a non-`false` value):

```rue
!true // false
```

## Comparison

### Integer Comparison

These operators can only be used with `Int` values, so if you want to compare other atoms as integers, you will need to cast them first.

The `<` operator checks if the left hand side is less than the right hand side:

```rue
10 < 20 // true
```

The `>` operator checks if the left hand side is greater than the right hand side:

```rue
34 > 42 // false
```

The `<=` operator checks if the left hand side is less than or equal to the right hand side:

```rue
9 <= 9 // true
```

The `>=` operator checks if the left hand side is greater than or equal to the right hand side:

```rue
22 >= 23 // false
```

### Equality Comparison

These comparisons can only be performed on atom values (`Int`, `Bytes`, etc). However, values which are not atoms, such as lists, cannot be compared directly (you will have to write a custom function to do so).

The `==` operator checks if two values are equal:

```rue
5 == 5 // true
```

And on the contrary, `!=` checks if they are not equal:

```rue
5 != 5 // false
```
