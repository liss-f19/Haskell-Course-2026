# MathLang Project

MathLang is a small DSL for scalar, vector, and matrix expressions.
This project includes:

- A parser for definitions, lets, arithmetic, vectors, matrices, and function calls
- A numeric evaluator with clear dimension/type errors
- Unit tests, end-to-end tests, and QuickCheck algebraic properties

## Run

Build and run tests:

```bash
stack test
```

Run the executable against a source file:

```bash
stack run mathlang-exe -- path/to/program.math
```

## Language Snapshot

- Top-level function definition: `def f(x, y) = x + y`
- Top-level variable definition: `x = [1,2,3]`
- Expression operators: `+`, `-`, `*`, `/`
- Built-in function: `dot(a, b)` for vector dot product
- Local binding: `let x = expr in expr`
- Literals: scalars, vectors (`[1,2,3]`), matrices (`[[1,2],[3,4]]`)