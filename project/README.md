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
stack run mathlang-exe -- examples/loss.math
```

## Project Structure

- `src/MathLang/AST.hs`: Core language syntax tree (programs, expressions, operators).
- `src/MathLang/Parser.hs`: Megaparsec parser from source text to AST.
- `src/MathLang/Eval.hs`: Evaluator for scalar/vector/matrix operations and errors.
- `src/MathLang.hs`: Public module re-exporting main language API.
- `src/Main.hs`: CLI entry point (`mathlang-exe`) that runs a program file.
- `test/Spec.hs`: Hspec + QuickCheck tests (unittests, algebraic properties).
- `examples/loss.math`: Sample MathLang program for quick execution.
- `stack.yaml` / `package.yaml`: Build configuration and package metadata.

## Language Snapshot

- Top-level function definition: `def f(x, y) = x + y`
- Top-level variable definition: `x = [1,2,3]`
- Expression operators: `+`, `-`, `*`, `/`
- Built-in function: `dot(a, b)` for vector dot product
- Local binding: `let x = expr in expr`
- Literals: scalars, vectors (`[1,2,3]`), matrices (`[[1,2],[3,4]]`)