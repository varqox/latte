# Latte compiler

## Implemented language extensions
- arrays
- classes with inheritance and virtual methods
- null implicitly converts to any array and to any class
- destructors (garbage collection); can be disabled -- see [Compiler flags](#c-flags).

## Language semantics
- variables namespace is independent of the functions (and methods) namespace, e.g. (accepted code):
```latte
int foo() { return 42; }
int main() {
    int foo = 3;
    if (foo() + foo != 45) error();
    return 0;
}
```
- field shadowing is not possible, e.g. (rejected code):
```latte
class X {
    int x;
}
class Y extends X {
    string x;
}
int main() {
    return 0;
}
```

## Tests
Tests comprehensively cover language construct, type conversions and required static analysis. Some tests reflect chosen language semantics, thus compilers implementing other interpretations may not pass them.

Tests are not divided into ones categories based on which extensions they require.
Test folder structure:
- `good/` -- tests that present correct programs
- `bad/` -- tests that present incorrect programs
- `warnings/` -- tests that present programs producing diagnostic warnings

## How to build
Just use
```
make
```

## Used tools
Project is implemented in C++17. For parsing BNFC with C backend was used.

## Project structure
- my tests in folders `good/`, `bad/`, `warnings/`
- `src/` -- project sources
    - `src/ByBnfc/` -- source files generated by BNFC from `src/Latte.cf`
    - `src/ast/` -- AST and code for transforming BNFC-generated AST into my AST
        - `src/ast/build.cc.template` -- transforming BNFC-generated AST into my AST
        - `build_gen.py` -- script for generating `src/ast/build.cc` from `src/ast/build.cc.template`
    - `src/backend/x86_64.cc` -- translating IR to x86_64 assembly
    - `src/frontend/error.hh` -- utility to pretty-print compilation errors and warnings
    - `src/frontend/global_symbols.cc` -- building table of all global symbols aka. functions, classes and their fields and methods + analyzing inheritance and virtual methods
    - `src/frontend/type_checker.cc` -- checking type correctness
    - `src/frontend/static_analyzer.cc` -- static analysis i.e. compilation time computations, code reachability and control flow checking
    - `src/ir/ast_to_ir.cc` -- translating AST to IR
    - `src/ir/ir.hh` -- definition of the IR language
    - `src/ir/ir_printer.cc` -- printing of the IR language
    - `src/latc_x86_64.cc` -- main code, that glues everything together
    - `src/persistent_map.hh` -- persistent map implemented using Treap (a randomized BST)
    - `src/persistent_map_tester.cc` -- test for `src/persistent_map.hh`

## Backend
Compiler backend is x86_64 in Intel flavor (NASM is used for assembly). So far it is quite simple and without optimizations. Nonetheless, everything (including memory reclaiming) seems to work.

### Calling convention
Arguments are passed on the stack, in a similar way that is in x86 C ABI.
All registers except `RAX` are callee-save, `RAX` is used for the result, and is caller-save.

### Memory reclaiming
Is realized via reference counting -- just like C++'s `std::shared_ptr`. Types that have reference counting: string, classes and arrays.

### Builtin functions
Are implemented in assembly (using libc) and are pasted at the beginning of every generated `*.s` file.

## Compiler flags <a name="c-flags"></a>
* `--emit-ir` -- save IR (after applying optimizations) to file (for file `foo/bar.lat` writes IR to `foo/bar.ir`)
* `--disable-destructors` -- disable destructors (garbage collection implementation)
* `--no-optimizations` -- disables all optimizations: constant propagation, GCSE, etc.
