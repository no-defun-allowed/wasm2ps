this project converts a very small subset (integers and control flow)
of WASM to PostScript so you can run it on the printer

corporate implementations target web browsers, servers, crabs, but
none target the untapped potential of the shitty PostScript
interpreter your printer has

Note wasm2ps requires `wasm2wat` cause I am lazy and to be fair this
project should not exist

# Usage

Write `fib.c`:
```c
int fib(int n) {
  return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

int ps_main() {
  return fib(20);
}
```

```sh
clang --target=wasm32-unknown-wasi -O3 -c fib.c 
```

```lisp
(wasm2ps:wasm2ps "fib.o" :output "fib.ps")
```

Now view `fib.ps` and the value returned by `ps_main()` will be
printed on the single page

# Performance

Non existent. The so-called compiler implemented uses the dumbest
possible implementation of control flow and looks up every local in a
lexical, so combined with a crappy PS implementation and a slow
printer processor, you will be waiting about 30 seconds for `fib(20)`.
