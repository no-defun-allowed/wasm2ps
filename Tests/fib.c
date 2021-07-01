int fib(int n) {
  return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

int ps_main() {
  return fib(20);
}
