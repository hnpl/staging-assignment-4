# Benchmarks

This folder consists of `riscv-tests` pulled from
[the official riscv tests](https://github.com/riscv-software-src/riscv-tests/tree/master/benchmarks)
and an extra benchmark, stream, which is inspired from
[the STREAM benchmark](https://www.cs.virginia.edu/stream/).

The benchmarks are mostly unmodified, except for,
- `matmul`: the matmul version in the official riscv-tests is multithread.
In this resource, we made it single-threaded. The matrix generator is also rewritten in Python.
- `stream`: our stream version only performs single-threaded memcpy(), while the original STREAM
benchmark performs 4 parallelized operations (copy, scale, add, triad).

Because we want to have various sizes for matrix multiplication and stream, we wrote new Makefile's
taking size of matricies/array as paramters for those two benchmarks and generate matricies and
arrays accordingly to the given sizes.

The new Makefile's are `Makefile-stream` and `Makefile-matmul`.
