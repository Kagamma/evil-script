# Matrix multiplication benchmark – optimized pure-Python version

import time

def create_matrix(n: int, value: float) -> list[list[float]]:
    """Create an n×n matrix filled with `value`."""
    return [[value] * n for _ in range(n)]


def matrix_mul(a: list[list[float]], b: list[list[float]], n: int) -> list[list[float]]:
    """
    Naive matrix multiplication C = A × B
    Optimized for pure Python:
    - local variables
    - row caching
    - minimal indexing
    """
    c = [[0.0] * n for _ in range(n)]

    for i in range(n):
        ci = c[i]          # cache row of C
        for j in range(n):   
            s = 0
            for k in range(n):
                s += a[i][k] * b[k][j]
            ci[j] = s
    return c


def run_benchmark(n: int = 200, iterations: int = 6) -> None:
    print(f"Creating {n}x{n} matrices...")
    A = create_matrix(n, 1.1)
    B = create_matrix(n, 2.2)

    print("Warm-up...")
    C = matrix_mul(A, B, n)

    print(f"Benchmarking {iterations} multiplications...")
    t0 = time.perf_counter()

    for _ in range(iterations):
        C = matrix_mul(A, B, n)

    elapsed_ms = (time.perf_counter() - t0) * 1000
    avg_ms = elapsed_ms / iterations
    gflops = (2.0 * n * n * n * iterations) / (elapsed_ms * 1e6)

    print(f"Result sample : C[0][0] = {C[0][0]}")
    print(f"Total time    : {elapsed_ms:.2f} ms")
    print(f"Average       : {avg_ms:.2f} ms per multiplication")
    print(f"GFLOPS (approx): {gflops:.4f}")


if __name__ == "__main__":
    run_benchmark(n=200, iterations=10)