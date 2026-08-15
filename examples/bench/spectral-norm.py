import math
import time
import sys


def spectral_norm(n: int) -> float:
    def A(i: int, j: int) -> float:
        return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)

    def multiply_av(v: list[float], Av: list[float]) -> None:
        for i in range(n):
            s = 0.0
            for j in range(n):
                s += A(i, j) * v[j]
            Av[i] = s

    def multiply_atv(v: list[float], Atv: list[float]) -> None:
        for i in range(n):
            s = 0.0
            for j in range(n):
                s += A(j, i) * v[j]
            Atv[i] = s

    def multiply_atav(v: list[float], AtAv: list[float]) -> None:
        u = [0.0] * n
        multiply_av(v, u)
        multiply_atv(u, AtAv)

    u = [1.0] * n
    v = [0.0] * n

    for _ in range(10):
        multiply_atav(u, v)
        multiply_atav(v, u)

    vBv = 0.0
    vv = 0.0
    for i in range(n):
        vBv += u[i] * v[i]
        vv += v[i] * v[i]

    return math.sqrt(vBv / vv)


def main() -> None:
    n = 1000
    if len(sys.argv) > 1:
        n = int(sys.argv[1])

    t0 = time.perf_counter()
    result = spectral_norm(n)
    t1 = time.perf_counter()

    print(f"{result:.9f}")
    print(f"Time: {(t1 - t0) * 1000:.2f} ms")


if __name__ == "__main__":
    main()