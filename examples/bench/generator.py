import time

def numeric_for_loop(N: int = 4_999_999) -> None:
    print("Numeric for loop")
    t = time.perf_counter()
    a = 0
    for i in range(N + 1):  # inclusive like "to N"
        a += i
    elapsed_ms = (time.perf_counter() - t) * 1000
    print(f"\nTime: {elapsed_ms:.3f}ms")
    print(f"Total: {a}")
    print()


def generator_for_loop(N: int = 4_999_999) -> None:
    print("For loop using generator function")
    t = time.perf_counter()

    def range_gen(minv: int, maxv: int):
        for i in range(minv, maxv + 1):  # inclusive
            yield i

    a = 0
    for i in range_gen(0, N):
        a += i
    elapsed_ms = (time.perf_counter() - t) * 1000
    print(f"\nTime: {elapsed_ms:.3f}ms")
    print(f"Total: {a}")


if __name__ == "__main__":
    numeric_for_loop()
    generator_for_loop()