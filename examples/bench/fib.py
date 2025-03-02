import time

def fib(n):
    # Base cases
    if n < 2:
        return n
    # Recursive case: Fib(n) = Fib(n-1) + Fib(n-2)
    else:
        return fib(n-1) + fib(n-2)

start = time.time()
f = fib(36)
print("Time: ", time.time() - start, "s")
print("Fib(36): ", f)