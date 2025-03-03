import time
import math

def test():
    start = time.time()
    n = 100000000
    pi_approx = 0
    denom = 1
    sgn = 1
    for i in range(n):
        pi_approx += sgn * (1 / denom)
        denom += 2
        sgn *= -1
    pi_approx = 4 * pi_approx

    # Print results
    print("Time: ", time.time() - start, "s")
    print("Actual π ≈", math.pi)
    print("Calculated π ≈ ", pi_approx)
    print("Error: ", abs(math.pi - pi_approx))

test()