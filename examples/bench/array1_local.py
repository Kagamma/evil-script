import time

def test():
    start = time.time()
    total = [0.0]
    for i in range(9999):
        for j in range(9999):
            total[0] += i * j
    print("Time: ", time.time() - start, "s")
    print("Total: ", total[0])

test()