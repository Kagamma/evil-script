import time
start = time.time()
total = 0.0
for i in range(9999):
    for j in range(9999):
        total += i * j
print("Time: ", time.time() - start, "s")
print("Total: ", total)