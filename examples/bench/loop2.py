import time

def test():
  start = time.time()
  total = 0.0
  i = 0
  while i <= 9999:
    j = 0
    while j <= 9999:
      total += i * j
      j += 1
    i += 1
  print("Time: ", time.time() - start, "s")
  print("Total: ", total)

test()