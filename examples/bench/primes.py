import time
import math 
        
start = time.time()

last_prime = 0
for n in range(3, 1000000):
  # Check divisibility up to square root of n
  for i in range(2, int(math.sqrt(n)) + 1):
    if n % i == 0:
      break
  else:
    last_prime = n

print("Time: ", time.time() - start, "s")  
print("Last prime found: ", last_prime)