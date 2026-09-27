-- Find the largest prime ≤ 1 000 000 (naive trial division)
-- Everything wrapped in a local function for better performance.

local function run_primes()
  local sqrt = math.sqrt
  local floor = math.floor

  local t0 = os.clock()

  local last_prime = 0

  for n = 3, 1000000 do
    local is_prime = true
    local limit = floor(sqrt(n))
    for i = 2, limit do
      if n % i == 0 then
        is_prime = false
        break
      end
    end
    if is_prime then
      last_prime = n
    end
  end

  local t1 = os.clock()

  print("\nTime: ", (t1 - t0) * 1000, " ms")
  print("Last prime found: ", last_prime)
end

-- Run it
run_primes()