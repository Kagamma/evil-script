-- Recursive Fibonacci (naive)
-- Everything wrapped in a local function for better performance.

local function run_fib()
  local function fib(n)
    if n < 2 then
      return n
    else
      return fib(n - 1) + fib(n - 2)
    end
  end

  local t0 = os.clock()

  local f = fib(36)

  local t1 = os.clock()

  print("\nTime: ", (t1 - t0) * 1000, " ms")
  print("Fib(36): ", f)
end

-- Run it
run_fib()