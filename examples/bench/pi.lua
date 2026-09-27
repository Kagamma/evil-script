-- Leibniz formula for π
-- Everything wrapped in a local function for better performance.

local function run_leibniz()
  local abs = math.abs
  local PI  = math.pi

  local t0 = os.clock()

  local n     = 100000000
  local pi    = 0.0
  local denom = 1.0
  local sgn   = 1.0

  for i = 0, n - 1 do
    pi = pi + sgn * (1.0 / denom)
    denom = denom + 2.0
    sgn = -sgn
  end

  pi = 4.0 * pi

  local t1 = os.clock()

  print("\nTime: ", (t1 - t0) * 1000, " ms")
  print("Actual pi: ", PI)
  print("Calculated pi: ", pi)
  print("Error: ", abs(PI - pi))
end

-- Run it
run_leibniz()