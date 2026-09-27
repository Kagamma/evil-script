-- Algorithm from the Computer Language Benchmarks Game
-- Spectral Norm (approximate largest singular value of matrix A)
-- Everything wrapped in a local function for better performance.

local function run_spectral_norm()
  local sqrt = math.sqrt
  local N = 1000

  local function A(i, j)
    -- note: i,j are 0-based like the original
    return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
  end

  local function MultiplyAv(n, v, Av)
    for i = 0, n - 1 do
      local s = 0.0
      for j = 0, n - 1 do
        s = s + A(i, j) * v[j + 1]   -- Lua arrays are 1-based
      end
      Av[i + 1] = s
    end
  end

  local function MultiplyAtv(n, v, Atv)
    for i = 0, n - 1 do
      local s = 0.0
      for j = 0, n - 1 do
        s = s + A(j, i) * v[j + 1]
      end
      Atv[i + 1] = s
    end
  end

  local function MultiplyAtAv(n, v, AtAv)
    local u = {}
    -- pre-size is not strictly necessary in Lua, but we can leave it empty
    MultiplyAv(n, v, u)
    MultiplyAtv(n, u, AtAv)
  end

  -- ---------- main ----------
  local u = {}
  local v = {}

  for i = 1, N do
    u[i] = 1.0
  end

  local t0 = os.clock()

  for i = 1, 10 do          -- 10 iterations of the power method
    MultiplyAtAv(N, u, v)
    MultiplyAtAv(N, v, u)
  end

  local vBv = 0.0
  local vv  = 0.0
  for i = 1, N do
    vBv = vBv + u[i] * v[i]
    vv  = vv  + v[i] * v[i]
  end

  local res = sqrt(vBv / vv)

  local t1 = os.clock()

  print(res)
  print("Time: ", (t1 - t0) * 1000, " ms")
end

-- Run it
run_spectral_norm()