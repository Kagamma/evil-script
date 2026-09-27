-- Simple solar-system n-body benchmark (Sun + 8 planets)
-- Units: AU, days, solar masses.  G is absorbed into SOLAR_MASS.
-- Everything wrapped in a local function for better performance (locals, no globals).

local function run_nbody()
  local sqrt = math.sqrt
  local PI = math.pi
  local SOLAR_MASS = 4 * PI * PI
  local DAYS_PER_YEAR = 365.24

  -- Body count
  local N = 9

  -- Parallel arrays (1-based in Lua)
  local x  = {}
  local y  = {}
  local z  = {}
  local vx = {}
  local vy = {}
  local vz = {}
  local m  = {}

  -- ---------------------------------------------------------------
  -- Initial conditions (approximate J2000-ish, heliocentric)
  -- ---------------------------------------------------------------

  -- 1 Sun  (Lua arrays are 1-based)
  x[1]  = 0
  y[1]  = 0
  z[1]  = 0
  vx[1] = 0
  vy[1] = 0
  vz[1] = 0
  m[1]  = SOLAR_MASS

  -- 2 Mercury
  x[2]  =  0.387098
  y[2]  =  0
  z[2]  =  0
  vx[2] =  0
  vy[2] =  0.024600 * DAYS_PER_YEAR
  vz[2] =  0
  m[2]  = 1.6601e-7 * SOLAR_MASS

  -- 3 Venus
  x[3]  =  0.723332
  y[3]  =  0
  z[3]  =  0
  vx[3] =  0
  vy[3] =  0.0179 * DAYS_PER_YEAR
  vz[3] =  0
  m[3]  = 2.4478e-6 * SOLAR_MASS

  -- 4 Earth
  x[4]  =  1.000000
  y[4]  =  0
  z[4]  =  0
  vx[4] =  0
  vy[4] =  0.017202 * DAYS_PER_YEAR
  vz[4] =  0
  m[4]  = 3.003e-6 * SOLAR_MASS

  -- 5 Mars
  x[5]  =  1.523679
  y[5]  =  0
  z[5]  =  0
  vx[5] =  0
  vy[5] =  0.01396 * DAYS_PER_YEAR
  vz[5] =  0
  m[5]  = 3.227e-7 * SOLAR_MASS

  -- 6 Jupiter
  x[6]  =  5.204267
  y[6]  =  0
  z[6]  =  0
  vx[6] =  0
  vy[6] =  0.00756 * DAYS_PER_YEAR
  vz[6] =  0
  m[6]  = 9.5479e-4 * SOLAR_MASS

  -- 7 Saturn
  x[7]  =  9.582017
  y[7]  =  0
  z[7]  =  0
  vx[7] =  0
  vy[7] =  0.00558 * DAYS_PER_YEAR
  vz[7] =  0
  m[7]  = 2.8588e-4 * SOLAR_MASS

  -- 8 Uranus
  x[8]  = 19.229411
  y[8]  =  0
  z[8]  =  0
  vx[8] =  0
  vy[8] =  0.00393 * DAYS_PER_YEAR
  vz[8] =  0
  m[8]  = 4.3662e-5 * SOLAR_MASS

  -- 9 Neptune
  x[9]  = 30.103661
  y[9]  =  0
  z[9]  =  0
  vx[9] =  0
  vy[9] =  0.00312 * DAYS_PER_YEAR
  vz[9] =  0
  m[9]  = 5.1514e-5 * SOLAR_MASS

  -- ---------------------------------------------------------------
  -- Offset momentum so the system barycentre stays at origin
  -- ---------------------------------------------------------------
  local function offset_momentum()
    local px, py, pz = 0, 0, 0
    for i = 1, N do
      px = px + vx[i] * m[i]
      py = py + vy[i] * m[i]
      pz = pz + vz[i] * m[i]
    end
    vx[1] = -px / m[1]
    vy[1] = -py / m[1]
    vz[1] = -pz / m[1]
  end

  -- ---------------------------------------------------------------
  -- Total energy (kinetic + potential)
  -- ---------------------------------------------------------------
  local function energy()
    local e = 0
    for i = 1, N do
      -- kinetic
      e = e + 0.5 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i])

      -- potential
      for j = i + 1, N do
        local dx = x[i] - x[j]
        local dy = y[i] - y[j]
        local dz = z[i] - z[j]
        local distance = sqrt(dx*dx + dy*dy + dz*dz)
        e = e - (m[i] * m[j]) / distance
      end
    end
    return e
  end

  -- ---------------------------------------------------------------
  -- Advance the system by dt (symplectic Euler)
  -- ---------------------------------------------------------------
  local function advance(dt)
    -- update velocities
    for i = 1, N do
      for j = i + 1, N do
        local dx = x[i] - x[j]
        local dy = y[i] - y[j]
        local dz = z[i] - z[j]
        local dist2 = dx*dx + dy*dy + dz*dz
        local dist = sqrt(dist2)
        local mag = dt / (dist2 * dist)

        -- mutual force
        local fx = dx * mag
        local fy = dy * mag
        local fz = dz * mag

        vx[i] = vx[i] - fx * m[j]
        vy[i] = vy[i] - fy * m[j]
        vz[i] = vz[i] - fz * m[j]

        vx[j] = vx[j] + fx * m[i]
        vy[j] = vy[j] + fy * m[i]
        vz[j] = vz[j] + fz * m[i]
      end
    end

    -- update positions
    for i = 1, N do
      x[i] = x[i] + dt * vx[i]
      y[i] = y[i] + dt * vy[i]
      z[i] = z[i] + dt * vz[i]
    end
  end

  -- ---------------------------------------------------------------
  -- Benchmark
  -- ---------------------------------------------------------------
  offset_momentum()

  print("Initial energy: ", energy())

  local t0 = os.clock()   -- seconds (high-resolution on most Lua implementations)

  local steps = 100000    -- increase for a heavier benchmark
  local dt    = 0.01      -- days

  for s = 1, steps do
    advance(dt)
  end

  local t1 = os.clock()

  print("Final energy:   ", energy())
  print("Steps:          ", steps)
  print("Time:           ", (t1 - t0) * 1000, " ms")  -- convert to ms
end

-- Run it
run_nbody()