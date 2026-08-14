import math
import time

def nbody_benchmark(steps=100000, dt=0.01):
    # Simple solar-system n-body benchmark (Sun + 8 planets)
    # Units: AU, days, solar masses. G is absorbed into SOLAR_MASS.

    SOLAR_MASS = 4 * math.pi * math.pi
    DAYS_PER_YEAR = 365.24
    N = 9

    # Parallel arrays (local for better performance)
    x = [0.0] * N
    y = [0.0] * N
    z = [0.0] * N
    vx = [0.0] * N
    vy = [0.0] * N
    vz = [0.0] * N
    m = [0.0] * N

    # ---------------------------------------------------------------
    # Initial conditions (approximate J2000-ish, heliocentric)
    # ---------------------------------------------------------------

    # 0 Sun
    x[0] = 0.0
    y[0] = 0.0
    z[0] = 0.0
    vx[0] = 0.0
    vy[0] = 0.0
    vz[0] = 0.0
    m[0] = SOLAR_MASS

    # 1 Mercury
    x[1] = 0.387098
    y[1] = 0.0
    z[1] = 0.0
    vx[1] = 0.0
    vy[1] = 0.024600 * DAYS_PER_YEAR
    vz[1] = 0.0
    m[1] = 1.6601e-7 * SOLAR_MASS

    # 2 Venus
    x[2] = 0.723332
    y[2] = 0.0
    z[2] = 0.0
    vx[2] = 0.0
    vy[2] = 0.0179 * DAYS_PER_YEAR
    vz[2] = 0.0
    m[2] = 2.4478e-6 * SOLAR_MASS

    # 3 Earth
    x[3] = 1.000000
    y[3] = 0.0
    z[3] = 0.0
    vx[3] = 0.0
    vy[3] = 0.017202 * DAYS_PER_YEAR
    vz[3] = 0.0
    m[3] = 3.003e-6 * SOLAR_MASS

    # 4 Mars
    x[4] = 1.523679
    y[4] = 0.0
    z[4] = 0.0
    vx[4] = 0.0
    vy[4] = 0.01396 * DAYS_PER_YEAR
    vz[4] = 0.0
    m[4] = 3.227e-7 * SOLAR_MASS

    # 5 Jupiter
    x[5] = 5.204267
    y[5] = 0.0
    z[5] = 0.0
    vx[5] = 0.0
    vy[5] = 0.00756 * DAYS_PER_YEAR
    vz[5] = 0.0
    m[5] = 9.5479e-4 * SOLAR_MASS

    # 6 Saturn
    x[6] = 9.582017
    y[6] = 0.0
    z[6] = 0.0
    vx[6] = 0.0
    vy[6] = 0.00558 * DAYS_PER_YEAR
    vz[6] = 0.0
    m[6] = 2.8588e-4 * SOLAR_MASS

    # 7 Uranus
    x[7] = 19.229411
    y[7] = 0.0
    z[7] = 0.0
    vx[7] = 0.0
    vy[7] = 0.00393 * DAYS_PER_YEAR
    vz[7] = 0.0
    m[7] = 4.3662e-5 * SOLAR_MASS

    # 8 Neptune
    x[8] = 30.103661
    y[8] = 0.0
    z[8] = 0.0
    vx[8] = 0.0
    vy[8] = 0.00312 * DAYS_PER_YEAR
    vz[8] = 0.0
    m[8] = 5.1514e-5 * SOLAR_MASS

    # ---------------------------------------------------------------
    # Offset momentum so the system barycentre stays at origin
    # ---------------------------------------------------------------
    def offset_momentum():
        px = 0.0
        py = 0.0
        pz = 0.0
        for i in range(N):
            px += vx[i] * m[i]
            py += vy[i] * m[i]
            pz += vz[i] * m[i]
        vx[0] = -px / m[0]
        vy[0] = -py / m[0]
        vz[0] = -pz / m[0]

    # ---------------------------------------------------------------
    # Total energy (kinetic + potential)
    # ---------------------------------------------------------------
    def energy():
        e = 0.0
        for i in range(N):
            # kinetic
            e += 0.5 * m[i] * (vx[i] * vx[i] + vy[i] * vy[i] + vz[i] * vz[i])
            # potential
            for j in range(i + 1, N):
                dx = x[i] - x[j]
                dy = y[i] - y[j]
                dz = z[i] - z[j]
                distance = math.sqrt(dx * dx + dy * dy + dz * dz)
                e -= (m[i] * m[j]) / distance
        return e

    # ---------------------------------------------------------------
    # Advance the system by dt (symplectic Euler)
    # ---------------------------------------------------------------
    def advance(dt):
        # update velocities
        for i in range(N):
            for j in range(i + 1, N):
                dx = x[i] - x[j]
                dy = y[i] - y[j]
                dz = z[i] - z[j]
                dist2 = dx * dx + dy * dy + dz * dz
                dist = math.sqrt(dist2)
                mag = dt / (dist2 * dist)
                # mutual force
                fx = dx * mag
                fy = dy * mag
                fz = dz * mag
                vx[i] -= fx * m[j]
                vy[i] -= fy * m[j]
                vz[i] -= fz * m[j]
                vx[j] += fx * m[i]
                vy[j] += fy * m[i]
                vz[j] += fz * m[i]
        # update positions
        for i in range(N):
            x[i] += dt * vx[i]
            y[i] += dt * vy[i]
            z[i] += dt * vz[i]

    # ---------------------------------------------------------------
    # Benchmark
    # ---------------------------------------------------------------
    offset_momentum()
    print('Initial energy:', energy())
    t0 = time.perf_counter()
    for _ in range(steps):
        advance(dt)
    t1 = time.perf_counter()
    print('Final energy:', energy())
    print('Steps:', steps)
    print('Time:', (t1 - t0) * 1000, 'ms')


if __name__ == "__main__":
    nbody_benchmark()