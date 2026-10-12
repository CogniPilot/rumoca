model E30x30
  "2-D compressible Euler on a periodic-free box, first-order Rusanov, method of lines."
  parameter Integer nx = 30;
  parameter Integer ny = 30;
  parameter Real g = 1.4;
  parameter Real dx = 1.0 / nx;
  parameter Real dy = 1.0 / ny;
  Real r[nx, ny](each start = 1.0, each fixed = true);
  Real mu[nx, ny](each start = 0.0, each fixed = true);
  Real mv[nx, ny](each start = 0.0, each fixed = true);
  Real E[nx, ny](each start = 2.5, each fixed = true);
  Real p[nx, ny];
  Real c[nx, ny];
  Real fr[nx + 1, ny];
  Real fmu[nx + 1, ny];
  Real fmv[nx + 1, ny];
  Real fE[nx + 1, ny];
  Real gr[nx, ny + 1];
  Real gmu[nx, ny + 1];
  Real gmv[nx, ny + 1];
  Real gE[nx, ny + 1];
  Real ax[nx + 1, ny];
  Real ay[nx, ny + 1];
equation
  for i in 1:nx loop
    for j in 1:ny loop
      p[i, j] = (g - 1) * (E[i, j] - 0.5 * (mu[i, j] ^ 2 + mv[i, j] ^ 2) / max(r[i, j], 1e-6));
      c[i, j] = sqrt(g * max(p[i, j], 1e-6) / max(r[i, j], 1e-6));
    end for;
  end for;
  // x faces: walls at both ends, interior Rusanov
  for j in 1:ny loop
    ax[1, j] = 0; fr[1, j] = 0; fmu[1, j] = p[1, j]; fmv[1, j] = 0; fE[1, j] = 0;
    ax[nx + 1, j] = 0; fr[nx + 1, j] = 0; fmu[nx + 1, j] = p[nx, j]; fmv[nx + 1, j] = 0; fE[nx + 1, j] = 0;
    for f in 2:nx loop
      ax[f, j] = noEvent(max(abs(mu[f - 1, j] / r[f - 1, j]) + c[f - 1, j], abs(mu[f, j] / r[f, j]) + c[f, j]));
      fr[f, j] = 0.5 * (mu[f - 1, j] + mu[f, j]) - 0.5 * ax[f, j] * (r[f, j] - r[f - 1, j]);
      fmu[f, j] = 0.5 * (mu[f - 1, j] ^ 2 / r[f - 1, j] + p[f - 1, j] + mu[f, j] ^ 2 / r[f, j] + p[f, j]) - 0.5 * ax[f, j] * (mu[f, j] - mu[f - 1, j]);
      fmv[f, j] = 0.5 * (mu[f - 1, j] * mv[f - 1, j] / r[f - 1, j] + mu[f, j] * mv[f, j] / r[f, j]) - 0.5 * ax[f, j] * (mv[f, j] - mv[f - 1, j]);
      fE[f, j] = 0.5 * ((E[f - 1, j] + p[f - 1, j]) * mu[f - 1, j] / r[f - 1, j] + (E[f, j] + p[f, j]) * mu[f, j] / r[f, j]) - 0.5 * ax[f, j] * (E[f, j] - E[f - 1, j]);
    end for;
  end for;
  // y faces
  for i in 1:nx loop
    ay[i, 1] = 0; gr[i, 1] = 0; gmu[i, 1] = 0; gmv[i, 1] = p[i, 1]; gE[i, 1] = 0;
    ay[i, ny + 1] = 0; gr[i, ny + 1] = 0; gmu[i, ny + 1] = 0; gmv[i, ny + 1] = p[i, ny]; gE[i, ny + 1] = 0;
    for f in 2:ny loop
      ay[i, f] = noEvent(max(abs(mv[i, f - 1] / r[i, f - 1]) + c[i, f - 1], abs(mv[i, f] / r[i, f]) + c[i, f]));
      gr[i, f] = 0.5 * (mv[i, f - 1] + mv[i, f]) - 0.5 * ay[i, f] * (r[i, f] - r[i, f - 1]);
      gmu[i, f] = 0.5 * (mu[i, f - 1] * mv[i, f - 1] / r[i, f - 1] + mu[i, f] * mv[i, f] / r[i, f]) - 0.5 * ay[i, f] * (mu[i, f] - mu[i, f - 1]);
      gmv[i, f] = 0.5 * (mv[i, f - 1] ^ 2 / r[i, f - 1] + p[i, f - 1] + mv[i, f] ^ 2 / r[i, f] + p[i, f]) - 0.5 * ay[i, f] * (mv[i, f] - mv[i, f - 1]);
      gE[i, f] = 0.5 * ((E[i, f - 1] + p[i, f - 1]) * mv[i, f - 1] / r[i, f - 1] + (E[i, f] + p[i, f]) * mv[i, f] / r[i, f]) - 0.5 * ay[i, f] * (E[i, f] - E[i, f - 1]);
    end for;
  end for;
  for i in 1:nx loop
    for j in 1:ny loop
      der(r[i, j]) = -(fr[i + 1, j] - fr[i, j]) / dx - (gr[i, j + 1] - gr[i, j]) / dy;
      der(mu[i, j]) = -(fmu[i + 1, j] - fmu[i, j]) / dx - (gmu[i, j + 1] - gmu[i, j]) / dy;
      der(mv[i, j]) = -(fmv[i + 1, j] - fmv[i, j]) / dx - (gmv[i, j + 1] - gmv[i, j]) / dy;
      der(E[i, j]) = -(fE[i + 1, j] - fE[i, j]) / dx - (gE[i, j + 1] - gE[i, j]) / dy;
    end for;
  end for;
end E30x30;
