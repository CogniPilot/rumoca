/* MuJoCo twin of HumanoidPhysical.mo: the Modelica controller ported 1:1 to C.
 *
 * mode hc     : the GroundContact.mo contact law applied as body wrenches, MuJoCo contacts off,
 *               classic RK4 over the full ODE (MuJoCo dynamics + controller states path, pd).
 * mode native : MuJoCo's own contact and integrator (from the XML); controller held over each step,
 *               controller states integrated with explicit Euler.
 * Gait events follow the Modelica when/elsewhen (edge semantics, event iteration), checked per step.
 *
 * usage: twin <model.xml> <hc|native> <t_end> <h> <out.csv|-> [repeats]
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <mujoco/mujoco.h>
#include "params.h"

#define PI 3.14159265358979323846
static mjModel *m;
static mjData *d;
static int native, act_mode;
static int act_id[2][9];
static int b_pelvis, b_thigh[2], b_foot[2];
static int jq[2][9], jv[2][9]; /* hipRoll hipPitch knee anklePitch ankleRoll shoulder elbow finger thumb */
static const char *JN[9] = {"hipRoll", "hipPitch", "knee", "anklePitch", "ankleRoll", "shoulder", "elbow", "finger", "thumb"};
static int sph_body[N_SPH];
static double fz_body_foot[2];
static double walk_cmd, side_cmd;

/* gait state */
static int phase = 0, lastSwing = 2, steps = 0;
static double t_step0 = 0, t_touchdown = -1, footL[2] = {0, P_hip_w}, footR[2] = {0, -P_hip_w}, swingFrom[2] = {0, 0};
static int prev_c[2];
static int nevents;
static double ev_t[64];
static int ev_kind[64], ev_ph[64];

static double softplus(double x, double s) { return x > 0 ? x + s * log1p(exp(-x / s)) : s * log1p(exp(x / s)); }
static double clamp(double x, double lo, double hi) { return x < lo ? lo : x > hi ? hi : x; }

static void legIK(const double dd[3], double T, double S, double q[5]) {
  q[0] = atan2(dd[1], -dd[2]);
  double h = sqrt(dd[1] * dd[1] + dd[2] * dd[2]);
  double L = fmin(sqrt(dd[0] * dd[0] + h * h), 0.999 * (T + S));
  q[2] = PI - acos((T * T + S * S - L * L) / (2 * T * S));
  q[1] = -(atan2(dd[0], h) + acos((T * T + L * L - S * S) / (2 * T * L)));
  q[3] = -(q[1] + q[2]);
  q[4] = -q[0];
}

static double supportExcess(const double fL[2], const double fR[2], const double p[2], double heel, double toe, double hw) {
  double a = fmax(fmin(fL[0], fR[0]) + heel - p[0], p[0] - fmax(fL[0], fR[0]) - toe);
  double b = fmax(fmin(fL[1], fR[1]) - hw - p[1], p[1] - fmax(fL[1], fR[1]) - hw);
  return fmax(a, b);
}

static void stepTarget(const double st[2], const double cp[2], int side, double T, double out[2]) {
  double c[2] = {(cp[0] - (st[0] + P_foot_c)) * exp(P_omega_cp * T), (cp[1] - st[1]) * exp(P_omega_cp * T)};
  double n = fmax(sqrt(c[0] * c[0] + c[1] * c[1]), 1e-6);
  c[0] *= 2 + P_step_margin / n;
  c[1] *= 2 + P_step_margin / n;
  double W = 2 * P_hip_w;
  c[0] = clamp(c[0], -P_step_max, P_step_max);
  c[1] = side > 0 ? fmin(fmax(c[1], 0.75 * W), P_step_side_max) : fmax(fmin(c[1], -0.75 * W), -P_step_side_max);
  out[0] = st[0] + c[0];
  out[1] = st[1] + c[1];
}

static void walkTarget(const double st[2], const double cp[2], int side, double T, const double L[2], double out[2]) {
  double W = 2 * P_hip_w, base[2] = {st[0] + P_foot_c, st[1]};
  double xi0 = base[0] + (cp[0] - base[0]) * exp(P_omega_cp * T), xi1 = base[1] + (cp[1] - base[1]) * exp(P_omega_cp * T);
  double e = exp(P_omega_cp * (P_T_step + P_T_ds));
  out[0] = xi0 - L[0] / (e - 1) - P_foot_c;
  out[1] = xi1 - L[1] / (e - 1) + side * W / (e + 1);
  out[0] = clamp(out[0], st[0] - P_step_max, st[0] + P_step_max);
  out[1] = side > 0 ? fmin(fmax(out[1], st[1] + 0.5 * W), st[1] + P_step_side_max) : fmax(fmin(out[1], st[1] - 0.5 * W), st[1] - P_step_side_max);
}

static double actuatorLimit(double cmd, double w, double peak, double wmax) {
  double avail = cmd * w > 0 ? peak * fmax(0.05, 1 - fabs(w) / wmax) : peak;
  return avail * tanh(cmd / avail);
}

/* GroundContact.mo, summed per body into xfrc_applied; also the two feet's normal force */
static void contact_wrench(void) {
  mju_zero(d->xfrc_applied, 6 * m->nbody);
  fz_body_foot[0] = fz_body_foot[1] = 0;
  for (int i = 0; i < N_SPH; i++) {
    int b = sph_body[i];
    const double *R = d->xmat + 9 * b, *org = d->xpos + 3 * b;
    double arm[3];
    mju_mulMatVec(arm, R, SPH_C[i], 3, 3);
    if (org[2] + arm[2] - SPH_R[i] >= 50 * P_eps) continue;
    double low[3] = {org[0] + arm[0], org[1] + arm[1], org[2] + arm[2] - SPH_R[i]};
    const double *w = d->cvel + 6 * b, *vc = d->cvel + 6 * b + 3, *com = d->subtree_com + 3 * m->body_rootid[b];
    double r[3] = {low[0] - com[0], low[1] - com[1], low[2] - com[2]}, wxr[3];
    mju_cross(wxr, w, r);
    double v[3] = {vc[0] + wxr[0], vc[1] + wxr[1], vc[2] + wxr[2]};
    double fn = P_k_c * softplus(-low[2], P_eps) * softplus(1 - P_d_c * v[2], 0.05);
    double vt = sqrt(v[0] * v[0] + v[1] * v[1] + P_v_eps * P_v_eps);
    double f[3] = {-P_mu * fn * v[0] / vt, -P_mu * fn * v[1] / vt, fn};
    const double *ci = d->xipos + 3 * b;
    double ra[3] = {low[0] - ci[0], low[1] - ci[1], low[2] - ci[2]}, tq[3];
    mju_cross(tq, ra, f);
    double *x = d->xfrc_applied + 6 * b;
    for (int k = 0; k < 3; k++) { x[k] += f[k]; x[3 + k] += tq[k]; }
    if (b == b_foot[0]) fz_body_foot[0] += fn;
    if (b == b_foot[1]) fz_body_foot[1] += fn;
  }
}

static void native_foot_fz(void) {
  fz_body_foot[0] = fz_body_foot[1] = 0;
  for (int i = 0; i < d->ncon; i++) {
    const mjContact *c = d->contact + i;
    int b1 = m->geom_bodyid[c->geom[0]], b2 = m->geom_bodyid[c->geom[1]];
    double f6[6];
    for (int s = 0; s < 2; s++)
      if (b1 == b_foot[s] || b2 == b_foot[s]) { mj_contactForce(m, d, i, f6); fz_body_foot[s] += f6[0]; }
  }
}

/* controller: joint torques into qfrc_applied, derivative of xc = (path, pd) */
static void control(double t, const double xc[4], double dxc[4]) {
  int walking = fabs(walk_cmd) > P_walk_on || fabs(side_cmd) > P_walk_on;
  const double *pel = d->xpos + 3 * b_pelvis, *Rp = d->xmat + 9 * b_pelvis, *vel = d->qvel;
  double cp[2] = {pel[0] + vel[0] / P_omega_cp, pel[1] + vel[1] / P_omega_cp};
  double Ts = P_T_step, hw = P_hip_w;
  if (walking) { dxc[0] = walk_cmd; dxc[1] = side_cmd; }
  else { dxc[0] = (pel[0] - xc[0]) / 0.2; dxc[1] = (pel[1] - xc[1]) / 0.2; }
  double L[2] = {walk_cmd * (Ts + P_T_ds) + P_k_path * (xc[0] - pel[0]), side_cmd * (Ts + P_T_ds) + P_k_path * (xc[1] - pel[1])};
  double Tleft = fmax(Ts - (t - t_step0), 0), swingTo[2] = {0, 0};
  if (phase == 1) { if (walking) walkTarget(footR, cp, 1, Tleft, L, swingTo); else stepTarget(footR, cp, 1, Tleft, swingTo); }
  if (phase == 2) { if (walking) walkTarget(footL, cp, -1, Tleft, L, swingTo); else stepTarget(footL, cp, -1, Tleft, swingTo); }
  double s = clamp((t - t_step0) / Ts, 0, 1), bl = s * s * (3 - 2 * s);
  double swing[3] = {swingFrom[0] + (swingTo[0] - swingFrom[0]) * bl, swingFrom[1] + (swingTo[1] - swingFrom[1]) * bl, P_ankle_h + P_h_swing * sin(PI * s)};
  double aL[3] = {footL[0], footL[1], P_ankle_h}, aR[3] = {footR[0], footR[1], P_ankle_h};
  if (phase == 1) memcpy(aL, swing, sizeof swing);
  if (phase == 2) memcpy(aR, swing, sizeof swing);
  if (phase == 0) {
    dxc[2] = ((footL[0] + footR[0]) / 2 + P_walk_lead * walk_cmd - xc[2]) / P_tau_centre;
    dxc[3] = ((footL[1] + footR[1]) / 2 + P_walk_lead * side_cmd - xc[3]) / P_tau_centre;
  } else { dxc[2] = (pel[0] - xc[2]) / 0.02; dxc[3] = (pel[1] - xc[3]) / 0.02; }
  double q[2][5];
  for (int i = 0; i < 2; i++) {
    const double *a = i == 0 ? aL : aR;
    if (phase == i + 1) {
      const double *hip = d->xpos + 3 * b_thigh[i];
      double dw[3] = {a[0] - hip[0], a[1] - hip[1], a[2] - hip[2]}, dl[3];
      mju_mulMatTVec(dl, Rp, dw, 3, 3);
      legIK(dl, P_l_thigh, P_l_shin, q[i]);
    } else {
      double dl[3] = {a[0] - xc[2], a[1] - xc[3] - (i == 0 ? hw : -hw), a[2] - P_z_pelvis0};
      legIK(dl, P_l_thigh, P_l_shin, q[i]);
    }
  }
  double x_feet, y_feet;
  if (phase == 1) { x_feet = footR[0] + P_foot_c; y_feet = footR[1]; }
  else if (phase == 2) { x_feet = footL[0] + P_foot_c; y_feet = footL[1]; }
  else { x_feet = (footL[0] + footR[0]) / 2 + P_foot_c; y_feet = (footL[1] + footR[1]) / 2; }
  double tam = P_tau_ankle_max;
  double tbp = tam * tanh(P_k_cp * (cp[0] - x_feet - P_walk_lead * walk_cmd) / tam);
  double tbr = -tam * tanh(P_k_cp * (cp[1] - y_feet - P_walk_lead * side_cmd) / tam);
  static const double peaks[5] = {P_tau_hip_roll_max, P_tau_hip_pitch_max, P_tau_knee_max, P_tau_ankle_pitch_max, P_tau_ankle_roll_max};
  double grip = 0.5 - 0.5 * cos(2 * PI * t / P_grip_period);
  for (int i = 0; i < 2; i++) {
    for (int k = 0; k < 5; k++) {
      double qa = d->qpos[jq[i][k]], va = d->qvel[jv[i][k]];
      double extra = (k == 3 && phase != i + 1) ? tbp : (k == 4 && phase != i + 1) ? tbr : 0;
      if (act_mode) { d->ctrl[act_id[i][k]] = q[i][k] + extra / P_kp_leg; continue; }
      double cmd = P_kp_leg * (q[i][k] - qa) - P_kd_leg * va + extra;
      d->qfrc_applied[jv[i][k]] = actuatorLimit(cmd, va, peaks[k], P_w_leg_max);
    }
    double refs[4] = {0.0, -0.35, -0.35 + 1.1 * grip, 0.4 - 1.0 * grip};
    for (int k = 5; k < 9; k++) {
      double qa = d->qpos[jq[i][k]], va = d->qvel[jv[i][k]];
      if (act_mode) { d->ctrl[act_id[i][k]] = refs[k - 5]; continue; }
      if (k < 7)
        d->qfrc_applied[jv[i][k]] = actuatorLimit(P_kp_arm * (refs[k - 5] - qa) - P_kd_arm * va, va, k == 5 ? P_tau_shoulder_max : P_tau_elbow_max, P_w_arm_max);
      else
        d->qfrc_applied[jv[i][k]] = actuatorLimit(P_kp_finger * (refs[k - 5] - qa) - P_kd_finger * va, va, P_tau_finger_max, P_w_finger_max);
    }
  }
}

/* one RHS evaluation of the hc ODE at the state in d: returns qacc in d, dxc */
static void rhs(double t, const double xc[4], double dxc[4]) {
  mj_fwdPosition(m, d);
  mj_fwdVelocity(m, d);
  contact_wrench();
  control(t, xc, dxc);
  mj_fwdActuation(m, d);
  mj_fwdAcceleration(m, d);
  mj_fwdConstraint(m, d);
}

static double *q0, *v0, *qt, *kv[4], *ka[4];
static void step_rk4(double *t, double xc[4], double h) {
  int nq = m->nq, nv = m->nv;
  double x0[4], kx[4][4], xs[4];
  mju_copy(q0, d->qpos, nq); mju_copy(v0, d->qvel, nv); memcpy(x0, xc, sizeof x0);
  static const double c[4] = {0, 0.5, 0.5, 1};
  for (int s = 0; s < 4; s++) {
    if (s == 0) { mju_copy(d->qpos, q0, nq); mju_copy(d->qvel, v0, nv); memcpy(xs, x0, sizeof xs); }
    else {
      mju_copy(d->qpos, q0, nq); mj_integratePos(m, d->qpos, kv[s - 1], c[s] * h);
      for (int i = 0; i < nv; i++) d->qvel[i] = v0[i] + c[s] * h * ka[s - 1][i];
      for (int i = 0; i < 4; i++) xs[i] = x0[i] + c[s] * h * kx[s - 1][i];
    }
    mju_copy(kv[s], d->qvel, nv);
    rhs(*t + c[s] * h, xs, kx[s]);
    mju_copy(ka[s], d->qacc, nv);
  }
  for (int i = 0; i < nv; i++) {
    qt[i] = (kv[0][i] + 2 * kv[1][i] + 2 * kv[2][i] + kv[3][i]) / 6;
    d->qvel[i] = v0[i] + h * (ka[0][i] + 2 * ka[1][i] + 2 * ka[2][i] + ka[3][i]) / 6;
  }
  mju_copy(d->qpos, q0, nq); mj_integratePos(m, d->qpos, qt, h);
  for (int i = 0; i < 4; i++) xc[i] = x0[i] + h * (kx[0][i] + 2 * kx[1][i] + 2 * kx[2][i] + kx[3][i]) / 6;
  *t += h;
}

static void conditions(double t, int *c1, int *c2, double cp[2]) {
  int walking = fabs(walk_cmd) > P_walk_on || fabs(side_cmd) > P_walk_on;
  const double *pel = d->xpos + 3 * b_pelvis;
  cp[0] = pel[0] + d->qvel[0] / P_omega_cp; cp[1] = pel[1] + d->qvel[1] / P_omega_cp;
  double ce = supportExcess(footL, footR, cp, -0.07 + P_cp_margin, 0.18 - P_cp_margin, 0.055 - P_cp_margin);
  double stepNeed = phase == 0 ? ce : -1;
  double swingLoad = phase == 1 ? fz_body_foot[0] : phase == 2 ? fz_body_foot[1] : 0;
  double swingHeight = phase == 1 ? d->xpos[3 * b_foot[0] + 2] : phase == 2 ? d->xpos[3 * b_foot[1] + 2] : 1;
  double el = t - t_step0, Ts = P_T_step;
  *c1 = phase == 0 && pel[2] > P_z_fallen && (stepNeed > 0 || (walking && t - t_touchdown > P_T_ds));
  *c2 = phase > 0 && (el >= 2 * Ts || (el >= Ts && swingHeight < P_ankle_h + 0.015) || (el >= 0.5 * Ts && swingLoad > P_f_touchdown));
}

static void handle_events(double t) {
  for (int it = 0; it < 10; it++) {
    int c1, c2; double cp[2];
    conditions(t, &c1, &c2, cp);
    int fired = 0;
    if (c1 && !prev_c[0]) {
      int walking = fabs(walk_cmd) > P_walk_on || fabs(side_cmd) > P_walk_on;
      int cont = t - t_touchdown < 0.3 || walking;
      int ph = (cont || fabs(cp[1] - (footL[1] + footR[1]) / 2) <= 0.02) ? (lastSwing == 1 ? 2 : 1) : (cp[1] > (footL[1] + footR[1]) / 2 ? 1 : 2);
      phase = lastSwing = ph;
      memcpy(swingFrom, ph == 1 ? footL : footR, sizeof swingFrom);
      t_step0 = t; fired = 1;
      if (nevents < 64) { ev_t[nevents] = t; ev_kind[nevents] = 0; ev_ph[nevents++] = ph; }
    } else if (c2 && !prev_c[1]) {
      int ph = phase;
      if (ph == 1) { footL[0] = d->xpos[3 * b_foot[0]]; footL[1] = d->xpos[3 * b_foot[0] + 1]; }
      if (ph == 2) { footR[0] = d->xpos[3 * b_foot[1]]; footR[1] = d->xpos[3 * b_foot[1] + 1]; }
      phase = 0; t_touchdown = t; steps++; fired = 1;
      if (nevents < 64) { ev_t[nevents] = t; ev_kind[nevents] = 1; ev_ph[nevents++] = ph; }
    }
    prev_c[0] = c1; prev_c[1] = c2;
    if (!fired) return;
  }
  fprintf(stderr, "event iteration did not converge at t=%g\n", t);
  exit(3);
}

static void walk_input(double t) { walk_cmd = (t >= 1 && t < 3) ? 0.3 : 0.0; side_cmd = 0; }

static void reset_state(void) {
  mj_resetData(m, d);
  for (int i = 0; i < 2; i++) {
    d->qpos[jq[i][1]] = P_q_hip0; d->qpos[jq[i][2]] = P_q_knee0; d->qpos[jq[i][3]] = P_q_ankle0;
    d->qpos[jq[i][6]] = -0.35; d->qpos[jq[i][7]] = -0.35; d->qpos[jq[i][8]] = 0.4;
  }
  phase = 0; lastSwing = 2; steps = 0; t_step0 = 0; t_touchdown = -1;
  footL[0] = 0; footL[1] = P_hip_w; footR[0] = 0; footR[1] = -P_hip_w; swingFrom[0] = swingFrom[1] = 0;
  nevents = 0;
}

static double now(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return ts.tv_sec + 1e-9 * ts.tv_nsec; }

int main(int argc, char **argv) {
  if (argc < 6) { fprintf(stderr, "usage: twin model.xml hc|native t_end h out.csv|- [repeats]\n"); return 2; }
  char err[1000];
  m = mj_loadXML(argv[1], NULL, err, sizeof err);
  if (!m) { fprintf(stderr, "%s\n", err); return 2; }
  native = strncmp(argv[2], "native", 6) == 0;
  act_mode = strcmp(argv[2], "native_act") == 0;
  double t_end = atof(argv[3]), h = atof(argv[4]);
  int repeats = argc > 6 ? atoi(argv[6]) : 1;
  m->opt.timestep = h;
  d = mj_makeData(m);
  b_pelvis = mj_name2id(m, mjOBJ_BODY, "pelvis");
  const char *S = "LR";
  char name[64];
  for (int i = 0; i < 2; i++) {
    snprintf(name, sizeof name, "thigh%c", S[i]); b_thigh[i] = mj_name2id(m, mjOBJ_BODY, name);
    snprintf(name, sizeof name, "foot%c", S[i]); b_foot[i] = mj_name2id(m, mjOBJ_BODY, name);
    for (int k = 0; k < 9; k++) {
      snprintf(name, sizeof name, "%s%c", JN[k], S[i]);
      int j = mj_name2id(m, mjOBJ_JOINT, name);
      if (j < 0) { fprintf(stderr, "no joint %s\n", name); return 2; }
      jq[i][k] = m->jnt_qposadr[j]; jv[i][k] = m->jnt_dofadr[j];
      snprintf(name, sizeof name, "a_%s%c", JN[k], S[i]);
      act_id[i][k] = mj_name2id(m, mjOBJ_ACTUATOR, name);
      if (act_mode && act_id[i][k] < 0) { fprintf(stderr, "no actuator %s\n", name); return 2; }
    }
  }
  for (int i = 0; i < N_SPH; i++) sph_body[i] = mj_name2id(m, mjOBJ_BODY, SPH_BODY[i]);
  q0 = calloc(m->nq, sizeof(double)); v0 = calloc(m->nv, sizeof(double)); qt = calloc(m->nv, sizeof(double));
  for (int s = 0; s < 4; s++) { kv[s] = calloc(m->nv, sizeof(double)); ka[s] = calloc(m->nv, sizeof(double)); }
  int n = (int)llround(t_end / h), every = (int)llround(0.01 / h);
  double best = 1e30, xc[4], t = 0;
  FILE *out = NULL;
  for (int r = 0; r < repeats; r++) {
    reset_state();
    t = 0; memset(xc, 0, sizeof xc);
    walk_input(0);
    double dxc[4];
    if (native) { mj_forward(m, d); native_foot_fz(); }
    else rhs(0, xc, dxc);
    { int c1, c2; double cp[2]; conditions(0, &c1, &c2, cp); prev_c[0] = c1; prev_c[1] = c2; }
    if (r == repeats - 1 && strcmp(argv[5], "-") != 0) {
      out = fopen(argv[5], "w");
      fprintf(out, "time,pelvis_x,pelvis_y,pelvis_z,ankleL_x,ankleL_y,ankleL_z,ankleR_x,ankleR_y,ankleR_z,grf_left,grf_right,phase,steps\n");
    }
    double w0 = now();
    for (int k = 0; k < n; k++) {
      walk_input(t + 1e-9);
      if (native) {
        control(t, xc, dxc);
        mj_step(m, d);
        for (int i = 0; i < 4; i++) xc[i] += h * dxc[i];
        t += h;
        native_foot_fz();
      } else {
        step_rk4(&t, xc, h);
        rhs(t, xc, dxc);
      }
      handle_events(t);
      if (out && (k + 1) % every == 0) {
        const double *p = d->xpos + 3 * b_pelvis, *a = d->xpos + 3 * b_foot[0], *b = d->xpos + 3 * b_foot[1];
        fprintf(out, "%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%d,%d\n", t, p[0], p[1], p[2], a[0], a[1], a[2], b[0], b[1], b[2], fz_body_foot[0], fz_body_foot[1], phase, steps);
      }
      if (!isfinite(d->qpos[2])) { fprintf(stderr, "diverged at t=%g\n", t); return 4; }
    }
    double wall = now() - w0;
    if (wall < best) best = wall;
  }
  if (out) fclose(out);
  const double *p = d->xpos + 3 * b_pelvis;
  printf("{\"mode\":\"%s\",\"t_end\":%.6f,\"h\":%g,\"repeats\":%d,\"wall_best_s\":%.6f,\"wall_per_sim_s\":%.6f,\"steps\":%d,\"pelvis\":[%.4f,%.4f,%.4f],\"events\":[",
         argv[2], t, h, repeats, best, best / t, steps, p[0], p[1], p[2]);
  for (int i = 0; i < nevents; i++) printf("%s[%.5f,\"%s\",%d]", i ? "," : "", ev_t[i], ev_kind[i] ? "touchdown" : "lift", ev_ph[i]);
  printf("]}\n");
  mj_deleteData(d);
  mj_deleteModel(m);
  return 0;
}
