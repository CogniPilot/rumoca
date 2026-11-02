"""Joint-coordinate (MuJoCo) twin of HumanoidPhysical.mo: parameters, MJCF, contact law, controller.

Every number is read from the Modelica source (parse_params) so the two models cannot drift.
Body origins sit at the MSL frame_a of each body, joints at the body origin, so MuJoCo xpos equals
MSL frame_a.r_0 and joint angles equal MSL Revolute.phi.
"""
import math, re


def parse_params(path):
    """Numeric `parameter Real a = 1, b = 2;` defaults and simple expressions from the model."""
    src = open(path).read()
    src = re.sub(r'"[^"]*"', '', src)  # drop description strings
    p = {}
    for decl in re.findall(r'parameter Real ([^;]+);', src):
        for part in re.split(r',(?![^{]*})', decl):
            m = re.match(r'\s*(\w+)\s*=\s*([^{}]+?)\s*$', part)
            if not m:
                continue
            name, expr = m.groups()
            try:
                p[name] = float(eval(expr.replace('^', '**'), {"__builtins__": {}, "cos": math.cos, "sqrt": math.sqrt}, p))
            except Exception:
                pass
    return p


def derived(p):
    p = dict(p)
    p['z_pelvis0'] = p['l_thigh'] * math.cos(p['q_hip0']) + p['l_shin'] * math.cos(p['q_hip0'] + p['q_knee0']) + p['ankle_h'] + 0.002
    p['omega_cp'] = math.sqrt(9.81 / p['z_com'])
    return p


def contact_spheres(p):
    """(body name, centre in body frame, radius) for every GroundContactBody sphere."""
    s = []
    rs, ah = p['r_sole'], p['ankle_h']
    for side in 'LR':
        for c in [(0.18, 0.055), (0.18, -0.055), (-0.07, 0.055), (-0.07, -0.055)]:
            s.append(('foot' + side, (c[0], c[1], rs - ah), rs))
        s += [('shin' + side, (0, 0, 0), 0.06), ('shin' + side, (0, 0, -p['l_shin']), 0.05)]
        s += [('thigh' + side, (0, 0, 0), 0.07), ('thigh' + side, (0, 0, -p['l_thigh']), 0.065)]
        s += [('upperArm' + side, (0, 0, 0), 0.06), ('upperArm' + side, (0, 0, -p['l_upper_arm']), 0.05)]
        s += [('forearm' + side, (0, 0, -p['l_forearm']), 0.05)]
        s += [('finger' + side, (0, 0, -p['l_finger']), 0.012), ('thumb' + side, (0, 0, -p['l_thumb']), 0.012)]
    th = (0.11, 0.18, p['shoulder_h'] / 2)
    rt = 0.04
    for sx in (1, -1):
        for sy in (1, -1):
            for sz in (1, -1):
                s.append(('torso', (sx * (th[0] - rt), sy * (th[1] - rt), th[2] + sz * (th[2] - rt)), rt))
    s.append(('pelvis', (0, 0, 0), 0.12))
    ln = p['l_neck']
    for sx in (1, -1):
        for sy in (1, -1):
            for sz in (1, -1):
                s.append(('neck', (0.01 * sx, 0.04 * sy, ln + 0.04 * sz), 0.06))
    return s


def mjcf(p, timestep=1e-4, integrator='RK4', native_contact=False, actuators=False):
    def inertial(pos, m, I):
        return f'<inertial pos="{pos[0]} {pos[1]} {pos[2]}" mass="{m}" diaginertia="{I[0]} {I[1]} {I[2]}"/>'

    def hinge(name, axis):
        return f'<joint name="{name}" type="hinge" axis="{axis}"/>'

    geoms = {}
    if native_contact:
        for i, (b, c, r) in enumerate(contact_spheres(p)):
            geoms.setdefault(b, []).append(f'<geom name="c{i}" type="sphere" size="{r}" pos="{c[0]} {c[1]} {c[2]}" contype="0" conaffinity="1" friction="{p['mu']} 0.005 0.0001"/>')
    g = lambda b: ''.join(geoms.get(b, []))
    lt, ls, ah = p['l_thigh'], p['l_shin'], p['ankle_h']
    legs = arms = ''
    for side, sgn in (('L', 1), ('R', -1)):
        legs += f'''
      <body name="thigh{side}" pos="0 {sgn * p['hip_w']} 0">
        {hinge('hipRoll' + side, '1 0 0')}{hinge('hipPitch' + side, '0 1 0')}
        {inertial((0, 0, -lt / 2), 8, (0.11, 0.11, 0.02))}{g('thigh' + side)}
        <body name="shin{side}" pos="0 0 {-lt}">
          {hinge('knee' + side, '0 1 0')}
          {inertial((0, 0, -ls / 2), 5, (0.067, 0.067, 0.008))}{g('shin' + side)}
          <body name="foot{side}" pos="0 0 {-ls}">
            {hinge('anklePitch' + side, '0 1 0')}{hinge('ankleRoll' + side, '1 0 0')}
            {inertial((0.05, 0, -ah / 2), 1.5, (0.003, 0.008, 0.008))}{g('foot' + side)}
          </body>
        </body>
      </body>'''
        arms += f'''
        <body name="upperArm{side}" pos="0 {sgn * p['shoulder_w']} 0">
          {hinge('shoulder' + side, '0 1 0')}
          {inertial((0, 0, -p['l_upper_arm'] / 2), 3.5, (0.021, 0.021, 0.004))}{g('upperArm' + side)}
          <body name="forearm{side}" pos="0 0 {-p['l_upper_arm']}">
            {hinge('elbow' + side, '0 1 0')}
            {inertial((0, 0, -p['l_forearm'] / 2), 2.5, (0.013, 0.013, 0.002))}{g('forearm' + side)}
            <body name="finger{side}" pos="0.02 0 {-p['l_forearm'] - 0.03}">
              {hinge('finger' + side, '0 1 0')}
              {inertial((0, 0, -p['l_finger'] / 2), 0.12, (4e-5, 4e-5, 1e-5))}{g('finger' + side)}
            </body>
            <body name="thumb{side}" pos="-0.025 0 {-p['l_forearm'] - 0.02}">
              {hinge('thumb' + side, '0 1 0')}
              {inertial((0, 0, -p['l_thumb'] / 2), 0.08, (2e-5, 2e-5, 5e-6))}{g('thumb' + side)}
            </body>
          </body>
        </body>'''
    sh = p['shoulder_h']
    flags = '' if native_contact else '<flag contact="disable"/>'
    floor = f'<geom name="floor" type="plane" size="0 0 1" contype="1" conaffinity="1" friction="{p['mu']} 0.005 0.0001"/>' if native_contact else ''
    acts = ''
    if actuators:
        leg_peak = [p['tau_hip_roll_max'], p['tau_hip_pitch_max'], p['tau_knee_max'], p['tau_ankle_pitch_max'], p['tau_ankle_roll_max']]
        for side in 'LR':
            for j, pk in zip(['hipRoll', 'hipPitch', 'knee', 'anklePitch', 'ankleRoll'], leg_peak):
                acts += f'<general name="a_{j}{side}" joint="{j}{side}" gainprm="{p["kp_leg"]}" biastype="affine" biasprm="0 {-p["kp_leg"]} {-p["kd_leg"]}" forcelimited="true" forcerange="{-pk} {pk}"/>'
            for j, pk in (('shoulder', p['tau_shoulder_max']), ('elbow', p['tau_elbow_max'])):
                acts += f'<general name="a_{j}{side}" joint="{j}{side}" gainprm="{p["kp_arm"]}" biastype="affine" biasprm="0 {-p["kp_arm"]} {-p["kd_arm"]}" forcelimited="true" forcerange="{-pk} {pk}"/>'
            for j in ('finger', 'thumb'):
                pk = p['tau_finger_max']
                acts += f'<general name="a_{j}{side}" joint="{j}{side}" gainprm="{p["kp_finger"]}" biastype="affine" biasprm="0 {-p["kp_finger"]} {-p["kd_finger"]}" forcelimited="true" forcerange="{-pk} {pk}"/>'
        acts = f'<actuator>{acts}</actuator>'
    return f'''<mujoco model="HumanoidPhysical">
  <option timestep="{timestep}" integrator="{integrator}" gravity="0 0 -9.81">{flags}</option>
  <worldbody>{floor}
    <body name="pelvis" pos="0 0 {p['z_pelvis0']}">
      <freejoint name="root"/>
      {inertial((0, 0, 0), 12, (0.12, 0.10, 0.12))}{g('pelvis')}
      <body name="torso" pos="0 0 0">
        {inertial((0, 0, sh / 2), 30, (0.56, 0.77, 0.44))}{g('torso')}
        <body name="neck" pos="0 0 {sh}">
          {inertial((0, 0, p['l_neck']), 5, (0.03, 0.03, 0.03))}{g('neck')}{arms}
        </body>
      </body>{legs}
    </body>
  </worldbody>
  {acts}
</mujoco>
'''
