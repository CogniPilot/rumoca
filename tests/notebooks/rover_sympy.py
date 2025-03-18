import sympy
import numpy as np
import scipy.integrate

cos = sympy.cos
sin = sympy.sin
tan = sympy.tan

class Model:
    """
    Flattened Modelica Model
    """

    def __init__(self):
        # ============================================
        # Declare time
        time = sympy.symbols('time')

        # ============================================
        # Declare u
        thr = sympy.symbols('thr')
        str = sympy.symbols('str')
        self.u = sympy.Matrix([
            thr, 
            str])
        self.u0 = { 
            'thr': 0.0, 
            'str': 0.0}
        
        # ============================================
        # Declare p
        l = sympy.symbols('l')
        r = sympy.symbols('r')
        m1_tau = sympy.symbols('m1_tau')
        self.p = sympy.Matrix([
            l, 
            r, 
            m1_tau])
        self.p0 = { 
            'l': 1.0, 
            'r': 0.1, 
            'm1_tau': 1.0}
        
        # ============================================
        # Declare cp
        self.cp = sympy.Matrix([])
        self.cp0 = { }
        
        # ============================================
        # Declare x
        m1_omega = sympy.symbols('m1_omega')
        x = sympy.symbols('x')
        y = sympy.symbols('y')
        theta = sympy.symbols('theta')
        self.x = sympy.Matrix([
            m1_omega, 
            x, 
            y, 
            theta])
        self.x0 = { 
            'm1_omega': 0.0, 
            'x': 0.0, 
            'y': 0.0, 
            'theta': 0.0}
        
        # ============================================
        # Declare y
        v = sympy.symbols('v')
        m1_omega_ref = sympy.symbols('m1_omega_ref')
        self.y = sympy.Matrix([
            v, 
            m1_omega_ref])
        self.y0 = { 
            'v': 0.0, 
            'm1_omega_ref': 0.0}
        
        # ============================================
        # Declare z
        self.z = sympy.Matrix([])
        self.z0 = { }
        
        

        # ============================================
        # Declare x_dot
        der_m1_omega = sympy.symbols('der_m1_omega')
        der_x = sympy.symbols('der_x')
        der_y = sympy.symbols('der_y')
        der_theta = sympy.symbols('der_theta')
        self.x_dot = sympy.Matrix([
            der_m1_omega, 
            der_x, 
            der_y, 
            der_theta])

        # ============================================
        # Define Continous Update Function: fx
        self.fx = sympy.Matrix([
            v - (r * m1_omega), 
            der_x - (v * cos(theta)), 
            der_y - (v * sin(theta)), 
            der_theta - (v / l * tan(str)), 
            m1_omega_ref - (thr), 
            der_m1_omega - (1 / m1_tau * m1_omega_ref - m1_omega)])

        # ============================================
        # Solve for explicit ODE
        sol = sympy.solve(self.fx, sympy.Matrix.vstack(self.x_dot, self.y))
        self.sol_x_dot = self.x_dot.subs(sol)
        self.sol_y = self.y.subs(sol)
        self.f_x_dot = sympy.lambdify([time, self.x, self.u, self.p], list(self.sol_x_dot))
        self.f_y = sympy.lambdify([time, self.x, self.u, self.p], list(self.sol_y))

    def __repr__(self):
        return repr(self.__dict__)

    def simulate(self, t=None, u=None):
        """
        Simulate the modelica model
        """
        if t is None:
            t = np.arange(0, 1, 0.01)
        if u is None:
            def u(t):
                return 0

        # ============================================
        # Declare initial vectors
        u0 = np.array([self.u0[k] for k in self.u0.keys()])
        p0 = np.array([self.p0[k] for k in self.p0.keys()])
        cp0 = np.array([self.cp0[k] for k in self.cp0.keys()])
        x0 = np.array([self.x0[k] for k in self.x0.keys()])
        y0 = np.array([self.y0[k] for k in self.y0.keys()])
        z0 = np.array([self.z0[k] for k in self.z0.keys()])
        

        res = scipy.integrate.solve_ivp(
            y0=x0,
            fun=lambda ti, x: self.f_x_dot(ti, x, u(ti), p0),
            t_span=[t[0], t[-1]],
            t_eval=t,
        )

        x = res['y']
        #y = [ self.f_y(ti, xi, u(ti), p0) for (ti, xi) in zip(t, x) ]
        #y = self.f_y(0, [1, 2, 3, 4], [1], [1, 2, 3, 4])
        print(self.sol_y)

        return {
            't': t,
            'x': x,
            'u': u(t),
            #'y': y,
        }
