import sympy
import numpy as np
import scipy.integrate

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
        m1_omega_ref = sympy.symbols('m1_omega_ref')
        m2_omega_ref = sympy.symbols('m2_omega_ref')
        m3_omega_ref = sympy.symbols('m3_omega_ref')
        m4_omega_ref = sympy.symbols('m4_omega_ref')
        self.u = sympy.Matrix([
            m1_omega_ref, 
            m2_omega_ref, 
            m3_omega_ref, 
            m4_omega_ref])
        self.u0 = { 
            'm1_omega_ref': 0.0, 
            'm2_omega_ref': 0.0, 
            'm3_omega_ref': 0.0, 
            'm4_omega_ref': 0.0}
        
        # ============================================
        # Declare p
        m1_tau = sympy.symbols('m1_tau')
        m2_tau = sympy.symbols('m2_tau')
        m3_tau = sympy.symbols('m3_tau')
        m4_tau = sympy.symbols('m4_tau')
        self.p = sympy.Matrix([
            m1_tau, 
            m2_tau, 
            m3_tau, 
            m4_tau])
        self.p0 = { 
            'm1_tau': 1.0, 
            'm2_tau': 1.0, 
            'm3_tau': 1.0, 
            'm4_tau': 1.0}
        
        # ============================================
        # Declare cp
        self.cp = sympy.Matrix([])
        self.cp0 = { }
        
        # ============================================
        # Declare x
        m1_omega = sympy.symbols('m1_omega')
        m2_omega = sympy.symbols('m2_omega')
        m3_omega = sympy.symbols('m3_omega')
        m4_omega = sympy.symbols('m4_omega')
        self.x = sympy.Matrix([
            m1_omega, 
            m2_omega, 
            m3_omega, 
            m4_omega])
        self.x0 = { 
            'm1_omega': 0.0, 
            'm2_omega': 0.0, 
            'm3_omega': 0.0, 
            'm4_omega': 0.0}
        
        # ============================================
        # Declare y
        self.y = sympy.Matrix([])
        self.y0 = { }
        
        # ============================================
        # Declare z
        self.z = sympy.Matrix([])
        self.z0 = { }
        
        

        # ============================================
        # Declare x_dot
        der_m1_omega = sympy.symbols('der_m1_omega')
        der_m2_omega = sympy.symbols('der_m2_omega')
        der_m3_omega = sympy.symbols('der_m3_omega')
        der_m4_omega = sympy.symbols('der_m4_omega')
        self.x_dot = sympy.Matrix([
            der_m1_omega, 
            der_m2_omega, 
            der_m3_omega, 
            der_m4_omega])

        # ============================================
        # Define Continous Update Function: fx
        self.fx = sympy.Matrix([
            m1_omega_ref - (time), 
            m2_omega_ref - (time), 
            m3_omega_ref - (time), 
            m4_omega_ref - (time), 
            der_m1_omega - (1 / m1_tau * m1_omega_ref - m1_omega), 
            der_m2_omega - (1 / m2_tau * m2_omega_ref - m2_omega), 
            der_m3_omega - (1 / m3_tau * m3_omega_ref - m3_omega), 
            der_m4_omega - (1 / m4_tau * m4_omega_ref - m4_omega)])

        # ============================================
        # Solve for explicit ODE
        self.x_dot_eq = self.x_dot.subs(sympy.solve(self.fx, self.x_dot))
        self.f_x_dot_eq = sympy.lambdify([self.x, self.u, self.p], list(self.x_dot_eq))

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
        

        print('x0', x0)

        print('p0', p0)
        print('u(1)', u(1))
        print('t', t)

        fun = lambda ti, x: self.f_x_dot_eq(x, u(ti), p0)

        res = scipy.integrate.solve_ivp(
            y0=x0,
            fun=fun,
            t_span=[t[0], t[-1]],
            t_eval=t,
        )
