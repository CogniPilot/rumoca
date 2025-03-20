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
        self.u = sympy.Matrix([[]]).T
        self.u0 = { }
        
        # ============================================
        # Declare p
        e = sympy.symbols('e')
        h0 = sympy.symbols('h0')
        self.p = sympy.Matrix([[
            e, 
            h0]]).T
        self.p0 = { 
            'e': 0.8, 
            'h0': 1.0}
        
        # ============================================
        # Declare cp
        self.cp = sympy.Matrix([[]]).T
        self.cp0 = { }
        
        # ============================================
        # Declare x
        h = sympy.symbols('h')
        v = sympy.symbols('v')
        self.x = sympy.Matrix([[
            h, 
            v]]).T
        self.x0 = { 
            'h': 0.0, 
            'v': 0.0}
        
        # ============================================
        # Declare m
        self.m = sympy.Matrix([[]]).T
        self.m0 = { }
        
        # ============================================
        # Declare y
        self.y = sympy.Matrix([[]]).T
        self.y0 = { }
        
        # ============================================
        # Declare z
        self.z = sympy.Matrix([[]]).T
        self.z0 = { }
        
        

        # ============================================
        # Declare x_dot
        der_h = sympy.symbols('der_h')
        der_v = sympy.symbols('der_v')
        self.x_dot = sympy.Matrix([[
            der_h, 
            der_v]]).T

        # ============================================
        # Define Continous Update Function: fx
        self.fx = sympy.Matrix([[
            v - (der_h), 
            der_v - (9.81), 
            ]]).T

        # ============================================
        # Events and Event callbacks
        self.events = []
        self.event_callback = {}

        # ============================================
        # Solve for explicit ODE
        try:
            print(self.x_dot.shape)
            print(self.y.shape)
            v = sympy.Matrix.vstack(self.x_dot, self.y)
            sol = sympy.solve(self.fx, v)
        except Exception as e:
            print('solving failed')
            for k in self.__dict__.keys():
                print(k, self.__dict__[k])
            raise(e)
        self.sol_x_dot = self.x_dot.subs(sol)
        self.sol_y = self.y.subs(sol)
        self.f_x_dot = sympy.lambdify([time, self.x, self.m, self.u, self.p], list(self.sol_x_dot))
        self.f_y = sympy.lambdify([time, self.x, self.m, self.u, self.p], list(self.sol_y))

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
                return np.zeros(self.u.shape[0])

        # ============================================
        # Declare initial vectors
        u0 = np.array([self.u0[k] for k in self.u0.keys()])
        p0 = np.array([self.p0[k] for k in self.p0.keys()])
        cp0 = np.array([self.cp0[k] for k in self.cp0.keys()])
        x0 = np.array([self.x0[k] for k in self.x0.keys()])
        m0 = np.array([self.m0[k] for k in self.m0.keys()])
        y0 = np.array([self.y0[k] for k in self.y0.keys()])
        z0 = np.array([self.z0[k] for k in self.z0.keys()])
        

        res = scipy.integrate.solve_ivp(
            y0=x0,
            fun=lambda ti, x: self.f_x_dot(ti, x, m0, u(ti), p0),
            t_span=[t[0], t[-1]],
            t_eval=t,
        )

        # check for event
        y0 = res['y'][:, -1]
        if res.t_events is not None:
            for i, t_event in enumerate(res.t_events):
                if len(t_event) > 0:
                    print('event', i)
                    if i in self.event_callback:
                        print('detected event', i, t_event[i])
                        y0 = self.event_callback[i](t_event[i], y0)

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
