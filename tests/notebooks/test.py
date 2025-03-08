import casadi as ca
import numpy as np


class BaseModel:

    def __init__(self):
        pass

    def __repr__(self):
        return repr(self.__dict__)

    def simulate(self, t=None, u=None):
        if t is None:
            t = np.arange(0, 1, 0.01)
        if u is None:
            u = 0

        p0 = np.array([self.p0[k] for k in self.p0.keys()])
        z0 = np.array([self.z0[k] for k in self.z0.keys()])
        x0 = np.array([self.x0[k] for k in self.x0.keys()])

        F = ca.integrator(
            'F', 'idas',
            {'x': self.x, 'z': self.z, 'p': self.p, 'u': self.u, 'ode': self.ode, 'alg': self.alg},
            t[0], t)

        res = F(x0=x0, z0=z0, p=p0, u=u)
        return {
            't': t,
            'x': res['xf'].T
        }
    
    def linearize(self):
        A = ca.jacobian(self.ode, self.x)
        B = ca.jacobian(self.ode, self.u)
        C = ca.jacobian(self.y, self.x)
        D = ca.jacobian(self.y, self.u)
        return (A, B, C, D)


def cat(axis, *args):
    return ca.vertcat(*args)


class Integrator(BaseModel):

    def __init__(self):

        # define the symbolic variables
        x = ca.SX.sym('x')

        # define the state vector
        self.x = ca.vertcat(
            x
        )

        der_x = 
    {"ComponentReference": {"local": false, "parts": [{"ident": {"location": {"end": 55, "end_column": 18, "end_line": 4, "file_name": "integrator.mo", "start": 51, "start_column": 14, "start_line": 4}, "text": "time", "token_number": 11, "token_type": 64}, "subs": none}]}}
    ;

        self.ode = ca.vertcat(
            der_x
        )
        self.z = ca.vertcat()
        self.u = ca.vertcat()
        self.y = ca.vertcat()
        self.p = ca.vertcat()
        self.alg = ca.vertcat()

        self.p0 = {}
        self.z0 = {}
        self.x0 = {}

if __name__ == '__main__':
    Clasee
