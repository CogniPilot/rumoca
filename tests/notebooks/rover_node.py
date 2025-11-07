import rclpy
from rclpy.node import Node
from geometry_msgs.msg import TransformStamped, PoseStamped
from tf2_ros import TransformBroadcaster
from sensor_msgs.msg import Joy
import numpy as np
import casadi as ca

import gen.rover_casadi_daebuilder as rover_casadi

class RoverNode(Node):
    def __init__(self):
        super().__init__('rover_node')
        self.tf_b = TransformBroadcaster(self)
        self.pose_pub = self.create_publisher(PoseStamped, '/rover/pose', 10)

        # Model + params
        self.model = rover_casadi.Model(model_name='rover')
        p_start = self.model.dae.start(self.model.dae.p())
        self.p = np.asarray(p_start, dtype=float).squeeze()
        self.p[0] = 0.30  # l
        self.p[1] = 0.05  # r
        self.p[2] = 0.20  # tau

        # State (m1_omega, theta, x, y, z)
        x_start = self.model.dae.start(self.model.dae.x())
        self.x = np.asarray(x_start, dtype=float).squeeze()

        # Timing
        self.dt_outer = 0.05   # 20 Hz
        self.dt_inner = 0.01

        # Controls
        self.thr = 0.0
        self.ste = 0.0
        self.create_subscription(Joy, 'joy', self.joy_cb, 10)

        # Loop
        self.create_timer(self.dt_outer, self.step)

    def joy_cb(self, msg: Joy):
        a_throttle = msg.axes[1] if len(msg.axes) > 1 else 0.0
        a_steer    = msg.axes[3] if len(msg.axes) > 3 else 0.0
        self.thr = 2.0 * float(a_throttle)  # rad/s
        self.ste = 0.6 * float(a_steer)     # rad

    def step(self):
        try:
            # u must be (nu, Nt) where tgrid = arange(0, dt_outer, dt_inner)
            tgrid = np.arange(0.0, self.dt_outer, self.dt_inner)
            Nt = max(1, tgrid.size)
            u = np.tile(np.array([[self.ste], [self.thr]], dtype=float), (1, Nt))

            # Integrate one step
            _, simres = self.model.simulate(
                t0=0.0, tf=self.dt_outer, dt=self.dt_inner,
                x0=self.x, p0=self.p, f_u=u
            )

            # simres['xf'] is DM (nx, Nt); take last column
            xf_dm = simres['xf']
            self.x = np.array(xf_dm[:, -1].full()).squeeze()

            # Unpack floats
            mot, yaw, px, py, pz = map(float, self.x[:5])

            # Print position each step
            self.get_logger().info(f"pos x={px:.3f} y={py:.3f} yaw={yaw:.3f} thr={self.thr:.2f} ste={self.ste:.2f} mot={mot:.2f}")

            # Publish TF (map -> rover)
            tfmsg = TransformStamped()
            tfmsg.header.stamp = self.get_clock().now().to_msg()
            tfmsg.header.frame_id = 'map'
            tfmsg.child_frame_id  = 'rover'
            tfmsg.transform.translation.x = px
            tfmsg.transform.translation.y = py
            tfmsg.transform.translation.z = 0.0
            cy = np.cos(yaw * 0.5); sy = np.sin(yaw * 0.5)
            tfmsg.transform.rotation.x = 0.0
            tfmsg.transform.rotation.y = 0.0
            tfmsg.transform.rotation.z = float(sy)
            tfmsg.transform.rotation.w = float(cy)
            self.tf_b.sendTransform(tfmsg)

            # Publish PoseStamped (easy to visualize in RViz)
            pose = PoseStamped()
            pose.header.stamp = tfmsg.header.stamp
            pose.header.frame_id = 'map'
            pose.pose.position.x = px
            pose.pose.position.y = py
            pose.pose.position.z = 0.0
            pose.pose.orientation.z = float(sy)
            pose.pose.orientation.w = float(cy)
            self.pose_pub.publish(pose)

        except Exception as e:
            self.get_logger().error(f'Rover step failed: {e}')

def run():
    rclpy.init()
    node = RoverNode()
    try:
        rclpy.spin(node)
    except KeyboardInterrupt:
        pass
    finally:
        node.destroy_node()
        rclpy.shutdown()

if __name__ == '__main__':
    run()