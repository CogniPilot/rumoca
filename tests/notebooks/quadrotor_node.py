import rclpy
from rclpy.executors import SingleThreadedExecutor
from rclpy.node import Node
from sensor_msgs.msg import Joy
from rosgraph_msgs.msg import Clock
from builtin_interfaces.msg import Time
from geometry_msgs.msg import PoseStamped, TransformStamped
from tf2_ros import TransformBroadcaster
import numpy as np  # Ensure numpy is imported
import quadrotor_sympy
import tf_transformations
from geometry_msgs.msg import Quaternion

class QuadrotorNode(Node):
    def __init__(self, context):
        super().__init__(node_name='quadrotor_node', context=context)
        self.subscription = self.create_subscription(
            Joy,
            'joy',
            self.joy_callback,
            10
        )
        self.model = quadrotor_sympy.Model()
        self.tf_broadcaster = TransformBroadcaster(self)  # Initialize the TF2 broadcaster
        self.sim_time_publisher = self.create_publisher(Clock, '/clock', 10)  # Sim time publisher
        self.timer = self.create_timer(0.1, self.step_simulation)  # Timer to publish sim time

        self.sim_time = 0.0  # Initialize simulation time
        self.x0 = np.zeros(16)
        self.joy_aileron = 0
        self.joy_elevator = 0
        self.joy_rudder = 0
        self.joy_throttle = 0.5
        self.get_logger().info(f"Simulation started")

    def step_simulation(self):
        """Step the simulation."""
        # publish sim time
        self.sim_time += 0.1  # Increment simulation time by 0.1 seconds
        sim_time_msg = Clock()
        sim_time_msg.clock = Time()
        sim_time_msg.clock.sec = int(self.sim_time)
        sim_time_msg.clock.nanosec = int((self.sim_time - int(self.sim_time)) * 1e9)
        self.sim_time_publisher.publish(sim_time_msg)
        # self.get_logger().info(f"Published sim_time: {sim_time_msg.clock.sec}.{sim_time_msg.clock.nanosec}")
        
        u= np.array([
            self.joy_aileron,
            self.joy_elevator,
            self.joy_rudder,
            self.joy_throttle])
        
        res = self.model.simulate(t0=0, tf=0.1, dt=0.01, x0=self.x0, f_u=lambda t: u)
        self.x0 = res['x'][:, -1]

        q = tf_transformations.quaternion_from_euler(
            self.x0[9], # roll (same)
            -self.x0[10],  # pitch (inverted in ros)
            -self.x0[11], # inverted in ros
            )

        # Publish the transform
        transform = TransformStamped()
        transform.header.stamp = sim_time_msg.clock
        transform.header.frame_id = "map"
        transform.child_frame_id = "quadrotor"
        transform.transform.translation.x = self.x0[0]
        transform.transform.translation.y = -self.x0[1] # y coordinate flip
        transform.transform.translation.z = self.x0[2]
        transform.transform.rotation.x = q[0]
        transform.transform.rotation.y = q[1]
        transform.transform.rotation.z = q[2]
        transform.transform.rotation.w = q[3]
        self.tf_broadcaster.sendTransform(transform)

    def joy_callback(self, msg):
        self.joy_aileron = -0.1*msg.axes[3]
        self.joy_elevator = 0.1*msg.axes[4]
        self.joy_rudder = -1*msg.axes[0]
        self.joy_throttle = msg.axes[1]/2 + 0.6
        print(f"ail: {self.joy_aileron}, elv: {self.joy_elevator}, rdr: {self.joy_rudder}, thr: {self.joy_throttle}")

def run(args=None):
    # Create a scoped ROS 2 context
    context = rclpy.Context()
    try:
        context.init()
        executor = SingleThreadedExecutor(context=context)
        executor.add_node(QuadrotorNode(context=context))
        executor.spin()
    except KeyboardInterrupt:
        pass
    finally:
        print('simulation finished')
        context.try_shutdown()
        del executor
        del context