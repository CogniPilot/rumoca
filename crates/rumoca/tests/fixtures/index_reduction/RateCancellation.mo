model RateCancellation
      Real q[3](each fixed=false), w[3](each fixed=false), rate[3];
      Real Rx[3,3], Ry[3,3], Rz[3,3], R[3,3];
      Real ax, az;
    initial equation q[2]=0; w[2]=-2.4;
    equation
      der(q)=rate;
      w={cos(q[3])*cos(q[2])*rate[1]+sin(q[3])*rate[2],
         -sin(q[3])*cos(q[2])*rate[1]+cos(q[3])*rate[2],
         sin(q[2])*rate[1]+rate[3]};
      der(w)={ax,0,az};
      Rx=[1,0,0;0,cos(q[1]),sin(q[1]);0,-sin(q[1]),cos(q[1])];
      Ry=[cos(q[2]),0,-sin(q[2]);0,1,0;sin(q[2]),0,cos(q[2])];
      Rz=[cos(q[3]),sin(q[3]),0;-sin(q[3]),cos(q[3]),0;0,0,1];
      R=Rz*Ry*Rx;
      R[2,1]=0;
      R[2,3]=0;
    end RateCancellation;
