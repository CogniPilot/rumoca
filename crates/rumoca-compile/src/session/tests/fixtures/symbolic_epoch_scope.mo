within;
package EpochRecord
  record Status
    Integer reseedCount;
  end Status;
  connector StatusOutput = output Status;
  partial block PartialEstimator
    discrete StatusOutput status(reseedCount(start=0, fixed=true));
  end PartialEstimator;
  block Estimator
    extends PartialEstimator;
  equation
    when sample(0, 0.01) then
      status.reseedCount = pre(status.reseedCount) + 1;
    end when;
  end Estimator;
  block ConcreteEstimator
    extends Estimator;
  end ConcreteEstimator;
  partial block PartialController
    input Integer navigationEpoch=0;
    parameter Boolean previous=false;
  end PartialController;
  block Task
    input Integer epoch;
  end Task;
  block Controller
    extends PartialController;
    Task task(epoch=navigationEpoch);
  end Controller;
  block PacedController
    extends Controller(final previous=true);
  end PacedController;
  model Vehicle
    replaceable block ControllerModel=Controller constrainedby PartialController;
    ControllerModel avionics(navigationEpoch=pre(estimator.status.reseedCount));
    replaceable block EstimatorModel=ConcreteEstimator constrainedby PartialEstimator;
    EstimatorModel estimator;
  end Vehicle;
  model Test
    extends Vehicle(redeclare block ControllerModel=PacedController);
  end Test;
  model Pair
    Vehicle first;
    Vehicle second;
  end Pair;
end EpochRecord;
