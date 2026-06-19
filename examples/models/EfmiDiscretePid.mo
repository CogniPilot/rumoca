model EfmiDiscretePid
  input Real wLoadRef(
    start=0.0,
    min=-1.0e5,
    max=1.0e5);
  input Real wMotor(
    start=0.0,
    min=-1.0e5,
    max=1.0e5);
  discrete output Real vMotor(
    start=0.0,
    fixed=true,
    min=-1.0e7,
    max=1.0e7);

  parameter Real limiterUMax(
    min=1.0,
    max=1.0e5) = 400.0;
  final parameter Real limiterUMin(
    min=-1.0e5,
    max=-1.0) = -limiterUMax;
  parameter Real gearRatio(
    min=10.0,
    max=500.0) = 105.0;
  parameter Real Ti(
    min=1.0e-7,
    max=100.0) = 0.1;
  parameter Real Td(
    min=1.0e-7,
    max=100.0) = 0.1;
  parameter Real kd(
    min=0.0,
    max=1000.0) = 0.1;
  parameter Real k(
    min=0.0,
    max=1000.0) = 10.0;
  parameter Real stepSize(
    min=1.0e-10,
    max=0.01) = 1.0e-3;
  constant Real samplePeriod(
    unit="s") = 1.0e-3;

protected
  discrete Real pidIx(start=0.0, fixed=true);
  discrete Real pidDx(start=0.0, fixed=true);
  discrete Real previousFeedback(start=0.0, fixed=true);
  discrete Boolean firstTick(start=true, fixed=true);

  discrete Real gainY(start=0.0, fixed=true);
  discrete Real feedbackY(start=0.0, fixed=true);
  discrete Real derivativePidIx(start=0.0, fixed=true);
  discrete Real derivativePidDx(start=0.0, fixed=true);
  discrete Real pidDy(start=0.0, fixed=true);
  discrete Real pidY(start=0.0, fixed=true);

algorithm
  when sample(0.0, samplePeriod) then
    if pre(firstTick) then
      firstTick := false;
    else
      derivativePidIx := pre(previousFeedback) / Ti;
      derivativePidDx := (pre(previousFeedback) - pre(pidDx)) / Td;

      pidIx := pre(pidIx) + stepSize * derivativePidIx;
      pidDx := pre(pidDx) + stepSize * derivativePidDx;
    end if;

    gainY := gearRatio * wLoadRef;
    feedbackY := gainY - wMotor;

    pidDy := kd * (feedbackY - pidDx) / Td;
    pidY := k * (pidDy + pidIx + feedbackY);

    vMotor := if pidY > limiterUMax then
        limiterUMax
      elseif pidY < limiterUMin then
        limiterUMin
      else
        pidY;

    previousFeedback := feedbackY;
  end when;
end EfmiDiscretePid;
