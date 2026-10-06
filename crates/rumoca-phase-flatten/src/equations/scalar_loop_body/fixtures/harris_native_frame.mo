// Full 160×90 grayscale and central gradients, then ordered 5×5 Harris.
// Outputs cover the complete detector's four-pixel-border interior. The frame
// adapter only places these scores at (x+4,y+4) and supplies zero border scores.
// Every intensity, gradient, product and response operation is compiled Modelica.
// Independent correctness and source admission are pending; no production preset.
// Regenerate with scripts/generate-native-harris.mjs.
model HarrisNativeFrame
  parameter Integer height = 90;
  parameter Integer width = 160;
  parameter Real harris_k = 0.04;
  input Real rgb[height,width,3] = fill(0.0,height,width,3);
  output Real score[height-8,width-8];
protected
  Real gray[height,width];
  Real dx[height-2,width-2]; Real dy[height-2,width-2];
  Real gxx[height-2,width-2]; Real gyy[height-2,width-2];
  Real gxy[height-2,width-2];
  Real xx[height-8,width-8]; Real yy[height-8,width-8];
  Real xy[height-8,width-8];
equation
  for y in 1:height loop
    for x in 1:width loop
      gray[y,x] = ((rgb[y,x,1]+rgb[y,x,2])+rgb[y,x,3])/3.0/255.0;
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      dx[y,x] = (gray[y+1,x+2]-gray[y+1,x])/2.0;
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      dy[y,x] = (gray[y+2,x+1]-gray[y,x+1])/2.0;
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      gxx[y,x] = dx[y,x]*dx[y,x];
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      gyy[y,x] = dy[y,x]*dy[y,x];
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      gxy[y,x] = dx[y,x]*dy[y,x];
    end for;
  end for;
  for y in 1:height-8 loop
    for x in 1:width-8 loop
      xx[y,x] = (0.0+gxx[y+1,x+1]+gxx[y+1,x+2]+gxx[y+1,x+3]+gxx[y+1,x+4]+gxx[y+1,x+5]+gxx[y+2,x+1]+gxx[y+2,x+2]+gxx[y+2,x+3]+gxx[y+2,x+4]+gxx[y+2,x+5]+gxx[y+3,x+1]+gxx[y+3,x+2]+gxx[y+3,x+3]+gxx[y+3,x+4]+gxx[y+3,x+5]+gxx[y+4,x+1]+gxx[y+4,x+2]+gxx[y+4,x+3]+gxx[y+4,x+4]+gxx[y+4,x+5]+gxx[y+5,x+1]+gxx[y+5,x+2]+gxx[y+5,x+3]+gxx[y+5,x+4]+gxx[y+5,x+5])/25.0;
    end for;
  end for;
  for y in 1:height-8 loop
    for x in 1:width-8 loop
      yy[y,x] = (0.0+gyy[y+1,x+1]+gyy[y+1,x+2]+gyy[y+1,x+3]+gyy[y+1,x+4]+gyy[y+1,x+5]+gyy[y+2,x+1]+gyy[y+2,x+2]+gyy[y+2,x+3]+gyy[y+2,x+4]+gyy[y+2,x+5]+gyy[y+3,x+1]+gyy[y+3,x+2]+gyy[y+3,x+3]+gyy[y+3,x+4]+gyy[y+3,x+5]+gyy[y+4,x+1]+gyy[y+4,x+2]+gyy[y+4,x+3]+gyy[y+4,x+4]+gyy[y+4,x+5]+gyy[y+5,x+1]+gyy[y+5,x+2]+gyy[y+5,x+3]+gyy[y+5,x+4]+gyy[y+5,x+5])/25.0;
    end for;
  end for;
  for y in 1:height-8 loop
    for x in 1:width-8 loop
      xy[y,x] = (0.0+gxy[y+1,x+1]+gxy[y+1,x+2]+gxy[y+1,x+3]+gxy[y+1,x+4]+gxy[y+1,x+5]+gxy[y+2,x+1]+gxy[y+2,x+2]+gxy[y+2,x+3]+gxy[y+2,x+4]+gxy[y+2,x+5]+gxy[y+3,x+1]+gxy[y+3,x+2]+gxy[y+3,x+3]+gxy[y+3,x+4]+gxy[y+3,x+5]+gxy[y+4,x+1]+gxy[y+4,x+2]+gxy[y+4,x+3]+gxy[y+4,x+4]+gxy[y+4,x+5]+gxy[y+5,x+1]+gxy[y+5,x+2]+gxy[y+5,x+3]+gxy[y+5,x+4]+gxy[y+5,x+5])/25.0;
    end for;
  end for;
  for y in 1:height-8 loop
    for x in 1:width-8 loop
      score[y,x] = xx[y,x]*yy[y,x]-xy[y,x]*xy[y,x]-harris_k*((xx[y,x]+yy[y,x])*(xx[y,x]+yy[y,x]));
    end for;
  end for;
end HarrisNativeFrame;
