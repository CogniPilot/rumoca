// Verbatim source sections from unchanged SLAM SHA256 2ef533e0834a75e774328fe5ff351469f9a0a2a8d0b47238f518d59180270a97.
// source.mo:429-438
function RGBDNominalCalibration
  input Integer imageSize[2] "Height, width";
  input Real fieldOfViewDegrees[2] "Horizontal, vertical";
  output Real calibration[4] "fx, fy, cx, cy";
algorithm
  calibration := {imageSize[2]/(2.0*tan(fieldOfViewDegrees[1]*3.141592653589793/360.0)),
    imageSize[1]/(2.0*tan(fieldOfViewDegrees[2]*3.141592653589793/360.0)),
    (imageSize[2]-1)/2.0,(imageSize[1]-1)/2.0};
end RGBDNominalCalibration;


// source.mo:14397-14402
package D435ImageProfile
  constant Integer width = 848;
  constant Integer height = 480;
  constant Integer colorChannels = 3;
  constant Real depthUnits = 0.001 "Default SDK scale, meters per Z16 unit";
end D435ImageProfile;

package RGBDKeyframes
// source.mo:5488-5491
  constant Integer imageHeight = 90;
  constant Integer imageWidth = 160;
  constant Integer featureCapacity = 350;
  constant Integer descriptorSize = 49;
end RGBDKeyframes;

model CalibrationBase
// source.mo:13107-13112
  parameter Integer imageHeight(min=1) = RGBDKeyframes.imageHeight;
  parameter Integer imageWidth(min=1) = RGBDKeyframes.imageWidth;
  constant Integer featureCapacity = RGBDKeyframes.featureCapacity;
  constant Integer descriptorSize = RGBDKeyframes.descriptorSize;
  final parameter Real defaultRgbCalibration[4] = RGBDNominalCalibration({imageHeight,imageWidth},{69.0,42.0});
  final parameter Real defaultDepthCalibration[4] = RGBDNominalCalibration({imageHeight,imageWidth},{87.0,58.0});
// source.mo:13115 and13119-13120
  parameter Integer channelCount(min=3,max=4) = 4 "RGB or historical RGBA storage";
// source.mo:13116-13117: retain original structural dimension uses.
  input Real rgb[imageHeight,imageWidth,channelCount];
  input Real depth[imageHeight,imageWidth];
  input Real rgbCalibration[4] = defaultRgbCalibration;
  input Real depthCalibration[4] = defaultDepthCalibration;
  output Real result[4];
equation
  result = rgbCalibration + depthCalibration
    + fill(rgb[1,1,1] + depth[1,1],4);
end CalibrationBase;

model CalibrationD435
  extends CalibrationBase(imageHeight=D435ImageProfile.height,
    imageWidth=D435ImageProfile.width,channelCount=D435ImageProfile.colorChannels);
end CalibrationD435;
