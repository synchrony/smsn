/*
  MMA7361.h - calibration and sampling with the Freescale Semiconductor MMA7361 3-axis accelerometer
  Created by Joshua Shinavier, 2013-2014
  Released into the public domain.
*/

#ifndef MMA7361_h
#define MMA7361_h

#include "Arduino.h"

class MMA7361
{
  public:
    MMA7361(uint8_t xPin, uint8_t yPin, uint8_t zPin);

    void calibrateX(int min, int max);  // (min, max): the raw sensor outputs at a_x = -g, g
    void calibrateY(int min, int max);  // (min, max): the raw sensor outputs at a_y = -g, g
    void calibrateZ(int min, int max);  // (min, max): the raw sensor outputs at a_z = -g, g

    double accelX();  // call calibrateX() once before using accelX()
    double accelY();  // call calibrateY() once before using accelY()
    double accelZ();  // call calibrateZ() once before using accelZ()

    int rawX();
    int rawY();
    int rawZ();

  private:
    uint8_t _xPin, _yPin, _zPin;

    unsigned int xmin, xmax, ymin, ymax, zmin, zmax;
    double xmid, ymid, zmid;
    double xrange, yrange, zrange;
};

#endif // MMA7361_h
