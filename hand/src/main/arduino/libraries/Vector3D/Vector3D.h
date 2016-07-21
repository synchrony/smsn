/*
  Vector3D.h - class of 3D vectors for use with motion sensor data
  See: http://github.com/joshsh/smsn

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef Vector3D_h
#define Vector3D_h

class Vector3D
{
  public:
    Vector3D();
    Vector3D(double x, double y, double z);
    void set(double x, double y, double z);

    double getX();
    double getY();
    double getZ();
    double getMagnitude();

  private:
    double x, y, z;
    double magnitude;
};

#endif // Vector3D_h
