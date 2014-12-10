/*
  Vector.h - class of 3D vectors for use with accelerometer data
  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef Vector_h
#define Vector_h

class Vector
{
  public:
    Vector();
    Vector(double x, double y, double z);
    void set(double x, double y, double z);

    double getX();
    double getY();
    double getZ();
    double getMagnitude();

  private:
    double x, y, z;
    double magnitude;
};

#endif // Vector_h
