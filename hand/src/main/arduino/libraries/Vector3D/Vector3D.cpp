/*
  Vector3D.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "Vector3D.h"
#include <math.h>

Vector3D::Vector3D() {
    // warning: x, y, z, and magnitude are initially undefined
}

Vector3D::Vector3D(double _x, double _y, double _z) {
    set(_x, _y, _z);
}

double Vector3D::getX() {
    return x;
}

double Vector3D::getY() {
    return y;
}

double Vector3D::getZ() {
    return z;
}

void Vector3D::set(double _x, double _y, double _z) {
    x = _x;
    y = _y;
    z = _z;
    magnitude = -1.0;
}

double Vector3D::getMagnitude() {
    if (magnitude < 0) {
        magnitude = sqrt(x*x + y*y + z*z);
    }

    return magnitude;
}
