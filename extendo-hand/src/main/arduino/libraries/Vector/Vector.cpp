/*
  Vector.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "Vector.h"
#include <math.h>

Vector::Vector() {
    // warning: x, y, z, and magnitude are initially undefined
}

Vector::Vector(double _x, double _y, double _z) {
    set(_x, _y, _z);
}

double Vector::getX() {
    return x;
}

double Vector::getY() {
    return y;
}

double Vector::getZ() {
    return z;
}

void Vector::set(double _x, double _y, double _z) {
    x = _x;
    y = _y;
    z = _z;
    magnitude = -1.0;
}

double Vector::getMagnitude() {
    if (magnitude < 0) {
        magnitude = sqrt(x*x + y*y + z*z);
    }

    return magnitude;
}
