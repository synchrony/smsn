/*
  DirectionFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "DirectionFilter.h"
#include <math.h>

DirectionFilter::DirectionFilter(Vector3D &center, double _radius) {
    double mag = center.getMagnitude();
    radius = _radius;

    // note: mag == 0 is an error condition, but we won't attempt to handle it in the Arduino environment.
    refx = center.getX()/mag;
    refy = center.getY()/mag;
    refz = center.getZ()/mag;
}

bool DirectionFilter::process(Vector3D &v) {
    double mag = v.getMagnitude();
    double dist = sqrt(v.getX()*refx + v.getY()*refy + v.getZ()*refz);
    return dist <= radius;
}

double DirectionFilter::processFuzzy(Vector3D &v) {
    double mag = v.getMagnitude();
    double dist = sqrt(v.getX()*refx + v.getY()*refy + v.getZ()*refz);
    return dist > radius ? -1 : dist / radius;
}
