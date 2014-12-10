/*
  VectorFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "VectorFilter.h"

VectorFilter::VectorFilter(ScalarFilter *_xFilter, ScalarFilter *_yFilter, ScalarFilter *_zFilter) {
    xFilter = _xFilter;
    yFilter = _yFilter;
    zFilter = _zFilter;
}

Vector *VectorFilter::processNext(unsigned long time, Vector *v) {
    double x = xFilter->processNext(time, v->getX());
    double y = yFilter->processNext(time, v->getY());
    double z = zFilter->processNext(time, v->getZ());
    current.set(x, y, z);
    return &current;
}

void VectorFilter::updateTimestep(double timestep) {
    xFilter->updateTimestep(timestep);
    yFilter->updateTimestep(timestep);
    zFilter->updateTimestep(timestep);
}
