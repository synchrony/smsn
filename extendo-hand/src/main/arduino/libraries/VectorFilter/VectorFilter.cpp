/*
  VectorFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "VectorFilter.h"

VectorFilter::VectorFilter(ScalarFilter &_xFilter, ScalarFilter &_yFilter, ScalarFilter &_zFilter) {
    xFilter = &_xFilter;
    yFilter = &_yFilter;
    zFilter = &_zFilter;
}

Vector3D VectorFilter::processNext(unsigned long time, Vector3D &in) {
    Vector3D out(
        xFilter->processNext(time, in.getX()),
        yFilter->processNext(time, in.getY()),
        zFilter->processNext(time, in.getZ()));
    return out;
}

void VectorFilter::updateTimestep(double timestep) {
    xFilter->updateTimestep(timestep);
    yFilter->updateTimestep(timestep);
    zFilter->updateTimestep(timestep);
}
