/*
  LowPassFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "LowPassFilter.h"

LowPassFilter::LowPassFilter(double _rc) {
    rc = _rc;
    alpha = 0;
}

void LowPassFilter::updateTimestep(double timestep) {
    alpha = timestep / (rc + timestep);
}

double LowPassFilter::processNext(unsigned long time, double amplitude) {
    outLast = alpha * amplitude + (1-alpha) * outLast;
    return outLast;
}
