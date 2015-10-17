/*
  HighPassFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "HighPassFilter.h"

HighPassFilter::HighPassFilter(double _rc) {
    rc = _rc;
    alpha = 0;
}

void HighPassFilter::updateTimestep(double timestep) {
    alpha = rc / (rc + timestep);
}

double HighPassFilter::processNext(unsigned long time, double amplitude) {
    outLast = alpha * (outLast + amplitude - inLast);
    inLast = amplitude;
    return outLast;
}

