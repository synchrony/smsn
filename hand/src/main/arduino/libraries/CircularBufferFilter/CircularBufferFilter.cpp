/*
  CircularBufferFilter.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "CircularBufferFilter.h"
#include <stdlib.h>

CircularBufferFilter::CircularBufferFilter(double _period, double minTimestep) {
    period = _period;
    updateTimestep(minTimestep);

    // TODO: restore
    //buffer = (double*) malloc(sizeof(double) * size);

    // don't start with undefined values in the buffer
    for (int i = 0; i < size; i++) {
        buffer[i] = 0;
    }

    bufferIndex = 0;
    sum = 0;
}

CircularBufferFilter::~CircularBufferFilter() {
    // TODO: restore
    //free(buffer);
}

void CircularBufferFilter::updateTimestep(double timestep) {
    // TODO: restore
    // size = (int) (period / timestep);

    // TODO: temporary
    size = BUFSIZE;
}

double CircularBufferFilter::processNext(unsigned long time, double value) {
    sum = sum - buffer[bufferIndex] + value;
    buffer[bufferIndex] = value;
    bufferIndex = (bufferIndex + 1) % size;
    return sum / size;
}