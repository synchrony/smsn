/*
  SpikeDetector.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "SpikeDetector.h"

SpikeDetector::SpikeDetector(double _minAmplitude, unsigned int _minPeriod) {
    minAmplitude = _minAmplitude;
    minPeriod = _minPeriod;
    timeOfLastSpike = 0;
}

unsigned long SpikeDetector::processNext(unsigned long time, double value) {
    if (value >= minAmplitude) {
        unsigned long r = time - timeOfLastSpike >= minPeriod ? time : 0;

        // this becomes the time of the last spike, even if it is not recognized as such;
        // tight sequences of spikes are counted as one, and end at the last spike
        timeOfLastSpike = time;

        return r;
    } else {
        return 0;
    }
}
