/*
  CrestDetector.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "CrestDetector.h"

CrestDetector::CrestDetector(double _minAmplitude, unsigned int _minPeriod) {
    minAmplitude = _minAmplitude;
    minPeriod = _minPeriod;
    timeOfLastCrest = 0;
    lastValue = 0;
    risingPrev = true;
    high = false;
    refValue = 0;
    refTime = 0;
}

unsigned long CrestDetector::processNext(unsigned long time, double value) {
    unsigned long toReturn = 0;

    bool rising = value >= lastValue;
    lastValue = value;

    if (rising) {
        if (!risingPrev) { // local minimum
            if (high) {
                if (refValue - value >= minAmplitude) {
                    toReturn = time - timeOfLastCrest >= minPeriod ? refTime : 0;

                    // this becomes the time of the last crest, even if it is not recognized as such;
                    // tight sequences of "crests" are counted as one, and end at the last applicable time step
                    timeOfLastCrest = time;

                    refValue = value;
                    high = false;
                }
            } else if (value < refValue) {
                refValue = value;
            }
        }
    } else if (risingPrev) { // local maximum
        if (high) {
            if (value > refValue) {
                refTime = time;
                refValue = value;
            }
        } else if (value - refValue >= minAmplitude) {
            refTime = time;
            refValue = value;
            high = true;
        }
    }

    risingPrev = rising;

    return toReturn;
}
