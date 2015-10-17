/*
  SpikeDetector.h - simple utility for identifying spikes in accelerometer data
  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef SpikeDetector_h
#define SpikeDetector_h

#include <BooleanFilter.h>

class SpikeDetector : public BooleanFilter
{
  public:
    SpikeDetector(double minAmplitude, unsigned int minPeriod);

    // returns whether the given data point is a spike, sufficiently far from the last spike
    unsigned long processNext(unsigned long time, double value);

  private:
    double minAmplitude;
    unsigned int minPeriod;

    unsigned long timeOfLastSpike;
};

#endif // SpikeDetector_h