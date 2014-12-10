/*
  CrestDetector.h - utility for identifying successive crests in a signal
  The signal need not be sinusoidal, but it should oscillate between extremes with a characteristic amplitude

  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef CrestDetector_h
#define CrestDetector_h

#include <BooleanFilter.h>

class CrestDetector : public BooleanFilter
{
  public:
    CrestDetector(double minAmplitude, unsigned int minPeriod);

    // returns whether the given data point is a crest, sufficiently far from the last crest
    unsigned long processNext(unsigned long time, double value);

  private:
    double minAmplitude;
    unsigned int minPeriod;

    unsigned long timeOfLastCrest;
    double lastValue;
    double refValue;
    unsigned long refTime;
    bool risingPrev;
    bool high;
};

#endif // CrestDetector_h