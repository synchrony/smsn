/*
  LowPassFilter.h - a low-pass filter implementation
  See: http://github.com/joshsh/smsn

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef LowPassFilter_h
#define LowPassFilter_h

#include <ScalarFilter.h>

class LowPassFilter : public ScalarFilter
{
  public:
    LowPassFilter(double rc);

    void updateTimestep(double timestep);

    double processNext(unsigned long time, double amplitude);

  private:
    double rc;
    double alpha;
    double outLast;
};

#endif // LowPassFilter_h
