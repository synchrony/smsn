/*
  HighPassFilter.h - a high-pass filter implementation
  See: http://github.com/joshsh/smsn

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef HighPassFilter_h
#define HighPassFilter_h

#include <ScalarFilter.h>

class HighPassFilter : public ScalarFilter
{
  public:
    HighPassFilter(double rc);

    void updateTimestep(double timestep);

    double processNext(unsigned long time, double amplitude);

  private:
    double rc;
    double alpha;
    double inLast, outLast;
};

#endif // HighPassFilter_h
