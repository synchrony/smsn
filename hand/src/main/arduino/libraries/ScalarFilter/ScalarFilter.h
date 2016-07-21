/*
  ScalarFilter.h - abstract base class of filters for time-series data which produce real values
  See: http://github.com/joshsh/smsn

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef ScalarFilter_h
#define ScalarFilter_h

class ScalarFilter
{
  public:
    virtual double processNext(unsigned long time, double value) = 0;

    virtual void updateTimestep(double timestep) = 0;
};

#endif // ScalarFilter_h
