/*
  CircularBufferFilter.h - alternative to a band-pass filter which uses a circular buffer to identify features of
  interest with a known characteristic frequency.
  Note: while this filter produces a cleaner signal than BandPassFilter for certain Extend-o-Hand data,
  it also consumes too much RAM to be used on the Arduino.

  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef CircularBufferFilter_h
#define CircularBufferFilter_h

#include <ScalarFilter.h>

// TODO: temporary
#define BUFSIZE 10

class CircularBufferFilter : public ScalarFilter
{
  public:
    CircularBufferFilter(double period, double minTimestep);
    ~CircularBufferFilter();

    void updateTimestep(double timestep);

    double processNext(unsigned long time, double value);

  private:
    double period;
    double sum;
    int size; // the number of items logically held in the buffer; less than or equal to the capacity of the buffer
    int bufferIndex;

    // TODO: temporary
    double buffer[BUFSIZE];
    //double *buffer;
};

#endif // CircularBufferFilter_h
