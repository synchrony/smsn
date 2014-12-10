/*
  VectorFilter.h - a three-dimensional filter
  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef VectorFilter_h
#define VectorFilter_h

#include <ScalarFilter.h>
#include <Vector.h>

class VectorFilter
{
  public:
    VectorFilter(ScalarFilter *xFilter, ScalarFilter *yFilter, ScalarFilter *zFilter);

    Vector *processNext(unsigned long time, Vector *v);

    void updateTimestep(double timestep);

  private:
    ScalarFilter *xFilter, *yFilter, *zFilter;
    Vector current;
};

#endif // VectorFilter_h
